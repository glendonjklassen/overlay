{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value, decode, encode, object, (.=))
import Data.Either (isLeft)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector as V
import System.FilePath ((</>))
import Test.Hspec

import Overlay (toRVerse)
import Overlay.Canon
    (Book (..), books, bookById, bookByImpName, bookIds)
import Overlay.Corpus
import Overlay.Import (parseBlock, splitBlocks, tokenize)
import Overlay.Patch
import Overlay.ReaderView (RTok (..), RVerse (..))
import Overlay.Rule
import Overlay.Strongs (StrongsEntry (..), occurrenceIndex, refLabel)
import Overlay.Thread
import Overlay.Weave
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

-- ── helpers ─────────────────────────────────────────────────────────────────

tok :: Text -> [Text] -> Token
tok w ss = Token "" w "" ss 0

tokP :: Text -> Text -> [Text] -> Token
tokP w post ss = Token "" w post ss 0

testCorpus :: Corpus
testCorpus = mkCorpus "test-tok1" $ V.fromList
    [ Verse "Gen" 1 1 [tok "In" [], tok "the" [], tok "beginning" ["H7225"]]
    , Verse "Gen" 1 2 [tok "And" [], tok "the" [], tok "earth" ["H776"]]
    , Verse "Gen" 2 1 [tok "Thus" [], tok "the" [], tok "heavens" ["H8064"]]
    , Verse "Exod" 1 1 [tok "Now" [], tok "these" []]
    ]

unsignedPatch :: Patch
unsignedPatch = Patch
    { pTokVersion = "test-tok1"
    , pBook = "Gen", pChapter = 1, pVerse = 1
    , pSpan = (2, 2)
    , pOriginal = ["beginning"]
    , pReplacement = ["start"]
    , pNote = Nothing
    , pAuthor = "ab", pCreated = "2026-01-01T00:00:00Z", pSig = ""
    }

mkLoaded :: FilePath -> Text -> (Int, Int) -> [Text] -> [Text] -> PatchStatus -> LoadedPatch
mkLoaded f created span_ orig repl = LoadedPatch f
    unsignedPatch { pSpan = span_, pOriginal = orig, pReplacement = repl
                  , pCreated = created }

unsignedRule :: Rule
unsignedRule = Rule
    { rTokVersion = "test-tok1"
    , rMatch = ["the"]
    , rReplacement = ["thee"]
    , rExclude = [("Gen", 1, 2)]
    , rNote = Nothing
    , rAuthor = "ab", rCreated = "2026-01-01T00:00:00Z", rSig = ""
    }

mkLR :: FilePath -> [Text] -> [Text] -> [(Text, Int, Int)] -> PatchStatus -> LoadedRule
mkLR f match repl excl st = LoadedRule f
    unsignedRule { rMatch = match, rReplacement = repl, rExclude = excl }
    st 0

-- ── tests ───────────────────────────────────────────────────────────────────

main :: IO ()
main = hspec $ do
    describe "tokenizer" $ do
        it "extracts words with normalized Strong's refs" $ do
            let (ts, _) = tokenize
                    "In the <w lemma=\"strong:H7225\">beginning</w> \
                    \<w lemma=\"strong:H0430\">God</w> created."
            map tokWord ts `shouldBe` ["In", "the", "beginning", "God", "created"]
            tokStrongs (ts !! 2) `shouldBe` ["H7225"]
            -- zero padding stripped to match the dictionary key style
            tokStrongs (ts !! 3) `shouldBe` ["H430"]
            tokPost (last ts) `shouldBe` "."

        it "flags translator italics" $ do
            let (ts, _) = tokenize
                    "there <transChange type=\"added\">it was</transChange> light"
            map (hasFlag flagAdded) ts `shouldBe` [False, True, True, False]

        it "flags the divine name through nested tags" $ do
            let (ts, _) = tokenize
                    "<divineName><w lemma=\"strong:H3068\">LORD</w></divineName>,"
            map tokWord ts `shouldBe` ["LORD"]
            hasFlag flagDivine (head ts) `shouldBe` True
            tokStrongs (head ts) `shouldBe` ["H3068"]
            tokPost (head ts) `shouldBe` ","

        it "flags superscription tokens" $ do
            let (ts, _) = tokenize
                    "<title canonical=\"true\" type=\"psalm\">A \
                    \<w lemma=\"strong:H4210\">Psalm</w></title> body"
            map (hasFlag flagTitle) ts `shouldBe` [True, True, False]

        it "turns pilcrows into a flag on the next word" $ do
            let (ts, _) = tokenize "light. \x00B6 And God said"
            map tokWord ts `shouldBe` ["light", "And", "God", "said"]
            map (hasFlag flagPara) ts `shouldBe` [False, True, False, False]
            T.isInfixOf "\x00B6" (T.unwords (map renderToken ts))
                `shouldBe` False

        it "extracts margin notes out of the token stream" $ do
            let (ts, notes) = tokenize
                    "light<note placement=\"foot\"><reference \
                    \type=\"annotateRef\">1.4 </reference>Heb. between</note> from"
            map tokWord ts `shouldBe` ["light", "from"]
            notes `shouldBe` ["1.4 Heb. between"]

        it "splits leading and trailing punctuation" $ do
            let (ts, _) = tokenize "(Selah."
            tokPre (head ts) `shouldBe` "("
            tokWord (head ts) `shouldBe` "Selah"
            tokPost (head ts) `shouldBe` "."

        it "glues stray punctuation to the previous word" $ do
            let (ts, _) = tokenize "me !"
            map tokWord ts `shouldBe` ["me"]
            tokPost (head ts) `shouldBe` "!"

    describe "imp block parsing" $ do
        it "accepts real verse headers" $ do
            let r = parseBlock ("Genesis 1:1", ["In the beginning"])
            fmap (vBook . fst) r `shouldBe` Just "Gen"
            fmap (\(v, _) -> (vChapter v, vVerse v)) r `shouldBe` Just (1, 1)

        it "maps Roman-numeral book names" $
            fmap (vBook . fst) (parseBlock ("I Samuel 3:4", ["x"]))
                `shouldBe` Just "1Sam"

        it "rejects verse zero, headings, unknown books" $ do
            parseBlock ("Genesis 0:0", ["x"]) `shouldBe` Nothing
            parseBlock ("[ Module Heading ]", ["x"]) `shouldBe` Nothing
            parseBlock ("Enoch 1:1", ["x"]) `shouldBe` Nothing

    describe "corpus" $ do
        it "JSON roundtrips verses" $ do
            let v = Verse "Gen" 1 1
                    [Token "(" "Selah" "." ["H5542"] (flagAdded + flagPara)]
            decode (encode v) `shouldBe` Just v

        it "indexes chapters and verse ranges" $ do
            chapterCount testCorpus "Gen" `shouldBe` 2
            chapterCount testCorpus "Exod" `shouldBe` 1
            map vVerse (chapterVerses testCorpus "Gen" 1) `shouldBe` [1, 2]
            map vBook (chapterVerses testCorpus "Exod" 1) `shouldBe` ["Exod"]

        it "separates body from superscription" $ do
            let v = Verse "Ps" 3 1
                    [ Token "" "A" "" [] flagTitle
                    , Token "" "Psalm" "" [] flagTitle
                    , Token "" "LORD" "," [] flagDivine
                    ]
            verseTitle v `shouldBe` "A Psalm"
            verseBody v `shouldBe` "LORD,"

    describe "patch signing" $ do
        it "signs and verifies, and rejects tampering" $ do
            keys <- generateKeys
            p <- mkPatch keys "test-tok1" ("Gen", 1, 1) (2, 2)
                ["beginning"] ["start"] (Just "why not")
            verifySig p `shouldBe` Right ()
            verifySig p { pReplacement = ["end"] } `shouldSatisfy` isLeft
            verifySig p { pNote = Nothing } `shouldSatisfy` isLeft

        it "signing bytes are frozen (golden)" $
            signingBytes unsignedPatch `shouldBe` TE.encodeUtf8
                "overlay-patch-v1\n\
                \tokenization:test-tok1\n\
                \target:Gen 1:1\n\
                \span:2 2\n\
                \original:beginning\n\
                \replacement:start\n\
                \note:\n\
                \author:ab\n\
                \created:2026-01-01T00:00:00Z"

        it "JSON roundtrips patches" $ do
            keys <- generateKeys
            p <- mkPatch keys "t" ("Gen", 1, 1) (0, 1) ["a", "b"] ["c"] Nothing
            decode (encode p) `shouldBe` Just p

    describe "patch text checks" $ do
        it "accepts a matching span" $
            checkAgainstText testCorpus unsignedPatch `shouldBe` Right ()

        it "rejects original-text mismatch" $
            checkAgainstText testCorpus
                unsignedPatch { pOriginal = ["ending"] }
                `shouldSatisfy` isLeft

        it "rejects tokenization mismatch" $
            checkAgainstText testCorpus
                unsignedPatch { pTokVersion = "other-tok" }
                `shouldSatisfy` isLeft

        it "rejects out-of-range spans" $
            checkAgainstText testCorpus
                unsignedPatch { pSpan = (2, 9), pOriginal = ["beginning"] }
                `shouldSatisfy` isLeft

    describe "overlap resolution" $ do
        it "keeps the earliest patch, invalidates overlaps" $ do
            let a = mkLoaded "a.json" "2026-01-01T00:00:00Z" (1, 2)
                    ["the", "beginning"] ["x"] POwn
                b = mkLoaded "b.json" "2026-01-02T00:00:00Z" (2, 2)
                    ["beginning"] ["y"] POwn
                out = resolveOverlaps [b, a]
                statusOfFile f = lpStatus (head (filter ((== f) . lpFile) out))
            statusOfFile "a.json" `shouldBe` POwn
            statusOfFile "b.json"
                `shouldBe` PInvalid "overlaps an earlier patch"

        it "non-overlapping patches both apply" $ do
            let a = mkLoaded "a.json" "2026-01-01T00:00:00Z" (0, 0)
                    ["In"] ["x"] POwn
                b = mkLoaded "b.json" "2026-01-02T00:00:00Z" (2, 2)
                    ["beginning"] ["y"] POwn
            map lpStatus (resolveOverlaps [a, b]) `shouldBe` [POwn, POwn]
            length (acceptedForVerse (resolveOverlaps [a, b]) ("Gen", 1, 1))
                `shouldBe` 2

    describe "patch overlay composition (toRVerse)" $ do
        let verse = Verse "Gen" 47 28
                [ tok "was" []
                , tokP "fourscore" "," ["H8084"]
                , tok "years" ["H8141"]
                ]
        it "replaces the span and inherits punctuation and Strong's" $ do
            let lp = LoadedPatch "p.json" unsignedPatch
                    { pBook = "Gen", pChapter = 47, pVerse = 28
                    , pSpan = (1, 1)
                    , pOriginal = ["fourscore"], pReplacement = ["eighty"]
                    } POwn
                rv = toRVerse "me" [lp] [] [] [] verse
                ws = rvTokens rv
            map (tokWord . rtTok) ws `shouldBe` ["was", "eighty", "years"]
            tokPost (rtTok (ws !! 1)) `shouldBe` ","
            tokStrongs (rtTok (ws !! 1)) `shouldBe` ["H8084"]
            map (void . rtPatch) ws
                `shouldBe` [Nothing, Just (), Nothing]
            -- canonical indices survive around the patch
            map rtIx ws `shouldBe` [0, 1, 2]

        it "multi-word replacement spans punctuation ends" $ do
            let lp = LoadedPatch "p.json" unsignedPatch
                    { pBook = "Gen", pChapter = 47, pVerse = 28
                    , pSpan = (1, 2)
                    , pOriginal = ["fourscore", "years"]
                    , pReplacement = ["eighty", "long", "years"]
                    } POwn
                rv = toRVerse "me" [lp] [] [] [] verse
                ws = rvTokens rv
            map (tokWord . rtTok) ws `shouldBe` ["was", "eighty", "long", "years"]
            -- punctuation of the original span ends up on the edges
            tokPost (rtTok (last ws)) `shouldBe` ""
            tokStrongs (rtTok (ws !! 1)) `shouldBe` ["H8084", "H8141"]

        it "marks thread spans, leaving other words unmarked" $ do
            let rv = toRVerse "me" [] [] [(1, 2)] [] verse
            map rtMark (rvTokens rv) `shouldBe` [False, True, True]

    describe "ref keys" $ do
        it "encodes and parses canonical refs" $ do
            refKey ("Gen", 1, 7) `shouldBe` "Gen 1:7"
            parseRefKey "Gen 1:7" `shouldBe` Just ("Gen", 1, 7)
            parseRefKey "1Sam 22:3" `shouldBe` Just ("1Sam", 22, 3)

        it "rejects malformed refs" $ do
            parseRefKey "Gen" `shouldBe` Nothing
            parseRefKey "Gen 1" `shouldBe` Nothing
            parseRefKey "Gen 1:x" `shouldBe` Nothing
            parseRefKey "Gen 1:2:3" `shouldBe` Nothing

    describe "rule signing" $ do
        it "signs and verifies, and rejects tampering" $ do
            keys <- generateKeys
            r <- mkRule keys "test-tok1" ["spake"] ["spoke"] (Just "modern")
            verifyRuleSig r `shouldBe` Right ()
            verifyRuleSig r { rReplacement = ["said"] } `shouldSatisfy` isLeft
            -- exclusions are part of the signed content
            verifyRuleSig r { rExclude = [("Gen", 1, 1)] }
                `shouldSatisfy` isLeft

        it "re-signing after an exclusion keeps created and verifies" $ do
            keys <- generateKeys
            r <- mkRule keys "test-tok1" ["spake"] ["spoke"] Nothing
            let r' = resignRule keys r { rExclude = [("Gen", 1, 2)] }
            rCreated r' `shouldBe` rCreated r
            verifyRuleSig r' `shouldBe` Right ()

        it "signing bytes are frozen (golden)" $
            ruleSigningBytes unsignedRule `shouldBe` TE.encodeUtf8
                "overlay-rule-v1\n\
                \tokenization:test-tok1\n\
                \match:the\n\
                \replacement:thee\n\
                \exclude:Gen 1:2\n\
                \note:\n\
                \author:ab\n\
                \created:2026-01-01T00:00:00Z"

        it "JSON roundtrips rules, exclusions included" $ do
            keys <- generateKeys
            r <- mkRule keys "t" ["a", "b"] ["c"] (Just "why")
            let r' = resignRule keys r { rExclude = [("Gen", 1, 2)] }
            decode (encode r) `shouldBe` Just r
            decode (encode r') `shouldBe` Just r'

    describe "rule text checks" $ do
        it "accepts a well-formed rule" $
            checkRule testCorpus unsignedRule `shouldBe` Right ()

        it "rejects tokenization mismatch" $
            checkRule testCorpus unsignedRule { rTokVersion = "other-tok" }
                `shouldSatisfy` isLeft

        it "rejects empty match and empty replacement" $ do
            checkRule testCorpus unsignedRule { rMatch = [] }
                `shouldSatisfy` isLeft
            checkRule testCorpus unsignedRule { rReplacement = [] }
                `shouldSatisfy` isLeft

        it "rejects exclusions pointing at missing verses" $
            checkRule testCorpus
                unsignedRule { rExclude = [("Gen", 9, 9)] }
                `shouldSatisfy` isLeft

    describe "rule matching" $ do
        it "finds all occurrences of a word" $
            matchSpans ["the"] [tok "the" [], tok "cat" [], tok "the" []]
                `shouldBe` [(0, 0), (2, 2)]

        it "finds multi-word sequences" $
            matchSpans ["the", "cat"]
                [tok "a" [], tok "the" [], tok "cat" []]
                `shouldBe` [(1, 2)]

        it "matches never overlap themselves" $
            matchSpans ["a", "a"] [tok "a" [], tok "a" [], tok "a" []]
                `shouldBe` [(0, 1)]

        it "counts matches corpus-wide minus exclusions" $ do
            -- "the" appears in Gen 1:1, Gen 1:2, Gen 2:1; Gen 1:2 is excluded
            countRuleMatches testCorpus unsignedRule `shouldBe` 2
            countRuleMatches testCorpus unsignedRule { rExclude = [] }
                `shouldBe` 3

        it "skips excluded verses" $ do
            let lr = mkLR "r.json" ["the"] ["thee"] [("Gen", 1, 1)] POwn
                v = Verse "Gen" 1 1 [tok "In" [], tok "the" [], tok "x" []]
            ruleHitsForVerse [lr] [] v `shouldBe` []

        it "yields to claimed spans (point patches win)" $ do
            let lr = mkLR "r.json" ["the"] ["thee"] [] POwn
                v = Verse "Gen" 1 1 [tok "In" [], tok "the" [], tok "x" []]
            ruleHitsForVerse [lr] [(1, 1)] v `shouldBe` []
            map snd (ruleHitsForVerse [lr] [(0, 0)] v) `shouldBe` [(1, 1)]

        it "earlier rule wins overlapping matches" $ do
            let a = mkLR "a.json" ["the", "beginning"] ["x"] [] POwn
                b = mkLR "b.json" ["the"] ["y"] [] POwn
                v = Verse "Gen" 1 1
                    [tok "In" [], tok "the" [], tok "beginning" []]
            map snd (ruleHitsForVerse [a, b] [] v) `shouldBe` [(1, 2)]

        it "never applies invalid rules" $ do
            let lr = mkLR "r.json" ["the"] ["thee"] [] (PInvalid "nope")
                v = Verse "Gen" 1 1 [tok "the" []]
            ruleHitsForVerse [lr] [] v `shouldBe` []

    describe "rule overlay composition (toRVerse)" $ do
        let verse = Verse "Gen" 47 28
                [ tok "was" []
                , tokP "fourscore" "," ["H8084"]
                , tok "years" ["H8141"]
                ]
        it "rewrites matches with punctuation and Strong's intact" $ do
            let lr = mkLR "r.json" ["fourscore"] ["eighty"] [] POwn
                ws = rvTokens (toRVerse "me" [] [lr] [] [] verse)
            map (tokWord . rtTok) ws `shouldBe` ["was", "eighty", "years"]
            tokPost (rtTok (ws !! 1)) `shouldBe` ","
            tokStrongs (rtTok (ws !! 1)) `shouldBe` ["H8084"]
            map (void . rtPatch) ws
                `shouldBe` [Nothing, Just (), Nothing]

        it "a point patch beats a rule on the same span" $ do
            let lp = LoadedPatch "p.json" unsignedPatch
                    { pBook = "Gen", pChapter = 47, pVerse = 28
                    , pSpan = (1, 1)
                    , pOriginal = ["fourscore"], pReplacement = ["eighty"]
                    } POwn
                lr = mkLR "r.json" ["fourscore"] ["XXX"] [] POwn
                ws = rvTokens (toRVerse "me" [lp] [lr] [] [] verse)
            map (tokWord . rtTok) ws `shouldBe` ["was", "eighty", "years"]

    describe "threads" $ do
        it "JSON roundtrips threads with entries" $ do
            let e = ThreadEntry ("Gen", 3, 15) (9, 11) ["her", "seed"]
                    (Just "first promise") "2026-06-10T00:00:00Z"
                t = Thread "Christ throughout the Bible" "test-tok1"
                    "running notes here" [e] "2026-06-10T00:00:00Z"
            decode (encode t) `shouldBe` Just t

        it "derives stable slug filenames" $ do
            threadFileFor "Christ throughout the Bible"
                `shouldBe` ("threads" </> "christ-throughout-the-bible.json")
            threadFileFor "  Hope! (2nd pass)  "
                `shouldBe` ("threads" </> "hope-2nd-pass.json")
            threadFileFor "???" `shouldBe` ("threads" </> "thread.json")

    describe "weaves" $ do
        let sampleWeave = Weave "Test" Retelling "test-tok1" "notes"
                "2026-06-14T00:00:00Z"
                [canonLink ("Exod", 20, 4) ("Deut", 5, 8)]
                False ""
            emptyW = emptyWeave "x" Retelling "t" "now"

        it "JSON roundtrips weaves with links" $
            decode (encode sampleWeave) `shouldBe` Just sampleWeave

        it "weaves default to unapproved and round-trip approval + tension" $ do
            wApproved emptyW `shouldBe` False
            wTension emptyW `shouldBe` ""
            let w = emptyW { wApproved = True
                           , wTension = "Satan provoked / the LORD moved" }
            decode (encode w) `shouldBe` Just w

        it "JSON roundtrips a labelled link and keeps its label" $ do
            let lbl = canonLinkL ("1Chr", 11, 11) ("2Sam", 23, 8) "Jashobeam"
                w = addLinks [lbl] emptyW
            decode (encode w) `shouldBe` Just w
            map lLabel (wLinks w) `shouldBe` ["Jashobeam"]

        it "omits the label field when empty (older files stay clean)" $
            T.isInfixOf "label" (T.pack (BLC.unpack (encode sampleWeave)))
                `shouldBe` False

        it "two labels between the same verse pair are distinct edges" $ do
            let w = addLinks
                    [ canonLinkL ("1Chr", 11, 11) ("2Sam", 23, 8) "Jashobeam"
                    , canonLinkL ("1Chr", 11, 11) ("2Sam", 23, 8) "the Tachmonite"
                    ] emptyW
            length (wLinks w) `shouldBe` 2
            -- still one graph component: same two verses
            length (components (wLinks w)) `shouldBe` 1

        it "an empty label canonicalises identically to canonLink" $
            canonLinkL ("Deut", 5, 8) ("Exod", 20, 4) ""
                `shouldBe` canonLink ("Exod", 20, 4) ("Deut", 5, 8)

        it "round-trips every kind token" $
            map (parseKind . kindToken) allKinds `shouldBe` map Just allKinds

        it "canonicalises link endpoints into reading order" $ do
            canonLink ("Deut", 5, 8) ("Exod", 20, 4)
                `shouldBe` canonLink ("Exod", 20, 4) ("Deut", 5, 8)
            -- Exodus precedes Deuteronomy in canon order
            lA (canonLink ("Deut", 5, 8) ("Exod", 20, 4))
                `shouldBe` ("Exod", 20, 4)

        it "addLinks dedups regardless of endpoint order" $ do
            let w = addLinks [ canonLink ("Exod", 20, 4) ("Deut", 5, 8)
                             , canonLink ("Deut", 5, 8) ("Exod", 20, 4) ]
                        sampleWeave
            length (wLinks w) `shouldBe` 1

        it "combine is the transitive merge (A–B + B–C share a component)" $ do
            let ab = addLinks [canonLink ("Matt", 1, 1) ("Mark", 1, 1)] emptyW
                bc = addLinks [canonLink ("Mark", 1, 1) ("Luke", 1, 1)] emptyW
                comps = components (wLinks (combine ab bc))
            length comps `shouldBe` 1
            length (head comps) `shouldBe` 3

        it "smartLinks zips two equal-length selections 1:1" $
            smartLinks [ [("Exod", 20, 4), ("Exod", 20, 5)]
                       , [("Deut", 5, 8), ("Deut", 5, 9)] ]
                `shouldBe` [ canonLink ("Exod", 20, 4) ("Deut", 5, 8)
                           , canonLink ("Exod", 20, 5) ("Deut", 5, 9) ]

        it "smartLinks connects all-to-all on a count mismatch" $
            length (smartLinks [ [("Exod", 20, 4), ("Exod", 20, 5)]
                               , [("Deut", 5, 8)] ])
                `shouldBe` 2

        it "derives stable slug filenames" $
            weaveFileFor "The Ten Commandments, twice"
                `shouldBe` ("weaves" </> "the-ten-commandments-twice.json")

    describe "concordance index" $ do
        it "collects occurrences in canonical order" $ do
            let ix = occurrenceIndex testCorpus
            M.lookup "H7225" ix `shouldBe` Just [("Gen", 1, 1)]
            M.lookup "H776" ix `shouldBe` Just [("Gen", 1, 2)]
            refLabel ("Gen", 1, 2) `shouldBe` "Genesis 1:2"

    describe "canon" $ do
        it "holds all 66 books in canonical order" $ do
            length books `shouldBe` 66
            head bookIds `shouldBe` "Gen"
            bookIds !! 39 `shouldBe` "Matt"  -- first NT book
            last bookIds `shouldBe` "Rev"

        it "looks up books by OSIS id and by imp name" $ do
            fmap bookName (M.lookup "Ps" bookById) `shouldBe` Just "Psalms"
            fmap bookId (M.lookup "I Samuel" bookByImpName)
                `shouldBe` Just "1Sam"
            fmap bookName (M.lookup "Rev" bookById) `shouldBe` Just "Revelation"
            M.lookup "Enoch" bookById `shouldBe` Nothing

    describe "corpus header and rendering" $ do
        it "renders a token sequence with single spaces" $
            renderTokens [tokP "In" "" [], tokP "Selah" "." []]
                `shouldBe` "In Selah."

        it "freezes the canonical-corpus header format (golden)" $
            corpusHeader "kjv1769-tok2" 7 `shouldBe` (object
                [ "format" .= ("overlay-kjv-canonical" :: Text)
                , "tokenization" .= ("kjv1769-tok2" :: Text)
                , "source" .=
                    ("engKJV2006eb 14.3 (CrossWire/eBible.org, public domain)"
                        :: Text)
                , "verses" .= (7 :: Int)
                ] :: Value)

    describe "imp block splitting" $ do
        it "drops preamble and groups headers with their body lines" $
            splitBlocks
                [ "ignored preamble"
                , "$$$Genesis 1:1", "In the beginning"
                , "$$$Genesis 1:2", "And the earth", "was without form"
                ]
                `shouldBe`
                [ ("Genesis 1:1", ["In the beginning"])
                , ("Genesis 1:2", ["And the earth", "was without form"])
                ]

        it "yields nothing when there is no header" $
            splitBlocks ["just", "loose", "lines"] `shouldBe` []

    describe "crypto primitives" $ do
        it "fingerprints to the first eight characters" $
            fingerprint "cf475b20edbecf1d" `shouldBe` "cf475b20"

        it "signs and verifies raw bytes, rejecting tampering" $ do
            keys <- generateKeys
            let msg = TE.encodeUtf8 "the quick brown fox"
                sig = signBytes keys msg
            verifyBytes (kPubHex keys) sig msg `shouldBe` Right ()
            verifyBytes (kPubHex keys) sig (TE.encodeUtf8 "tampered")
                `shouldSatisfy` isLeft

        it "rejects malformed author/signature hex" $ do
            keys <- generateKeys
            let msg = TE.encodeUtf8 "x"
            verifyBytes "not-hex" (signBytes keys msg) msg
                `shouldSatisfy` isLeft

        it "selfTest confirms the local signing path round-trips" $ do
            keys <- generateKeys
            selfTest keys "test-tok1" >>= (`shouldBe` Right ())

    describe "strongs entry codec" $ do
        it "parses a full dictionary entry" $
            decode "{\"lemma\":\"\\u03b8\",\"xlit\":\"theos\",\
                   \\"pron\":\"theh'-os\",\"derivation\":\"of uncertain\",\
                   \\"strongs_def\":\"a deity\",\"kjv_def\":\"God, god\"}"
                `shouldBe` Just (StrongsEntry (Just "\x03b8") (Just "theos")
                    (Just "theh'-os") (Just "of uncertain") (Just "a deity")
                    (Just "God, god"))

        it "fills absent fields with Nothing" $
            decode "{\"lemma\":\"only\"}"
                `shouldBe` Just (StrongsEntry (Just "only") Nothing Nothing
                    Nothing Nothing Nothing)

    describe "weave graph ops" $ do
        let emptyW = emptyWeave "x" Retelling "t" "now"
            mark = canonLink ("Exod", 20, 4) ("Deut", 5, 8)
            harmony = addLinks
                [ canonLink ("Matt", 1, 1) ("Mark", 1, 1)
                , canonLink ("Mark", 1, 1) ("Luke", 1, 1) ] emptyW

        it "removeLink deletes exactly the named edge" $ do
            let w = addLinks [mark] emptyW
            wLinks (removeLink mark w) `shouldBe` []

        it "componentOf returns the transitive group, self for an island" $ do
            length (componentOf (wLinks harmony) ("Luke", 1, 1)) `shouldBe` 3
            componentOf (wLinks harmony) ("John", 1, 1)
                `shouldBe` [("John", 1, 1)]

        it "linksTouching keeps edges with an on-screen endpoint" $ do
            let w = addLinks [mark] harmony
            linksTouching (Set.fromList [("Exod", 20, 4)]) w `shouldBe` [mark]
            linksTouching (Set.fromList [("Gen", 1, 1)]) w `shouldBe` []

        it "labels kinds and rejects unknown kind tokens" $ do
            kindLabel Prophecy `shouldBe` "prophecy & fulfillment"
            kindLabel Retelling `shouldBe` "retelling"
            parseKind "nonsense" `shouldBe` Nothing

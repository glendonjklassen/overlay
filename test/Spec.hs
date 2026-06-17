{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value, decode, encode, object, (.=))
import Data.Either (isLeft)
import Data.Functor (void)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector as V
import System.FilePath ((</>))
import Test.Hspec

import Overlay (toRVerse)
import Overlay.Bridge
import Overlay.Canon
    (Book (..), books, bookById, bookByImpName, bookIds)
import Overlay.Concept
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

-- A fixture for the concept engine: a 5-lemma Hebrew run shared by Exod 20:4 and
-- Deut 5:8 (a Decalogue-style parallel, with an untagged word in Deut to prove
-- the function-word drop), H6213 recurring across three OT books, a hapax
-- (H9999), and a Greek (NT) verse so the testament split has something on each
-- side. OT carries H-numbers, NT carries G-numbers, so the two never run together.
conceptCorpus :: Corpus
conceptCorpus = mkCorpus "test-tok1" $ V.fromList
    [ Verse "Exod" 20 4
        [ tok "make" ["H6213"], tok "thee" ["H6459"], tok "any" ["H3605"]
        , tok "graven" ["H5566"], tok "image" ["H8544"] ]
    , Verse "Deut" 5 8
        [ tok "make" ["H6213"], tok "to" [], tok "thee" ["H6459"]
        , tok "any" ["H3605"], tok "graven" ["H5566"], tok "image" ["H8544"] ]
    , Verse "Gen" 1 1 [tok "God" ["H430"], tok "made" ["H6213"], tok "alone" ["H9999"]]
    , Verse "John" 3 16 [tok "loved" ["G25"], tok "God" ["G2316"], tok "world" ["G2889"]]
    ]

-- Fixtures for the OT↔NT bridge. The dictionary exercises the etymology layer
-- (one Greek entry cites a Hebrew origin, one does not). The corpus exercises
-- the rendering layer: "love" bridges H157↔G26 distinctively; the generic word
-- "things" bridges several pairs but scores lower; "god" is too short to count;
-- "earth" never appears with a Greek lemma so yields no candidate.
bridgeDict :: M.Map Text StrongsEntry
bridgeDict = M.fromList
    [ ("G10", StrongsEntry (Just "A") Nothing Nothing
        (Just "of Hebrew origin (H0031);") Nothing (Just "Abiud"))
    , ("G26", StrongsEntry (Just "a") Nothing Nothing
        (Just "from G25;") Nothing (Just "love"))
    , ("H31", StrongsEntry (Just "x") Nothing Nothing Nothing Nothing (Just "Abihud"))
    ]

bridgeCorpus :: Corpus
bridgeCorpus = mkCorpus "test-tok1" $ V.fromList
    [ Verse "Gen" 1 1 [tok "love" ["H157"]]
    , Verse "John" 3 16 [tok "love" ["G26"]]
    , Verse "Exod" 1 1 [tok "things" ["H1"], tok "things" ["H3"]]
    , Verse "Acts" 1 1 [tok "things" ["G2"], tok "things" ["G4"]]
    , Verse "Gen" 1 3 [tok "God" ["H430"]]   -- "god" is 3 letters, filtered
    , Verse "John" 1 1 [tok "God" ["G2316"]]
    , Verse "Gen" 1 2 [tok "earth" ["H776"]] -- OT-only, no Greek partner
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
                rv = toRVerse "me" [lp] [] [] [] 0 verse
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
                rv = toRVerse "me" [lp] [] [] [] 0 verse
                ws = rvTokens rv
            map (tokWord . rtTok) ws `shouldBe` ["was", "eighty", "long", "years"]
            -- punctuation of the original span ends up on the edges
            tokPost (rtTok (last ws)) `shouldBe` ""
            tokStrongs (rtTok (ws !! 1)) `shouldBe` ["H8084", "H8141"]

        it "marks thread spans, leaving other words unmarked" $ do
            let rv = toRVerse "me" [] [] [(1, 2)] [] 0 verse
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
                ws = rvTokens (toRVerse "me" [] [lr] [] [] 0 verse)
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
                ws = rvTokens (toRVerse "me" [lp] [lr] [] [] 0 verse)
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
                False
            emptyW = emptyWeave "x" Retelling "t" "now"

        it "JSON roundtrips weaves with links" $
            decode (encode sampleWeave) `shouldBe` Just sampleWeave

        it "weaves default to unapproved and round-trip approval" $ do
            wApproved emptyW `shouldBe` False
            let w = emptyW { wApproved = True }
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

    describe "concept engine" $ do
        let cix = buildConceptIx conceptCorpus

        it "counts token instances per concept and per book" $ do
            -- H6213 appears once each in Exod, Deut, Gen
            fmap csTotal (conceptStat cix "H6213") `shouldBe` Just 3
            fmap csByBook (conceptStat cix "H6213")
                `shouldBe` Just (M.fromList [("Exod", 1), ("Deut", 1), ("Gen", 1)])
            -- a token tagged with one Strong's in two verses
            fmap csTotal (conceptStat cix "H6459") `shouldBe` Just 2
            conceptStat cix "H0000" `shouldBe` Nothing

        it "splits occurrences across the testaments" $ do
            -- H6213 is Old Testament only; G2316 is New Testament only
            fmap testamentSplit (conceptStat cix "H6213") `shouldBe` Just (3, 0)
            fmap testamentSplit (conceptStat cix "G2316") `shouldBe` Just (0, 1)

        it "ranks the books a concept occurs in" $
            fmap (map fst . topBooks 2) (conceptStat cix "H6213")
                `shouldBe` Just ["Deut", "Exod"]  -- alphabetical tie-break at count 1

        it "classifies rarity by total occurrences" $ do
            rarityTier (ConceptStat 1 mempty) `shouldBe` Hapax
            rarityTier (ConceptStat 3 mempty) `shouldBe` Rare 3
            rarityTier (ConceptStat 9 mempty) `shouldBe` Common
            -- H9999 occurs exactly once across the corpus
            "H9999" `elem` hapaxes cix `shouldBe` True
            "H6213" `elem` hapaxes cix `shouldBe` False

        it "counts co-occurring lemma pairs within a verse" $ do
            let co = coOccurrence conceptCorpus
            -- H6213+H6459 share Exod 20:4 and Deut 5:8
            M.lookup ("H6213", "H6459") co `shouldBe` Just 2
            -- H430+H6213 share only Gen 1:1; pair key is in ascending order
            M.lookup ("H430", "H6213") co `shouldBe` Just 1
            -- a cross-verse, never-together pair is absent
            M.lookup ("H8544", "G2316") co `shouldBe` Nothing

        it "detects a within-language shared-lemma run, dropping function words" $ do
            let cands = quotationCandidates defaultMinRun conceptCorpus
            length cands `shouldBe` 1
            let c = head cands
            (qcA c, qcB c) `shouldBe` (("Exod", 20, 4), ("Deut", 5, 8))
            qcLen c `shouldBe` 5
            qcRun c `shouldBe` ["H6213", "H6459", "H3605", "H5566", "H8544"]

        it "never runs a lemma sequence across the Hebrew/Greek divide" $
            -- the Greek verse shares no Strong's numbers with any Hebrew verse
            all (\c -> qcA c /= ("John", 3, 16) && qcB c /= ("John", 3, 16))
                (quotationCandidates 1 conceptCorpus) `shouldBe` True

    describe "OT↔NT bridge" $ do
        it "extracts and normalises Hebrew refs from free text" $ do
            hebRefsIn "of Hebrew origin (H0031);" `shouldBe` ["H31"]
            hebRefsIn "compare H1234 and H56" `shouldBe` ["H1234", "H56"]
            hebRefsIn "Hebrew" `shouldBe` []   -- no digits, no false match

        it "links a Greek entry to its Strong's-cited Hebrew origin only" $
            etymologyLinks bridgeDict `shouldBe` [BridgeLink "H31" "G10"]

        it "proposes rendering candidates from shared 1769 words" $ do
            let cs = renderingCandidates bridgeCorpus
                pairOf c = (rcHeb c, rcGrk c)
            -- "love" bridges H157↔G26 distinctively
            map pairOf cs `shouldContain` [("H157", "G26")]
            -- and it is the strongest candidate (fewest lemmas share the word)
            pairOf (head cs) `shouldBe` ("H157", "G26")
            rcWord (head cs) `shouldBe` "love"

        it "scores distinctive renderings above generic ones" $ do
            let cs = renderingCandidates bridgeCorpus
                score p = rcScore <$> find ((== p) . \c -> (rcHeb c, rcGrk c)) cs
            -- "love" (1 H + 1 G) beats "things" (2 H + 2 G)
            score ("H157", "G26") `shouldSatisfy` (> score ("H1", "G2"))

        it "skips short words and lemmas with no cross-testament partner" $ do
            let cs = renderingCandidates bridgeCorpus
                heb = map rcHeb cs
            -- "god" (3 letters) is filtered, so H430 never bridges
            heb `shouldNotContain` ["H430"]
            -- "earth" only ever appears with a Hebrew lemma, so H776 has no partner
            heb `shouldNotContain` ["H776"]

        it "resolves a bidirectional bridge from etymology + approvals" $ do
            let store = approveLink ("H157", "G26") emptyStore
                br = buildBridge bridgeDict store
            -- etymology link, both directions
            bridgedPartners br "H31" `shouldBe` ["G10"]
            bridgedPartners br "G10" `shouldBe` ["H31"]
            -- approved rendering link, both directions
            bridgedPartners br "H157" `shouldBe` ["G26"]
            bridgedPartners br "G26" `shouldBe` ["H157"]

        it "honours rejection over an approval" $ do
            let store = rejectLink ("H31", "G10") (approveLink ("H31", "G10") emptyStore)
                br = buildBridge bridgeDict store
            bridgedPartners br "H31" `shouldBe` []

        it "round-trips the approval store through JSON" $ do
            let store = rejectLink ("H1", "G2")
                    (approveLink ("H157", "G26") emptyStore)
            decode (encode store) `shouldBe` Just store

        it "spans per-book counts across bridged partners" $ do
            let cix = M.fromList
                    [ ("H6664", ConceptStat 5 (M.singleton "Ps" 5))
                    , ("G1343", ConceptStat 3 (M.singleton "Rom" 3)) ]
                br = buildBridge bridgeDict (approveLink ("H6664", "G1343") emptyStore)
            spannedByBook br cix "H6664"
                `shouldBe` M.fromList [("Ps", 5), ("Rom", 3)]

        it "indexes external source links by both endpoints" $ do
            let ix = sourceLinkIndex
                    [ SourceLink "H1" "G2" "lxx"
                    , SourceLink "H1" "G3" "stepbible-tbesg" ]
            extraPartners ix "H1" `shouldBe` ["G2", "G3"]
            extraPartners ix "G2" `shouldBe` ["H1"]
            extraPartners ix "H9" `shouldBe` []

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

        it "approving every link approves the whole weave" $ do
            wApproved harmony `shouldBe` False
            let w = setAllApproval True harmony
            wApproved w `shouldBe` True
            approvedCount w `shouldBe` 2
            wApproved (setAllApproval False w) `shouldBe` False

        it "approves one link at a time; the weave flag follows the last one" $ do
            let ls = wLinks harmony
                w1 = setLinkApproval (head ls) True harmony
            approvedCount w1 `shouldBe` 1
            wApproved w1 `shouldBe` False                       -- not all yet
            let w2 = setLinkApproval (ls !! 1) True w1
            wApproved w2 `shouldBe` True                        -- now all approved

        it "link identity ignores approval (no duplicate edges, clean removal)" $ do
            let w = setAllApproval True (addLinks [mark] emptyW)
            -- re-adding the same edge unapproved must not duplicate it
            length (wLinks (addLinks [mark] w)) `shouldBe` 1
            -- removeLink matches regardless of approval state
            wLinks (removeLink mark w) `shouldBe` []

        it "per-link approval round-trips through JSON" $ do
            let w = setAllApproval True (addLinks [mark] emptyW)
            decode (encode w) `shouldBe` Just w
            (lApproved . head . wLinks <$> decode (encode w)) `shouldBe` Just True

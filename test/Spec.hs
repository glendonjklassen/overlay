{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (decode, encode)
import Data.Either (isLeft)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Test.Hspec

import Overlay (toRVerse)
import Overlay.Corpus
import Overlay.Import (parseBlock, tokenize)
import Overlay.Patch
import Overlay.ReaderView (RTok (..), RVerse (..))
import Overlay.Strongs (occurrenceIndex, refLabel)
import qualified Data.Map.Strict as M

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
                rv = toRVerse "me" [lp] [] verse
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
                rv = toRVerse "me" [lp] [] verse
                ws = rvTokens rv
            map (tokWord . rtTok) ws `shouldBe` ["was", "eighty", "long", "years"]
            -- punctuation of the original span ends up on the edges
            tokPost (rtTok (last ws)) `shouldBe` ""
            tokStrongs (rtTok (ws !! 1)) `shouldBe` ["H8084", "H8141"]

    describe "concordance index" $ do
        it "collects occurrences in canonical order" $ do
            let ix = occurrenceIndex testCorpus
            M.lookup "H7225" ix `shouldBe` Just [("Gen", 1, 1)]
            M.lookup "H776" ix `shouldBe` Just [("Gen", 1, 2)]
            refLabel ("Gen", 1, 2) `shouldBe` "Genesis 1:2"

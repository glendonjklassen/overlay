{-# LANGUAGE OverloadedStrings #-}

module Overlay.Render where

import Data.List (elemIndex, nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Overlay.Canon (bookIds)
import Overlay.Corpus
import Overlay.Patch
import Overlay.ReaderView
import Overlay.Rule
import Overlay.Weave

-- ── overlay composition ─────────────────────────────────────────────────────

-- | Compose the patch and rule overlays over a verse, producing renderable
-- tokens. Patches claim their spans first, so rules never override them.
-- @marks@ are word spans to highlight (thread passages / weave selection).
-- | Per-verse "witnesses": for each verse, the other verses linked to it by
-- any weave (with the edge label). The count is its degree in the union of all
-- weave graphs — the heatmap and the verse-level cross-reference list both read
-- from this.
type VRef3 = (Text, Int, Int)

witnessIndex :: [LoadedWeave] -> M.Map VRef3 [(VRef3, Text)]
witnessIndex weaves = M.map dedup $ M.fromListWith (<>) $ concat
    [ [ (lA l, [(lB l, lLabel l)]), (lB l, [(lA l, lLabel l)]) ]
    | lw <- weaves, l <- wLinks (lwWeave lw) ]
  where
    -- one entry per distinct neighbour verse, keeping a non-empty label if any
    dedup xs = M.toList (M.fromListWith pickLabel xs)
    pickLabel a b = if T.null a then b else a

-- | Sort verse references into canonical reading order (book, chapter, verse).
canonKey :: VRef3 -> (Int, Int, Int)
canonKey (b, c, v) = (fromMaybe maxBound (elemIndex b bookIds), c, v)

sortOnCanon :: [(VRef3, Text)] -> [(VRef3, Text)]
sortOnCanon = sortOn (canonKey . fst)

-- | Where a weave first lands in the canon — the earliest verse among all its
-- link endpoints — for ordering the weaves list. Linkless weaves sort last.
weaveStartKey :: Weave -> (Int, Int, Int)
weaveStartKey w = case concatMap (\l -> [lA l, lB l]) (wLinks w) of
    [] -> (maxBound, maxBound, maxBound)
    es -> minimum (map canonKey es)

-- | Heat tier 0…4 for a witness count, relative to the busiest verse so the
-- scale always spans the data on hand: 0 none, then four even bands.
heatTierFor :: Int -> Int -> Int
heatTierFor maxWit d
    | d <= 0 || maxWit <= 0 = 0
    | otherwise = max 1 (min 4 (ceiling (fromIntegral d / fromIntegral maxWit * 4 :: Double)))

toRVerse
    :: Text -> [LoadedPatch] -> [LoadedRule] -> [(Int, Int)] -> [Text]
    -> Int -> Verse -> RVerse
toRVerse ownHex patches rules marks notes heat v =
    RVerse (vVerse v) (walk 0 (vTokens v)) notes heat
  where
    ref = (vBook v, vChapter v, vVerse v)
    patchEdits =
        [ (pSpan p, pReplacement p, patchInfo ownHex lp)
        | lp <- acceptedForVerse patches ref
        , let p = lpPatch lp
        ]
    ruleEdits =
        [ (s, rReplacement (lrRule lr), ruleInfo ownHex lr)
        | (lr, s) <- ruleHitsForVerse rules
            [sp | (sp, _, _) <- patchEdits] v
        ]
    byStart = M.fromList
        [ (fst s, edit) | edit@(s, _, _) <- patchEdits <> ruleEdits ]

    marked i = any (\(s, e) -> i >= s && i <= e) marks

    walk _ [] = []
    walk i toks@(t : rest) = case M.lookup i byStart of
        Nothing -> RTok t ref i Nothing (marked i) : walk (i + 1) rest
        Just ((s, e), repl, info) ->
            let spanToks = take (e - s + 1) toks
                firstTok = head spanToks
                lastTok = last spanToks
                strongs = nub (concatMap tokStrongs spanToks)
                keepFlags = tokFlags firstTok
                    `div` flagTitle `mod` 2 * flagTitle
                    + tokFlags firstTok `div` flagPara `mod` 2 * flagPara
                n = length repl
                mark = any marked [s .. e]
                mkTok j w = Token
                    { tokPre = if j == 0 then tokPre firstTok else ""
                    , tokWord = w
                    , tokPost = if j == n - 1 then tokPost lastTok else ""
                    , tokStrongs = strongs
                    , tokFlags = keepFlags
                    }
            in [ RTok (mkTok j w) ref i (Just info) mark
               | (j, w) <- zip [0 ..] repl
               ]
               <> walk (e + 1) (drop (e - s + 1) toks)

patchInfo :: Text -> LoadedPatch -> PatchInfo
patchInfo ownHex lp = PatchInfo
    { piLines =
        [ statusLine
        , "was: " <> T.unwords (pOriginal p)
        , "by " <> author <> " · " <> T.take 10 (pCreated p)
        ]
        <> maybe [] (\n -> ["note: " <> n]) (pNote p)
    , piWarn = warn
    }
  where
    p = lpPatch lp
    warn = lpStatus lp == PUnknownKey
    author
        | pAuthor p == ownHex = "you (" <> fingerprint (pAuthor p) <> ")"
        | otherwise = fingerprint (pAuthor p) <> "…"
    statusLine = case lpStatus lp of
        POwn -> "patched · ed25519 verified"
        PTrusted -> "patched · ed25519 verified (trusted key)"
        PUnknownKey -> "patched · valid signature, UNKNOWN KEY"
        PInvalid r -> "INVALID: " <> r  -- not rendered; invalid never applies

ruleInfo :: Text -> LoadedRule -> PatchInfo
ruleInfo ownHex lr = PatchInfo
    { piLines =
        [ statusLine
        , "was: " <> T.unwords (rMatch r)
        , "by " <> author <> " · " <> T.take 10 (rCreated r)
        ]
        <> maybe [] (\n -> ["note: " <> n]) (rNote r)
    , piWarn = lrStatus lr == PUnknownKey
    }
  where
    r = lrRule lr
    author
        | rAuthor r == ownHex = "you (" <> fingerprint (rAuthor r) <> ")"
        | otherwise = fingerprint (rAuthor r) <> "…"
    statusLine = case lrStatus lr of
        POwn -> "rule · ed25519 verified"
        PTrusted -> "rule · ed25519 verified (trusted key)"
        PUnknownKey -> "rule · valid signature, UNKNOWN KEY"
        PInvalid reason -> "INVALID: " <> reason

statusText :: PatchStatus -> Text
statusText st = case st of
    POwn -> "verified (you)"
    PTrusted -> "verified (trusted)"
    PUnknownKey -> "valid sig, unknown key"
    PInvalid r -> "INVALID: " <> r

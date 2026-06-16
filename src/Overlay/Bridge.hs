{-# LANGUAGE OverloadedStrings #-}

-- | The Old↔New Testament concept bridge: links between Hebrew Strong's numbers
-- (@H…@, OT) and Greek ones (@G…@, NT), which otherwise share no numbering.
-- Without it the concept engine cannot follow a theme across the testaments.
--
-- Two era-faithful sources, both derived from data already in the repo — no
-- Septuagint, no modern critical lexicon, no embeddings:
--
--   * 'etymologyLinks' — Strong's own 1890 cross-references: Greek entries whose
--     derivation says \"of Hebrew origin (Hxxxx)\". Authoritative but narrow
--     (loanwords, proper nouns, cultic terms). Applied automatically.
--
--   * 'renderingCandidates' — the 1769 translators' own equivalences: a Hebrew
--     and a Greek word the KJV renders with the same English word. Reaches the
--     abstract vocabulary etymology misses, but is noisier, so candidates are
--     rarity-weighted (a distinctive shared rendering like \"propitiation\"
--     scores high; a generic one like \"set\" scores low) and are meant to be
--     reviewed/approved, not trusted blindly.
module Overlay.Bridge
    ( BridgeLink (..)
    , etymologyLinks
    , RenderCand (..)
    , renderingCandidates
    , hebRefsIn
    ) where

import Data.Char (isDigit, isLetter)
import Data.List (foldl', partition, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Overlay.Corpus
import Overlay.Strongs

-- | A Hebrew↔Greek concept link.
data BridgeLink = BridgeLink
    { blHeb :: !Text  -- ^ Hebrew Strong's number (H####)
    , blGrk :: !Text  -- ^ Greek Strong's number (G####)
    } deriving (Eq, Ord, Show)

-- ── etymology layer (Strong's 1890, automatic) ──────────────────────────────

-- | Links Strong himself recorded: every Greek entry whose derivation cites a
-- Hebrew origin. The Hebrew numbers are normalised to the dictionary's
-- zero-stripped style (@H0031@ → @H31@) so they match corpus tags.
etymologyLinks :: StrongsDict -> [BridgeLink]
etymologyLinks dict =
    [ BridgeLink h g
    | (g, e) <- M.toList dict
    , "G" `T.isPrefixOf` g
    , Just d <- [seDeriv e]
    , "ebrew" `T.isInfixOf` d        -- "of Hebrew origin"
    , h <- hebRefsIn d
    ]

-- | Extract Hebrew Strong's references (@H####@) embedded in free text, e.g. the
-- @(H0031)@ in a derivation, normalised to the zero-stripped dictionary style.
hebRefsIn :: Text -> [Text]
hebRefsIn = go . T.unpack
  where
    go [] = []
    go (c : cs)
        | c == 'H' || c == 'h' =
            let (ds, rest) = span isDigit cs
            in if null ds then go cs else norm ds : go rest
        | otherwise = go cs
    norm ds = T.pack ('H' : show (read ds :: Int))

-- ── rendering layer (1769 translators, reviewable candidates) ────────────────

-- | A candidate bridge link from a shared KJV rendering, with the distinctive
-- English word behind it and a rarity score (higher = the word maps to fewer
-- lemmas overall, so the equivalence is more specific and trustworthy).
data RenderCand = RenderCand
    { rcHeb   :: !Text
    , rcGrk   :: !Text
    , rcWord  :: !Text
    , rcScore :: !Double
    } deriving (Eq, Show)

-- | Candidate H↔G links drawn from the 1769 renderings: words the KJV uses over
-- both a Hebrew and a Greek lemma. Each pair keeps its single most distinctive
-- shared word, and the list is sorted strongest-first. Generic, low-content
-- words (under four letters, or not purely alphabetic) are skipped.
renderingCandidates :: Corpus -> [RenderCand]
renderingCandidates corpus =
    sortBy (comparing (Down . rcScore)) (M.elems byPair)
  where
    -- lowercased English word -> (Hebrew lemma set, Greek lemma set)
    wordMap :: Map Text (Set Text, Set Text)
    wordMap = V.foldl' addVerse M.empty (cVerses corpus)
    addVerse m v = foldl' addTok m (vTokens v)
    addTok m t =
        let w = T.toLower (tokWord t)
            (hs, gs) = partition ("H" `T.isPrefixOf`) (tokStrongs t)
        in if T.length w < 4 || not (T.all isLetter w) || (null hs && null gs)
            then m
            else M.insertWith mergeSets w (S.fromList hs, S.fromList gs) m
    mergeSets (h1, g1) (h2, g2) = (S.union h1 h2, S.union g1 g2)

    -- one candidate per (H, G) pair, keeping the most distinctive shared word
    byPair :: Map (Text, Text) RenderCand
    byPair = M.foldrWithKey addWord M.empty wordMap
    addWord w (hs, gs) acc
        | S.null hs || S.null gs = acc
        | otherwise =
            let wScore = 1 / fromIntegral (S.size hs + S.size gs)
            in foldl'
                (\a (h, g) -> M.insertWith stronger (h, g) (RenderCand h g w wScore) a)
                acc
                [ (h, g) | h <- S.toList hs, g <- S.toList gs ]
    stronger new old = if rcScore new > rcScore old then new else old

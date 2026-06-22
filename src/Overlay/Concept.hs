{-# LANGUAGE OverloadedStrings #-}

-- | The concept-analysis engine: derived, era-neutral statistics over the
-- Strong's-tagged corpus. A Strong's number is a controlled, original-language
-- concept vocabulary, so everything here counts and relates /lemmas/, never the
-- 1769 English surface — it cannot drift with modern English by construction.
--
-- The light per-concept stats ('ConceptIx') are cheap to build at startup (one
-- fold over the corpus). The heavier indices — the full co-occurrence matrix
-- and the within-language shared-lemma-run candidates — are precomputed offline
-- by the @overlay-analyze@ tool.
--
-- Note on testaments: Old Testament verses carry Hebrew Strong's numbers
-- (@H…@) and New Testament verses Greek (@G…@). The vocabularies are disjoint,
-- so shared-lemma-run detection is /within-language/ (synoptic parallels,
-- Kings/Chronicles retellings, Psalm doublets) — it never bridges an OT→NT
-- quotation, whose textual link lives in the cross-reference layer instead.
module Overlay.Concept
    ( -- * Per-concept statistics
      ConceptStat (..)
    , ConceptIx
    , buildConceptIx
    , conceptStat
      -- * Distribution and rarity
    , testamentSplit
    , topBooks
    , RarityTier (..)
    , rarityTier
    , hapaxes
      -- * Co-occurrence
    , coOccurrence
      -- * Within-language shared-lemma runs
    , QuoteCand (..)
    , quotationCandidates
    , defaultMinRun
      -- * Reviewable suggested parallels (from the cached candidates)
    , Suggestion (..)
    , loadSuggestions
    ) where

import Data.Aeson
import Data.List (foldl', sortBy, tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesFileExist)

import Overlay.Canon (bookIds)
import Overlay.Corpus

-- ── per-concept statistics ──────────────────────────────────────────────────

-- | Occurrence statistics for one Strong's number. Counts are over token
-- instances (every tagged word), so they answer \"how often\", and the
-- per-book breakdown answers \"where\".
data ConceptStat = ConceptStat
    { csTotal  :: !Int             -- ^ token instances corpus-wide
    , csByBook :: !(Map Text Int)  -- ^ token instances per OSIS book id
    } deriving (Eq, Show)

-- | Strong's number → its occurrence statistics.
type ConceptIx = Map Text ConceptStat

-- | Build the per-concept index in a single fold over the corpus. Cheap enough
-- to run at startup (~360k tagged tokens). A token tagged with several Strong's
-- numbers counts once for each.
buildConceptIx :: Corpus -> ConceptIx
buildConceptIx corpus = V.foldl' addVerse M.empty (cVerses corpus)
  where
    addVerse ix v =
        foldl' (addTok (vBook v)) ix (concatMap tokStrongs (vTokens v))
    addTok b ix s = M.insertWith mergeStat s (ConceptStat 1 (M.singleton b 1)) ix
    mergeStat (ConceptStat t1 m1) (ConceptStat t2 m2) =
        ConceptStat (t1 + t2) (M.unionWith (+) m1 m2)

conceptStat :: ConceptIx -> Text -> Maybe ConceptStat
conceptStat = flip M.lookup

-- ── distribution and rarity ─────────────────────────────────────────────────

-- | OSIS ids of the New Testament books (Matthew onward).
ntBookSet :: Set Text
ntBookSet = S.fromList (dropWhile (/= "Matt") bookIds)

-- | (Old Testament total, New Testament total) token instances.
testamentSplit :: ConceptStat -> (Int, Int)
testamentSplit = M.foldrWithKey step (0, 0) . csByBook
  where
    step b n (ot, nt)
        | b `S.member` ntBookSet = (ot, nt + n)
        | otherwise              = (ot + n, nt)

-- | The @n@ books where this concept occurs most, highest first. Returns OSIS
-- ids; callers map them to display names.
topBooks :: Int -> ConceptStat -> [(Text, Int)]
topBooks n = take n . sortBy (comparing (Down . snd)) . M.toList . csByBook

data RarityTier
    = Hapax       -- ^ occurs exactly once in the tagged corpus
    | Rare !Int   -- ^ occurs 2…5 times (the count)
    | Common      -- ^ occurs more than 5 times
    deriving (Eq, Show)

rarityTier :: ConceptStat -> RarityTier
rarityTier cs
    | csTotal cs <= 1 = Hapax
    | csTotal cs <= 5 = Rare (csTotal cs)
    | otherwise       = Common

-- | Every Strong's number occurring exactly once among the tagged words.
hapaxes :: ConceptIx -> [Text]
hapaxes = M.keys . M.filter ((== 1) . csTotal)

-- ── co-occurrence ───────────────────────────────────────────────────────────

-- | Unordered Strong's pairs that share a verse, with how many verses each pair
-- co-occurs in. Heavier than 'buildConceptIx' (precompute offline). The pair is
-- always (lo, hi) with lo < hi, so it has one canonical key.
coOccurrence :: Corpus -> Map (Text, Text) Int
coOccurrence corpus = V.foldl' addVerse M.empty (cVerses corpus)
  where
    addVerse m v =
        let present = S.toAscList (S.fromList (concatMap tokStrongs (vTokens v)))
            prs = [ (a, b) | (a : rest) <- tails present, b <- rest ]
        in foldl' (\acc p -> M.insertWith (+) p 1 acc) m prs

-- ── within-language shared-lemma runs ───────────────────────────────────────

-- | A candidate parallel: two verses sharing a contiguous run of Strong's
-- numbers, the longest such run, and its length.
data QuoteCand = QuoteCand
    { qcA   :: !(Text, Int, Int)
    , qcB   :: !(Text, Int, Int)
    , qcRun :: ![Text]            -- ^ the shared Strong's-number run
    , qcLen :: !Int               -- ^ length of the run
    } deriving (Eq, Show)

-- | Minimum run length to count as a candidate parallel.
defaultMinRun :: Int
defaultMinRun = 4

-- | A verse's \"lemma backbone\": its Strong's numbers in token order, taking
-- the first tag of each tagged token and dropping untagged (function) words.
backbone :: Verse -> [Text]
backbone = concatMap take1 . vTokens
  where
    take1 t = case tokStrongs t of
        (s : _) -> [s]
        []      -> []

-- | Within-language candidate parallels: verse pairs sharing a contiguous lemma
-- run of at least @minRun@. An inverted trigram index keeps this tractable —
-- only verse pairs that share a 3-lemma window are ever compared. Since OT
-- (Hebrew) and NT (Greek) Strong's numbers never coincide, every candidate is
-- naturally within one language. Results are sorted by run length, longest
-- first.
quotationCandidates :: Int -> Corpus -> [QuoteCand]
quotationCandidates minRun corpus =
    sortBy (comparing (Down . qcLen)) (M.elems best)
  where
    vs = cVerses corpus
    n  = V.length vs

    refOf i = let v = vs V.! i in (vBook v, vChapter v, vVerse v)
    boneOf  = V.generate n (backbone . (vs V.!))

    -- trigram → verse indices that contain it
    trigramIx :: Map (Text, Text, Text) [Int]
    trigramIx = V.ifoldl' addV M.empty boneOf
      where
        addV m i bone =
            foldl' (\acc g -> M.insertWith (<>) g [i] acc) m (trigrams bone)
    trigrams (a : b : c : rest) = (a, b, c) : trigrams (b : c : rest)
    trigrams _                  = []

    -- candidate verse-index pairs: any two verses sharing a trigram (i < j)
    candidatePairs :: Set (Int, Int)
    candidatePairs = M.foldr addGroup S.empty trigramIx
      where
        addGroup is acc =
            let sorted = S.toAscList (S.fromList is)
            in foldl' (\s (i, j) -> S.insert (i, j) s) acc
                   [ (i, j) | (i : rest) <- tails sorted, j <- rest ]

    -- the longest shared run for each candidate pair that clears the threshold,
    -- keyed by canonical ref pair so duplicates collapse to the best run
    best :: Map ((Text, Int, Int), (Text, Int, Int)) QuoteCand
    best = S.foldr consider M.empty candidatePairs
      where
        consider (i, j) acc =
            let run = longestRun (boneOf V.! i) (boneOf V.! j)
            in if length run < minRun
                then acc
                else let cand = QuoteCand (refOf i) (refOf j) run (length run)
                     in M.insertWith keepLonger (refOf i, refOf j) cand acc
        keepLonger new old = if qcLen new > qcLen old then new else old

-- | Longest common /contiguous/ run (substring) of two lemma lists. Verses are
-- short, so the straightforward suffix × common-prefix scan is both fast enough
-- and obviously correct.
longestRun :: Eq a => [a] -> [a] -> [a]
longestRun xs ys =
    foldl' longer [] [ commonPrefix sx sy | sx <- tails xs, sy <- tails ys ]
  where
    longer best r = if length r > length best then r else best
    commonPrefix (a : as) (b : bs) | a == b = a : commonPrefix as bs
    commonPrefix _ _ = []

-- ── reviewable suggested parallels ──────────────────────────────────────────

-- | A candidate within-language parallel surfaced for review: two verses, the
-- length of their shared lemma run, and a readable snippet of the shared words.
data Suggestion = Suggestion
    { sgA     :: !(Text, Int, Int)
    , sgB     :: !(Text, Int, Int)
    , sgLen   :: !Int
    , sgLabel :: !Text
    } deriving (Eq, Show)

-- one raw candidate as written into the concept cache by --analyze
data RawCand = RawCand !Text !Text ![Text] !Int

instance FromJSON RawCand where
    parseJSON = withObject "candidate" $ \o ->
        RawCand <$> o .: "a" <*> o .: "b" <*> o .: "run" <*> o .: "len"

newtype CandFile = CandFile [RawCand]
instance FromJSON CandFile where
    parseJSON = withObject "concept-cache" $ \o ->
        CandFile <$> o .:? "candidates" .!= []

-- | Load the cached shared-lemma-run candidates as 'Suggestion's, longest first.
-- Each gets a readable label: the shared words as they read in the first verse.
-- Returns none if the cache is absent (run @overlay --analyze@ to build it).
loadSuggestions :: Corpus -> Int -> FilePath -> IO [Suggestion]
loadSuggestions corpus topN path = do
    present <- doesFileExist path
    if not present
        then pure []
        else do
            decoded <- decodeFileStrict path
            let cands = maybe [] (\(CandFile cs) -> cs) decoded
            pure $ take topN
                [ s | RawCand a b run len <- cands
                    , Just ra <- [parseRefKey a], Just rb <- [parseRefKey b]
                    , differentChapter ra rb
                    , let s = Suggestion ra rb len (sharedText ra run) ]
  where
    -- a same-chapter pair (e.g. Matt 5:29/5:30) is an adjacent restatement, not
    -- an interesting parallel; require at least a chapter between the two verses
    differentChapter (ba, ca, _) (bb, cb, _) = ba /= bb || ca /= cb
    sharedText ref run =
        case M.lookup ref (cByRef corpus) >>= (cVerses corpus V.!?) of
            Nothing -> ""
            Just v  ->
                let runSet = S.fromList run
                    ws = [ tokWord t | t <- vTokens v
                         , any (`S.member` runSet) (tokStrongs t) ]
                in T.unwords (take 9 ws)

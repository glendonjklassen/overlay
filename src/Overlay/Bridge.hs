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
      -- * Persisted approvals and the resolved bridge
    , BridgeStore (..)
    , emptyStore
    , approveLink
    , rejectLink
    , loadApprovals
    , saveApprovals
    , bridgeFile
    , Bridge
    , etymologyBridge
    , applyStore
    , buildBridge
    , bridgedPartners
    , bridgeSize
    , spannedByBook
    , candidateIndex
    , crossPartners
    , unionByBook
      -- * External (hydrated) source links
    , SourceLink (..)
    , bridgeSourcesFile
    , loadBridgeSources
    , sourceLinkIndex
    , extraPartners
    , sourcePrior
    , sourceLabel
    ) where

import Data.Aeson
import Data.Char (isDigit, isLetter)
import Data.Either (fromRight)
import Data.List (foldl', nub, partition, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesFileExist)

import Overlay.Concept (ConceptIx, ConceptStat (..), conceptStat)
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

-- ── persisted approvals ─────────────────────────────────────────────────────

-- | The user's curation of the rendering candidates: which (H, G) pairs they
-- have blessed, and which they have struck out (so they don't resurface). Each
-- pair is canonical (Hebrew first). Etymology links are not stored — they are
-- always active — but an explicit rejection still overrides one.
data BridgeStore = BridgeStore
    { bsApproved :: !(Set (Text, Text))
    , bsRejected :: !(Set (Text, Text))
    } deriving (Eq, Show)

emptyStore :: BridgeStore
emptyStore = BridgeStore S.empty S.empty

approveLink :: (Text, Text) -> BridgeStore -> BridgeStore
approveLink p bs = bs
    { bsApproved = S.insert p (bsApproved bs)
    , bsRejected = S.delete p (bsRejected bs) }

rejectLink :: (Text, Text) -> BridgeStore -> BridgeStore
rejectLink p bs = bs
    { bsRejected = S.insert p (bsRejected bs)
    , bsApproved = S.delete p (bsApproved bs) }

instance ToJSON BridgeStore where
    toJSON bs = object
        [ "format"   .= ("overlay-bridge-v1" :: Text)
        , "approved" .= map asList (S.toList (bsApproved bs))
        , "rejected" .= map asList (S.toList (bsRejected bs)) ]
      where asList (h, g) = [h, g]

instance FromJSON BridgeStore where
    parseJSON = withObject "BridgeStore" $ \o -> do
        ap <- o .:? "approved" .!= []
        rj <- o .:? "rejected" .!= []
        pure (BridgeStore (asSet ap) (asSet rj))
      where
        asSet = S.fromList . mapMaybe asPair
        asPair [h, g] = Just (h, g)
        asPair _      = Nothing

-- | The committed file the approvals persist to.
bridgeFile :: FilePath
bridgeFile = "bridge.json"

-- | Load the approval store, or an empty one if the file is absent or corrupt
-- (corruption shouldn't wipe the user's text experience — the bridge just falls
-- back to etymology-only).
loadApprovals :: FilePath -> IO BridgeStore
loadApprovals path = do
    present <- doesFileExist path
    if not present
        then pure emptyStore
        else fromRight emptyStore <$> eitherDecodeFileStrict path

saveApprovals :: FilePath -> BridgeStore -> IO ()
saveApprovals = encodeFile

-- ── the resolved bridge ─────────────────────────────────────────────────────

-- | The active bridge: each Strong's number to its bridged partners across the
-- testaments, both directions, after combining automatic etymology links with
-- the user's approvals and honouring rejections.
newtype Bridge = Bridge (Map Text (Set Text))

insertBoth :: Text -> Text -> Map Text (Set Text) -> Map Text (Set Text)
insertBoth a b =
    M.insertWith S.union a (S.singleton b) . M.insertWith S.union b (S.singleton a)

deleteBoth :: Text -> Text -> Map Text (Set Text) -> Map Text (Set Text)
deleteBoth a b = M.adjust (S.delete b) a . M.adjust (S.delete a) b

-- | The automatic layer: only Strong's etymological cross-references.
etymologyBridge :: StrongsDict -> Bridge
etymologyBridge dict =
    Bridge (foldl' (\m l -> insertBoth (blHeb l) (blGrk l) m) M.empty (etymologyLinks dict))

-- | Overlay the user's approvals (added) and rejections (removed) onto a base
-- bridge. Used at runtime to fold live model approvals over the static
-- etymology layer without re-parsing the dictionary.
applyStore :: BridgeStore -> Bridge -> Bridge
applyStore store (Bridge m0) = Bridge m2
  where
    m1 = foldl' (\m (h, g) -> insertBoth h g m) m0 (S.toList (bsApproved store))
    m2 = foldl' (\m (h, g) -> deleteBoth h g m) m1 (S.toList (bsRejected store))

-- | Etymology layer plus a store's approvals, minus its rejections.
buildBridge :: StrongsDict -> BridgeStore -> Bridge
buildBridge dict store = applyStore store (etymologyBridge dict)

-- | The Strong's numbers bridged to this one (the other testament's lemmas).
bridgedPartners :: Bridge -> Text -> [Text]
bridgedPartners (Bridge m) s = S.toList (M.findWithDefault S.empty s m)

-- | How many Strong's numbers participate in at least one bridge link.
bridgeSize :: Bridge -> Int
bridgeSize (Bridge m) = M.size m

-- | A concept's per-book counts unified with its bridged partners', so a theme
-- like \"righteousness\" shows a single canon-wide footprint spanning the
-- Hebrew (OT) and Greek (NT) lemmas the bridge ties together.
spannedByBook :: Bridge -> ConceptIx -> Text -> Map Text Int
spannedByBook br cix s =
    M.unionsWith (+)
        [ maybe M.empty csByBook (conceptStat cix p) | p <- s : bridgedPartners br s ]

-- | Index rendering candidates under both endpoints, so the panel for any
-- Strong's number can fetch its candidate partners directly. Per-key order
-- follows the input's (so a score-sorted input yields score-sorted lists).
candidateIndex :: [RenderCand] -> Map Text [RenderCand]
candidateIndex = foldr add M.empty
  where add c = M.insertWith (<>) (rcHeb c) [c] . M.insertWith (<>) (rcGrk c) [c]

-- | The cross-testament partner lemmas for a Strong's number: its etymology
-- links plus its top-@n@ rendering candidates (the other-testament side of
-- each), de-duplicated. Used to paint the bridge's footprint on the strip.
crossPartners :: Bridge -> Map Text [RenderCand] -> Int -> Text -> [Text]
crossPartners br candIx n s =
    nub (bridgedPartners br s <> take n
        [ if rcHeb c == s then rcGrk c else rcHeb c
        | c <- M.findWithDefault [] s candIx ])

-- | Per-book occurrence counts summed over a set of lemmas (e.g. a concept's
-- bridged partners), for one combined footprint.
unionByBook :: ConceptIx -> [Text] -> Map Text Int
unionByBook cix ps =
    M.unionsWith (+) [ maybe M.empty csByBook (conceptStat cix p) | p <- ps ]

-- ── external (hydrated) source links ─────────────────────────────────────────

-- | A Hebrew↔Greek link asserted by an external FOSS source, hydrated offline
-- into 'bridgeSourcesFile'. @slSource@ names the witness (e.g. "stepbible-tbesg")
-- so its trust and label resolve via 'sourcePrior' / 'sourceLabel'.
data SourceLink = SourceLink
    { slHeb    :: !Text
    , slGrk    :: !Text
    , slSource :: !Text
    } deriving (Eq, Show)

instance FromJSON SourceLink where
    parseJSON = withObject "SourceLink" $ \o ->
        SourceLink <$> o .: "h" <*> o .: "g" <*> o .: "source"

newtype BridgeSources = BridgeSources { bsLinks :: [SourceLink] }
instance FromJSON BridgeSources where
    parseJSON = withObject "BridgeSources" $ \o ->
        BridgeSources <$> o .:? "links" .!= []

-- | Where hydrated external bridge links live (gitignored; the hydrate script
-- builds it from STEPBible / LXX / … and rebuilds it locally).
bridgeSourcesFile :: FilePath
bridgeSourcesFile = "data/bridge-sources.json"

-- | Load external source links, or none if the file is absent or unreadable
-- (the bridge then runs on the in-repo etymology + rendering sources alone).
loadBridgeSources :: FilePath -> IO [SourceLink]
loadBridgeSources path = do
    present <- doesFileExist path
    if not present
        then pure []
        else bsLinks . fromRight (BridgeSources []) <$> eitherDecodeFileStrict path

-- | Index source links under both endpoints, for per-lemma lookup.
sourceLinkIndex :: [SourceLink] -> Map Text [SourceLink]
sourceLinkIndex = foldr add M.empty
  where add l = M.insertWith (<>) (slHeb l) [l] . M.insertWith (<>) (slGrk l) [l]

-- | The other-testament lemmas an external source ties to this one.
extraPartners :: Map Text [SourceLink] -> Text -> [Text]
extraPartners ix s =
    nub [ if slHeb l == s then slGrk l else slHeb l | l <- M.findWithDefault [] s ix ]

-- | Trust prior for a source tag (seed weights from the bridge research).
sourcePrior :: Text -> Double
sourcePrior s = case s of
    "stepbible-tbesg" -> 0.90
    "stepbible-tipnr" -> 0.92
    "lxx"             -> 0.80
    _                 -> 0.50

-- | Human label for a source tag, shown as provenance.
sourceLabel :: Text -> Text
sourceLabel s = case s of
    "stepbible-tbesg" -> "STEPBible"
    "stepbible-tipnr" -> "STEPBible names"
    "lxx"             -> "LXX"
    _                 -> s

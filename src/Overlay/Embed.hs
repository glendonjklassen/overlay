-- | Concept embeddings: dense vectors per Strong's number, learned offline by
-- @ml/train_concept2vec.py@ (skip-gram over the corpus read as sentences of
-- Strong's numbers). This module loads that artifact and answers two questions
-- the symbolic indices cannot:
--
--   * /concepts near this one/ — cosine nearest neighbours of a Strong's number,
--     surfacing words that share contexts even when they never co-occur in the
--     same verse (synonyms, antonyms, members of a semantic field);
--   * /verses like this one/ — pooling a verse's concept vectors and ranking the
--     whole corpus by cosine, returning to KJV text.
--
-- It is era-faithful by construction: the vectors were trained only on the KJV
-- and its original-language tags, never the English surface. Everything degrades
-- gracefully — if @data/concept-vectors.vec@ is absent, 'loadEmbedding' returns
-- 'Nothing' and callers fall back to the symbolic co-occurrence layer.
--
-- Hebrew (@H…@) and Greek (@G…@) vectors live in one space but were never in a
-- shared context (no verse mixes the languages), so cross-language cosines are
-- meaningless; neighbour search is therefore restricted to the query's language.
module Overlay.Embed
    ( Embedding
    , embDim
    , embSize
    , loadEmbedding
    , conceptVector
    , nearestConcepts
      -- * Verse similarity (SIF-weighted)
    , VerseSim
    , vsCount
    , buildVerseSim
    , similarVersesIn
    ) where

import Data.List (foldl', foldl1', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Directory (doesFileExist)

import Overlay.Corpus

-- | A loaded embedding: row-normalised vectors so cosine similarity is a plain
-- dot product. Vectors are packed into one flat array (row @i@ is the @d@-slice
-- at @i*d@) to keep neighbour scans tight.
data Embedding = Embedding
    { embDim  :: !Int               -- ^ vector dimensionality
    , embKeys :: !(V.Vector Text)   -- ^ row index → Strong's number
    , embIx   :: !(Map Text Int)    -- ^ Strong's number → row index
    , embVecs :: !(VU.Vector Double) -- ^ flat, row-major, each row unit-length
    }

-- | Number of concepts with a vector.
embSize :: Embedding -> Int
embSize = V.length . embKeys

-- | Load @data/concept-vectors.vec@ (word2vec text format). Returns 'Nothing'
-- if the file is missing or unparseable, so the app runs fine without it.
loadEmbedding :: FilePath -> IO (Maybe Embedding)
loadEmbedding path = do
    present <- doesFileExist path
    if not present
        then pure Nothing
        else do
            txt <- TIO.readFile path
            pure $ case T.lines txt of
                (hdr : rows) | Just d <- dimOf hdr, d > 0 -> build d rows
                _                                         -> Nothing
  where
    dimOf l = case mapMaybe readInt (T.words l) of
        (_ : d : _) -> Just d
        _           -> Nothing
    readInt t = case TR.decimal t of
        Right (n, rest) | T.null rest -> Just n
        _                             -> Nothing

    build d rows =
        let parsed = mapMaybe (parseRow d) rows
        in if null parsed
            then Nothing
            else Just Embedding
                { embDim  = d
                , embKeys = V.fromList (map fst parsed)
                , embIx   = M.fromList (zip (map fst parsed) [0 ..])
                , embVecs = VU.concat (map snd parsed)
                }

    parseRow d line = case T.words line of
        (key : rest) ->
            let xs = mapMaybe readDouble rest
            in if length xs == d then Just (key, normalize (VU.fromList xs))
                                 else Nothing
        _ -> Nothing
    readDouble t = case TR.double t of
        Right (x, _) -> Just x
        _            -> Nothing

-- ── vector helpers ──────────────────────────────────────────────────────────

normalize :: VU.Vector Double -> VU.Vector Double
normalize v =
    let n = sqrt (VU.sum (VU.map (\x -> x * x) v))
    in if n == 0 then v else VU.map (/ n) v

dot :: VU.Vector Double -> VU.Vector Double -> Double
dot a b = VU.sum (VU.zipWith (*) a b)

rowAt :: Embedding -> Int -> VU.Vector Double
rowAt emb i = VU.slice (i * embDim emb) (embDim emb) (embVecs emb)

-- | The (unit-length) vector for a Strong's number, if present.
conceptVector :: Embedding -> Text -> Maybe (VU.Vector Double)
conceptVector emb w = rowAt emb <$> M.lookup w (embIx emb)

-- | First letter (@H@/@G@) marks the testament/language of a Strong's number.
sameLang :: Text -> Text -> Bool
sameLang a b = T.take 1 a == T.take 1 b

addV :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
addV = VU.zipWith (+)

-- ── queries ─────────────────────────────────────────────────────────────────

-- | The @k@ concepts whose vectors are nearest the given one by cosine,
-- restricted to the same language, strongest first.
nearestConcepts :: Embedding -> Int -> Text -> [(Text, Double)]
nearestConcepts emb k w = case M.lookup w (embIx emb) of
    Nothing -> []
    Just i  ->
        let q = rowAt emb i
            scored =
                [ (key, dot q (rowAt emb j))
                | j <- [0 .. embSize emb - 1], j /= i
                , let key = embKeys emb V.! j, sameLang key w ]
        in take k (sortBy (comparing (Down . snd)) scored)

-- ── verse similarity (SIF) ──────────────────────────────────────────────────

-- | Precomputed verse vectors for "verses like this one". A naive average of a
-- verse's concept vectors is dominated by ubiquitous words (LORD, God, the
-- object marker), so every verse looks alike. We use the SIF recipe instead:
-- weight each concept by @a / (a + p(concept))@ to damp the frequent ones, then
-- remove the dominant common direction (per testament). What survives is the
-- verse's distinctive theme, which is what makes the ranking meaningful.
data VerseSim = VerseSim
    { vsDim  :: !Int
    , vsRefs :: !(V.Vector (Text, Int, Int))
    , vsNT   :: !(VU.Vector Bool)            -- ^ Greek (NT) verse?
    , vsVecs :: !(VU.Vector Double)          -- ^ flat, row-major, unit-length
    , vsIx   :: !(Map (Text, Int, Int) Int)
    }

-- | Number of verses with a vector.
vsCount :: VerseSim -> Int
vsCount = V.length . vsRefs

sifA :: Double
sifA = 1.0e-3

-- | Build the SIF verse model from the embedding and the corpus. Pure but
-- heavy (one vector per verse); build it once at startup.
buildVerseSim :: Embedding -> Corpus -> VerseSim
buildVerseSim emb corpus = VerseSim d refsV ntV vecsV ixM
  where
    d      = embDim emb
    vlist  = V.toList (cVerses corpus)
    counts = foldl' (\m s -> M.insertWith (+) s (1 :: Int) m) M.empty
                 [ s | v <- vlist, t <- vTokens v, s <- tokStrongs t ]
    total  = fromIntegral (max 1 (sum (M.elems counts))) :: Double
    wOf s  = sifA / (sifA + fromIntegral (M.findWithDefault 0 s counts) / total)

    -- SIF weighted average of a verse's in-vocabulary concept vectors
    rawOf ss = case [ VU.map (* wOf s) cv | s <- ss, Just cv <- [conceptVector emb s] ] of
        []   -> Nothing
        rows -> Just (VU.map (/ fromIntegral (length rows)) (foldl1' addV rows))

    entries =
        [ (ref, greek, raw)
        | v <- vlist
        , let ref = (vBook v, vChapter v, vVerse v)
              ss  = concatMap tokStrongs (vTokens v)
              greek = case ss of (s : _) -> T.take 1 s == T.pack "G"; _ -> False
        , Just raw <- [rawOf ss] ]

    -- the dominant direction within each testament, to subtract off
    meanDir p = case [ raw | (_, g, raw) <- entries, g == p ] of
        [] -> VU.replicate d 0
        rs -> normalize (VU.map (/ fromIntegral (length rs)) (foldl1' addV rs))
    muH = meanDir False
    muG = meanDir True
    adj g raw = let mu = if g then muG else muH
                in normalize (VU.zipWith (\x m -> x - dot raw mu * m) raw mu)

    adjusted = [ (ref, g, adj g raw) | (ref, g, raw) <- entries ]
    refsV = V.fromList [ r | (r, _, _) <- adjusted ]
    ntV   = VU.fromList [ g | (_, g, _) <- adjusted ]
    vecsV = VU.concat   [ a | (_, _, a) <- adjusted ]
    ixM   = M.fromList (zip [ r | (r, _, _) <- adjusted ] [0 ..])

rowVS :: VerseSim -> Int -> VU.Vector Double
rowVS vs i = VU.slice (i * vsDim vs) (vsDim vs) (vsVecs vs)

-- | The @k@ verses most similar to a reference verse (cosine over SIF vectors),
-- within the same testament, nearest first; the verse itself excluded.
similarVersesIn :: VerseSim -> Int -> (Text, Int, Int) -> [((Text, Int, Int), Double)]
similarVersesIn vs k ref = case M.lookup ref (vsIx vs) of
    Nothing -> []
    Just i  ->
        let q = rowVS vs i
            g = vsNT vs VU.! i
            scored =
                [ (vsRefs vs V.! j, dot q (rowVS vs j))
                | j <- [0 .. vsCount vs - 1], j /= i, vsNT vs VU.! j == g ]
        in take k (sortBy (comparing (Down . snd)) scored)

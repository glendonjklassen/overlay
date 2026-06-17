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
    , similarVerses
    ) where

import Data.List (foldl1', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
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

-- | The pooled, renormalised vector of a bag of Strong's numbers (a verse's
-- concept backbone). 'Nothing' when none of them have a vector.
pooled :: Embedding -> [Text] -> Maybe (VU.Vector Double)
pooled emb ws = case mapMaybe (conceptVector emb) ws of
    []   -> Nothing
    rows -> Just (normalize (foldl1' (VU.zipWith (+)) rows))

-- | First letter (@H@/@G@) marks the testament/language of a Strong's number.
sameLang :: Text -> Text -> Bool
sameLang a b = T.take 1 a == T.take 1 b

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

-- | The @k@ verses most similar to a reference verse, by cosine over pooled
-- concept vectors, within the same testament, nearest first. The reference
-- verse itself is excluded.
similarVerses :: Embedding -> Corpus -> Int -> (Text, Int, Int)
              -> [((Text, Int, Int), Double)]
similarVerses emb corpus k ref =
    case strongsAt ref >>= pooled emb of
        Nothing -> []
        Just q  ->
            let lang = T.take 1 (head (strongsOf ref))
                scored =
                    [ (r, dot q vv)
                    | v <- V.toList (cVerses corpus)
                    , let r = (vBook v, vChapter v, vVerse v), r /= ref
                    , let ss = concatMap tokStrongs (vTokens v)
                    , sameStart lang ss
                    , Just vv <- [pooled emb ss] ]
            in take k (sortBy (comparing (Down . snd)) scored)
  where
    strongsAt r = if null (strongsOf r) then Nothing else Just (strongsOf r)
    strongsOf r = maybe [] (concatMap tokStrongs . vTokens . (cVerses corpus V.!))
        (M.lookup r (cByRef corpus))
    sameStart lang ss = case listToMaybe ss of
        Just s  -> T.take 1 s == lang
        Nothing -> False

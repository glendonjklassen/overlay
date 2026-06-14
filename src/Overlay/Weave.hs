{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Weaves: parallel passages as a graph of verse-to-verse links.
--
-- A weave is a set of undirected links (edges) between single verses. The
-- graph is the whole model: a drawn connector line per edge is its faithful
-- rendering (two edges into one verse draw as converging lines — a 2-to-1
-- correspondence), combining two weaves is union of their edges (so A↔B plus
-- B↔C transitively gives A↔B↔C), and a verse with no parallel is simply an
-- unlinked node — it still reads in place, never collapsed away.
--
-- Like threads, weaves are personal study data: plain unsigned JSON, one file
-- per weave (they never alter the rendered text).
module Overlay.Weave
    ( VRef
    , Link (..)
    , WeaveKind (..)
    , Weave (..)
    , LoadedWeave (..)
    , allKinds
    , kindToken
    , kindLabel
    , parseKind
    , emptyWeave
    , weavesDir
    , weaveFileFor
    , loadWeaves
    , writeWeave
      -- graph ops
    , canonLink
    , addLinks
    , removeLink
    , combine
    , components
    , componentOf
    , linksTouching
    , smartLinks
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.Either (lefts, rights)
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))

import Overlay.Canon (bookIds)
import Overlay.Corpus (parseRefKey, refKey)

-- | A verse address: (OSIS book id, chapter, verse).
type VRef = (Text, Int, Int)

-- | An undirected edge between two verses, stored canonically (lA <= lB in
-- reading order) so equal links compare equal.
data Link = Link { lA :: !VRef, lB :: !VRef }
    deriving (Eq, Ord, Show)

-- | Reading-order key for a verse (book in canon order, then chapter, verse).
vrKey :: VRef -> (Int, Int, Int)
vrKey (b, c, v) = (fromMaybe maxBound (elemIndex b bookIds), c, v)

-- | Build a link with its endpoints in reading order.
canonLink :: VRef -> VRef -> Link
canonLink a b
    | vrKey a <= vrKey b = Link a b
    | otherwise          = Link b a

-- | What sort of parallel a weave records. Stored as a frozen token.
data WeaveKind = Retelling | Typological | Prophecy | Quotation
    deriving (Eq, Show)

allKinds :: [WeaveKind]
allKinds = [Retelling, Typological, Prophecy, Quotation]

kindToken :: WeaveKind -> Text
kindToken k = case k of
    Retelling   -> "retelling"
    Typological -> "type"
    Prophecy    -> "prophecy"
    Quotation   -> "quotation"

kindLabel :: WeaveKind -> Text
kindLabel k = case k of
    Retelling   -> "retelling"
    Typological -> "type"
    Prophecy    -> "prophecy & fulfillment"
    Quotation   -> "quotation"

parseKind :: Text -> Maybe WeaveKind
parseKind t = case t of
    "retelling" -> Just Retelling
    "type"      -> Just Typological
    "prophecy"  -> Just Prophecy
    "quotation" -> Just Quotation
    _           -> Nothing

instance ToJSON Link where
    toJSON (Link a b) = object ["a" .= refKey a, "b" .= refKey b]

instance FromJSON Link where
    parseJSON = withObject "Link" $ \o -> do
        aT <- o .: "a"
        bT <- o .: "b"
        a <- maybe (fail ("bad link ref: " <> T.unpack aT)) pure (parseRefKey aT)
        b <- maybe (fail ("bad link ref: " <> T.unpack bT)) pure (parseRefKey bT)
        pure (canonLink a b)

data Weave = Weave
    { wName       :: !Text
    , wKind       :: !WeaveKind
    , wTokVersion :: !Text
    , wNotes      :: !Text       -- ^ running notes document
    , wCreated    :: !Text       -- ^ UTC timestamp
    , wLinks      :: ![Link]     -- ^ the graph
    } deriving (Eq, Show)

instance ToJSON Weave where
    toJSON w = object
        [ "format" .= ("overlay-weave-v2" :: Text)
        , "name" .= wName w
        , "kind" .= kindToken (wKind w)
        , "tokenization" .= wTokVersion w
        , "notes" .= wNotes w
        , "created" .= wCreated w
        , "links" .= wLinks w
        ]

instance FromJSON Weave where
    parseJSON = withObject "Weave" $ \o -> do
        fmt <- o .: "format"
        unless (fmt == ("overlay-weave-v2" :: Text)) $
            fail ("not overlay-weave-v2: " <> T.unpack fmt)
        kindT <- o .:? "kind" .!= "retelling"
        kind <- maybe (fail ("unknown weave kind: " <> T.unpack kindT)) pure
            (parseKind kindT)
        Weave
            <$> o .: "name"
            <*> pure kind
            <*> o .: "tokenization"
            <*> o .:? "notes" .!= ""
            <*> o .: "created"
            <*> o .:? "links" .!= []

data LoadedWeave = LoadedWeave
    { lwFile  :: !FilePath
    , lwWeave :: !Weave
    } deriving (Eq, Show)

-- | A fresh, empty weave (no links yet).
emptyWeave :: Text -> WeaveKind -> Text -> Text -> Weave
emptyWeave name kind tokv created = Weave name kind tokv "" created []

-- ── graph operations ────────────────────────────────────────────────────────

-- | Add links, keeping the set deduplicated and sorted.
addLinks :: [Link] -> Weave -> Weave
addLinks new w = w { wLinks = Set.toList (Set.fromList (wLinks w <> new)) }

removeLink :: Link -> Weave -> Weave
removeLink l w = w { wLinks = filter (/= l) (wLinks w) }

-- | Union two weaves' edges into the first (the transitive merge: shared
-- verses join their components). The first weave's metadata is kept.
combine :: Weave -> Weave -> Weave
combine a b = addLinks (wLinks b) a

-- | Connected components of the link graph, each a list of verses.
components :: [Link] -> [[VRef]]
components links = go (Set.fromList verts) []
  where
    verts = concatMap (\(Link a b) -> [a, b]) links
    adj = M.fromListWith (<>)
        (concatMap (\(Link a b) -> [(a, [b]), (b, [a])]) links)
    go remaining acc = case Set.lookupMin remaining of
        Nothing -> reverse acc
        Just v  -> let comp = bfs [v] Set.empty
                   in go (remaining Set.\\ comp) (Set.toList comp : acc)
    bfs [] seen = seen
    bfs (v : vs) seen
        | v `Set.member` seen = bfs vs seen
        | otherwise = bfs (M.findWithDefault [] v adj <> vs) (Set.insert v seen)

-- | All verses linked (transitively) to a given verse, the verse included.
componentOf :: [Link] -> VRef -> [VRef]
componentOf links v =
    fromMaybe [v] (find (elem v) (components links))
  where
    find p = foldr (\x r -> if p x then Just x else r) Nothing

-- | The links with at least one endpoint among the given verses (for ambient
-- rendering: edges of weaves that touch what is currently on screen).
linksTouching :: Set VRef -> Weave -> [Link]
linksTouching vs w =
    [ l | l@(Link a b) <- wLinks w, a `Set.member` vs || b `Set.member` vs ]

-- | Build links from a per-pane selection. Two equal-length panes zip 1:1
-- (4→8, 5→9, …); anything else connects every selected verse to every selected
-- verse in another pane (the many-to-many / convergent case).
smartLinks :: [[VRef]] -> [Link]
smartLinks panes = case nonEmpty of
    [a, b] | length a == length b -> zipWith canonLink a b
    _ -> [ canonLink x y
         | (i, px) <- indexed, (j, py) <- indexed, i < j
         , x <- px, y <- py ]
  where
    nonEmpty = filter (not . null) panes
    indexed = zip [0 :: Int ..] nonEmpty

-- ── files ───────────────────────────────────────────────────────────────────

weavesDir :: FilePath
weavesDir = "weaves"

weaveFileFor :: Text -> FilePath
weaveFileFor name = weavesDir </> T.unpack slug <> ".json"
  where
    cleaned = T.intercalate "-"
        (T.words (T.toLower (T.map keep (T.strip name))))
    keep ch = if isAlphaNum ch then ch else ' '
    slug = if T.null cleaned then "weave" else cleaned

-- | Read every weaves/*.json. v1 (grid) files are migrated to the graph model
-- and rewritten in place. Files that fail to parse are reported, not dropped.
loadWeaves :: IO ([LoadedWeave], [String])
loadWeaves = do
    exists <- doesDirectoryExist weavesDir
    files <- if exists
        then sortOn id . filter (T.isSuffixOf ".json" . T.pack)
                <$> listDirectory weavesDir
        else pure []
    results <- forM files $ \f -> do
        let path = weavesDir </> f
        bytes <- try (B.readFile path)
            :: IO (Either SomeException ByteString)
        case bytes of
            Left err -> pure (Left (path <> ": " <> show err))
            Right bs -> case eitherDecodeStrict bs of
                Right w -> pure (Right (LoadedWeave path w))
                Left errV2 -> case decodeStrict bs :: Maybe V1Weave of
                    Just v1 -> do
                        let w = migrateV1 v1
                        writeWeave path w
                        pure (Right (LoadedWeave path w))
                    Nothing -> pure (Left (path <> ": " <> errV2))
    pure ( sortOn (T.toLower . wName . lwWeave) (rights results)
         , lefts results )

writeWeave :: FilePath -> Weave -> IO ()
writeWeave path w = do
    createDirectoryIfMissing True weavesDir
    BL.writeFile path (encode w <> "\n")

-- ── v1 → v2 migration ───────────────────────────────────────────────────────
-- The v1 format was a grid: columns × rows, each cell a list of verse ranges.
-- We only need the rows' cells to derive verse links, so the decoder is
-- deliberately minimal.

data V1Weave = V1Weave
    { v1Name    :: Text
    , v1Kind    :: WeaveKind
    , v1Tok     :: Text
    , v1Notes   :: Text
    , v1Created :: Text
    , v1Rows    :: [[[ (VRef, Int) ]]]  -- rows -> cells -> ranges as (startRef, endVerse)
    }

instance FromJSON V1Weave where
    parseJSON = withObject "V1Weave" $ \o -> do
        fmt <- o .: "format"
        unless (fmt == ("overlay-weave-v1" :: Text)) $
            fail "not overlay-weave-v1"
        kindT <- o .:? "kind" .!= "retelling"
        kind <- maybe (fail "kind") pure (parseKind kindT)
        rowsV <- o .:? "rows" .!= []
        rows <- mapM parseRow rowsV
        V1Weave
            <$> o .: "name"
            <*> pure kind
            <*> o .: "tokenization"
            <*> o .:? "notes" .!= ""
            <*> o .: "created"
            <*> pure rows
      where
        parseRow = withObject "row" $ \o -> do
            cellsV <- o .:? "cells" .!= []
            mapM (mapM parseRange) cellsV
        parseRange = withObject "range" $ \o -> do
            refT <- o .: "ref"
            ref <- maybe (fail "ref") pure (parseRefKey refT)
            let (_, _, s) = ref
            end <- o .:? "end" .!= s
            pure (ref, end)

migrateV1 :: V1Weave -> Weave
migrateV1 v = Weave (v1Name v) (v1Kind v) (v1Tok v) (v1Notes v) (v1Created v)
    (Set.toList (Set.fromList (concatMap rowLinks (v1Rows v))))
  where
    rowLinks cells =
        let lists = map (concatMap expand) cells
        in [ l | (i, xs) <- zip [0 :: Int ..] lists
               , (j, ys) <- zip [0 :: Int ..] lists, i < j
               , l <- pairLinks xs ys ]
    expand ((b, c, s), e) = [(b, c, vn) | vn <- [s .. e]]

-- | Zip two verse lists 1:1, fanning any surplus of the longer list onto the
-- last verse of the shorter (so a count mismatch becomes converging links).
pairLinks :: [VRef] -> [VRef] -> [Link]
pairLinks xs ys
    | null xs || null ys = []
    | otherwise =
        let n = min (length xs) (length ys)
            base = zipWith canonLink (take n xs) (take n ys)
            extraX = [canonLink x (last ys) | x <- drop n xs]
            extraY = [canonLink (last xs) y | y <- drop n ys]
        in base <> extraX <> extraY

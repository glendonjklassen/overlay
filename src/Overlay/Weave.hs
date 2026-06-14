{-# LANGUAGE OverloadedStrings #-}

-- | Weaves: parallel passages woven into aligned rows.
--
-- Where a thread (see "Overlay.Thread") is a single ordered trail of word
-- spans, a weave lines several passages up /side by side/ — a Gospel harmony,
-- a prophecy against its fulfillment, an OT text against the NT that quotes
-- it. It is a small table: named @columns@ (the witnesses — Matthew, Mark,
-- Luke…) crossed with @rows@ (alignment groups — \"the Beatitudes\"), each
-- cell holding the passage(s) that belong at that intersection.
--
-- A cell holds a /list/ of verse ranges, not one: a single alignment row can
-- gather scattered parallels (the Sermon on the Mount sits in one block in
-- Matthew but is spread across several Lukan passages). Cells are kept
-- positionally aligned to 'wColumns'.
--
-- Like threads, weaves are personal study data: plain JSON, one file per
-- weave, no signatures (they never alter the rendered text). Ranges store a
-- snapshot of their opening words so a file reads on its own; the live text
-- is rendered from the corpus, so patches and rules show through.
module Overlay.Weave
    ( VerseRange (..)
    , WeaveCell (..)
    , WeaveRow (..)
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
      -- pure editors
    , addColumn
    , renameColumn
    , removeColumn
    , addRow
    , appendRow
    , removeRow
    , setRowLabel
    , setCell
    , appendRangeToCell
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.Either (lefts, rights)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))

import Overlay.Corpus (parseRefKey, refKey)

-- | What sort of parallel a weave records. Stored as a frozen token.
data WeaveKind = Retelling | Typological | Prophecy | Quotation
    deriving (Eq, Show)

allKinds :: [WeaveKind]
allKinds = [Retelling, Typological, Prophecy, Quotation]

-- | The frozen JSON token for a kind.
kindToken :: WeaveKind -> Text
kindToken k = case k of
    Retelling   -> "retelling"
    Typological -> "type"
    Prophecy    -> "prophecy"
    Quotation   -> "quotation"

-- | A human label for the badge / selector.
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

-- | A passage within one chapter: a verse range plus a snapshot of its
-- opening words (a readability fallback; the live text is rendered from the
-- corpus).
data VerseRange = VerseRange
    { vrBook  :: !Text
    , vrChap  :: !Int
    , vrStart :: !Int   -- ^ start verse
    , vrEnd   :: !Int   -- ^ end verse (>= start; == start for a single verse)
    , vrText  :: ![Text]
    } deriving (Eq, Show)

instance ToJSON VerseRange where
    toJSON r = object
        [ "ref"  .= refKey (vrBook r, vrChap r, vrStart r)
        , "end"  .= vrEnd r
        , "text" .= vrText r
        ]

instance FromJSON VerseRange where
    parseJSON = withObject "VerseRange" $ \o -> do
        refT <- o .: "ref"
        (b, c, s) <- maybe (fail ("bad range ref: " <> T.unpack refT)) pure
            (parseRefKey refT)
        VerseRange b c s <$> o .:? "end" .!= s <*> o .:? "text" .!= []

-- | The passages at one (row, column) intersection. Empty means a blank cell
-- (a pericope present in one witness but not another).
newtype WeaveCell = WeaveCell { wcRanges :: [VerseRange] }
    deriving (Eq, Show)

instance ToJSON WeaveCell where
    toJSON = toJSON . wcRanges

instance FromJSON WeaveCell where
    parseJSON v = WeaveCell <$> parseJSON v

data WeaveRow = WeaveRow
    { wrLabel :: !(Maybe Text)  -- ^ optional row label, e.g. \"The Beatitudes\"
    , wrCells :: ![WeaveCell]   -- ^ positional, aligned to 'wColumns'
    } deriving (Eq, Show)

instance ToJSON WeaveRow where
    toJSON r = object ["label" .= wrLabel r, "cells" .= wrCells r]

instance FromJSON WeaveRow where
    parseJSON = withObject "WeaveRow" $ \o -> WeaveRow
        <$> o .:? "label"
        <*> o .:? "cells" .!= []

data Weave = Weave
    { wName       :: !Text
    , wKind       :: !WeaveKind
    , wTokVersion :: !Text
    , wColumns    :: ![Text]      -- ^ column headers, left -> right
    , wRows       :: ![WeaveRow]
    , wNotes      :: !Text        -- ^ running notes document
    , wCreated    :: !Text        -- ^ UTC timestamp
    } deriving (Eq, Show)

instance ToJSON Weave where
    toJSON w = object
        [ "format" .= ("overlay-weave-v1" :: Text)
        , "name" .= wName w
        , "kind" .= kindToken (wKind w)
        , "tokenization" .= wTokVersion w
        , "columns" .= wColumns w
        , "rows" .= wRows w
        , "notes" .= wNotes w
        , "created" .= wCreated w
        ]

instance FromJSON Weave where
    parseJSON = withObject "Weave" $ \o -> do
        fmt <- o .: "format"
        unless (fmt == ("overlay-weave-v1" :: Text)) $
            fail ("unknown weave format: " <> T.unpack fmt)
        kindT <- o .:? "kind" .!= "retelling"
        kind <- maybe (fail ("unknown weave kind: " <> T.unpack kindT)) pure
            (parseKind kindT)
        Weave
            <$> o .: "name"
            <*> pure kind
            <*> o .: "tokenization"
            <*> o .:? "columns" .!= []
            <*> o .:? "rows" .!= []
            <*> o .:? "notes" .!= ""
            <*> o .: "created"

data LoadedWeave = LoadedWeave
    { lwFile  :: !FilePath
    , lwWeave :: !Weave
    } deriving (Eq, Show)

-- | A fresh, empty weave (no columns or rows yet).
emptyWeave :: Text -> WeaveKind -> Text -> Text -> Weave
emptyWeave name kind tokv = Weave name kind tokv [] [] ""

weavesDir :: FilePath
weavesDir = "weaves"

weaveFileFor :: Text -> FilePath
weaveFileFor name = weavesDir </> T.unpack slug <> ".json"
  where
    cleaned = T.intercalate "-"
        (T.words (T.toLower (T.map keep (T.strip name))))
    keep ch = if isAlphaNum ch then ch else ' '
    slug = if T.null cleaned then "weave" else cleaned

-- | Read every weaves/*.json, sorted by name. Files that fail to parse are
-- reported with their error instead of silently dropped.
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
        pure $ case bytes of
            Left err -> Left (path <> ": " <> show err)
            Right bs -> case eitherDecodeStrict bs of
                Left err -> Left (path <> ": " <> err)
                Right w -> Right (LoadedWeave path w)
    pure ( sortOn (T.toLower . wName . lwWeave) (rights results)
         , lefts results )

writeWeave :: FilePath -> Weave -> IO ()
writeWeave path w = do
    createDirectoryIfMissing True weavesDir
    BL.writeFile path (encode w <> "\n")

-- ── pure editors ──────────────────────────────────────────────────────────
-- Out-of-range indices are a no-op, so callers needn't bounds-check.

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = [ x | (j, x) <- zip [0 ..] xs, j /= i ]

-- | Pad a row's cells out to @n@ columns (rows loaded from hand-authored
-- files may carry fewer cells than there are columns).
padCells :: Int -> WeaveRow -> WeaveRow
padCells n r = r { wrCells = take n (wrCells r <> repeat (WeaveCell [])) }

-- | Append a column; every row grows a blank cell to stay aligned.
addColumn :: Text -> Weave -> Weave
addColumn title w = w
    { wColumns = wColumns w <> [title]
    , wRows = map (\r -> r { wrCells = wrCells r <> [WeaveCell []] }) (wRows w)
    }

renameColumn :: Int -> Text -> Weave -> Weave
renameColumn i title w = w { wColumns = modifyAt i (const title) (wColumns w) }

removeColumn :: Int -> Weave -> Weave
removeColumn i w = w
    { wColumns = deleteAt i (wColumns w)
    , wRows = map (\r -> r { wrCells = deleteAt i (wrCells r) }) (wRows w)
    }

-- | Append an empty row, its cells sized to the current column count.
addRow :: Maybe Text -> Weave -> Weave
addRow label w = w
    { wRows = wRows w
        <> [WeaveRow label (replicate (length (wColumns w)) (WeaveCell []))]
    }

-- | Append a row with the given cells, padded/truncated to the column count.
appendRow :: Maybe Text -> [WeaveCell] -> Weave -> Weave
appendRow label cells w = w
    { wRows = wRows w
        <> [WeaveRow label (take n (cells <> repeat (WeaveCell [])))]
    }
  where n = length (wColumns w)

removeRow :: Int -> Weave -> Weave
removeRow i w = w { wRows = deleteAt i (wRows w) }

setRowLabel :: Int -> Maybe Text -> Weave -> Weave
setRowLabel i label w =
    w { wRows = modifyAt i (\r -> r { wrLabel = label }) (wRows w) }

-- | Replace the ranges in one (row, column) cell.
setCell :: Int -> Int -> [VerseRange] -> Weave -> Weave
setCell rowIx colIx ranges = onCell rowIx colIx (const (WeaveCell ranges))

-- | Append one range to a (row, column) cell.
appendRangeToCell :: Int -> Int -> VerseRange -> Weave -> Weave
appendRangeToCell rowIx colIx rng =
    onCell rowIx colIx (\c -> WeaveCell (wcRanges c <> [rng]))

onCell :: Int -> Int -> (WeaveCell -> WeaveCell) -> Weave -> Weave
onCell rowIx colIx f w =
    let n = length (wColumns w)
    in w { wRows = modifyAt rowIx
            (\r -> let r' = padCells n r
                   in r' { wrCells = modifyAt colIx f (wrCells r') })
            (wRows w)
         }

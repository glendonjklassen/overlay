{-# LANGUAGE OverloadedStrings #-}

module Overlay.Session where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist, getXdgDirectory,
                         XdgDirectory (XdgConfig), createDirectoryIfMissing)
import System.FilePath ((</>))

import Overlay.Canon (bookIds)
import Overlay.Corpus
import Overlay.Types

-- | The last-open session: which passage (book, chapter) each reading pane
-- showed. Saved on close and restored on the next launch, so the app reopens
-- where you left it instead of resetting to Genesis 1. Selections and scroll
-- offsets are transient and not persisted.
data Session = Session
    { sessPanes   :: [(Text, Int)]
    , sessMaxCols :: Int
    }

instance ToJSON Session where
    toJSON (Session ps mc) = object
        [ "panes" .= [ object ["book" .= b, "chapter" .= c] | (b, c) <- ps ]
        , "maxColumns" .= mc ]

instance FromJSON Session where
    parseJSON = withObject "Session" $ \o -> do
        arr <- o .:? "panes" .!= []
        ps  <- mapM (withObject "pane" $ \p ->
            (,) <$> p .: "book" <*> p .: "chapter") arr
        mc  <- o .:? "maxColumns" .!= defaultMaxCols
        pure (Session ps mc)

-- | The absolute ceiling on reading columns: panes cycle colours and layout
-- caps at four. The user's live limit (amMaxCols) is clamped into 1…this.
maxColsCap :: Int
maxColsCap = 4

-- | The column limit a fresh install opens with: two side-by-side panes read
-- comfortably on a typical screen, where four crush the text.
defaultMaxCols :: Int
defaultMaxCols = 2

-- | Hold a column limit inside the supported 1…cap range.
clampMaxCols :: Int -> Int
clampMaxCols = max 1 . min maxColsCap

sessionPath :: IO FilePath
sessionPath = do
    dir <- getXdgDirectory XdgConfig "overlay"
    pure (dir </> "session.json")

-- | Persist the panes' passages. Best-effort: a write failure must never crash
-- the app on close, so errors are swallowed.
saveSession :: Int -> [PaneState] -> IO ()
saveSession maxCols panes = do
    dir <- getXdgDirectory XdgConfig "overlay"
    path <- sessionPath
    let sess = Session [ (_psBook p, _psChapter p) | p <- panes ] maxCols
    _ <- try (createDirectoryIfMissing True dir
        >> BL.writeFile path (encode sess <> "\n")) :: IO (Either SomeException ())
    pure ()

-- | Read the saved session (raw, unvalidated). Missing or unreadable falls back
-- to no panes and the default column limit.
loadSession :: IO Session
loadSession = do
    path <- sessionPath
    exists <- doesFileExist path
    if not exists then pure (Session [] defaultMaxCols) else do
        raw <- BC.readFile path
        pure $ fromRight (Session [] defaultMaxCols) (eitherDecodeStrict raw)

-- | Build the initial panes from a saved session, validated against the corpus:
-- unknown books are dropped, chapters clamped to range, capped at the saved
-- column limit. An empty or invalid session falls back to a single Genesis 1
-- pane.
restorePanes :: Corpus -> Int -> [(Text, Int)] -> [PaneState]
restorePanes corpus maxCols saved =
    case mapMaybe valid (take (clampMaxCols maxCols) saved) of
        []  -> [PaneState "Gen" 1 Nothing []]
        ps  -> ps
  where
    valid (b, c)
        | b `elem` bookIds =
            Just (PaneState b (max 1 (min c (chapterCount corpus b))) Nothing [])
        | otherwise = Nothing

{-# LANGUAGE OverloadedStrings #-}

-- | Threads: named trails of passages through the text, with notes.
--
-- A thread (e.g. \"Christ throughout the Bible\") collects entries — a verse
-- ref plus a word span, as small as a single word — each with an optional
-- note, alongside a running notes document on the thread itself. Threads are
-- personal study data: plain JSON, one file per thread, no signatures (they
-- never alter the rendered text, so the patch trust model doesn't apply).
--
-- Entries snapshot the canonical words they covered when added, so a thread
-- file remains readable on its own and survives retokenization gracefully
-- (the snapshot may just no longer match what the reader shows).
module Overlay.Thread
    ( Thread (..)
    , ThreadEntry (..)
    , LoadedThread (..)
    , threadsDir
    , threadFileFor
    , loadThreads
    , addToThread
    , writeThread
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless)
import Data.Either (lefts, rights)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
import System.FilePath ((</>))

import Overlay.Corpus (parseRefKey, refKey)

data ThreadEntry = ThreadEntry
    { teRef   :: !(Text, Int, Int)
    , teSpan  :: !(Int, Int)  -- ^ inclusive word indices, like patches
    , teText  :: ![Text]      -- ^ snapshot of the canonical words
    , teNote  :: !(Maybe Text)
    , teAdded :: !Text        -- ^ UTC timestamp
    } deriving (Eq, Show)

instance ToJSON ThreadEntry where
    toJSON e = object
        [ "ref" .= refKey (teRef e)
        , "span" .= teSpan e
        , "text" .= teText e
        , "note" .= teNote e
        , "added" .= teAdded e
        ]

instance FromJSON ThreadEntry where
    parseJSON = withObject "ThreadEntry" $ \o -> do
        refT <- o .: "ref"
        ref <- maybe (fail ("bad entry ref: " <> T.unpack refT)) pure
            (parseRefKey refT)
        ThreadEntry ref
            <$> o .: "span"
            <*> o .: "text"
            <*> o .:? "note"
            <*> o .: "added"

data Thread = Thread
    { thName       :: !Text
    , thTokVersion :: !Text
    , thNotes      :: !Text  -- ^ the running notes document
    , thEntries    :: ![ThreadEntry]
    , thCreated    :: !Text
    } deriving (Eq, Show)

instance ToJSON Thread where
    toJSON t = object
        [ "format" .= ("overlay-thread-v1" :: Text)
        , "name" .= thName t
        , "tokenization" .= thTokVersion t
        , "notes" .= thNotes t
        , "entries" .= thEntries t
        , "created" .= thCreated t
        ]

instance FromJSON Thread where
    parseJSON = withObject "Thread" $ \o -> do
        fmt <- o .: "format"
        unless (fmt == ("overlay-thread-v1" :: Text)) $
            fail ("unknown thread format: " <> T.unpack fmt)
        Thread
            <$> o .: "name"
            <*> o .: "tokenization"
            <*> o .:? "notes" .!= ""
            <*> o .:? "entries" .!= []
            <*> o .: "created"

data LoadedThread = LoadedThread
    { ltFile   :: !FilePath
    , ltThread :: !Thread
    } deriving (Eq, Show)

threadsDir :: FilePath
threadsDir = "threads"

threadFileFor :: Text -> FilePath
threadFileFor name = threadsDir </> T.unpack slug <> ".json"
  where
    cleaned = T.intercalate "-"
        (T.words (T.toLower (T.map keep (T.strip name))))
    keep ch = if isAlphaNum ch then ch else ' '
    slug = if T.null cleaned then "thread" else cleaned

-- | Read every threads/*.json, sorted by name. Files that fail to parse are
-- reported with their error instead of silently dropped.
loadThreads :: IO ([LoadedThread], [String])
loadThreads = do
    exists <- doesDirectoryExist threadsDir
    files <- if exists
        then sortOn id . filter (T.isSuffixOf ".json" . T.pack)
                <$> listDirectory threadsDir
        else pure []
    results <- forM files $ \f -> do
        let path = threadsDir </> f
        bytes <- try (B.readFile path)
            :: IO (Either SomeException ByteString)
        pure $ case bytes of
            Left err -> Left (path <> ": " <> show err)
            Right bs -> case eitherDecodeStrict bs of
                Left err -> Left (path <> ": " <> err)
                Right t -> Right (LoadedThread path t)
    pure ( sortOn (T.toLower . thName . ltThread) (rights results)
         , lefts results )

-- | Append an entry to the thread named @name@, creating its file on first
-- use. Name matching against loaded threads is case-insensitive.
addToThread
    :: [LoadedThread]
    -> Text               -- ^ thread name
    -> Text               -- ^ tokenization version (stamped on creation)
    -> (Text, Int, Int)   -- ^ verse ref
    -> (Int, Int)         -- ^ word span (inclusive)
    -> [Text]             -- ^ snapshot of the canonical words
    -> Maybe Text
    -> IO (Either String FilePath)
addToThread loaded name tokv ref span_ ws note = do
    now <- getCurrentTime
    let stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
        entry = ThreadEntry ref span_ ws (fmap (T.replace "\n" " ") note) stamp
        wanted = T.toLower (T.strip name)
        match lt = T.toLower (thName (ltThread lt)) == wanted
    case filter match loaded of
        (lt : _) -> do
            let t = ltThread lt
            writeThread (ltFile lt) t { thEntries = thEntries t <> [entry] }
            pure (Right (ltFile lt))
        [] -> do
            let path = threadFileFor name
            exists <- doesFileExist path
            if exists
                -- a file is there but didn't load (parse error): refuse to
                -- clobber it rather than lose whatever it holds
                then pure (Left (path <> " exists but could not be read"))
                else do
                    writeThread path
                        (Thread (T.strip name) tokv "" [entry] stamp)
                    pure (Right path)

writeThread :: FilePath -> Thread -> IO ()
writeThread path t = do
    createDirectoryIfMissing True threadsDir
    BL.writeFile path (encode t <> "\n")

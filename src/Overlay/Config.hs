{-# LANGUAGE OverloadedStrings #-}

module Overlay.Config where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, getXdgDirectory,
                         XdgDirectory (XdgConfig), createDirectoryIfMissing)
import System.FilePath ((</>))

notesPath :: FilePath
notesPath = "data/kjv-notes.jsonl"

-- ── settings ────────────────────────────────────────────────────────────────

data Settings = Settings
    { sSerif       :: Maybe Text  -- ^ path to the body serif font
    , sSerifItalic :: Maybe Text
    , sBodySize    :: Double
    , sLineSpacing :: Double
    } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings Nothing Nothing 17 1.45

instance FromJSON Settings where
    parseJSON = withObject "Settings" $ \o -> Settings
        <$> o .:? "serifRegular"
        <*> o .:? "serifItalic"
        <*> o .:? "bodySize" .!= sBodySize defaultSettings
        <*> o .:? "lineSpacing" .!= sLineSpacing defaultSettings

instance ToJSON Settings where
    toJSON s = object
        [ "serifRegular" .= sSerif s
        , "serifItalic" .= sSerifItalic s
        , "bodySize" .= sBodySize s
        , "lineSpacing" .= sLineSpacing s
        ]

-- | Read ~/.config/overlay/config.json, writing a template on first run so
-- the knobs are discoverable.
loadSettings :: IO Settings
loadSettings = do
    dir <- getXdgDirectory XdgConfig "overlay"
    createDirectoryIfMissing True dir
    let path = dir </> "config.json"
    exists <- doesFileExist path
    if not exists
        then do
            BL.writeFile path (encode defaultSettings <> "\n")
            pure defaultSettings
        else do
            raw <- BC.readFile path
            case eitherDecodeStrict raw of
                Right s -> pure s
                Left err -> do
                    putStrLn ("config.json ignored (" <> err <> ")")
                    pure defaultSettings

-- | Persist settings back to config.json (best-effort), e.g. after a live zoom.
saveSettings :: Settings -> IO ()
saveSettings s = do
    dir <- getXdgDirectory XdgConfig "overlay"
    _ <- try (createDirectoryIfMissing True dir
        >> BL.writeFile (dir </> "config.json") (encode s <> "\n"))
        :: IO (Either SomeException ())
    pure ()

-- DejaVu lives in a different directory on every distro (Debian, Arch, Fedora),
-- so probe them all rather than hard-coding one. Bundled EB Garamond is always
-- present, so it backstops every list — the UI never ends up glyphless.
dejavuDirs :: [Text]
dejavuDirs =
    [ "/usr/share/fonts/truetype/dejavu/"  -- Debian, Ubuntu, WSL
    , "/usr/share/fonts/TTF/"              -- Arch
    , "/usr/share/fonts/dejavu/"           -- Fedora and others
    ]

-- | Pick the first font path that exists, from an optional explicit override
-- then a list of fallbacks; if none exist, return the last candidate.
pickFont :: Maybe Text -> [Text] -> IO Text
pickFont explicit fallbacks = do
    let candidates = maybe [] (pure . T.unpack) explicit <> map T.unpack fallbacks
    found <- firstExisting candidates
    pure (T.pack (fromMaybe (last candidates) found))
  where
    firstExisting (c : cs) = do
        ok <- doesFileExist c
        if ok then pure (Just c) else firstExisting cs
    firstExisting [] = pure Nothing

-- | Resolve the serif faces: explicit config path, else bundled EB Garamond,
-- else DejaVu.
resolveFonts :: Settings -> IO (Text, Text)
resolveFonts s = do
    regular <- pickFont (sSerif s)
        ("assets/fonts/EBGaramond.ttf" : map (<> "DejaVuSerif.ttf") dejavuDirs)
    italic <- pickFont (sSerifItalic s)
        ("assets/fonts/EBGaramond-Italic.ttf" : map (<> "DejaVuSerif-Italic.ttf") dejavuDirs)
    pure (regular, italic)

-- | Resolve the sans UI faces (regular, bold). Prefers the bundled DejaVu Sans
-- (so symbols like ✓ ↔ ⚠ always render and the UI never depends on a system
-- font), then any system DejaVu, then the bundled serif as a last resort.
resolveSans :: IO (Text, Text)
resolveSans = do
    regular <- pickFont Nothing
        ("assets/fonts/DejaVuSans.ttf"
            : map (<> "DejaVuSans.ttf") dejavuDirs <> ["assets/fonts/EBGaramond.ttf"])
    bold <- pickFont Nothing
        ("assets/fonts/DejaVuSans-Bold.ttf"
            : map (<> "DejaVuSans-Bold.ttf") dejavuDirs <> ["assets/fonts/EBGaramond.ttf"])
    pure (regular, bold)

-- ── margin notes ────────────────────────────────────────────────────────────

loadNotes :: IO (M.Map (Text, Int, Int) [Text])
loadNotes = do
    exists <- doesFileExist notesPath
    if not exists then pure M.empty else do
        raw <- BC.readFile notesPath
        let recs = mapMaybe decodeStrict (BC.lines raw)
        pure $ M.fromListWith (flip (<>))
            [ ((b, c, n), [t]) | NoteRec b c n t <- recs ]

data NoteRec = NoteRec Text Int Int Text

instance FromJSON NoteRec where
    parseJSON = withObject "NoteRec" $ \o -> NoteRec
        <$> o .: "b" <*> o .: "c" <*> o .: "v" <*> o .: "note"

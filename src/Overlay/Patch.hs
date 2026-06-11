{-# LANGUAGE OverloadedStrings #-}

-- | Signed point-patches to the text.
--
-- A patch replaces a span of words in one verse, addressed by word index
-- against the frozen canonical tokenization. It is signed (Ed25519) over a
-- deterministic byte encoding of its content; the JSON file is just an
-- envelope. The base text is never modified — patches are an overlay,
-- composed at render time.
--
-- Trust model: the local keypair (generated on first run) is always trusted;
-- additional public keys can be listed in trusted-keys. Patches with valid
-- signatures from unknown keys still apply but are visually flagged; invalid
-- ones (bad signature, text mismatch, wrong tokenization, overlap) never
-- apply.
module Overlay.Patch
    ( Patch (..)
    , PatchStatus (..)
    , LoadedPatch (..)
    , Keys (..)
    , patchesDir
    , loadOrCreateKeys
    , loadTrustedKeys
    , generateKeys
    , fingerprint
    , signBytes
    , verifyBytes
    , mkPatch
    , savePatch
    , loadPatches
    , acceptedForVerse
    , selfTest
      -- exported for the test suite
    , signingBytes
    , verifySig
    , checkAgainstText
    , resolveOverlaps
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, when)
import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed
import Data.Aeson
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
import System.FilePath ((</>))

import Overlay.Corpus

data Patch = Patch
    { pTokVersion  :: !Text
    , pBook        :: !Text
    , pChapter     :: !Int
    , pVerse       :: !Int
    , pSpan        :: !(Int, Int)   -- ^ inclusive word indices into vTokens
    , pOriginal    :: ![Text]       -- ^ expected tokWord of the span
    , pReplacement :: ![Text]
    , pNote        :: !(Maybe Text)
    , pAuthor      :: !Text         -- ^ Ed25519 public key, hex
    , pCreated     :: !Text         -- ^ UTC timestamp
    , pSig         :: !Text         -- ^ Ed25519 signature, hex
    } deriving (Eq, Show)

instance ToJSON Patch where
    toJSON p = object
        [ "format" .= ("overlay-patch-v1" :: Text)
        , "tokenization" .= pTokVersion p
        , "book" .= pBook p
        , "chapter" .= pChapter p
        , "verse" .= pVerse p
        , "span" .= pSpan p
        , "original" .= pOriginal p
        , "replacement" .= pReplacement p
        , "note" .= pNote p
        , "author" .= pAuthor p
        , "created" .= pCreated p
        , "signature" .= pSig p
        ]

instance FromJSON Patch where
    parseJSON = withObject "Patch" $ \o -> do
        fmt <- o .: "format"
        unless (fmt == ("overlay-patch-v1" :: Text)) $
            fail ("unknown patch format: " <> T.unpack fmt)
        Patch
            <$> o .: "tokenization"
            <*> o .: "book"
            <*> o .: "chapter"
            <*> o .: "verse"
            <*> o .: "span"
            <*> o .: "original"
            <*> o .: "replacement"
            <*> o .:? "note"
            <*> o .: "author"
            <*> o .: "created"
            <*> o .: "signature"

data PatchStatus
    = POwn          -- ^ valid signature from this machine's key
    | PTrusted      -- ^ valid signature from a key in trusted-keys
    | PUnknownKey   -- ^ valid signature, unrecognized key (applied, flagged)
    | PInvalid Text -- ^ never applied; reason inside
    deriving (Eq, Show)

data LoadedPatch = LoadedPatch
    { lpFile   :: !FilePath
    , lpPatch  :: !Patch
    , lpStatus :: !PatchStatus
    } deriving (Eq, Show)

data Keys = Keys
    { kSecret :: !Ed.SecretKey
    , kPublic :: !Ed.PublicKey
    , kPubHex :: !Text
    }

patchesDir :: FilePath
patchesDir = "patches"

-- ── encoding helpers ────────────────────────────────────────────────────────

toHex :: ByteString -> Text
toHex = TE.decodeUtf8 . convertToBase Base16

fromHex :: Text -> Either String ByteString
fromHex = convertFromBase Base16 . TE.encodeUtf8

fingerprint :: Text -> Text
fingerprint = T.take 8

cryptoEither :: CE.CryptoFailable a -> Either String a
cryptoEither r = case r of
    CE.CryptoPassed a -> Right a
    CE.CryptoFailed e -> Left (show e)

-- | Sign raw bytes with the local key; hex-encoded signature.
signBytes :: Keys -> ByteString -> Text
signBytes keys = toHex . convert . Ed.sign (kSecret keys) (kPublic keys)

-- | Verify a hex author/signature pair over raw bytes.
verifyBytes :: Text -> Text -> ByteString -> Either String ()
verifyBytes authorHex sigHex bs = do
    pkRaw <- fromHex authorHex
    sigRaw <- fromHex sigHex
    pk <- cryptoEither (Ed.publicKey pkRaw)
    sig <- cryptoEither (Ed.signature sigRaw)
    if Ed.verify pk bs sig
        then Right ()
        else Left "signature does not verify"

-- | The signed bytes. Frozen: any change breaks every existing signature.
signingBytes :: Patch -> ByteString
signingBytes p = TE.encodeUtf8 $ T.intercalate "\n"
    [ "overlay-patch-v1"
    , "tokenization:" <> pTokVersion p
    , "target:" <> pBook p <> " " <> showt (pChapter p)
        <> ":" <> showt (pVerse p)
    , "span:" <> showt (fst (pSpan p)) <> " " <> showt (snd (pSpan p))
    , "original:" <> T.unwords (pOriginal p)
    , "replacement:" <> T.unwords (pReplacement p)
    , "note:" <> fromMaybe "" (pNote p)
    , "author:" <> pAuthor p
    , "created:" <> pCreated p
    ]
  where
    showt = T.pack . show

-- ── keys ────────────────────────────────────────────────────────────────────

-- | Loads ~/.config/overlay/ed25519.secret, generating it on first run.
loadOrCreateKeys :: IO (Either String Keys)
loadOrCreateKeys = do
    dir <- getXdgDirectory XdgConfig "overlay"
    createDirectoryIfMissing True dir
    let secretPath = dir </> "ed25519.secret"
    exists <- doesFileExist secretPath
    if exists
        then do
            txt <- T.strip . TE.decodeUtf8 <$> B.readFile secretPath
            pure $ do
                raw <- fromHex txt
                sk <- cryptoEither (Ed.secretKey raw)
                Right (mkKeys sk)
        else do
            sk <- Ed.generateSecretKey
            B.writeFile secretPath (TE.encodeUtf8 (toHex (convert sk)) <> "\n")
            -- convenience copy of the public key for sharing
            let keys = mkKeys sk
            B.writeFile (dir </> "ed25519.public")
                (TE.encodeUtf8 (kPubHex keys) <> "\n")
            pure (Right keys)
  where
    mkKeys sk = let pk = Ed.toPublic sk in Keys sk pk (toHex (convert pk))

-- | Fresh in-memory keypair (tests; no files touched).
generateKeys :: IO Keys
generateKeys = do
    sk <- Ed.generateSecretKey
    let pk = Ed.toPublic sk
    pure (Keys sk pk (toHex (convert pk)))

loadTrustedKeys :: IO [Text]
loadTrustedKeys = do
    dir <- getXdgDirectory XdgConfig "overlay"
    let path = dir </> "trusted-keys"
    exists <- doesFileExist path
    if not exists then pure [] else do
        txt <- TE.decodeUtf8 <$> B.readFile path
        pure [ l | l <- map T.strip (T.lines txt)
             , not (T.null l), not ("#" `T.isPrefixOf` l) ]

-- ── creation ────────────────────────────────────────────────────────────────

mkPatch
    :: Keys
    -> Text               -- ^ tokenization version
    -> (Text, Int, Int)   -- ^ target verse
    -> (Int, Int)         -- ^ word span (inclusive)
    -> [Text]             -- ^ original words
    -> [Text]             -- ^ replacement words
    -> Maybe Text
    -> IO Patch
mkPatch keys tokv (b, c, v) span_ orig repl note = do
    now <- getCurrentTime
    let created = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
        cleanNote = fmap (T.replace "\n" " ") note
        unsigned = Patch tokv b c v span_ orig repl cleanNote
            (kPubHex keys) created ""
    pure unsigned { pSig = signBytes keys (signingBytes unsigned) }

savePatch :: Patch -> IO FilePath
savePatch p = do
    createDirectoryIfMissing True patchesDir
    let name = T.unpack $ T.intercalate "-"
            [ pBook p, T.pack (show (pChapter p)), T.pack (show (pVerse p))
            , T.pack (show (fst (pSpan p))), fingerprint (pSig p)
            ] <> ".json"
        path = patchesDir </> name
    BL.writeFile path (encode p <> "\n")
    pure path

-- ── loading and verification ────────────────────────────────────────────────

verifySig :: Patch -> Either String ()
verifySig p = verifyBytes (pAuthor p) (pSig p) (signingBytes p)

checkAgainstText :: Corpus -> Patch -> Either String ()
checkAgainstText corpus p = do
    unless (pTokVersion p == cTokVersion corpus) $
        Left ("tokenization mismatch: patch has " <> T.unpack (pTokVersion p)
              <> ", corpus has " <> T.unpack (cTokVersion corpus))
    i <- maybe (Left "target verse does not exist") Right $
        M.lookup (pBook p, pChapter p, pVerse p) (cByRef corpus)
    let toks = vTokens (cVerses corpus V.! i)
        (s, e) = pSpan p
    unless (s >= 0 && e >= s && e < length toks) $
        Left "span out of range"
    let actual = map tokWord (take (e - s + 1) (drop s toks))
    unless (actual == pOriginal p) $
        Left ("text mismatch: span reads " <> show actual)
    when (null (pReplacement p)) $
        Left "empty replacement"

-- | Read every patches/*.json, verify, and resolve overlaps (earlier created
-- wins; later overlapping patches are marked invalid).
loadPatches :: Keys -> Corpus -> IO [LoadedPatch]
loadPatches keys corpus = do
    trusted <- loadTrustedKeys
    exists <- doesDirectoryExist patchesDir
    files <- if exists
        then sortOn id . filter (T.isSuffixOf ".json" . T.pack)
                <$> listDirectory patchesDir
        else pure []
    raws <- forM files $ \f -> do
        let path = patchesDir </> f
        bytes <- try (B.readFile path)
            :: IO (Either SomeException ByteString)
        pure (path, bytes)
    let parsed =
            [ case bytes of
                Left err -> LoadedPatch path placeholder
                    (PInvalid ("unreadable: " <> T.pack (show err)))
                Right bs -> case eitherDecodeStrict bs of
                    Left err -> LoadedPatch path placeholder
                        (PInvalid ("parse error: " <> T.pack err))
                    Right p -> LoadedPatch path p (statusOf trusted p)
            | (path, bytes) <- raws
            ]
    pure (resolveOverlaps parsed)
  where
    placeholder = Patch "" "" 0 0 (0, 0) [] [] Nothing "" "" ""
    statusOf trusted p =
        case verifySig p >> checkAgainstText corpus p of
            Left err -> PInvalid (T.pack err)
            Right ()
                | pAuthor p == kPubHex keys -> POwn
                | pAuthor p `elem` trusted -> PTrusted
                | otherwise -> PUnknownKey

resolveOverlaps :: [LoadedPatch] -> [LoadedPatch]
resolveOverlaps lps = snd (foldl' step (M.empty, []) (sortOn orderKey lps))
  where
    orderKey lp = (pCreated (lpPatch lp), lpFile lp)
    step (claimed, out) lp = case lpStatus lp of
        PInvalid _ -> (claimed, out <> [lp])
        _ ->
            let p = lpPatch lp
                ref = (pBook p, pChapter p, pVerse p)
                (s, e) = pSpan p
                taken = M.findWithDefault [] ref claimed
                overlaps = any (\(s', e') -> s <= e' && s' <= e) taken
            in if overlaps
                then (claimed, out <> [lp { lpStatus =
                    PInvalid "overlaps an earlier patch" }])
                else ( M.insertWith (<>) ref [(s, e)] claimed
                     , out <> [lp] )

-- | Accepted (applied) patches for one verse, sorted by span start.
acceptedForVerse :: [LoadedPatch] -> (Text, Int, Int) -> [LoadedPatch]
acceptedForVerse lps (b, c, v) = sortOn (fst . pSpan . lpPatch)
    [ lp | lp <- lps
    , let p = lpPatch lp
    , (pBook p, pChapter p, pVerse p) == (b, c, v)
    , applied (lpStatus lp)
    ]
  where
    applied (PInvalid _) = False
    applied _ = True

-- | In-memory sign/verify roundtrip for --check.
selfTest :: Keys -> Text -> IO (Either String ())
selfTest keys tokv = do
    p <- mkPatch keys tokv ("Gen", 1, 1) (2, 2) ["beginning"] ["start"]
        (Just "selftest")
    let tampered = p { pReplacement = ["end"] }
    pure $ case (verifySig p, verifySig tampered) of
        (Right (), Left _) -> Right ()
        (Left err, _) -> Left ("valid patch failed to verify: " <> err)
        (_, Right ()) -> Left "tampered patch verified — signing is broken"

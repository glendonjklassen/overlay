{-# LANGUAGE OverloadedStrings #-}

-- | Signed corpus-wide rewrite rules.
--
-- A rule replaces every occurrence of a word sequence across the whole
-- corpus, addressed by content rather than position: wherever the canonical
-- tokenization reads the match sequence, the replacement is rendered.
-- Per-verse exclusions opt individual verses back out. Like patches, rules
-- are an overlay — Ed25519 signed, verified on load, never touching the
-- base text.
--
-- Composition at render time (see the caller and 'ruleHitsForVerse'):
--
--   * point patches always win over rules (specific beats general);
--   * among rules, the earlier created wins where matches overlap;
--   * rules match the canonical text only — never the output of patches or
--     other rules — so application never cascades.
--
-- Adding an exclusion re-signs the rule but keeps its @created@ stamp, so
-- the rule keeps its precedence slot. Only the rule's author can do that;
-- to override someone else's rule at one spot, author a point patch there.
module Overlay.Rule
    ( Rule (..)
    , LoadedRule (..)
    , rulesDir
    , mkRule
    , resignRule
    , saveRule
    , loadRules
    , excludeVerse
      -- exported for the test suite
    , ruleSigningBytes
    , verifyRuleSig
    , checkRule
    , matchSpans
    , ruleHitsForVerse
    , countRuleMatches
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.List (foldl', isPrefixOf, sortOn)
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
import Overlay.Patch (Keys (..), PatchStatus (..), fingerprint,
                      loadTrustedKeys, signBytes, verifyBytes)

data Rule = Rule
    { rTokVersion  :: !Text
    , rMatch       :: ![Text]             -- ^ consecutive tokWords to match
    , rReplacement :: ![Text]
    , rExclude     :: ![(Text, Int, Int)] -- ^ verses the rule skips
    , rNote        :: !(Maybe Text)
    , rAuthor      :: !Text               -- ^ Ed25519 public key, hex
    , rCreated     :: !Text               -- ^ UTC timestamp
    , rSig         :: !Text               -- ^ Ed25519 signature, hex
    } deriving (Eq, Show)

instance ToJSON Rule where
    toJSON r = object
        [ "format" .= ("overlay-rule-v1" :: Text)
        , "tokenization" .= rTokVersion r
        , "match" .= rMatch r
        , "replacement" .= rReplacement r
        , "exclude" .= map refKey (rExclude r)
        , "note" .= rNote r
        , "author" .= rAuthor r
        , "created" .= rCreated r
        , "signature" .= rSig r
        ]

instance FromJSON Rule where
    parseJSON = withObject "Rule" $ \o -> do
        fmt <- o .: "format"
        unless (fmt == ("overlay-rule-v1" :: Text)) $
            fail ("unknown rule format: " <> T.unpack fmt)
        exclRaw <- o .:? "exclude" .!= []
        excl <- forM exclRaw $ \t -> maybe
            (fail ("bad exclusion ref: " <> T.unpack t)) pure (parseRefKey t)
        Rule
            <$> o .: "tokenization"
            <*> o .: "match"
            <*> o .: "replacement"
            <*> pure excl
            <*> o .:? "note"
            <*> o .: "author"
            <*> o .: "created"
            <*> o .: "signature"

data LoadedRule = LoadedRule
    { lrFile    :: !FilePath
    , lrRule    :: !Rule
    , lrStatus  :: !PatchStatus
    , lrMatches :: !Int  -- ^ corpus-wide matches, exclusions already out
    } deriving (Eq, Show)

rulesDir :: FilePath
rulesDir = "rules"

-- | The signed bytes. Frozen: any change breaks every existing signature.
-- Exclusion refs are signed in their canonical 'refKey' form.
ruleSigningBytes :: Rule -> ByteString
ruleSigningBytes r = TE.encodeUtf8 $ T.intercalate "\n"
    [ "overlay-rule-v1"
    , "tokenization:" <> rTokVersion r
    , "match:" <> T.unwords (rMatch r)
    , "replacement:" <> T.unwords (rReplacement r)
    , "exclude:" <> T.intercalate "," (map refKey (rExclude r))
    , "note:" <> fromMaybe "" (rNote r)
    , "author:" <> rAuthor r
    , "created:" <> rCreated r
    ]

verifyRuleSig :: Rule -> Either String ()
verifyRuleSig r = verifyBytes (rAuthor r) (rSig r) (ruleSigningBytes r)

mkRule :: Keys -> Text -> [Text] -> [Text] -> Maybe Text -> IO Rule
mkRule keys tokv match repl note = do
    now <- getCurrentTime
    let created = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
        cleanNote = fmap (T.replace "\n" " ") note
    pure (resignRule keys (Rule tokv match repl [] cleanNote "" created ""))

-- | Sign (or re-sign) under our key, keeping @created@ — exclusions edit
-- the rule without costing it its precedence slot.
resignRule :: Keys -> Rule -> Rule
resignRule keys r =
    let r' = r { rAuthor = kPubHex keys, rSig = "" }
    in r' { rSig = signBytes keys (ruleSigningBytes r') }

saveRule :: Rule -> IO FilePath
saveRule r = do
    createDirectoryIfMissing True rulesDir
    let slug = T.intercalate "-"
            (map (T.filter isAlphaNum) (take 3 (rMatch r)))
        name = T.unpack ("rule-" <> slug <> "-" <> fingerprint (rSig r))
            <> ".json"
        path = rulesDir </> name
    BL.writeFile path (encode r <> "\n")
    pure path

checkRule :: Corpus -> Rule -> Either String ()
checkRule corpus r = do
    unless (rTokVersion r == cTokVersion corpus) $
        Left ("tokenization mismatch: rule has " <> T.unpack (rTokVersion r)
              <> ", corpus has " <> T.unpack (cTokVersion corpus))
    when (null (rMatch r)) $ Left "empty match"
    when (null (rReplacement r)) $ Left "empty replacement"
    forM_ (rExclude r) $ \ref ->
        unless (M.member ref (cByRef corpus)) $
            Left ("excluded verse does not exist: " <> T.unpack (refKey ref))

-- | Non-overlapping occurrences of the match sequence, leftmost first.
matchSpans :: [Text] -> [Token] -> [(Int, Int)]
matchSpans [] _ = []
matchSpans ws toks = go 0 (map tokWord toks)
  where
    n = length ws
    go _ [] = []
    go i ts
        | ws `isPrefixOf` ts = (i, i + n - 1) : go (i + n) (drop n ts)
        | otherwise = go (i + 1) (drop 1 ts)

-- | Matches the rule would rewrite, with its exclusions already removed
-- (other rules and patches are not consulted — this is the authoring count).
countRuleMatches :: Corpus -> Rule -> Int
countRuleMatches corpus r = sum
    [ length (matchSpans (rMatch r) (vTokens v))
    | v <- V.toList (cVerses corpus)
    , (vBook v, vChapter v, vVerse v) `notElem` rExclude r
    ]

-- | Rule applications for one verse. Rules claim matches in list order
-- (callers pass them sorted by created); spans already claimed — point
-- patches go in @claimed0@, which is how patches always win — are skipped,
-- as are excluded verses and rules that failed verification.
ruleHitsForVerse
    :: [LoadedRule] -> [(Int, Int)] -> Verse -> [(LoadedRule, (Int, Int))]
ruleHitsForVerse rules claimed0 v = snd (foldl' step (claimed0, []) rules)
  where
    ref = (vBook v, vChapter v, vVerse v)
    step (claimed, out) lr
        | not (applies (lrStatus lr)) = (claimed, out)
        | ref `elem` rExclude (lrRule lr) = (claimed, out)
        | otherwise =
            let free s = not (any (overlaps s) claimed)
                hits = filter free
                    (matchSpans (rMatch (lrRule lr)) (vTokens v))
            in (claimed <> hits, out <> [(lr, s) | s <- hits])
    overlaps (s, e) (s', e') = s <= e' && s' <= e
    applies (PInvalid _) = False
    applies _ = True

-- | Read every rules/*.json, verify, and sort by (created, file) — the
-- order 'ruleHitsForVerse' resolves conflicts in.
loadRules :: Keys -> Corpus -> IO [LoadedRule]
loadRules keys corpus = do
    trusted <- loadTrustedKeys
    exists <- doesDirectoryExist rulesDir
    files <- if exists
        then sortOn id . filter (T.isSuffixOf ".json" . T.pack)
                <$> listDirectory rulesDir
        else pure []
    lrs <- forM files $ \f -> do
        let path = rulesDir </> f
        bytes <- try (B.readFile path)
            :: IO (Either SomeException ByteString)
        pure $ case bytes of
            Left err -> LoadedRule path placeholder
                (PInvalid ("unreadable: " <> T.pack (show err))) 0
            Right bs -> case eitherDecodeStrict bs of
                Left err -> LoadedRule path placeholder
                    (PInvalid ("parse error: " <> T.pack err)) 0
                Right r -> LoadedRule path r (statusOf trusted r)
                    (countRuleMatches corpus r)
    pure (sortOn orderKey lrs)
  where
    placeholder = Rule "" [] [] [] Nothing "" "" ""
    orderKey lr = (rCreated (lrRule lr), lrFile lr)
    statusOf trusted r =
        case verifyRuleSig r >> checkRule corpus r of
            Left err -> PInvalid (T.pack err)
            Right ()
                | rAuthor r == kPubHex keys -> POwn
                | rAuthor r `elem` trusted -> PTrusted
                | otherwise -> PUnknownKey

-- | Add a verse exclusion to one of our own rules: re-sign (keeping
-- created) and rewrite the file. The filename tracks the signature, so the
-- old file is replaced by a new one.
excludeVerse :: Keys -> FilePath -> (Text, Int, Int) -> IO (Either String FilePath)
excludeVerse keys path ref = do
    bytes <- try (B.readFile path) :: IO (Either SomeException ByteString)
    case bytes of
        Left err -> pure (Left (show err))
        Right bs -> case eitherDecodeStrict bs of
            Left err -> pure (Left err)
            Right r
                | rAuthor r /= kPubHex keys ->
                    pure (Left "not your rule — patch over it instead")
                | ref `elem` rExclude r -> pure (Right path)
                | otherwise -> do
                    let r' = resignRule keys
                            r { rExclude = rExclude r <> [ref] }
                    newPath <- saveRule r'
                    when (newPath /= path) (removeFile path)
                    pure (Right newPath)

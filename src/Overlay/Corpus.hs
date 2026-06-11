{-# LANGUAGE OverloadedStrings #-}

-- | Canonical tokenized text: types, JSON codec, loading, and rendering.
--
-- A verse is a sequence of word tokens. Patches address words by their index
-- in this sequence, so the token JSON layout and the tokenizer that produces
-- it are frozen (see 'Overlay.Canon.tokenizationVersion').
module Overlay.Corpus
    ( Token (..)
    , Verse (..)
    , Corpus (..)
    , flagAdded
    , flagDivine
    , flagTitle
    , flagPara
    , hasFlag
    , renderToken
    , renderTokens
    , verseBody
    , verseTitle
    , corpusHeader
    , loadCorpus
    , mkCorpus
    , chapterCount
    , chapterVerses
    , refKey
    , parseRefKey
    ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Vector (Vector)
import qualified Data.Vector as V

-- Token flag bits (stored in JSON, frozen).
flagAdded, flagDivine, flagTitle, flagPara :: Int
flagAdded = 1   -- ^ KJV italics: word supplied by the translators
flagDivine = 2  -- ^ divine name (LORD), traditionally small caps
flagTitle = 4   -- ^ part of a canonical superscription (psalm titles)
flagPara = 8    -- ^ a 1769 paragraph mark (¶) preceded this word

hasFlag :: Int -> Token -> Bool
hasFlag f t = tokFlags t `div` f `mod` 2 == 1

data Token = Token
    { tokPre     :: !Text    -- ^ leading punctuation, usually empty
    , tokWord    :: !Text    -- ^ the word itself, no punctuation
    , tokPost    :: !Text    -- ^ trailing punctuation
    , tokStrongs :: ![Text]  -- ^ normalized Strong's refs, e.g. [\"H7225\"]
    , tokFlags   :: !Int
    } deriving (Eq, Show)

-- Frozen JSON layout: [pre, word, post, [strongs], flags]
instance ToJSON Token where
    toJSON (Token pre w post ss fl) =
        toJSON (pre, w, post, ss, fl)

instance FromJSON Token where
    parseJSON v = do
        (pre, w, post, ss, fl) <- parseJSON v
        pure (Token pre w post ss fl)

data Verse = Verse
    { vBook    :: !Text  -- ^ OSIS book id
    , vChapter :: !Int
    , vVerse   :: !Int
    , vTokens  :: ![Token]
    } deriving (Eq, Show)

instance ToJSON Verse where
    toJSON (Verse b c n ts) =
        object ["b" .= b, "c" .= c, "v" .= n, "t" .= ts]

instance FromJSON Verse where
    parseJSON = withObject "Verse" $ \o ->
        Verse <$> o .: "b" <*> o .: "c" <*> o .: "v" <*> o .: "t"

data Corpus = Corpus
    { cVerses     :: !(Vector Verse)
    , cByRef      :: !(Map (Text, Int, Int) Int)   -- ^ (book, chap, verse) -> index
    , cChapters   :: !(Map Text Int)               -- ^ book -> chapter count
    , cChapterIx  :: !(Map (Text, Int) (Int, Int)) -- ^ (book, chap) -> (start, len)
    , cTokVersion :: !Text                         -- ^ tokenization stamp from the header
    }

corpusHeader :: Text -> Int -> Value
corpusHeader tokVersion nVerses = object
    [ "format" .= ("overlay-kjv-canonical" :: Text)
    , "tokenization" .= tokVersion
    , "source" .= ("engKJV2006eb 14.3 (CrossWire/eBible.org, public domain)" :: Text)
    , "verses" .= nVerses
    ]

renderToken :: Token -> Text
renderToken t = tokPre t <> tokWord t <> tokPost t

renderTokens :: [Token] -> Text
renderTokens = T.intercalate " " . map renderToken

-- | Verse text without any superscription.
verseBody :: Verse -> Text
verseBody = renderTokens . filter (not . hasFlag flagTitle) . vTokens

-- | Superscription text (psalm titles), empty for most verses.
verseTitle :: Verse -> Text
verseTitle = renderTokens . filter (hasFlag flagTitle) . vTokens

loadCorpus :: FilePath -> IO (Either String Corpus)
loadCorpus path = do
    raw <- BC.readFile path
    case BC.lines raw of
        [] -> pure (Left "corpus file is empty")
        (hdr : rest) -> pure $ do
            hdrVal <- maybe (Left "bad corpus header") Right
                (decodeStrict hdr :: Maybe Value)
            (declared, tokv) <- case hdrVal of
                Object o -> do
                    n <- case KM.lookup "verses" o of
                        Just n -> maybe (Left "bad verse count") Right
                            (parseMaybeInt n)
                        Nothing -> Left "header missing verse count"
                    tv <- case KM.lookup "tokenization" o of
                        Just (String t) -> Right t
                        _ -> Left "header missing tokenization version"
                    pure (n, tv)
                _ -> Left "corpus header is not an object"
            vs <- traverse parseVerse (filter (not . BC.null) rest)
            if length vs /= declared
                then Left ("verse count mismatch: header says "
                           <> show declared <> ", file has " <> show (length vs))
                else Right (mkCorpus tokv (V.fromList vs))
  where
    parseVerse l = maybe (Left ("bad verse line: " <> show (BC.take 60 l))) Right
        (decodeStrict l)
    parseMaybeInt v = case fromJSON v of
        Success n -> Just (n :: Int)
        _         -> Nothing

mkCorpus :: Text -> Vector Verse -> Corpus
mkCorpus tokv vs = Corpus
    { cVerses = vs
    , cByRef = M.fromList
        [ ((vBook v, vChapter v, vVerse v), i) | (i, v) <- indexed ]
    , cChapters = M.fromListWith max
        [ (vBook v, vChapter v) | v <- V.toList vs ]
    , cChapterIx = M.fromListWith merge
        [ ((vBook v, vChapter v), (i, 1)) | (i, v) <- indexed ]
    , cTokVersion = tokv
    }
  where
    indexed = zip [0 ..] (V.toList vs)
    -- fromListWith applies f new old; verses are contiguous per chapter
    merge (i2, n2) (i1, n1) = (min i1 i2, n1 + n2)

-- | Compact canonical ref form, e.g. \"Gen 1:7\" — used by rule exclusions
-- and thread entries, including inside signed bytes, so the format is frozen.
refKey :: (Text, Int, Int) -> Text
refKey (b, c, v) = b <> " " <> T.pack (show c) <> ":" <> T.pack (show v)

parseRefKey :: Text -> Maybe (Text, Int, Int)
parseRefKey t = case T.words (T.strip t) of
    [b, cv] -> case T.splitOn ":" cv of
        [c, v] -> (,,) b <$> readInt c <*> readInt v
        _ -> Nothing
    _ -> Nothing
  where
    readInt s = case TR.decimal s of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing

chapterCount :: Corpus -> Text -> Int
chapterCount corpus bid = M.findWithDefault 1 bid (cChapters corpus)

chapterVerses :: Corpus -> Text -> Int -> [Verse]
chapterVerses corpus bid ch =
    case M.lookup (bid, ch) (cChapterIx corpus) of
        Nothing -> []
        Just (start, len) -> V.toList (V.slice start len (cVerses corpus))

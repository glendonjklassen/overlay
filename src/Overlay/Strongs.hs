{-# LANGUAGE OverloadedStrings #-}

-- | Classic Strong's (1890) dictionary entries and the concordance index:
-- which verses contain each Strong's number. The index is derived purely from
-- the tagged text — no external cross-reference dataset.
module Overlay.Strongs
    ( StrongsEntry (..)
    , StrongsDict
    , loadStrongs
    , OccurrenceIx
    , occurrenceIndex
    , refLabel
    ) where

import Data.Aeson
import qualified Data.ByteString as B
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Overlay.Canon (Book (..), bookById)
import Overlay.Corpus

data StrongsEntry = StrongsEntry
    { seLemma :: !(Maybe Text)
    , seXlit  :: !(Maybe Text)
    , sePron  :: !(Maybe Text)
    , seDeriv :: !(Maybe Text)
    , seDef   :: !(Maybe Text)
    , seKjv   :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON StrongsEntry where
    parseJSON = withObject "StrongsEntry" $ \o -> StrongsEntry
        <$> o .:? "lemma"
        <*> o .:? "xlit"
        <*> o .:? "pron"
        <*> o .:? "derivation"
        <*> o .:? "strongs_def"
        <*> o .:? "kjv_def"

type StrongsDict = Map Text StrongsEntry

loadStrongs :: FilePath -> IO (Either String StrongsDict)
loadStrongs path = do
    raw <- B.readFile path
    pure $ case decodeStrict raw of
        Just dict -> Right dict
        Nothing   -> Left ("could not parse " <> path)

-- | Strong's ref -> verses containing it, in canonical order.
type OccurrenceIx = Map Text [(Text, Int, Int)]

occurrenceIndex :: Corpus -> OccurrenceIx
occurrenceIndex corpus = M.map reverse (V.foldl' addVerse M.empty (cVerses corpus))
  where
    addVerse ix v =
        let ref = (vBook v, vChapter v, vVerse v)
            refs = S.toList (S.fromList (concatMap tokStrongs (vTokens v)))
        in foldl' (\m r -> M.insertWith (<>) r [ref] m) ix refs

refLabel :: (Text, Int, Int) -> Text
refLabel (bid, c, v) =
    let name = maybe bid bookName (M.lookup bid bookById)
    in name <> " " <> T.pack (show c) <> ":" <> T.pack (show v)

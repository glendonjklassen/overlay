{-# LANGUAGE OverloadedStrings #-}

-- | One-shot importer: turns the raw SWORD dump and the Open Scriptures
-- Strong's dictionaries into the app's canonical data files.
--
--   data/raw/kjv.imp                     -> data/kjv.jsonl (frozen tokenization)
--                                        -> data/kjv-notes.jsonl (1769 margin notes)
--   data/raw/strongs-{hebrew,greek}-...  -> data/strongs.json
--
-- The tokenizer here defines 'tokenizationVersion'. Any change to its
-- behaviour must bump that version — existing signed patches depend on it.
module Overlay.Import
    ( importMain
    , splitBlocks
    , parseBlock
    , tokenize
    ) where

import Control.Monad (when)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import Text.HTML.TagSoup (Attribute, Tag (..), parseTags)

import Overlay.Canon (Book (..), bookByImpName, tokenizationVersion)
import Overlay.Corpus

importMain :: IO ()
importMain = do
    createDirectoryIfMissing True "data"
    impTxt <- TIO.readFile "data/raw/kjv.imp"
    let blocks = splitBlocks (T.lines impTxt)
        parsed = mapMaybe parseBlock blocks
        verses = map fst parsed
        notes = concatMap snd parsed
        nVerses = length verses
        nTokens = sum (map (length . vTokens) verses)
        nBooks = S.size (S.fromList (map vBook verses))

    when (nVerses /= 31102) $
        die ("FATAL: expected 31102 verses, got " <> show nVerses)

    BL.writeFile "data/kjv.jsonl" $ BL.intercalate "\n" $
        encode (corpusHeader tokenizationVersion nVerses) : map encode verses
    BL.writeFile "data/kjv-notes.jsonl" $ BL.intercalate "\n" $ map encode notes

    heb <- loadDict "data/raw/strongs-hebrew-dictionary.js"
    grk <- loadDict "data/raw/strongs-greek-dictionary.js"
    BL.writeFile "data/strongs.json" (encode (Object (KM.union heb grk)))

    putStrLn $ unlines
        [ "tokenization: " <> T.unpack tokenizationVersion
        , "books:   " <> show nBooks
        , "verses:  " <> show nVerses
        , "tokens:  " <> show nTokens
        , "notes:   " <> show (length notes)
        , "strongs: " <> show (KM.size heb) <> " Hebrew + "
            <> show (KM.size grk) <> " Greek"
        ]

-- ── imp file structure ──────────────────────────────────────────────────────

-- | Group "$$$Header" lines with their following content lines.
splitBlocks :: [Text] -> [(Text, [Text])]
splitBlocks = go . dropWhile (not . isHeader)
  where
    isHeader = T.isPrefixOf "$$$"
    go [] = []
    go (h : rest) =
        let (body, more) = break isHeader rest
        in (T.drop 3 h, body) : go more

-- | A block is a verse iff its header is "<known book> C:V" with V > 0.
parseBlock :: (Text, [Text]) -> Maybe (Verse, [Value])
parseBlock (hdr, body) = do
    let ws = T.words hdr
    (refTxt, nameWs) <- unsnoc ws
    (cTxt, vTxt) <- splitRef refTxt
    c <- readInt cTxt
    v <- readInt vTxt
    book <- M.lookup (T.unwords nameWs) bookByImpName
    if v < 1 || c < 1 then Nothing else do
        let (toks, noteTexts) = tokenize (T.intercalate " " body)
            bid = bookId book
            mkNote n = object ["b" .= bid, "c" .= c, "v" .= v, "note" .= n]
        pure (Verse bid c v toks, map mkNote noteTexts)
  where
    unsnoc xs = if null xs then Nothing else Just (last xs, init xs)
    splitRef r = case T.breakOn ":" r of
        (a, b) | T.null b -> Nothing
               | otherwise -> Just (a, T.drop 1 b)
    readInt t = if not (T.null t) && T.all isDigit t
        then Just (read (T.unpack t)) else Nothing

-- ── tokenizer (FROZEN — bump tokenizationVersion on any change) ─────────────

data TkSt = TkSt
    { tsStrongs :: ![[Text]]  -- stack of refs from nested <w>
    , tsAdded   :: !Int
    , tsDivine  :: !Int
    , tsTitle   :: !Int
    , tsNote    :: !Int
    , tsNoteBuf :: ![Text]    -- reversed chunks of the current note
    , tsNotes   :: ![Text]    -- reversed completed notes
    , tsToks    :: ![Token]   -- reversed tokens
    , tsPend    :: !Text      -- pending pre-punctuation for the next word
    , tsPara    :: !Bool      -- a ¶ was seen; next word gets flagPara
    }

tokenize :: Text -> ([Token], [Text])
tokenize content =
    let st0 = TkSt [] 0 0 0 0 [] [] [] "" False
        st1 = foldl step st0 (parseTags content)
        toks = case (reverse (tsToks st1), tsPend st1) of
            (ts, "") -> ts
            ([], _)  -> []
            (ts, p)  -> init ts <> [(last ts) { tokPost = tokPost (last ts) <> p }]
    in (toks, reverse (tsNotes st1))

step :: TkSt -> Tag Text -> TkSt
step st tag = case tag of
    TagOpen "w" attrs -> st { tsStrongs = strongsRefs attrs : tsStrongs st }
    TagClose "w" -> st { tsStrongs = drop 1 (tsStrongs st) }
    TagOpen "transChange" _ -> st { tsAdded = tsAdded st + 1 }
    TagClose "transChange" -> st { tsAdded = max 0 (tsAdded st - 1) }
    TagOpen "divineName" _ -> st { tsDivine = tsDivine st + 1 }
    TagClose "divineName" -> st { tsDivine = max 0 (tsDivine st - 1) }
    TagOpen "title" _ -> st { tsTitle = tsTitle st + 1 }
    TagClose "title" -> st { tsTitle = max 0 (tsTitle st - 1) }
    TagOpen "note" _ -> st { tsNote = tsNote st + 1 }
    TagClose "note"
        | tsNote st == 1 ->
            let noteTxt = T.strip (T.concat (reverse (tsNoteBuf st)))
            in st { tsNote = 0, tsNoteBuf = []
                  , tsNotes = if T.null noteTxt then tsNotes st
                              else noteTxt : tsNotes st }
        | otherwise -> st { tsNote = max 0 (tsNote st - 1) }
    TagText txt
        | tsNote st > 0 -> st { tsNoteBuf = txt : tsNoteBuf st }
        | otherwise -> foldl addWord st (T.words txt)
    _ -> st

strongsRefs :: [Attribute Text] -> [Text]
strongsRefs attrs =
    [ normalize raw
    | ("lemma", val) <- attrs
    , ref <- T.words val
    , Just raw <- [T.stripPrefix "strong:" ref]
    ]
  where
    -- H0430 -> H430 to match the dictionary's key style
    normalize r =
        let (letter, digits) = T.splitAt 1 r
            stripped = T.dropWhile (== '0') digits
        in letter <> (if T.null stripped then "0" else stripped)

openPunct, closePunct :: [Char]
openPunct = "([\x2018\x201C"
closePunct = ".,;:!?)]'\"\x2019\x201D"

addWord :: TkSt -> Text -> TkSt
addWord st0 rawChunk
    -- 1769 paragraph mark: record it as a flag on the next word, not a token
    | T.null chunk = st
    | otherwise =
        let (pre, rest) = T.span (`elem` openPunct) chunk
            core = T.dropWhileEnd (`elem` closePunct) rest
            post = T.takeWhileEnd (`elem` closePunct) rest
        in if T.null core
            -- pure punctuation: glue to previous token, else save for the next
            then case tsToks st of
                (t : ts) ->
                    st { tsToks = t { tokPost = tokPost t <> chunk } : ts }
                [] -> st { tsPend = tsPend st <> chunk }
            else
                let flags = (if tsAdded st > 0 then flagAdded else 0)
                          + (if tsDivine st > 0 then flagDivine else 0)
                          + (if tsTitle st > 0 then flagTitle else 0)
                          + (if tsPara st then flagPara else 0)
                    refs = case tsStrongs st of
                        (r : _) -> r
                        []      -> []
                    tok = Token (tsPend st <> pre) core post refs flags
                in st { tsToks = tok : tsToks st, tsPend = "", tsPara = False }
  where
    sawPara = T.any (== '\x00B6') rawChunk
    chunk = T.filter (/= '\x00B6') rawChunk
    st = st0 { tsPara = tsPara st0 || sawPara }

-- ── Strong's dictionaries ───────────────────────────────────────────────────

-- | The dictionary files are JSON wrapped in "var x = ...; module.exports".
loadDict :: FilePath -> IO (KM.KeyMap Value)
loadDict path = do
    txt <- TIO.readFile path
    let afterEq = case T.breakOn "= {" txt of
            (_, rest) | not (T.null rest) -> T.drop 2 rest
            _ -> T.dropWhile (/= '{') txt
        jsonTxt = T.dropWhileEnd (\ch -> ch == ';' || ch == '\n' || ch == ' ')
            (fst (T.breakOn "module.exports" afterEq))
    case decodeStrict (TE.encodeUtf8 jsonTxt) of
        Just (Object o) -> pure o
        _ -> die ("could not parse dictionary: " <> path)

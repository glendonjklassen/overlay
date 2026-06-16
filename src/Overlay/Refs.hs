{-# LANGUAGE OverloadedStrings #-}

module Overlay.Refs where

import Data.List (elemIndex, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Monomer

import Overlay.Canon (Book (..), bookById, bookIds)
import Overlay.Weave

displayName :: Text -> Text
displayName bid = maybe bid bookName (M.lookup bid bookById)

showt :: Show a => a -> Text
showt = T.pack . show

refText :: (Text, Int, Int) -> Text
refText (b, c, v) = displayName b <> " " <> showt c <> ":" <> showt v

-- | The verse span each book covers in a weave, in canon order: for every book
-- among the link endpoints, its lowest and highest (chapter, verse). Lets the
-- reader see at a glance which passages a weave sets side by side.
weaveSpans :: Weave -> [(Text, (Int, Int), (Int, Int))]
weaveSpans w =
    [ (b, minimum rs, maximum rs)
    | b <- books, let rs = [(c, v) | (bb, c, v) <- refs, bb == b], not (null rs) ]
  where
    refs = concatMap (\(Link a b _ _) -> [a, b]) (wLinks w)
    books = sortOn (\b -> fromMaybe maxBound (elemIndex b bookIds))
        (nub (map fst3 refs))

-- | The passages a weave wants side by side, one (book, chapter) per column, in
-- canon order. A weave spanning several books gives one track per book (at its
-- earliest chapter); a same-book weave (two creation accounts, Ps 14 / Ps 53)
-- would otherwise collapse to one column, so it splits by chapter instead.
weaveTracks :: Weave -> [(Text, Int)]
weaveTracks w =
    let refs = concatMap (\(Link a b _ _) -> [a, b]) (wLinks w)
        books = sortOn (\b -> fromMaybe maxBound (elemIndex b bookIds))
            (nub (map fst3 refs))
    in case books of
        [b] -> [ (b, c) | c <- nub (sort [c | (_, c, _) <- refs]) ]
        _   -> [ (b, minimum [c | (bb, c, _) <- refs, bb == b]) | b <- books ]

-- | A short label for a weave passage track, for the per-column picker.
trackText :: (Text, Int) -> Text
trackText (b, c) = displayName b <> " " <> showt c

-- | Compact text for one book's span: a lone verse, a verse-range within one
-- chapter, or a cross-chapter range.
spanText :: (Text, (Int, Int), (Int, Int)) -> Text
spanText (b, (c1, v1), (c2, v2))
    | (c1, v1) == (c2, v2) = refText (b, c1, v1)
    | c1 == c2             = refText (b, c1, v1) <> "–" <> showt v2
    | otherwise            = refText (b, c1, v1) <> " – " <> refText (b, c2, v2)

-- | The canon's sections as (label, first book index, last book index) over the
-- 66 books in OSIS order — for the shared canon-overview map.
canonSegments :: [(Text, Int, Int)]
canonSegments =
    [ ("Law", 0, 4)
    , ("History", 5, 16)
    , ("Wisdom", 17, 21)
    , ("Prophets", 22, 38)
    , ("Gospels", 39, 42)
    , ("Acts", 43, 43)
    , ("Letters", 44, 64)
    , ("Revelation", 65, 65)
    ]

-- | A distinct colour per pane, for its pin on the canon map (cycles past four).
paneColor :: Int -> Color
paneColor i = case i `mod` 4 of
    0 -> rgbHex "#D2B46E"
    1 -> rgbHex "#7FB4E6"
    2 -> rgbHex "#8FB88A"
    _ -> rgbHex "#D98C8C"

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

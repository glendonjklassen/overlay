{-# LANGUAGE OverloadedStrings #-}

-- | The fixed canon: book identities and ordering, plus the tokenization
-- version stamp. Patches address into tokenized text, so everything in this
-- module is part of a frozen contract — never change existing entries.
module Overlay.Canon
    ( Book (..)
    , books
    , bookByImpName
    , bookById
    , bookIds
    , tokenizationVersion
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)

-- | Bump only when the tokenizer algorithm changes; existing patches refuse
-- to apply across a bump.
--
-- tok2: pilcrows (¶) are no longer tokens; they became 'flagPara' on the
-- following word.
tokenizationVersion :: Text
tokenizationVersion = "kjv1769-tok2"

data Book = Book
    { bookId      :: !Text  -- ^ OSIS id, used in data files and patch targets
    , bookImpName :: !Text  -- ^ name as printed by mod2imp
    , bookName    :: !Text  -- ^ display name
    } deriving (Eq, Show)

books :: [Book]
books =
    [ Book "Gen" "Genesis" "Genesis"
    , Book "Exod" "Exodus" "Exodus"
    , Book "Lev" "Leviticus" "Leviticus"
    , Book "Num" "Numbers" "Numbers"
    , Book "Deut" "Deuteronomy" "Deuteronomy"
    , Book "Josh" "Joshua" "Joshua"
    , Book "Judg" "Judges" "Judges"
    , Book "Ruth" "Ruth" "Ruth"
    , Book "1Sam" "I Samuel" "1 Samuel"
    , Book "2Sam" "II Samuel" "2 Samuel"
    , Book "1Kgs" "I Kings" "1 Kings"
    , Book "2Kgs" "II Kings" "2 Kings"
    , Book "1Chr" "I Chronicles" "1 Chronicles"
    , Book "2Chr" "II Chronicles" "2 Chronicles"
    , Book "Ezra" "Ezra" "Ezra"
    , Book "Neh" "Nehemiah" "Nehemiah"
    , Book "Esth" "Esther" "Esther"
    , Book "Job" "Job" "Job"
    , Book "Ps" "Psalms" "Psalms"
    , Book "Prov" "Proverbs" "Proverbs"
    , Book "Eccl" "Ecclesiastes" "Ecclesiastes"
    , Book "Song" "Song of Solomon" "Song of Solomon"
    , Book "Isa" "Isaiah" "Isaiah"
    , Book "Jer" "Jeremiah" "Jeremiah"
    , Book "Lam" "Lamentations" "Lamentations"
    , Book "Ezek" "Ezekiel" "Ezekiel"
    , Book "Dan" "Daniel" "Daniel"
    , Book "Hos" "Hosea" "Hosea"
    , Book "Joel" "Joel" "Joel"
    , Book "Amos" "Amos" "Amos"
    , Book "Obad" "Obadiah" "Obadiah"
    , Book "Jonah" "Jonah" "Jonah"
    , Book "Mic" "Micah" "Micah"
    , Book "Nah" "Nahum" "Nahum"
    , Book "Hab" "Habakkuk" "Habakkuk"
    , Book "Zeph" "Zephaniah" "Zephaniah"
    , Book "Hag" "Haggai" "Haggai"
    , Book "Zech" "Zechariah" "Zechariah"
    , Book "Mal" "Malachi" "Malachi"
    , Book "Matt" "Matthew" "Matthew"
    , Book "Mark" "Mark" "Mark"
    , Book "Luke" "Luke" "Luke"
    , Book "John" "John" "John"
    , Book "Acts" "Acts" "Acts"
    , Book "Rom" "Romans" "Romans"
    , Book "1Cor" "I Corinthians" "1 Corinthians"
    , Book "2Cor" "II Corinthians" "2 Corinthians"
    , Book "Gal" "Galatians" "Galatians"
    , Book "Eph" "Ephesians" "Ephesians"
    , Book "Phil" "Philippians" "Philippians"
    , Book "Col" "Colossians" "Colossians"
    , Book "1Thess" "I Thessalonians" "1 Thessalonians"
    , Book "2Thess" "II Thessalonians" "2 Thessalonians"
    , Book "1Tim" "I Timothy" "1 Timothy"
    , Book "2Tim" "II Timothy" "2 Timothy"
    , Book "Titus" "Titus" "Titus"
    , Book "Phlm" "Philemon" "Philemon"
    , Book "Heb" "Hebrews" "Hebrews"
    , Book "Jas" "James" "James"
    , Book "1Pet" "I Peter" "1 Peter"
    , Book "2Pet" "II Peter" "2 Peter"
    , Book "1John" "I John" "1 John"
    , Book "2John" "II John" "2 John"
    , Book "3John" "III John" "3 John"
    , Book "Jude" "Jude" "Jude"
    , Book "Rev" "Revelation of John" "Revelation"
    ]

bookByImpName :: Map Text Book
bookByImpName = M.fromList [(bookImpName b, b) | b <- books]

bookById :: Map Text Book
bookById = M.fromList [(bookId b, b) | b <- books]

-- | Canonical order of OSIS ids.
bookIds :: [Text]
bookIds = map bookId books

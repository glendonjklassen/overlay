{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Overlay
    ( guiMain
    , checkMain
    , mkPatchCli
    , mkRuleCli
      -- exported for the test suite
    , toRVerse
    ) where

import Control.Exception (SomeException, try)
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (delete, elemIndex, find, findIndex, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Monomer
import qualified Monomer.Lens as L
import System.Directory (doesFileExist, getXdgDirectory, removeFile,
                         XdgDirectory (XdgConfig), createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath ((</>))

import Overlay.Canon (Book (..), bookById, bookIds)
import Overlay.CanonMap
import Overlay.Corpus
import Overlay.Patch
import Overlay.ReaderView
import Overlay.Rule
import Overlay.Strongs
import Overlay.Thread
import Overlay.Weave

corpusPath, strongsPath, notesPath :: FilePath
corpusPath = "data/kjv.jsonl"
strongsPath = "data/strongs.json"
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

-- | UI scale factor for the chrome: text rides the same zoom as the scripture
-- body (1.0 at the default size), so buttons/labels/panels grow with Ctrl+/-.
uiScaleOf :: AppModel -> Double
uiScaleOf m = _amBodySize m / sBodySize defaultSettings

-- | Persist settings back to config.json (best-effort), e.g. after a live zoom.
saveSettings :: Settings -> IO ()
saveSettings s = do
    dir <- getXdgDirectory XdgConfig "overlay"
    _ <- try (createDirectoryIfMissing True dir
        >> BL.writeFile (dir </> "config.json") (encode s <> "\n"))
        :: IO (Either SomeException ())
    pure ()

-- | The last-open session: which passage (book, chapter) each reading pane
-- showed. Saved on close and restored on the next launch, so the app reopens
-- where you left it instead of resetting to Genesis 1. Selections and scroll
-- offsets are transient and not persisted.
newtype Session = Session { sessPanes :: [(Text, Int)] }

instance ToJSON Session where
    toJSON (Session ps) = object
        [ "panes" .= [ object ["book" .= b, "chapter" .= c] | (b, c) <- ps ] ]

instance FromJSON Session where
    parseJSON = withObject "Session" $ \o -> do
        arr <- o .:? "panes" .!= []
        Session <$> mapM (withObject "pane" $ \p ->
            (,) <$> p .: "book" <*> p .: "chapter") arr

sessionPath :: IO FilePath
sessionPath = do
    dir <- getXdgDirectory XdgConfig "overlay"
    pure (dir </> "session.json")

-- | Persist the panes' passages. Best-effort: a write failure must never crash
-- the app on close, so errors are swallowed.
saveSession :: [PaneState] -> IO ()
saveSession panes = do
    dir <- getXdgDirectory XdgConfig "overlay"
    path <- sessionPath
    let sess = Session [ (_psBook p, _psChapter p) | p <- panes ]
    _ <- try (createDirectoryIfMissing True dir
        >> BL.writeFile path (encode sess <> "\n")) :: IO (Either SomeException ())
    pure ()

-- | Read the saved passages (raw, unvalidated). Missing or unreadable → none.
loadSession :: IO [(Text, Int)]
loadSession = do
    path <- sessionPath
    exists <- doesFileExist path
    if not exists then pure [] else do
        raw <- BC.readFile path
        pure $ either (const []) sessPanes (eitherDecodeStrict raw)

-- | Build the initial panes from a saved session, validated against the corpus:
-- unknown books are dropped, chapters clamped to range, at most four panes. An
-- empty or invalid session falls back to a single Genesis 1 pane.
restorePanes :: Corpus -> [(Text, Int)] -> [PaneState]
restorePanes corpus saved =
    case mapMaybe valid (take 4 saved) of
        []  -> [PaneState "Gen" 1 Nothing []]
        ps  -> ps
  where
    valid (b, c)
        | b `elem` bookIds =
            Just (PaneState b (max 1 (min c (chapterCount corpus b))) Nothing [])
        | otherwise = Nothing

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

-- ── environment and model ───────────────────────────────────────────────────

-- Static environment shared by the whole UI (not part of the model: it never
-- changes, and keeping it out avoids Eq checks over megabytes of text).
data Env = Env
    { envCorpus   :: Corpus
    , envStrongs  :: StrongsDict
    , envOccIx    :: OccurrenceIx
    , envKeys     :: Keys
    , envNotes    :: M.Map (Text, Int, Int) [Text]
    , envSettings :: Settings
    }

data EditTarget = EditTarget
    { etRef     :: (Text, Int, Int)
    , etSpan    :: (Int, Int)
    , etWords   :: [Text]
    , etMatches :: Int  -- ^ corpus-wide occurrences of the span's words
    , etRuleHit :: Maybe (FilePath, Text, Bool)
      -- ^ (file, \"match → repl\", is ours) when a rule rewrites this span
    } deriving (Eq, Show)

data PanelMode
    = PNone
    | PStrongs Text Text (Text, Int, Int)  -- ^ clicked word, Strong's ref, verse
    | PEdit EditTarget
    | PPatches
    | PThreads
    | PThreadView FilePath
    | PWeaves
    | PWeaveView FilePath       -- ^ inspect / edit one weave
    deriving (Eq, Show)

-- | A reading pane: where it points, plus the verses currently selected there
-- (anchored, so Shift-click extends from the anchor). One pane is ordinary
-- reading; several are parallel passages.
data PaneState = PaneState
    { _psBook    :: !Text
    , _psChapter :: !Int
    , _psAnchor  :: !(Maybe Int)  -- ^ last verse clicked, for Shift-extend
    , _psSel     :: ![Int]        -- ^ selected verse numbers
    } deriving (Eq, Show)

data AppModel = AppModel
    { _amPanel       :: PanelMode
    , _amNotesOn     :: Bool
    , _amHeatmapOn   :: Bool  -- ^ shade verses by their number of weave witnesses
    , _amLinesOn     :: Bool  -- ^ draw the weave connector lines across panes
    , _amPatches     :: [LoadedPatch]
    , _amRules       :: [LoadedRule]
    , _amThreads     :: [LoadedThread]
    , _amReplace     :: Text
    , _amNote        :: Text
    , _amEverywhere  :: Bool  -- ^ editor scope: save as rule, not patch
    , _amThreadPick  :: Text  -- ^ existing thread chosen in the editor
    , _amThreadNew   :: Text  -- ^ new thread name typed in the editor
    , _amThreadNotes :: Text  -- ^ notes draft for the open thread
    , _amStatus      :: Text
    -- weaves: a graph of verse links, shown across reading panes
    , _amPanes       :: [PaneState]
    , _amWeaves      :: [LoadedWeave]
    , _amWeaveNew    :: Text       -- ^ new weave name
    , _amWeaveKind   :: WeaveKind  -- ^ kind for new / linked / inspected weave
    , _amWeaveNotes  :: Text       -- ^ notes draft for the inspected weave
    , _amCombinePick :: Text       -- ^ weave to combine into the inspected one
    , _amCompare     :: Maybe ((Text, Int, Int), Double, Double)
      -- ^ hovered linked verse + window x,y, for the floating compare card
    , _amBodySize    :: Double   -- ^ live scripture text size (Ctrl +/-/0 zoom)
    } deriving (Eq, Show)

data AppEvent
    = EvInit
    | EvWordClicked RTok
    | EvWordAlt RTok
    | EvSpanSelected (Text, Int, Int) (Int, Int)
    | EvVerseClicked Int (Text, Int, Int) Bool
    | EvGoRef Text Int
    | EvClosePanel
    | EvTogglePatches
    | EvSavePatch
    | EvDeletePatch FilePath
    | EvPatchesLoaded [LoadedPatch] Text
    | EvDeleteRule FilePath
    | EvExcludeRule FilePath (Text, Int, Int)
    | EvRulesLoaded [LoadedRule] Text
    | EvToggleThreads
    | EvShowThreads
    | EvOpenThread FilePath
    | EvAddToThread
    | EvSaveThreadNotes FilePath
    | EvDeleteThread FilePath
    | EvDeleteThreadEntry FilePath Int
    | EvThreadsLoaded [LoadedThread] Text
    -- panes
    | EvAddPane Int
    | EvClosePane Int
    | EvPaneBook Int Text
    | EvPaneChapter Int Int
    | EvPanePrev Int
    | EvPaneNext Int
    -- weaves
    | EvToggleWeaves
    | EvShowWeaves
    | EvOpenWeave FilePath
    | EvNewWeave
    | EvLink
    | EvSetWeaveKind WeaveKind
    | EvSaveWeaveNotes
    | EvRemoveLink Link
    | EvApproveLink Link Bool
    | EvApproveWeave Bool
    | EvCombineWeave Text
    | EvDeleteWeave FilePath
    | EvWeavesLoaded [LoadedWeave] Text
    | EvStatus Text
    | EvSaveSession
    | EvVerseInspect (Text, Int, Int) Double Double
    | EvCloseCompare
    | EvApproveLinkIn FilePath Link Bool
    | EvRejectLinkIn FilePath Link
    | EvZoom Double  -- ^ change scripture text size by delta px (0 = reset)
    | EvNoop
    deriving (Eq, Show)

makeLenses ''PaneState
makeLenses ''AppModel

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

-- ── overlay composition ─────────────────────────────────────────────────────

-- | Compose the patch and rule overlays over a verse, producing renderable
-- tokens. Patches claim their spans first, so rules never override them.
-- @marks@ are word spans to highlight (thread passages / weave selection).
-- | Per-verse "witnesses": for each verse, the other verses linked to it by
-- any weave (with the edge label). The count is its degree in the union of all
-- weave graphs — the heatmap and the verse-level cross-reference list both read
-- from this.
type VRef3 = (Text, Int, Int)

witnessIndex :: [LoadedWeave] -> M.Map VRef3 [(VRef3, Text)]
witnessIndex weaves = M.map dedup $ M.fromListWith (<>) $ concat
    [ [ (lA l, [(lB l, lLabel l)]), (lB l, [(lA l, lLabel l)]) ]
    | lw <- weaves, l <- wLinks (lwWeave lw) ]
  where
    -- one entry per distinct neighbour verse, keeping a non-empty label if any
    dedup xs = M.toList (M.fromListWith pickLabel xs)
    pickLabel a b = if T.null a then b else a

-- | Sort verse references into canonical reading order (book, chapter, verse).
canonKey :: VRef3 -> (Int, Int, Int)
canonKey (b, c, v) = (fromMaybe maxBound (elemIndex b bookIds), c, v)

sortOnCanon :: [(VRef3, Text)] -> [(VRef3, Text)]
sortOnCanon = sortOn (canonKey . fst)

-- | Where a weave first lands in the canon — the earliest verse among all its
-- link endpoints — for ordering the weaves list. Linkless weaves sort last.
weaveStartKey :: Weave -> (Int, Int, Int)
weaveStartKey w = case concatMap (\l -> [lA l, lB l]) (wLinks w) of
    [] -> (maxBound, maxBound, maxBound)
    es -> minimum (map canonKey es)

-- | Heat tier 0…4 for a witness count, relative to the busiest verse so the
-- scale always spans the data on hand: 0 none, then four even bands.
heatTierFor :: Int -> Int -> Int
heatTierFor maxWit d
    | d <= 0 || maxWit <= 0 = 0
    | otherwise = max 1 (min 4 (ceiling (fromIntegral d / fromIntegral maxWit * 4 :: Double)))

toRVerse
    :: Text -> [LoadedPatch] -> [LoadedRule] -> [(Int, Int)] -> [Text]
    -> Int -> Verse -> RVerse
toRVerse ownHex patches rules marks notes heat v =
    RVerse (vVerse v) (walk 0 (vTokens v)) notes heat
  where
    ref = (vBook v, vChapter v, vVerse v)
    patchEdits =
        [ (pSpan p, pReplacement p, patchInfo ownHex lp)
        | lp <- acceptedForVerse patches ref
        , let p = lpPatch lp
        ]
    ruleEdits =
        [ (s, rReplacement (lrRule lr), ruleInfo ownHex lr)
        | (lr, s) <- ruleHitsForVerse rules
            [sp | (sp, _, _) <- patchEdits] v
        ]
    byStart = M.fromList
        [ (fst s, edit) | edit@(s, _, _) <- patchEdits <> ruleEdits ]

    marked i = any (\(s, e) -> i >= s && i <= e) marks

    walk _ [] = []
    walk i toks@(t : rest) = case M.lookup i byStart of
        Nothing -> RTok t ref i Nothing (marked i) : walk (i + 1) rest
        Just ((s, e), repl, info) ->
            let spanToks = take (e - s + 1) toks
                firstTok = head spanToks
                lastTok = last spanToks
                strongs = nub (concatMap tokStrongs spanToks)
                keepFlags = tokFlags firstTok
                    `div` flagTitle `mod` 2 * flagTitle
                    + tokFlags firstTok `div` flagPara `mod` 2 * flagPara
                n = length repl
                mark = any marked [s .. e]
                mkTok j w = Token
                    { tokPre = if j == 0 then tokPre firstTok else ""
                    , tokWord = w
                    , tokPost = if j == n - 1 then tokPost lastTok else ""
                    , tokStrongs = strongs
                    , tokFlags = keepFlags
                    }
            in [ RTok (mkTok j w) ref i (Just info) mark
               | (j, w) <- zip [0 ..] repl
               ]
               <> walk (e + 1) (drop (e - s + 1) toks)

patchInfo :: Text -> LoadedPatch -> PatchInfo
patchInfo ownHex lp = PatchInfo
    { piLines =
        [ statusLine
        , "was: " <> T.unwords (pOriginal p)
        , "by " <> author <> " · " <> T.take 10 (pCreated p)
        ]
        <> maybe [] (\n -> ["note: " <> n]) (pNote p)
    , piWarn = warn
    }
  where
    p = lpPatch lp
    warn = lpStatus lp == PUnknownKey
    author
        | pAuthor p == ownHex = "you (" <> fingerprint (pAuthor p) <> ")"
        | otherwise = fingerprint (pAuthor p) <> "…"
    statusLine = case lpStatus lp of
        POwn -> "patched · ed25519 verified"
        PTrusted -> "patched · ed25519 verified (trusted key)"
        PUnknownKey -> "patched · valid signature, UNKNOWN KEY"
        PInvalid r -> "INVALID: " <> r  -- not rendered; invalid never applies

ruleInfo :: Text -> LoadedRule -> PatchInfo
ruleInfo ownHex lr = PatchInfo
    { piLines =
        [ statusLine
        , "was: " <> T.unwords (rMatch r)
        , "by " <> author <> " · " <> T.take 10 (rCreated r)
        ]
        <> maybe [] (\n -> ["note: " <> n]) (rNote r)
    , piWarn = lrStatus lr == PUnknownKey
    }
  where
    r = lrRule lr
    author
        | rAuthor r == ownHex = "you (" <> fingerprint (rAuthor r) <> ")"
        | otherwise = fingerprint (rAuthor r) <> "…"
    statusLine = case lrStatus lr of
        POwn -> "rule · ed25519 verified"
        PTrusted -> "rule · ed25519 verified (trusted key)"
        PUnknownKey -> "rule · valid signature, UNKNOWN KEY"
        PInvalid reason -> "INVALID: " <> reason

statusText :: PatchStatus -> Text
statusText st = case st of
    POwn -> "verified (you)"
    PTrusted -> "verified (trusted)"
    PUnknownKey -> "valid sig, unknown key"
    PInvalid r -> "INVALID: " <> r

-- ── UI ──────────────────────────────────────────────────────────────────────

panelW, panelInnerW :: Double
panelW = 330
panelInnerW = panelW - 24

-- | Secondary label text. Monomer's built-in 'gray' (#808080) only reaches
-- ~3.7:1 on the dark panels — below WCAG AA at the small sizes used here — so
-- the side panels use this lighter warm grey (~7:1) for readable section
-- labels and help text.
muted :: Color
muted = rgbHex "#B7B2A8"

buildUI :: Env -> WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI env wenv model = widgetTree
  where
    -- UI scale: chrome text rides the same zoom as the scripture body, so it
    -- grows together (Ctrl +/-/scroll). 1.0 at the default body size.
    sc = model ^. amBodySize / sBodySize defaultSettings
    corpus = envCorpus env
    own = kPubHex (envKeys env)
    patches = model ^. amPatches
    rules = model ^. amRules
    threads = model ^. amThreads
    panes = model ^. amPanes
    npanes = length panes

    canLink = length (filter (not . null . _psSel) panes) >= 2

    header = hstack $
        [ labeledCheckbox "1769 notes" amNotesOn `styleBasic` [textSize (12 * sc)]
        , spacer
        , labeledCheckbox "heatmap" amHeatmapOn `styleBasic` [textSize (12 * sc)]
        , spacer
        , labeledCheckbox "links" amLinesOn `styleBasic` [textSize (12 * sc)]
        , spacer
        , button ("patches (" <> showt (length patches + length rules) <> ")")
            EvTogglePatches `styleBasic` [textSize (12 * sc)]
        , spacer
        , button ("threads (" <> showt (length threads) <> ")")
            EvToggleThreads `styleBasic` [textSize (12 * sc)]
        , spacer
        , button ("weaves (" <> showt (length (model ^. amWeaves)) <> ")")
            EvToggleWeaves `styleBasic` [textSize (12 * sc)]
        ]
        <> (if canLink
            then [ spacer, button "+ link" EvLink
                    `styleBasic` [textSize (12 * sc), textColor (rgbHex "#C9A24B")] ]
            else [])
        <>
        [ spacer
        , label (model ^. amStatus) `styleBasic` [textSize (11 * sc), textColor muted]
        , filler
        , label "overlay" `styleBasic` [textColor muted, textSize (12 * sc)]
        ]

    bookRow b = label (displayName b) `styleBasic` [textSize (12 * sc)]
    chRow n = label (showt n) `styleBasic` [textSize (12 * sc)]

    notesFor v = if model ^. amNotesOn
        then M.findWithDefault [] (vBook v, vChapter v, vVerse v) (envNotes env)
        else []

    -- thread passages highlight while a thread is open
    threadMarks = case model ^. amPanel of
        PThreadView f -> M.fromListWith (<>)
            [ (teRef e, [teSpan e])
            | lt <- threads, ltFile lt == f, e <- thEntries (ltThread lt) ]
        _ -> M.empty

    marksFor p v =
        let ref = (vBook v, vChapter v, vVerse v)
            sel = [(0, maxBound) | vVerse v `elem` _psSel p]
        in M.findWithDefault [] ref threadMarks <> sel

    -- witness graph (all weaves): adjacency for the sidebar, counts for heat
    witAdj = witnessIndex (model ^. amWeaves)
    witCount = M.map length witAdj
    maxWit = if M.null witCount then 0 else maximum (M.elems witCount)
    heatFor v = if model ^. amHeatmapOn
        then heatTierFor maxWit
            (M.findWithDefault 0 (vBook v, vChapter v, vVerse v) witCount)
        else 0

    paneColumn i p = ColumnCfg
        (showt i <> ":" <> _psBook p <> ":" <> showt (_psChapter p))
        [ toRVerse own patches rules (marksFor p v) (notesFor v) (heatFor v) v
        | v <- chapterVerses corpus (_psBook p) (_psChapter p) ]
        (alignFor p)

    -- where this pane sits in the canon, 0 (Genesis 1) … 1 (Revelation): book
    -- index plus how far through the book's chapters, over the 66 books
    canonPosOf p =
        let bi = fromMaybe 0 (elemIndex (_psBook p) bookIds)
            nb = max 1 (length bookIds)
            nc = max 1 (chapterCount corpus (_psBook p))
        in (fromIntegral bi + fromIntegral (_psChapter p - 1) / fromIntegral nc)
            / fromIntegral nb

    -- when a weave is open, line its first link up: the pane showing an
    -- endpoint of that link scrolls so the endpoint verse sits at the top
    openWeaveFirstLink = case model ^. amPanel of
        PWeaveView file ->
            case find ((== file) . lwFile) (model ^. amWeaves) of
                Just lw -> case wLinks (lwWeave lw) of
                    (l : _) -> Just l
                    []      -> Nothing
                Nothing -> Nothing
        _ -> Nothing
    alignFor p = case openWeaveFirstLink of
        Just (Link a b _ _) ->
            let m (bk, c, v) = if bk == _psBook p && c == _psChapter p
                    then Just v else Nothing
            in case m a of
                Just v  -> Just v
                Nothing -> m b
        Nothing -> Nothing

    -- ambient: every weave link whose verses are both visible in some pane
    visibleRefs = Set.fromList
        [ (_psBook p, _psChapter p, vVerse v)
        | p <- panes, v <- chapterVerses corpus (_psBook p) (_psChapter p) ]
    ambientLinks = nub
        [ (a, b, lbl)
        | lw <- model ^. amWeaves, Link a b lbl _ <- wLinks (lwWeave lw)
        , a `Set.member` visibleRefs, b `Set.member` visibleRefs ]

    navStrip i p = hstack
        ( [ dropdown_ (amPanes . singular (ix i) . psBook) bookIds
                bookRow bookRow [onChange (EvPaneBook i)]
                `styleBasic` [width 150, textSize (12 * sc)]
          , spacer
          , dropdown_ (amPanes . singular (ix i) . psChapter)
                [1 .. chapterCount corpus (_psBook p)] chRow chRow
                [onChange (EvPaneChapter i)]
                `styleBasic` [width 70, textSize (12 * sc)]
                `nodeKey` ("paneCh_" <> showt i <> "_" <> _psBook p)
          , button "<" (EvPanePrev i) `styleBasic` [textSize (12 * sc), padding 2]
          , button ">" (EvPaneNext i) `styleBasic` [textSize (12 * sc), padding 2]
          , filler
          , button "+ pane" (EvAddPane i)
                `styleBasic` [textSize (11 * sc), padding 3]
          ]
          <> [ button "x" (EvClosePane i)
                `styleBasic` [textSize (11 * sc), padding 3, textColor (rgbHex "#B07A7A")]
             | npanes > 1 ]
        ) `styleBasic` [padding 4, bgColor (rgbHex "#202225")]

    navRow = hgrid (zipWith navStrip [0 ..] panes)

    reader = readerView ReaderCfg
        { rcColumns = zipWith paneColumn [0 :: Int ..] panes
        , rcBodySize = model ^. amBodySize
        , rcLineSpacing = sLineSpacing (envSettings env)
        , rcLinks = if model ^. amLinesOn then ambientLinks else []
        , rcHeatOn = model ^. amHeatmapOn
        , rcOnWordClick = EvWordClicked
        , rcOnWordAlt = EvWordAlt
        , rcOnSpanSelect = EvSpanSelected
        , rcOnVerseClick = EvVerseClicked
        , rcOnPaneNav = \c d -> if d < 0 then EvPanePrev c else EvPaneNext c
        , rcOnVerseInspect = EvVerseInspect
        , rcOnZoom = EvZoom
        } `nodeKey` "reader"

    sidePanel = case model ^. amPanel of
        PNone -> []
        PEdit et -> [editorPanel model et]
        PStrongs word ref vref ->
            [strongsPanel env sc (sortOnCanon (M.findWithDefault [] vref witAdj))
                vref (word, ref)]
        PPatches -> [patchesPanel model]
        PThreads -> [threadsPanel model]
        PThreadView f -> [threadViewPanel model f]
        PWeaves -> [weavesPanel model]
        PWeaveView f -> [weaveViewPanel model f]

    -- the shared canon overview strip: one map, a pin per pane
    canonMap = canonMapView CanonMapCfg
        { cmcSegs = [ CanonSeg lbl (fromIntegral lo / nb)
                        (fromIntegral (hi + 1) / nb) (lo >= otNT)
                    | (lbl, lo, hi) <- canonSegments ]
        , cmcPins = [ CanonPin (canonPosOf p) (_psBook p) (paneColor i)
                    | (i, p) <- zip [0 :: Int ..] panes ]
        , cmcDivider = fromIntegral otNT / nb
        }
      where nb = fromIntegral (length bookIds)
            otNT = 39  -- Matthew is the 40th book (index 39)

    mainArea = vstack [navRow, reader, canonMap]

    baseTree = vstack
        [ header `styleBasic` [padding 10]
        , hstack (mainArea : sidePanel)
        ]

    -- every passage linked to a verse, with the weave + edge it rides, so the
    -- compare card can approve or reject each correspondence in place
    comparePassagesFor ref = sortOn (\(r, _, _, _) -> canonKey r)
        [ (other, lLabel l, lwFile lw, l)
        | lw <- model ^. amWeaves, l <- wLinks (lwWeave lw)
        , lA l == ref || lB l == ref
        , let other = if lA l == ref then lB l else lA l ]

    verseTextOf ref = maybe "" (T.unwords . map renderToken . vTokens)
        (M.lookup ref (cByRef corpus) >>= (cVerses corpus V.!?))

    compareRow (other, lbl, file, l) = vstack_ [childSpacing_ 2] $
        [ hstack
            [ box_ [onClick (EvGoRef (fst3 other) (snd3 other)), alignLeft]
                (label (refText other)
                    `styleBasic` [textSize (12 * sc), textColor lightSkyBlue])
            , filler
            , button (if lApproved l then "✓" else "approve")
                (EvApproveLinkIn file l (not (lApproved l)))
                `styleBasic` [textSize (10 * sc), padding 2, textColor (rgbHex "#EAE6DE")
                    , bgColor (if lApproved l then rgbHex "#3E5239" else rgbHex "#403A30")]
            , button "reject" (EvRejectLinkIn file l)
                `styleBasic` [textSize (10 * sc), padding 2, textColor (rgbHex "#EAE6DE")
                    , bgColor (rgbHex "#5A3A36")]
            ]
        , wrapLabel (verseTextOf other)
            `styleBasic` [textSize (model ^. amBodySize), textColor lightGray, width 336]
        ]
        <> [ wrapLabel ("· " <> lbl) `styleBasic`
                 [textSize (10 * sc), textColor (rgbHex "#D2B46E"), width 336] | not (T.null lbl) ]

    -- header pinned; the verse + parallels scroll, so a verse with many
    -- witnesses never runs off the bottom of the screen
    compareCard ref = vstack_ [childSpacing_ 6]
        [ hstack
            [ label (refText ref) `styleBasic` [textSize (13 * sc), textColor lightGray]
            , filler
            , button "✕" EvCloseCompare `styleBasic` [textSize (11 * sc), padding 2]
            ]
        , vscroll_ [wheelRate 50] (vstack_ [childSpacing_ 6] $
            [ wrapLabel (verseTextOf ref)
                `styleBasic` [textSize (model ^. amBodySize), width 336]
            , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
            , label "parallels" `styleBasic` [textSize (10 * sc), textColor muted]
            ]
            <> map compareRow (comparePassagesFor ref))
        ]

    -- floating overlay near the hovered verse; empty area is click-through
    compareOverlay = case model ^. amCompare of
        Just (ref, x, y) | not (null (comparePassagesFor ref)) ->
            let Size winW winH = wenv ^. L.windowSize
                cardW = 360
                maxH = min 700 (max 200 (winH - 80))
                px = max 10 (min x (winW - cardW - 12))
                py = max 10 (min y (winH - maxH - 20))
            in [ box_ [alignLeft, alignTop, ignoreEmptyArea]
                    (compareCard ref `styleBasic`
                        [width cardW, maxHeight maxH, padding 10, radius 6
                        , bgColor (rgbHex "#23262B"), border 1 (rgbHex "#3A3F45")])
                    `styleBasic` [paddingL px, paddingT py] ]
        _ -> []

    widgetTree = zstack (baseTree : compareOverlay)

-- multiline label for panel body text (see Label.getSizeReq); resizeFactorH 0
-- fixes the wrapped height so a sibling vscroll can't squeeze it
wrapLabel :: Text -> WidgetNode AppModel AppEvent
wrapLabel t = label_ t [multiline, resizeFactorH 0]

captionField :: Double -> Text -> Maybe Text -> WidgetNode AppModel AppEvent
captionField sc name mval = widgetMaybe mval $ \v -> vstack_ [childSpacing_ 2]
    [ label name `styleBasic` [textSize (10 * sc), textColor muted]
    , wrapLabel v `styleBasic` [textSize (13 * sc), width panelInnerW]
    ]

panelBox :: [WidgetNode AppModel AppEvent] -> WidgetNode AppModel AppEvent
panelBox items = vstack_ [childSpacing_ 8] items
    `styleBasic` [width panelW, padding 12, bgColor (rgbHex "#26282B")]

panelHeader :: Double -> Text -> AppEvent -> WidgetNode AppModel AppEvent
panelHeader sc title closeEvt = hstack
    [ label title `styleBasic` [textSize (15 * sc)]
    , filler
    , button "✕" closeEvt `styleBasic` [textSize (11 * sc), padding 4]
    ]

-- | The Ctrl-click side panel: verse-level cross-references (weave witnesses)
-- on top, then word-level Strong's detail below — both levels in one place.
strongsPanel
    :: Env -> Double -> [((Text, Int, Int), Text)] -> (Text, Int, Int)
    -> (Text, Text) -> WidgetNode AppModel AppEvent
strongsPanel env sc witnesses vref (word, ref) = panel
  where
    entry = M.lookup ref (envStrongs env)
    occs = M.findWithDefault [] ref (envOccIx env)
    occShown = take 200 occs
    occMore = length occs - length occShown

    occRow r@(b, c, _) = box_ [onClick (EvGoRef b c), alignLeft]
        (label (refLabel r) `styleBasic` [textSize (12 * sc), textColor lightSkyBlue])
        `styleHover` [bgColor (rgbHex "#3A3F45")]

    -- a cross-referenced verse: jump on click, shared wording underneath
    witRow (r@(b, c, _), lbl) = box_ [onClick (EvGoRef b c), alignLeft]
        (vstack_ [childSpacing_ 1] $
            (label (refLabel r) `styleBasic` [textSize (12 * sc), textColor lightSkyBlue])
            : [ wrapLabel lbl `styleBasic`
                  [textSize (10 * sc), textColor (rgbHex "#D2B46E"), width panelInnerW]
              | not (T.null lbl) ])
        `styleHover` [bgColor (rgbHex "#3A3F45")]

    verseSection =
        [ label "this verse" `styleBasic` [textSize (10 * sc), textColor muted]
        , label (refText vref) `styleBasic` [textSize (14 * sc), textColor lightGray]
        , label (showt (length witnesses) <> " cross-reference"
                <> (if length witnesses == 1 then "" else "s")
                <> " (witnesses)")
            `styleBasic` [textSize (11 * sc), textColor muted]
        ]
        <> map witRow witnesses
        <> [ wrapLabel "no linked passages yet — add weave links to build them"
                `styleBasic` [textSize (10 * sc), textColor muted, width panelInnerW]
           | null witnesses ]
        <> [separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]]

    wordSection =
        [ label ("word — " <> ref) `styleBasic` [textSize (10 * sc), textColor muted]
        , widgetMaybe (entry >>= seLemma) $ \l ->
            label l `styleBasic` [textSize (22 * sc)]
        , hstack_ [childSpacing_ 8]
            [ widgetMaybe (entry >>= seXlit) $ \x ->
                label x `styleBasic` [textSize (13 * sc), textColor lightGray]
            , widgetMaybe (entry >>= sePron) $ \p ->
                label p `styleBasic` [textSize (12 * sc), textColor muted]
            ]
        , captionField sc "derivation" (entry >>= seDeriv)
        , captionField sc "definition" (entry >>= seDef)
        , captionField sc "KJV renderings" (entry >>= seKjv)
        , label (showt (length occs) <> " occurrences")
            `styleBasic` [textSize (11 * sc), textColor muted]
        ]
        <> [occRow r | r <- occShown]
        <> [label ("… and " <> showt occMore <> " more")
                `styleBasic` [textSize (11 * sc), textColor muted]
           | occMore > 0]

    panel = panelBox
        [ panelHeader sc (refText vref <> " — " <> word) EvClosePanel
        , vscroll $ vstack_ [childSpacing_ 8] (verseSection <> wordSection)
        ]

editorPanel :: AppModel -> EditTarget -> WidgetNode AppModel AppEvent
editorPanel model et = panelBox $
    [ panelHeader sc (if everywhere then "new rule" else "new patch") EvClosePanel
    , wrapLabel ("For modernizing archaisms only — e.g. fourscore → eighty. "
        <> "Never to add to, take from, or change the meaning of the text.")
        `styleBasic` [textSize (10 * sc), textColor (rgbHex "#C99A4B"), width panelInnerW]
    , label (refText (etRef et) <> ", words "
        <> showt (fst (etSpan et)) <> "–" <> showt (snd (etSpan et)))
        `styleBasic` [textSize (11 * sc), textColor muted]
    , wrapLabel ("replacing: " <> T.unwords (etWords et))
        `styleBasic` [textSize (13 * sc), width panelInnerW]
    , widgetMaybe (etRuleHit et) ruleHitBox
    , label "replacement" `styleBasic` [textSize (10 * sc), textColor muted]
    , textField amReplace `nodeKey` "replace"
    , label "note (optional)" `styleBasic` [textSize (10 * sc), textColor muted]
    , textField amNote
    , label "scope" `styleBasic` [textSize (10 * sc), textColor muted]
    , labeledRadio "this verse only" False amEverywhere
        `styleBasic` [textSize (12 * sc)]
    , labeledRadio ("everywhere — " <> showt (etMatches et) <> " match"
        <> (if etMatches et == 1 then "" else "es") <> " (rule)")
        True amEverywhere
        `styleBasic` [textSize (12 * sc)]
    , spacer
    , hstack
        [ button (if everywhere then "Sign & save rule" else "Sign & save")
            EvSavePatch
        , spacer
        , button "Cancel" EvClosePanel
        ]
    , wrapLabel ("signed with your key, applied instantly; manage from the "
        <> "patches panel")
        `styleBasic` [textSize (10 * sc), textColor muted, width panelInnerW]
    , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
    , label "add to thread (the note above travels with it)"
        `styleBasic` [textSize (10 * sc), textColor muted]
    ]
    <> [ textDropdown amThreadPick threadNames `styleBasic` [textSize (12 * sc)]
       | not (null threadNames) ]
    <> [ textField_ amThreadNew
            [placeholder (if null threadNames
                then "thread name" else "or a new thread name")]
       , box_ [alignLeft] (button "Add span to thread" EvAddToThread
            `styleBasic` [textSize (11 * sc), padding 4])
       ]
  where
    sc = uiScaleOf model
    everywhere = model ^. amEverywhere
    threadNames = [thName (ltThread lt) | lt <- model ^. amThreads]
    ruleHitBox (file, desc, own) = vstack_ [childSpacing_ 4] $
        [ wrapLabel ("this span is rewritten by rule: " <> desc)
            `styleBasic` [textSize (10 * sc), textColor muted, width panelInnerW]
        ]
        <> [ box_ [alignLeft]
                (button "exclude this verse from the rule"
                    (EvExcludeRule file (etRef et))
                    `styleBasic` [textSize (11 * sc), padding 4])
           | own ]

patchesPanel :: AppModel -> WidgetNode AppModel AppEvent
patchesPanel model = panelBox
    [ panelHeader sc "patches & rules" EvClosePanel
    , if null lps && null lrs
        then label "none yet — right-click a word, or drag across several"
            `styleBasic` [textSize (12 * sc), textColor muted]
        else vscroll (vstack_ [childSpacing_ 6] rows)
    ]
  where
    sc = uiScaleOf model
    lps = model ^. amPatches
    lrs = model ^. amRules
    rows =
        [ sectionLabel "patches" | not (null lps) ]
        <> map row lps
        <> [ sectionLabel "rules" | not (null lrs) ]
        <> map ruleRow lrs
    sectionLabel t = label t `styleBasic` [textSize (10 * sc), textColor muted]
    ruleRow lr =
        let r = lrRule lr
            excl = case length (rExclude r) of
                0 -> ""
                k -> " · " <> showt k <> " excluded"
        in vstack_ [childSpacing_ 2]
            [ hstack
                [ wrapLabel (T.unwords (rMatch r) <> " → "
                    <> T.unwords (rReplacement r))
                    `styleBasic` [textSize (12 * sc), width (panelInnerW - 64)]
                , filler
                , button "delete" (EvDeleteRule (lrFile lr))
                    `styleBasic` [textSize (10 * sc), padding 3]
                ]
            , label (statusText (lrStatus lr) <> " · "
                <> showt (lrMatches lr) <> " places" <> excl <> " · "
                <> T.take 10 (rCreated r))
                `styleBasic` [textSize (10 * sc), textColor muted]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]
    row lp =
        let p = lpPatch lp
            ref = (pBook p, pChapter p, pVerse p)
        in vstack_ [childSpacing_ 2]
            [ hstack
                [ box_ [onClick (EvGoRef (pBook p) (pChapter p)), alignLeft]
                    (label (refText ref)
                        `styleBasic` [textSize (12 * sc), textColor lightSkyBlue])
                , filler
                , button "delete" (EvDeletePatch (lpFile lp))
                    `styleBasic` [textSize (10 * sc), padding 3]
                ]
            , wrapLabel (T.unwords (pOriginal p) <> " → "
                <> T.unwords (pReplacement p))
                `styleBasic` [textSize (12 * sc), width (panelInnerW - 8)]
            , label (statusText (lpStatus lp) <> " · "
                <> T.take 10 (pCreated p))
                `styleBasic` [textSize (10 * sc), textColor muted]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]

threadsPanel :: AppModel -> WidgetNode AppModel AppEvent
threadsPanel model = panelBox
    [ panelHeader sc "threads" EvClosePanel
    , if null lts
        then wrapLabel ("none yet — right-click a word or drag a span, "
                <> "then \"add span to thread\"")
            `styleBasic` [textSize (12 * sc), textColor muted, width panelInnerW]
        else vscroll (vstack_ [childSpacing_ 6] (map row lts))
    ]
  where
    sc = uiScaleOf model
    lts = model ^. amThreads
    row lt =
        let t = ltThread lt
        in box_ [onClick (EvOpenThread (ltFile lt)), alignLeft]
            (vstack_ [childSpacing_ 2]
                [ label (thName t)
                    `styleBasic` [textSize (13 * sc), textColor lightSkyBlue]
                , label (showt (length (thEntries t)) <> " passages · since "
                    <> T.take 10 (thCreated t))
                    `styleBasic` [textSize (10 * sc), textColor muted]
                ])
            `styleHover` [bgColor (rgbHex "#3A3F45")]

threadViewPanel :: AppModel -> FilePath -> WidgetNode AppModel AppEvent
threadViewPanel model file =
    case find ((== file) . ltFile) (model ^. amThreads) of
        Nothing -> panelBox
            [ panelHeader sc "thread" EvClosePanel
            , label "thread not found" `styleBasic` [textSize (12 * sc), textColor muted]
            ]
        Just lt -> render (ltThread lt)
  where
    sc = uiScaleOf model
    render t = panelBox
        [ panelHeader sc (thName t) EvClosePanel
        , hstack
            [ button "← all threads" EvShowThreads
                `styleBasic` [textSize (10 * sc), padding 3]
            , filler
            , button "delete thread" (EvDeleteThread file)
                `styleBasic` [textSize (10 * sc), padding 3]
            ]
        , label (showt (length (thEntries t)) <> " passages · since "
            <> T.take 10 (thCreated t))
            `styleBasic` [textSize (11 * sc), textColor muted]
        , label "thread notes" `styleBasic` [textSize (10 * sc), textColor muted]
        , textArea amThreadNotes
            `styleBasic` [textSize (12 * sc), height 140]
            `nodeKey` "threadNotes"
        , box_ [alignLeft] (button "Save notes" (EvSaveThreadNotes file)
            `styleBasic` [textSize (11 * sc), padding 4])
        , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
        , label "passages (highlighted in the text)"
            `styleBasic` [textSize (10 * sc), textColor muted]
        , vscroll (vstack_ [childSpacing_ 6]
            (zipWith entryRow [0 ..] (thEntries t)))
        ]
    entryRow i e =
        let (b, c, _) = teRef e
        in vstack_ [childSpacing_ 2]
            [ hstack
                [ box_ [onClick (EvGoRef b c), alignLeft]
                    (label (refText (teRef e))
                        `styleBasic` [textSize (12 * sc), textColor lightSkyBlue])
                    `styleHover` [bgColor (rgbHex "#3A3F45")]
                , filler
                , button "remove" (EvDeleteThreadEntry file i)
                    `styleBasic` [textSize (10 * sc), padding 3]
                ]
            , wrapLabel ("“" <> T.unwords (teText e) <> "”")
                `styleBasic` [textSize (12 * sc), width (panelInnerW - 8)]
            , widgetMaybe (teNote e) $ \nt -> wrapLabel nt
                `styleBasic` [ textSize (11 * sc), textColor muted
                             , width (panelInnerW - 8) ]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]

-- ── weave panels ────────────────────────────────────────────────────────────

kindRowW :: Double -> WeaveKind -> WidgetNode AppModel AppEvent
kindRowW sc k = label (kindLabel k) `styleBasic` [textSize (12 * sc)]

weavesPanel :: AppModel -> WidgetNode AppModel AppEvent
weavesPanel model = panelBox
    [ panelHeader sc "weaves" EvClosePanel
    , label "parallel passages — links between verses, drawn across panes"
        `styleBasic` [textSize (10 * sc), textColor muted]
    , if null lws
        then wrapLabel ("none yet — open two panes, select verses in each, "
                <> "then \"+ link\"")
            `styleBasic` [textSize (12 * sc), textColor muted, width panelInnerW]
        else vscroll (vstack_ [childSpacing_ 6] (map row lws))
    , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
    , label "kind for new links" `styleBasic` [textSize (10 * sc), textColor muted]
    , dropdown amWeaveKind allKinds (kindRowW sc) (kindRowW sc) `styleBasic` [textSize (12 * sc)]
    , label "new empty weave" `styleBasic` [textSize (10 * sc), textColor muted]
    , textField_ amWeaveNew [placeholder "weave name"]
    , box_ [alignLeft] (button "Create" EvNewWeave
        `styleBasic` [textSize (11 * sc), padding 4])
    ]
  where
    sc = uiScaleOf model
    -- order by where each weave first lands in the canon (filters to come)
    lws = sortOn (weaveStartKey . lwWeave) (model ^. amWeaves)
    row lw =
        let w = lwWeave lw
        in box_ [onClick (EvOpenWeave (lwFile lw)), alignLeft]
            (vstack_ [childSpacing_ 2]
                [ label (wName w)
                    `styleBasic` [textSize (13 * sc), textColor lightSkyBlue]
                , label (kindLabel (wKind w) <> " · "
                    <> showt (length (wLinks w)) <> " links"
                    <> (if wApproved w then "" else " · ⚠ unapproved"))
                    `styleBasic` [textSize (10 * sc), textColor
                        (if wApproved w then gray else rgbHex "#C99A4B")]
                , label (T.intercalate "  ↔  " (map spanText (weaveSpans w)))
                    `styleBasic` [textSize (10 * sc), textColor muted]
                ])
            `styleHover` [bgColor (rgbHex "#3A3F45")]

weaveViewPanel :: AppModel -> FilePath -> WidgetNode AppModel AppEvent
weaveViewPanel model file =
    case find ((== file) . lwFile) (model ^. amWeaves) of
        Nothing -> panelBox
            [ panelHeader sc "weave" EvClosePanel
            , label "weave not found" `styleBasic` [textSize (12 * sc), textColor muted]
            ]
        Just lw -> render (lwWeave lw)
  where
    sc = uiScaleOf model
    others = [wName (lwWeave o) | o <- model ^. amWeaves, lwFile o /= file]
    render w = panelBox $
        [ panelHeader sc (wName w) EvClosePanel
        , hstack
            [ button "← all weaves" EvShowWeaves
                `styleBasic` [textSize (10 * sc), padding 3]
            , filler
            , button "delete weave" (EvDeleteWeave file)
                `styleBasic` [textSize (10 * sc), padding 3]
            ]
        , wrapLabel (if wApproved w
                then "✓ reviewed and approved"
                else "⚠ AI-generated for study — not reviewed or approved")
            `styleBasic` [textSize (11 * sc), padding 5, width panelInnerW
                , textColor (if wApproved w
                    then rgbHex "#8FB88A" else rgbHex "#E0B05A")
                , bgColor (rgbHex "#2A2620")]
        , hstack
            [ button (if wApproved w then "Clear approval" else "✓ Approve whole weave")
                (EvApproveWeave (not (wApproved w)))
                `styleBasic` [textSize (11 * sc), padding 4
                    , textColor (rgbHex "#EAE6DE")
                    , bgColor (if wApproved w then rgbHex "#574A38" else rgbHex "#3E5239")]
                `styleHover` [bgColor (if wApproved w then rgbHex "#6A5942" else rgbHex "#4B6344")]
            , filler
            , label (showt (approvedCount w) <> " / " <> showt (length (wLinks w))
                    <> " verse links approved")
                `styleBasic` [textSize (10 * sc), textColor muted]
            ]
        ]
        <> [ wrapLabel "opening a weave points the panes at its passages; its lines draw automatically"
            `styleBasic` [textSize (10 * sc), textColor muted, width panelInnerW]
        , label "comparing" `styleBasic` [textSize (10 * sc), textColor muted]
        , vstack_ [childSpacing_ 1]
            [ label ("· " <> spanText s)
                `styleBasic` [textSize (11 * sc), textColor (rgbHex "#C8C4BD")]
            | s <- weaveSpans w ]
        , label "kind" `styleBasic` [textSize (10 * sc), textColor muted]
        , dropdown_ amWeaveKind allKinds (kindRowW sc) (kindRowW sc) [onChange EvSetWeaveKind]
            `styleBasic` [textSize (12 * sc)]
        , label "weave notes" `styleBasic` [textSize (10 * sc), textColor muted]
        , textArea amWeaveNotes
            `styleBasic` [textSize (12 * sc), height 110]
            `nodeKey` "weaveNotes"
        , box_ [alignLeft] (button "Save notes" EvSaveWeaveNotes
            `styleBasic` [textSize (11 * sc), padding 4])
        , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
        , label (showt (length (wLinks w)) <> " links")
            `styleBasic` [textSize (10 * sc), textColor muted]
        , vscroll (vstack_ [childSpacing_ 4] (map linkRow (wLinks w)))
        ]
        <> combineSeg
    linkRow l@(Link a b lbl _) = hstack
        ( [ box_ [onClick (EvGoRef (fst3 a) (snd3 a)), alignLeft]
            (label (refText a) `styleBasic` [textSize (11 * sc), textColor lightSkyBlue])
          , label " ↔ " `styleBasic` [textSize (11 * sc), textColor muted]
          , box_ [onClick (EvGoRef (fst3 b) (snd3 b)), alignLeft]
            (label (refText b) `styleBasic` [textSize (11 * sc), textColor lightSkyBlue])
          ]
          <> [ label ("· " <> lbl)
                 `styleBasic` [textSize (10 * sc), textColor (rgbHex "#D2B46E")]
             | not (T.null lbl) ]
          <> [ filler
             , button (if lApproved l then "✓" else "approve")
                 (EvApproveLink l (not (lApproved l)))
                 `styleBasic` [textSize (10 * sc), padding 2, textColor (rgbHex "#EAE6DE")
                     , bgColor (if lApproved l then rgbHex "#3E5239" else rgbHex "#403A30")]
                 `styleHover` [bgColor (if lApproved l then rgbHex "#4B6344" else rgbHex "#524A3C")]
             , button "x" (EvRemoveLink l) `styleBasic` [textSize (10 * sc), padding 2]
             ] )
    combineSeg =
        [ separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
        , label "combine another weave in (merge links)"
            `styleBasic` [textSize (10 * sc), textColor muted]
        , textDropdown amCombinePick others `styleBasic` [textSize (12 * sc)]
        , box_ [alignLeft] (button "Combine" (EvCombineWeave (model ^. amCombinePick))
            `styleBasic` [textSize (11 * sc), padding 4])
        ] `orEmpty` not (null others)
    orEmpty xs cond = if cond then xs else []

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- ── events ──────────────────────────────────────────────────────────────────

handleEvent
    :: Env
    -> WidgetEnv AppModel AppEvent
    -> WidgetNode AppModel AppEvent
    -> AppModel
    -> AppEvent
    -> [AppEventResponse AppModel AppEvent]
handleEvent env _wenv _node model evt = case evt of
    EvInit -> [SetFocusOnKey "reader", saveLater]
    EvWordClicked rt -> case tokStrongs (rtTok rt) of
        (r : _) -> [Model (model & amPanel
            .~ PStrongs (tokWord (rtTok rt)) r (rtRef rt))]
        [] -> []
    EvWordAlt rt -> altClickAt (rtRef rt) (rtIx rt)
    EvSpanSelected ref span_ -> openEditor ref span_
    EvVerseClicked i (_, _, v) shift ->
        [Model (setPane i (verseClickPane v shift))]
    EvGoRef b c ->
        [ Model (setPane (0 :: Int) (\p -> p & psBook .~ b & psChapter .~ c
            & psAnchor .~ Nothing & psSel .~ []))
        , SetFocusOnKey "reader"
        ]
    EvClosePanel ->
        [ Model (model & amPanel .~ PNone), SetFocusOnKey "reader" ]
    EvTogglePatches ->
        [Model (model & amPanel %~ \pm ->
            if pm == PPatches then PNone else PPatches)]
    EvSavePatch -> case model ^. amPanel of
        PEdit et ->
            let repl = T.words (T.strip (model ^. amReplace))
                note = let n = T.strip (model ^. amNote)
                       in if T.null n then Nothing else Just n
            in if null repl
                then [Model (model & amStatus .~ "replacement is empty")]
                else if model ^. amEverywhere
                    then [Task (saveRuleTask env et repl note)]
                    else [Task (saveTask env et repl note)]
        _ -> []
    EvDeletePatch path -> [Task (deleteTask env path)]
    EvPatchesLoaded lps msg ->
        [ Model (model & amPatches .~ lps & amPanel %~ closeEditorOnly
            & amStatus .~ msg)
        , SetFocusOnKey "reader"
        ]
    EvDeleteRule path -> [Task (deleteRuleTask env path)]
    EvExcludeRule path ref -> [Task (excludeRuleTask env path ref)]
    EvRulesLoaded lrs msg ->
        [ Model (model & amRules .~ lrs & amPanel %~ closeEditorOnly
            & amStatus .~ msg)
        , SetFocusOnKey "reader"
        ]
    EvToggleThreads -> [Model (model & amPanel %~ toggleThreads)]
    EvShowThreads -> [Model (model & amPanel .~ PThreads)]
    EvOpenThread file ->
        let notes = maybe "" (thNotes . ltThread)
                (find ((== file) . ltFile) (model ^. amThreads))
        in [Model (model & amPanel .~ PThreadView file & amThreadNotes .~ notes)]
    EvAddToThread -> case model ^. amPanel of
        PEdit et ->
            let name = T.strip $ if T.null (T.strip (model ^. amThreadNew))
                    then model ^. amThreadPick
                    else model ^. amThreadNew
                note = let n = T.strip (model ^. amNote)
                       in if T.null n then Nothing else Just n
            in if T.null name
                then [Model (model & amStatus .~ "name or pick a thread")]
                else [Task (addThreadTask env (model ^. amThreads) name et note)]
        _ -> []
    EvSaveThreadNotes file ->
        case find ((== file) . ltFile) (model ^. amThreads) of
            Nothing -> [Model (model & amStatus .~ "thread not found")]
            Just lt -> [Task (saveThreadNotesTask lt (model ^. amThreadNotes))]
    EvDeleteThread file -> [Task (deleteThreadTask file)]
    EvDeleteThreadEntry file entryIx ->
        case find ((== file) . ltFile) (model ^. amThreads) of
            Nothing -> [Model (model & amStatus .~ "thread not found")]
            Just lt -> [Task (deleteThreadEntryTask lt entryIx)]
    EvThreadsLoaded lts msg ->
        [Model (model & amThreads .~ lts & amPanel %~ adjustThreadPanel lts
            & amStatus .~ msg)]
    EvAddPane i -> [Model (model & amPanes %~ insertPaneAfter i), saveLater]
    EvClosePane i -> [Model (model & amPanes %~ closePane i), saveLater]
    EvPaneBook i b -> [Model (setPane i (\p ->
        p & psBook .~ b & psChapter .~ 1 & psAnchor .~ Nothing & psSel .~ []))
        , SetFocusOnKey "reader", saveLater]
    EvPaneChapter i c -> [Model (setPane i (\p ->
        p & psChapter .~ c & psAnchor .~ Nothing & psSel .~ []))
        , SetFocusOnKey "reader", saveLater]
    EvPanePrev i -> [Model (stepPane i (-1)), saveLater]
    EvPaneNext i -> [Model (stepPane i 1), saveLater]
    EvToggleWeaves ->
        [Model (model & amPanel %~ \pm ->
            if pm == PWeaves then PNone else PWeaves)]
    EvShowWeaves -> [Model (model & amPanel .~ PWeaves)]
    EvOpenWeave file -> case find ((== file) . lwFile) (model ^. amWeaves) of
        Nothing -> [Model (model & amStatus .~ "weave not found")]
        Just lw ->
            let w = lwWeave lw
                tracks = weaveTracks w
                newPanes = [PaneState b c Nothing [] | (b, c) <- take 4 tracks]
            in [Model (model
                    & amPanel .~ PWeaveView file
                    & amPanes .~ (if null newPanes then model ^. amPanes else newPanes)
                    & amWeaveKind .~ wKind w
                    & amWeaveNotes .~ wNotes w
                    & amCombinePick .~ "")]
    EvNewWeave ->
        let name = T.strip (model ^. amWeaveNew)
        in if T.null name
            then [Model (model & amStatus .~ "name the weave")]
            else [Task (newWeaveTask name (model ^. amWeaveKind)
                    (cTokVersion (envCorpus env)) [])]
    EvLink ->
        let refsByPane = [[(_psBook p, _psChapter p, v) | v <- _psSel p]
                         | p <- model ^. amPanes]
            links = smartLinks refsByPane
            selected = concat refsByPane
            clear = model & amPanes %~ map (\p ->
                        p & psSel .~ [] & psAnchor .~ Nothing)
        in if null links
            then [Model (model & amStatus .~ "select verses in two panes to link")]
            else case find (\lw -> any (`elem` weaveVerses lw) selected)
                    (model ^. amWeaves) of
                Just lw ->
                    [Model clear, Task (editWeaveTask lw (addLinks links)
                        ("linked into " <> wName (lwWeave lw)))]
                Nothing ->
                    [Model clear, Task (newWeaveTask (autoName selected)
                        (model ^. amWeaveKind) (cTokVersion (envCorpus env)) links)]
    EvSetWeaveKind k -> withInspected $ \lw ->
        [Task (editWeaveTask lw (\w -> w { wKind = k }) "kind set")]
    EvSaveWeaveNotes -> withInspected $ \lw ->
        [Task (editWeaveTask lw (\w -> w { wNotes = model ^. amWeaveNotes })
            "notes saved")]
    EvRemoveLink l -> withInspected $ \lw ->
        [Task (editWeaveTask lw (removeLink l) "link removed")]
    EvApproveLink l val -> withInspected $ \lw ->
        [Task (editWeaveTask lw (setLinkApproval l val)
            (if val then "verse link approved" else "approval cleared"))]
    EvApproveWeave val -> withInspected $ \lw ->
        [Task (editWeaveTask lw (setAllApproval val)
            (if val then "whole weave approved" else "approval cleared"))]
    EvCombineWeave name -> withInspected $ \lw ->
        case find ((== name) . wName . lwWeave) (model ^. amWeaves) of
            Nothing -> [Model (model & amStatus .~ "pick a weave to combine")]
            Just other ->
                [Task (editWeaveTask lw (combine (lwWeave other))
                    ("combined " <> name))]
    EvDeleteWeave file -> [Task (deleteWeaveTask file)]
    EvWeavesLoaded lws msg ->
        [Model (model & amWeaves .~ lws & amPanel %~ adjustWeavePanel lws
            & amStatus .~ msg)]
    EvStatus t -> [Model (model & amStatus .~ t)]
    EvSaveSession -> [Task (EvNoop <$ saveSession (model ^. amPanes))]
    EvVerseInspect ref x y ->
        -- open the compare card only for verses that actually have witnesses
        let touched = any (any (\l -> lA l == ref || lB l == ref)
                            . wLinks . lwWeave) (model ^. amWeaves)
        in [Model (model & amCompare ?~ (ref, x, y)) | touched]
    EvCloseCompare -> [Model (model & amCompare .~ Nothing)]
    EvApproveLinkIn file l val ->
        case find ((== file) . lwFile) (model ^. amWeaves) of
            Just lw -> [Task (editWeaveTask lw (setLinkApproval l val)
                (if val then "witness approved" else "approval cleared"))]
            Nothing -> []
    EvRejectLinkIn file l ->
        case find ((== file) . lwFile) (model ^. amWeaves) of
            Just lw -> [Task (editWeaveTask lw (removeLink l) "witness rejected")]
            Nothing -> []
    EvZoom delta ->
        let new | delta == 0 = sBodySize defaultSettings
                | otherwise  = max 10 (min 40 (model ^. amBodySize + delta))
        in [ Model (model & amBodySize .~ new
                & amStatus .~ ("text size " <> showt (round new :: Int) <> "px"))
           , Task (EvNoop <$ saveSettings (envSettings env) { sBodySize = new }) ]
    EvNoop -> []
  where
    -- queue a session save as a follow-up event, so it runs against the model
    -- *after* the current pane change has been applied
    saveLater = Task (pure EvSaveSession)
    closeEditorOnly pm = case pm of
        PEdit _ -> PNone
        other -> other

    toggleThreads pm = case pm of
        PThreads -> PNone
        PThreadView _ -> PNone
        _ -> PThreads

    adjustThreadPanel lts pm = case pm of
        PEdit _ -> PNone
        PThreadView f | f `notElem` map ltFile lts -> PThreads
        other -> other

    adjustWeavePanel lws pm = case pm of
        PWeaveView f | f `notElem` map lwFile lws -> PWeaves
        other -> other

    withInspected f = case model ^. amPanel of
        PWeaveView file ->
            maybe [Model (model & amStatus .~ "weave not found")] f
                (find ((== file) . lwFile) (model ^. amWeaves))
        _ -> []

    weaveVerses lw = concatMap (\(Link a b _ _) -> [a, b]) (wLinks (lwWeave lw))

    autoName refs = case refs of
        (r : _) -> "parallel: " <> refText r
        [] -> "parallel"

    -- distinct books among a weave's links, canon order, each at its first
    -- linked chapter — used to point the panes at the weave's passages
    weaveTracks w =
        let refs = concatMap (\(Link a b _ _) -> [a, b]) (wLinks w)
            books = sortOn (\b -> fromMaybe maxBound (elemIndex b bookIds))
                (nub (map fst3 refs))
        in case books of
            -- a same-book weave (two creation accounts, Ps 14 / Ps 53) would
            -- otherwise collapse to one pane: split it by chapter instead
            [b] -> [ (b, c) | c <- nub (sort [c | (_, c, _) <- refs]) ]
            _   -> [ (b, minimum [c | (bb, c, _) <- refs, bb == b]) | b <- books ]

    setPane i f = model & amPanes %~ \ps ->
        [ if j == i then f p else p | (j, p) <- zip [0 ..] ps ]

    -- a new pane opens just after pane i, at the same place (then navigate it);
    -- capped at four panes
    insertPaneAfter i ps
        | length ps >= 4 = ps
        | otherwise =
            let seed = case drop i ps of
                    (p : _) -> p { _psSel = [], _psAnchor = Nothing }
                    [] -> PaneState "Gen" 1 Nothing []
                (before, after) = splitAt (i + 1) ps
            in before <> [seed] <> after

    closePane i ps
        | length ps <= 1 = ps
        | otherwise = [p | (j, p) <- zip [0 ..] ps, j /= i]

    stepPane i dir = setPane i $ \p ->
        let nch = chapterCount (envCorpus env) (_psBook p)
        in p & psChapter %~ (\c -> max 1 (min nch (c + dir)))
             & psAnchor .~ Nothing & psSel .~ []

    -- plain click toggles a verse; Shift-click extends a contiguous run
    verseClickPane v shift p = case (shift, _psAnchor p) of
        (True, Just a) -> p & psSel %~ (\s -> nub (s <> [min a v .. max a v]))
        _ -> p & psAnchor ?~ v
                & psSel %~ (\s -> if v `elem` s then delete v s else s <> [v])

    altClickAt ref wordIx
        | any covers patchSpans =
            [Model (model & amStatus .~
                "already patched — delete it from the patches panel first")]
        | otherwise = case ruleHitAt ref wordIx of
            Just (_, s) -> openEditor ref s
            Nothing -> openEditor ref (wordIx, wordIx)
      where
        covers (s, e) = wordIx >= s && wordIx <= e
        patchSpans =
            [ pSpan (lpPatch lp)
            | lp <- acceptedForVerse (model ^. amPatches) ref ]

    ruleHitAt ref wordIx = do
        i <- M.lookup ref (cByRef (envCorpus env))
        let v = cVerses (envCorpus env) V.! i
            claimed = [ pSpan (lpPatch lp)
                      | lp <- acceptedForVerse (model ^. amPatches) ref ]
            hits = ruleHitsForVerse (model ^. amRules) claimed v
        find (\(_, (s, e)) -> wordIx >= s && wordIx <= e) hits

    describeHit (lr, _) =
        ( lrFile lr
        , T.unwords (rMatch (lrRule lr)) <> " → "
            <> T.unwords (rReplacement (lrRule lr))
        , lrStatus lr == POwn
        )

    openEditor ref span_ =
        case spanWords env ref span_ of
            Nothing -> [Model (model & amStatus .~ "bad span")]
            Just ws ->
                let names = [thName (ltThread lt) | lt <- model ^. amThreads]
                    pick
                        | model ^. amThreadPick `elem` names =
                            model ^. amThreadPick
                        | (n : _) <- names = n
                        | otherwise = ""
                in
                [ Model (model
                    & amPanel .~ PEdit (EditTarget ref span_ ws
                        (countSpanMatches env ws)
                        (describeHit <$> ruleHitAt ref (fst span_)))
                    & amReplace .~ T.unwords ws
                    & amNote .~ ""
                    & amEverywhere .~ False
                    & amThreadPick .~ pick
                    & amThreadNew .~ ""
                    & amStatus .~ "")
                , SetFocusOnKey "replace"
                ]

-- | The canonical words under a span, straight from the corpus.
spanWords :: Env -> (Text, Int, Int) -> (Int, Int) -> Maybe [Text]
spanWords env ref (s, e) = do
    i <- M.lookup ref (cByRef (envCorpus env))
    let toks = vTokens (cVerses (envCorpus env) V.! i)
    if s >= 0 && e >= s && e < length toks
        then Just (map tokWord (take (e - s + 1) (drop s toks)))
        else Nothing

-- | Corpus-wide occurrence count of a word sequence (for the scope radio).
countSpanMatches :: Env -> [Text] -> Int
countSpanMatches env ws = sum
    [ length (matchSpans ws (vTokens v))
    | v <- V.toList (cVerses (envCorpus env))
    ]

saveTask :: Env -> EditTarget -> [Text] -> Maybe Text -> IO AppEvent
saveTask env et repl note = do
    r <- try $ do
        p <- mkPatch (envKeys env) (cTokVersion (envCorpus env))
            (etRef et) (etSpan et) (etWords et) repl note
        path <- savePatch p
        lps <- loadPatches (envKeys env) (envCorpus env)
        pure (lps, path)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("save failed: " <> showt e)
        Right (lps, path) -> EvPatchesLoaded lps ("saved " <> T.pack path)

deleteTask :: Env -> FilePath -> IO AppEvent
deleteTask env path = do
    r <- try $ do
        removeFile path
        loadPatches (envKeys env) (envCorpus env)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("delete failed: " <> showt e)
        Right lps -> EvPatchesLoaded lps ("deleted " <> T.pack path)

saveRuleTask :: Env -> EditTarget -> [Text] -> Maybe Text -> IO AppEvent
saveRuleTask env et repl note = do
    r <- try $ do
        rule <- mkRule (envKeys env) (cTokVersion (envCorpus env))
            (etWords et) repl note
        path <- saveRule rule
        lrs <- loadRules (envKeys env) (envCorpus env)
        pure (lrs, path)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("save failed: " <> showt e)
        Right (lrs, path) -> EvRulesLoaded lrs ("saved " <> T.pack path)

deleteRuleTask :: Env -> FilePath -> IO AppEvent
deleteRuleTask env path = do
    r <- try $ do
        removeFile path
        loadRules (envKeys env) (envCorpus env)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("delete failed: " <> showt e)
        Right lrs -> EvRulesLoaded lrs ("deleted " <> T.pack path)

excludeRuleTask :: Env -> FilePath -> (Text, Int, Int) -> IO AppEvent
excludeRuleTask env path ref = do
    r <- try (excludeVerse (envKeys env) path ref)
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("exclude failed: " <> showt e))
        Right (Left err) ->
            pure (EvStatus ("exclude failed: " <> T.pack err))
        Right (Right _) -> do
            lrs <- loadRules (envKeys env) (envCorpus env)
            pure (EvRulesLoaded lrs ("excluded " <> refText ref))

addThreadTask
    :: Env -> [LoadedThread] -> Text -> EditTarget -> Maybe Text -> IO AppEvent
addThreadTask env lts name et note = do
    r <- try $ addToThread lts name (cTokVersion (envCorpus env))
        (etRef et) (etSpan et) (etWords et) note
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("thread save failed: " <> showt e))
        Right (Left err) ->
            pure (EvStatus ("thread save failed: " <> T.pack err))
        Right (Right _) -> reloadThreads ("added to \x201C" <> name <> "\x201D")

reloadThreads :: Text -> IO AppEvent
reloadThreads msg = do
    (lts, errs) <- loadThreads
    pure (EvThreadsLoaded lts
        (msg <> if null errs then "" else " · " <> threadErrText errs))

threadErrText :: [String] -> Text
threadErrText errs = showt (length errs) <> " thread file(s) unreadable"

saveThreadNotesTask :: LoadedThread -> Text -> IO AppEvent
saveThreadNotesTask lt notes = do
    r <- try (writeThread (ltFile lt) (ltThread lt) { thNotes = notes })
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("save failed: " <> showt e))
        Right () -> reloadThreads "notes saved"

deleteThreadTask :: FilePath -> IO AppEvent
deleteThreadTask path = do
    r <- try (removeFile path)
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("delete failed: " <> showt e))
        Right () -> reloadThreads ("deleted " <> T.pack path)

deleteThreadEntryTask :: LoadedThread -> Int -> IO AppEvent
deleteThreadEntryTask lt entryIx = do
    let t = ltThread lt
        keep = [e | (i, e) <- zip [0 ..] (thEntries t), i /= entryIx]
    r <- try (writeThread (ltFile lt) t { thEntries = keep })
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("remove failed: " <> showt e))
        Right () -> reloadThreads "passage removed"

-- ── weave tasks ───────────────────────────────────────────────────────────────

reloadWeaves :: Text -> IO AppEvent
reloadWeaves msg = do
    (lws, errs) <- loadWeaves
    pure (EvWeavesLoaded lws
        (msg <> if null errs then "" else " · " <> weaveErrText errs))

weaveErrText :: [String] -> Text
weaveErrText errs = showt (length errs) <> " weave file(s) unreadable"

-- | Create a weave (optionally seeded with links), then reload.
newWeaveTask :: Text -> WeaveKind -> Text -> [Link] -> IO AppEvent
newWeaveTask name kind tokv links = do
    now <- getCurrentTime
    let stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
        path = weaveFileFor name
        w = addLinks links (emptyWeave name kind tokv stamp)
    exists <- doesFileExist path
    if exists
        then pure (EvStatus ("a weave file already exists: " <> T.pack path))
        else do
            r <- try (writeWeave path w)
            case r of
                Left (e :: SomeException) ->
                    pure (EvStatus ("create failed: " <> showt e))
                Right () -> reloadWeaves ("created \x201C" <> name <> "\x201D")

editWeaveTask :: LoadedWeave -> (Weave -> Weave) -> Text -> IO AppEvent
editWeaveTask lw f msg = do
    r <- try (writeWeave (lwFile lw) (f (lwWeave lw)))
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("weave save failed: " <> showt e))
        Right () -> reloadWeaves msg

deleteWeaveTask :: FilePath -> IO AppEvent
deleteWeaveTask path = do
    r <- try (removeFile path)
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("delete failed: " <> showt e))
        Right () -> reloadWeaves ("deleted " <> T.pack path)

-- ── startup ─────────────────────────────────────────────────────────────────

loadEnv :: IO Env
loadEnv = do
    corpus <- loadCorpus corpusPath >>= either dieLoad pure
    strongs <- loadStrongs strongsPath >>= either dieLoad pure
    keys <- loadOrCreateKeys >>= either (die . ("key setup failed: " <>)) pure
    notes <- loadNotes
    Env corpus strongs (occurrenceIndex corpus) keys notes <$> loadSettings
  where
    dieLoad err = die $
        "could not load data: " <> err
        <> "\nrun the importer first: .\\run.ps1 run overlay-import"

guiMain :: IO ()
guiMain = do
    env <- loadEnv
    patches <- loadPatches (envKeys env) (envCorpus env)
    rules <- loadRules (envKeys env) (envCorpus env)
    (threads, terrs) <- loadThreads
    (weaves, werrs) <- loadWeaves
    (serifR, serifI) <- resolveFonts (envSettings env)
    (sansR, sansB) <- resolveSans
    panes <- restorePanes (envCorpus env) <$> loadSession
    let status = T.intercalate " · " (filter (not . T.null)
            [ if null terrs then "" else threadErrText terrs
            , if null werrs then "" else weaveErrText werrs ])
        model = AppModel PNone False False True patches rules threads "" "" False "" "" ""
            status panes weaves "" Retelling "" "" Nothing (sBodySize (envSettings env))
        config =
            [ appWindowTitle "overlay — KJV 1769"
            , appWindowState MainWindowMaximized
            , appTheme darkTheme
            , appFontDef "Regular" sansR
            , appFontDef "Bold" sansB
            , appFontDef "Serif" serifR
            , appFontDef "Serif Italic" serifI
            , appInitEvent EvInit
            , appDisposeEvent EvSaveSession
            ]
    startApp model (handleEvent env) (buildUI env) config

-- | Headless sanity check of the data pipeline and overlays (--check).
checkMain :: IO ()
checkMain = do
    env <- loadEnv
    let corpus = envCorpus env
        vs = cVerses corpus
        nTokens = V.sum (V.map (length . vTokens) vs)
        render bid c v = case M.lookup (bid, c, v) (cByRef corpus) of
            Nothing -> "MISSING " <> bid <> " " <> showt c <> ":" <> showt v
            Just i ->
                let vr = vs V.! i
                    title = verseTitle vr
                in (if T.null title then "" else "[" <> title <> "] ")
                    <> verseBody vr
        lordOccs = M.findWithDefault [] "H3068" (envOccIx env)
    st <- selfTest (envKeys env) (cTokVersion corpus)
    patches <- loadPatches (envKeys env) corpus
    rules <- loadRules (envKeys env) corpus
    (threads, terrs) <- loadThreads
    (weaves, werrs) <- loadWeaves
    (serifR, serifI) <- resolveFonts (envSettings env)
    let applied status = case status of
            PInvalid _ -> False
            _ -> True
        isApplied = applied . lpStatus
        renderVerse v = T.unwords (map (renderToken . rtTok) (rvTokens
            (toRVerse (kPubHex (envKeys env)) patches rules [] [] 0 v)))
        renderRef ref = case M.lookup ref (cByRef corpus) of
            Nothing -> "?"
            Just i -> renderVerse (vs V.! i)
        linkRender (Link a b lbl _) =
            refKey a <> " ↔ " <> refKey b
                <> (if T.null lbl then "" else " (" <> lbl <> ")")
                <> " — “"
                <> T.take 28 (renderRef a) <> "…” ↔ “"
                <> T.take 28 (renderRef b) <> "…”"
        allLinks = [l | lw <- weaves, l <- wLinks (lwWeave lw)]
        patchedRender lp =
            let p = lpPatch lp
                ref = (pBook p, pChapter p, pVerse p)
            in case M.lookup ref (cByRef corpus) of
                Nothing -> "  (missing verse)"
                Just i -> "  " <> refText ref <> " → " <> renderVerse (vs V.! i)
        ruleRender lr =
            case V.find (not . null . ruleHitsForVerse [lr] []) vs of
                Nothing -> "  (no matches)"
                Just v -> "  " <> refText (vBook v, vChapter v, vVerse v)
                    <> " → " <> renderVerse v
    putStrLn $ T.unpack $ T.unlines $
        [ "verses:   " <> showt (V.length vs)
        , "books:    " <> showt (M.size (cChapters corpus))
        , "tokens:   " <> showt nTokens
        , "tokver:   " <> cTokVersion corpus
        , "strongs:  " <> showt (M.size (envStrongs env))
        , "notes:    " <> showt (sum (map length (M.elems (envNotes env))))
            <> " margin notes on " <> showt (M.size (envNotes env)) <> " verses"
        , "fonts:    " <> serifR <> " / " <> serifI
        , "type:     " <> showt (sBodySize (envSettings env)) <> "px, spacing "
            <> showt (sLineSpacing (envSettings env))
        , "H3068 in: " <> showt (length lordOccs) <> " verses, first: "
            <> T.intercalate "; " (map refLabel (take 3 lordOccs))
        , "key:      " <> fingerprint (kPubHex (envKeys env)) <> "… ("
            <> showt (T.length (kPubHex (envKeys env))) <> " hex chars)"
        , "ed25519:  " <> either T.pack (const "sign/verify roundtrip OK") st
        , "patches:  " <> showt (length patches) <> " loaded"
        ]
        <> [ "  " <> T.pack (lpFile lp) <> " — " <> showt (lpStatus lp)
           | lp <- patches ]
        <> [ patchedRender lp | lp <- patches, isApplied lp ]
        <> [ "rules:    " <> showt (length rules) <> " loaded" ]
        <> [ "  " <> T.pack (lrFile lr) <> " — " <> showt (lrStatus lr)
             <> " · " <> showt (lrMatches lr) <> " places"
           | lr <- rules ]
        <> [ ruleRender lr | lr <- rules, applied (lrStatus lr) ]
        <> [ "threads:  " <> showt (length threads) <> " ("
             <> showt (sum [length (thEntries (ltThread lt)) | lt <- threads])
             <> " passages)"
             <> (if null terrs then "" else " · " <> threadErrText terrs) ]
        <> [ "weaves:   " <> showt (length weaves) <> " ("
             <> showt (length allLinks) <> " links, "
             <> showt (sum [length (components (wLinks (lwWeave lw)))
                           | lw <- weaves]) <> " groups)"
             <> (if null werrs then "" else " · " <> weaveErrText werrs) ]
        <> [ "  " <> wName (lwWeave lw) <> " — " <> kindLabel (wKind (lwWeave lw))
             <> " · " <> showt (length (wLinks (lwWeave lw))) <> " links"
           | lw <- weaves ]
        <> [ "  " <> linkRender l | l <- take 1 allLinks ]
        <> [ ""
           , "Gen 1:1   " <> render "Gen" 1 1
           , "Ps 3:1    " <> render "Ps" 3 1
           , "John 11:35  " <> render "John" 11 35
           ]

-- | Dev helper: create a signed patch from the command line.
-- Usage: overlay --mkpatch <book> <chapter> <verse> <original-word> <replacement...>
mkPatchCli :: [String] -> IO ()
mkPatchCli args = case args of
    (b : c : v : orig : replWords@(_ : _)) -> do
        env <- loadEnv
        let corpus = envCorpus env
            bid = T.pack b
            ref = (bid, read c, read v)
            origT = T.pack orig
        i <- maybe (die "no such verse") pure (M.lookup ref (cByRef corpus))
        let toks = vTokens (cVerses corpus V.! i)
        wordIx <- maybe (die ("word not found; verse reads: "
                <> T.unpack (T.unwords (map tokWord toks)))) pure
            (findIndex ((== origT) . tokWord) toks)
        p <- mkPatch (envKeys env) (cTokVersion corpus) ref (wordIx, wordIx)
            [origT] (map T.pack replWords) Nothing
        path <- savePatch p
        putStrLn ("wrote " <> path)
    _ -> die "usage: overlay --mkpatch <book> <ch> <v> <original> <replacement...>"

-- | Dev helper: create a signed corpus-wide rule from the command line.
-- Usage: overlay --mkrule <match-words...> => <replacement-words...>
mkRuleCli :: [String] -> IO ()
mkRuleCli args = case break (== "=>") args of
    (match@(_ : _), _ : repl@(_ : _)) -> do
        env <- loadEnv
        let corpus = envCorpus env
        rule <- mkRule (envKeys env) (cTokVersion corpus)
            (map T.pack match) (map T.pack repl) Nothing
        let n = countRuleMatches corpus rule
        if n == 0
            then die "no matches in the corpus; rule not written"
            else do
                path <- saveRule rule
                putStrLn ("wrote " <> path <> " (" <> show n <> " matches)")
    _ -> die "usage: overlay --mkrule <match-words...> => <replacement-words...>"

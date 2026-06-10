{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Overlay
    ( guiMain
    , checkMain
    , mkPatchCli
      -- exported for the test suite
    , toRVerse
    ) where

import Control.Exception (SomeException, try)
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (elemIndex, findIndex, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Monomer
import System.Directory (doesFileExist, getXdgDirectory, removeFile,
                         XdgDirectory (XdgConfig), createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath ((</>))

import Overlay.Canon (Book (..), bookById, bookIds)
import Overlay.Corpus
import Overlay.Patch
import Overlay.ReaderView
import Overlay.Strongs

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

-- | Resolve the serif faces: explicit config path, else bundled EB Garamond,
-- else DejaVu.
resolveFonts :: Settings -> IO (Text, Text)
resolveFonts s = do
    regular <- pick (sSerif s)
        ["assets/fonts/EBGaramond.ttf", dejavu <> "DejaVuSerif.ttf"]
    italic <- pick (sSerifItalic s)
        ["assets/fonts/EBGaramond-Italic.ttf", dejavu <> "DejaVuSerif-Italic.ttf"]
    pure (regular, italic)
  where
    dejavu = "/usr/share/fonts/truetype/dejavu/"
    pick explicit fallbacks = do
        let candidates = maybe [] (pure . T.unpack) explicit
                <> map T.unpack fallbacks
        found <- filterMissing candidates
        pure (T.pack (fromMaybe (last candidates) found))
    filterMissing (c : cs) = do
        ok <- doesFileExist c
        if ok then pure (Just c) else filterMissing cs
    filterMissing [] = pure Nothing

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
    { etRef   :: (Text, Int, Int)
    , etSpan  :: (Int, Int)
    , etWords :: [Text]
    } deriving (Eq, Show)

data PanelMode
    = PNone
    | PStrongs Text Text       -- ^ clicked word, Strong's ref
    | PEdit EditTarget
    | PPatches
    deriving (Eq, Show)

data AppModel = AppModel
    { _amBook    :: Text
    , _amChapter :: Int
    , _amPanel   :: PanelMode
    , _amNotesOn :: Bool
    , _amPatches :: [LoadedPatch]
    , _amReplace :: Text
    , _amNote    :: Text
    , _amStatus  :: Text
    } deriving (Eq, Show)

data AppEvent
    = EvInit
    | EvBookChanged Text
    | EvPrevChapter
    | EvNextChapter
    | EvWordClicked RTok
    | EvWordAlt RTok
    | EvSpanSelected (Text, Int, Int) (Int, Int)
    | EvGoRef Text Int
    | EvClosePanel
    | EvTogglePatches
    | EvSavePatch
    | EvDeletePatch FilePath
    | EvPatchesLoaded [LoadedPatch] Text
    | EvStatus Text
    deriving (Eq, Show)

makeLenses ''AppModel

displayName :: Text -> Text
displayName bid = maybe bid bookName (M.lookup bid bookById)

showt :: Show a => a -> Text
showt = T.pack . show

refText :: (Text, Int, Int) -> Text
refText (b, c, v) = displayName b <> " " <> showt c <> ":" <> showt v

-- ── patch overlay composition ───────────────────────────────────────────────

-- | Compose the patch overlay over a verse, producing renderable tokens.
toRVerse :: Text -> [LoadedPatch] -> [Text] -> Verse -> RVerse
toRVerse ownHex patches notes v =
    RVerse (vVerse v) (walk 0 (vTokens v)) notes
  where
    ref = (vBook v, vChapter v, vVerse v)
    byStart = M.fromList
        [ (fst (pSpan (lpPatch lp)), lp)
        | lp <- acceptedForVerse patches ref
        ]

    walk _ [] = []
    walk i toks@(t : rest) = case M.lookup i byStart of
        Nothing -> RTok t ref i Nothing : walk (i + 1) rest
        Just lp ->
            let p = lpPatch lp
                (s, e) = pSpan p
                spanToks = take (e - s + 1) toks
                info = patchInfo ownHex lp
                firstTok = head spanToks
                lastTok = last spanToks
                strongs = nub (concatMap tokStrongs spanToks)
                keepFlags = tokFlags firstTok
                    `div` flagTitle `mod` 2 * flagTitle
                    + tokFlags firstTok `div` flagPara `mod` 2 * flagPara
                n = length (pReplacement p)
                mkTok j w = Token
                    { tokPre = if j == 0 then tokPre firstTok else ""
                    , tokWord = w
                    , tokPost = if j == n - 1 then tokPost lastTok else ""
                    , tokStrongs = strongs
                    , tokFlags = keepFlags
                    }
            in [ RTok (mkTok j w) ref i (Just info)
               | (j, w) <- zip [0 ..] (pReplacement p)
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

buildUI :: Env -> WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI env _wenv model = widgetTree
  where
    corpus = envCorpus env
    bid = model ^. amBook
    ch = model ^. amChapter
    nch = chapterCount corpus bid

    header = hstack
        [ dropdown_ amBook bookIds bookRow bookRow [onChange EvBookChanged]
            `styleBasic` [width 240]
        , spacer
        , dropdown amChapter [1 .. nch] chapterRow chapterRow
            `styleBasic` [width 90]
            `nodeKey` ("chapters_" <> bid)
        , spacer
        , button "<" EvPrevChapter
        , spacer
        , button ">" EvNextChapter
        , spacer
        , labeledCheckbox "1769 notes" amNotesOn
            `styleBasic` [textSize 12]
        , spacer
        , button ("patches (" <> showt (length (model ^. amPatches)) <> ")")
            EvTogglePatches `styleBasic` [textSize 12]
        , spacer
        , label (model ^. amStatus) `styleBasic` [textSize 11, textColor gray]
        , filler
        , label "overlay" `styleBasic` [textColor gray, textSize 12]
        ]
    bookRow b = label (displayName b)
    chapterRow n = label (showt n)

    notesFor v = if model ^. amNotesOn
        then M.findWithDefault [] (vBook v, vChapter v, vVerse v) (envNotes env)
        else []
    rverses =
        [ toRVerse (kPubHex (envKeys env)) (model ^. amPatches) (notesFor v) v
        | v <- chapterVerses corpus bid ch
        ]

    reader = readerView ReaderCfg
        { rcKey = bid <> ":" <> showt ch
        , rcVerses = rverses
        , rcBodySize = sBodySize (envSettings env)
        , rcLineSpacing = sLineSpacing (envSettings env)
        , rcOnWordClick = EvWordClicked
        , rcOnWordAlt = EvWordAlt
        , rcOnSpanSelect = EvSpanSelected
        , rcOnPrev = EvPrevChapter
        , rcOnNext = EvNextChapter
        } `nodeKey` "reader"

    sidePanel = case model ^. amPanel of
        PNone -> []
        PEdit et -> [editorPanel model et]
        PStrongs word ref -> [strongsPanel env (word, ref)]
        PPatches -> [patchesPanel model]

    widgetTree = vstack
        [ header `styleBasic` [padding 10]
        , hstack (reader : sidePanel)
        ]

-- small caption + multiline value; the explicit width is what lets the
-- multiline label compute its height correctly (see Label.getSizeReq)
captionField :: Text -> Maybe Text -> WidgetNode AppModel AppEvent
captionField name mval = widgetMaybe mval $ \v -> vstack_ [childSpacing_ 2]
    [ label name `styleBasic` [textSize 10, textColor gray]
    , label_ v [multiline]
        `styleBasic` [textSize 13, width panelInnerW]
    ]

panelBox :: [WidgetNode AppModel AppEvent] -> WidgetNode AppModel AppEvent
panelBox children = vstack_ [childSpacing_ 8] children
    `styleBasic` [width panelW, padding 12, bgColor (rgbHex "#26282B")]

panelHeader :: Text -> AppEvent -> WidgetNode AppModel AppEvent
panelHeader title closeEvt = hstack
    [ label title `styleBasic` [textSize 15]
    , filler
    , button "✕" closeEvt `styleBasic` [textSize 11, padding 4]
    ]

strongsPanel :: Env -> (Text, Text) -> WidgetNode AppModel AppEvent
strongsPanel env (word, ref) = panel
  where
    entry = M.lookup ref (envStrongs env)
    occs = M.findWithDefault [] ref (envOccIx env)
    occShown = take 200 occs
    occMore = length occs - length occShown

    occRow r@(b, c, _) = box_ [onClick (EvGoRef b c), alignLeft]
        (label (refLabel r) `styleBasic` [textSize 12, textColor lightSkyBlue])
        `styleHover` [bgColor (rgbHex "#3A3F45")]

    panel = panelBox
        [ panelHeader (ref <> " — " <> word) EvClosePanel
        , widgetMaybe (entry >>= seLemma) $ \l ->
            label l `styleBasic` [textSize 22]
        , hstack_ [childSpacing_ 8]
            [ widgetMaybe (entry >>= seXlit) $ \x ->
                label x `styleBasic` [textSize 13, textColor lightGray]
            , widgetMaybe (entry >>= sePron) $ \p ->
                label p `styleBasic` [textSize 12, textColor gray]
            ]
        , captionField "derivation" (entry >>= seDeriv)
        , captionField "definition" (entry >>= seDef)
        , captionField "KJV renderings" (entry >>= seKjv)
        , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
        , label (showt (length occs) <> " occurrences")
            `styleBasic` [textSize 11, textColor gray]
        , vscroll (vstack
            ([occRow r | r <- occShown]
             <> [label ("… and " <> showt occMore <> " more")
                    `styleBasic` [textSize 11, textColor gray]
                | occMore > 0]))
        ]

editorPanel :: AppModel -> EditTarget -> WidgetNode AppModel AppEvent
editorPanel _model et = panelBox
    [ panelHeader "new patch" EvClosePanel
    , label (refText (etRef et) <> ", words "
        <> showt (fst (etSpan et)) <> "–" <> showt (snd (etSpan et)))
        `styleBasic` [textSize 11, textColor gray]
    , label_ ("replacing: " <> T.unwords (etWords et)) [multiline]
        `styleBasic` [textSize 13, width panelInnerW]
    , label "replacement" `styleBasic` [textSize 10, textColor gray]
    , textField amReplace `nodeKey` "replace"
    , label "note (optional)" `styleBasic` [textSize 10, textColor gray]
    , textField amNote
    , spacer
    , hstack
        [ button "Sign & save" EvSavePatch
        , spacer
        , button "Cancel" EvClosePanel
        ]
    , label_ ("signed with your key, applied instantly; manage patches "
        <> "from the patches panel") [multiline]
        `styleBasic` [textSize 10, textColor gray, width panelInnerW]
    ]

patchesPanel :: AppModel -> WidgetNode AppModel AppEvent
patchesPanel model = panelBox
    [ panelHeader "patches" EvClosePanel
    , if null lps
        then label "none yet — right-click a word, or drag across several"
            `styleBasic` [textSize 12, textColor gray]
        else vscroll (vstack_ [childSpacing_ 6] (map row lps))
    ]
  where
    lps = model ^. amPatches
    row lp =
        let p = lpPatch lp
            ref = (pBook p, pChapter p, pVerse p)
        in vstack_ [childSpacing_ 2]
            [ hstack
                [ box_ [onClick (EvGoRef (pBook p) (pChapter p)), alignLeft]
                    (label (refText ref)
                        `styleBasic` [textSize 12, textColor lightSkyBlue])
                , filler
                , button "delete" (EvDeletePatch (lpFile lp))
                    `styleBasic` [textSize 10, padding 3]
                ]
            , label_ (T.unwords (pOriginal p) <> " → "
                <> T.unwords (pReplacement p)) [multiline]
                `styleBasic` [textSize 12, width (panelInnerW - 8)]
            , label (statusText (lpStatus lp) <> " · "
                <> T.take 10 (pCreated p))
                `styleBasic` [textSize 10, textColor gray]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]

-- ── events ──────────────────────────────────────────────────────────────────

handleEvent
    :: Env
    -> WidgetEnv AppModel AppEvent
    -> WidgetNode AppModel AppEvent
    -> AppModel
    -> AppEvent
    -> [AppEventResponse AppModel AppEvent]
handleEvent env _wenv _node model evt = case evt of
    EvInit -> [SetFocusOnKey "reader"]
    EvBookChanged b ->
        [ Model (model & amBook .~ b & amChapter .~ 1)
        , SetFocusOnKey "reader"
        ]
    EvPrevChapter -> navTo (stepChapter (-1))
    EvNextChapter -> navTo (stepChapter 1)
    EvWordClicked rt -> case tokStrongs (rtTok rt) of
        (r : _) -> [Model (model & amPanel .~ PStrongs (tokWord (rtTok rt)) r)]
        [] -> []
    EvWordAlt rt -> openEditor (rtRef rt) (rtIx rt, rtIx rt)
    EvSpanSelected ref span_ -> openEditor ref span_
    EvGoRef b c ->
        [ Model (model & amBook .~ b & amChapter .~ c)
        , SetFocusOnKey "reader"
        ]
    EvClosePanel ->
        [ Model (model & amPanel .~ PNone)
        , SetFocusOnKey "reader"
        ]
    EvTogglePatches ->
        [ Model (model & amPanel %~ \pm ->
            if pm == PPatches then PNone else PPatches)
        ]
    EvSavePatch -> case model ^. amPanel of
        PEdit et ->
            let repl = T.words (T.strip (model ^. amReplace))
                note = let n = T.strip (model ^. amNote)
                       in if T.null n then Nothing else Just n
            in if null repl
                then [Model (model & amStatus .~ "replacement is empty")]
                else [Task (saveTask env et repl note)]
        _ -> []
    EvDeletePatch path -> [Task (deleteTask env path)]
    EvPatchesLoaded lps msg ->
        [ Model (model & amPatches .~ lps
            & amPanel %~ closeEditorOnly
            & amStatus .~ msg)
        , SetFocusOnKey "reader"
        ]
    EvStatus t -> [Model (model & amStatus .~ t)]
  where
    closeEditorOnly pm = case pm of
        PEdit _ -> PNone
        other -> other

    openEditor ref span_ =
        case spanWords env ref span_ of
            Nothing -> [Model (model & amStatus .~ "bad span")]
            Just ws ->
                [ Model (model
                    & amPanel .~ PEdit (EditTarget ref span_ ws)
                    & amReplace .~ T.unwords ws
                    & amNote .~ ""
                    & amStatus .~ "")
                , SetFocusOnKey "replace"
                ]

    navTo (b, c) =
        [ Model (model & amBook .~ b & amChapter .~ c)
        , SetFocusOnKey "reader"
        ]

    -- steps across book boundaries: Gen 50 -> Exod 1, Exod 1 -> Gen 50
    stepChapter dir
        | ch < 1 && bIx > 0 =
            let pb = bookIds !! (bIx - 1) in (pb, chapterCount corpus pb)
        | ch > nch && bIx < length bookIds - 1 = (bookIds !! (bIx + 1), 1)
        | otherwise = (bid, max 1 (min nch ch))
      where
        corpus = envCorpus env
        bid = model ^. amBook
        ch = model ^. amChapter + dir
        nch = chapterCount corpus bid
        bIx = fromMaybe 0 (elemIndex bid bookIds)

-- | The canonical words under a span, straight from the corpus.
spanWords :: Env -> (Text, Int, Int) -> (Int, Int) -> Maybe [Text]
spanWords env ref (s, e) = do
    i <- M.lookup ref (cByRef (envCorpus env))
    let toks = vTokens (cVerses (envCorpus env) V.! i)
    if s >= 0 && e >= s && e < length toks
        then Just (map tokWord (take (e - s + 1) (drop s toks)))
        else Nothing

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
        Right (lps, path) ->
            EvPatchesLoaded lps ("saved " <> T.pack path)

deleteTask :: Env -> FilePath -> IO AppEvent
deleteTask env path = do
    r <- try $ do
        removeFile path
        loadPatches (envKeys env) (envCorpus env)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("delete failed: " <> showt e)
        Right lps -> EvPatchesLoaded lps ("deleted " <> T.pack path)

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
    (serifR, serifI) <- resolveFonts (envSettings env)
    let model = AppModel "Gen" 1 PNone False patches "" "" ""
        fontDir = "/usr/share/fonts/truetype/dejavu/"
        config =
            [ appWindowTitle "overlay — KJV 1769"
            , appWindowState MainWindowMaximized
            , appTheme darkTheme
            , appFontDef "Regular" (T.pack (fontDir <> "DejaVuSans.ttf"))
            , appFontDef "Bold" (T.pack (fontDir <> "DejaVuSans-Bold.ttf"))
            , appFontDef "Serif" serifR
            , appFontDef "Serif Italic" serifI
            , appInitEvent EvInit
            ]
    startApp model (handleEvent env) (buildUI env) config

-- | Headless sanity check of the data pipeline and patch layer (--check).
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
    (serifR, serifI) <- resolveFonts (envSettings env)
    let isApplied lp = case lpStatus lp of
            PInvalid _ -> False
            _ -> True
        patchedRender lp =
            let p = lpPatch lp
                ref = (pBook p, pChapter p, pVerse p)
            in case M.lookup ref (cByRef corpus) of
                Nothing -> "  (missing verse)"
                Just i ->
                    let rv = toRVerse (kPubHex (envKeys env)) patches []
                            (vs V.! i)
                    in "  " <> refText ref <> " → " <> T.unwords
                        (map (renderToken . rtTok) (rvTokens rv))
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

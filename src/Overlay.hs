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
import Data.List (elemIndex, find, findIndex, nub)
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
import Overlay.Rule
import Overlay.Strongs
import Overlay.Thread

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
    { etRef     :: (Text, Int, Int)
    , etSpan    :: (Int, Int)
    , etWords   :: [Text]
    , etMatches :: Int  -- ^ corpus-wide occurrences of the span's words
    , etRuleHit :: Maybe (FilePath, Text, Bool)
      -- ^ (file, \"match → repl\", is ours) when a rule rewrites this span
    } deriving (Eq, Show)

data PanelMode
    = PNone
    | PStrongs Text Text       -- ^ clicked word, Strong's ref
    | PEdit EditTarget
    | PPatches
    | PThreads
    | PThreadView FilePath
    deriving (Eq, Show)

data AppModel = AppModel
    { _amBook        :: Text
    , _amChapter     :: Int
    , _amPanel       :: PanelMode
    , _amNotesOn     :: Bool
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
    | EvStatus Text
    deriving (Eq, Show)

makeLenses ''AppModel

displayName :: Text -> Text
displayName bid = maybe bid bookName (M.lookup bid bookById)

showt :: Show a => a -> Text
showt = T.pack . show

refText :: (Text, Int, Int) -> Text
refText (b, c, v) = displayName b <> " " <> showt c <> ":" <> showt v

-- ── overlay composition ─────────────────────────────────────────────────────

-- | Compose the patch and rule overlays over a verse, producing renderable
-- tokens. Patches claim their spans first, so rules never override them.
-- @marks@ are word spans to highlight (the open thread's passages).
toRVerse
    :: Text -> [LoadedPatch] -> [LoadedRule] -> [(Int, Int)] -> [Text]
    -> Verse -> RVerse
toRVerse ownHex patches rules marks notes v =
    RVerse (vVerse v) (walk 0 (vTokens v)) notes
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
        , button ("patches (" <> showt (length (model ^. amPatches)
            + length (model ^. amRules)) <> ")")
            EvTogglePatches `styleBasic` [textSize 12]
        , spacer
        , button ("threads (" <> showt (length (model ^. amThreads)) <> ")")
            EvToggleThreads `styleBasic` [textSize 12]
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
    -- highlight the open thread's passages
    markSpans = case model ^. amPanel of
        PThreadView f -> M.fromListWith (<>)
            [ (teRef e, [teSpan e])
            | lt <- model ^. amThreads
            , ltFile lt == f
            , e <- thEntries (ltThread lt)
            ]
        _ -> M.empty

    rverses =
        [ toRVerse (kPubHex (envKeys env)) (model ^. amPatches)
            (model ^. amRules)
            (M.findWithDefault [] (vBook v, vChapter v, vVerse v) markSpans)
            (notesFor v) v
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
        PThreads -> [threadsPanel model]
        PThreadView f -> [threadViewPanel model f]

    widgetTree = vstack
        [ header `styleBasic` [padding 10]
        , hstack (reader : sidePanel)
        ]

-- multiline label for panel body text; the explicit width lets the label
-- compute its wrapped height up front (see Label.getSizeReq), and
-- resizeFactorH 0 makes that height a fixed request — multiline labels
-- default to flex height, so a sibling vscroll (whose flex request is the
-- full content height) would otherwise squeeze them below their text
wrapLabel :: Text -> WidgetNode AppModel AppEvent
wrapLabel t = label_ t [multiline, resizeFactorH 0]

-- small caption + multiline value
captionField :: Text -> Maybe Text -> WidgetNode AppModel AppEvent
captionField name mval = widgetMaybe mval $ \v -> vstack_ [childSpacing_ 2]
    [ label name `styleBasic` [textSize 10, textColor gray]
    , wrapLabel v `styleBasic` [textSize 13, width panelInnerW]
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

    -- everything below the header shares one vscroll: the multiline entry
    -- labels keep their full wrapped height instead of competing with the
    -- occurrence list for the panel's fixed height (long entries like H6213
    -- used to overflow and draw over the captions beneath them)
    panel = panelBox
        [ panelHeader (ref <> " — " <> word) EvClosePanel
        , vscroll $ vstack_ [childSpacing_ 8] $
            [ widgetMaybe (entry >>= seLemma) $ \l ->
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
            ]
            <> [occRow r | r <- occShown]
            <> [label ("… and " <> showt occMore <> " more")
                    `styleBasic` [textSize 11, textColor gray]
               | occMore > 0]
        ]

editorPanel :: AppModel -> EditTarget -> WidgetNode AppModel AppEvent
editorPanel model et = panelBox $
    [ panelHeader (if everywhere then "new rule" else "new patch") EvClosePanel
    , label (refText (etRef et) <> ", words "
        <> showt (fst (etSpan et)) <> "–" <> showt (snd (etSpan et)))
        `styleBasic` [textSize 11, textColor gray]
    , wrapLabel ("replacing: " <> T.unwords (etWords et))
        `styleBasic` [textSize 13, width panelInnerW]
    , widgetMaybe (etRuleHit et) ruleHitBox
    , label "replacement" `styleBasic` [textSize 10, textColor gray]
    , textField amReplace `nodeKey` "replace"
    , label "note (optional)" `styleBasic` [textSize 10, textColor gray]
    , textField amNote
    , label "scope" `styleBasic` [textSize 10, textColor gray]
    , labeledRadio "this verse only" False amEverywhere
        `styleBasic` [textSize 12]
    , labeledRadio ("everywhere — " <> showt (etMatches et) <> " match"
        <> (if etMatches et == 1 then "" else "es") <> " (rule)")
        True amEverywhere
        `styleBasic` [textSize 12]
    , spacer
    , hstack
        [ button (if everywhere then "Sign & save rule" else "Sign & save")
            EvSavePatch
        , spacer
        , button "Cancel" EvClosePanel
        ]
    , wrapLabel ("signed with your key, applied instantly; manage from the "
        <> "patches panel")
        `styleBasic` [textSize 10, textColor gray, width panelInnerW]
    , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
    , label "add to thread (the note above travels with it)"
        `styleBasic` [textSize 10, textColor gray]
    ]
    <> [ textDropdown amThreadPick threadNames `styleBasic` [textSize 12]
       | not (null threadNames) ]
    <> [ textField_ amThreadNew
            [placeholder (if null threadNames
                then "thread name" else "or a new thread name")]
       , box_ [alignLeft] (button "Add span to thread" EvAddToThread
            `styleBasic` [textSize 11, padding 4])
       ]
  where
    everywhere = model ^. amEverywhere
    threadNames = [thName (ltThread lt) | lt <- model ^. amThreads]
    ruleHitBox (file, desc, own) = vstack_ [childSpacing_ 4] $
        [ wrapLabel ("this span is rewritten by rule: " <> desc)
            `styleBasic` [textSize 10, textColor gray, width panelInnerW]
        ]
        <> [ box_ [alignLeft]
                (button "exclude this verse from the rule"
                    (EvExcludeRule file (etRef et))
                    `styleBasic` [textSize 11, padding 4])
           | own ]

patchesPanel :: AppModel -> WidgetNode AppModel AppEvent
patchesPanel model = panelBox
    [ panelHeader "patches & rules" EvClosePanel
    , if null lps && null lrs
        then label "none yet — right-click a word, or drag across several"
            `styleBasic` [textSize 12, textColor gray]
        else vscroll (vstack_ [childSpacing_ 6] rows)
    ]
  where
    lps = model ^. amPatches
    lrs = model ^. amRules
    rows =
        [ sectionLabel "patches" | not (null lps) ]
        <> map row lps
        <> [ sectionLabel "rules" | not (null lrs) ]
        <> map ruleRow lrs
    sectionLabel t = label t `styleBasic` [textSize 10, textColor gray]
    ruleRow lr =
        let r = lrRule lr
            excl = case length (rExclude r) of
                0 -> ""
                k -> " · " <> showt k <> " excluded"
        in vstack_ [childSpacing_ 2]
            [ hstack
                [ wrapLabel (T.unwords (rMatch r) <> " → "
                    <> T.unwords (rReplacement r))
                    `styleBasic` [textSize 12, width (panelInnerW - 64)]
                , filler
                , button "delete" (EvDeleteRule (lrFile lr))
                    `styleBasic` [textSize 10, padding 3]
                ]
            , label (statusText (lrStatus lr) <> " · "
                <> showt (lrMatches lr) <> " places" <> excl <> " · "
                <> T.take 10 (rCreated r))
                `styleBasic` [textSize 10, textColor gray]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]
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
            , wrapLabel (T.unwords (pOriginal p) <> " → "
                <> T.unwords (pReplacement p))
                `styleBasic` [textSize 12, width (panelInnerW - 8)]
            , label (statusText (lpStatus lp) <> " · "
                <> T.take 10 (pCreated p))
                `styleBasic` [textSize 10, textColor gray]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]

threadsPanel :: AppModel -> WidgetNode AppModel AppEvent
threadsPanel model = panelBox
    [ panelHeader "threads" EvClosePanel
    , if null lts
        then wrapLabel ("none yet — right-click a word or drag a span, "
                <> "then \"add span to thread\"")
            `styleBasic` [textSize 12, textColor gray, width panelInnerW]
        else vscroll (vstack_ [childSpacing_ 6] (map row lts))
    ]
  where
    lts = model ^. amThreads
    row lt =
        let t = ltThread lt
        in box_ [onClick (EvOpenThread (ltFile lt)), alignLeft]
            (vstack_ [childSpacing_ 2]
                [ label (thName t)
                    `styleBasic` [textSize 13, textColor lightSkyBlue]
                , label (showt (length (thEntries t)) <> " passages · since "
                    <> T.take 10 (thCreated t))
                    `styleBasic` [textSize 10, textColor gray]
                ])
            `styleHover` [bgColor (rgbHex "#3A3F45")]

threadViewPanel :: AppModel -> FilePath -> WidgetNode AppModel AppEvent
threadViewPanel model file =
    case find ((== file) . ltFile) (model ^. amThreads) of
        Nothing -> panelBox
            [ panelHeader "thread" EvClosePanel
            , label "thread not found" `styleBasic` [textSize 12, textColor gray]
            ]
        Just lt -> render (ltThread lt)
  where
    render t = panelBox
        [ panelHeader (thName t) EvClosePanel
        , hstack
            [ button "← all threads" EvShowThreads
                `styleBasic` [textSize 10, padding 3]
            , filler
            , button "delete thread" (EvDeleteThread file)
                `styleBasic` [textSize 10, padding 3]
            ]
        , label (showt (length (thEntries t)) <> " passages · since "
            <> T.take 10 (thCreated t))
            `styleBasic` [textSize 11, textColor gray]
        , label "thread notes" `styleBasic` [textSize 10, textColor gray]
        , textArea amThreadNotes
            `styleBasic` [textSize 12, height 140]
            `nodeKey` "threadNotes"
        , box_ [alignLeft] (button "Save notes" (EvSaveThreadNotes file)
            `styleBasic` [textSize 11, padding 4])
        , separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
        , label "passages (highlighted in the text)"
            `styleBasic` [textSize 10, textColor gray]
        , vscroll (vstack_ [childSpacing_ 6]
            (zipWith entryRow [0 ..] (thEntries t)))
        ]
    entryRow i e =
        let (b, c, _) = teRef e
        in vstack_ [childSpacing_ 2]
            [ hstack
                [ box_ [onClick (EvGoRef b c), alignLeft]
                    (label (refText (teRef e))
                        `styleBasic` [textSize 12, textColor lightSkyBlue])
                    `styleHover` [bgColor (rgbHex "#3A3F45")]
                , filler
                , button "remove" (EvDeleteThreadEntry file i)
                    `styleBasic` [textSize 10, padding 3]
                ]
            , wrapLabel ("“" <> T.unwords (teText e) <> "”")
                `styleBasic` [textSize 12, width (panelInnerW - 8)]
            , widgetMaybe (teNote e) $ \nt -> wrapLabel nt
                `styleBasic` [ textSize 11, textColor gray
                             , width (panelInnerW - 8) ]
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
    EvWordAlt rt -> altClickAt (rtRef rt) (rtIx rt)
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
                else if model ^. amEverywhere
                    then [Task (saveRuleTask env et repl note)]
                    else [Task (saveTask env et repl note)]
        _ -> []
    EvDeletePatch path -> [Task (deleteTask env path)]
    EvPatchesLoaded lps msg ->
        [ Model (model & amPatches .~ lps
            & amPanel %~ closeEditorOnly
            & amStatus .~ msg)
        , SetFocusOnKey "reader"
        ]
    EvDeleteRule path -> [Task (deleteRuleTask env path)]
    EvExcludeRule path ref -> [Task (excludeRuleTask env path ref)]
    EvRulesLoaded lrs msg ->
        [ Model (model & amRules .~ lrs
            & amPanel %~ closeEditorOnly
            & amStatus .~ msg)
        , SetFocusOnKey "reader"
        ]
    EvToggleThreads -> [Model (model & amPanel %~ toggleThreads)]
    EvShowThreads -> [Model (model & amPanel .~ PThreads)]
    EvOpenThread file ->
        let notes = maybe "" (thNotes . ltThread)
                (find ((== file) . ltFile) (model ^. amThreads))
        in [ Model (model & amPanel .~ PThreadView file
                & amThreadNotes .~ notes) ]
    EvAddToThread -> case model ^. amPanel of
        PEdit et ->
            let name = T.strip $ if T.null (T.strip (model ^. amThreadNew))
                    then model ^. amThreadPick
                    else model ^. amThreadNew
                note = let n = T.strip (model ^. amNote)
                       in if T.null n then Nothing else Just n
            in if T.null name
                then [Model (model & amStatus .~ "name or pick a thread")]
                else [Task (addThreadTask env (model ^. amThreads)
                        name et note)]
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
        [ Model (model & amThreads .~ lts
            & amPanel %~ adjustThreadPanel lts
            & amStatus .~ msg)
        ]
    EvStatus t -> [Model (model & amStatus .~ t)]
  where
    closeEditorOnly pm = case pm of
        PEdit _ -> PNone
        other -> other

    toggleThreads pm = case pm of
        PThreads -> PNone
        PThreadView _ -> PNone
        _ -> PThreads

    -- after a thread reload: close the editor (an add just landed), and if
    -- the open thread's file vanished (delete / rename), fall back to the list
    adjustThreadPanel lts pm = case pm of
        PEdit _ -> PNone
        PThreadView f | f `notElem` map ltFile lts -> PThreads
        other -> other

    -- right-click: patched spans explain themselves, rule hits open the
    -- editor over the whole matched span (with the exclusion affordance),
    -- untouched words open a one-word editor
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
    (serifR, serifI) <- resolveFonts (envSettings env)
    let status = if null terrs then "" else threadErrText terrs
        model = AppModel "Gen" 1 PNone False patches rules threads
            "" "" False "" "" "" status
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
    rules <- loadRules (envKeys env) corpus
    (threads, terrs) <- loadThreads
    (serifR, serifI) <- resolveFonts (envSettings env)
    let applied status = case status of
            PInvalid _ -> False
            _ -> True
        isApplied = applied . lpStatus
        renderVerse v = T.unwords (map (renderToken . rtTok) (rvTokens
            (toRVerse (kPubHex (envKeys env)) patches rules [] [] v)))
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

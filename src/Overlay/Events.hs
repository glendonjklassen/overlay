{-# LANGUAGE OverloadedStrings #-}

module Overlay.Events where

import Control.Lens hiding ((.=))
import Data.List (delete, find, nub)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Monomer

import Overlay.Config
import Overlay.Corpus
import Overlay.Patch
import Overlay.ReaderView
import Overlay.Refs
import Overlay.Rule
import Overlay.Session
import Overlay.Tasks
import Overlay.Thread
import Overlay.Types
import Overlay.Weave

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
    EvToggleOptions ->
        [Model (model & amPanel %~ \pm ->
            if pm == POptions then PNone else POptions)]
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
    EvPaneTrack i (b, c) ->
        [ Model (setPane i (\p -> p & psBook .~ b & psChapter .~ c
                & psAnchor .~ Nothing & psSel .~ []))
        , SetFocusOnKey "reader", saveLater ]
    EvSetMaxCols n ->
        let m = clampMaxCols n
        in [Model (model & amMaxCols .~ m & amPanes %~ take m), saveLater]
    EvToggleWeaves ->
        [Model (model & amPanel %~ \pm ->
            if pm == PWeaves then PNone else PWeaves)]
    EvShowWeaves -> [Model (model & amPanel .~ PWeaves)]
    EvOpenWeave file -> case find ((== file) . lwFile) (model ^. amWeaves) of
        Nothing -> [Model (model & amStatus .~ "weave not found")]
        Just lw ->
            let w = lwWeave lw
                tracks = weaveTracks w
                newPanes = [ PaneState b c Nothing []
                           | (b, c) <- take (clampMaxCols (model ^. amMaxCols)) tracks ]
            in [Model (model
                    & amPanel .~ PWeaveView file
                    & amPanes .~ (if null newPanes then model ^. amPanes else newPanes)
                    & amWeaveViewKind .~ wKind w
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
    EvSaveSession ->
        [Task (EvNoop <$ saveSession (model ^. amMaxCols) (model ^. amPanes))]
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

    setPane i f = model & amPanes %~ \ps ->
        [ if j == i then f p else p | (j, p) <- zip [0 ..] ps ]

    -- a new pane opens just after pane i, at the same place (then navigate it);
    -- capped at the live column limit
    insertPaneAfter i ps
        | length ps >= clampMaxCols (model ^. amMaxCols) = ps
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

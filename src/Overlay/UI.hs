{-# LANGUAGE OverloadedStrings #-}

module Overlay.UI where

import Control.Lens hiding ((.=))
import Data.List (elemIndex, find, nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Monomer
import qualified Monomer.Lens as L

import Overlay.Bridge (crossPartners, extraPartners, unionByBook)
import Overlay.Canon (bookIds)
import Overlay.CanonMap
import Overlay.ConceptMap
import Overlay.Config
import Overlay.Corpus
import Overlay.Panels
import Overlay.Patch
import Overlay.ReaderView
import Overlay.Refs
import Overlay.Render
import Overlay.Session
import Overlay.Thread
import Overlay.Types
import Overlay.Weave

-- ── UI ──────────────────────────────────────────────────────────────────────

buildUI :: Env -> WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI env wenv model = widgetTree
  where
    -- UI scale: chrome text rides the same zoom as the scripture body, so it
    -- grows together (Ctrl +/-/scroll). 1.0 at the default body size.
    sc = model ^. amBodySize / sBodySize defaultSettings
    -- the side panel widens with the zoom so its text fits, but never past half
    -- the window, so it can't be pushed off-screen on a narrow/high-DPI display
    panelPW = let Size winW _ = wenv ^. L.windowSize in panelWidthFor winW sc
    corpus = envCorpus env
    own = kPubHex (envKeys env)
    patches = model ^. amPatches
    rules = model ^. amRules
    threads = model ^. amThreads
    panes = model ^. amPanes
    npanes = length panes

    canLink = length (filter (not . null . _psSel) panes) >= 2

    threadPanelOpen = case model ^. amPanel of
        PThreads -> True; PThreadView _ -> True; _ -> False
    weavePanelOpen = case model ^. amPanel of
        PWeaves -> True; PWeaveView _ -> True; _ -> False

    -- a panel switcher styled as an underlined tab: muted when closed, bright
    -- with an accent underline when its panel is open
    navTab name n active ev = button (name <> " " <> showt n) ev
        `styleBasic` ([ textSize (13 * sc), padding 6, bgColor (rgba 0 0 0 0)
                      , textColor (if active then rgbHex "#EAE6DE" else muted) ]
                      <> [ borderB 2 (rgbHex "#6E93C9") | active ])
        `styleHover` [ bgColor (rgbHex "#2E3238"), textColor (rgbHex "#EAE6DE") ]

    -- a flat icon button (gear, + link) that has no filled-block look
    flatBtn lbl col ev = button lbl ev
        `styleBasic` [ textSize (13 * sc), padding 6, bgColor (rgba 0 0 0 0)
                     , textColor col ]
        `styleHover` [ bgColor (rgbHex "#2E3238") ]

    -- top bar (app toolbar): brand left · panel tabs · status · gear (settings)
    header = hstack $
        [ label "overlay" `styleBasic`
            [ textSize (15 * sc), textColor (rgbHex "#C8C4BD"), paddingH 8, paddingV 6 ]
        , spacer
        , navTab "patches" (length patches + length rules)
            (model ^. amPanel == PPatches) EvTogglePatches
        , navTab "threads" (length threads) threadPanelOpen EvToggleThreads
        , navTab "weaves" (length (model ^. amWeaves)) weavePanelOpen EvToggleWeaves
        ]
        <> [ flatBtn "+ link" (rgbHex "#C9A24B") EvLink | canLink ]
        <>
        [ filler
        , label (model ^. amStatus) `styleBasic` [textSize (11 * sc), textColor muted]
        , spacer
        , flatBtn "⚙" (if model ^. amPanel == POptions
                          then rgbHex "#EAE6DE" else muted) EvToggleOptions
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
    -- gutter heat: an active concept's per-verse density (the leitwort signal —
    -- where a word clusters in the open chapter) takes priority; otherwise the
    -- weave-witness heat when its toggle is on.
    conceptCountIn v =
        length [ () | t <- vTokens v, s <- tokStrongs t, s `elem` stripConcepts ]
    heatFor v
        | not (null stripConcepts) = min 4 (conceptCountIn v)
        | model ^. amHeatmapOn = heatTierFor maxWit
            (M.findWithDefault 0 (vBook v, vChapter v, vVerse v) witCount)
        | otherwise = 0

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

    -- the passages of the open weave, if any — drives the per-column picker
    openWeaveTracks = case model ^. amPanel of
        PWeaveView file -> case find ((== file) . lwFile) (model ^. amWeaves) of
            Just lw -> weaveTracks (lwWeave lw)
            Nothing -> []
        _ -> []
    -- the weave passage picker reads in the weave accent colour with a ✦ marker,
    -- so it is unmistakably "this weave's passages" and not the plain book/chapter
    -- jump controls beside it
    weaveAccent = rgbHex "#C9A24B"
    trackRow t = label ("✦ " <> trackText t)
        `styleBasic` [textSize (12 * sc), textColor weaveAccent]

    -- when a weave is open, a picker swaps this column among its passages (the
    -- weave may carry more passages than there are columns); the plain book +
    -- chapter dropdowns beside it stay, prefixed "jump", for free navigation
    trackPicker i =
        if null openWeaveTracks then [] else
            [ dropdown_ (amPanes . singular (ix i) . psTrack)
                openWeaveTracks trackRow trackRow [onChange (EvPaneTrack i)]
                `styleBasic` [rangeWidth 96 160, textSize (12 * sc), border 1 weaveAccent]
                `nodeKey` ("paneTrack_" <> showt i)
            , spacer
            , label "jump" `styleBasic` [textSize (10 * sc), textColor muted]
            ]

    navStrip i p = hstack
        ( trackPicker i
          <>
          [ dropdown_ (amPanes . singular (ix i) . psBook) bookIds
                bookRow bookRow [onChange (EvPaneBook i)]
                `styleBasic` [rangeWidth 96 150, textSize (12 * sc)]
          , spacer
          , dropdown_ (amPanes . singular (ix i) . psChapter)
                [1 .. chapterCount corpus (_psBook p)] chRow chRow
                [onChange (EvPaneChapter i)]
                `styleBasic` [rangeWidth 48 70, textSize (12 * sc)]
                `nodeKey` ("paneCh_" <> showt i <> "_" <> _psBook p)
          , button "<" (EvPanePrev i) `styleBasic` [textSize (12 * sc), padding 2]
          , button ">" (EvPaneNext i) `styleBasic` [textSize (12 * sc), padding 2]
          , filler
          ]
          <> [ button "+ pane" (EvAddPane i)
                `styleBasic` [textSize (11 * sc), padding 3]
             | npanes < clampMaxCols (model ^. amMaxCols) ]
          <> [ button "✕" (EvClosePane i)
                `styleBasic` [textSize (11 * sc), padding 3, textColor (rgbHex "#B07A7A")]
             | npanes > 1 ]
        ) `styleBasic` [padding 4, bgColor (rgbHex "#202225")]

    navRow = hgrid (zipWith navStrip [0 ..] panes)

    reader = readerView ReaderCfg
        { rcColumns = zipWith paneColumn [0 :: Int ..] panes
        , rcBodySize = model ^. amBodySize
        , rcLineSpacing = model ^. amLineSpacing
        , rcLinks = if model ^. amLinesOn then ambientLinks else []
        , rcHeatOn = model ^. amHeatmapOn || not (null stripConcepts)
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
        POptions -> [optionsPanel model panelPW]
        PEdit et -> [editorPanel model panelPW et]
        PStrongs word ref vref ->
            [strongsPanel env (model ^. amBridgeExtraOn) (model ^. amPinnedConcepts)
                sc panelPW (sortOnCanon (M.findWithDefault [] vref witAdj)) vref (word, ref)]
        PPatches -> [patchesPanel model panelPW]
        PThreads -> [threadsPanel model panelPW]
        PThreadView f -> [threadViewPanel model panelPW f]
        PWeaves -> [weavesPanel model panelPW]
        PWeaveView f -> [weaveViewPanel model panelPW f]

    -- the shared canon overview strip: one map, a pin per pane
    canonMap = canonMapView CanonMapCfg
        { cmcSegs = [ CanonSeg lbl (fromIntegral lo / nb)
                        (fromIntegral (hi + 1) / nb) (lo >= otNT)
                    | (lbl, lo, hi) <- canonSegments ]
        , cmcPins = [ CanonPin (canonPosOf p) (_psBook p) (paneColor i)
                    | (i, p) <- zip [0 :: Int ..] panes ]
        , cmcDivider = fromIntegral otNT / nb
        , cmcOnClick = Just EvCanonGoto
        }
      where nb = fromIntegral (length bookIds)
            otNT = 39  -- Matthew is the 40th book (index 39)

    -- the concept dispersion strip: shown above the canon map while a Strong's
    -- number is active, sharing its book-fraction coordinates so the two line up.
    -- Each concept paints its own footprint plus, in a distinct colour, the
    -- footprint of its cross-testament partners — so an OT-only (or NT-only) word
    -- still shows its bridged lemmas across the seam instead of a bare gap.
    -- the active concept plus any pinned for comparison (dedup)
    stripConcepts = nub (model ^. amConcepts <> model ^. amPinnedConcepts)
    -- each concept shows its own footprint plus, when ≤2 are on the strip, its
    -- cross-testament bridge band (so a single concept spans both OT and NT);
    -- at 3–4 concepts we drop the bridge bands so the four series still fit
    conceptSeriesFor withBridge r =
        let cix = envConcept env
            partners = nub (crossPartners (envBridge env) (envBridgeCands env) 6 r
                            <> [ p | model ^. amBridgeExtraOn
                                   , p <- extraPartners (envBridgeExtra env) r ])
        in ConceptSeries r (unionByBook cix [r])
            : [ ConceptSeries (r <> "  ↔ bridge") (unionByBook cix partners)
              | withBridge, not (null partners) ]
    conceptStrip
        | null stripConcepts = []
        | otherwise =
            [ conceptMapView ConceptMapCfg
                { cmpBooks = bookIds
                , cmpSeries = concatMap (conceptSeriesFor (length stripConcepts <= 2))
                    stripConcepts
                , cmpDivider = 39 / fromIntegral (length bookIds)
                , cmpOnClick = Just EvCanonGoto
                } ]

    mainArea = vstack ([navRow, reader] <> conceptStrip <> [canonMap])

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

    compareRow (other, _lbl, file, l) = vstack_ [childSpacing_ 2]
        [ hstack
            [ box_ [onClick (EvGoRef (fst3 other) (snd3 other)), alignLeft]
                (label (refText other)
                    `styleBasic` [textSize (12 * sc), textColor lightSkyBlue])
            , filler
            , approveToggle sc (lApproved l) (EvApproveLinkIn file l (not (lApproved l)))
            , dangerBtn sc "reject" (EvRejectLinkIn file l)
            ]
        , wrapLabel (verseTextOf other)
            `styleBasic` [textSize (model ^. amBodySize), textColor lightGray, width 336]
        ]

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

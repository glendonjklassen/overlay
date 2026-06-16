{-# LANGUAGE OverloadedStrings #-}

module Overlay.Panels where

import Control.Lens hiding ((.=))
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Monomer

import Overlay.Patch
import Overlay.Refs
import Overlay.Render
import Overlay.Rule
import Overlay.Session (maxColsCap)
import Overlay.Strongs
import Overlay.Thread
import Overlay.Types
import Overlay.Weave

-- | The side panel's base (unzoomed) width.
panelW :: Double
panelW = 330

-- | The side panel's width for the current zoom and window. It tracks the UI
-- zoom so its scaled text never outgrows it, but is capped to half the window
-- so it can't be pushed off a narrow or high-DPI screen (e.g. a tablet where
-- the body text has been zoomed up). The inner content width is this minus the
-- panel's 12px padding on each side.
panelWidthFor :: Double -> Double -> Double
panelWidthFor winW sc = max 248 (min (winW * 0.5) (panelW * sc))

-- | Secondary label text. Monomer's built-in 'gray' (#808080) only reaches
-- ~3.7:1 on the dark panels — below WCAG AA at the small sizes used here — so
-- the side panels use this lighter warm grey (~7:1) for readable section
-- labels and help text.
muted :: Color
muted = rgbHex "#B7B2A8"

-- multiline label for panel body text (see Label.getSizeReq); resizeFactorH 0
-- fixes the wrapped height so a sibling vscroll can't squeeze it
wrapLabel :: Text -> WidgetNode AppModel AppEvent
wrapLabel t = label_ t [multiline, resizeFactorH 0]

captionField :: Double -> Double -> Text -> Maybe Text -> WidgetNode AppModel AppEvent
captionField sc piw name mval = widgetMaybe mval $ \v -> vstack_ [childSpacing_ 2]
    [ label name `styleBasic` [textSize (10 * sc), textColor muted]
    , wrapLabel v `styleBasic` [textSize (13 * sc), width piw]
    ]

panelBox :: Double -> [WidgetNode AppModel AppEvent] -> WidgetNode AppModel AppEvent
panelBox pw items = vstack_ [childSpacing_ 8] items
    `styleBasic` [width pw, padding 12, bgColor (rgbHex "#26282B")]

panelHeader :: Double -> Text -> AppEvent -> WidgetNode AppModel AppEvent
panelHeader sc title closeEvt = hstack
    [ label title `styleBasic` [textSize (15 * sc)]
    , filler
    , button "✕" closeEvt `styleBasic` [textSize (11 * sc), padding 4]
    ]

-- | The central options panel: every global display/reading control in one
-- place, opened from the header gear. Font and line-spacing still live in
-- config.json (not yet live-editable here).
optionsPanel :: AppModel -> Double -> WidgetNode AppModel AppEvent
optionsPanel model pw = panelBox pw
    [ panelHeader sc "Options" EvClosePanel
    , sectionLabel "Display"
    , labeledCheckbox "1769 margin notes" amNotesOn `styleBasic` [textSize (12 * sc)]
    , labeledCheckbox "weave heatmap" amHeatmapOn `styleBasic` [textSize (12 * sc)]
    , labeledCheckbox "weave links" amLinesOn `styleBasic` [textSize (12 * sc)]
    , separator
    , sectionLabel "Reading columns"
    , dropdown_ amMaxCols [1 .. maxColsCap] colRow colRow [onChange EvSetMaxCols]
        `styleBasic` [width (90 * sc), textSize (12 * sc)]
    , separator
    , sectionLabel "Text size"
    , hstack_ [childSpacing_ 6]
        [ button "−" (EvZoom (-1)) `styleBasic` [textSize (13 * sc), padding 4]
        , label (showt (round (model ^. amBodySize) :: Int) <> " px")
            `styleBasic` [textSize (12 * sc)]
        , button "+" (EvZoom 1) `styleBasic` [textSize (13 * sc), padding 4]
        , filler
        , button "reset" (EvZoom 0) `styleBasic` [textSize (11 * sc), padding 4]
        ]
    , wrapLabel "Text size also follows Ctrl + scroll and Ctrl + / − / 0."
        `styleBasic` [textSize (10 * sc), textColor muted, width (pw - 24)]
    ]
  where
    sc = uiScaleOf model
    sectionLabel t = label t `styleBasic` [textSize (11 * sc), textColor muted]
    separator = separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]
    colRow n = label (showt n) `styleBasic` [textSize (12 * sc)]

-- | The Ctrl-click side panel: verse-level cross-references (weave witnesses)
-- on top, then word-level Strong's detail below — both levels in one place.
strongsPanel
    :: Env -> Double -> Double -> [((Text, Int, Int), Text)] -> (Text, Int, Int)
    -> (Text, Text) -> WidgetNode AppModel AppEvent
strongsPanel env sc pw witnesses vref (word, ref) = panel
  where
    piw = pw - 24
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
                  [textSize (10 * sc), textColor (rgbHex "#D2B46E"), width piw]
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
                `styleBasic` [textSize (10 * sc), textColor muted, width piw]
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
        , captionField sc piw "derivation" (entry >>= seDeriv)
        , captionField sc piw "definition" (entry >>= seDef)
        , captionField sc piw "KJV renderings" (entry >>= seKjv)
        , label (showt (length occs) <> " occurrences")
            `styleBasic` [textSize (11 * sc), textColor muted]
        ]
        <> [occRow r | r <- occShown]
        <> [label ("… and " <> showt occMore <> " more")
                `styleBasic` [textSize (11 * sc), textColor muted]
           | occMore > 0]

    panel = panelBox pw
        [ panelHeader sc (refText vref <> " — " <> word) EvClosePanel
        , vscroll $ vstack_ [childSpacing_ 8] (verseSection <> wordSection)
        ]

editorPanel :: AppModel -> Double -> EditTarget -> WidgetNode AppModel AppEvent
editorPanel model pw et = panelBox pw $
    [ panelHeader sc (if everywhere then "new rule" else "new patch") EvClosePanel
    , wrapLabel ("For modernizing archaisms only — e.g. fourscore → eighty. "
        <> "Never to add to, take from, or change the meaning of the text.")
        `styleBasic` [textSize (10 * sc), textColor (rgbHex "#C99A4B"), width piw]
    , label (refText (etRef et) <> ", words "
        <> showt (fst (etSpan et)) <> "–" <> showt (snd (etSpan et)))
        `styleBasic` [textSize (11 * sc), textColor muted]
    , wrapLabel ("replacing: " <> T.unwords (etWords et))
        `styleBasic` [textSize (13 * sc), width piw]
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
        `styleBasic` [textSize (10 * sc), textColor muted, width piw]
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
    piw = pw - 24
    everywhere = model ^. amEverywhere
    threadNames = [thName (ltThread lt) | lt <- model ^. amThreads]
    ruleHitBox (file, desc, own) = vstack_ [childSpacing_ 4] $
        [ wrapLabel ("this span is rewritten by rule: " <> desc)
            `styleBasic` [textSize (10 * sc), textColor muted, width piw]
        ]
        <> [ box_ [alignLeft]
                (button "exclude this verse from the rule"
                    (EvExcludeRule file (etRef et))
                    `styleBasic` [textSize (11 * sc), padding 4])
           | own ]

patchesPanel :: AppModel -> Double -> WidgetNode AppModel AppEvent
patchesPanel model pw = panelBox pw
    [ panelHeader sc "patches & rules" EvClosePanel
    , if null lps && null lrs
        then label "none yet — right-click a word, or drag across several"
            `styleBasic` [textSize (12 * sc), textColor muted]
        else vscroll (vstack_ [childSpacing_ 6] rows)
    ]
  where
    sc = uiScaleOf model
    piw = pw - 24
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
                    `styleBasic` [textSize (12 * sc), width (piw - 64)]
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
                `styleBasic` [textSize (12 * sc), width (piw - 8)]
            , label (statusText (lpStatus lp) <> " · "
                <> T.take 10 (pCreated p))
                `styleBasic` [textSize (10 * sc), textColor muted]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]

threadsPanel :: AppModel -> Double -> WidgetNode AppModel AppEvent
threadsPanel model pw = panelBox pw
    [ panelHeader sc "threads" EvClosePanel
    , if null lts
        then wrapLabel ("none yet — right-click a word or drag a span, "
                <> "then \"add span to thread\"")
            `styleBasic` [textSize (12 * sc), textColor muted, width piw]
        else vscroll (vstack_ [childSpacing_ 6] (map row lts))
    ]
  where
    sc = uiScaleOf model
    piw = pw - 24
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

threadViewPanel :: AppModel -> Double -> FilePath -> WidgetNode AppModel AppEvent
threadViewPanel model pw file =
    case find ((== file) . ltFile) (model ^. amThreads) of
        Nothing -> panelBox pw
            [ panelHeader sc "thread" EvClosePanel
            , label "thread not found" `styleBasic` [textSize (12 * sc), textColor muted]
            ]
        Just lt -> render (ltThread lt)
  where
    sc = uiScaleOf model
    piw = pw - 24
    render t = panelBox pw
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
                `styleBasic` [textSize (12 * sc), width (piw - 8)]
            , widgetMaybe (teNote e) $ \nt -> wrapLabel nt
                `styleBasic` [ textSize (11 * sc), textColor muted
                             , width (piw - 8) ]
            , separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
            ]

-- ── weave panels ────────────────────────────────────────────────────────────

kindRowW :: Double -> WeaveKind -> WidgetNode AppModel AppEvent
kindRowW sc k = label (kindLabel k) `styleBasic` [textSize (12 * sc)]

weavesPanel :: AppModel -> Double -> WidgetNode AppModel AppEvent
weavesPanel model pw = panelBox pw
    [ panelHeader sc "weaves" EvClosePanel
    , label "parallel passages — links between verses, drawn across panes"
        `styleBasic` [textSize (10 * sc), textColor muted]
    , if null lws
        then wrapLabel ("none yet — open two panes, select verses in each, "
                <> "then \"+ link\"")
            `styleBasic` [textSize (12 * sc), textColor muted, width piw]
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
    piw = pw - 24
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

weaveViewPanel :: AppModel -> Double -> FilePath -> WidgetNode AppModel AppEvent
weaveViewPanel model pw file =
    case find ((== file) . lwFile) (model ^. amWeaves) of
        Nothing -> panelBox pw
            [ panelHeader sc "weave" EvClosePanel
            , label "weave not found" `styleBasic` [textSize (12 * sc), textColor muted]
            ]
        Just lw -> render (lwWeave lw)
  where
    sc = uiScaleOf model
    piw = pw - 24
    others = [wName (lwWeave o) | o <- model ^. amWeaves, lwFile o /= file]
    render w = panelBox pw $
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
            `styleBasic` [textSize (11 * sc), padding 5, width piw
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
            `styleBasic` [textSize (10 * sc), textColor muted, width piw]
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

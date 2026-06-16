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

-- ── shared panel design system ──────────────────────────────────────────────
-- One look per role, used by every panel so threads, weaves, patches and the
-- editor read as one family: same section labels, help text, rules and buttons.

-- | A small muted section heading.
sectionLabel :: Double -> Text -> WidgetNode AppModel AppEvent
sectionLabel sc t = label t `styleBasic` [textSize (10 * sc), textColor muted]

-- | Wrapped muted help/explanatory text, sized to the panel's inner width.
panelHint :: Double -> Double -> Text -> WidgetNode AppModel AppEvent
panelHint sc piw t = wrapLabel t
    `styleBasic` [textSize (10 * sc), textColor muted, width piw]

-- | The thin divider between panel sections.
hrule :: WidgetNode AppModel AppEvent
hrule = separatorLine `styleBasic` [fgColor (rgbHex "#3A3A3A")]

-- | Primary action (save / create / combine): the one filled button per group.
primaryBtn :: Double -> Text -> AppEvent -> WidgetNode AppModel AppEvent
primaryBtn sc lbl ev = button lbl ev
    `styleBasic` [textSize (11 * sc), padding 5, textColor (rgbHex "#EAE6DE")
                 , bgColor (rgbHex "#3E4F6B"), radius 3]
    `styleHover` [bgColor (rgbHex "#4A5D7E")]

-- | Neutral secondary action (back, exclude): plain button, consistent size.
ghostBtn :: Double -> Text -> AppEvent -> WidgetNode AppModel AppEvent
ghostBtn sc lbl ev = button lbl ev
    `styleBasic` [textSize (10 * sc), padding 4, textColor muted]
    `styleHover` [textColor (rgbHex "#EAE6DE")]

-- | Destructive action (delete / remove / reject): red text, no heavy fill.
dangerBtn :: Double -> Text -> AppEvent -> WidgetNode AppModel AppEvent
dangerBtn sc lbl ev = button lbl ev
    `styleBasic` [textSize (10 * sc), padding 4, textColor (rgbHex "#D98C8C")]
    `styleHover` [textColor (rgbHex "#EAE6DE"), bgColor (rgbHex "#4A302E")]

-- | The approve/clear toggle, green when approved — shared by the weave panel
-- and the compare card so a witness reads the same in both.
approveToggle :: Double -> Bool -> AppEvent -> WidgetNode AppModel AppEvent
approveToggle sc approved ev = button (if approved then "✓" else "approve") ev
    `styleBasic` [textSize (10 * sc), padding 4, textColor (rgbHex "#EAE6DE")
                 , bgColor (if approved then rgbHex "#3E5239" else rgbHex "#403A30")]
    `styleHover` [bgColor (if approved then rgbHex "#4B6344" else rgbHex "#524A3C")]

-- | The header of a detail panel: "← all X" on the left, "delete X" on the
-- right. Shared by the thread and weave detail views.
detailBar
    :: Double -> Text -> AppEvent -> Text -> AppEvent -> WidgetNode AppModel AppEvent
detailBar sc backLbl backEv delLbl delEv = hstack
    [ ghostBtn sc backLbl backEv, filler, dangerBtn sc delLbl delEv ]

-- | A clickable master-list card: blue title that opens the item, then muted
-- meta lines beneath. Shared by the threads and weaves lists.
listCard
    :: Double -> AppEvent -> Text -> [WidgetNode AppModel AppEvent]
    -> WidgetNode AppModel AppEvent
listCard sc click title metas = box_ [onClick click, alignLeft]
    (vstack_ [childSpacing_ 2]
        ((label title `styleBasic` [textSize (13 * sc), textColor lightSkyBlue]) : metas))
    `styleHover` [bgColor (rgbHex "#3A3F45")]

-- | The central options panel: every global display/reading control in one
-- place, opened from the header gear. (Body font still lives in config.json.)
optionsPanel :: AppModel -> Double -> WidgetNode AppModel AppEvent
optionsPanel model pw = panelBox pw $
    [ panelHeader sc "Options" EvClosePanel
    , sectionLabel sc "Display"
    , labeledCheckbox "1769 margin notes" amNotesOn `styleBasic` [textSize (12 * sc)]
    , labeledCheckbox "weave heatmap" amHeatmapOn `styleBasic` [textSize (12 * sc)]
    , labeledCheckbox "weave links" amLinesOn `styleBasic` [textSize (12 * sc)]
    , hrule
    , sectionLabel sc "Reading columns"
    , dropdown_ amMaxCols [1 .. maxColsCap] colRow colRow [onChange EvSetMaxCols]
        `styleBasic` [width (90 * sc), textSize (12 * sc)]
    , hrule
    , sectionLabel sc "Text size"
    , stepper (EvZoom (-1)) (showt (round (model ^. amBodySize) :: Int) <> " px")
        (EvZoom 1) (EvZoom 0)
    , panelHint sc piw "Text size also follows Ctrl + scroll and Ctrl + / − / 0."
    , hrule
    , sectionLabel sc "Line spacing"
    , stepper (EvLineSpacing (-0.05)) (showt (model ^. amLineSpacing) <> "×")
        (EvLineSpacing 0.05) (EvLineSpacing 0)
    , hrule
    , sectionLabel sc "Keyboard & mouse"
    ]
    <> map (panelHint sc piw) shortcuts
  where
    sc = uiScaleOf model
    piw = pw - 24
    colRow n = label (showt n) `styleBasic` [textSize (12 * sc)]
    -- a − / value / + / reset row, shared by the text-size and line-spacing knobs
    stepper decEv val incEv resetEv = hstack_ [childSpacing_ 6]
        [ button "−" decEv `styleBasic` [textSize (13 * sc), padding 4]
        , label val `styleBasic` [textSize (12 * sc)]
        , button "+" incEv `styleBasic` [textSize (13 * sc), padding 4]
        , filler
        , ghostBtn sc "reset" resetEv
        ]
    shortcuts =
        [ "Ctrl + scroll · text size"
        , "Shift + scroll · scroll all panes together"
        , "← → · previous / next chapter"
        , "Ctrl-click a word · Strong's; Ctrl-click a verse number · cross-references"
        , "right-click a word or drag a span · edit a patch"
        , "click the canon strip below · jump there"
        ]

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
        [ sectionLabel sc "this verse"
        , label (refText vref) `styleBasic` [textSize (14 * sc), textColor lightGray]
        , label (showt (length witnesses) <> " cross-reference"
                <> (if length witnesses == 1 then "" else "s")
                <> " (witnesses)")
            `styleBasic` [textSize (11 * sc), textColor muted]
        ]
        <> map witRow witnesses
        <> [ panelHint sc piw "no linked passages yet — add weave links to build them"
           | null witnesses ]
        <> [hrule]

    wordSection =
        [ sectionLabel sc ("word — " <> ref)
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
    , sectionLabel sc "replacement"
    , textField amReplace `nodeKey` "replace"
    , sectionLabel sc "note (optional)"
    , textField amNote
    , sectionLabel sc "scope"
    , labeledRadio "this verse only" False amEverywhere
        `styleBasic` [textSize (12 * sc)]
    , labeledRadio ("everywhere — " <> showt (etMatches et) <> " match"
        <> (if etMatches et == 1 then "" else "es") <> " (rule)")
        True amEverywhere
        `styleBasic` [textSize (12 * sc)]
    , spacer
    , hstack
        [ primaryBtn sc (if everywhere then "Sign & save rule" else "Sign & save")
            EvSavePatch
        , spacer
        , ghostBtn sc "Cancel" EvClosePanel
        , filler
        ]
    , panelHint sc piw
        "signed with your key, applied instantly; manage from the patches panel"
    , hrule
    , sectionLabel sc "add to thread (the note above travels with it)"
    ]
    <> [ textDropdown amThreadPick threadNames `styleBasic` [textSize (12 * sc)]
       | not (null threadNames) ]
    <> [ textField_ amThreadNew
            [placeholder (if null threadNames
                then "thread name" else "or a new thread name")]
       , box_ [alignLeft] (primaryBtn sc "Add span to thread" EvAddToThread)
       ]
  where
    sc = uiScaleOf model
    piw = pw - 24
    everywhere = model ^. amEverywhere
    threadNames = [thName (ltThread lt) | lt <- model ^. amThreads]
    ruleHitBox (file, desc, own) = vstack_ [childSpacing_ 4] $
        [ panelHint sc piw ("this span is rewritten by rule: " <> desc) ]
        <> [ box_ [alignLeft]
                (ghostBtn sc "exclude this verse from the rule"
                    (EvExcludeRule file (etRef et)))
           | own ]

patchesPanel :: AppModel -> Double -> WidgetNode AppModel AppEvent
patchesPanel model pw = panelBox pw
    [ panelHeader sc "patches & rules" EvClosePanel
    , panelHint sc piw
        "Signed text corrections — a patch fixes one verse, a rule applies everywhere."
    , if null lps && null lrs
        then panelHint sc piw "none yet — right-click a word, or drag across several"
        else vscroll (vstack_ [childSpacing_ 6] rows)
    ]
  where
    sc = uiScaleOf model
    piw = pw - 24
    lps = model ^. amPatches
    lrs = model ^. amRules
    rows =
        [ sectionLabel sc "patches" | not (null lps) ]
        <> map row lps
        <> [ sectionLabel sc "rules" | not (null lrs) ]
        <> map ruleRow lrs
    metaLine t = label t `styleBasic` [textSize (10 * sc), textColor muted]
    itemRule = separatorLine `styleBasic` [fgColor (rgbHex "#33363A")]
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
                , dangerBtn sc "delete" (EvDeleteRule (lrFile lr))
                ]
            , metaLine (statusText (lrStatus lr) <> " · "
                <> showt (lrMatches lr) <> " places" <> excl <> " · "
                <> T.take 10 (rCreated r))
            , itemRule
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
                , dangerBtn sc "delete" (EvDeletePatch (lpFile lp))
                ]
            , wrapLabel (T.unwords (pOriginal p) <> " → "
                <> T.unwords (pReplacement p))
                `styleBasic` [textSize (12 * sc), width (piw - 8)]
            , metaLine (statusText (lpStatus lp) <> " · " <> T.take 10 (pCreated p))
            , itemRule
            ]

threadsPanel :: AppModel -> Double -> WidgetNode AppModel AppEvent
threadsPanel model pw = panelBox pw
    [ panelHeader sc "threads" EvClosePanel
    , panelHint sc piw "Threads collect passages on a shared theme."
    , if null lts
        then panelHint sc piw
            "none yet — right-click a word or drag a span, then \"add to thread\""
        else vscroll (vstack_ [childSpacing_ 6] (map row lts))
    ]
  where
    sc = uiScaleOf model
    piw = pw - 24
    lts = model ^. amThreads
    row lt =
        let t = ltThread lt
        in listCard sc (EvOpenThread (ltFile lt)) (thName t)
            [ label (showt (length (thEntries t)) <> " passages · since "
                <> T.take 10 (thCreated t))
                `styleBasic` [textSize (10 * sc), textColor muted] ]

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
        , detailBar sc "← all threads" EvShowThreads
            "delete thread" (EvDeleteThread file)
        , label (showt (length (thEntries t)) <> " passages · since "
            <> T.take 10 (thCreated t))
            `styleBasic` [textSize (11 * sc), textColor muted]
        , sectionLabel sc "thread notes"
        , textArea amThreadNotes
            `styleBasic` [textSize (12 * sc), height 140]
            `nodeKey` "threadNotes"
        , box_ [alignLeft] (primaryBtn sc "Save notes" (EvSaveThreadNotes file))
        , hrule
        , sectionLabel sc "passages (highlighted in the text)"
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
                , dangerBtn sc "remove" (EvDeleteThreadEntry file i)
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
    , panelHint sc piw "Weaves link parallel passages, drawn across the panes."
    , if null lws
        then panelHint sc piw
            "none yet — open two panes, select verses in each, then \"+ link\""
        else vscroll (vstack_ [childSpacing_ 6] (map row lws))
    , hrule
    , sectionLabel sc "kind for new weaves"
    , dropdown amWeaveKind allKinds (kindRowW sc) (kindRowW sc) `styleBasic` [textSize (12 * sc)]
    , sectionLabel sc "new empty weave"
    , textField_ amWeaveNew [placeholder "weave name"]
    , box_ [alignLeft] (primaryBtn sc "Create" EvNewWeave)
    ]
  where
    sc = uiScaleOf model
    piw = pw - 24
    -- order by where each weave first lands in the canon (filters to come)
    lws = sortOn (weaveStartKey . lwWeave) (model ^. amWeaves)
    row lw =
        let w = lwWeave lw
        in listCard sc (EvOpenWeave (lwFile lw)) (wName w)
            [ label (kindLabel (wKind w) <> " · "
                <> showt (length (wLinks w)) <> " links"
                <> (if wApproved w then "" else " · ⚠ unapproved"))
                `styleBasic` [textSize (10 * sc), textColor
                    (if wApproved w then gray else rgbHex "#C99A4B")]
            , label (T.intercalate "  ↔  " (map spanText (weaveSpans w)))
                `styleBasic` [textSize (10 * sc), textColor muted]
            ]

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
        , detailBar sc "← all weaves" EvShowWeaves "delete weave" (EvDeleteWeave file)
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
        <> [ panelHint sc piw "opening a weave points the panes at its passages and draws its lines; closing it restores your previous panes"
        , sectionLabel sc "comparing"
        , vstack_ [childSpacing_ 1]
            [ label ("· " <> spanText s)
                `styleBasic` [textSize (11 * sc), textColor (rgbHex "#C8C4BD")]
            | s <- weaveSpans w ]
        , sectionLabel sc "this weave's kind"
        , dropdown_ amWeaveViewKind allKinds (kindRowW sc) (kindRowW sc) [onChange EvSetWeaveKind]
            `styleBasic` [textSize (12 * sc)]
        , sectionLabel sc "weave notes"
        , textArea amWeaveNotes
            `styleBasic` [textSize (12 * sc), height 110]
            `nodeKey` "weaveNotes"
        , box_ [alignLeft] (primaryBtn sc "Save notes" EvSaveWeaveNotes)
        , hrule
        , sectionLabel sc (showt (length (wLinks w)) <> " links")
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
             , approveToggle sc (lApproved l) (EvApproveLink l (not (lApproved l)))
             , dangerBtn sc "remove" (EvRemoveLink l)
             ] )
    combineSeg =
        [ hrule
        , sectionLabel sc "combine another weave in (merge links)"
        , textDropdown amCombinePick others `styleBasic` [textSize (12 * sc)]
        , box_ [alignLeft] (primaryBtn sc "Combine" (EvCombineWeave (model ^. amCombinePick)))
        ] `orEmpty` not (null others)
    orEmpty xs cond = if cond then xs else []

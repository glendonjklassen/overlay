{-# LANGUAGE OverloadedStrings #-}

-- | The reading surface: a custom widget that lays out verse tokens word by
-- word across one or more side-by-side columns, so every word stays
-- addressable — hover affordances, Strong's lookup, patch markers + hover
-- cards, and patch span selection all ride on the same layout and hit-testing.
--
-- One column is ordinary reading. Several columns are parallel passages: each
-- scrolls independently, clicking a verse number selects that verse, and
-- weave links are drawn as connector lines between the selected/linked verses
-- across columns. The widget owns all columns, which is what lets it draw
-- those lines across the gaps between them.
--
-- Interactions (per column): Ctrl+left-click a word -> Strong's (Ctrl so a
-- stray click while scrolling can't swap the side panel out); right-click ->
-- patch editor for that word; left-drag across words of one verse -> patch
-- editor for the span; left-click a verse number -> toggle weave selection
-- (Shift-click extends). Left/Right arrows raise per-column chapter nav.
-- Up/Down + wheel scroll the active column; holding Shift locks the columns so
-- they scroll together (for reading parallels line by line).
module Overlay.ReaderView
    ( ReaderCfg (..)
    , ColumnCfg (..)
    , RVerse (..)
    , RTok (..)
    , PatchInfo (..)
    , readerView
    ) where

import Control.Lens ((&), (.~), (^.))
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Default (def)
import Data.List (find, findIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import Overlay.Corpus

-- | Display annotation for a patched word; built by the caller so this
-- module stays independent of the patch machinery.
data PatchInfo = PatchInfo
    { piLines :: ![Text]  -- ^ hover card content
    , piWarn  :: !Bool    -- ^ valid signature but unknown key
    } deriving (Eq, Show)

-- | A renderable token: the canonical token plus its address and an
-- optional patch annotation.
data RTok = RTok
    { rtTok   :: !Token
    , rtRef   :: !(Text, Int, Int)
    , rtIx    :: !Int               -- ^ index into the verse's token list
    , rtPatch :: !(Maybe PatchInfo)
    , rtMark  :: !Bool              -- ^ highlighted (thread / weave selection)
    } deriving (Eq, Show)

data RVerse = RVerse
    { rvNum    :: !Int
    , rvTokens :: ![RTok]
    , rvNotes  :: ![Text]  -- ^ 1769 margin notes, when the toggle is on
    , rvHeat   :: !Int     -- ^ witness heat tier, 0 (none) … 4 (extravaganza)
    } deriving (Eq, Show)

-- | One reading column: an identity (changing it resets that column's scroll)
-- and the verses to show.
data ColumnCfg = ColumnCfg
    { ccKey      :: !Text
    , ccVerses   :: ![RVerse]
    , ccAlign    :: !(Maybe Int)
      -- ^ when this column first appears (key change), scroll so this verse
      -- sits at the top, so a freshly opened weave lines its passages up
    } deriving (Eq, Show)

data ReaderCfg e = ReaderCfg
    { rcColumns      :: [ColumnCfg]
    , rcBodySize     :: Double      -- ^ body text size in px
    , rcLineSpacing  :: Double      -- ^ line height multiplier
    , rcLinks        :: [((Text, Int, Int), (Text, Int, Int), Text)]
      -- ^ weave edges to draw between verses (any columns); the third element
      -- is the edge label (empty for a plain connector)
    , rcHeatOn       :: Bool   -- ^ draw the per-verse witness heat strips
    , rcOnWordClick  :: RTok -> e   -- ^ Ctrl+left-click on a word with Strong's
    , rcOnWordAlt    :: RTok -> e   -- ^ right click: start a one-word patch
    , rcOnSpanSelect :: (Text, Int, Int) -> (Int, Int) -> e
      -- ^ drag selection released: verse ref + inclusive token span
    , rcOnVerseClick :: Int -> (Text, Int, Int) -> Bool -> e
      -- ^ verse number clicked: column index + verse ref + Shift held
    , rcOnPaneNav    :: Int -> Int -> e
      -- ^ Left/Right arrow over a column: (column index, direction -1/+1)
    , rcOnVerseInspect :: (Text, Int, Int) -> Double -> Double -> e
      -- ^ Ctrl+click on a verse number: verse ref + window x,y (compare card)
    , rcOnZoom       :: Double -> e
      -- ^ Ctrl+wheel: zoom the text by this delta (+1 in, -1 out)
    }

-- A word placed on a line. Verse numbers carry their ref in 'pwVerse'; note
-- text and body words leave it Nothing (body words carry 'pwTok' instead).
data PWord = PWord
    { pwX     :: !Double
    , pwWidth :: !Double
    , pwText  :: !Text
    , pwFont  :: !Font
    , pwSize  :: !FontSize
    , pwColor :: !Color
    , pwTok   :: !(Maybe RTok)
    , pwVerse :: !(Maybe (Text, Int, Int))
    }

data RLine = RLine
    { rlY     :: !Double
    , rlH     :: !Double
    , rlBase  :: !Double  -- ^ baseline offset from line top
    , rlWords :: ![PWord]
    }

-- | Per-column scroll + layout state.
--
-- 'clOffset' is the committed (target) scroll position — what hit-testing,
-- layout, and the saved session use. Scrolling animates the *displayed* offset
-- from 'clAnimFrom' toward 'clOffset' over 'scrollAnimMs', starting at
-- 'clAnimAt' (0 = settled, drawing straight at 'clOffset'). See 'displayedOffset'.
data ColState = ColState
    { clKey      :: !Text
    , clOffset   :: !Double
    , clLines    :: ![RLine]
    , clContentH :: !Double
    , clX        :: !Double   -- ^ left edge of this column (relative to content area)
    , clW        :: !Double   -- ^ this column's width
    , clAnimFrom :: !Double   -- ^ displayed offset when the current glide began
    , clAnimAt   :: !Millisecond  -- ^ when it began (0 = settled, no animation)
    , clHeat     :: !(M.Map Int Int)  -- ^ verse number -> witness heat tier 0..4
    }

-- | Scroll glide duration. ~150 ms with an ease-out curve reads as smooth
-- without feeling laggy.
scrollAnimMs :: Double
scrollAnimMs = 150

-- | The on-screen offset of a column at time @now@: the eased interpolation
-- from 'clAnimFrom' to 'clOffset' while a glide is running, else 'clOffset'.
displayedOffset :: Millisecond -> ColState -> Double
displayedOffset now c
    | clAnimAt c == 0 = clOffset c
    | otherwise =
        let elapsed = fromIntegral (now - clAnimAt c) :: Double
            t = max 0 (min 1 (elapsed / scrollAnimMs))
            e = 1 - (1 - t) ** 3   -- ease-out cubic
        in clAnimFrom c + (clOffset c - clAnimFrom c) * e

data ReaderState = ReaderState
    { rsCols    :: ![ColState]
    , rsViewW   :: !Double
    , rsViewH   :: !Double
    , rsActive  :: !Int                  -- ^ column the wheel / keys act on
    , rsHover   :: !(Maybe (Int, Int, Int))            -- ^ (col, line, word)
    , rsAnchor  :: !(Maybe (Int, (Text, Int, Int), Int))
    , rsSel     :: !(Maybe (Int, (Text, Int, Int), Int, Int))  -- ^ (col, ref, s, e)
    , rsDragged :: !Bool
    , rsHeatAnim :: !Bool   -- ^ a perpetual heat-pulse render loop is running
    }

emptyState :: ReaderState
emptyState = ReaderState [] 0 0 0 Nothing Nothing Nothing False False

-- palette (dark theme)
bodyColor, titleColor, numColor, noteColor, underlineColor, selColor :: Color
bodyColor = rgbHex "#D8D4CD"
titleColor = rgbHex "#9C988F"
numColor = rgbHex "#94908A"
noteColor = rgbHex "#9C9488"
underlineColor = rgbHex "#7FB4E6"
selColor = rgbHex "#2F4156"

-- soft warm wash behind selected / thread-or-weave-member words
markColor :: Color
markColor = rgbHex "#3B331D"

patchColor, patchWarnColor, sbTrackColor, sbThumbColor :: Color
patchColor = rgbHex "#D9A95B"
patchWarnColor = rgbHex "#D97C5B"
sbTrackColor = rgbHex "#3A3A3A"
sbThumbColor = rgbHex "#5C5C5C"

-- active-pane left-edge marker: bright when the reader holds keyboard focus,
-- dim when it's the active pane but focus is elsewhere (e.g. a dropdown)
activeBarColor, idleBarColor :: Color
activeBarColor = rgbHex "#7FB4E6"
idleBarColor = rgbHex "#454B52"

-- witness heat ramp for the left-margin strip: cool → warm with rising witness
-- count, none (0) drawn nothing. The top two tiers breathe — their alpha rides
-- a 0..1 pulse — so the most cross-referenced verses quietly stand out.
heatStripColor :: Int -> Double -> Maybe Color
heatStripColor tier pulse = case tier of
    1 -> Just (rgba 108 124 98 0.50)               -- some
    2 -> Just (rgba 196 158 78 0.72)               -- more
    3 -> Just (rgba 226 150 60 (0.45 + 0.45 * pulse))  -- lots (animated)
    4 -> Just (rgba 233 96 60 (0.50 + 0.50 * pulse))   -- extravaganza (animated)
    _ -> Nothing

-- weave connector lines: translucent gold, brighter when a verse is hovered
linkColorBase, linkColorHot :: Color
linkColorBase = rgba 201 162 75 0.45
linkColorHot = rgba 236 206 120 0.95

-- edge label: muted gold text on a near-opaque dark pill
linkLabelColor, linkLabelBg :: Color
linkLabelColor = rgba 210 180 110 0.9
linkLabelBg = rgba 23 25 28 0.88

cardBgColor, cardTextColor :: Color
cardBgColor = rgbHex "#17191C"
cardTextColor = rgbHex "#D8D4CD"

bodyFont, italicFont, titleFont, numFont :: Font
bodyFont = "Serif"
italicFont = "Serif Italic"
titleFont = "Serif Italic"
numFont = "Regular"

cardSize :: FontSize
cardSize = FontSize 12

gutterW, rightPad, maxTextW, verseGap, lineStep, colGap :: Double
gutterW = 46
rightPad = 26
maxTextW = 700
verseGap = 9
lineStep = 64
colGap = 22

readerView :: (WidgetModel s, WidgetEvent e) => ReaderCfg e -> WidgetNode s e
readerView cfg = node
  where
    widget = makeReader cfg emptyState
    node = defaultWidgetNode (WidgetType "readerView") widget
        & L.info . L.focusable .~ True

makeReader :: (WidgetModel s, WidgetEvent e) => ReaderCfg e -> ReaderState -> Widget s e
makeReader cfg state = widget
  where
    widget = createSingle state def
        { singleFocusOnBtnPressed = True
        , singleUseScissor = True
        , singleMerge = merge
        , singleHandleEvent = handleEvent
        , singleGetSizeReq = getSizeReq
        , singleResize = resize
        , singleRender = render
        }

    replace newState node = node & L.widget .~ makeReader cfg newState

    -- the heat pulse needs a steady frame source; start a perpetual render loop
    -- when the heatmap turns on, stop it when it turns off (this same loop also
    -- carries scroll glides, so they ask for no extra frames while it runs)
    merge wenv node _oldNode oldState =
        let st0 = relayout wenv (rsViewW oldState) (rsViewH oldState) oldState
            want = rcHeatOn cfg
            wid = node ^. L.info . L.widgetId
            st = st0 { rsHeatAnim = want }
            reqs = RenderOnce :
                if want && not (rsHeatAnim oldState) then [RenderEvery wid 16 Nothing]
                else [RenderStop wid | not want && rsHeatAnim oldState]
        in resultReqs (replace st node) reqs

    getSizeReq _wenv _node = (expandSize 100 1, expandSize 100 1)

    resize wenv node vp = resultNode (replace st node)
      where
        Rect _ _ w h = vp
        st | w /= rsViewW state || h /= rsViewH state = relayout wenv w h state
           | otherwise = relayout wenv w h state  -- columns may have changed

    -- (re)lay out every column at the current width, preserving each column's
    -- scroll offset when its key is unchanged at the same index
    relayout wenv w h st =
        let fm = wenv ^. L.fontManager
            cfgs = rcColumns cfg
            n = max 1 (length cfgs)
            rawW = (w - fromIntegral (n - 1) * colGap) / fromIntegral n
            -- a single pane fills the width (ordinary reading); several panes
            -- are capped to a readable measure and centred as a group, so the
            -- text blocks sit close and the connector lines stay short
            maxCW = maxTextW + gutterW + rightPad + 8
            cw = if n == 1 then rawW else min rawW maxCW
            groupW = fromIntegral n * cw + fromIntegral (n - 1) * colGap
            leftPad = if n == 1 then 0 else max 0 ((w - groupW) / 2)
            old = rsCols st
            mk j (ColumnCfg key vs av) =
                let (lns, ch) = layoutVerses fm cw
                        (rcBodySize cfg) (rcLineSpacing cfg) vs
                    sameKey = case drop j old of
                        (c : _) -> clKey c == key
                        _ -> False
                    -- top of a given verse's first line, for align-on-open
                    firstY vn = listToMaybe
                        [ rlY ln | ln <- lns
                        , Just (_, _, v) <- [lineVerse ln], v == vn ]
                    off | sameKey = case drop j old of
                            (c : _) -> clOffset c
                            _ -> 0
                        | otherwise = maybe 0 (fromMaybe 0 . firstY) av
                    off' = clampOffset ch h off
                    heat = M.fromList [ (rvNum rv, rvHeat rv) | rv <- vs ]
                in ColState key off' lns ch
                    (leftPad + fromIntegral j * (cw + colGap)) cw off' 0 heat
            cols = zipWith mk [0 ..] cfgs
        in st { rsCols = cols, rsViewW = w, rsViewH = h
              , rsActive = min (rsActive st) (max 0 (n - 1)) }

    handleEvent wenv node _target evt = case evt of
        Move p -> onMove p
        Leave _ ->
            let st = state { rsHover = Nothing, rsDragged = False }
            in if isJust (rsHover state) || rsDragged state
                then Just (resultReqs (replace st node) [RenderOnce])
                else Nothing
        ButtonAction p BtnLeft BtnPressed _ -> onPress p
        ButtonAction _ BtnLeft BtnReleased _ -> onRelease
        Click _ BtnLeft _ | rsDragged state ->
            Just (resultReqs
                (replace state { rsDragged = False } node) [RenderOnce])
        Click p BtnLeft _ -> onLeftClick p
        Click p BtnRight _ -> onAltClick p
        WheelScroll p (Point _ wy) dir ->
            let mul = if dir == WheelNormal then 1 else -1
                d = negate wy * 60 * mul
                z = wy * mul
            in if ctrlHeld
                -- Ctrl+wheel zooms the text (browser-style) instead of scrolling.
                -- Ignore zero/near-zero deltas (horizontal scroll, trackpad
                -- jitter) so merely holding Ctrl never drifts the zoom.
                then if abs z < 0.01 then Just (resultNode node)
                    else Just (resultEvts node [rcOnZoom cfg (if z > 0 then 1 else -1)])
                else if shiftHeld then scrollAllBy d
                    else scrollBy (colAtX state carea p) d
        KeyAction _ code KeyPressed
            | ctrlHeld && code == keyEquals -> Just (resultEvts node [rcOnZoom cfg 1])
            | ctrlHeld && code == keyMinus -> Just (resultEvts node [rcOnZoom cfg (-1)])
            | ctrlHeld && code == key0 -> Just (resultEvts node [rcOnZoom cfg 0])
            | isKeyUp code -> vScroll (negate lineStep)
            | isKeyDown code -> vScroll lineStep
            | isKeyPageUp code -> vScroll (negate pageStep)
            | isKeyPageDown code -> vScroll pageStep
            | isKeySpace code -> vScroll pageStep
            | isKeyHome code -> scrollTo (rsActive state) 0
            | isKeyEnd code -> scrollTo (rsActive state) (1 / 0)
            | isKeyLeft code -> Just (resultEvts node [rcOnPaneNav cfg (rsActive state) (-1)])
            | isKeyRight code -> Just (resultEvts node [rcOnPaneNav cfg (rsActive state) 1])
        _ -> Nothing
      where
        carea = getContentArea node (currentStyle wenv node)
        pageStep = rsViewH state * 0.85

        now = wenv ^. L.timestamp
        wid = node ^. L.info . L.widgetId
        -- keep re-rendering for the length of the glide, then stop on its own;
        -- when the heat pulse loop is already running it carries the frames
        animReqs | rcHeatOn cfg = []
                 | otherwise    = [RenderEvery wid 15 (Just 14)]
        -- begin a glide on one column: aim at o', easing out from wherever it is
        -- shown right now (so a mid-glide nudge re-eases from the visible spot)
        glide c o' = c { clOffset = o'
                       , clAnimFrom = displayedOffset now c, clAnimAt = now }
        updCol ci f st = st { rsCols =
            [ if i == ci then f c else c | (i, c) <- zip [0 ..] (rsCols st) ] }

        shiftHeld =
            let km = wenv ^. L.inputStatus . L.keyMod
            in km ^. L.leftShift || km ^. L.rightShift

        ctrlHeld =
            let km = wenv ^. L.inputStatus . L.keyMod
            in km ^. L.leftCtrl || km ^. L.rightCtrl

        leftHeld = M.lookup BtnLeft
            (wenv ^. L.inputStatus . L.buttons) == Just BtnPressed

        -- arrows / wheel scroll one pane; holding Shift locks the panes so they
        -- scroll together (parallel reading)
        vScroll d
            | shiftHeld = scrollAllBy d
            | otherwise = scrollBy (Just (rsActive state)) d

        scrollBy Nothing _ = Nothing
        scrollBy (Just ci) d = case drop ci (rsCols state) of
            (c : _) -> scrollTo ci (clOffset c + d)
            [] -> Nothing

        scrollAllBy d =
            let st = state { rsCols =
                    [ glide c (clampOffset (clContentH c) (rsViewH state)
                            (clOffset c + d))
                    | c <- rsCols state ] }
            in Just (resultReqs (replace st node) animReqs)
        scrollTo ci o = case drop ci (rsCols state) of
            (c : _) ->
                let o' = clampOffset (clContentH c) (rsViewH state) o
                in if o' == clOffset c
                    then Just (resultNode node)
                    else Just (resultReqs
                        (replace (updCol ci (`glide` o') state) node) animReqs)
            [] -> Nothing

        hitWord p = do
            (ci, li, wi) <- hitTest state carea p
            wordTokenAt state ci li wi

        onMove p =
            let h = hitTest state carea p
                active = fromMaybe (rsActive state) (colAtX state carea p)
                dragSel = do
                    (ac, ref, aIx) <- rsAnchor state
                    rt <- hitWord p
                    if rtRef rt == ref && isNothing (rtPatch rt)
                        then Just (ac, ref, min aIx (rtIx rt), max aIx (rtIx rt))
                        else rsSel state
                st | leftHeld && isJust (rsAnchor state) =
                        let sel = dragSel
                            moved = case sel of
                                Just (_, _, s, e) -> s /= e
                                _ -> False
                        in state { rsHover = h, rsSel = sel, rsActive = active
                                 , rsDragged = rsDragged state || moved }
                   | otherwise = state { rsHover = h, rsActive = active }
                changed = rsHover st /= rsHover state
                    || rsSel st /= rsSel state
                    || rsDragged st /= rsDragged state
                    || rsActive st /= rsActive state
            in if changed
                then Just (resultReqs (replace st node) [RenderOnce])
                else Nothing

        onPress p =
            let anchor = do
                    rt <- hitWord p
                    ci <- colAtX state carea p
                    if isJust (rtPatch rt) then Nothing
                        else Just (ci, rtRef rt, rtIx rt)
                st = state { rsAnchor = anchor, rsSel = Nothing
                           , rsDragged = False
                           , rsActive = fromMaybe (rsActive state)
                                (colAtX state carea p) }
            in Just (resultReqs (replace st node) [RenderOnce])

        onRelease = case (rsDragged state, rsSel state) of
            (True, Just (_, ref, s, e)) | e > s ->
                let st = state { rsAnchor = Nothing, rsSel = Nothing }
                in Just (resultReqsEvts (replace st node) [RenderOnce]
                    [rcOnSpanSelect cfg ref (s, e)])
            _ ->
                let st = state { rsAnchor = Nothing, rsSel = Nothing
                               , rsDragged = False }
                in Just (resultReqs (replace st node) [RenderOnce])

        -- a click on a verse number selects that verse for weaving; Ctrl+click
        -- on the number instead opens the compare card (deliberate, never on a
        -- stray pass). Strong's likewise needs Ctrl-click on a word.
        onLeftClick p = case verseNumberAt state carea p of
            Just (ci, ref)
                | ctrlHeld -> let Point px py = p
                              in Just (resultEvts node
                                   [rcOnVerseInspect cfg ref px py])
                | otherwise ->
                    Just (resultEvts node [rcOnVerseClick cfg ci ref shiftHeld])
            Nothing
                | not ctrlHeld -> Just (resultNode node)
                | otherwise -> case hitWord p of
                    Just rt | not (null (tokStrongs (rtTok rt))) ->
                        Just (resultEvts node [rcOnWordClick cfg rt])
                    _ -> Just (resultNode node)

        onAltClick p = do
            rt <- hitWord p
            Just (resultEvts node [rcOnWordAlt cfg rt])

    render wenv node renderer = do
        let style = currentStyle wenv node
            carea = getContentArea node style
            Rect cx cy _cw chh = carea
            fm = wenv ^. L.fontManager
            st = state
            now = wenv ^. L.timestamp
            focused = isNodeFocused wenv node
            active = rsActive st
        forM_ (zip [0 ..] (rsCols st)) $
            uncurry (renderColumn renderer now focused active cx cy chh st)
        -- weave connector lines, on top of the text
        let hov = do
                (ci, li, _) <- rsHover st
                col <- listToMaybe (drop ci (rsCols st))
                ln <- listToMaybe (drop li (clLines col))
                lineVerse ln
        drawLinks renderer now fm cx cy chh st hov (rcLinks cfg)
        -- patch hover card, last so it sits above everything
        forM_ (rsHover st) $ \(ci, li, wi) ->
            forM_ (cardFor st ci li wi) $ \(col, pw, ln, pinfo) -> do
                let baseY = cy + rlY ln - displayedOffset now col + rlBase ln
                drawCard renderer fm carea (cx + clX col + pwX pw) baseY pinfo

    renderColumn renderer now focused active cx cy chh st ci col = do
        let offset = displayedOffset now col
            ox = cx + clX col
            visible ln = rlY ln + rlH ln >= offset && rlY ln <= offset + chh
            selected pw = case (rsSel st, pwTok pw) of
                (Just (sc, ref, s, e), Just rt) ->
                    sc == ci && rtRef rt == ref && rtIx rt >= s && rtIx rt <= e
                _ -> False
        -- a thin bar on the active column's left edge marks where keyboard
        -- scroll/arrows will land; it brightens when the reader holds focus,
        -- so it's never a mystery what's receiving the keys
        when (ci == active && length (rsCols st) > 1 || ci == active && focused) $
            drawRect renderer (Rect ox cy 2.5 chh)
                (Just (if focused then activeBarColor else idleBarColor)) Nothing
        -- witness heat: a per-verse strip in the gutter, tier by cross-reference
        -- count, the top two tiers breathing on a slow pulse
        when (rcHeatOn cfg) $ do
            let pulse = (sin (fromIntegral now / 320) + 1) / 2
            forM_ (M.toList (verseBands col)) $ \((_, _, vn), (top, bot)) ->
                forM_ (heatStripColor (M.findWithDefault 0 vn (clHeat col)) pulse) $ \hc ->
                    let y0 = max cy (cy + top - offset)
                        y1 = min (cy + chh) (cy + bot - offset)
                    in when (y1 > y0 + 2) $
                        drawRect renderer (Rect (ox + 4) (y0 + 1) 3.5 (y1 - y0 - 2))
                            (Just hc) Nothing
        forM_ (zip [0 ..] (clLines col)) $ \(li, ln) -> when (visible ln) $ do
            let lineTop = cy + rlY ln - offset
                baseY = lineTop + rlBase ln
            forM_ (zip [0 ..] (rlWords ln)) $ \(wi, pw) -> do
                when (maybe False rtMark (pwTok pw)) $
                    drawRect renderer
                        (Rect (ox + pwX pw - 1) (lineTop + 1)
                            (pwWidth pw + 3) (rlH ln - 2))
                        (Just markColor) Nothing
                when (selected pw) $
                    drawRect renderer
                        (Rect (ox + pwX pw - 1) (lineTop + 1)
                            (pwWidth pw + 3) (rlH ln - 2))
                        (Just selColor) Nothing
                setFillColor renderer (pwColor pw)
                renderText renderer (Point (ox + pwX pw) baseY)
                    (pwFont pw) (pwSize pw) def (pwText pw)
                let hovered = rsHover st == Just (ci, li, wi)
                    patched = rtPatch =<< pwTok pw
                    hasStrongs =
                        maybe False (not . null . tokStrongs . rtTok) (pwTok pw)
                forM_ patched $ \pinfo -> do
                    let dotColor = if piWarn pinfo
                            then patchWarnColor else patchColor
                        y = baseY + 3
                        xs = takeWhile (< pwX pw + pwWidth pw - 2)
                            [pwX pw, pwX pw + 5 ..]
                    forM_ xs $ \dx ->
                        drawRect renderer (Rect (ox + dx) y 2.5 1.2)
                            (Just dotColor) Nothing
                when (hovered && hasStrongs && isNothing patched) $
                    drawRect renderer
                        (Rect (ox + pwX pw) (baseY + 3) (pwWidth pw) 1.2)
                        (Just underlineColor) Nothing
        when (clContentH col > chh) $ do
            let trackX = ox + clW col - 6
                thumbH = max 30 (chh * chh / clContentH col)
                thumbY = cy + (chh - thumbH) * (offset / (clContentH col - chh))
            drawRect renderer (Rect trackX cy 4 chh) (Just sbTrackColor) Nothing
            drawRect renderer (Rect trackX thumbY 4 thumbH)
                (Just sbThumbColor) Nothing

    -- Connectors are routed by connected component, not per raw edge: for each
    -- group of linked verses we find which columns it occupies, then join only
    -- *consecutive* occupied columns. So a 3- or 4-way parallel chains
    -- A→B→C→D and no line is ever drawn straight across a middle column's text.
    -- (Where a middle column holds no member, the hop spans it — nothing to
    -- anchor to.) Within a pair of adjacent columns every member joins every
    -- member, so 2-to-1 correspondences still draw as converging lines.
    drawLinks renderer now fm cx cy chh st hov links = do
        let cols = rsCols st
            locate ref = listToMaybe
                [ (i, c, cy + (top + bot) / 2 - displayedOffset now c)
                | (i, c) <- zip [0 :: Int ..] cols
                , Just (top, bot) <- [M.lookup ref (verseBands c)] ]
            clampY y = max (cy + 2) (min (cy + chh - 2) y)
            -- undirected adjacency + label lookup over the visible edges
            adj = M.fromListWith (<>)
                (concat [ [(a, [b]), (b, [a])] | (a, b, _) <- links ])
            labelOf a b = fromMaybe "" $ listToMaybe
                [ l | (x, y, l) <- links, not (T.null l)
                    , (x == a && y == b) || (x == b && y == a) ]
            reach acc [] = acc
            reach acc (x : xs)
                | x `elem` acc = reach acc xs
                | otherwise = reach (x : acc) (M.findWithDefault [] x adj <> xs)
            comps = foldl step [] (M.keys adj)
              where step seen r | any (r `elem`) seen = seen
                                | otherwise = reach [] [r] : seen
            drawSeg (lc, ly) (rc, ry) lbl hot = do
                let col = if hot then linkColorHot else linkColorBase
                    wdt = if hot then 2.4 else 1.3
                    p1@(Point x1 y1) = Point (cx + clX lc + clW lc - 14) (clampY ly)
                    p2@(Point x2 y2) = Point (cx + clX rc + 14) (clampY ry)
                drawCurve renderer wdt col p1 p2
                drawDot renderer col p1
                drawDot renderer col p2
                when (hot && not (T.null lbl)) $
                    drawLinkLabel renderer fm
                        (Point ((x1 + x2) / 2) ((y1 + y2) / 2 - 10)) hot lbl
        forM_ comps $ \comp -> do
            -- members of this component that are on screen, grouped by column
            let byCol = M.toAscList $ M.fromListWith (<>)
                    [ (i, [(ref, c, y)]) | ref <- comp, Just (i, c, y) <- [locate ref] ]
            -- join each pair of consecutive occupied columns, all-to-all
            forM_ (zip byCol (drop 1 byCol)) $ \((_, ms1), (_, ms2)) ->
                forM_ ms1 $ \(ra, ca, ya) ->
                    forM_ ms2 $ \(rb, cb, yb) ->
                        drawSeg (ca, ya) (cb, yb) (labelOf ra rb)
                            (hov == Just ra || hov == Just rb)

    cardFor st ci li wi = do
        col <- nth ci (rsCols st)
        ln <- nth li (clLines col)
        pw <- nth wi (rlWords ln)
        rt <- pwTok pw
        pinfo <- rtPatch rt
        Just (col, pw, ln, pinfo)
      where
        nth i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

-- | Verse vertical bands (top, bottom in content coords) for one column.
verseBands :: ColState -> M.Map (Text, Int, Int) (Double, Double)
verseBands col = M.fromListWith merge
    [ (ref, (rlY ln, rlY ln + rlH ln))
    | ln <- clLines col, Just ref <- [lineVerse ln] ]
  where merge (t1, b1) (t2, b2) = (min t1 t2, max b1 b2)

lineVerse :: RLine -> Maybe (Text, Int, Int)
lineVerse ln = listToMaybe
    (mapMaybe (\pw -> pwVerse pw <|> (rtRef <$> pwTok pw)) (rlWords ln))

drawCard :: Renderer -> FontManager -> Rect -> Double -> Double -> PatchInfo -> IO ()
drawCard renderer fm (Rect cx cy cw chh) wordX baseY pinfo = do
    let lns = piLines pinfo
        measure t = _sW (computeTextSize fm numFont cardSize def t)
        lineH = 17
        padX = 10
        padY = 8
        cardW = min 380 (maximum (1 : map measure lns) + padX * 2)
        cardH = fromIntegral (length lns) * lineH + padY * 2
        x = max (cx + 4) (min (cx + cw - cardW - 8) wordX)
        below = baseY + 8
        y = if below + cardH > cy + chh - 4
            then max (cy + 4) (baseY - 22 - cardH)
            else below
    drawRect renderer (Rect x y cardW cardH) (Just cardBgColor) Nothing
    forM_ (zip [0 ..] lns) $ \(i, l) -> do
        let isFirst = i == (0 :: Int)
            color
                | isFirst && piWarn pinfo = patchWarnColor
                | isFirst = patchColor
                | otherwise = cardTextColor
        setFillColor renderer color
        renderText renderer
            (Point (x + padX) (y + padY + fromIntegral i * lineH + 12))
            numFont cardSize def l

-- | A smooth connector between two verses: a cubic that leaves the left verse
-- and arrives at the right one horizontally (sampled as short segments), so a
-- fan of links reads as gentle curves rather than crossing straight streaks.
drawCurve :: Renderer -> Double -> Color -> Point -> Point -> IO ()
drawCurve renderer lineW col p1@(Point x1 y1) (Point x2 y2) = do
    let d = max 30 (abs (x2 - x1) * 0.5)
        bx = x1 + d
        cx2 = x2 - d
        segs = 18 :: Int
        cubic t =
            let u = 1 - t
            in Point
                (u*u*u*x1 + 3*u*u*t*bx + 3*u*t*t*cx2 + t*t*t*x2)
                (u*u*u*y1 + 3*u*u*t*y1 + 3*u*t*t*y2 + t*t*t*y2)
        pts = [cubic (fromIntegral i / fromIntegral segs) | i <- [0 .. segs]]
    beginPath renderer
    setStrokeColor renderer col
    setStrokeWidth renderer lineW
    moveTo renderer p1
    mapM_ (renderLineTo renderer) (drop 1 pts)
    stroke renderer

drawDot :: Renderer -> Color -> Point -> IO ()
drawDot renderer col (Point x y) =
    drawRect renderer (Rect (x - 2.5) (y - 2.5) 5 5) (Just col) Nothing

-- | An edge label (the exact shared text), centred on the connector midpoint
-- on a small dark pill so it stays legible over the curve and the text behind.
drawLinkLabel :: Renderer -> FontManager -> Point -> Bool -> Text -> IO ()
drawLinkLabel renderer fm (Point mx my) hot lbl = do
    let sz = FontSize 10.5
        tw = _sW (computeTextSize fm numFont sz def lbl)
        padX = 5
        pillW = tw + padX * 2
        pillH = 15
        x = mx - pillW / 2
        y = my - pillH / 2
    drawRect renderer (Rect x y pillW pillH) (Just linkLabelBg) Nothing
    setFillColor renderer (if hot then linkColorHot else linkLabelColor)
    renderText renderer (Point (x + padX) (y + 11)) numFont sz def lbl

clampOffset :: Double -> Double -> Double -> Double
clampOffset contentH viewH = max 0 . min (max 0 (contentH - viewH))

wordTokenAt :: ReaderState -> Int -> Int -> Int -> Maybe RTok
wordTokenAt st ci li wi = do
    col <- nth ci (rsCols st)
    ln <- nth li (clLines col)
    pw <- nth wi (rlWords ln)
    pwTok pw
  where
    nth i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

-- | The column whose horizontal band contains the point, if any.
colAtX :: ReaderState -> Rect -> Point -> Maybe Int
colAtX st (Rect cx _ _ _) (Point px _) =
    findIndex (\c -> x >= clX c && x <= clX c + clW c) (rsCols st)
  where x = px - cx

-- | If the point is on a verse-number word, its column index and verse ref.
verseNumberAt :: ReaderState -> Rect -> Point -> Maybe (Int, (Text, Int, Int))
verseNumberAt st carea@(Rect cx cy _ chh) p@(Point _ py) = do
    ci <- colAtX st carea p
    col <- nth ci (rsCols st)
    let y = py - cy + clOffset col
        inLine ln = y >= rlY ln && y < rlY ln + rlH ln
    ln <- find inLine (clLines col)
    let x = pointX p - cx - clX col
        inWord pw = x >= pwX pw && x <= pwX pw + pwWidth pw
    pw <- find (\w -> inWord w && isJust (pwVerse w)) (rlWords ln)
    if py >= cy && py <= cy + chh
        then (,) ci <$> pwVerse pw
        else Nothing
  where
    nth i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing
    pointX (Point xx _) = xx

hitTest :: ReaderState -> Rect -> Point -> Maybe (Int, Int, Int)
hitTest st carea@(Rect cx cy _ chh) p@(Point px py)
    | py < cy || py > cy + chh = Nothing
    | otherwise = do
        ci <- colAtX st carea p
        col <- nth ci (rsCols st)
        let y = py - cy + clOffset col
            x = px - cx - clX col
            inLine ln = y >= rlY ln && y < rlY ln + rlH ln
            inWord pw = x >= pwX pw && x <= pwX pw + pwWidth pw
                && isJust (pwTok pw)
        case filter (inLine . snd) (zip [0 ..] (clLines col)) of
            [] -> Nothing
            ((li, ln) : _) ->
                case filter (inWord . snd) (zip [0 ..] (rlWords ln)) of
                    [] -> Nothing
                    ((wi, _) : _) -> Just (ci, li, wi)
  where
    nth i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

-- ── layout ──────────────────────────────────────────────────────────────────

layoutVerses
    :: FontManager -> Double -> Double -> Double -> [RVerse]
    -> ([RLine], Double)
layoutVerses fm availW bodyPx spacingMul verses = (allLines, totalH + 18)
  where
    textW = max 120 (min maxTextW (availW - gutterW - rightPad))

    bodySize = FontSize bodyPx
    titleSize = FontSize (max 11 (bodyPx - 3.5))
    numSize = FontSize (max 9 (bodyPx * 0.62))
    noteSize = FontSize (max 10 (bodyPx * 0.7))

    measure fnt sz t = _sW (computeTextSize fm fnt sz def t)
    spaceOf fnt sz = measure fnt sz "x x" - measure fnt sz "xx"

    metricsOf fnt sz =
        let m = computeTextMetrics fm fnt sz
            lh = _txmLineH m * spacingMul
            base = (lh - _txmLineH m) / 2 + _txmAsc m
        in (lh, base)

    (bodyLH, bodyBase) = metricsOf bodyFont bodySize
    (titleLH, titleBase) = metricsOf titleFont titleSize
    (noteLH, noteBase) = metricsOf numFont noteSize
    bodySpace = spaceOf bodyFont bodySize
    titleSpace = spaceOf titleFont titleSize
    noteSpace = spaceOf numFont noteSize

    (allLines, totalH) = go 12 verses []
    go y [] acc = (reverse acc, y)
    go y (v : vs) acc =
        let isTitle = hasFlag flagTitle . rtTok
            titleToks = filter isTitle (rvTokens v)
            bodyToks = filter (not . isTitle) (rvTokens v)
            vref = rtRef <$> listToMaybe (rvTokens v)
            (y1, acc1) = addLines y acc $ wrapTokens
                titleFont titleSize titleColor titleSpace titleLH titleBase
                (Nothing :: Maybe Int) Nothing titleToks
            (y2, acc2) = addLines y1 acc1 $ wrapTokens
                bodyFont bodySize bodyColor bodySpace bodyLH bodyBase
                (Just (rvNum v)) vref bodyToks
            (y3, acc3) = foldl addNote (y2, acc2) (rvNotes v)
        in go (y3 + verseGap) vs acc3

    addNote (y, acc) note = addLines y acc
        (wrapPlain numFont noteSize noteColor noteSpace noteLH noteBase
            ("\x2020 " <> note))

    addLines y acc = foldl step (y, acc)
      where
        step (yy, aa) mk = let ln = mk yy in (yy + rlH ln, ln : aa)

    wrapPlain fnt sz col spw lh base txt =
        let indent = gutterW + 14
            place (linesAcc, cur, x) w =
                let ww = measure fnt sz w
                    pw = PWord x ww w fnt sz col Nothing Nothing
                in if x + ww > gutterW + textW && not (null cur)
                    then (reverse cur : linesAcc, [pw { pwX = indent }],
                          indent + ww + spw)
                    else (linesAcc, pw : cur, x + ww + spw)
            (doneRev, lastRev, _) = foldl place ([], [], indent) (T.words txt)
            wordLines = reverse
                (filter (not . null) (reverse lastRev : doneRev))
        in [ \yy -> RLine yy lh base ws | ws <- wordLines ]

    wrapTokens fnt sz col spw lh base mnum mref toks =
        let wordOf rt =
                let tok = rtTok rt
                    italicish = hasFlag flagAdded tok || hasFlag flagTitle tok
                    f = if italicish && fnt == bodyFont then italicFont else fnt
                    c = case rtPatch rt of
                        Just pinfo | piWarn pinfo -> patchWarnColor
                                   | otherwise -> patchColor
                        Nothing -> col
                    txt = renderToken tok
                in (txt, f, c, measure f sz txt, rt)

            place (linesAcc, cur, x) rt =
                let (txt, f, c, w, t) = wordOf rt
                    pw = PWord x w txt f sz c (Just t) Nothing
                in if x + w > gutterW + textW && not (null cur)
                    then (reverse cur : linesAcc, [pw { pwX = gutterW }],
                          gutterW + w + spw)
                    else (linesAcc, pw : cur, x + w + spw)

            (doneRev, lastRev, _) = foldl place ([], [], gutterW) toks
            wordLines = reverse
                (filter (not . null) (reverse lastRev : doneRev))

            withNum i ws = case (i, mnum) of
                (0, Just n) ->
                    let t = T.pack (show n)
                        nw = measure numFont numSize t
                        x = max 2 (gutterW - 12 - nw)
                    in PWord x nw t numFont numSize numColor Nothing mref : ws
                _ -> ws
        in [ \yy -> RLine yy lh base (withNum i ws)
           | (i, ws) <- zip [0 :: Int ..] wordLines
           ]

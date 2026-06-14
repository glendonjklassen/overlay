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
-- Interactions (per column): left-click a word -> Strong's; right-click ->
-- patch editor for that word; left-drag across words of one verse -> patch
-- editor for the span; left-click a verse number -> toggle weave selection
-- (Shift-click extends). Left/Right arrows raise per-column chapter nav.
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
    } deriving (Eq, Show)

-- | One reading column: an identity (changing it resets that column's scroll)
-- and the verses to show.
data ColumnCfg = ColumnCfg
    { ccKey    :: !Text
    , ccVerses :: ![RVerse]
    } deriving (Eq, Show)

data ReaderCfg e = ReaderCfg
    { rcColumns      :: [ColumnCfg]
    , rcBodySize     :: Double      -- ^ body text size in px
    , rcLineSpacing  :: Double      -- ^ line height multiplier
    , rcLinks        :: [((Text, Int, Int), (Text, Int, Int))]
      -- ^ weave edges to draw between verses (any columns)
    , rcOnWordClick  :: RTok -> e   -- ^ left click on a word with Strong's
    , rcOnWordAlt    :: RTok -> e   -- ^ right click: start a one-word patch
    , rcOnSpanSelect :: (Text, Int, Int) -> (Int, Int) -> e
      -- ^ drag selection released: verse ref + inclusive token span
    , rcOnVerseClick :: Int -> (Text, Int, Int) -> Bool -> e
      -- ^ verse number clicked: column index + verse ref + Shift held
    , rcOnPaneNav    :: Int -> Int -> e
      -- ^ Left/Right arrow over a column: (column index, direction -1/+1)
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
data ColState = ColState
    { clKey      :: !Text
    , clOffset   :: !Double
    , clLines    :: ![RLine]
    , clContentH :: !Double
    , clX        :: !Double   -- ^ left edge of this column (relative to content area)
    , clW        :: !Double   -- ^ this column's width
    }

data ReaderState = ReaderState
    { rsCols    :: ![ColState]
    , rsViewW   :: !Double
    , rsViewH   :: !Double
    , rsActive  :: !Int                  -- ^ column the wheel / keys act on
    , rsHover   :: !(Maybe (Int, Int, Int))            -- ^ (col, line, word)
    , rsAnchor  :: !(Maybe (Int, (Text, Int, Int), Int))
    , rsSel     :: !(Maybe (Int, (Text, Int, Int), Int, Int))  -- ^ (col, ref, s, e)
    , rsDragged :: !Bool
    }

emptyState :: ReaderState
emptyState = ReaderState [] 0 0 0 Nothing Nothing Nothing False

-- palette (dark theme)
bodyColor, titleColor, numColor, noteColor, underlineColor, selColor :: Color
bodyColor = rgbHex "#D8D4CD"
titleColor = rgbHex "#8E8B85"
numColor = rgbHex "#6E6B66"
noteColor = rgbHex "#8A8273"
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

-- weave connector lines: translucent gold, brighter when a verse is hovered
linkColorBase, linkColorHot :: Color
linkColorBase = rgba 201 162 75 0.45
linkColorHot = rgba 236 206 120 0.95

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

    merge wenv node _oldNode oldState =
        let st = relayout wenv (rsViewW oldState) (rsViewH oldState) oldState
        in resultReqs (replace st node) [RenderOnce]

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
            mk j (ColumnCfg key vs) =
                let (lns, ch) = layoutVerses fm cw
                        (rcBodySize cfg) (rcLineSpacing cfg) vs
                    prevOff = case drop j old of
                        (c : _) | clKey c == key -> clOffset c
                        _ -> 0
                in ColState key (clampOffset ch h prevOff) lns ch
                    (leftPad + fromIntegral j * (cw + colGap)) cw
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
                col = colAtX state carea p
            in scrollBy col (negate wy * 60 * mul)
        KeyAction _ code KeyPressed
            | isKeyUp code -> scrollBy (Just (rsActive state)) (negate lineStep)
            | isKeyDown code -> scrollBy (Just (rsActive state)) lineStep
            | isKeyPageUp code -> scrollBy (Just (rsActive state)) (negate pageStep)
            | isKeyPageDown code -> scrollBy (Just (rsActive state)) pageStep
            | isKeySpace code -> scrollBy (Just (rsActive state)) pageStep
            | isKeyHome code -> scrollTo (rsActive state) 0
            | isKeyEnd code -> scrollTo (rsActive state) (1 / 0)
            | isKeyLeft code -> Just (resultEvts node [rcOnPaneNav cfg (rsActive state) (-1)])
            | isKeyRight code -> Just (resultEvts node [rcOnPaneNav cfg (rsActive state) 1])
        _ -> Nothing
      where
        carea = getContentArea node (currentStyle wenv node)
        pageStep = rsViewH state * 0.85

        shiftHeld =
            let km = wenv ^. L.inputStatus . L.keyMod
            in km ^. L.leftShift || km ^. L.rightShift

        leftHeld = M.lookup BtnLeft
            (wenv ^. L.inputStatus . L.buttons) == Just BtnPressed

        scrollBy Nothing _ = Nothing
        scrollBy (Just ci) d = case drop ci (rsCols state) of
            (c : _) -> scrollTo ci (clOffset c + d)
            [] -> Nothing
        scrollTo ci o = case drop ci (rsCols state) of
            (c : _) ->
                let o' = clampOffset (clContentH c) (rsViewH state) o
                in if o' == clOffset c
                    then Just (resultNode node)
                    else Just (resultReqs (replace
                        (setOffset ci o' state) node) [RenderOnce])
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
                           , rsDragged = False }
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

        -- a click on a verse number selects that verse for weaving; a click on
        -- a word with Strong's opens the lookup
        onLeftClick p = case verseNumberAt state carea p of
            Just (ci, ref) ->
                Just (resultEvts node [rcOnVerseClick cfg ci ref shiftHeld])
            Nothing -> do
                rt <- hitWord p
                if null (tokStrongs (rtTok rt))
                    then Just (resultNode node)
                    else Just (resultEvts node [rcOnWordClick cfg rt])

        onAltClick p = do
            rt <- hitWord p
            Just (resultEvts node [rcOnWordAlt cfg rt])

    render wenv node renderer = do
        let style = currentStyle wenv node
            carea = getContentArea node style
            Rect cx cy _cw chh = carea
            fm = wenv ^. L.fontManager
            st = state
        forM_ (zip [0 ..] (rsCols st)) $
            uncurry (renderColumn renderer cx cy chh st)
        -- weave connector lines, on top of the text
        let hov = do
                (ci, li, _) <- rsHover st
                col <- listToMaybe (drop ci (rsCols st))
                ln <- listToMaybe (drop li (clLines col))
                lineVerse ln
        drawLinks renderer cx cy chh st hov (rcLinks cfg)
        -- patch hover card, last so it sits above everything
        forM_ (rsHover st) $ \(ci, li, wi) ->
            forM_ (cardFor st ci li wi) $ \(col, pw, ln, pinfo) -> do
                let baseY = cy + rlY ln - clOffset col + rlBase ln
                drawCard renderer fm carea (cx + clX col + pwX pw) baseY pinfo

    renderColumn renderer cx cy chh st ci col = do
        let offset = clOffset col
            ox = cx + clX col
            visible ln = rlY ln + rlH ln >= offset && rlY ln <= offset + chh
            selected pw = case (rsSel st, pwTok pw) of
                (Just (sc, ref, s, e), Just rt) ->
                    sc == ci && rtRef rt == ref && rtIx rt >= s && rtIx rt <= e
                _ -> False
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

    -- draw a connector for every link whose endpoints fall in two different
    -- columns, from the right edge of the left verse to the left edge of the
    -- right verse (vertical centre of each verse's lines, clamped to view)
    drawLinks renderer cx cy chh st hov links = do
        let findIn ref = listToMaybe
                [ (col, cy + (top + bot) / 2 - clOffset col)
                | col <- rsCols st
                , Just (top, bot) <- [M.lookup ref (verseBands col)] ]
            clampY y = max (cy + 2) (min (cy + chh - 2) y)
        forM_ links $ \(a, b) -> case (findIn a, findIn b) of
            (Just (cola, ya), Just (colb, yb)) | clX cola /= clX colb -> do
                let (lc, ly, rc, ry)
                        | clX cola <= clX colb = (cola, ya, colb, yb)
                        | otherwise = (colb, yb, cola, ya)
                    hot = hov == Just a || hov == Just b
                    col = if hot then linkColorHot else linkColorBase
                    wdt = if hot then 2.4 else 1.3
                    p1 = Point (cx + clX lc + clW lc - 14) (clampY ly)
                    p2 = Point (cx + clX rc + 14) (clampY ry)
                drawCurve renderer wdt col p1 p2
                drawDot renderer col p1
                drawDot renderer col p2
            _ -> pure ()

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

setOffset :: Int -> Double -> ReaderState -> ReaderState
setOffset ci o st = st
    { rsCols = [ if i == ci then c { clOffset = o } else c
              | (i, c) <- zip [0 ..] (rsCols st) ] }

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

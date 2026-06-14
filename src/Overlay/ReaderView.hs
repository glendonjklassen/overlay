{-# LANGUAGE OverloadedStrings #-}

-- | The reading surface: a custom widget that lays out verse tokens word by
-- word, so every word remains addressable — hover affordances, Strong's
-- lookup, patch markers, patch hover cards and span selection all ride on
-- the same layout and hit-testing.
--
-- It owns its scroll offset: mouse wheel, Up/Down, PageUp/PageDown, Space,
-- Home/End all work directly and predictably (the built-in scroll container
-- behaves erratically under WSLg). Left/Right raise chapter-nav events.
--
-- Interactions: left click a word -> Strong's; right click -> patch editor
-- for that word; left-drag across words of one verse -> patch editor for
-- the span.
module Overlay.ReaderView
    ( ReaderCfg (..)
    , RVerse (..)
    , RTok (..)
    , PatchInfo (..)
    , readerView
    ) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (forM_, when)
import Data.Default (def)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
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
    , rtMark  :: !Bool              -- ^ highlighted (thread membership)
    } deriving (Eq, Show)

data RVerse = RVerse
    { rvNum    :: !Int
    , rvTokens :: ![RTok]
    , rvNotes  :: ![Text]  -- ^ 1769 margin notes, when the toggle is on
    } deriving (Eq, Show)

data ReaderCfg e = ReaderCfg
    { rcKey          :: Text        -- ^ chapter identity; change resets scroll
    , rcVerses       :: [RVerse]
    , rcBodySize     :: Double      -- ^ body text size in px
    , rcLineSpacing  :: Double      -- ^ line height multiplier
    , rcOnWordClick  :: RTok -> e   -- ^ left click on a word with Strong's
    , rcOnWordAlt    :: RTok -> e   -- ^ right click: start a one-word patch
    , rcOnSpanSelect :: (Text, Int, Int) -> (Int, Int) -> e
      -- ^ drag selection released: verse ref + inclusive token span
    , rcOnPrev       :: e           -- ^ Left arrow
    , rcOnNext       :: e           -- ^ Right arrow
    , rcRangeSelect  :: Bool        -- ^ verse-range select mode (weave workbench)
    , rcOnVerseClick :: (Text, Int, Int) -> Bool -> e
      -- ^ in range-select mode: clicked verse ref + whether Shift was held
    }

-- A word placed on a line. Verse numbers and note text are PWords without
-- a token.
data PWord = PWord
    { pwX     :: !Double
    , pwWidth :: !Double
    , pwText  :: !Text
    , pwFont  :: !Font
    , pwSize  :: !FontSize
    , pwColor :: !Color
    , pwTok   :: !(Maybe RTok)
    }

data RLine = RLine
    { rlY     :: !Double
    , rlH     :: !Double
    , rlBase  :: !Double  -- ^ baseline offset from line top
    , rlWords :: ![PWord]
    }

data ReaderState = ReaderState
    { rsKey      :: !Text
    , rsWidth    :: !Double
    , rsViewH    :: !Double
    , rsLines    :: ![RLine]
    , rsContentH :: !Double
    , rsOffset   :: !Double
    , rsHover    :: !(Maybe (Int, Int))
    , rsAnchor   :: !(Maybe ((Text, Int, Int), Int))
    , rsSel      :: !(Maybe ((Text, Int, Int), Int, Int))
    , rsDragged  :: !Bool
    }

emptyState :: Text -> ReaderState
emptyState key = ReaderState key 0 0 [] 0 0 Nothing Nothing Nothing False

-- palette (dark theme)
bodyColor, titleColor, numColor, noteColor, underlineColor, selColor :: Color
bodyColor = rgbHex "#D8D4CD"
titleColor = rgbHex "#8E8B85"
numColor = rgbHex "#6E6B66"
noteColor = rgbHex "#8A8273"
underlineColor = rgbHex "#7FB4E6"
selColor = rgbHex "#2F4156"

-- soft warm wash behind words that belong to the open thread
markColor :: Color
markColor = rgbHex "#3B331D"

patchColor, patchWarnColor, sbTrackColor, sbThumbColor :: Color
patchColor = rgbHex "#D9A95B"
patchWarnColor = rgbHex "#D97C5B"
sbTrackColor = rgbHex "#3A3A3A"
sbThumbColor = rgbHex "#5C5C5C"

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

gutterW, rightPad, maxTextW, verseGap, lineStep :: Double
gutterW = 46
rightPad = 26
maxTextW = 700
verseGap = 9
lineStep = 64

readerView :: (WidgetModel s, WidgetEvent e) => ReaderCfg e -> WidgetNode s e
readerView cfg = node
  where
    widget = makeReader cfg (emptyState (rcKey cfg))
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

    merge wenv node _oldNode oldState
        | rsKey oldState == rcKey cfg =
            -- same chapter, possibly new overlay/notes/type: relayout in place
            let st = relayout wenv (rsWidth oldState) (rsViewH oldState) oldState
            in resultReqs (replace st node) [RenderOnce]
        | otherwise =
            let st = relayout wenv (rsWidth oldState) (rsViewH oldState)
                        (emptyState (rcKey cfg))
            in resultReqs (replace st node) [RenderOnce]

    getSizeReq _wenv _node = (expandSize 100 1, expandSize 100 1)

    resize wenv node vp = resultNode (replace st node)
      where
        Rect _ _ w h = vp
        st  | w /= rsWidth state || h /= rsViewH state = relayout wenv w h state
            | otherwise = state

    relayout wenv w h st =
        let fm = wenv ^. L.fontManager
            (lns, contentH) = layoutVerses fm w
                (rcBodySize cfg) (rcLineSpacing cfg) (rcVerses cfg)
        in st { rsWidth = w, rsViewH = h, rsLines = lns, rsContentH = contentH
              , rsOffset = clampOffset contentH h (rsOffset st)
              }

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
        Click p BtnLeft _ -> onClickWord p
        Click p BtnRight _ -> onAltClick p
        WheelScroll _ (Point _ wy) dir ->
            let mul = if dir == WheelNormal then 1 else -1
            in scrollBy (negate wy * 60 * mul)
        KeyAction _ code KeyPressed
            | isKeyUp code -> scrollBy (negate lineStep)
            | isKeyDown code -> scrollBy lineStep
            | isKeyPageUp code -> scrollBy (negate pageStep)
            | isKeyPageDown code -> scrollBy pageStep
            | isKeySpace code -> scrollBy pageStep
            | isKeyHome code -> scrollTo 0
            | isKeyEnd code -> scrollTo (rsContentH state)
            | isKeyLeft code -> Just (resultEvts node [rcOnPrev cfg])
            | isKeyRight code -> Just (resultEvts node [rcOnNext cfg])
        _ -> Nothing
      where
        carea = getContentArea node (currentStyle wenv node)
        pageStep = rsViewH state * 0.85

        scrollBy d = scrollTo (rsOffset state + d)
        scrollTo o =
            let o' = clampOffset (rsContentH state) (rsViewH state) o
            in if o' == rsOffset state
                then Just (resultNode node)
                else Just (resultReqs
                    (replace state { rsOffset = o' } node) [RenderOnce])

        hitWord p = do
            (li, wi) <- hitTest state carea p
            wordTokenAt state li wi

        leftHeld = M.lookup BtnLeft
            (wenv ^. L.inputStatus . L.buttons) == Just BtnPressed

        shiftHeld =
            let km = wenv ^. L.inputStatus . L.keyMod
            in km ^. L.leftShift || km ^. L.rightShift

        onMove p =
            let h = hitTest state carea p
                dragSel = do
                    (ref, aIx) <- rsAnchor state
                    rt <- hitWord p
                    if rtRef rt == ref && isNothing (rtPatch rt)
                        then Just (ref, min aIx (rtIx rt), max aIx (rtIx rt))
                        else rsSel state
                st  | leftHeld && isJust (rsAnchor state) =
                        let sel = dragSel
                            moved = case (sel, rsAnchor state) of
                                (Just (_, s, e), Just _) -> s /= e
                                _ -> False
                        in state { rsHover = h, rsSel = sel
                                 , rsDragged = rsDragged state || moved }
                    | otherwise = state { rsHover = h }
                changed = rsHover st /= rsHover state
                    || rsSel st /= rsSel state || rsDragged st /= rsDragged state
            in if changed
                then Just (resultReqs (replace st node) [RenderOnce])
                else Nothing

        onPress p
            -- range-select mode (weave workbench): no word-drag, so a plain
            -- click is read as a verse pick by 'onClickWord'
            | rcRangeSelect cfg = Just (resultReqs (replace
                state { rsAnchor = Nothing, rsSel = Nothing, rsDragged = False }
                node) [RenderOnce])
            | otherwise =
                let anchor = do
                        rt <- hitWord p
                        if isJust (rtPatch rt) then Nothing
                            else Just (rtRef rt, rtIx rt)
                    st = state { rsAnchor = anchor, rsSel = Nothing
                               , rsDragged = False }
                in Just (resultReqs (replace st node) [RenderOnce])

        onRelease = case (rsDragged state, rsSel state) of
            (True, Just (ref, s, e)) | e > s ->
                let st = state { rsAnchor = Nothing, rsSel = Nothing }
                in Just (resultReqsEvts (replace st node) [RenderOnce]
                    [rcOnSpanSelect cfg ref (s, e)])
            _ ->
                let st = state { rsAnchor = Nothing, rsSel = Nothing
                               , rsDragged = False }
                in Just (resultReqs (replace st node) [RenderOnce])

        onClickWord p = do
            rt <- hitWord p
            if rcRangeSelect cfg
                then Just (resultEvts node
                    [rcOnVerseClick cfg (rtRef rt) shiftHeld])
                else if null (tokStrongs (rtTok rt))
                    then Just (resultNode node)
                    else Just (resultEvts node [rcOnWordClick cfg rt])

        onAltClick p
            | rcRangeSelect cfg = Nothing
            | otherwise = do
                rt <- hitWord p
                -- patched words too: the app decides what editing one means
                -- (rule hits offer exclusion; patch hits explain themselves)
                Just (resultEvts node [rcOnWordAlt cfg rt])

    render wenv node renderer = do
        let style = currentStyle wenv node
            carea = getContentArea node style
            Rect cx cy cw chh = carea
            fm = wenv ^. L.fontManager
            st = state
            offset = rsOffset st
            visible ln = rlY ln + rlH ln >= offset && rlY ln <= offset + chh
            selected pw = case (rsSel st, pwTok pw) of
                (Just (ref, s, e), Just rt) ->
                    rtRef rt == ref && rtIx rt >= s && rtIx rt <= e
                _ -> False
        forM_ (zip [0 ..] (rsLines st)) $ \(li, ln) -> when (visible ln) $ do
            let lineTop = cy + rlY ln - offset
                baseY = lineTop + rlBase ln
            forM_ (zip [0 ..] (rlWords ln)) $ \(wi, pw) -> do
                when (maybe False rtMark (pwTok pw)) $
                    drawRect renderer
                        (Rect (cx + pwX pw - 1) (lineTop + 1)
                            (pwWidth pw + 3) (rlH ln - 2))
                        (Just markColor) Nothing
                when (selected pw) $
                    drawRect renderer
                        (Rect (cx + pwX pw - 1) (lineTop + 1)
                            (pwWidth pw + 3) (rlH ln - 2))
                        (Just selColor) Nothing
                setFillColor renderer (pwColor pw)
                renderText renderer (Point (cx + pwX pw) baseY)
                    (pwFont pw) (pwSize pw) def (pwText pw)
                let hovered = rsHover st == Just (li, wi)
                    patched = rtPatch =<< pwTok pw
                    hasStrongs =
                        maybe False (not . null . tokStrongs . rtTok) (pwTok pw)
                -- patched words carry a persistent dotted underline
                forM_ patched $ \pinfo -> do
                    let dotColor = if piWarn pinfo
                            then patchWarnColor else patchColor
                        y = baseY + 3
                        xs = takeWhile (< pwX pw + pwWidth pw - 2)
                            [pwX pw, pwX pw + 5 ..]
                    forM_ xs $ \dx ->
                        drawRect renderer (Rect (cx + dx) y 2.5 1.2)
                            (Just dotColor) Nothing
                when (hovered && hasStrongs && isNothing patched) $
                    drawRect renderer
                        (Rect (cx + pwX pw) (baseY + 3) (pwWidth pw) 1.2)
                        (Just underlineColor) Nothing
        -- minimal scrollbar
        when (rsContentH st > chh) $ do
            let trackX = cx + cw - 6
                thumbH = max 30 (chh * chh / rsContentH st)
                thumbY = cy + (chh - thumbH)
                    * (offset / (rsContentH st - chh))
            drawRect renderer (Rect trackX cy 4 chh) (Just sbTrackColor) Nothing
            drawRect renderer (Rect trackX thumbY 4 thumbH)
                (Just sbThumbColor) Nothing
        -- patch hover card, drawn last so it sits on top
        forM_ (rsHover st) $ \(li, wi) ->
            forM_ (cardFor st li wi) $ \(pw, ln, pinfo) -> do
                let baseY = cy + rlY ln - offset + rlBase ln
                drawCard renderer fm carea (cx + pwX pw) baseY pinfo

    cardFor st li wi = do
        ln <- nth li (rsLines st)
        pw <- nth wi (rlWords ln)
        rt <- pwTok pw
        pinfo <- rtPatch rt
        Just (pw, ln, pinfo)
      where
        nth i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

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

clampOffset :: Double -> Double -> Double -> Double
clampOffset contentH viewH = max 0 . min (max 0 (contentH - viewH))

wordTokenAt :: ReaderState -> Int -> Int -> Maybe RTok
wordTokenAt st li wi = do
    ln <- nth li (rsLines st)
    pw <- nth wi (rlWords ln)
    pwTok pw
  where
    nth i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

hitTest :: ReaderState -> Rect -> Point -> Maybe (Int, Int)
hitTest st (Rect cx cy cw chh) (Point px py)
    | px < cx || px > cx + cw || py < cy || py > cy + chh = Nothing
    | otherwise =
        let y = py - cy + rsOffset st
            x = px - cx
            inLine ln = y >= rlY ln && y < rlY ln + rlH ln
            inWord pw = x >= pwX pw && x <= pwX pw + pwWidth pw
                && isJust (pwTok pw)
        in case filter (inLine . snd) (zip [0 ..] (rsLines st)) of
            [] -> Nothing
            ((li, ln) : _) ->
                case filter (inWord . snd) (zip [0 ..] (rlWords ln)) of
                    [] -> Nothing
                    ((wi, _) : _) -> Just (li, wi)

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
    -- computeTextSize returns ink bounds: a lone " " reports ~2px while the
    -- real space advance is ~5px. The subtraction cancels the ink bias.
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
            (y1, acc1) = addLines y acc $ wrapTokens
                titleFont titleSize titleColor titleSpace titleLH titleBase
                (Nothing :: Maybe Int) titleToks
            (y2, acc2) = addLines y1 acc1 $ wrapTokens
                bodyFont bodySize bodyColor bodySpace bodyLH bodyBase
                (Just (rvNum v)) bodyToks
            (y3, acc3) = foldl addNote (y2, acc2) (rvNotes v)
        in go (y3 + verseGap) vs acc3

    addNote (y, acc) note = addLines y acc
        (wrapPlain numFont noteSize noteColor noteSpace noteLH noteBase
            ("† " <> note))

    addLines y acc = foldl step (y, acc)
      where
        step (yy, aa) mk = let ln = mk yy in (yy + rlH ln, ln : aa)

    -- plain text (notes): wrapped, indented, no tokens
    wrapPlain fnt sz col spw lh base txt =
        let indent = gutterW + 14
            place (linesAcc, cur, x) w =
                let ww = measure fnt sz w
                    pw = PWord x ww w fnt sz col Nothing
                in if x + ww > gutterW + textW && not (null cur)
                    then (reverse cur : linesAcc, [pw { pwX = indent }],
                          indent + ww + spw)
                    else (linesAcc, pw : cur, x + ww + spw)
            (doneRev, lastRev, _) = foldl place ([], [], indent) (T.words txt)
            wordLines = reverse
                (filter (not . null) (reverse lastRev : doneRev))
        in [ \yy -> RLine yy lh base ws | ws <- wordLines ]

    -- wraps tokens into lines; returns line builders awaiting their y position
    wrapTokens fnt sz col spw lh base mnum toks =
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
                    pw = PWord x w txt f sz c (Just t)
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
                    in PWord x nw t numFont numSize numColor Nothing : ws
                _ -> ws
        in [ \yy -> RLine yy lh base (withNum i ws)
           | (i, ws) <- zip [0 :: Int ..] wordLines
           ]

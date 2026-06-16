{-# LANGUAGE OverloadedStrings #-}

-- | A single, shared overview of the whole Bible drawn as one horizontal strip:
-- the 66 books left to right, banded into their sections (Law, History, Wisdom,
-- Prophets | Gospels, Acts, Letters, Revelation) with the Old/New Testament
-- divider marked, and a coloured pin per reading pane showing where in the
-- canon that pane is sitting. It is its own section of the screen so you can see
-- at a glance where across Scripture a weave is reaching.
module Overlay.CanonMap
    ( CanonSeg (..)
    , CanonPin (..)
    , CanonMapCfg (..)
    , canonMapView
    ) where

import Control.Lens ((^.))
import Control.Monad (when, zipWithM_)
import Data.Default (def)
import Data.Text (Text)

import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

-- | One labelled band of the canon (a section), as fractions 0…1 of the whole.
data CanonSeg = CanonSeg
    { csLabel :: !Text
    , csStart :: !Double
    , csEnd   :: !Double
    , csNT    :: !Bool
    }

-- | A pane's position on the map: where it sits (0…1), a short label, a colour.
data CanonPin = CanonPin
    { cpFrac  :: !Double
    , cpLabel :: !Text
    , cpColor :: !Color
    }

data CanonMapCfg e = CanonMapCfg
    { cmcSegs    :: ![CanonSeg]
    , cmcPins    :: ![CanonPin]
    , cmcDivider :: !Double      -- ^ fraction at the Old/New Testament seam
    , cmcOnClick :: !(Maybe (Double -> e))
      -- ^ click anywhere on the strip to jump there; carries the fraction 0…1
    }

mapH :: Double
mapH = 62

bandColorOT, bandColorNT, bandAltOT, bandAltNT :: Color
bandColorOT = rgbHex "#272B30"
bandColorNT = rgbHex "#2C2A26"
bandAltOT = rgbHex "#2E343A"
bandAltNT = rgbHex "#36322B"

sepColor, dividerColor, segTextColor, testTextColor :: Color
sepColor = rgba 20 20 20 0.7
dividerColor = rgba 210 180 110 0.85
segTextColor = rgba 180 176 168 0.85
testTextColor = rgba 150 146 138 0.9

mapFont :: Font
mapFont = "Regular"

canonMapView :: WidgetEvent e => CanonMapCfg e -> WidgetNode s e
canonMapView cfg = defaultWidgetNode (WidgetType "canonMap") widget
  where
    widget = createSingle () def
        { singleGetSizeReq = getSizeReq
        , singleHandleEvent = handleEvent
        , singleRender = render
        }

    getSizeReq _wenv _node = (expandSize 100 1, fixedSize mapH)

    -- a click anywhere on the strip jumps to that point in the canon
    handleEvent wenv node _target evt = case evt of
        Click (Point px _) BtnLeft _ -> case cmcOnClick cfg of
            Just toEvent ->
                let style = currentStyle wenv node
                    Rect cx _ cw _ = getContentArea node style
                    frac = max 0 (min 1 ((px - cx) / max 1 cw))
                in Just (resultEvts node [toEvent frac])
            Nothing -> Nothing
        _ -> Nothing

    render wenv node renderer = do
        let style = currentStyle wenv node
            Rect cx cy cw ch = getContentArea node style
            fm = wenv ^. L.fontManager
            xAt f = cx + max 0 (min 1 f) * cw
            bandTop = cy + 30
            bandH = ch - 30 - 4
            segSize = FontSize 10
            pinSize = FontSize 10
            testSize = FontSize 9

        -- section bands, alternating shade, OT cool / NT warm
        let drawSeg i (CanonSeg lbl s e nt) = do
                let x0 = xAt s
                    x1 = xAt e
                    w = max 0 (x1 - x0)
                    alt = even (i :: Int)
                    col | nt = if alt then bandColorNT else bandAltNT
                        | otherwise = if alt then bandColorOT else bandAltOT
                drawRect renderer (Rect x0 bandTop w bandH) (Just col) Nothing
                drawRect renderer (Rect (x1 - 0.5) bandTop 1 bandH) (Just sepColor) Nothing
                -- label, centred, only when it fits
                let tw = _sW (computeTextSize fm mapFont segSize def lbl)
                when (tw + 6 < w) $ do
                    setFillColor renderer segTextColor
                    renderText renderer
                        (Point (x0 + (w - tw) / 2) (bandTop + bandH / 2 + 4))
                        mapFont segSize def lbl
        zipWithM_ drawSeg [0 ..] (cmcSegs cfg)

        -- Old / New Testament divider — kept inside the band so it never cuts
        -- through a pin or its label above; tags sit on the top row
        let dx = xAt (cmcDivider cfg)
            otW = _sW (computeTextSize fm mapFont testSize def "OT")
        drawRect renderer (Rect (dx - 1) bandTop 2 bandH) (Just dividerColor) Nothing
        setFillColor renderer testTextColor
        renderText renderer (Point (dx - 5 - otW) (cy + 11)) mapFont testSize def "OT"
        renderText renderer (Point (dx + 5) (cy + 11)) mapFont testSize def "NT"

        -- one pin per pane: its label on its own row, a coloured head sitting on
        -- the band, a short tick joining them (labels sit below the OT/NT row, so
        -- a pin at the divider — Matthew — no longer collides with the tags)
        let drawPin (CanonPin f lbl col) = do
                let x = xAt f
                    tw = _sW (computeTextSize fm mapFont pinSize def lbl)
                drawRect renderer (Rect (x - 0.75) (cy + 24) 1.5 (bandTop - cy - 24))
                    (Just col) Nothing
                drawRect renderer (Rect (x - 3) (bandTop - 3) 6 6) (Just col) Nothing
                setFillColor renderer col
                renderText renderer
                    (Point (x - tw / 2) (cy + 22)) mapFont pinSize def lbl
        mapM_ drawPin (cmcPins cfg)

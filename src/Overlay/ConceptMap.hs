{-# LANGUAGE OverloadedStrings #-}

-- | A dispersion strip drawn in the same coordinate space as the canon map:
-- one to four Strong's-number concepts laid across the 66 books, each book
-- shaded by how densely that concept occurs there. It answers \"where, and how
-- often, does this concept occur\" at a glance, and lines up book-for-book with
-- the canon overview below it (book @i@ spans @[i/66, (i+1)/66]@, the same
-- fractions the canon map's section bands and OT/NT divider use).
--
-- Counts are over the Strong's tag — the original-language lemma — so the
-- footprint reflects the underlying word, not the KJV's varied renderings of it.
module Overlay.ConceptMap
    ( ConceptSeries (..)
    , ConceptMapCfg (..)
    , conceptMapView
    ) where

import Control.Lens ((^.))
import Control.Monad (foldM_, forM_, when)
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

-- | One concept's per-book occurrence counts, with a display label.
data ConceptSeries = ConceptSeries
    { cseLabel  :: !Text
    , cseCounts :: !(Map Text Int)  -- ^ OSIS book id -> token-instance count
    }

data ConceptMapCfg e = ConceptMapCfg
    { cmpBooks   :: ![Text]            -- ^ OSIS ids in canon order (defines the x axis)
    , cmpSeries  :: ![ConceptSeries]   -- ^ 1…4 concepts, drawn as stacked rows
    , cmpDivider :: !Double            -- ^ OT/NT seam as a fraction 0…1
    , cmpOnClick :: !(Maybe (Double -> e))  -- ^ click anywhere → canon fraction 0…1
    }

mapH :: Double
mapH = 34

-- | Distinct colours per series, matching the canon map's pane palette.
seriesRGB :: Int -> (Int, Int, Int)
seriesRGB i = case i `mod` 4 of
    0 -> (210, 180, 110)
    1 -> (127, 180, 230)
    2 -> (143, 184, 138)
    _ -> (217, 140, 140)

mapFont :: Font
mapFont = "Regular"

conceptMapView :: WidgetEvent e => ConceptMapCfg e -> WidgetNode s e
conceptMapView cfg = defaultWidgetNode (WidgetType "conceptMap") widget
  where
    widget = createSingle () def
        { singleGetSizeReq = getSizeReq
        , singleHandleEvent = handleEvent
        , singleRender = render
        }

    getSizeReq _wenv _node = (expandSize 100 1, fixedSize mapH)

    handleEvent wenv node _target evt = case evt of
        Click (Point px _) BtnLeft _ -> case cmpOnClick cfg of
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
            books = cmpBooks cfg
            nb = max 1 (length books)
            series = take 4 (cmpSeries cfg)
            ns = max 1 (length series)
            labelH = 12
            bandTop = cy + labelH
            bandH = ch - labelH
            rowH = bandH / fromIntegral ns
            xAt f = cx + max 0 (min 1 f) * cw
            cellW = max 1 (cw / fromIntegral nb)

        -- faint base track so the strip reads as one band even where sparse
        drawRect renderer (Rect cx bandTop cw bandH) (Just (rgba 20 22 25 0.55)) Nothing

        -- each concept is a row of per-book heat cells, shaded by density
        forM_ (zip [(0 :: Int) ..] series) $ \(k, ConceptSeries _ counts) -> do
            let (r, g, b) = seriesRGB k
                mx = fromIntegral (maximum (1 : M.elems counts)) :: Double
                rowTop = bandTop + fromIntegral k * rowH
            forM_ (zip [(0 :: Int) ..] books) $ \(i, bid) -> do
                let c = M.findWithDefault 0 bid counts
                when (c > 0) $ do
                    let x0 = xAt (fromIntegral i / fromIntegral nb)
                        a = 0.25 + 0.75 * (fromIntegral c / mx)
                    drawRect renderer (Rect x0 (rowTop + 0.5) cellW (rowH - 1))
                        (Just (rgba r g b a)) Nothing

        -- the Old / New Testament seam, matching the canon map
        let dx = xAt (cmpDivider cfg)
        drawRect renderer (Rect (dx - 0.5) bandTop 1 bandH)
            (Just (rgba 210 180 110 0.7)) Nothing

        -- legend: each concept's label in its colour along the top row
        let drawLegend xoff (k, ConceptSeries lbl _) = do
                let (r, g, b) = seriesRGB k
                    sz = FontSize 10
                    tw = _sW (computeTextSize fm mapFont sz def lbl)
                drawRect renderer (Rect xoff (cy + 1) 8 8) (Just (rgb r g b)) Nothing
                setFillColor renderer (rgb r g b)
                renderText renderer (Point (xoff + 12) (cy + 9)) mapFont sz def lbl
                pure (xoff + 12 + tw + 16)
        foldM_ drawLegend (cx + 2) (zip [(0 :: Int) ..] series)

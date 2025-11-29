-- | Scales Demo: Visualizing Functional Programming with Scales
-- |
-- | This module demonstrates how scales embody FP principles:
-- | - Profunctor-like behavior (contramap, map, dimap)
-- | - Scale composition (andThen)
-- | - Sampling as unfold
-- | - Modifiers as endomorphism composition
module D3.Viz.FPFTW.ScalesDemo
  ( drawScalesDemo
  , drawGradientComparison
  , drawScalePipeline
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Effect (Effect)
import PSD3.Scale as Scale
import PSD3.Scale (applyScale)
import PSD3.Scale.FP (normalize)

-- ============================================================================
-- FFI for D3 rendering
-- ============================================================================

foreign import drawGradientStrip_ :: String -> String -> Array String -> Effect Unit
foreign import drawPipelineDiagram_ :: String -> Array { x :: Number, label :: String, color :: String } -> Effect Unit
foreign import drawScaleComposition_ :: String -> Effect Unit

-- ============================================================================
-- GRADIENT COMPARISON
-- | Shows multiple interpolators side-by-side as color strips
-- ============================================================================

drawGradientComparison :: String -> Effect Unit
drawGradientComparison selector = do
  -- Sample each interpolator at 100 points
  let viridisColors = Array.range 0 99 <#> \i -> Scale.interpolateViridis (toNumber i / 99.0)
  let plasmaColors = Array.range 0 99 <#> \i -> Scale.interpolatePlasma (toNumber i / 99.0)
  let infernoColors = Array.range 0 99 <#> \i -> Scale.interpolateInferno (toNumber i / 99.0)
  let turboColors = Array.range 0 99 <#> \i -> Scale.interpolateTurbo (toNumber i / 99.0)
  let rdYlGnColors = Array.range 0 99 <#> \i -> Scale.interpolateRdYlGn (toNumber i / 99.0)

  -- Draw each gradient strip
  drawGradientStrip_ (selector <> "-viridis") "Viridis" viridisColors
  drawGradientStrip_ (selector <> "-plasma") "Plasma" plasmaColors
  drawGradientStrip_ (selector <> "-inferno") "Inferno" infernoColors
  drawGradientStrip_ (selector <> "-turbo") "Turbo" turboColors
  drawGradientStrip_ (selector <> "-rdylgn") "RdYlGn (Diverging)" rdYlGnColors

-- ============================================================================
-- SCALE PIPELINE
-- | Demonstrates scale composition: data → normalize → color
-- ============================================================================

drawScalePipeline :: String -> Effect Unit
drawScalePipeline selector = do
  -- Example: Temperature data flowing through a pipeline
  -- Raw temps: [-10, 0, 15, 25, 40] °C
  -- Step 1: Normalize to [0, 1]
  -- Step 2: Apply color scale

  let temps = [-10.0, 0.0, 15.0, 25.0, 40.0]

  -- Create the normalization scale
  let normalizer = normalize (-10.0) 40.0

  -- Normalize each temperature
  let normalized = temps <#> applyScale normalizer

  -- Apply color
  let colors = normalized <#> Scale.interpolateRdYlGn

  -- Create pipeline visualization data
  let pipelineData = Array.zipWith (\temp color ->
        { x: temp
        , label: show temp <> "°C"
        , color
        }) temps colors

  drawPipelineDiagram_ selector pipelineData

-- ============================================================================
-- MAIN DEMO
-- | Combines all scale demos
-- ============================================================================

drawScalesDemo :: String -> Effect Unit
drawScalesDemo selector = do
  drawGradientComparison (selector <> "-gradients")
  drawScalePipeline (selector <> "-pipeline")
  drawScaleComposition_ (selector <> "-composition")

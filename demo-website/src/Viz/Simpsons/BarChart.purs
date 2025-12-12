-- | Bar Chart - Mini bars for data table
-- |
-- | Small bars showing applied/admitted counts in the illustration table.
module D3.Viz.Simpsons.BarChart
  ( miniBar
  , MiniBarConfig
  , defaultConfig
  ) where

import Prelude

import D3.Viz.Simpsons.Types (green, purple, gray, Gender(..))
-- v3 Integration: all attributes via v3Attr/v3AttrStr (no ToAttr typeclass)
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree (Tree)
import PSD3v2.VizTree.Tree as T

-- =============================================================================
-- Configuration
-- =============================================================================

type MiniBarConfig =
  { width :: Number
  , height :: Number
  , maxValue :: Number
  }

defaultConfig :: MiniBarConfig
defaultConfig =
  { width: 50.0
  , height: 20.0
  , maxValue: 2691.0  -- Max population (male count)
  }

-- =============================================================================
-- Mini Bar
-- =============================================================================

-- | Create a mini bar for displaying a value
-- |
-- | Parameters:
-- | - config: Bar dimensions and scale
-- | - value: The value to display
-- | - gender: Gender for color coding
miniBar :: MiniBarConfig -> Number -> Gender -> Tree Unit
miniBar config value gender =
  let
    barWidth = (value / config.maxValue) * config.width
    barColor = case gender of
      Male -> purple
      Female -> green
  in
    T.elem Group
      []
      `T.withChildren`
        [ -- Background border
          T.elem Rect
            [ v3Attr "width" (lit config.width)
            , v3Attr "height" (lit config.height)
            , v3AttrStr "fill" (str "none")
            , v3AttrStr "stroke" (str gray)
            , v3Attr "stroke-width" (lit 2.0)
            ]
        -- Filled bar
        , T.elem Rect
            [ v3Attr "width" (lit barWidth)
            , v3Attr "height" (lit config.height)
            , v3AttrStr "fill" (str barColor)
            ]
        ]

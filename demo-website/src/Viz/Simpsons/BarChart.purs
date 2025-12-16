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
import PSD3.Expr.Friendly (num, text, fill, stroke, strokeWidth, width, height)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree)
import PSD3.AST as T

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
            [ width $ num config.width
            , height $ num config.height
            , fill $ text "none"
            , stroke $ text gray
            , strokeWidth $ num 2.0
            ]
        -- Filled bar
        , T.elem Rect
            [ width $ num barWidth
            , height $ num config.height
            , fill $ text barColor
            ]
        ]

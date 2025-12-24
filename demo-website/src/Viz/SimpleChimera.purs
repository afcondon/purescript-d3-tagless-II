-- | Simple Chimera - Minimal test of ConditionalRender
-- |
-- | Demonstrates the simplest possible chimeric visualization:
-- | - Static data (no force simulation)
-- | - Single join with conditional rendering
-- | - Monomorphic: entire tree has type Tree Circle
-- | - Circles render as either big/red or small/blue based on size property
module D3.Viz.SimpleChimera
  ( simpleChimera
  ) where

import Prelude

import Effect (Effect)
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, r, fill, stroke, strokeWidth)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- =============================================================================
-- Data Type
-- =============================================================================

type Circle =
  { x :: Number
  , y :: Number
  , size :: Int
  }

-- =============================================================================
-- Test Data
-- =============================================================================

testCircles :: Array Circle
testCircles =
  [ { x: 100.0, y: 100.0, size: 15 }  -- Big
  , { x: 200.0, y: 100.0, size: 5 }   -- Small
  , { x: 300.0, y: 100.0, size: 20 }  -- Big
  , { x: 400.0, y: 100.0, size: 8 }   -- Small
  , { x: 150.0, y: 200.0, size: 12 }  -- Big
  , { x: 250.0, y: 200.0, size: 3 }   -- Small
  , { x: 350.0, y: 200.0, size: 18 }  -- Big
  ]

-- =============================================================================
-- Templates
-- =============================================================================

-- Big circles: Red, radius 20
bigCircleTemplate :: Circle -> T.Tree Circle
bigCircleTemplate _circ =
  T.elem Circle
    [ r $ num 20.0
    , fill $ text "#e74c3c"
    , stroke $ text "#c0392b"
    , strokeWidth $ num 2.0
    ]

-- Small circles: Blue, radius 10
smallCircleTemplate :: Circle -> T.Tree Circle
smallCircleTemplate _circ =
  T.elem Circle
    [ r $ num 10.0
    , fill $ text "#3498db"
    , stroke $ text "#2980b9"
    , strokeWidth $ num 2.0
    ]

-- =============================================================================
-- Visualization
-- =============================================================================

simpleChimera :: String -> Effect Unit
simpleChimera selector = do
  runD3v2M do
    -- Step 1: Create SVG scaffolding (Tree Unit)
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      scaffoldTree :: T.Tree Unit
      scaffoldTree =
        T.named SVG "svg"
          [ width $ num 500.0
          , height $ num 300.0
          , viewBox 0.0 0.0 500.0 300.0
          , attr "id" $ text "simple-chimera-svg"
          ]
          `T.withChildren`
            [ T.named Group "circles-group" [ attr "id" $ text "circles-group" ]
            ]
    _ <- renderTree container scaffoldTree

    -- Step 2: Select the group and render the data join (Tree Circle)
    circlesGroup <- select "#circles-group" :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      -- MONOMORPHIC: Tree has type Tree Circle
      vizTree :: T.Tree Circle
      vizTree =
        T.joinData "circles" "g" testCircles $ \circ ->
          T.elem Group
            [ attr "transform" $ text ("translate(" <> show circ.x <> "," <> show circ.y <> ")")
            ]
            `T.withChildren`
              [ -- CONDITIONAL RENDER: Choose template based on size
                T.conditionalRender
                  [ { predicate: \c -> c.size >= 10
                    , spec: bigCircleTemplate
                    }
                  , { predicate: \c -> c.size < 10
                    , spec: smallCircleTemplate
                    }
                  ]
              ]
    _ <- renderTree circlesGroup vizTree

    pure unit

module D3.Viz.TreeAPI.ParabolaNoAxes where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, cx, cy, r, fill)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree, joinData)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Parabola (No Axes) - Data-driven positioning
-- |
-- | Data: 10 points where y = x²
-- | Visual: Ten green circles forming a parabola
-- |
-- | Key concept: Y-position comes from the data itself

type ParabolaPoint = { x :: Number, y :: Number }

-- Generate 10 points where y = x²
parabolaData :: Array ParabolaPoint
parabolaData = map makePoint (range 0 9)
  where
    makePoint :: Int -> ParabolaPoint
    makePoint i =
      let x = toNumber i
          y = x * x  -- y = x²
      in { x, y }

parabolaNoAxes :: String -> Effect Unit
parabolaNoAxes selector = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- We need to scale our data to fit the SVG
  -- Data x range: [0, 9], y range: [0, 81]
  -- Very compact size for foundational example
  let padding = 30.0
      svgWidth = 400.0
      svgHeight = 200.0
      plotWidth = svgWidth - (2.0 * padding)
      plotHeight = svgHeight - (2.0 * padding)

      -- Simple linear scaling
      scaleX :: Number -> Number
      scaleX x = padding + (x / 9.0) * plotWidth

      scaleY :: Number -> Number
      scaleY y = svgHeight - padding - (y / 81.0) * plotHeight  -- Invert y (SVG coordinates)

  -- Define the tree with a DATA JOIN
  let tree :: Tree ParabolaPoint
      tree =
        T.named SVG "svg"
          [ width $ num svgWidth
          , height $ num svgHeight
          , viewBox 0.0 0.0 svgWidth svgHeight
          , attr "id" $ text "parabola-no-axes-svg"
          , attr "class" $ text "tree-api-example"
          ]
          `T.withChild`
            -- joinData creates one circle per point
            -- Position comes from the data: x and y = x²
            (joinData "circles" "circle" parabolaData $ \d ->
              T.elem Circle
                [ cx $ num (scaleX d.x)        -- X from datum
                , cy $ num (scaleY d.y)        -- Y from datum (y = x²)
                , r $ num 5.0             -- Small circles
                , fill $ text "green"           -- All green
                ])

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Parabola (No Axes) ==="
    Console.log ""
    Console.log "Data: 10 points where y = x²"
    Console.log "Result: 10 circles forming a parabola"
    Console.log ""

    case Map.lookup "circles" selections of
      Just _ -> Console.log "✓ Circles positioned by data (y = x²)"
      Nothing -> Console.log "✗ Missing circles"

    Console.log ""
    Console.log "The key concept: Position is a function of the data."
    Console.log "Each point (x, y) where y = x² gets mapped to a circle."
    Console.log "We can see the mathematical relationship visually!"
    Console.log ""

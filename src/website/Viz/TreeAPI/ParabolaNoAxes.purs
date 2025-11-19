module D3.Viz.TreeAPI.ParabolaNoAxes where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Attribute.Types (width, height, viewBox, id_, class_, cx, cy, radius, fill)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree (Tree, joinData)
import PSD3v2.VizTree.Tree as T
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
  -- Compact size for foundational example
  let padding = 30.0
      svgWidth = 300.0
      svgHeight = 225.0
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
          [ width svgWidth
          , height svgHeight
          , viewBox ("0 0 " <> show svgWidth <> " " <> show svgHeight)
          , id_ "parabola-no-axes-svg"
          , class_ "tree-api-example"
          ]
          `T.withChild`
            -- joinData creates one circle per point
            -- Position comes from the data: x and y = x²
            (joinData "circles" "circle" parabolaData $ \d ->
              T.elem Circle
                [ cx (scaleX d.x)        -- X from datum
                , cy (scaleY d.y)        -- Y from datum (y = x²)
                , radius 5.0             -- Small circles
                , fill "green"           -- All green
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

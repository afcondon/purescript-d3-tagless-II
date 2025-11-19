module D3.Viz.TreeAPI.ParabolaWithAxes where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Attribute.Types (width, height, viewBox, id_, class_, cx, cy, radius, fill, transform)
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree (Tree, joinData)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Parabola (With Axes) - Introducing scales and axes
-- |
-- | Data: 10 points where y = x²
-- | Visual: Ten green circles forming a parabola with X and Y axes
-- |
-- | Key concepts: Scales map data space → pixel space
-- |               Axes provide visual reference

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

parabolaWithAxes :: String -> Effect Unit
parabolaWithAxes selector = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Define dimensions and margins
  let margin = { top: 20.0, right: 20.0, bottom: 50.0, left: 60.0 }
      -- Compact size for foundational example
      svgWidth = 300.0
      svgHeight = 225.0
      plotWidth = svgWidth - margin.left - margin.right
      plotHeight = svgHeight - margin.top - margin.bottom

      -- Create scales
      xScale = { domain: { min: 0.0, max: 9.0 }, range: { min: 0.0, max: plotWidth } }
      yScale = { domain: { min: 0.0, max: 81.0 }, range: { min: plotHeight, max: 0.0 } }  -- Inverted for SVG

      -- Scale helper functions
      scaleX :: Number -> Number
      scaleX value =
        let domainSpan = xScale.domain.max - xScale.domain.min
            rangeSpan = xScale.range.max - xScale.range.min
            normalized = (value - xScale.domain.min) / domainSpan
        in xScale.range.min + (normalized * rangeSpan)

      scaleY :: Number -> Number
      scaleY value =
        let domainSpan = yScale.domain.max - yScale.domain.min
            rangeSpan = yScale.range.max - yScale.range.min
            normalized = (value - yScale.domain.min) / domainSpan
        in yScale.range.min + (normalized * rangeSpan)

  -- Define the tree with DATA JOIN and AXES
  let tree :: Tree ParabolaPoint
      tree =
        T.named SVG "svg"
          [ width svgWidth
          , height svgHeight
          , viewBox ("0 0 " <> show svgWidth <> " " <> show svgHeight)
          , id_ "parabola-with-axes-svg"
          , class_ "tree-api-example"
          ]
          `T.withChild`
            -- Main group with margins
            (T.named Group "plot-area"
              [ transform ("translate(" <> show margin.left <> "," <> show margin.top <> ")")
              , class_ "plot-area"
              ]
              `T.withChildren`
                [ -- X-axis (transformed to bottom)
                  T.named Group "x-axis"
                    [ transform ("translate(0," <> show plotHeight <> ")")
                    , class_ "x-axis"
                    ]
                    `T.withChild` renderAxis (axisBottom xScale)
                  -- Y-axis
                , T.named Group "y-axis"
                    [ class_ "y-axis" ]
                    `T.withChild` renderAxis (axisLeft yScale)
                  -- Data points
                , joinData "circles" "circle" parabolaData $ \d ->
                    T.elem Circle
                      [ cx (scaleX d.x)
                      , cy (scaleY d.y)
                      , radius 5.0
                      , fill "green"
                      ]
                ])

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Parabola (With Axes) ==="
    Console.log ""
    Console.log "Data: 10 points where y = x²"
    Console.log "Result: Parabola with labeled axes"
    Console.log ""

    case Map.lookup "circles" selections of
      Just _ -> Console.log "✓ Circles positioned using scales"
      Nothing -> Console.log "✗ Missing circles"

    Console.log ""
    Console.log "Key concepts introduced:"
    Console.log "  - SCALES: Map data values to pixel positions"
    Console.log "  - AXES: Visual reference showing the scale"
    Console.log ""
    Console.log "The x-axis shows values 0-9, y-axis shows 0-81."
    Console.log "Now we can see exactly what the data values are!"
    Console.log ""

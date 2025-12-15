module D3.Viz.TreeAPI.ParabolaWithAxes where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Axis.Axis (axisBottom, axisLeft, renderAxis)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree, joinData)
import PSD3.AST as T
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
      -- Very compact size for foundational example
      svgWidth = 400.0
      svgHeight = 200.0
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
          [ v3Attr "width" (lit svgWidth)
          , v3Attr "height" (lit svgHeight)
          , v3AttrStr "viewBox" (str ("0 0 " <> show svgWidth <> " " <> show svgHeight))
          , v3AttrStr "id" (str "parabola-with-axes-svg")
          , v3AttrStr "class" (str "tree-api-example")
          ]
          `T.withChild`
            -- Main group with margins
            (T.named Group "plot-area"
              [ v3AttrStr "transform" (str ("translate(" <> show margin.left <> "," <> show margin.top <> ")"))
              , v3AttrStr "class" (str "plot-area")
              ]
              `T.withChildren`
                [ -- X-axis (transformed to bottom)
                  T.named Group "x-axis"
                    [ v3AttrStr "transform" (str ("translate(0," <> show plotHeight <> ")"))
                    , v3AttrStr "class" (str "x-axis")
                    ]
                    `T.withChild` renderAxis (axisBottom xScale)
                  -- Y-axis
                , T.named Group "y-axis"
                    [ v3AttrStr "class" (str "y-axis") ]
                    `T.withChild` renderAxis (axisLeft yScale)
                  -- Data points
                , joinData "circles" "circle" parabolaData $ \d ->
                    T.elem Circle
                      [ v3Attr "cx" (lit (scaleX d.x))
                      , v3Attr "cy" (lit (scaleY d.y))
                      , v3Attr "r" (lit 5.0)
                      , v3AttrStr "fill" (str "green")
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

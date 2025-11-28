module D3.Viz.TreeAPI.ThreeLittleCircles where

import Prelude

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

-- | Three Little Circles - The classic D3 example using declarative tree API
-- |
-- | This demonstrates:
-- | - Data joins with the declarative API
-- | - Template functions that customize each element based on datum
-- | - Polymorphic attributes (functions of datum)

type CircleData =
  { x :: Number
  , y :: Number
  , r :: Number
  , color :: String
  }

circleData :: Array CircleData
circleData =
  [ { x: 100.0, y: 100.0, r: 40.0, color: "red" }
  , { x: 200.0, y: 100.0, r: 30.0, color: "green" }
  , { x: 300.0, y: 100.0, r: 50.0, color: "blue" }
  ]

threeLittleCircles :: Effect Unit
threeLittleCircles = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Define the tree with a DATA JOIN
  -- Note: The datum type is CircleData because we're using joinData with CircleData
  let tree :: Tree CircleData
      tree =
        T.named SVG "svg"
          [ width 400.0
          , height 200.0
          , viewBox "0 0 400 200"
          , id_ "three-circles-svg"
          , class_ "tree-api-example"
          ]
          `T.withChild`
            -- Here's the magic: joinData creates N copies of the template
            (joinData "circles" "circle" circleData $ \d ->
              T.elem Circle
                [ cx d.x
                , cy d.y
                , radius d.r
                , fill d.color
                ])

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Three Little Circles (Declarative API) ==="
    Console.log ""
    Console.log "Structure: SVG → [Circle × 3] (via joinData)"
    Console.log ""

    case Map.lookup "svg" selections of
      Just _ -> Console.log "✓ SVG created"
      Nothing -> Console.log "✗ Missing SVG"

    case Map.lookup "circles" selections of
      Just _ -> Console.log "✓ Circles collection created (3 circles via data join)"
      Nothing -> Console.log "✗ Missing circles collection"

    Console.log ""
    Console.log "Expected result:"
    Console.log "  - Red circle (r=40) at (100, 100)"
    Console.log "  - Green circle (r=30) at (200, 100)"
    Console.log "  - Blue circle (r=50) at (300, 100)"
    Console.log ""
    Console.log "Check the browser to verify!"

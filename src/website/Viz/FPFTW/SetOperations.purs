module D3.Viz.FPFTW.SetOperations where

-- | Set operations visualization using phylotaxis layout
-- | Demonstrates mapping one visualizer across four Set operations

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import PSD3v2.Attribute.Types (class_, cx, cy, fill, fillOpacity, height, radius, stroke, strokeWidth, textAnchor, textContent, transform, viewBox, width, x, y)
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_, runD3v2M)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Phylotaxis layout constants (sunflower spiral pattern)
goldenAngle :: Number
goldenAngle = Number.pi * (3.0 - Number.sqrt 5.0)

initialRadius :: Number
initialRadius = 8.0

-- | Calculate phylotaxis position for an element at given index
phylotaxisPosition :: Int -> { x :: Number, y :: Number }
phylotaxisPosition index =
  let
    i = toNumber index
    rad = initialRadius * Number.sqrt (0.5 + i)
    angle = i * goldenAngle
  in
    { x: rad * Number.cos angle
    , y: rad * Number.sin angle
    }

-- | Our example sets
setA :: Set Int
setA = Set.fromFoldable [1, 2, 3, 4, 5, 6, 7, 8]

setB :: Set Int
setB = Set.fromFoldable [4, 5, 6, 7, 8, 9, 10, 11, 12]

-- | Four sets to visualize
setsToVisualize :: Array { name :: String, set :: Set Int, color :: String }
setsToVisualize =
  [ { name: "Set A", set: setA, color: "#E74C3C" }
  , { name: "Set B", set: setB, color: "#3498DB" }
  , { name: "A ∪ B (Union)", set: Set.union setA setB, color: "#9B59B6" }
  , { name: "A ∩ B (Intersection)", set: Set.intersection setA setB, color: "#27AE60" }
  ]

-- | Visualize a single set as circles in phylotaxis layout
-- | This is the component we'll MAP over four different sets!
visualizeSet :: String -> Set Int -> String -> T.Tree Unit
visualizeSet setName elements color =
  T.named Group ("set-" <> setName)
    [ class_ "set-visualization" ]
    `T.withChildren`
      ([ -- Container circle
         T.elem Circle
           [ cx 0.0
           , cy 0.0
           , radius 120.0
           , fill "none"
           , stroke "#ddd"
           , strokeWidth 2.0
           , class_ "set-boundary"
           ]
       -- Title
       , T.elem Text
           [ x 0.0
           , y (-135.0)
           , textContent setName
           , textAnchor "middle"
           , fill "#333"
           , class_ "set-title"
           ]
       -- Count label
       , T.elem Text
           [ x 0.0
           , y 145.0
           , textContent ("(" <> show (Set.size elements) <> " elements)")
           , textAnchor "middle"
           , fill "#666"
           , class_ "set-count"
           ]
       ] <>
       -- Elements as small circles in phylotaxis layout
       -- This demonstrates Foldable - we're folding over a Set, not an Array!
       (Set.toUnfoldable elements
         # Array.mapWithIndex (\index value ->
             let pos = phylotaxisPosition index
             in T.elem Circle
                  [ cx pos.x
                  , cy pos.y
                  , radius 6.0
                  , fill color
                  , fillOpacity 0.8
                  , stroke "#fff"
                  , strokeWidth 1.5
                  , class_ "set-element"
                  ]
                  `T.withChild`
                    T.elem Text
                      [ x pos.x
                      , y (pos.y + 1.5)
                      , textContent (show value)
                      , textAnchor "middle"
                      , fill "#fff"
                      , class_ "element-label"
                      ]
           )
       ))

-- | Draw all four set visualizations in a 2x2 grid
-- | This demonstrates MAP: same component, different sets!
drawSetOperations :: String -> Effect Unit
drawSetOperations containerSelector = runD3v2M do
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Create the main SVG with 2x2 grid layout
  let setsTree =
        T.named SVG "svg"
          [ width 1000.0
          , height 700.0
          , viewBox "0 0 1000 700"
          , class_ "set-operations"
          ]
          `T.withChildren`
            -- Map the visualizeSet component over all four sets!
            -- This is the FP win: one definition, four instances
            (Array.mapWithIndex (\index { name, set, color } ->
              let col = index `mod` 2
                  row = index / 2
                  xOffset = toNumber col * 500.0 + 250.0
                  yOffset = toNumber row * 350.0 + 200.0
              in T.named Group ("group-" <> name)
                   [ transform ("translate(" <> show xOffset <> "," <> show yOffset <> ")") ]
                   `T.withChild` visualizeSet name set color
            ) setsToVisualize)

  _ <- renderTree container setsTree

  pure unit

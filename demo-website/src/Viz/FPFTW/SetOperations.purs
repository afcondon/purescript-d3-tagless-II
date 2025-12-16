module D3.Viz.FPFTW.SetOperations where

-- | Set operations visualization using phylotaxis layout
-- | Demonstrates mapping one visualizer across four Set operations

import Prelude

import Data.Array (find)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Interpreter.D3 (D3v2Selection_, runD3v2M)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import PSD3.Expr.Friendly (num, text, attr, width, height, viewBox, cx, cy, r, x, y, fill, stroke, strokeWidth, opacity, transform, textContent, textAnchor)
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

-- | Color names with their hex values for visualization
type ColorName = String
type ColorHex = String

colorMap :: Array { name :: ColorName, hex :: ColorHex }
colorMap =
  [ { name: "yellow", hex: "#FFD700" }
  , { name: "blue", hex: "#4169E1" }
  , { name: "red", hex: "#DC143C" }
  , { name: "orange", hex: "#FF8C00" }
  , { name: "purple", hex: "#9370DB" }
  , { name: "green", hex: "#32CD32" }
  , { name: "brown", hex: "#8B4513" }
  , { name: "black", hex: "#2C2C2C" }
  , { name: "pink", hex: "#FF69B4" }
  ]

-- | Our example sets - using color names to make semantics clear!
setA :: Set String
setA = Set.fromFoldable ["yellow", "blue", "red", "orange", "purple"]

setB :: Set String
setB = Set.fromFoldable ["blue", "green", "brown", "black", "purple"]

-- | Four sets to visualize
setsToVisualize :: Array { name :: String, set :: Set String, borderColor :: String }
setsToVisualize =
  [ { name: "Set A", set: setA, borderColor: "#E74C3C" }
  , { name: "Set B", set: setB, borderColor: "#3498DB" }
  , { name: "A ∪ B (Union)", set: Set.union setA setB, borderColor: "#9B59B6" }
  , { name: "A ∩ B (Intersection)", set: Set.intersection setA setB, borderColor: "#27AE60" }
  ]

-- | Get the hex color for a color name
getColorHex :: String -> String
getColorHex colorName =
  case find (\c -> c.name == colorName) colorMap of
    Just c -> c.hex
    Nothing -> "#999999"  -- Default gray for unknown colors

-- | Visualize a single set as circles in phylotaxis layout
-- | This is the component we'll MAP over four different sets!
visualizeSet :: String -> Set String -> String -> T.Tree Unit
visualizeSet setName elements borderColor =
  T.named Group ("set-" <> setName)
    [ attr "class" $ text "set-visualization" ]
    `T.withChildren`
      ([ -- Container circle
         T.elem Circle
           [ cx $ num 0.0
           , cy $ num 0.0
           , r $ num 120.0
           , fill $ text "none"
           , stroke $ text borderColor
           , strokeWidth $ num 3.0
           , attr "class" $ text "set-boundary"
           ]
       -- Title
       , T.elem Text
           [ x $ num 0.0
           , y $ num (-135.0)
           , textContent $ text setName
           , textAnchor $ text "middle"
           , fill $ text "#333"
           , attr "class" $ text "set-title"
           ]
       -- Count label
       , T.elem Text
           [ x $ num 0.0
           , y $ num 145.0
           , textContent $ text ("(" <> show (Set.size elements) <> " colors)")
           , textAnchor $ text "middle"
           , fill $ text "#666"
           , attr "class" $ text "set-count"
           ]
       ] <>
       -- Elements as colored circles in phylotaxis layout
       -- This demonstrates Foldable - we're folding over a Set, not an Array!
       -- Each circle is colored with its actual color!
       (Set.toUnfoldable elements
         # Array.mapWithIndex (\index colorName ->
             let pos = phylotaxisPosition index
                 colorHex = getColorHex colorName
             in T.elem Circle
                  [ cx $ num pos.x
                  , cy $ num pos.y
                  , r $ num 12.0  -- Larger to show colors better
                  , fill $ text colorHex  -- Use the actual color!
                  , opacity $ num 0.9
                  , stroke $ text "#fff"
                  , strokeWidth $ num 2.0
                  , attr "class" $ text "set-element"
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
          [ width $ num 1000.0
          , height $ num 700.0
          , viewBox 0.0 0.0 1000.0 700.0
          , attr "class" $ text "set-operations"
          ]
          `T.withChildren`
            -- Map the visualizeSet component over all four sets!
            -- This is the FP win: one definition, four instances
            (Array.mapWithIndex (\index { name, set, borderColor } ->
              let col = index `mod` 2
                  row = index / 2
                  xOffset = toNumber col * 500.0 + 250.0
                  yOffset = toNumber row * 350.0 + 200.0
              in T.named Group ("group-" <> name)
                   [ transform $ text ("translate(" <> show xOffset <> "," <> show yOffset <> ")") ]
                   `T.withChild` visualizeSet name set borderColor
            ) setsToVisualize)

  _ <- renderTree container setsTree

  pure unit

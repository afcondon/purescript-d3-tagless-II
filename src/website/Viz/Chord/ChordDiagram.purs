module D3.Viz.ChordDiagram where

import Prelude

import D3.Attributes.Sugar (classed, d, fill, fillOpacity, fontSize, height, strokeColor, strokeWidth, text, textAnchor, transform, viewBox, width)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Selector)
import D3.FFI (arcGenerator_, arcPath_, chordArray_, chordGroups_, chordLayout_, keyIsID_, ribbonGenerator_, ribbonPath_, setArcInnerRadius_, setArcOuterRadius_, setRibbonRadius_)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach, simpleJoin)
import Data.Array (length, (!!), (..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)

-- Sample dependency matrix (relationships between entities)
-- Each row represents dependencies from one entity to others
type DependencyMatrix = Array (Array Number)

-- Labels for the entities in the matrix
type EntityLabels = Array String

-- Example data: dependencies between programming concepts
exampleMatrix :: DependencyMatrix
exampleMatrix = [
  [0.0, 5.0, 3.0, 2.0, 1.0],  -- Data structures depends on...
  [5.0, 0.0, 4.0, 1.0, 2.0],  -- Algorithms depends on...
  [3.0, 4.0, 0.0, 3.0, 4.0],  -- Patterns depends on...
  [2.0, 1.0, 3.0, 0.0, 5.0],  -- Testing depends on...
  [1.0, 2.0, 4.0, 5.0, 0.0]   -- Architecture depends on...
]

exampleLabels :: EntityLabels
exampleLabels = [
  "Data Structures",
  "Algorithms",
  "Patterns",
  "Testing",
  "Architecture"
]

-- Colors for each entity
colors :: Array String
colors = [
  "#e74c3c",  -- red
  "#3498db",  -- blue
  "#2ecc71",  -- green
  "#f39c12",  -- orange
  "#9b59b6"   -- purple
]

-- Accessor helpers for chord data
getSourceIndex :: Datum_ -> Int
getSourceIndex d = (unsafeCoerce d).source.index

-- Snippet_Start
-- Name: ChordDiagramDraw
-- Main drawing function for chord diagram
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  DependencyMatrix -> EntityLabels -> Selector D3Selection_ -> m Unit
draw matrix _ selector = do
  let dims = { width: 800.0, height: 800.0 }
  let outerR = 300.0
  let innerR = 290.0
  let centerX = dims.width / 2.0
  let centerY = dims.height / 2.0

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 dims.width dims.height
    , classed "chord-diagram"
    , width dims.width
    , height dims.height
    ]

  -- Create main group centered in the SVG
  centerGroup <- appendTo svg Group [
      transform [ \_ -> "translate(" <> show centerX <> "," <> show centerY <> ")" ]
    , classed "chord-group"
    ]

  -- Create chord layout
  let chordData = chordLayout_ matrix
  let groups = chordGroups_ chordData
  let chords = chordArray_ chordData

  -- Create generators
  let ribbonGen0 = ribbonGenerator_ unit
  let ribbonGen = setRibbonRadius_ ribbonGen0 innerR
  let arcGen0 = arcGenerator_ unit
  let arcGen1 = setArcInnerRadius_ arcGen0 innerR
  let arcGen = setArcOuterRadius_ arcGen1 outerR

  -- Draw the ribbons (chords)
  ribbonsGroup <- appendTo centerGroup Group [ classed "ribbons" ]

  let drawRibbon :: Datum_ -> m Unit
      drawRibbon chord = do
        let sourceIdx = getSourceIndex chord
        let color = case colors !! sourceIdx of
                      Just c -> c
                      Nothing -> "#999999"
        let pathData = ribbonPath_ ribbonGen chord
        _ <- appendTo ribbonsGroup Path [
            d pathData
          , fill color
          , fillOpacity 0.67
          , strokeColor "#000000"
          , strokeWidth 0.5
          , classed "ribbon"
          ]
        pure unit

  _ <- traverse_ drawRibbon chords

  -- Draw the arcs (groups)
  arcsGroup <- appendTo centerGroup Group [ classed "arcs" ]

  groupsJoin <- simpleJoin arcsGroup Group groups keyIsID_

  let drawArc :: Int -> Datum_ -> m Unit
      drawArc idx group = do
        let color = case colors !! idx of
                      Just c -> c
                      Nothing -> "#999999"
        let pathData = arcPath_ arcGen group
        _ <- appendTo groupsJoin Path [
            d pathData
          , fill color
          , strokeColor "#ffffff"
          , strokeWidth 2.0
          , classed "arc"
          ]
        pure unit

  _ <- traverse_ (\idx -> case groups !! idx of
                           Just g -> drawArc idx g
                           Nothing -> pure unit)
                (0 .. (length groups - 1))

  pure unit
-- Snippet_End

-- | Tree Builder Types
-- |
-- | UI-specific types for the interactive tree builder.
-- | Core types (BuilderTree, AttributeChoice, etc.) are in PSD3v2.Interpreter.SemiQuine.Types
module TreeBuilder.Types
  ( -- * Re-exports from SemiQuine
    module SemiQuineTypes
    -- * Sample Data
  , SampleDatum
  , defaultSampleData
  , sudokuSampleData
  , chessSampleData
  , goSampleData
    -- * Element Types for UI
  , ElementOption
  , availableElements
    -- * Attribute Options
  , AttributeOption
  , availableAttributes
  , attributeOptionsFor
    -- * State
  , BuilderState
  , initialBuilderState
    -- * Utilities
  , generateNodeId
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin) as Math
import Effect (Effect)
import Effect.Random (randomInt)
import PSD3v2.Interpreter.SemiQuine.Types (AttributeBinding, AttributeChoice(..), BuilderNode, BuilderTree(..), NodeId, defaultAttributesFor, emptyNode) as SemiQuineTypes

-- =============================================================================
-- Node ID Generation
-- =============================================================================

-- | Generate a random node ID (simple approach)
generateNodeId :: Effect SemiQuineTypes.NodeId
generateNodeId = randomInt 1000 999999

-- =============================================================================
-- Sample Data Schema
-- =============================================================================

-- | Sample data that users can edit and pipe through visualizations.
-- | Contains common fields needed for most visualization types.
-- | Multiple coordinate systems for different tree structures:
-- |   x, y      - Grid layout (square board)
-- |   cx, cy    - Center coordinates (for text labels)
-- |   rx, ry    - Radial layout (polar arrangement)
-- |   sx, sy    - Strip layout (linear sequence)
type SampleDatum =
  { x :: Number       -- Grid X position
  , y :: Number       -- Grid Y position
  , cx :: Number      -- Center X (x + width/2)
  , cy :: Number      -- Center Y for text baseline (y + height*0.7)
  , rx :: Number      -- Radial X (polar → cartesian)
  , ry :: Number      -- Radial Y (polar → cartesian)
  , sx :: Number      -- Strip X (sequential)
  , sy :: Number      -- Strip Y (constant)
  , radius :: Number  -- Circle radius (for scatter)
  , width :: Number   -- Cell width
  , height :: Number  -- Cell height
  , color :: String   -- Fill color
  , label :: String   -- Text label
  , name :: String    -- Datum name/type
  , value :: Number   -- Numeric value
  , index :: Int      -- Array index
  }

-- | Default sample dataset for new builders
defaultSampleData :: Array SampleDatum
defaultSampleData =
  [ { x: 50.0, y: 50.0, cx: 70.0, cy: 71.0, rx: 50.0, ry: 50.0, sx: 0.0, sy: 100.0, radius: 15.0, width: 40.0, height: 30.0, color: "#1f77b4", label: "Alpha", name: "A", value: 10.0, index: 0 }
  , { x: 150.0, y: 80.0, cx: 175.0, cy: 108.0, rx: 150.0, ry: 80.0, sx: 50.0, sy: 100.0, radius: 20.0, width: 50.0, height: 40.0, color: "#ff7f0e", label: "Beta", name: "B", value: 25.0, index: 1 }
  , { x: 100.0, y: 150.0, cx: 117.5, cy: 167.5, rx: 100.0, ry: 150.0, sx: 100.0, sy: 100.0, radius: 12.0, width: 35.0, height: 25.0, color: "#2ca02c", label: "Gamma", name: "C", value: 15.0, index: 2 }
  , { x: 200.0, y: 120.0, cx: 222.5, cy: 144.5, rx: 200.0, ry: 120.0, sx: 150.0, sy: 100.0, radius: 18.0, width: 45.0, height: 35.0, color: "#d62728", label: "Delta", name: "D", value: 30.0, index: 3 }
  ]

-- | Sudoku-style sample data: full 9x9 grid (81 cells)
-- | Classic sudoku puzzle with given clues and empty cells
-- | Includes grid (x,y), radial (rx,ry), and strip (sx,sy) coordinates
sudokuSampleData :: Array SampleDatum
sudokuSampleData = Array.concat
  [ sudokuRow 0 [5,3,0, 0,7,0, 0,0,0]
  , sudokuRow 1 [6,0,0, 1,9,5, 0,0,0]
  , sudokuRow 2 [0,9,8, 0,0,0, 0,6,0]
  , sudokuRow 3 [8,0,0, 0,6,0, 0,0,3]
  , sudokuRow 4 [4,0,0, 8,0,3, 0,0,1]
  , sudokuRow 5 [7,0,0, 0,2,0, 0,0,6]
  , sudokuRow 6 [0,6,0, 0,0,0, 2,8,0]
  , sudokuRow 7 [0,0,0, 4,1,9, 0,0,5]
  , sudokuRow 8 [0,0,0, 0,8,0, 0,7,9]
  ]
  where
  gridSize = 9
  cellSize = 28.0
  gap = 2.0
  margin = 10.0

  -- Radial layout params
  radialCenter = 150.0
  radialInner = 30.0
  radialOuter = 140.0

  -- Strip layout params
  stripCellWidth = 3.2
  stripHeight = 150.0

  -- Color based on 3x3 box (alternating for visual distinction)
  boxColor :: Int -> Int -> String
  boxColor row col =
    let boxRow = row `div` 3
        boxCol = col `div` 3
        isLightBox = (boxRow + boxCol) `mod` 2 == 0
    in if isLightBox then "#f5f2e8" else "#e8e0cc"

  sudokuRow :: Int -> Array Int -> Array SampleDatum
  sudokuRow row vals = Array.mapWithIndex (mkCell row) vals

  mkCell :: Int -> Int -> Int -> SampleDatum
  mkCell row col val =
    let
      idx = row * gridSize + col
      -- Grid coordinates
      gx = margin + (toNumber col) * (cellSize + gap)
      gy = margin + (toNumber row) * (cellSize + gap)
      -- Center coordinates for text
      centerX = gx + cellSize / 2.0
      centerY = gy + cellSize * 0.7
      -- Radial coordinates (row = ring, col = angle)
      ringRadius = radialInner + (toNumber row) * ((radialOuter - radialInner) / toNumber (gridSize - 1))
      angle = (toNumber col) * (2.0 * 3.14159 / toNumber gridSize) - (3.14159 / 2.0)
      radX = radialCenter + ringRadius * cos angle
      radY = radialCenter + ringRadius * sin angle
      -- Strip coordinates (all cells in a row)
      stripX = margin + (toNumber idx) * stripCellWidth
      stripY = stripHeight / 2.0
    in
      { x: gx
      , y: gy
      , cx: centerX
      , cy: centerY
      , rx: radX
      , ry: radY
      , sx: stripX
      , sy: stripY
      , radius: 10.0  -- For radial circles
      , width: cellSize
      , height: cellSize
      , color: boxColor row col
      , label: if val == 0 then "" else show val
      , name: "cell"
      , value: toNumber val
      , index: idx
      }

  toNumber :: Int -> Number
  toNumber = Int.toNumber

  cos :: Number -> Number
  cos = Math.cos

  sin :: Number -> Number
  sin = Math.sin

-- | Chess sample data: 8x8 board with starting position
-- | Uses Unicode chess symbols: ♔♕♖♗♘♙ (white) ♚♛♜♝♞♟ (black)
-- | Includes grid (x,y), radial (rx,ry), and strip (sx,sy) coordinates
chessSampleData :: Array SampleDatum
chessSampleData = Array.concat
  [ chessRow 0 ["♜","♞","♝","♛","♚","♝","♞","♜"]  -- black back rank
  , chessRow 1 ["♟","♟","♟","♟","♟","♟","♟","♟"]  -- black pawns
  , chessRow 2 ["","","","","","","",""]           -- empty
  , chessRow 3 ["","","","","","","",""]           -- empty
  , chessRow 4 ["","","","","","","",""]           -- empty
  , chessRow 5 ["","","","","","","",""]           -- empty
  , chessRow 6 ["♙","♙","♙","♙","♙","♙","♙","♙"]  -- white pawns
  , chessRow 7 ["♖","♘","♗","♕","♔","♗","♘","♖"]  -- white back rank
  ]
  where
  gridSize = 8
  cellSize = 36.0
  margin = 10.0

  -- Radial layout params
  radialCenter = 150.0
  radialInner = 25.0
  radialOuter = 140.0

  -- Strip layout params
  stripCellWidth = 4.0
  stripHeight = 150.0

  -- Alternating square colors
  squareColor :: Int -> Int -> String
  squareColor row col =
    if (row + col) `mod` 2 == 0
      then "#f0d9b5"  -- light square (cream)
      else "#b58863"  -- dark square (brown)

  chessRow :: Int -> Array String -> Array SampleDatum
  chessRow row pieces = Array.mapWithIndex (mkCell row) pieces

  mkCell :: Int -> Int -> String -> SampleDatum
  mkCell row col piece =
    let
      idx = row * gridSize + col
      -- Grid coordinates
      gx = margin + (Int.toNumber col) * cellSize
      gy = margin + (Int.toNumber row) * cellSize
      -- Center coordinates for text
      centerX = gx + cellSize / 2.0
      centerY = gy + cellSize * 0.75  -- Chess pieces need slightly more offset
      -- Radial coordinates (row = ring, col = angle)
      ringRadius = radialInner + (Int.toNumber row) * ((radialOuter - radialInner) / Int.toNumber (gridSize - 1))
      angle = (Int.toNumber col) * (2.0 * 3.14159 / Int.toNumber gridSize) - (3.14159 / 2.0)
      radX = radialCenter + ringRadius * Math.cos angle
      radY = radialCenter + ringRadius * Math.sin angle
      -- Strip coordinates
      stripX = margin + (Int.toNumber idx) * stripCellWidth
      stripY = stripHeight / 2.0
    in
      { x: gx
      , y: gy
      , cx: centerX
      , cy: centerY
      , rx: radX
      , ry: radY
      , sx: stripX
      , sy: stripY
      , radius: 12.0  -- For radial circles
      , width: cellSize
      , height: cellSize
      , color: squareColor row col
      , label: piece
      , name: if piece == "" then "empty" else "piece"
      , value: Int.toNumber (if piece == "" then 0 else 1)
      , index: idx
      }

-- | Go sample data: 19x19 board with a few stones
-- | Classic game opening - shows the scale of Go vs Chess/Sudoku
-- | Includes grid (x,y), radial (rx,ry), and strip (sx,sy) coordinates
goSampleData :: Array SampleDatum
goSampleData = do
  row <- Array.range 0 18
  col <- Array.range 0 18
  pure (mkCell row col)
  where
  gridSize = 19
  cellSize = 14.0
  margin = 10.0

  -- Radial layout params (tighter for 19x19)
  radialCenter = 150.0
  radialInner = 15.0
  radialOuter = 140.0

  -- Strip layout params
  stripCellWidth = 0.75
  stripHeight = 150.0

  -- Some stones placed in a classic opening pattern
  stones :: Array { r :: Int, c :: Int, stone :: String }
  stones =
    [ { r: 3, c: 3, stone: "●" }    -- black
    , { r: 3, c: 15, stone: "○" }   -- white
    , { r: 15, c: 15, stone: "●" }  -- black
    , { r: 15, c: 3, stone: "○" }   -- white
    , { r: 9, c: 9, stone: "●" }    -- black tengen
    , { r: 3, c: 9, stone: "○" }    -- white
    , { r: 9, c: 3, stone: "●" }    -- black
    , { r: 9, c: 15, stone: "○" }   -- white
    , { r: 15, c: 9, stone: "●" }   -- black
    ]

  getStone :: Int -> Int -> String
  getStone r c = case Array.find (\s -> s.r == r && s.c == c) stones of
    Just s -> s.stone
    Nothing -> ""

  -- Star points (hoshi) get a dot
  isStarPoint :: Int -> Int -> Boolean
  isStarPoint r c = (r == 3 || r == 9 || r == 15) && (c == 3 || c == 9 || c == 15)

  mkCell :: Int -> Int -> SampleDatum
  mkCell row col =
    let
      idx = row * gridSize + col
      stone = getStone row col
      -- Grid coordinates
      gx = margin + (Int.toNumber col) * cellSize
      gy = margin + (Int.toNumber row) * cellSize
      -- Center coordinates for text
      centerX = gx + cellSize / 2.0
      centerY = gy + cellSize * 0.7
      -- Radial coordinates
      ringRadius = radialInner + (Int.toNumber row) * ((radialOuter - radialInner) / Int.toNumber (gridSize - 1))
      angle = (Int.toNumber col) * (2.0 * 3.14159 / Int.toNumber gridSize) - (3.14159 / 2.0)
      radX = radialCenter + ringRadius * Math.cos angle
      radY = radialCenter + ringRadius * Math.sin angle
      -- Strip coordinates
      stripX = margin + (Int.toNumber idx) * stripCellWidth
      stripY = stripHeight / 2.0
      -- Color: board tan, with star point markers
      baseColor = "#DEB887"  -- burlywood/tan
    in
      { x: gx
      , y: gy
      , cx: centerX
      , cy: centerY
      , rx: radX
      , ry: radY
      , sx: stripX
      , sy: stripY
      , radius: 5.0  -- Small for 19x19
      , width: cellSize
      , height: cellSize
      , color: baseColor
      , label: stone
      , name: if stone == "" then (if isStarPoint row col then "star" else "empty") else "stone"
      , value: Int.toNumber (if stone == "●" then 1 else if stone == "○" then 2 else 0)
      , index: idx
      }

-- =============================================================================
-- Element Options for UI
-- =============================================================================

-- | An element option for the palette
type ElementOption =
  { id :: String
  , label :: String
  , category :: String
  , description :: String
  }

-- | Available elements users can add
availableElements :: Array ElementOption
availableElements =
  [ { id: "svg", label: "SVG", category: "Container", description: "Root SVG container" }
  , { id: "group", label: "Group", category: "Container", description: "Group element (g) for nesting" }
  , { id: "circle", label: "Circle", category: "Shape", description: "Circle with cx, cy, r" }
  , { id: "rect", label: "Rect", category: "Shape", description: "Rectangle with x, y, width, height" }
  , { id: "line", label: "Line", category: "Shape", description: "Line from x1,y1 to x2,y2" }
  , { id: "text", label: "Text", category: "Text", description: "Text element with x, y position" }
  ]

-- =============================================================================
-- Attribute Options for UI
-- =============================================================================

-- | An attribute option for the picker
type AttributeOption =
  { name :: String
  , label :: String
  , valueType :: String  -- "number", "string", "color"
  , description :: String
  }

-- | All available attributes
availableAttributes :: Array AttributeOption
availableAttributes =
  -- Position attributes
  [ { name: "cx", label: "Center X", valueType: "number", description: "Circle center X coordinate" }
  , { name: "cy", label: "Center Y", valueType: "number", description: "Circle center Y coordinate" }
  , { name: "x", label: "X", valueType: "number", description: "X coordinate" }
  , { name: "y", label: "Y", valueType: "number", description: "Y coordinate" }
  , { name: "x1", label: "X1", valueType: "number", description: "Line start X" }
  , { name: "y1", label: "Y1", valueType: "number", description: "Line start Y" }
  , { name: "x2", label: "X2", valueType: "number", description: "Line end X" }
  , { name: "y2", label: "Y2", valueType: "number", description: "Line end Y" }
  -- Size attributes
  , { name: "r", label: "Radius", valueType: "number", description: "Circle radius" }
  , { name: "width", label: "Width", valueType: "number", description: "Element width" }
  , { name: "height", label: "Height", valueType: "number", description: "Element height" }
  -- Style attributes
  , { name: "fill", label: "Fill", valueType: "color", description: "Fill color" }
  , { name: "stroke", label: "Stroke", valueType: "color", description: "Stroke color" }
  , { name: "stroke-width", label: "Stroke Width", valueType: "number", description: "Stroke thickness" }
  , { name: "opacity", label: "Opacity", valueType: "number", description: "Transparency (0-1)" }
  -- Text attributes
  , { name: "text", label: "Text Content", valueType: "string", description: "Text to display" }
  , { name: "font-size", label: "Font Size", valueType: "number", description: "Text size in pixels" }
  ]

-- | Get relevant attribute options for an element type
attributeOptionsFor :: String -> Array AttributeOption
attributeOptionsFor = case _ of
  "circle" -> Array.filter (\a -> a.name `Array.elem` ["cx", "cy", "r", "fill", "stroke", "stroke-width", "opacity"]) availableAttributes
  "rect" -> Array.filter (\a -> a.name `Array.elem` ["x", "y", "width", "height", "fill", "stroke", "stroke-width", "opacity"]) availableAttributes
  "line" -> Array.filter (\a -> a.name `Array.elem` ["x1", "y1", "x2", "y2", "stroke", "stroke-width", "opacity"]) availableAttributes
  "text" -> Array.filter (\a -> a.name `Array.elem` ["x", "y", "text", "fill", "font-size", "opacity"]) availableAttributes
  "group" -> Array.filter (\a -> a.name `Array.elem` ["opacity"]) availableAttributes
  "svg" -> Array.filter (\a -> a.name `Array.elem` ["width", "height"]) availableAttributes
  _ -> availableAttributes

-- =============================================================================
-- Builder State
-- =============================================================================

-- | The complete state of the tree builder
type BuilderState =
  { tree :: Maybe SemiQuineTypes.BuilderTree
  , sampleData :: Array SampleDatum
  , selectedNodeId :: Maybe SemiQuineTypes.NodeId
  , nextNodeId :: SemiQuineTypes.NodeId
  , previewError :: Maybe String
  }

-- | Initial state for a new builder session
initialBuilderState :: BuilderState
initialBuilderState =
  { tree: Nothing
  , sampleData: defaultSampleData
  , selectedNodeId: Nothing
  , nextNodeId: 1
  , previewError: Nothing
  }

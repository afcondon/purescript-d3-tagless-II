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
type SampleDatum =
  { x :: Number
  , y :: Number
  , radius :: Number
  , width :: Number
  , height :: Number
  , color :: String
  , label :: String
  , name :: String
  , value :: Number
  , index :: Int
  }

-- | Default sample dataset for new builders
defaultSampleData :: Array SampleDatum
defaultSampleData =
  [ { x: 50.0, y: 50.0, radius: 15.0, width: 40.0, height: 30.0, color: "#1f77b4", label: "Alpha", name: "A", value: 10.0, index: 0 }
  , { x: 150.0, y: 80.0, radius: 20.0, width: 50.0, height: 40.0, color: "#ff7f0e", label: "Beta", name: "B", value: 25.0, index: 1 }
  , { x: 100.0, y: 150.0, radius: 12.0, width: 35.0, height: 25.0, color: "#2ca02c", label: "Gamma", name: "C", value: 15.0, index: 2 }
  , { x: 200.0, y: 120.0, radius: 18.0, width: 45.0, height: 35.0, color: "#d62728", label: "Delta", name: "D", value: 30.0, index: 3 }
  ]

-- | Sudoku-style sample data: full 9x9 grid (81 cells)
-- | Classic sudoku puzzle with given clues and empty cells
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
  cellSize = 28.0
  gap = 2.0
  margin = 10.0

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
    { x: margin + (toNumber col) * (cellSize + gap)
    , y: margin + (toNumber row) * (cellSize + gap)
    , radius: 0.0
    , width: cellSize
    , height: cellSize
    , color: boxColor row col
    , label: if val == 0 then "" else show val
    , name: "cell"
    , value: toNumber val
    , index: row * 9 + col
    }

  toNumber :: Int -> Number
  toNumber = Int.toNumber

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

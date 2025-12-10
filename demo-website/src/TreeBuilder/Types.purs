-- | Tree Builder Types
-- |
-- | Core types for the interactive tree builder. These represent
-- | the editable form of a visualization tree before it's interpreted.
module TreeBuilder.Types
  ( -- * Builder Types
    BuilderTree(..)
  , BuilderNode
  , NodeId
  , AttributeChoice(..)
  , AttributeBinding
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
  , emptyNode
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (randomInt)

-- =============================================================================
-- Node Identification
-- =============================================================================

-- | Unique identifier for nodes in the builder tree
type NodeId = Int

-- | Generate a random node ID (simple approach)
generateNodeId :: Effect NodeId
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

-- | Sudoku-style sample data: 3x3 grid of cells with values
-- | Each datum represents a cell in the grid (scaled up for visibility)
sudokuSampleData :: Array SampleDatum
sudokuSampleData =
  -- Row 0
  [ { x: 20.0, y: 20.0, radius: 0.0, width: 80.0, height: 80.0, color: "#f5f2e8", label: "5", name: "cell", value: 5.0, index: 0 }
  , { x: 100.0, y: 20.0, radius: 0.0, width: 80.0, height: 80.0, color: "#e8e0cc", label: "3", name: "cell", value: 3.0, index: 1 }
  , { x: 180.0, y: 20.0, radius: 0.0, width: 80.0, height: 80.0, color: "#f5f2e8", label: "", name: "cell", value: 0.0, index: 2 }
  -- Row 1
  , { x: 20.0, y: 100.0, radius: 0.0, width: 80.0, height: 80.0, color: "#e8e0cc", label: "6", name: "cell", value: 6.0, index: 3 }
  , { x: 100.0, y: 100.0, radius: 0.0, width: 80.0, height: 80.0, color: "#f5f2e8", label: "", name: "cell", value: 0.0, index: 4 }
  , { x: 180.0, y: 100.0, radius: 0.0, width: 80.0, height: 80.0, color: "#e8e0cc", label: "9", name: "cell", value: 9.0, index: 5 }
  -- Row 2
  , { x: 20.0, y: 180.0, radius: 0.0, width: 80.0, height: 80.0, color: "#f5f2e8", label: "", name: "cell", value: 0.0, index: 6 }
  , { x: 100.0, y: 180.0, radius: 0.0, width: 80.0, height: 80.0, color: "#e8e0cc", label: "8", name: "cell", value: 8.0, index: 7 }
  , { x: 180.0, y: 180.0, radius: 0.0, width: 80.0, height: 80.0, color: "#f5f2e8", label: "1", name: "cell", value: 1.0, index: 8 }
  ]

-- =============================================================================
-- Attribute Binding System
-- =============================================================================

-- | How an attribute value is determined.
-- | This is the key abstraction that avoids runtime PureScript compilation.
data AttributeChoice
  = FromField String          -- ^ Read from datum field (e.g., "x", "color")
  | ConstantNumber Number     -- ^ Fixed number value
  | ConstantString String     -- ^ Fixed string value
  | IndexBased                -- ^ Use the datum's index
  | Computed String           -- ^ Named computation (e.g., "scaled_x", "category_color")

derive instance eqAttributeChoice :: Eq AttributeChoice

instance showAttributeChoice :: Show AttributeChoice where
  show (FromField f) = "_.\"" <> f <> "\""
  show (ConstantNumber n) = show n
  show (ConstantString s) = "\"" <> s <> "\""
  show IndexBased = "_.index"
  show (Computed c) = c

-- | An attribute binding: which attribute + how to get its value
type AttributeBinding =
  { attrName :: String           -- e.g., "cx", "fill", "x"
  , choice :: AttributeChoice    -- how to get the value
  }

-- =============================================================================
-- Builder Tree Structure
-- =============================================================================

-- | A node in the builder tree (editable form)
type BuilderNode =
  { id :: NodeId
  , elementType :: String        -- "circle", "rect", "text", "group", "svg", "line"
  , name :: Maybe String         -- Optional name for selection retrieval
  , attributes :: Array AttributeBinding
  , expanded :: Boolean          -- UI state: is this node expanded in tree view?
  }

-- | The builder tree structure
data BuilderTree
  = BNode BuilderNode (Array BuilderTree)  -- Node with children
  | BDataJoin                               -- Data join point
      { id :: NodeId
      , name :: String
      , elementType :: String
      , template :: BuilderNode            -- Template node for each datum
      , expanded :: Boolean
      }

-- Manual Eq instance (can't derive due to record type alias)
instance eqBuilderTree :: Eq BuilderTree where
  eq (BNode n1 c1) (BNode n2 c2) = n1.id == n2.id && c1 == c2
  eq (BDataJoin j1) (BDataJoin j2) = j1.id == j2.id
  eq _ _ = false

-- | Create an empty node of a given type
emptyNode :: NodeId -> String -> BuilderNode
emptyNode nid elemType =
  { id: nid
  , elementType: elemType
  , name: Nothing
  , attributes: defaultAttributesFor elemType
  , expanded: true
  }

-- | Default attributes based on element type
defaultAttributesFor :: String -> Array AttributeBinding
defaultAttributesFor = case _ of
  "circle" ->
    [ { attrName: "cx", choice: FromField "x" }
    , { attrName: "cy", choice: FromField "y" }
    , { attrName: "r", choice: FromField "radius" }
    , { attrName: "fill", choice: FromField "color" }
    ]
  "rect" ->
    [ { attrName: "x", choice: FromField "x" }
    , { attrName: "y", choice: FromField "y" }
    , { attrName: "width", choice: FromField "width" }
    , { attrName: "height", choice: FromField "height" }
    , { attrName: "fill", choice: FromField "color" }
    ]
  "text" ->
    [ { attrName: "x", choice: FromField "x" }
    , { attrName: "y", choice: FromField "y" }
    , { attrName: "text", choice: FromField "label" }
    , { attrName: "fill", choice: ConstantString "#333" }
    ]
  "line" ->
    [ { attrName: "x1", choice: ConstantNumber 0.0 }
    , { attrName: "y1", choice: ConstantNumber 0.0 }
    , { attrName: "x2", choice: FromField "x" }
    , { attrName: "y2", choice: FromField "y" }
    , { attrName: "stroke", choice: FromField "color" }
    ]
  "group" -> []
  "svg" ->
    [ { attrName: "width", choice: ConstantNumber 400.0 }
    , { attrName: "height", choice: ConstantNumber 300.0 }
    ]
  _ -> []

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
  { tree :: Maybe BuilderTree     -- Current tree being built
  , sampleData :: Array SampleDatum
  , selectedNodeId :: Maybe NodeId
  , nextNodeId :: NodeId          -- For generating IDs
  , previewError :: Maybe String  -- Any error in preview rendering
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

-- =============================================================================
-- Eq helpers for BuilderTree
-- =============================================================================

-- | Compare BuilderNode by ID (simplified equality)
nodeEq :: BuilderNode -> BuilderNode -> Boolean
nodeEq a b = a.id == b.id

-- | Compare AttributeBinding
bindingEq :: AttributeBinding -> AttributeBinding -> Boolean
bindingEq a b = a.attrName == b.attrName && a.choice == b.choice

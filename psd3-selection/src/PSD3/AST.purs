-- | PSD3.AST - Abstract Syntax Tree for Visualization Specifications
-- |
-- | This module provides the declarative API for building visualizations.
-- | An AST node describes WHAT to render, and interpreters decide HOW to render it.
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | import PSD3.AST as A
-- | import PSD3.Interpreter.D3 (render)
-- |
-- | myChart :: A.AST DataPoint
-- | myChart =
-- |   A.named SVG "svg" [width 800.0, height 600.0]
-- |     `A.withChildren`
-- |       [ A.joinData "circles" "circle" myData $ \d ->
-- |           A.elem Circle [cx d.x, cy d.y, r 5.0, fill d.color]
-- |       ]
-- |
-- | main = render "#chart" myChart
-- | ```
-- |
-- | ## Interpreters
-- |
-- | The same AST can be interpreted multiple ways:
-- | - `PSD3.Interpreter.D3` - Renders to DOM via D3.js
-- | - `PSD3.Interpreter.Mermaid` - Generates Mermaid diagram of structure
-- | - `PSD3.Interpreter.English` - Produces English description (debugging)
-- |
-- | ## Key Types
-- |
-- | - `AST datum` - A visualization specification parameterized by datum type
-- | - `ASTNode datum` - A node in the AST (element with attributes, behaviors, children)
-- | - `EnterBehavior`, `UpdateBehavior`, `ExitBehavior` - GUP phase specifications
-- |
-- | ## Smart Constructors
-- |
-- | - `named` - Create a named element (can be retrieved after rendering)
-- | - `elem` - Create an anonymous element
-- | - `withChild`, `withChildren` - Add children to a node
-- | - `withBehaviors` - Attach zoom, drag, click handlers
-- |
-- | ## Data Joins
-- |
-- | - `joinData` - Simple data join (datum type stays same)
-- | - `nestedJoin` - Data join with type decomposition
-- | - `sceneJoin` - Data join with enter/update/exit behaviors
-- | - `sceneNestedJoin` - Type decomposition + GUP (recommended for most cases)
-- |
module PSD3.AST
  ( -- * Core Types
    AST
  , ASTNode
    -- * Element Types (Circle, Rect, SVG, etc.)
  , module ElementTypes
    -- * Re-exports from VizTree.Tree (all functions and type aliases)
  , module VizTree
  ) where

import PSD3v2.VizTree.Tree (EnterBehavior, ExitBehavior, Tree(..), TreeNode, UpdateBehavior, beside, elem, joinData, named, nestedJoin, sceneJoin, sceneNestedJoin, siblings, withBehaviors, withChild, withChildren, (+:), (>:)) as VizTree
import PSD3v2.Selection.Types (ElementType(..)) as ElementTypes

-- | AST - Abstract Syntax Tree for visualization specifications
-- |
-- | An AST describes the structure of a visualization declaratively.
-- | It can be interpreted by multiple backends (D3 DOM, Mermaid, English, etc.)
type AST datum = VizTree.Tree datum

-- | ASTNode - A node in the visualization AST
-- |
-- | Contains:
-- | - `name`: Optional identifier for later retrieval
-- | - `elemType`: Element type (SVG, Circle, Rect, etc.)
-- | - `attrs`: Attributes to apply
-- | - `behaviors`: Attached behaviors (zoom, drag, etc.)
-- | - `children`: Child nodes
type ASTNode datum = VizTree.TreeNode datum

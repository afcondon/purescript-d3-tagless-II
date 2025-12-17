-- | PSD3.AST - Abstract Syntax Tree for Visualization Specifications
-- |
-- | This module provides the declarative API for building visualizations.
-- | An AST node describes WHAT to render, and interpreters decide HOW to render it.
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | import PSD3.AST as A
-- | import PSD3.Render (runD3, select, renderTree)
-- |
-- | myChart :: A.AST DataPoint
-- | myChart =
-- |   A.named SVG "svg" [width 800.0, height 600.0]
-- |     `A.withChildren`
-- |       [ A.joinData "circles" "circle" myData $ \d ->
-- |           A.elem Circle [cx d.x, cy d.y, radius 5.0, fill d.color]
-- |       ]
-- |
-- | main = void $ runD3 do
-- |   container <- select "#chart"
-- |   renderTree container myChart
-- | ```
-- |
-- | ## Interpreters
-- |
-- | The same AST can be interpreted multiple ways:
-- | - `PSD3.Render` - Renders to DOM via D3.js
-- | - `PSD3.Interpreter.Mermaid` - Generates Mermaid diagram of structure
-- | - `PSD3.Interpreter.English` - Produces English description (debugging)
-- |
-- | ## Key Types
-- |
-- | - `AST datum` / `Tree datum` - A visualization specification parameterized by datum type
-- | - `ASTNode datum` / `TreeNode datum` - A node in the AST
-- | - `PhaseBehavior`, `GUPBehaviors` - GUP phase specifications
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
-- | - `updateJoin` - Data join with enter/update/exit behaviors
-- | - `updateNestedJoin` - Type decomposition + GUP (recommended for most cases)
-- |
module PSD3.AST
  ( -- * Core Types
    AST
  , ASTNode
  , Tree(..)
  , TreeNode
  , PhaseBehavior
  , GUPBehaviors
    -- * Element Types (Circle, Rect, SVG, etc.)
  , module ElementTypes
    -- * Smart Constructors
  , named
  , elem
  , withChild
  , withChildren
  , withBehaviors
    -- * Data Joins
  , joinData
  , nestedJoin
  , updateJoin
  , updateNestedJoin
    -- * Combinators
  , beside
  , siblings
    -- * Operators
  , (>:)
  , (+:)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import PSD3.Internal.Attribute (Attribute)
import PSD3.Internal.Behavior.Types (Behavior)
import PSD3.Internal.Selection.Types (ElementType(..)) as ElementTypes
import PSD3.Internal.Selection.Types (ElementType)
import PSD3.Internal.Transition.Types (TransitionConfig)
import Unsafe.Coerce (unsafeCoerce)

-- | AST - Abstract Syntax Tree for visualization specifications
-- |
-- | An AST describes the structure of a visualization declaratively.
-- | It can be interpreted by multiple backends (D3 DOM, Mermaid, English, etc.)
-- |
-- | `AST` and `Tree` are synonyms - use whichever reads better in your code.
type AST datum = Tree datum

-- | ASTNode - A node in the visualization AST
-- |
-- | Contains:
-- | - `name`: Optional identifier for later retrieval
-- | - `elemType`: Element type (SVG, Circle, Rect, etc.)
-- | - `attrs`: Attributes to apply
-- | - `behaviors`: Attached behaviors (zoom, drag, etc.)
-- | - `children`: Child nodes
-- |
-- | `ASTNode` and `TreeNode` are synonyms.
type ASTNode datum = TreeNode datum

-- | A node in the visualization tree
-- | - name: Optional name to retrieve this selection later (Nothing = anonymous)
-- | - elemType: What kind of element (SVG, Group, Circle, etc.)
-- | - attrs: Attributes to set on this element
-- | - behaviors: Behaviors to attach (zoom, drag, click handlers, etc.)
-- | - children: Child nodes
type TreeNode datum =
  { name :: Maybe String
  , elemType :: ElementType
  , attrs :: Array (Attribute datum)
  , behaviors :: Array (Behavior datum)
  , children :: Array (Tree datum)
  }

-- | A tree is either a regular node or a data join point
data Tree datum
  = Node (TreeNode datum)
  -- | DataJoin creates N copies of a template tree, one per data item
  -- | This is how we handle enter/update/exit with the declarative API
  -- |
  -- | The join itself is a named node in the tree - it represents the COLLECTION
  -- | of elements created from the data.
  | Join
      { name :: String              -- Name for this join (becomes a selection you can access)
      , key :: String               -- Join key (e.g., "circle", "g")
      , joinData :: Array datum     -- Data to join
      , template :: datum -> Tree datum  -- Template builder - given datum, builds subtree
      }
  -- | NestedJoin allows datum type to change during decomposition
  -- | Used for nested data structures like 2D arrays, arrays of records with arrays, etc.
  -- |
  -- | Uses unsafeCoerce internally to handle type changing.
  -- | This enables patterns like: Array (Array a) → table rows → table cells
  -- |
  -- | SAFETY: The decompose and template functions are provided together by nestedJoin,
  -- | ensuring type safety at the call site. Internally we erase the inner type.
  | NestedJoin
      { name :: String                     -- Name for this join
      , key :: String                      -- Element type to create (e.g., "tr")
      , joinData :: Array datum            -- Outer data (e.g., rows)
      , decompose :: datum -> Array datum   -- Decomposer (type-erased)
      , template :: datum -> Tree datum     -- Template (type-erased)
      }
  -- | UpdateJoin handles General Update Pattern (GUP) with enter/update/exit
  -- |
  -- | This variant declaratively specifies behavior for all three phases:
  -- | - Enter: New data items appearing (with optional transition)
  -- | - Update: Existing data items that remain (with optional transition)
  -- | - Exit: Old data items being removed (with optional transition)
  -- |
  -- | The interpreter handles all the complexity of:
  -- | - Computing the join (determining enter/update/exit sets)
  -- | - Applying the correct template and attributes to each set
  -- | - Running transitions
  -- | - Removing exited elements
  | UpdateJoin
      { name :: String                         -- Name for this join
      , key :: String                          -- Element type (e.g., "circle", "g")
      , joinData :: Array datum                -- Data to join
      , template :: datum -> Tree datum        -- Base template (used for all phases)
      , keyFn :: Maybe (datum -> String)       -- Optional key function for identity matching
      , behaviors :: GUPBehaviors datum        -- Enter/update/exit behaviors
      }
  -- | UpdateNestedJoin combines NestedJoin's type decomposition with UpdateJoin's GUP behaviors
  -- |
  -- | This is the IDEAL variant for most use cases - it allows:
  -- | 1. Type transitions (e.g., SceneData -> Array DataPoint)
  -- | 2. Full GUP with enter/update/exit behaviors
  -- |
  -- | Example: Container holds scene data, decompose extracts array of items,
  -- | each item gets enter/update/exit transitions.
  -- |
  -- | Uses unsafeCoerce internally to handle type changing (same as NestedJoin).
  | UpdateNestedJoin
      { name :: String                              -- Name for this join
      , key :: String                               -- Element type (e.g., "circle", "g")
      , joinData :: Array datum                     -- Outer data (e.g., scene data)
      , decompose :: datum -> Array datum           -- Decomposer: extract inner collection (type-erased)
      , template :: datum -> Tree datum             -- Template for inner elements (type-erased)
      , behaviors :: GUPBehaviors datum             -- Enter/update/exit behaviors (type-erased)
      }

-- | Unified behavior specification for any GUP phase (enter, update, or exit)
-- |
-- | Each phase can specify:
-- | - `attrs`: Attributes to apply for this phase
-- | - `transition`: Optional animation configuration
-- |
-- | For enter: attrs are initial state before animating to template attrs
-- | For update: attrs override template attrs, then animate
-- | For exit: attrs applied before animating out and removing
type PhaseBehavior datum =
  { attrs :: Array (Attribute datum)         -- Attributes for this phase
  , transition :: Maybe TransitionConfig      -- Optional animation
  }

-- | Complete GUP (General Update Pattern) behavior specification
-- |
-- | Bundles enter/update/exit behaviors together. Each phase is optional -
-- | if Nothing, the phase uses default behavior (no special attrs, no animation).
type GUPBehaviors datum =
  { enter :: Maybe (PhaseBehavior datum)     -- New elements appearing
  , update :: Maybe (PhaseBehavior datum)    -- Existing elements with changed data
  , exit :: Maybe (PhaseBehavior datum)      -- Elements being removed
  }

-- | Smart constructors

-- | Create a named element
-- |
-- | Usage: named SVG "svg" [width 800, height 600]
named :: forall datum. ElementType -> String -> Array (Attribute datum) -> Tree datum
named elemType name attrs =
  Node { name: Just name, elemType, attrs, behaviors: [], children: [] }

-- | Create an anonymous element (won't be in the returned selections)
-- |
-- | Usage: elem Group [class_ "container"]
elem :: forall datum. ElementType -> Array (Attribute datum) -> Tree datum
elem elemType attrs =
  Node { name: Nothing, elemType, attrs, behaviors: [], children: [] }

-- | Add a single child to a tree node
-- |
-- | Usage: parent `withChild` child
withChild :: forall datum. Tree datum -> Tree datum -> Tree datum
withChild parent child = case parent of
  Node node -> Node node { children = node.children <> [child] }
  Join j -> Join j  -- Joins can't have additional children (template already defined)
  NestedJoin nj -> NestedJoin nj  -- Nested joins can't have additional children
  UpdateJoin sj -> UpdateJoin sj  -- Scene joins can't have additional children (template already defined)
  UpdateNestedJoin snj -> UpdateNestedJoin snj  -- Scene nested joins can't have additional children

-- | Add multiple children to a tree node
-- |
-- | Usage: parent `withChildren` [child1, child2, child3]
withChildren :: forall datum. Tree datum -> Array (Tree datum) -> Tree datum
withChildren parent newChildren = case parent of
  Node node -> Node node { children = node.children <> newChildren }
  Join j -> Join j
  NestedJoin nj -> NestedJoin nj
  UpdateJoin sj -> UpdateJoin sj
  UpdateNestedJoin snj -> UpdateNestedJoin snj

-- | Add behaviors to a tree node
-- |
-- | Behaviors are attached to the element after creation.
-- | This enables declarative specification of zoom, drag, click handlers, etc.
-- |
-- | Usage:
-- | ```purescript
-- | named SVG "svg" [width 800, height 600]
-- |   `withBehaviors` [Zoom $ defaultZoom (ScaleExtent 0.1 10.0) ".zoom-group"]
-- |   `withChildren` [...]
-- | ```
-- |
-- | Multiple behaviors can be attached:
-- | ```purescript
-- | elem Circle [radius 5.0]
-- |   `withBehaviors` [Drag SimpleDrag, onClickWithDatum \d -> log d.name]
-- | ```
withBehaviors :: forall datum. Tree datum -> Array (Behavior datum) -> Tree datum
withBehaviors tree newBehaviors = case tree of
  Node node -> Node node { behaviors = node.behaviors <> newBehaviors }
  Join j -> Join j  -- Joins apply behaviors via template
  NestedJoin nj -> NestedJoin nj
  UpdateJoin sj -> UpdateJoin sj
  UpdateNestedJoin snj -> UpdateNestedJoin snj

-- | Create a named data join
-- |
-- | The join itself becomes a named selection representing the COLLECTION
-- | of elements created from the data.
-- |
-- | Usage:
-- | ```purescript
-- | joinData "nodes" "g" nodeData $ \node ->
-- |   elem Group [transform (translate node)] `withChildren`
-- |     [ elem Circle [radius node.r]
-- |     , elem Text [textContent node.name]
-- |     ]
-- | ```
-- |
-- | Later you can access the collection:
-- | ```purescript
-- | case Map.lookup "nodes" selections of
-- |   Just nodeGroups -> addTickFunction "nodes" $ Step nodeGroups [...]
-- | ```
joinData :: forall datum. String -> String -> Array datum -> (datum -> Tree datum) -> Tree datum
joinData name key data' templateBuilder =
  Join { name, key, joinData: data', template: templateBuilder }

-- | Create a nested data join with decomposition
-- |
-- | This allows the datum type to change at each level of nesting.
-- | The decomposer extracts inner collections from outer data items.
-- |
-- | Usage:
-- | ```purescript
-- | -- 2D array → table
-- | nestedJoin "rows" "tr" matrixData identity $ \rowData ->
-- |   nestedJoin "cells" "td" [rowData] identity $ \cellValue ->
-- |     elem Td [textContent (show cellValue)]
-- | ```
-- |
-- | Or more commonly:
-- | ```purescript
-- | -- Array of records with nested arrays
-- | nestedJoin "groups" "g" groups (_.items) $ \item ->
-- |   elem Circle [cx item.x, cy item.y]
-- | ```
nestedJoin
  :: forall outerDatum innerDatum
   . String                                  -- Name for this join
  -> String                                  -- Element type (e.g., "tr", "g")
  -> Array outerDatum                        -- Outer data
  -> (outerDatum -> Array innerDatum)        -- Decomposer: extract inner collection
  -> (innerDatum -> Tree innerDatum)         -- Template for inner elements
  -> Tree outerDatum
nestedJoin name key data' decomposeFn templateFn =
  NestedJoin
    { name
    , key
    , joinData: data'
    -- Use unsafeCoerce to erase the inner type
    -- SAFETY: decomposeFn and templateFn are provided together, so they agree on innerDatum
    , decompose: unsafeCoerce decomposeFn
    , template: unsafeCoerce templateFn
    }

-- | Create an update join with General Update Pattern behavior
-- |
-- | This is the declarative way to specify enter/update/exit behavior.
-- | The interpreter handles all the complexity of computing joins and applying transitions.
-- |
-- | Usage:
-- | ```purescript
-- | updateJoin "nodes" "circle" nodeData
-- |   (\node -> elem Circle [ cx node.x, cy node.y, radius 5.0 ])
-- |   { enter: Just { attrs: [ y 0.0, opacity 0.0 ], transition: Just slideDown }
-- |   , update: Just { attrs: [], transition: Just moveToPosition }
-- |   , exit: Just { attrs: [ class_ "exit" ], transition: Just fadeOut }
-- |   , keyFn: Just _.id  -- Optional key function for identity matching
-- |   }
-- | ```
updateJoin
  :: forall datum
   . String                                      -- Name for this join
  -> String                                      -- Element type (e.g., "circle", "g")
  -> Array datum                                 -- Data to join
  -> (datum -> Tree datum)                       -- Template builder
  -> { enter :: Maybe (PhaseBehavior datum)
     , update :: Maybe (PhaseBehavior datum)
     , exit :: Maybe (PhaseBehavior datum)
     , keyFn :: Maybe (datum -> String)
     }
  -> Tree datum
updateJoin name key data' template behaviors =
  UpdateJoin
    { name
    , key
    , joinData: data'
    , template
    , keyFn: behaviors.keyFn
    , behaviors: { enter: behaviors.enter, update: behaviors.update, exit: behaviors.exit }
    }

-- | Create an update nested join with type decomposition and GUP behaviors
-- |
-- | This is the RECOMMENDED way to use UpdateJoin - it combines type decomposition
-- | with enter/update/exit behaviors, solving the type mixing problem.
-- |
-- | Usage:
-- | ```purescript
-- | -- Container has SceneData, decompose to DataPoints, each gets GUP
-- | updateNestedJoin "circles" "circle"
-- |   [sceneData]              -- Outer data (SceneData)
-- |   (_.points)               -- Decompose: SceneData -> Array DataPoint
-- |   (\point -> elem Circle   -- Template for each DataPoint
-- |     [ cx point.x
-- |     , cy point.y
-- |     ])
-- |   { enter: Just { attrs: [radius 0.0], transition: Just fadeIn }
-- |   , update: Just { attrs: [], transition: Just move }
-- |   , exit: Just { attrs: [], transition: Just fadeOut }
-- |   }
-- | ```
updateNestedJoin
  :: forall outerDatum innerDatum
   . String                                           -- Name for this join
  -> String                                           -- Element type (e.g., "circle")
  -> Array outerDatum                                 -- Outer data (e.g., scene data)
  -> (outerDatum -> Array innerDatum)                 -- Decomposer: extract inner collection
  -> (innerDatum -> Tree innerDatum)                  -- Template for inner elements
  -> GUPBehaviors innerDatum                          -- Enter/update/exit behaviors
  -> Tree outerDatum
updateNestedJoin name key data' decomposeFn templateFn behaviors =
  UpdateNestedJoin
    { name
    , key
    , joinData: data'
    -- Use unsafeCoerce to erase the inner type (same safety as NestedJoin)
    -- SAFETY: decomposeFn, templateFn, and behaviors are provided together,
    -- so they agree on innerDatum type
    , decompose: unsafeCoerce decomposeFn
    , template: unsafeCoerce templateFn
    , behaviors: unsafeCoerce behaviors
    }

-- | Operators for Emmet-style syntax

infixl 6 withChild as >:
infixl 5 beside as +:

-- | Combine two trees as siblings
-- | Returns an array, meant to be used with `withChildren`
-- |
-- | Usage: group `withChildren` (circle +: text)
beside :: forall datum. Tree datum -> Tree datum -> Array (Tree datum)
beside left right = [left, right]

-- | Helper to combine multiple siblings
-- |
-- | Usage: siblings [child1, child2, child3]
siblings :: forall datum. Array (Tree datum) -> Array (Tree datum)
siblings = identity

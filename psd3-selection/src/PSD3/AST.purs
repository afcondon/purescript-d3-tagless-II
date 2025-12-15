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
-- | - `PSD3v2.Interpreter.MermaidTree` - Generates Mermaid diagram of structure
-- | - `PSD3v2.Interpreter.English` - Produces English description (debugging)
-- |
-- | ## Key Types
-- |
-- | - `AST datum` / `Tree datum` - A visualization specification parameterized by datum type
-- | - `ASTNode datum` / `TreeNode datum` - A node in the AST
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
  , Tree(..)
  , TreeNode
  , EnterBehavior
  , UpdateBehavior
  , ExitBehavior
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
  , sceneJoin
  , sceneNestedJoin
    -- * Combinators
  , beside
  , siblings
    -- * Operators
  , (>:)
  , (+:)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import PSD3v2.Attribute.Types (Attribute)
import PSD3v2.Behavior.Types (Behavior)
import PSD3v2.Selection.Types (ElementType(..)) as ElementTypes
import PSD3v2.Selection.Types (ElementType)
import PSD3v2.Transition.Types (TransitionConfig)
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
  -- | SceneJoin handles General Update Pattern (GUP) with enter/update/exit
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
  | SceneJoin
      { name :: String                         -- Name for this join
      , key :: String                          -- Element type (e.g., "circle", "g")
      , joinData :: Array datum                -- Data to join
      , template :: datum -> Tree datum        -- Base template (used for all phases)
      , keyFn :: Maybe (datum -> String)       -- Optional key function for identity matching
      , enterBehavior :: Maybe (EnterBehavior datum)
      , updateBehavior :: Maybe (UpdateBehavior datum)
      , exitBehavior :: Maybe (ExitBehavior datum)
      }
  -- | SceneNestedJoin combines NestedJoin's type decomposition with SceneJoin's GUP behaviors
  -- |
  -- | This is the IDEAL variant for most use cases - it allows:
  -- | 1. Type transitions (e.g., SceneData -> Array DataPoint)
  -- | 2. Full GUP with enter/update/exit behaviors
  -- |
  -- | Example: Container holds scene data, decompose extracts array of items,
  -- | each item gets enter/update/exit transitions.
  -- |
  -- | Uses unsafeCoerce internally to handle type changing (same as NestedJoin).
  | SceneNestedJoin
      { name :: String                              -- Name for this join
      , key :: String                               -- Element type (e.g., "circle", "g")
      , joinData :: Array datum                     -- Outer data (e.g., scene data)
      , decompose :: datum -> Array datum           -- Decomposer: extract inner collection (type-erased)
      , template :: datum -> Tree datum             -- Template for inner elements (type-erased)
      , enterBehavior :: Maybe (EnterBehavior datum)   -- Enter behavior (type-erased)
      , updateBehavior :: Maybe (UpdateBehavior datum) -- Update behavior (type-erased)
      , exitBehavior :: Maybe (ExitBehavior datum)     -- Exit behavior (type-erased)
      }

-- | Behavior specification for entering elements
-- |
-- | Entering elements are new data items that don't have existing DOM elements.
-- | They need to be created and optionally animated in.
type EnterBehavior datum =
  { initialAttrs :: Array (Attribute datum)  -- Initial state before transition (e.g., y 0.0, opacity 0.0)
  , transition :: Maybe TransitionConfig      -- How to animate to final state
  }

-- | Behavior specification for updating elements
-- |
-- | Updating elements are existing DOM elements whose data has changed.
-- | They typically transition to new positions/styles.
type UpdateBehavior datum =
  { attrs :: Array (Attribute datum)         -- Attributes to set immediately
  , transition :: Maybe TransitionConfig      -- How to transition to new state
  }

-- | Behavior specification for exiting elements
-- |
-- | Exiting elements are old DOM elements whose data is no longer present.
-- | They should be animated out then removed.
type ExitBehavior datum =
  { attrs :: Array (Attribute datum)         -- Attributes to set before transition (e.g., class "exit")
  , transition :: Maybe TransitionConfig      -- How to animate out (element removed after)
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
  SceneJoin sj -> SceneJoin sj  -- Scene joins can't have additional children (template already defined)
  SceneNestedJoin snj -> SceneNestedJoin snj  -- Scene nested joins can't have additional children

-- | Add multiple children to a tree node
-- |
-- | Usage: parent `withChildren` [child1, child2, child3]
withChildren :: forall datum. Tree datum -> Array (Tree datum) -> Tree datum
withChildren parent newChildren = case parent of
  Node node -> Node node { children = node.children <> newChildren }
  Join j -> Join j
  NestedJoin nj -> NestedJoin nj
  SceneJoin sj -> SceneJoin sj
  SceneNestedJoin snj -> SceneNestedJoin snj

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
  SceneJoin sj -> SceneJoin sj
  SceneNestedJoin snj -> SceneNestedJoin snj

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

-- | Create a scene join with General Update Pattern behavior
-- |
-- | This is the declarative way to specify enter/update/exit behavior.
-- | The interpreter handles all the complexity of computing joins and applying transitions.
-- |
-- | Usage:
-- | ```purescript
-- | sceneJoin "nodes" "circle" nodeData
-- |   (\node -> elem Circle [ cx node.x, cy node.y, radius 5.0 ])
-- |   { enterBehavior: Just
-- |       { initialAttrs: [ y 0.0, opacity 0.0 ]
-- |       , transition: Just slideDown
-- |       }
-- |   , updateBehavior: Just
-- |       { attrs: []
-- |       , transition: Just moveToPosition
-- |       }
-- |   , exitBehavior: Just
-- |       { attrs: [ class_ "exit" ]
-- |       , transition: Just fadeOut
-- |       }
-- |   }
-- | ```
sceneJoin
  :: forall datum
   . String                                      -- Name for this join
  -> String                                      -- Element type (e.g., "circle", "g")
  -> Array datum                                 -- Data to join
  -> (datum -> Tree datum)                       -- Template builder
  -> { enterBehavior :: Maybe (EnterBehavior datum)
     , updateBehavior :: Maybe (UpdateBehavior datum)
     , exitBehavior :: Maybe (ExitBehavior datum)
     , keyFn :: Maybe (datum -> String)          -- Optional: identity function for matching
     }
  -> Tree datum
sceneJoin name key data' template behaviors =
  SceneJoin
    { name
    , key
    , joinData: data'
    , template
    , keyFn: behaviors.keyFn
    , enterBehavior: behaviors.enterBehavior
    , updateBehavior: behaviors.updateBehavior
    , exitBehavior: behaviors.exitBehavior
    }

-- | Create a scene nested join with type decomposition and GUP behaviors
-- |
-- | This is the RECOMMENDED way to use SceneJoin - it combines type decomposition
-- | with enter/update/exit behaviors, solving the type mixing problem.
-- |
-- | Usage:
-- | ```purescript
-- | -- Container has SceneData, decompose to DataPoints, each gets GUP
-- | sceneNestedJoin "circles" "circle"
-- |   [sceneData]              -- Outer data (SceneData)
-- |   (_.points)               -- Decompose: SceneData -> Array DataPoint
-- |   (\point -> elem Circle   -- Template for each DataPoint
-- |     [ cx point.x
-- |     , cy point.y
-- |     ])
-- |   { enterBehavior: Just { initialAttrs: [radius 0.0], transition: Just fadeIn }
-- |   , updateBehavior: Just { attrs: [], transition: Just move }
-- |   , exitBehavior: Just { attrs: [], transition: Just fadeOut }
-- |   }
-- | ```
sceneNestedJoin
  :: forall outerDatum innerDatum
   . String                                           -- Name for this join
  -> String                                           -- Element type (e.g., "circle")
  -> Array outerDatum                                 -- Outer data (e.g., scene data)
  -> (outerDatum -> Array innerDatum)                 -- Decomposer: extract inner collection
  -> (innerDatum -> Tree innerDatum)                  -- Template for inner elements
  -> { enterBehavior :: Maybe (EnterBehavior innerDatum)
     , updateBehavior :: Maybe (UpdateBehavior innerDatum)
     , exitBehavior :: Maybe (ExitBehavior innerDatum)
     }
  -> Tree outerDatum
sceneNestedJoin name key data' decomposeFn templateFn behaviors =
  SceneNestedJoin
    { name
    , key
    , joinData: data'
    -- Use unsafeCoerce to erase the inner type (same safety as NestedJoin)
    -- SAFETY: decomposeFn, templateFn, and behaviors are provided together,
    -- so they agree on innerDatum type
    , decompose: unsafeCoerce decomposeFn
    , template: unsafeCoerce templateFn
    , enterBehavior: unsafeCoerce behaviors.enterBehavior
    , updateBehavior: unsafeCoerce behaviors.updateBehavior
    , exitBehavior: unsafeCoerce behaviors.exitBehavior
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

-- | PSD3: PureScript D3 - Type-safe, composable data visualization
-- |
-- | PSD3 is a PureScript library for creating D3.js visualizations using phantom types
-- | for compile-time safety and a declarative Tree API for clean, maintainable code.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import PSD3v2.Interpreter.D3v2 as D3v2
-- | import PSD3v2.Capabilities.Selection (select, renderTree)
-- | import PSD3v2.VizTree.Tree as T
-- | import PSD3v2.Attribute.Types (width, height, cx, cy, radius, fill)
-- |
-- | main :: Effect Unit
-- | main = void $ D3v2.runD3v2M unit do
-- |   container <- select "body"
-- |   let tree =
-- |         T.elem SVG [width 800.0, height 600.0]
-- |           `T.withChild`
-- |             T.elem Circle [cx 100.0, cy 100.0, radius 50.0, fill "steelblue"]
-- |   renderTree container tree
-- | ```
-- |
-- | ## Tree API Example
-- |
-- | Declarative DOM structure with data binding:
-- |
-- | ```purescript
-- | import PSD3v2.VizTree.Tree as T
-- |
-- | barChart :: Array Number -> T.Tree Number
-- | barChart data =
-- |   T.elem SVG [width 500.0, height 300.0]
-- |     `T.withChild`
-- |       T.joinData "bars" "rect" data \d ->
-- |         T.elem Rect
-- |           [ x (\_ i -> toNumber i * 25.0)
-- |           , y (\val _ -> 300.0 - val)
-- |           , width 20.0
-- |           , height (\val _ -> val)
-- |           , fill "steelblue"
-- |           ]
-- | ```
-- |
-- | ## Force Simulation
-- |
-- | For force-directed graphs, use the `psd3-simulation` package which provides:
-- | - `PSD3.ForceEngine` - Pure PureScript force engine
-- | - `PSD3.Config.Force` - Force configuration DSL
-- | - `PSD3.Config.Scene` - Scene configuration
-- |
-- | ## Module Organization
-- |
-- | **Core Modules** (PSD3v2):
-- | - `PSD3v2.Interpreter.D3v2` - Main interpreter with phantom types
-- | - `PSD3v2.Capabilities.Selection` - Selection operations + Tree API
-- | - `PSD3v2.Capabilities.Transition` - Smooth transitions
-- | - `PSD3v2.VizTree.Tree` - Declarative Tree API
-- | - `PSD3v2.Attribute.Types` - All attributes (width, height, cx, cy, etc.)
-- | - `PSD3v2.Behavior.Types` - Behaviors (drag, zoom)
-- |
-- | **Selection System**:
-- | - `PSD3v2.Selection.Types` - Phantom type states (SUnbound, SBound, SJoined)
-- | - `PSD3v2.Selection.Operations` - Core operations
-- | - `PSD3v2.Selection.Join` - Data join implementation (GUP)
-- |
-- | **Shared Modules**:
-- | - `PSD3.Data.Node` - SimulationNode, D3Link types
-- | - `PSD3.Data.Tree` - Tree data structures
-- | - `PSD3.Layout.Hierarchy.*` - Pure PureScript layouts (Tree, Cluster, Pack, etc.)
-- | - `PSD3.Layout.Sankey` - Pure PureScript Sankey layout
-- | - `PSD3.Internal.FFI` - D3.js FFI bindings
-- |
-- | ## Working Examples
-- |
-- | See these for complete, working demonstrations:
-- |
-- | 1. **CodeExplorerV3** (`src/website/Component/CodeExplorerV3.purs`)
-- |    - Full-featured force simulation with multiple scenes
-- |    - Uses SimulationManager + SceneConfigs (Effect-based)
-- |    - Scene transitions with different force configurations
-- |
-- | 2. **TreeAPI Examples** (`src/website/Component/TreeAPI.purs`)
-- |    - Tree API patterns
-- |    - Declarative structure examples
-- |
-- | ## Import Patterns
-- |
-- | **Basic visualization**:
-- | ```purescript
-- | import PSD3v2.Interpreter.D3v2 as D3v2
-- | import PSD3v2.Capabilities.Selection (select, renderTree)
-- | import PSD3v2.VizTree.Tree as T
-- | import PSD3v2.Attribute.Types (width, height, cx, cy, radius, fill, stroke)
-- | ```
-- |
-- | **With behaviors**:
-- | ```purescript
-- | import PSD3v2.Capabilities.Selection (on)
-- | import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, defaultZoom, simulationDrag)
-- | ```
-- |
-- | ## Exports
-- |
-- | This module re-exports the core PSD3 modules for convenient imports.
-- |
module PSD3 (module X) where

import Prelude as X -- TODO don't include Prelude, let each module include it with selective includes, prevents clashes over append and so on

-- PSD3v2 Core Exports
import PSD3v2.Interpreter.D3v2 (D3v2M, D3v2Selection_, runD3v2M, reselectD3v2) as X

import PSD3v2.Capabilities.Selection (class SelectionM, select, appendChildInheriting, appendData, renderTree, renderData, setAttrs, on, clear) as X
import PSD3v2.Capabilities.Transition (class TransitionM, withTransition, withTransitionExit) as X

-- Tree API
import PSD3v2.VizTree.Tree (Tree, elem, joinData, withChild, withChildren, named) as X

-- Types
import PSD3v2.Selection.Types (SEmpty, SBoundOwns, SBoundInherits, SPending, SExiting, Selection(..)) as X
import PSD3v2.Attribute.Types (Attribute, cx, cy, x, y, x1, y1, x2, y2, width, height, radius, fill, stroke, strokeWidth, opacity, transform, viewBox, id_, class_) as X
import PSD3v2.Behavior.Types (Behavior(..), defaultDrag, ScaleExtent(..)) as X

-- Shared data types
import PSD3.Data.Node (SimulationNode) as X
import PSD3.Internal.Types (D3Simulation_, Datum_, Index_, Selector) as X

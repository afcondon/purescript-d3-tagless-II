-- | PSD3: PureScript D3 - Type-safe, composable data visualization
-- |
-- | PSD3 is a PureScript library for creating D3.js visualizations using phantom types
-- | for compile-time safety and a declarative AST for clean, maintainable code.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import PSD3.AST as A
-- | import PSD3.Expr.Friendly (computed, computedStr, num, text)
-- | import PSD3.Render (runD3, select, renderTree)
-- |
-- | main :: Effect Unit
-- | main = void $ runD3 do
-- |   container <- select "body"
-- |   let ast =
-- |         A.elem SVG
-- |           [ computed "width" (num 800.0)
-- |           , computed "height" (num 600.0)
-- |           ]
-- |           `A.withChild`
-- |             A.elem Circle
-- |               [ computed "cx" (num 100.0)
-- |               , computed "cy" (num 100.0)
-- |               , computed "r" (num 50.0)
-- |               , computedStr "fill" (text "steelblue")
-- |               ]
-- |   renderTree container ast
-- | ```
-- |
-- | ## AST Example
-- |
-- | Declarative DOM structure with data binding:
-- |
-- | ```purescript
-- | import PSD3.AST as A
-- | import PSD3.Expr.Friendly (computed, computedStr, from, num, text)
-- |
-- | barChart :: Array Number -> A.AST Number
-- | barChart data =
-- |   A.elem SVG
-- |     [ computed "width" (num 500.0)
-- |     , computed "height" (num 300.0)
-- |     ]
-- |     `A.withChild`
-- |       A.joinData "bars" "rect" data \d ->
-- |         A.elem Rect
-- |           [ from "x" (\_ i -> toNumber i * 25.0)
-- |           , from "y" (\val _ -> 300.0 - val)
-- |           , computed "width" (num 20.0)
-- |           , from "height" (\val _ -> val)
-- |           , computedStr "fill" (text "steelblue")
-- |           ]
-- | ```
-- |
-- | ## Force Simulation
-- |
-- | For force-directed graphs, use the `psd3-simulation` package which provides:
-- | - `PSD3.ForceEngine` - Force simulation engine
-- | - `PSD3.ForceEngine.Setup` - Declarative force configuration
-- | - `PSD3.ForceEngine.Halogen` - Halogen integration
-- |
-- | ## Module Organization
-- |
-- | **Core Modules**:
-- | - `PSD3.AST` - Declarative visualization AST (element types, data joins)
-- | - `PSD3.Expr.Friendly` - Finally-tagless attribute DSL (computed, from, num, text)
-- | - `PSD3.Render` - D3 DOM rendering (runD3, select, renderTree)
-- | - `PSD3.Internal.Behavior.Types` - Behaviors (drag, zoom)
-- |
-- | **Interpreters** (for debugging):
-- | - `PSD3.Interpreter.Mermaid` - Mermaid diagram of AST structure
-- | - `PSD3.Interpreter.English` - English description of AST
-- |
-- | **Shared Modules**:
-- | - `PSD3.Data.Node` - SimulationNode, D3Link types
-- | - `PSD3.Data.Tree` - Tree data structures (for layouts)
-- | - `PSD3.Layout.Hierarchy.*` - Pure PureScript layouts (Tree, Cluster, Pack, etc.)
-- | - `PSD3.Layout.Sankey` - Pure PureScript Sankey layout
-- | - `PSD3.Scale` - D3 scale wrappers
-- |
-- | ## Import Patterns
-- |
-- | **Basic visualization**:
-- | ```purescript
-- | import PSD3.AST as A
-- | import PSD3.Expr.Friendly (computed, computedStr, from, num, text)
-- | import PSD3.Render (runD3, select, renderTree)
-- | ```
-- |
-- | **With behaviors**:
-- | ```purescript
-- | import PSD3.Render (on)
-- | import PSD3.Internal.Behavior.Types (Behavior(..), defaultDrag, defaultZoom)
-- | ```
-- |
-- | ## Exports
-- |
-- | This module re-exports the core PSD3 modules for convenient imports.
-- |
module PSD3 (module X) where

-- Clean Public API
import PSD3.AST (AST, ASTNode, ElementType(..), elem, joinData, withChild, withChildren, named, nestedJoin, updateJoin, updateNestedJoin, withBehaviors, beside, siblings, (>:), (+:)) as X
import PSD3.Internal.Attribute (Attribute(..)) as X
import PSD3.Expr.Friendly (computed, computedStr, computedWithIndex, computedStrWithIndex, from, fromStr, fromWithIndex, fromStrWithIndex, static, staticStr, num, text, bool, field, index, plus, minus, times, dividedBy, plusN, minusN, timesN, dividedByN, negated, lessThan, lessOrEqual, greaterThan, greaterOrEqual, equals, textEquals, textNotEquals, and_, or_, not_, ifThen, sin, cos, tan, asin, acos, atan, atan2, pi, append) as X
import PSD3.Render (runD3, D3M, D3Selection) as X

-- Selection & Transition Capabilities
import PSD3.Internal.Capabilities.Selection (class SelectionM, select, appendChildInheriting, appendData, renderTree, renderData, setAttrs, on, clear) as X
import PSD3.Internal.Capabilities.Transition (class TransitionM, withTransition, withTransitionExit) as X

-- Legacy type names (keep for backwards compatibility, prefer D3M/D3Selection)
import PSD3.Interpreter.D3 (D3v2M, D3v2Selection_, runD3v2M, reselectD3v2) as X

-- Internal Types (for advanced use)
import PSD3.Internal.Selection.Types (SEmpty, SBoundOwns, SBoundInherits, SPending, SExiting, Selection(..)) as X
import PSD3.Internal.Behavior.Types (Behavior(..), defaultDrag, ScaleExtent(..)) as X

-- Shared data types
import PSD3.Data.Node (SimulationNode) as X
import PSD3.Internal.Types (D3Simulation_, Datum_, Index_, Selector) as X
import PSD3.Data.Tree (TreeLayout(..)) as X
import PSD3.Data.DAGTree (DAGTree, DAGLink, PositionedDAGTree, PositionedNode, dagTree, addLink, addLinks, layoutDAGTree, getNodePosition, getExtraLinkPositions) as X

-- Scales
import PSD3.Scale (Scale, ContinuousScale, BandScale, OrdinalScale, Continuous, Ordinal, Band, linear, log, pow, sqrt, symlog, ordinal, band, point, domain, range, clamp, nice, padding, applyScale, ticks, invert, schemeCategory10, schemeCategory10At, schemePaired, schemePairedAt, interpolateViridis, interpolatePlasma, interpolateInferno, interpolateRdYlGn, interpolateTurbo) as X

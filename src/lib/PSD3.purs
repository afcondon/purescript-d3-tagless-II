-- | PSD3: PureScript D3 - Type-safe, composable data visualization
-- |
-- | This is the main entry point for the PSD3 library. For most use cases,
-- | importing this module will give you everything you need:
-- |
-- | ```purescript
-- | import PSD3
-- | ```
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import PSD3
-- | import PSD3.Attributes (fill, stroke, x, y, width, height)
-- | import Effect (Effect)
-- |
-- | main :: Effect Unit
-- | main = eval_D3M do
-- |   root <- attach "#chart"
-- |   svg <- appendTo root Svg [width 800.0, height 600.0]
-- |   circle <- appendTo svg Circle [x 100.0, y 100.0, fill "red"]
-- |   pure unit
-- | ```
-- |
-- | ## What's Exported
-- |
-- | - **Type Classes**: `SelectionM`, `SimulationM`, `SankeyM`
-- | - **Interpreters**: `D3M` monad and run functions
-- | - **Common Types**: Elements, Selectors, etc.
-- |
-- | ## For Advanced Use
-- |
-- | - `PSD3.Attributes` - All attribute functions
-- | - `PSD3.Types` - All type definitions
-- | - `PSD3.Capabilities.*` - Individual capability type classes
-- | - `PSD3.Interpreter.*` - Alternative interpreters (String, MetaTree)
-- | - `PSD3.Data.*` - Data structures (Tree, Graph, Node)
module PSD3 (module X) where

import Prelude as X

import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, filterSelection, mergeSelections, on, openSelection, selectUnder, setAttributes, simpleJoin, updateJoin) as X
import PSD3.Capabilities.Simulation (class SimulationM, actualizeForces, addTickFunction, mergeNewDataWithSim, removeTickFunction, setConfigVariable, setLinks, setLinksFromSelection, setNodes, setNodesFromSelection, simulationHandle, start, stop) as X
import PSD3.Capabilities.Sankey (class SankeyM, setSankeyData, setSankeyDataWithConfig) as X

import PSD3.Interpreter.D3 (D3M, D3SankeyM, D3SimM, eval_D3M, evalEffectSankey, evalEffectSimulation, eval_D3M_Sankey, eval_D3M_Simulation, exec_D3M, exec_D3M_Sankey, exec_D3M_Simulation, runD3M, runWithD3_Sankey, runWithD3_Simulation, run_D3M_Sankey, run_D3M_Simulation) as X

import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector) as X
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute) as X

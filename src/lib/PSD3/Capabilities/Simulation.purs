module PSD3.Capabilities.Simulation where

import PSD3.Capabilities.Selection (class SelectionM)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Types (D3Simulation_, Datum_, Index_)
import PSD3.Data.Node (D3Link, D3LinkSwizzled, D3_SimulationNode)
import PSD3.Internal.Simulation.Types (SimVariable, Step)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Prelude (class Eq, class Monad, Unit)

-- | These data types are to prevent "boolean blindness" when choosing forces to enable and disable
type ForceConfigLists = { enable :: Array Label, disable :: Array Label }

-- TODO
-- parameterize out the D3_ part of SimulationNode - could we make all this opaque?
-- note in the implementation we're putting the nodes and links into the SimulationState, but we
-- return them for the Join to use using the same type...however, they may actually be changed
-- from what was sent...it's not tidy yet
class (Monad m, SelectionM selection m) <= SimulationM selection m | m -> selection where
  -- control
  start :: m Unit
  stop  :: m Unit
  -- config
  setConfigVariable    :: SimVariable -> m Unit
  -- management of forces
  -- | Enable only the forces in the given Set, disable all others
  actualizeForces:: Set Label -> m Unit
  -- setForcesByLabel :: { enable :: Array Label, disable :: Array Label } -> m Unit -- REVIEW not convinced this function is necessary
  -- management of data (nodes and links)
  setNodes :: forall d.   Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  setLinks :: forall d r id. (Eq id) => Array (D3Link id r) -> Array (D3_SimulationNode d) -> (Datum_ -> Index_ ) -> m (Array (D3LinkSwizzled (D3_SimulationNode d) r))
  -- the following versions are less type-safe but they are necessary for updating simulations
  -- it's up to the "script" writer to make sure the selection data matches the D3SimulationNode and/or D3LinkSwizzled
  setNodesFromSelection :: selection -> m Unit
  setLinksFromSelection :: selection -> (Datum_ -> Boolean) -> m Unit
  mergeNewDataWithSim :: forall d r id. (Eq id) =>
    selection -> -- nodes selection
    (Datum_ -> Index_) -> -- nodes keyFn
    selection -> -- links selection
    (Datum_ -> Index_) -> -- links KeyFn
    RawData d r id -> -- links and nodes raw data
    m { links :: (Array (D3LinkSwizzled (D3_SimulationNode d) r)), nodes :: (Array (D3_SimulationNode d))}

  -- simulationHandle is needed for (at least) the following tick functions
  simulationHandle :: m D3Simulation_
  -- adding functions that occur on every tick of the simulation clock
  -- this could potentially be extracted from here by doing each step of sim in PureScript
  addTickFunction    :: Label -> Step selection -> m Unit
  removeTickFunction :: Label                   -> m Unit

-- RawData type exists to clean up types of mergeNewDataWithSim
type RawData d r id = {
  nodes :: Array (D3_SimulationNode d)
, links :: Array (D3Link id r)
}

type Staging selection d r id = {
    selections :: {
      nodes :: Maybe selection
    , links :: Maybe selection
    }
   -- filter for links given to simulation engine, you don't necessarily want all links to be exerting force
  , linksWithForce :: Datum_ -> Boolean
  , rawdata :: RawData d r id
}

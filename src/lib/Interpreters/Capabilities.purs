module D3Tagless.Capabilities where

import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Simulation_, Datum_, Element, Index_, Selector)
import D3.Node (D3Link, D3LinkSwizzled, D3_SimulationNode)
import D3.Selection (Behavior, SelectionAttribute)
import D3.Simulation.Types (Force, ForceStatus, SimVariable, Step)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Prelude (class Eq, class Monad, Unit)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= SelectionM selection m where
  appendTo        :: selection -> Element -> Array (SelectionAttribute) -> m selection
  selectUnder     :: selection -> Selector selection -> m selection
  attach          :: Selector selection -> m selection
  filterSelection :: selection -> Selector selection -> m selection
  mergeSelections :: selection -> selection -> m selection
  setAttributes   :: selection -> Array (SelectionAttribute) -> m Unit
  on              :: selection -> Behavior selection -> m Unit
  -- `openSelection` hopefully isn't needed and can be folded back into the UpdateJoin somehow (perhaps as a "first time only" thing or "when null")
  openSelection   :: selection -> Selector selection -> m selection 
  simpleJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_) -> m selection
  updateJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_)
    -> m { enter :: selection, exit :: selection, update :: selection }


-- TODO things that are not handled by this (deliberately) ultra-simple grammar so far:
-- 1) say you wanted to attach to "div#hook" and then select an _already existing_ <h1> in it and apply Attrs to that h1
-- 2)...

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
  -- | make the forces in the simulation match the forces in the simulation state
  actualizeForces:: Map Label ForceStatus -> m Unit 
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

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
  appendTo   :: selection -> Element -> Array (SelectionAttribute) -> m selection
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
  -- removeAllForces       ::                m Unit
  addForces             :: Array Force -> m Unit
  -- addForce              :: Force       -> m Unit
  -- toggleForceByLabel    :: Label       -> m Unit
  setForcesByLabel      :: { enable :: Array Label, disable :: Array Label } -> m Unit
  -- enableOnlyTheseForces :: Array Label -> m Unit

  -- management of data (nodes and links)
  -- merge these functions back together later
  carryOverSimStateN :: forall d r id. selection -> RawData d r id -> (Datum_ -> Index_) ->  m (Array (D3_SimulationNode d))
  
  -- REVIEW probably we can actually model the simulation merge bit analogously to the swizzling for links?
  setNodes :: forall d.   Array (D3_SimulationNode d) -> m Unit
  setLinks :: forall d r. Array (D3LinkSwizzled (D3_SimulationNode d) r) -> m Unit
  setForces :: Map Label ForceStatus -> m Unit
  -- should be able to merge carryOverSimStateL and swizzleLinks (and possibly carryOverSimStateN)
  carryOverSimStateL :: forall d r id. (Eq id) => selection -> RawData d r id -> (Datum_ -> Index_) ->  m (Array (D3Link id r))
  swizzleLinks :: forall d r id. 
    Array (D3Link id r) ->
    Array (D3_SimulationNode d) ->
    (Datum_ -> Index_) ->
    m (Array (D3LinkSwizzled (D3_SimulationNode d) r))

  getNodes :: forall d.   m (Array (D3_SimulationNode d))
  getLinks :: forall d r. m (Array (D3LinkSwizzled (D3_SimulationNode d) r))

  -- adding functions that occur on every tick of the simulation clock
  addTickFunction    :: Label -> Step selection -> m Unit 
  removeTickFunction :: Label                   -> m Unit
  defaultNodeTick    :: Label -> selection      -> m Unit
  defaultLinkTick    :: Label -> selection      -> m Unit

  -- writing callbacks from JavaScript will sometimes necessitate having the simulation handle
  -- this can't easily be solved until / unless D3 is replaced with something else
  simulationHandle :: m D3Simulation_

-- RawData type exists to clean up types of carryOverSimState functions and maybe can just fold into Staging if 
-- Simulation capability can wrap up, ie hide, the complexity around updates of running simulations and the 
-- corresponding need for copying data from existing selections without losing simulation position/velocity information
type RawData d r id = {
  nodes :: Array (D3_SimulationNode d)
, links :: Array (D3Link id r) 
}
type Staging selection d r id = {
    selections :: { 
      nodes :: Maybe selection
    , links :: Maybe selection
    }
  , rawdata :: RawData d r id
  , forces :: Map Label ForceStatus
}

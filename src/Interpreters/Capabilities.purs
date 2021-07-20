module D3Tagless.Capabilities where

import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Simulation_, Datum_, Index_, Selector)
import D3.Node (D3_Link, D3_SimulationNode)
import D3.Selection (Behavior, ChainableS, D3_Node, Join)
import D3.Simulation.Types (Force, SimVariable, Step)
import Prelude (class Monad, Unit)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= SelectionM selection m where
  attach :: Selector selection -> m selection

  appendElement :: selection -> D3_Node              -> m selection
  filterSelection :: selection -> Selector selection -> m selection
  modifySelection :: selection -> Array (ChainableS) -> m Unit
  
  on     :: selection -> Behavior -> m Unit

  join   :: ∀ datum.  selection -> Join datum -> m selection

infix 4 join as <+>
infix 4 appendElement as +

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
  removeAllForces      ::                m Unit
  addForces            :: Array Force -> m Unit
  addForce             :: Force       -> m Unit
  toggleForceByLabel   :: Label            -> m Unit
  setForcesByLabel     :: ForceConfigLists -> m Unit

  -- management of data (nodes and links)
  setNodes :: forall d.   Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  setLinks :: forall id d r. Array (D3_Link id r)    -> (Datum_ -> id) -> m (Array (D3_Link d r)) -- we require a key function for links
  
  -- adding functions that occur on every tick of the simulation clock
  addTickFunction    :: Label -> Step selection -> m Unit 
  removeTickFunction :: Label                   -> m Unit
  defaultNodeTick    :: Label -> selection      -> m Unit
  defaultLinkTick    :: Label -> selection      -> m Unit

  -- writing callbacks from JavaScript will sometimes necessitate having the simulation handle
  -- this can't easily be solved until / unless D3 is replaced with something else
  simulationHandle :: m D3Simulation_



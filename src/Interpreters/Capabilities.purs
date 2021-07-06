module D3Tagless.Capabilities where

import D3.Attributes.Instances (Label)
import D3.Data.Types (Selector)
import D3.Node (D3_Link, D3_SimulationNode)
import D3.Selection (Behavior, ChainableS, D3_Node, Join)
import D3.Simulation.Types (Force, SimVariable, Step)
import Prelude (class Monad, Unit)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= SelectionM selection m where
  attach :: Selector -> m selection

  appendElement :: selection -> D3_Node                -> m selection
  filterSelection :: selection -> Selector             -> m selection
  modifySelection :: selection -> Array (ChainableS)   -> m Unit
  
  on     :: selection -> Behavior -> m Unit

  join   :: ∀ datum.  selection -> Join datum -> m selection

infix 4 join as <+>

-- TODO things that are not handled by this (deliberately) ultra-simple grammar so far:
-- 1) say you wanted to attach to "div#hook" and then select an _already existing_ <h1> in it and apply Attrs to that h1
-- 2)...

-- REVIEW this might need to be parameterized with the selection type too, so that the two capabilities match
class (Monad m) <= SimulationM m where
  -- control
  start :: m Unit
  stop  :: m Unit
  -- config
  setConfigVariable    :: SimVariable -> m Unit
  -- management of forces
  removeAllForces      ::                m Unit
  loadForces           :: Array Force -> m Unit
  addForce             :: Force       -> m Unit
  disableForcesByLabel :: Array Label -> m Unit
  enableForcesByLabel  :: Array Label -> m Unit
  -- management of data (nodes and links)
  -- TODO parameterize out the D3_ part of SimulationNode
  setNodes :: forall d.   Array (D3_SimulationNode d) -> m Unit
  setLinks :: forall d r. Array (D3_Link d r)         -> m Unit
  -- setNodes :: forall d.   Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  -- setLinks :: forall d r. Array (D3_Link d r)         -> m (Array (D3_Link d r))
  -- tick functions
  addTickFunction    :: forall selection. Label -> Step selection -> m Unit 
  removeTickFunction ::                   Label                   -> m Unit 



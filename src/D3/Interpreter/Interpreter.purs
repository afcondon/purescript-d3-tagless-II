module D3.Interpreter where

import Control.Monad.State (class MonadState)
import D3.Attributes.Instances (Label)
import D3.Data.Types (D3Selection_, Selector)
import D3.Node (D3_SimulationNode)
import D3.Selection (Behavior, ChainableS, D3_Node, Join)
import D3.Simulation.Forces (Force)
import Prelude (class Monad, Unit)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= SelectionM selection m where
  attach :: Selector                          -> m selection
  append :: selection -> D3_Node              -> m selection
  filter :: selection -> Selector             -> m selection
  modify :: selection -> Array (ChainableS)   -> m selection
  on     :: selection -> Behavior             -> m selection

  join   :: ∀ datum.  selection -> Join datum -> m selection

infix 4 join as <+>

-- TODO things that are not handled by this (deliberately) ultra-simple grammar so far:
-- 1) say you wanted to attach to "div#hook" and then select an _already existing_ <h1> in it and apply Attrs to that h1
-- 2)...

-- REVIEW this might need to be parameterized with the selection type too, so that the two capabilities match
class (Monad m) <= SimulationM m where
  -- management of forces
  removeAllForces      ::                m Unit
  loadForces           :: Array Force -> m Unit
  addForce             :: Force       -> m Unit
  disableForcesByLabel :: Array Label -> m Unit
  enableForcesByLabel  :: Array Label -> m Unit
  -- config
  setAlpha             :: Number      -> m Unit
  setAlphaTarget       :: Number      -> m Unit
  setAlphaMin          :: Number      -> m Unit
  setAlphaDecay        :: Number      -> m Unit
  setVelocityDecay     :: Number      -> m Unit
  -- control
  start      :: m Unit
  stop       :: m Unit
  -- management of data (nodes and links)
  -- TODO parameterize out the D3_ part of SimulationNode
  setNodes :: forall d. Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  -- tick functions
  createTickFunction :: forall selection. Step selection -> m Unit 

data Step selection = Step Label selection (Array ChainableS)


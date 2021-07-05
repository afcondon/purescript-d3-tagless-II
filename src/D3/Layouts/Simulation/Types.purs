module D3.Simulation.Types where

import Prelude

import D3.Attributes.Instances (Attribute, Label)
import D3.Data.Types (D3Selection_, D3Simulation_, Datum_, PointXY)
import D3.FFI (D3ForceHandle_, SimulationConfig_, initSimulation_)
import D3.Node (D3_Link, NodeID)
import D3.Selection (ChainableS)
import Data.Map as M
import Data.Newtype (class Newtype)

data SimVariable = Alpha Number | AlphaTarget Number | AlphaMin Number | AlphaDecay Number | VelocityDecay Number

data Step selection = Step selection (Array ChainableS)

data SimCommand =
    Start
  | Stop
  | RemoveAllForcesSim
  | SetConfigVariable    SimVariable
  | LoadForces           (Array Force)
  | AddForce             Force
  | DisableForcesByLabel (Array Label)
  | EnableForcesByLabel  (Array Label)
  -- | SetNodes             (Array d)
  -- | SetLinks             (Array l)
  -- | AddTickFunction Label (Step selection)
  | RemoveTickFunction    Label

-- TODO we won't export the constructor here when we close exports
data Force = Force Label ForceStatus ForceType (Array ChainableF) D3ForceHandle_

instance Show Force where
  show (Force label status t cs h) = "Force: " <> label <> " " <> show status 

-- not sure if there needs to be a separate type for force attributes, maybe not, but we'll start assuming so
newtype ChainableF = ForceT Attribute
    -- following are used in ChainableS but probably not here on Forces...delete when sure
              -- | TextT Attribute
              -- | TransitionT (Array ChainableS) Transition
              -- | RemoveT
              -- | OnT MouseEvent Listener_
derive instance Newtype ChainableF _

data ForceStatus = ForceActive | ForceDisabled
derive instance eqForceStatus :: Eq ForceStatus
instance showForceStatus :: Show ForceStatus where
  show ForceActive = "active"
  show ForceDisabled = "inactive"

data ForceType = 
    ForceManyBody                                  -- strength, theta, distanceMax, distanceMin
  | ForceCenter                                    -- strength, x, y
  | ForceCollide                                   -- strength, radius, iterations
  | ForceX                                         -- strength, x
  | ForceY                                         -- strength, y
  | ForceRadial                                    -- strength, radius, x, y
  | ForceLink (forall r. Array (D3_Link NodeID r)) -- strength, id, distance, iterations, links
  | ForceFixPositionXY (Datum_ -> PointXY) -- function is static, provided to constructor
  | ForceFixPositionX  (Datum_ -> Number)
  | ForceFixPositionY  (Datum_ -> Number)
                                                   -- TODO need something to hold extra custom force config, perhaps?
  | CustomForce                                    -- ???

instance Show ForceType where
  show ForceManyBody           = "ForceManyBody"
  show ForceCenter             = "ForceCenter"
  show ForceCollide            = "ForceCollide"
  show ForceX                  = "ForceX"
  show ForceY                  = "ForceY"
  show ForceRadial             = "ForceRadial"
  show (ForceFixPositionXY xy) = "ForceFixPositionXY"
  show (ForceFixPositionX x)   = "ForceFixPositionX"
  show (ForceFixPositionY y)   = "ForceFixPositionY"
  show (ForceLink _)           = "ForceLink"
  show CustomForce             = "CustomForce"

data SimulationState_ = SS_ { -- TODO move back to Simulation.purs ?
    simulation    :: D3Simulation_
  , running       :: Boolean
  , forces        :: M.Map Label Force
  , ticks         :: M.Map Label (Step D3Selection_)

  , alpha         :: Number
  , alphaTarget   :: Number
  , alphaMin      :: Number
  , alphaDecay    :: Number
  , velocityDecay :: Number
}

initialSimulationState :: SimulationState_
initialSimulationState = SS_
   {  simulation   : initSimulation_ defaultConfigSimulation  
    , alpha        : defaultConfigSimulation.alpha
    , alphaTarget  : defaultConfigSimulation.alphaTarget
    , alphaMin     : defaultConfigSimulation.alphaMin
    , alphaDecay   : defaultConfigSimulation.alphaDecay
    , velocityDecay: defaultConfigSimulation.velocityDecay
    , running      : defaultConfigSimulation.running
    , forces       : M.empty
    , ticks        : M.empty
  }

defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
    , running      : true
}

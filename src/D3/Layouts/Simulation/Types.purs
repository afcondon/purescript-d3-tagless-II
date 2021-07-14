module D3.Simulation.Types where

import Prelude

import D3.Attributes.Instances (Attribute, Label)
import D3.Data.Types (D3Selection_, D3Simulation_, Datum_, PointXY)
import D3.Examples.Spago.Files (SpagoGraphLinkID)
import D3.Examples.Spago.Model (SpagoSimNode)
import D3.FFI (D3ForceHandle_, OpaqueLinkType_, OpaqueNodeType_, SimulationConfig_, initSimulation_)
import D3.Node (D3_Link, D3_SimulationNode, NodeID)
import D3.Selection (ChainableS)
import Data.Map as M
import Data.Newtype (class Newtype)
import Debug (trace)

data SimVariable = Alpha Number | AlphaTarget Number | AlphaMin Number | AlphaDecay Number | VelocityDecay Number

data Step selection = Step selection (Array ChainableS) | StepTransformFFI selection (Datum_ -> String)

data SimBusCommand selection =
    Start
  | Stop
  | RemoveAllForces
  | SetConfigVariable     SimVariable
  | LoadForces            (Array Force)
  | AddForce              Force
  | DisableForcesByLabel  (Array Label)
  | EnableForcesByLabel   (Array Label)
  | SetNodes              (forall d. Array (D3_SimulationNode d))
  | SetLinks              (forall d r. Array (D3_Link d r))
  | AddTickFunction Label (Step selection)
  | RemoveTickFunction    Label

instance showSimVariable :: Show SimVariable where
  show (Alpha n)         = "Alpha: " <> show n
  show (AlphaTarget n)   = "AlphaTarget: " <> show n
  show (AlphaMin n)      = "AlphaMin: " <> show n
  show (AlphaDecay n)    = "AlphaDecay: " <> show n
  show (VelocityDecay n) = "VelocityDecay: " <> show n

instance showSimCommand :: Show (SimBusCommand D3Selection_) where
  show Start                      = "Start"
  show Stop                       = "Stop"
  show RemoveAllForces            = "RemoveAllForces"
  show (SetConfigVariable c)      = "(SetConfigVariable" <> " " <> show c <> ")"
  show (LoadForces _)             = "(LoadForces _)"
  show (AddForce _)               = "(AddForce _)"
  show (DisableForcesByLabel _)   = "(DisableForcesByLabel _)"
  show (EnableForcesByLabel _)    = "(EnableForcesByLabel _)"
  show (SetNodes _)               = "(SetNodes _)"
  show (SetLinks _)               = "(SetLinks _)"
  show (AddTickFunction _ _)      = "(AddTickFunction _ _)"
  show (RemoveTickFunction _)     = "(RemoveTickFunction _)"

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
  | ForceFixPositionXY (Datum_ -> { x :: Number, y :: Number }) (Datum_ -> Boolean) -- function is static, provided to constructor
  | ForceFixPositionX  (Datum_ -> { x :: Number }) (Datum_ -> Boolean) 
  | ForceFixPositionY  (Datum_ -> { y :: Number }) (Datum_ -> Boolean) 
                                                   -- TODO need something to hold extra custom force config, perhaps?
  | CustomForce                                    -- ???

instance Show ForceType where
  show ForceManyBody           = "ForceManyBody"
  show ForceCenter             = "ForceCenter"
  show ForceCollide            = "ForceCollide"
  show ForceX                  = "ForceX"
  show ForceY                  = "ForceY"
  show ForceRadial             = "ForceRadial"
  show (ForceFixPositionXY _ _) = "ForceFixPositionXY"
  show (ForceFixPositionX _ _)   = "ForceFixPositionX"
  show (ForceFixPositionY _ _)   = "ForceFixPositionY"
  show (ForceLink _)           = "ForceLink"
  show CustomForce             = "CustomForce"

data SimulationState_ = SS_ { -- TODO move back to Simulation.purs ?
    simulation_   :: D3Simulation_
  , running       :: Boolean
  , forces        :: M.Map Label Force
  , ticks         :: M.Map Label (Step D3Selection_)

  , links         :: Array OpaqueLinkType_
  , nodes         :: Array OpaqueNodeType_

  , alpha         :: Number
  , alphaTarget   :: Number
  , alphaMin      :: Number
  , alphaDecay    :: Number
  , velocityDecay :: Number
}

-- need (perhaps) to put the simulation running in a fiber but get the simulation handle out via an AVar or something?
-- this presupposes that the reason the simulation runs so slowly is something to do with event loops and get be solved
-- with a thread, this is not at all certain
initialSimulationState :: SimulationState_
initialSimulationState = SS_
   {  simulation_  : initSimulation_ defaultConfigSimulation  
    , nodes        : []
    , links        : []
    , alpha        : defaultConfigSimulation.alpha
    , alphaTarget  : defaultConfigSimulation.alphaTarget
    , alphaMin     : defaultConfigSimulation.alphaMin
    , alphaDecay   : defaultConfigSimulation.alphaDecay
    , velocityDecay: defaultConfigSimulation.velocityDecay
    , running      : defaultConfigSimulation.running
    , forces       : M.empty
    , ticks        : M.empty
  }
  where
    _ = trace { simulation: "initialized" } \_ -> unit

defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
    , running      : true
}

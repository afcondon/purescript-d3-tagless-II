module D3.Simulation.Types where

import Prelude

import D3.Attributes.Instances (Attribute, Label)
import D3.Data.Types (D3Selection_, D3Simulation_, Datum_, Index_, PointXY)
import D3.Examples.Spago.Files (SpagoGraphLinkID)
import D3.Examples.Spago.Model (SpagoSimNode)
import D3.FFI (D3ForceHandle_, OpaqueLinkType_, OpaqueNodeType_, SimulationConfig_, forceLink_, initSimulation_)
import D3.Node (D3_Link, D3_SimulationNode, NodeID)
import D3.Selection (ChainableS)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Debug (trace)

data SimVariable = Alpha Number | AlphaTarget Number | AlphaMin Number | AlphaDecay Number | VelocityDecay Number

data Step selection = Step selection (Array ChainableS) | StepTransformFFI selection (Datum_ -> String)

instance showSimVariable :: Show SimVariable where
  show (Alpha n)         = "Alpha: " <> show n
  show (AlphaTarget n)   = "AlphaTarget: " <> show n
  show (AlphaMin n)      = "AlphaMin: " <> show n
  show (AlphaDecay n)    = "AlphaDecay: " <> show n
  show (VelocityDecay n) = "VelocityDecay: " <> show n

-- TODO we won't export Force constructor here when we close exports
data Force = Force Label ForceStatus ForceType (Maybe ForceFilter) (Array ChainableF) D3ForceHandle_

instance Show Force where
  show (Force label status t (Just f) cs h) = "Force: " <> label <> " " <> show status <> " " <> show f
  show (Force label status t (Nothing) cs h) = "Force: " <> label <> " " <> show status <> " applying to all nodes"

-- not sure if there needs to be a separate type for force attributes, maybe not, but we'll start assuming so
newtype ChainableF = ForceT Attribute
derive instance Newtype ChainableF _

data ForceStatus = ForceActive | ForceDisabled
derive instance eqForceStatus :: Eq ForceStatus
instance showForceStatus :: Show ForceStatus where
  show ForceActive = "active"
  show ForceDisabled = "inactive"

allNodes = Nothing -- just some sugar so that force declarations are nicer to read, Nothing == No filter == applies to all nodes
data ForceFilter = FilterNodes String (Datum_ -> Boolean)
instance Show ForceFilter where
  show (FilterNodes description _) = "applying to " <> description

showForceFilter (Just (FilterNodes description _)) = "applying to " <> description
showForceFilter Nothing = " (applies to all nodes)"

data ForceType = 
    ForceManyBody                                  -- strength, theta, distanceMax, distanceMin
  | ForceCenter                                    -- strength, x, y
  | ForceCollide                                   -- strength, radius, iterations
  | ForceX                                         -- strength, x
  | ForceY                                         -- strength, y
  | ForceRadial                                    -- strength, radius, x, y
  | ForceLink -- data for links can _only_ be provided when setting links in a simulation, initial links array will always be []
  | ForceFixPositionXY (Datum_ -> Index_ -> { x :: Number, y :: Number }) 
  | ForceFixPositionX  (Datum_ -> Index_ -> { x :: Number })
  | ForceFixPositionY  (Datum_ -> Index_ -> { y :: Number })
                                                   -- TODO need something to hold extra custom force config, perhaps?
  | CustomForce                                    -- ???

instance Show ForceType where
  show ForceManyBody            = "ForceManyBody"
  show ForceCenter              = "ForceCenter"
  show ForceCollide             = "ForceCollide"
  show ForceX                   = "ForceX"
  show ForceY                   = "ForceY"
  show ForceRadial              = "ForceRadial"
  show (ForceFixPositionXY _) = "ForceFixPositionXY"
  show (ForceFixPositionX _)  = "ForceFixPositionX"
  show (ForceFixPositionY _)  = "ForceFixPositionY"
  show ForceLink                = "ForceLink"
  show CustomForce              = "CustomForce"

data SimulationState_ = SS_ { -- TODO move back to Simulation.purs ?
    simulation_   :: D3Simulation_
  , running       :: Boolean
  , forces        :: M.Map Label Force
  , ticks         :: M.Map Label (Step D3Selection_)

  , nodes         :: Array OpaqueNodeType_
  , selections    :: M.Map Label D3Selection_

  , alpha         :: Number
  , alphaTarget   :: Number
  , alphaMin      :: Number
  , alphaDecay    :: Number
  , velocityDecay :: Number
}

-- unused parameter is to ensure a NEW simulation is created so that, for example, two Halogen components won't _accidentally_ share one
initialSimulationState :: Int -> SimulationState_
initialSimulationState id = SS_
   {  simulation_  : initSimulation_ defaultConfigSimulation  
    , nodes        : []
    , alpha        : defaultConfigSimulation.alpha
    , alphaTarget  : defaultConfigSimulation.alphaTarget
    , alphaMin     : defaultConfigSimulation.alphaMin
    , alphaDecay   : defaultConfigSimulation.alphaDecay
    , velocityDecay: defaultConfigSimulation.velocityDecay
    , running      : defaultConfigSimulation.running
    , forces       : M.empty
    , ticks        : M.empty
    , selections   : M.empty
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

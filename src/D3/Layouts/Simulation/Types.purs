module D3.Simulation.Types where

import Prelude

import D3.Attributes.Instances (AttributeSetter, Label)
import D3.Data.Types (D3Selection_, D3Simulation_, Datum_, Index_)
import D3.FFI (D3ForceHandle_, SimulationConfig_, defaultKeyFunction_, initSimulation_)
import D3.Selection (ChainableS)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
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
data Force = 
    Force     Label ForceStatus RegularForceType (Maybe ForceNodesFilter) (Array ChainableF) D3ForceHandle_
  | LinkForce Label ForceStatus                  (Maybe ForceLinksFilter) (Array ChainableF) D3ForceHandle_
  | FixForce  Label ForceStatus FixForceType     (Maybe ForceNodesFilter) (Array ChainableF) D3ForceHandle_

instance Show Force where
  show (Force label status t f cs h) = "Force: " <> label <> " " <> show status <> " applying to all nodes"
  show (LinkForce label status (Just f) cs h) = "LinkForce: " <> label <> " " <> show status <> " " <> show f
  show (LinkForce label status (Nothing) cs h) = "LinkForce: " <> label <> " " <> show status <> " applying to all nodes"
  show (FixForce label status t (Just f) cs h) = "FixForce: " <> label <> " " <> show status <> " " <> show f
  show (FixForce label status t (Nothing) cs h) = "FixForce: " <> label <> " " <> show status <> " applying to all nodes"

-- not sure if there needs to be a separate type for force attributes, maybe not, but we'll start assuming so
newtype ChainableF = ForceT AttributeSetter
derive instance Newtype ChainableF _

data ForceStatus = ForceActive | ForceDisabled
derive instance eqForceStatus :: Eq ForceStatus
instance showForceStatus :: Show ForceStatus where
  show ForceActive = "active"
  show ForceDisabled = "inactive"

allNodes :: forall t69. Maybe t69
allNodes = Nothing -- just some sugar so that force declarations are nicer to read, Nothing == No filter == applies to all nodes
-- with sufficiently nice types might be possible to have just one filter type which would be better
-- but for now we have a separate one for Nodes and Links so that the predicate can be tweaked for each
data ForceNodesFilter = FilterNodes Label (Datum_ -> Boolean)
data ForceLinksFilter = FilterLinks Label (Datum_ -> Boolean) 
instance Show ForceNodesFilter where
  show (FilterNodes description _) = description
instance Show ForceLinksFilter where
  show (FilterLinks description _) = description

showForceFilter :: Force -> String
showForceFilter =
  case _ of
    (Force _ _ _ f _ _)    -> showForceNodesFilter f
    (LinkForce _ _ f _ _)  -> showForceLinksFilter f
    (FixForce _ _ _ f _ _) -> showForceNodesFilter f

showForceLinksFilter :: Maybe ForceLinksFilter -> String
showForceLinksFilter (Just (FilterLinks description _)) = description
showForceLinksFilter Nothing = " (links are not filtered)"

showForceNodesFilter :: Maybe ForceNodesFilter -> String
showForceNodesFilter (Just (FilterNodes description _)) = description
showForceNodesFilter Nothing = " (all nodes)"

data RegularForceType = 
    ForceManyBody -- strength, theta, distanceMax, distanceMin
  | ForceCenter   -- strength, x, y
  | ForceCollide  -- strength, radius, iterations
  | ForceX        -- strength, x
  | ForceY        -- strength, y
  | ForceRadial   -- strength, radius, x, y

data LinkForceType = 
  -- NOTE because data for links can _only_ be provided when setting links in a simulation, initial links array will always be []
  ForceLink     -- strength, distance, iterations, keyFn

data FixForceType =
    ForceFixPositionXY (Datum_ -> Index_ -> { x :: Number, y :: Number }) 
  | ForceFixPositionX  (Datum_ -> Index_ -> { x :: Number })
  | ForceFixPositionY  (Datum_ -> Index_ -> { y :: Number })

data CustomForceType = CustomForce -- TODO need something to hold extra custom force config, perhaps?

instance Show RegularForceType where
  show ForceManyBody          = "ForceManyBody"
  show ForceCenter            = "ForceCenter"
  show ForceCollide           = "ForceCollide"
  show ForceX                 = "ForceX"
  show ForceY                 = "ForceY"
  show ForceRadial            = "ForceRadial"

instance Show LinkForceType where
  show ForceLink              = "ForceLink"

instance Show FixForceType where
  show (ForceFixPositionXY _) = "ForceFixPositionXY"
  show (ForceFixPositionX _)  = "ForceFixPositionX"
  show (ForceFixPositionY _)  = "ForceFixPositionY"

-- representation of all that is stateful in the D3 simulation engine
-- not generalized because we have no other examples of simulation engines ATM
-- perhaps it can become a more abstract interface in the future
data D3SimulationState_ = SimState_ { 
    simulation_   :: D3Simulation_
  , forces        :: M.Map Label Force
  , ticks         :: M.Map Label (Step D3Selection_)

  , nodes         :: Nullable D3Selection_
  , links         :: Nullable D3Selection_
  , key           :: (Datum_ -> Index_)

  , alpha         :: Number
  , alphaTarget   :: Number
  , alphaMin      :: Number
  , alphaDecay    :: Number
  , velocityDecay :: Number
}

-- unused parameter is to ensure a NEW simulation is created so that, 
-- for example, two Halogen components won't _accidentally_ share one
initialSimulationState :: Int -> D3SimulationState_
initialSimulationState id = SimState_
   {  -- common state for all D3 Simulation
      simulation_  : initSimulation_ defaultConfigSimulation
    , nodes        : null
    , links        : null
    , key          : defaultConfigSimulation.key

    , forces       : M.empty
    , ticks        : M.empty
    -- parameters of the D3 simulation engine
    , alpha        : defaultConfigSimulation.alpha
    , alphaTarget  : defaultConfigSimulation.alphaTarget
    , alphaMin     : defaultConfigSimulation.alphaMin
    , alphaDecay   : defaultConfigSimulation.alphaDecay
    , velocityDecay: defaultConfigSimulation.velocityDecay
  }
  where
    _ = trace { simulation: "initialized", engineNo: id } \_ -> unit

defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
    , key          : defaultKeyFunction_
}


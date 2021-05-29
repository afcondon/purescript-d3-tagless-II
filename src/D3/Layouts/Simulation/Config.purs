module D3.Simulation.Config where

import D3.Attributes.Instances (class ToAttr, Attribute(..), toAttr)
import D3.Data.Types (Datum_, Index_)
import Data.Number (infinity)
import Prelude (negate, (<<<))
import Unsafe.Coerce (unsafeCoerce)

foreign import data D3ForceHandle_     :: Type
foreign import data CustomForceConfig_ :: Type

-- not sure if there needs to be a separate type for force attributes, maybe not, but we'll start assuming so
data ChainableF = ForceT Attribute
    -- following are used in ChainableS but probably not here on Forces...delete when sure
              -- | TextT Attribute
              -- | TransitionT (Array ChainableS) Transition
              -- | RemoveT
              -- | OnT MouseEvent Listener_
    
defaultForceRadialConfig       :: String -> (Datum_ -> Index_ -> Number) -> Array ChainableF
defaultForceRadialConfig name r =  
    [ forceName name, radius r, strength 0.1, cx 0.0, cy 0.0 ]

defaultForceManyConfig         :: String -> Array ChainableF
defaultForceManyConfig name = 
  [ forceName name, strength (-30.0), theta 0.9, distanceMin 1.0, distanceMax infinity ]

defaultForceCenterConfig       :: String -> Array ChainableF
defaultForceCenterConfig name = 
  [ forceName name, cx 0.0, cy 0.0, strength 1.0 ]


defaultForceCollideConfig      :: String -> (Datum_ -> Index_ -> Number) -> Array ChainableF
defaultForceCollideConfig name r = 
  [ forceName name, radius r, strength 1.0, iterations 1.0 ]

defaultForceXConfig            :: String -> Array ChainableF
defaultForceXConfig name = 
  [ forceName name, strength 0.1, x 0.0 ]

defaultForceYConfig            :: String -> Array ChainableF
defaultForceYConfig name = 
  [ forceName name, strength 0.1, y 0.0 ]

-- TODO links will need to be separate since they are not a chainable / attr type thing
defaultForceLinkConfig         :: forall d. String -> (d -> Index_ -> Number) -> Array ChainableF
defaultForceLinkConfig name id = 
  [ forceName name, strength 1.0, distance 30.0, iterations 1.0, index defaultIndex  ]
  
-- | a record to initialize / configure simulations
type SimulationConfig_ = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
}



defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
}


-- | ==================================================================================================
-- | ========================= sugar for the various attributes of forces =============================
-- | ==================================================================================================
forceName :: ∀ a. ToAttr String a => a -> ChainableF
forceName = ForceT <<< ToAttribute "name" <<< toAttr

radius :: ∀ a. ToAttr Number a => a -> ChainableF
radius = ForceT <<< ToAttribute "radius" <<< toAttr

strength :: ∀ a. ToAttr Number a => a -> ChainableF
strength = ForceT <<< ToAttribute "strength" <<< toAttr

cx :: ∀ a. ToAttr Number a => a -> ChainableF
cx = ForceT <<< ToAttribute "cx" <<< toAttr

cy :: ∀ a. ToAttr Number a => a -> ChainableF
cy = ForceT <<< ToAttribute "cy" <<< toAttr

theta :: ∀ a. ToAttr Number a => a -> ChainableF
theta = ForceT <<< ToAttribute "theta" <<< toAttr

distanceMin :: ∀ a. ToAttr Number a => a -> ChainableF
distanceMin = ForceT <<< ToAttribute "distanceMin" <<< toAttr

distanceMax :: ∀ a. ToAttr Number a => a -> ChainableF
distanceMax = ForceT <<< ToAttribute "distanceMax" <<< toAttr

iterations :: ∀ a. ToAttr Number a => a -> ChainableF
iterations = ForceT <<< ToAttribute "iterations" <<< toAttr

x :: ∀ a. ToAttr Number a => a -> ChainableF
x = ForceT <<< ToAttribute "x" <<< toAttr

y :: ∀ a. ToAttr Number a => a -> ChainableF
y = ForceT <<< ToAttribute "y" <<< toAttr

distance :: ∀ a. ToAttr Number a => a -> ChainableF
distance = ForceT <<< ToAttribute "distance" <<< toAttr

index :: ∀ a. ToAttr Number a => a -> ChainableF -- TODO in fact this would be an Int correctly
index = ForceT <<< ToAttribute "distance" <<< toAttr

defaultIndex :: Datum_ -> Index_ -> Number
defaultIndex datum = d.index
  where
    d = (unsafeCoerce datum)



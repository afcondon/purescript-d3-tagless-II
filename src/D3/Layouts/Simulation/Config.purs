module D3.Simulation.Config where

import D3.Node

import D3.Attributes.Instances (Attribute)
import D3.Data.Types (Datum_, Index_)
import Data.Number (infinity)
import Prelude (negate)
import Unsafe.Coerce (unsafeCoerce)

foreign import data D3ForceHandle_     :: Type
foreign import data CustomForceConfig_ :: Type

data ChainableF =  AttrT Attribute
                | ForceT Attribute
      -- following are used in ChainableS but probably not here on Forces...delete when sure
                -- | TextT Attribute
                -- | TransitionT (Array ChainableS) Transition
                -- | RemoveT
                -- | OnT MouseEvent Listener_

-- | a record to initialize / configure simulations
type SimulationConfig_ = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
}

defaultForceManyConfig         :: String -> ForceManyConfig_
defaultForceManyConfig name = { 
    name
  , strength                   : (-30.0)
  , theta                      : 0.9
  , distanceMin                : 1.0
  , distanceMax                : infinity 
}

defaultForceCenterConfig       :: String -> ForceCenterConfig_
defaultForceCenterConfig name = { 
    name
  , cx                         : 0.0
  , cy                         : 0.0
  , strength                   : 1.0 
}

defaultForceCollideConfig      :: String -> (Datum_ -> Index_ -> Number) -> ForceCollideConfig_
defaultForceCollideConfig name radius = { 
    name
  , radius
  , strength                   : 1.0
  , iterations                 : 1.0 
}

defaultForceXConfig            :: String -> ForceXConfig_
defaultForceXConfig name = { 
    name
  , strength                   : 0.1
  , x                          : 0.0 
}

defaultForceYConfig            :: String -> ForceYConfig_
defaultForceYConfig name = { 
    name
  , strength                   : 0.1
  , y                          : 0.0 
}

defaultForceRadialConfig       :: String -> (Datum_ -> Index_ -> Number) -> ForceRadialConfig_
defaultForceRadialConfig name radius = { 
    name
  , strength                   : 0.1
  , radius
  , cx                         : 0.0
  , cy                         : 0.0 
}

-- TODO seems like we are forced to ignore the type parameter for the link here or else pollute all the force types
defaultForceLinkConfigEmpty    :: forall d. String -> (d -> Index_ -> Number) -> ForceLinkConfig_
defaultForceLinkConfigEmpty name id = { 
    name
  , links                      : []
  , strength                   : 1.0
  , distance                   : (\d i -> 30.0)
  , iterations                 : 1.0
  , id                         : unsafeCoerce id 
}

defaultForceLinkConfig         :: forall d r. String -> Array (D3_Link NodeID r) -> (d -> Index_ -> Number) -> ForceLinkConfig_
defaultForceLinkConfig name links id = { 
    name
  , links                      : unsafeCoerce links
  , strength                   : 1.0
  , distance                   : (\d i -> 30.0)
  , iterations                 : 1.0
  , id                         : unsafeCoerce id 
}
  


defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
}

type ForceManyConfig_ = { 
    name        :: String
  , strength    :: Number
  , theta       :: Number
  , distanceMin :: Number
  , distanceMax :: Number
}

type ForceCenterConfig_ = {
    name        :: String
  , strength    :: Number
  , cx          :: Number
  , cy          :: Number
}

type ForceCollideConfig_ = {
    name        :: String
  , strength    :: Number
  , radius      :: (Datum_ -> Index_ -> Number)
  , iterations  :: Number
}

type ForceXConfig_ = {
    name        :: String
  , strength    :: Number
  , x           :: Number
}

type ForceYConfig_ = {
    name        :: String
  , strength    :: Number
  , y           :: Number
}

type ForceRadialConfig_ = {
    name        :: String
  , radius      :: (Datum_ -> Index_ -> Number)
  , strength    :: Number
  , cx          :: Number
  , cy          :: Number
}

type ForceLinkConfig_ = {
    name        :: String
  , strength    :: Number
  , distance    :: (Datum_ -> Index_ -> Number)
  , iterations  :: Number
  , id          :: (Datum_ -> Index_ -> Number)
  , links       :: forall r. Array (D3_Link NodeID r)
}



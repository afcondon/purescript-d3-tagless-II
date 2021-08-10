module D3.Simulation.Config where

import D3.Attributes.Instances (class ToAttr, Attr(..), AttrBuilder(..), AttributeSetter(..), toAttr)
import D3.Data.Types (Datum_, Index_)
import D3.Simulation.Types (ChainableF(..))
import Data.Number (infinity)
import Prelude (negate, (<<<))

    
defaultForceRadialConfig       :: (Datum_ -> Index_ -> Number) -> Array ChainableF
defaultForceRadialConfig r =  
    [ radius r, strength 0.1, x 0.0, y 0.0 ]

defaultForceManyConfig         :: Array ChainableF
defaultForceManyConfig = 
  [ strength (-30.0), theta 0.9, distanceMin 1.0, distanceMax infinity ]

defaultForceCenterConfig       :: Array ChainableF
defaultForceCenterConfig = 
  [ x 0.0, y 0.0, strength 1.0 ]

defaultForceCollideConfig      :: (Datum_ -> Index_ -> Number) -> Array ChainableF
defaultForceCollideConfig r = 
  [ radius r, strength 1.0, iterations 1.0 ]

defaultForceXConfig            :: Array ChainableF
defaultForceXConfig = 
  [ strength 0.1, x 0.0 ]

defaultForceYConfig            :: Array ChainableF
defaultForceYConfig = 
  [ strength 0.1, y 0.0 ]
  
-- | ==================================================================================================
-- | ========================= sugar for the various attributes of forces =============================
-- | ==================================================================================================
radius :: ∀ a. ToAttr Number a => a -> ChainableF
radius = ForceT <<< AttributeSetter "radius" <<< toAttr

strength :: ∀ a. ToAttr Number a => a -> ChainableF
strength = ForceT <<< AttributeSetter "strength" <<< toAttr

-- cx :: ∀ a. ToAttr Number a => a -> ChainableF
-- cx = ForceT <<< AttributeSetter "cx" <<< toAttr

-- cy :: ∀ a. ToAttr Number a => a -> ChainableF
-- cy = ForceT <<< AttributeSetter "cy" <<< toAttr

theta :: ∀ a. ToAttr Number a => a -> ChainableF
theta = ForceT <<< AttributeSetter "theta" <<< toAttr

distanceMin :: ∀ a. ToAttr Number a => a -> ChainableF
distanceMin = ForceT <<< AttributeSetter "distanceMin" <<< toAttr

distanceMax :: ∀ a. ToAttr Number a => a -> ChainableF
distanceMax = ForceT <<< AttributeSetter "distanceMax" <<< toAttr

iterations :: ∀ a. ToAttr Number a => a -> ChainableF
iterations = ForceT <<< AttributeSetter "iterations" <<< toAttr

x :: ∀ a. ToAttr Number a => a -> ChainableF
x = ForceT <<< AttributeSetter "x" <<< toAttr

y :: ∀ a. ToAttr Number a => a -> ChainableF
y = ForceT <<< AttributeSetter "y" <<< toAttr

fx :: ∀ a. ToAttr Number a => a -> ChainableF
fx = ForceT <<< AttributeSetter "fx" <<< toAttr

fy :: ∀ a. ToAttr Number a => a -> ChainableF
fy = ForceT <<< AttributeSetter "fy" <<< toAttr

distance :: ∀ a. ToAttr Number a => a -> ChainableF
distance = ForceT <<< AttributeSetter "distance" <<< toAttr

index :: ∀ a. ToAttr Number a => a -> ChainableF -- TODO in fact this would be an Int correctly
index = ForceT <<< AttributeSetter "distance" <<< toAttr

-- these next two are for specifying how a link should swizzle its "id" to an object reference
intKey :: (Datum_ -> Number)  -> ChainableF
intKey = ForceT <<< AttributeSetter "keyFn" <<< NumberAttr <<< Fn

stringKey :: (Datum_ -> String) -> ChainableF
stringKey = ForceT <<< AttributeSetter "keyFn" <<< StringAttr <<< Fn


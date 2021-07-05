module D3.Simulation.Config where

import D3.Attributes.Instances (class ToAttr, Attribute(..), toAttr)
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
radius = ForceT <<< ToAttribute "radius" <<< toAttr

strength :: ∀ a. ToAttr Number a => a -> ChainableF
strength = ForceT <<< ToAttribute "strength" <<< toAttr

-- cx :: ∀ a. ToAttr Number a => a -> ChainableF
-- cx = ForceT <<< ToAttribute "cx" <<< toAttr

-- cy :: ∀ a. ToAttr Number a => a -> ChainableF
-- cy = ForceT <<< ToAttribute "cy" <<< toAttr

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

fx :: ∀ a. ToAttr Number a => a -> ChainableF
fx = ForceT <<< ToAttribute "fx" <<< toAttr

fy :: ∀ a. ToAttr Number a => a -> ChainableF
fy = ForceT <<< ToAttribute "fy" <<< toAttr

distance :: ∀ a. ToAttr Number a => a -> ChainableF
distance = ForceT <<< ToAttribute "distance" <<< toAttr

index :: ∀ a. ToAttr Number a => a -> ChainableF -- TODO in fact this would be an Int correctly
index = ForceT <<< ToAttribute "distance" <<< toAttr



module PSD3.Internal.Simulation.Config where

import PSD3.Internal.Attributes.Instances (class ToAttr, Attr(..), AttrBuilder(..), AttributeSetter(..), toAttr)
import PSD3.Internal.Types (Datum_, Index_)
import PSD3.Internal.Simulation.Types (ChainableF(..))
import Data.Number (infinity)
import Prelude (Unit, negate, ($), (<<<))
import Unsafe.Coerce (unsafeCoerce)


defaultForceRadialConfig       :: forall d. (Datum_ -> Index_ -> Number) -> Array (ChainableF d)
defaultForceRadialConfig r =
    [ radiusFn r, strengthVal 0.1, xVal 0.0, yVal 0.0 ]

defaultForceManyConfig         :: forall d. Array (ChainableF d)
defaultForceManyConfig =
  [ strengthVal (-30.0), thetaVal 0.9, distanceMinVal 1.0, distanceMaxVal infinity ]

defaultForceCenterConfig       :: forall d. Array (ChainableF d)
defaultForceCenterConfig =
  [ xVal 0.0, yVal 0.0, strengthVal 1.0 ]

defaultForceCollideConfig      :: forall d. (Datum_ -> Index_ -> Number) -> Array (ChainableF d)
defaultForceCollideConfig r =
  [ radiusFn r, strengthVal 1.0, iterationsVal 1.0 ]

defaultForceXConfig            :: forall d. Array (ChainableF d)
defaultForceXConfig =
  [ strengthVal 0.1, xVal 0.0 ]

defaultForceYConfig            :: forall d. Array (ChainableF d)
defaultForceYConfig =
  [ strengthVal 0.1, yVal 0.0 ]

-- | ==================================================================================================
-- | ========================= sugar for the various attributes of forces =============================
-- | ==================================================================================================
-- Note: Force attributes work with Datum_ at runtime
-- We bypass the ToAttr typeclass and construct Attr directly using unsafeCoerce

radiusFn :: forall d. (Datum_ -> Index_ -> Number) -> ChainableF d
radiusFn fn = ForceT $ AttributeSetter "radius" $ NumberAttr $ FnI (unsafeCoerce fn)

radiusVal :: forall d. Number -> ChainableF d
radiusVal n = ForceT $ AttributeSetter "radius" $ NumberAttr $ Static n

strengthVal :: forall d. Number -> ChainableF d
strengthVal n = ForceT $ AttributeSetter "strength" $ NumberAttr $ Static n

thetaVal :: forall d. Number -> ChainableF d
thetaVal n = ForceT $ AttributeSetter "theta" $ NumberAttr $ Static n

distanceMinVal :: forall d. Number -> ChainableF d
distanceMinVal n = ForceT $ AttributeSetter "distanceMin" $ NumberAttr $ Static n

distanceMaxVal :: forall d. Number -> ChainableF d
distanceMaxVal n = ForceT $ AttributeSetter "distanceMax" $ NumberAttr $ Static n

iterationsVal :: forall d. Number -> ChainableF d
iterationsVal n = ForceT $ AttributeSetter "iterations" $ NumberAttr $ Static n

xVal :: forall d. Number -> ChainableF d
xVal n = ForceT $ AttributeSetter "x" $ NumberAttr $ Static n

yVal :: forall d. Number -> ChainableF d
yVal n = ForceT $ AttributeSetter "y" $ NumberAttr $ Static n

fxVal :: forall d. Number -> ChainableF d
fxVal n = ForceT $ AttributeSetter "fx" $ NumberAttr $ Static n

fyVal :: forall d. Number -> ChainableF d
fyVal n = ForceT $ AttributeSetter "fy" $ NumberAttr $ Static n

distanceVal :: forall d. Number -> ChainableF d
distanceVal n = ForceT $ AttributeSetter "distance" $ NumberAttr $ Static n

-- these next two are for specifying how a link should swizzle its "id" to an object reference
-- Note: These functions work with Datum_ at runtime
numKey :: forall d. (Datum_ -> Number)  -> ChainableF d
numKey fn = ForceT $ AttributeSetter "keyFn" $ NumberAttr $ Fn (unsafeCoerce fn)

stringKey :: forall d. (Datum_ -> String) -> ChainableF d
stringKey fn = ForceT $ AttributeSetter "keyFn" $ StringAttr $ Fn (unsafeCoerce fn)


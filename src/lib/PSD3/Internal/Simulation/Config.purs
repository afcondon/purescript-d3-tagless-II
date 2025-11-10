module PSD3.Internal.Simulation.Config where

import PSD3.Internal.Attributes.Instances (class ToAttr, Attr(..), AttrBuilder(..), AttributeSetter(..), toAttr)
import PSD3.Internal.Types (Datum_, Index_)
import PSD3.Internal.Simulation.Types (ChainableF(..))
import Data.Number (infinity)
import Prelude (Unit, negate, ($), (<<<))
import Unsafe.Coerce (unsafeCoerce)


defaultForceRadialConfig       :: (Datum_ -> Index_ -> Number) -> Array (ChainableF Unit)
defaultForceRadialConfig r =
    [ radiusFn r, strengthVal 0.1, xVal 0.0, yVal 0.0 ]

defaultForceManyConfig         :: Array (ChainableF Unit)
defaultForceManyConfig =
  [ strengthVal (-30.0), thetaVal 0.9, distanceMinVal 1.0, distanceMaxVal infinity ]

defaultForceCenterConfig       :: Array (ChainableF Unit)
defaultForceCenterConfig =
  [ xVal 0.0, yVal 0.0, strengthVal 1.0 ]

defaultForceCollideConfig      :: (Datum_ -> Index_ -> Number) -> Array (ChainableF Unit)
defaultForceCollideConfig r =
  [ radiusFn r, strengthVal 1.0, iterationsVal 1.0 ]

defaultForceXConfig            :: Array (ChainableF Unit)
defaultForceXConfig =
  [ strengthVal 0.1, xVal 0.0 ]

defaultForceYConfig            :: Array (ChainableF Unit)
defaultForceYConfig =
  [ strengthVal 0.1, yVal 0.0 ]

-- | ==================================================================================================
-- | ========================= sugar for the various attributes of forces =============================
-- | ==================================================================================================
-- Note: Force attributes work with Datum_ at runtime but use Unit as phantom type
-- We bypass the ToAttr typeclass and construct Attr directly using unsafeCoerce

radiusFn :: (Datum_ -> Index_ -> Number) -> ChainableF Unit
radiusFn fn = ForceT $ AttributeSetter "radius" $ NumberAttr $ FnI (unsafeCoerce fn)

radiusVal :: Number -> ChainableF Unit
radiusVal n = ForceT $ AttributeSetter "radius" $ NumberAttr $ Static n

strengthVal :: Number -> ChainableF Unit
strengthVal n = ForceT $ AttributeSetter "strength" $ NumberAttr $ Static n

thetaVal :: Number -> ChainableF Unit
thetaVal n = ForceT $ AttributeSetter "theta" $ NumberAttr $ Static n

distanceMinVal :: Number -> ChainableF Unit
distanceMinVal n = ForceT $ AttributeSetter "distanceMin" $ NumberAttr $ Static n

distanceMaxVal :: Number -> ChainableF Unit
distanceMaxVal n = ForceT $ AttributeSetter "distanceMax" $ NumberAttr $ Static n

iterationsVal :: Number -> ChainableF Unit
iterationsVal n = ForceT $ AttributeSetter "iterations" $ NumberAttr $ Static n

xVal :: Number -> ChainableF Unit
xVal n = ForceT $ AttributeSetter "x" $ NumberAttr $ Static n

yVal :: Number -> ChainableF Unit
yVal n = ForceT $ AttributeSetter "y" $ NumberAttr $ Static n

fxVal :: Number -> ChainableF Unit
fxVal n = ForceT $ AttributeSetter "fx" $ NumberAttr $ Static n

fyVal :: Number -> ChainableF Unit
fyVal n = ForceT $ AttributeSetter "fy" $ NumberAttr $ Static n

distanceVal :: Number -> ChainableF Unit
distanceVal n = ForceT $ AttributeSetter "distance" $ NumberAttr $ Static n

-- these next two are for specifying how a link should swizzle its "id" to an object reference
-- Note: These functions work with Datum_ at runtime, but ChainableF uses Unit as phantom type
-- since forces don't participate in typed phantom tracking
numKey :: (Datum_ -> Number)  -> ChainableF Unit
numKey fn = ForceT $ AttributeSetter "keyFn" $ NumberAttr $ Fn (unsafeCoerce fn)

stringKey :: (Datum_ -> String) -> ChainableF Unit
stringKey fn = ForceT $ AttributeSetter "keyFn" $ StringAttr $ Fn (unsafeCoerce fn)


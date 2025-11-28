module PSD3.Internal.Attributes.Instances where

import Prelude

import PSD3.Internal.Types (D3This_, Datum_, Index_)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn3)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (Event)

-- | Newtype wrapper for Datum_ accessor functions
-- | Use this when you have a function of type (Datum_ -> a) that needs to work
-- | with any phantom-typed selection
-- | Example: x (DatumFn \d -> if treeDatum_.hasChildren d then 8.0 else (-8.0))
newtype DatumFn a = DatumFn (Datum_ -> a)

-- | Unwrap a DatumFn to get the underlying Datum_ accessor function
-- | Useful when you need to pass accessor functions to functions that expect raw (Datum_ -> a)
-- | Example: transform [ unwrapDatumFn (DatumFn positionXY) ]
unwrapDatumFn :: forall a. DatumFn a -> (Datum_ -> a)
unwrapDatumFn (DatumFn fn) = fn

-- | Newtype wrapper for indexed Datum_ accessor functions
-- | Use this when you have a function of type (Datum_ -> Index_ -> a) that needs to work
-- | with any phantom-typed selection
-- | Example: cx (DatumFnI \_ i -> index_ToNumber i * 20.0)
newtype DatumFnI a = DatumFnI (Datum_ -> Index_ -> a)

-- | Some useful type aliases
type IndexedLambda a = Fn2 Datum_ Index_ a
type Listener           = (Event -> Datum_ -> D3This_ -> Unit) 
type Listener_          = Fn3 Event Datum_ D3This_ Unit 
type EffectfulListener  = (Event -> Datum_ -> D3This_ -> Effect Unit) 
type EffectfulListener_ = EffectFn3 Event Datum_ D3This_ Unit 
type Label           = String

-- | Central feature of this module,
data AttributeSetter d = AttributeSetter Label (Attr d)

attributeLabel :: forall d. AttributeSetter d -> String
attributeLabel (AttributeSetter label _) = label

attributeAttr :: forall d. AttributeSetter d -> Attr d
attributeAttr (AttributeSetter _ a) = a

data AttrBuilder a d =
    Static a
  | Fn (d -> a)
  | FnI (d -> Index_ -> a) 

data Attr d =
    StringAttr (AttrBuilder String d)
  | NumberAttr (AttrBuilder Number d)
  | ArrayAttr  (AttrBuilder (Array Number) d)

-- Kind annotation to avoid "fun" with polykinds.
class ToAttr :: Type -> Type -> Type -> Constraint
-- | typeclass to enable polymorphic forms of the attribute setter
class ToAttr to from d | from -> to  where
  toAttr :: from -> Attr d

-- | we only unbox the attr at the point where we ship it over the FFI to JavaScript
-- | the JavaScript API (D3) is polymorphic in this sense, can accept any of AttrBuilder forms
-- | NOTE: Coerces typed functions (d -> a) to untyped (Datum_ -> a) for FFI boundary
unboxAttr :: ∀ a d. Attr d -> a
unboxAttr =
  case _ of
    (StringAttr (Static a)) -> unsafeCoerce a
    (StringAttr (Fn a))     -> unsafeCoerce a  -- Coerces (d -> a) to (Datum_ -> a)
    (StringAttr (FnI a))    -> unsafeCoerce (mkFn2 a)  -- Coerces and wraps

    (NumberAttr (Static a)) -> unsafeCoerce a
    (NumberAttr (Fn a))     -> unsafeCoerce a  -- Coerces (d -> a) to (Datum_ -> a)
    (NumberAttr (FnI a))    -> unsafeCoerce (mkFn2 a)  -- Coerces and wraps

    (ArrayAttr (Static a))  -> unsafeCoerce a
    (ArrayAttr (Fn a))      -> unsafeCoerce a  -- Coerces (d -> a) to (Datum_ -> a)
    (ArrayAttr (FnI a))     -> unsafeCoerce (mkFn2 a)  -- Coerces and wraps

-- | because the text attribute can only be String, it has only Static|Fn|FnI forms
unboxText :: ∀ a d. AttrBuilder String d -> a
unboxText =
  case _ of
    (Static a)   -> unsafeCoerce a
    (Fn a)       -> unsafeCoerce a  -- Coerces (d -> String) to (Datum_ -> String)
    (FnI a)      -> unsafeCoerce (mkFn2 a)  -- Coerces and wraps

-- | Instances for the 9 combinations of attributeSetters we need
-- | ie (Static, Fn, FnI) * (String, Number, Array Number)
-- | Now with typed datum parameter d!
instance toAttrString :: ToAttr String String d where
  toAttr = StringAttr <<< Static
instance toAttrStringFn :: ToAttr String (d -> String) d where
  toAttr = StringAttr <<< Fn
instance toAttrStringFnI :: ToAttr String (d -> Index_ -> String) d where
  toAttr = StringAttr <<< FnI

instance toAttrNumber :: ToAttr Number Number d where
  toAttr = NumberAttr <<< Static
instance toAttrNumberFn :: ToAttr Number (d -> Number) d where
  toAttr = NumberAttr <<< Fn
instance toAttrNumberFnI :: ToAttr Number (d -> Index_ -> Number) d where
  toAttr = NumberAttr <<< FnI

instance toAttrArray :: ToAttr (Array Number) (Array Number) d where
  toAttr = ArrayAttr <<< Static
instance toAttrArrayFn :: ToAttr (Array Number) (d -> Array Number) d where
  toAttr = ArrayAttr <<< Fn
instance toAttrArrayFnI :: ToAttr (Array Number) (d -> Index_ -> Array Number) d where
  toAttr = ArrayAttr <<< FnI

-- | ToAttr instances for DatumFn wrapper
-- | These allow Datum_ accessor functions to work with any phantom-typed selection
-- | The unsafeCoerce is safe because phantom types exist only at compile time
instance toAttrStringDatumFn :: ToAttr String (DatumFn String) d where
  toAttr (DatumFn fn) = StringAttr $ Fn (unsafeCoerce fn)

instance toAttrNumberDatumFn :: ToAttr Number (DatumFn Number) d where
  toAttr (DatumFn fn) = NumberAttr $ Fn (unsafeCoerce fn)

instance toAttrArrayDatumFn :: ToAttr (Array Number) (DatumFn (Array Number)) d where
  toAttr (DatumFn fn) = ArrayAttr $ Fn (unsafeCoerce fn)

-- | ToAttr instances for DatumFnI wrapper (indexed functions)
-- | These allow indexed Datum_ accessor functions to work with any phantom-typed selection
instance toAttrStringDatumFnI :: ToAttr String (DatumFnI String) d where
  toAttr (DatumFnI fn) = StringAttr $ FnI (unsafeCoerce fn)

instance toAttrNumberDatumFnI :: ToAttr Number (DatumFnI Number) d where
  toAttr (DatumFnI fn) = NumberAttr $ FnI (unsafeCoerce fn)

instance toAttrArrayDatumFnI :: ToAttr (Array Number) (DatumFnI (Array Number)) d where
  toAttr (DatumFnI fn) = ArrayAttr $ FnI (unsafeCoerce fn)

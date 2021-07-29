module D3.Attributes.Instances where

import Prelude

import D3.Data.Types (D3This_, Datum_, Index_, UnitType)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (Event)

-- | Some useful type aliases
type IndexedLambda a = Fn2 Datum_ Index_ a
type Listener        = (Event -> Datum_ -> D3This_ -> Unit) 
type Listener_       = Fn3 Event Datum_ D3This_ Unit 
type Label           = String

-- | Central feature of this module, 
data AttributeSetter = AttributeSetter Label Attr

attributeLabel :: AttributeSetter -> String
attributeLabel (AttributeSetter label _) = label

attributeAttr :: AttributeSetter -> Attr
attributeAttr (AttributeSetter _ a) = a

data AttrBuilder a =
    Static a
  | Fn (Datum_ -> a)
  | FnI (IndexedLambda a)

data Attr = 
    StringAttr (AttrBuilder String)
  | NumberAttr (AttrBuilder Number)
  | ArrayAttr  (AttrBuilder (Array Number))

-- Kind annotation to avoid "fun" with polykinds.
class ToAttr :: Type -> Type -> Constraint
-- | typeclass to enable polymorphic forms of the attribute setter
class ToAttr to from | from -> to  where
  toAttr :: from -> Attr

-- | we only unbox the attr at the point where we ship it over the FFI to JavaScript
-- | the JavaScript API (D3) is polymorphic in this sense, can accept any of AttrBuilder forms
unboxAttr :: ∀ a. Attr -> a
unboxAttr = 
  case _ of
    (StringAttr (Static a)) -> unsafeCoerce a
    (StringAttr (Fn a))     -> unsafeCoerce a
    (StringAttr (FnI a))    -> unsafeCoerce a

    (NumberAttr (Static a)) -> unsafeCoerce a
    (NumberAttr (Fn a))     -> unsafeCoerce a
    (NumberAttr (FnI a))    -> unsafeCoerce a

    (ArrayAttr (Static a))  -> unsafeCoerce a
    (ArrayAttr (Fn a))      -> unsafeCoerce a
    (ArrayAttr (FnI a))     -> unsafeCoerce a

-- | because the text attribute can only be String, it has only Static|Fn|FnI forms
unboxText :: ∀ a. AttrBuilder String -> a
unboxText = 
  case _ of
    (Static a)   -> unsafeCoerce a
    (Fn a)       -> unsafeCoerce a
    (FnI a)      -> unsafeCoerce a

-- | Instances for the 9 combinations of attributeSetters we need
-- | ie (Static, Fn, FnI) * (String, Number, Array Number)
instance toAttrString :: ToAttr String String where
  toAttr = StringAttr <<< Static
instance toAttrStringFn :: ToAttr String (Datum_ -> String) where
  toAttr = StringAttr <<< Fn
instance toAttrStringFnI :: ToAttr String (Datum_ -> Index_ -> String) where
  toAttr = StringAttr <<< FnI <<< mkFn2

instance toAttrNumber :: ToAttr Number Number where
  toAttr = NumberAttr <<< Static
instance toAttrNumberFn :: ToAttr Number (Datum_ -> Number) where
  toAttr = NumberAttr <<< Fn
instance toAttrNumberFnI :: ToAttr Number (Datum_ -> Index_ -> Number) where
  toAttr = NumberAttr <<< FnI <<< mkFn2

instance toAttrArray :: ToAttr (Array Number) (Array Number) where
  toAttr = ArrayAttr <<< Static
instance toAttrArrayFn :: ToAttr (Array Number) (Datum_ -> Array Number) where
  toAttr = ArrayAttr <<< Fn
instance toAttrArrayFnI :: ToAttr (Array Number) (Datum_ -> Index_ -> Array Number) where
  toAttr = ArrayAttr <<< FnI <<< mkFn2

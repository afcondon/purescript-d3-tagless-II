module D3.Attributes.Instances where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, mkFn2)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (Event)
import D3.Data.Types


type IndexedLambda a = Fn2 Datum_ Index_ a
type Listener        = (Event -> Datum_ -> D3This_ -> Unit) 
type Listener_       = Fn3 Event Datum_ D3This_ Unit 
type Label           = String

newtype NWU = NWU { i :: Int, u :: UnitType} -- scope here for adding show function to remove constraints elsewhere
instance showNWU :: Show NWU where
  show (NWU n) = show n.i <> show n.u

data AttrBuilder a =
    Static a
  | Fn (Datum_ -> a)
  | FnI (IndexedLambda a)

data Attr = 
    StringAttr (AttrBuilder String)
  | NumberAttr (AttrBuilder Number)
  | NWUAttr    (AttrBuilder NWU)
  | ArrayAttr  (AttrBuilder (Array Number))

data Attribute = ToAttribute Label Attr

attrLabel :: Attribute -> String
attrLabel (ToAttribute label _) = label


unbox :: ∀ a. Attr -> a
unbox = 
  case _ of
    (StringAttr (Static a)) -> unsafeCoerce a
    (StringAttr (Fn a))     -> unsafeCoerce a
    (StringAttr (FnI a))    -> unsafeCoerce a

    (NumberAttr (Static a)) -> unsafeCoerce a
    (NumberAttr (Fn a))     -> unsafeCoerce a
    (NumberAttr (FnI a))    -> unsafeCoerce a

    (NWUAttr (Static a))    -> unsafeCoerce "NWU-static" -- a
    (NWUAttr (Fn a))        -> unsafeCoerce (\d -> "NWU-lambda") -- a
    (NWUAttr (FnI a))       -> unsafeCoerce $ mkFn2 (\d i -> "NWU-di") -- a

    (ArrayAttr (Static a))  -> unsafeCoerce a
    (ArrayAttr (Fn a))      -> unsafeCoerce a
    (ArrayAttr (FnI a))     -> unsafeCoerce a


unboxText :: ∀ a. AttrBuilder String -> a
unboxText = 
  case _ of
    (Static a)   -> unsafeCoerce a
    (Fn a)       -> unsafeCoerce a
    (FnI a)      -> unsafeCoerce a

-- Kind annotation to avoid "fun" with polykinds.
class ToAttr :: Type -> Type -> Constraint
class ToAttr to from | from -> to  where
  toAttr :: from -> Attr

-- Boilerplate, boilerplate, boilerplate...
-- Might be a better way to do these.

instance toAttrNWU :: ToAttr NWU NWU where
  toAttr = NWUAttr <<< Static

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

-- common coercions
datumIsChar :: Datum_ -> Char
datumIsChar = unsafeCoerce

datumIsNumber :: Datum_ -> Number
datumIsNumber = unsafeCoerce

datumIsString :: Datum_ -> String
datumIsString = unsafeCoerce

indexIsNumber :: Index_ -> Number
indexIsNumber = unsafeCoerce

indexIsString :: Index_ -> String
indexIsString = unsafeCoerce

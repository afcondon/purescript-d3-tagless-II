module D3.Attributes.Instances where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Unsafe.Coerce (unsafeCoerce)


foreign import data Datum :: Type
foreign import data Index :: Type

type Label = String

-- TODO find a way to get units back in without making DSL hideous
data UnitType = Px | Pt | Em | Rem | Percent

data Attribute = Attribute Label Attr

data Attr = StringAttr (Attrib String)
          | NumberAttr (Attrib Number)
          | ArrayAttr (Attrib (Array Number))

unbox :: ∀ a. Attr -> a
unbox = 
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

unboxText :: ∀ a. Attrib String -> a
unboxText = 
  case _ of
    (Static a) -> unsafeCoerce a
    (Fn a)     -> unsafeCoerce a
    (FnI a)    -> unsafeCoerce a

type IndexedLambda a = Fn2 Datum Index a

data Attrib a = Static a
              | Fn (Datum -> a)
              | FnI (IndexedLambda a)


-- Kind annotation to avoid "fun" with polykinds.
class ToAttr :: Type -> Type -> Constraint
class ToAttr to from | from -> to  where
  toAttr :: from -> Attr

-- Boilerplate, boilerplate, boilerplate...
-- Might be a better way to do these.

instance toAttrString :: ToAttr String String where
  toAttr = StringAttr <<< Static

instance toAttrStringFn :: ToAttr String (Datum -> String) where
  toAttr = StringAttr <<< Fn

instance toAttrStringFnI :: ToAttr String (Datum -> Index -> String) where
  toAttr = StringAttr <<< FnI <<< mkFn2

instance toAttrNumber :: ToAttr Number Number where
  toAttr = NumberAttr <<< Static

instance toAttrNumberFn :: ToAttr Number (Datum -> Number) where
  toAttr = NumberAttr <<< Fn

instance toAttrNumberFnI :: ToAttr Number (Datum -> Index -> Number) where
  toAttr = NumberAttr <<< FnI <<< mkFn2

instance toAttrArray :: ToAttr (Array Number) (Array Number) where
  toAttr = ArrayAttr <<< Static

instance toAttrArrayFn :: ToAttr (Array Number) (Datum -> Array Number) where
  toAttr = ArrayAttr <<< Fn

instance toAttrArrayFnI :: ToAttr (Array Number) (Datum -> Index -> Array Number) where
  toAttr = ArrayAttr <<< FnI <<< mkFn2


-- common coercions
datumIsChar :: Datum -> Char
datumIsChar = unsafeCoerce

datumIsNumber :: Datum -> Number
datumIsNumber = unsafeCoerce

datumIsString :: Datum -> String
datumIsString = unsafeCoerce

indexIsNumber :: Index -> Number
indexIsNumber = unsafeCoerce

indexIsString :: Index -> String
indexIsString = unsafeCoerce


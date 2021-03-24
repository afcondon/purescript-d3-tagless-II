module Attributes.Instances where

import Prelude


foreign import data Datum :: Type

type Label = String

data UnitType = Px | Pt | Em | Rem | Percent

data Attribute = Attribute Label Attr

data Attr = StringAttr (Attrib String)
          | NumberAttr (Attrib Number)
          | ArrayAttr (Attrib (Array Number))

data Attrib a = Static a
              | Fn (Datum -> a)
              | FnI (Datum -> Int -> a)


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

instance toAttrStringFnI :: ToAttr String (Datum -> Int -> String) where
  toAttr = StringAttr <<< FnI

instance toAttrNumber :: ToAttr Number Number where
  toAttr = NumberAttr <<< Static

instance toAttrNumberFn :: ToAttr Number (Datum -> Number) where
  toAttr = NumberAttr <<< Fn

instance toAttrNumberFnI :: ToAttr Number (Datum -> Int -> Number) where
  toAttr = NumberAttr <<< FnI

instance toAttrArray :: ToAttr (Array Number) (Array Number) where
  toAttr = ArrayAttr <<< Static

instance toAttrArrayFn :: ToAttr (Array Number) (Datum -> Array Number) where
  toAttr = ArrayAttr <<< Fn

instance toAttrArrayFnI :: ToAttr (Array Number) (Datum -> Int -> Array Number) where
  toAttr = ArrayAttr <<< FnI
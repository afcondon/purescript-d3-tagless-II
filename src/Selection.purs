module Selection where

import Attributes.Instances (Attribute)
import Data.Foldable (class Foldable)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Submodel :: Type

data Selection model =
    Hook String (Selection model)
  | Append String (Array Attribute) (Array (Selection model))
  | Join String (JoinData model Submodel)
  | Join' String (JoinData' model Submodel)
  | End

type JoinData  model b =
  model -> Tuple b (Selection b)

type JoinData' model b = forall f. 
  (Foldable f) => 
  model -> Tuple (f b) (Selection b)

mkJoin :: forall a b. JoinData a b -> JoinData a Submodel
mkJoin = unsafeCoerce


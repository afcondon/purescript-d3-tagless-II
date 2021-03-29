module D3.Examples.GUP where

import D3.Attributes.Sugar
import Prelude

import D3.Attributes.Instances (Datum, Attributes)
import D3.Interpreter.Tagless (class D3Tagless, append, hook, join, model)
import D3.Selection (D3Selection, D3State, D3_Node(..), Element(..), node__)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- simple attributes don't use data, no hassle
svgAttributes :: Attributes
svgAttributes = [
    strokeColor "green"
  , strokeOpacity 0.75
]

-- attributes that use (\d -> <something>) or (\d i -> <something>) need type hints
-- and coercion functions
type Model = Array Char
coerceToChar :: Datum -> Char
coerceToChar = unsafeCoerce

_char :: Proxy "char"
_char = Proxy

_string :: Proxy "string"
_string = Proxy

_int :: Proxy "int"
_int = Proxy


circleAttributes :: forall a. Proxy a -> Attributes
circleAttributes proxy = [
    strokeColor "green"
  , strokeOpacity 0.75
  , strokeWidth $ toNumber <<< (_ - 97) <<< toCharCode <<< coerceToChar
]

script :: ∀ m. (D3Tagless m) => m D3Selection
script = do
  _    <- model [ 'a', 'b', 'c', 'd' ]

  root <- hook "div#root"
  
  svg  <- append $ D3_Node Svg svgAttributes [ D3_Node Group [] [ node__ Circle ] ]

  _    <- join Circle { enter: [ Tuple (circleAttributes _char) Nothing ], update: [], exit: [] }

  pure svg
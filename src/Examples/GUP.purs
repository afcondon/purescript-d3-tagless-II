module D3.Examples.GUP where

import Prelude

import D3.Attributes.Instances (Datum, Attributes)
import D3.Attributes.Sugar (strokeColor, strokeOpacity, strokeWidth)
import D3.Interpreter.Tagless (class D3Tagless, append, hook, join, model)
import D3.Selection (D3Selection, D3_Node(..), Element(..), TransitionStage(..), node__)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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

circleAttributes :: (Datum -> Char) -> Attributes
circleAttributes f = [
    strokeColor "blue"
  , strokeOpacity 0.25
  , strokeWidth $ toNumber <<< (_ * 3) <<< (_ - 97) <<< toCharCode <<< f
]

script :: ∀ m. (D3Tagless m) => m D3Selection
script = do
  _    <- model [ 'a', 'b', 'c', 'd' ]

  root <- hook "div#root"
  
  svg     <- append $ D3_Node Svg svgAttributes []
  circles <- append $ node__ Group
  -- now the active selection is "circles" and these circles that are joining will be inside it
  _    <- join Circle { enter: [ OnlyAttrs (circleAttributes coerceToChar)  ], update: [], exit: [] }

  pure svg
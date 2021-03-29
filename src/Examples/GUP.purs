module D3.Examples.GUP where

import Prelude

import D3.Attributes.Instances (Datum, Attributes)
import D3.Attributes.Sugar
import D3.Interpreter.Tagless (class D3Tagless, append, hook, join, model)
import D3.Selection (D3Selection, D3_Node(..), EasingFunction(..), Element(..), Milliseconds, Transition, TransitionStage(..), node__)
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

d_Char :: Datum -> Char
d_Char = unsafeCoerce

enterAttributes :: (Datum -> Char) -> Attributes
enterAttributes f = [
    classed "enter1"
  , strokeColor "blue"
  , strokeOpacity 0.25
  , strokeWidth $ toNumber <<< (_ * 3) <<< (_ - 97) <<< toCharCode <<< f
  , r 10.0
  , cx $ ((\d i -> (toNumber i) * 16.0) :: Datum -> Int -> Number) -- TODO this is obviously unacceptably clunky
  , cy 25.0
]

t :: Transition
t = { name: "", delay: 2000, duration: 5000, easing: DefaultCubic }

enterAttributes2 :: (Datum -> Char) -> Attributes
enterAttributes2 f = [
    strokeColor "red"
  , strokeOpacity 0.85
  , strokeWidth $ toNumber <<< (_ * 5) <<< (_ - 97) <<< toCharCode <<< f
]

updateAttributes1 = [ classed "update1", strokeColor "green" ]
-- updateAttributes2 :: (Datum -> Char) -> Attributes
-- updateAttributes2 f = [ strokeColor "purple"
--                       , cx $ (\d i -> i * 16 ) :: Datum -> Int -> Number ]


script :: ∀ m. (D3Tagless m) => m D3Selection -- TODO we can actually return much more structured output, selection tree etc
script = do
  _    <- model [ 'a', 'b', 'c', 'd' ]

  root <- hook "div#root"
  
  svg     <- append $ D3_Node Svg svgAttributes []
  circles <- append $ node__ Group
  -- now the active selection is "circles" and these circles that are joining will be inside it
  _    <- join Circle { enter: [ AttrsAndTransition (enterAttributes d_Char) t
                               , OnlyAttrs (enterAttributes2 d_Char) ]
                      , update: [ AttrsAndTransition updateAttributes1 t
                      ]
                                -- , OnlyAttrs (updateAttributes2 d_Char) ]
                      , exit: [] }

  pure svg
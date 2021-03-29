module D3.Examples.GUP where

import D3.Attributes.Sugar

import D3.Attributes.Instances (Datum, Attributes)
import D3.Interpreter.Tagless (class D3Tagless, append, hook, join, model)
import D3.Selection (D3Selection, D3State, D3_Node(..), Element(..), node__)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Prelude 
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- WIP all the attr functions are written in terms of Datum which is opaque, but we know at compile-time what they 
-- really are so if we coerce the function AFTER we've type checked it against the type we know it will really be
-- we should be able to have polymorphism AND type-checking
type CharDatum = Char
_CharDatum :: Proxy CharDatum
_CharDatum = Proxy

someAttributes :: Proxy CharDatum -> Attributes
someAttributes proxy = [
    strokeColor "green"
  , strokeOpacity 0.75
  , strokeWidth (strokeWidthFn proxy )
]

strokeWidthFn :: Proxy CharDatum -> Datum -> Number
strokeWidthFn proxy d = toNumber $ 
                        (toCharCode
                        (unsafeCoerce d :: CharDatum)) - 97 


type Model = Array Char

script :: ∀ m. (D3Tagless m) => m D3Selection
script = do
  _    <- model [ 'a', 'b', 'c', 'd' ]

  root <- hook "div#root"
  
  svg  <- append $ D3_Node Svg (someAttributes _CharDatum) [ D3_Node Group [] [ node__ Circle ] ]

  _    <- join Circle { enter: [], update: [], exit: [] }

  pure svg
module D3.Examples.GUP where

import D3.Attributes.Sugar

import D3.Attributes.Instances (Datum, Attributes)
import D3.Interpreter.Tagless (class D3Tagless, append, hook, join)
import D3.Selection (D3Selection, D3State, D3_Node(..), Element(..), node__)
import Prelude (bind, pure, ($))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)



type SomeDatum = forall r. { fillColorField :: String | r }
_SomeDatum :: Proxy SomeDatum
_SomeDatum = Proxy

coerceFromSomeDatum :: (SomeDatum -> String) -> (Datum -> String)
coerceFromSomeDatum= unsafeCoerce

type SomeOtherDatum = forall r. { snek :: String | r }
_SomeOtherDatum :: Proxy SomeOtherDatum
_SomeOtherDatum = Proxy

-- WIP all the attr functions are written in terms of Datum which is opaque, but we know at compile-time what they 
-- really are so if we coerce the function AFTER we've type checked it against the type we know it will really be
-- we should be able to have polymorphism AND type-checking
someAttributes :: Proxy SomeDatum -> Attributes
someAttributes _ = [
    strokeColor "green"
  , strokeOpacity 0.75
  , fill $ coerceFromSomeDatum (\d -> d.fillColorField)
]

script :: ∀ m. (D3Tagless m) => m D3Selection
script = do
  root <- hook "div#root"
  
  svg <- append $ D3_Node Svg (someAttributes _SomeDatum ) [ D3_Node Group [] [ node__ Circle ] ]

  _ <- join Circle { enter: [], update: [], exit: [] }

  pure svg
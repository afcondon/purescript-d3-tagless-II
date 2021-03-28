module D3.Examples.GUP where

import Prelude (bind, pure, ($))
import D3.Selection (D3Selection, Element(..), Node(..), node__, nullD3Selection) 
import D3.Interpreter.Tagless (class D3Tagless, _SomeDatum, append, hook, join, someAttributes)



script :: ∀ m. (D3Tagless m) => m D3Selection
script = do
    root <- hook "div#root"
    
    svg <- append $ Node Svg (someAttributes _SomeDatum ) [ Node Group [] [ node__ Circle ] ]

    _ <- join Circle { enter: [], update: [], exit: [] }

    pure nullD3Selection
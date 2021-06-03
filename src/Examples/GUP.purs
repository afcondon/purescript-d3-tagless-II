module D3.Examples.GUP where

import D3.Attributes.Sugar

import D3.Attributes.Instances (datumIsChar, indexIsNumber)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Selection (Join(..), Keys(..), node, node_)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Prelude (class Bind, Unit, bind, discard, pure, unit, ($), (*), (*>), (+), (<<<))

-- | ====================================================================================
-- | Simple-as-can-be example of the more complex Join which allows for new data to be
-- | entered, existing data to be updated and disappearing data to be removed
-- | ====================================================================================
type Model = Array Char

script :: forall m. D3InterpreterM D3Selection_ m => m ((Array Char) -> m D3Selection_)
script = do 
  root        <- attach "div#gup"
  svg         <- append root $ node Svg [ viewBox 0.0 0.0 650.0 650.0 ]
  letterGroup <- append svg  $ node_ Group

  let 
    transition = transitionWithDuration $ Milliseconds 2000.0
    -- new entries enter at this position, updating entries need to transition to it on each update
    xFromIndex :: Datum_ -> Index_ -> Number
    xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0)

  pure $ \letters -> -- since Model is simply Array Char it can be used directly in the Join
    do 
      letterGroup <+> JoinGeneral {
          element   : Text
        , key       : UseDatumAsKey
        , "data"    : letters
        , behaviour : { 
            enter: [ classed  "enter"
                    , fill     "green"
                    , x        xFromIndex
                    , y        0.0
                    -- , yu (NWU { i: 0, u: Px })
                    , text     (singleton <<< datumIsChar)
                    , fontSize 48.0
                    ]  
                    `andThen` (transition `to` [ y 200.0 ]) 

          , update: [ classed "update"
                    , fill "gray"
                    , y 200.0 ] 
                    `andThen` (transition `to` [ x xFromIndex ] ) 

          , exit:   [ classed "exit"
                    , fill "brown"
                    ] 
                    `andThen` (transition `to` [ y 400.0, remove ])
          }
      }


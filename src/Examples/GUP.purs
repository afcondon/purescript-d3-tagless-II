module D3.Examples.GUP where

import D3.Attributes.Sugar

import D3.Attributes.Instances (datumIsChar, indexIsNumber)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Selection (ChainableS, Join(..), Keys(..), node, node_)
import Data.String.CodeUnits (singleton)
import Effect.Aff (Milliseconds(..))
import Prelude (bind, pure, ($), (*), (+), (<<<))

-- | ====================================================================================
-- | Simple-as-can-be example of the more complex Join which allows for new data to be
-- | entered, existing data to be updated and disappearing data to be removed
-- | ====================================================================================
type Model = Array Char

script :: forall m. D3InterpreterM D3Selection_ m => m ((Array Char) -> m D3Selection_)
script = do 
  let 
    transition :: ChainableS
    transition = transitionWithDuration $ Milliseconds 2000.0
    -- new entries enter at this position, updating entries need to transition to it on each update
    xFromIndex :: Datum_ -> Index_ -> Number
    xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0)

  root        <- attach "div#gup"
  svg         <- append root $ node Svg [ viewBox 0.0 0.0 650.0 650.0 ]
  letterGroup <- append svg  $ node_ Group

  pure $ \letters -> 
    do 
      letterGroup <+> JoinGeneral {
          element   : Text
        , key       : UseDatumAsKey
        , "data"    : letters
        , behaviour : { 
            enter:  [ classed  "enter"
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
                    , y 200.0
                    ] 
                    `andThen` (transition `to` [ x xFromIndex ] ) 

          , exit:   [ classed "exit"
                    , fill "brown"
                    ] 
                    `andThen` (transition `to` [ y 400.0, remove ])
          }
      }


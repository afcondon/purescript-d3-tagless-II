module D3.Examples.GUP where

import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum, Index, datumIsChar, indexIsNumber)
import D3.Attributes.Sugar (classed, fill, fontSize, height, remove, strokeColor, strokeOpacity, text, viewBox, width, with, x, y)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join)
import D3.Selection (Chainable, D3Selection_, D3State(..), D3_Node(..), Element(..), EnterUpdateExit, Join(..), Keys(..), SelectionName(..), node_)
import Data.Maybe (Maybe)
import Data.String.CodeUnits (singleton)
import Prelude (class Bind, bind, negate, pure, ($), (*), (+), (<<<), (<>))
import Unsafe.Coerce (unsafeCoerce)


-- simple attributes don't use data, no hassle
svgAttributes :: Array Chainable
svgAttributes = [
    strokeColor "green"
  , strokeOpacity 0.75
  , width 1000.0
  , height 1000.0
  , viewBox (-500.0) (-500.0) 1000.0 1000.0
]

-- attributes that use (\d -> <something>) or (\d i -> <something>) need type hints
-- and coercion functions
type Model = Array Char

offsetXByIndex :: Datum -> Index -> Number
offsetXByIndex d i = offset + ((indexIsNumber i) * factor)
  where
    offset = 500.0
    factor = 48.0

textFromDatum :: Datum -> String
textFromDatum = singleton <<< datumIsChar

enterUpdateExit :: Chainable -> EnterUpdateExit
enterUpdateExit t =
  { enter:  
    [ classed "enter" -- TODO remove totally meaningless attributes only here to demo
    , fill "green"
    , x offsetXByIndex
    , y 0.0
    , text textFromDatum
    , fontSize 48.0
    ]  <> 
    (t `with` [ y 200.0 ] )

  , update: [ classed "update", fill "gray", y 200.0 ] <> (t `with` [ x offsetXByIndex ] ) 

  , exit:   [ classed "exit",   fill "brown" ] <> (t `with` [ y 400.0, remove ])
  }

-- modelData is probably already in stateT but doesn't need to be until we hit a join
enter :: ∀ m. (D3Tagless m) => m D3Selection_ 
enter = do 
  root <- hook "div#gup"
  svg  <- appendTo root "svg-gup" $ D3_Node Svg svgAttributes
  appendTo svg "letter-group" $ node_ Group

update :: ∀ m. Bind m => MonadState (D3State (Array Char)) m => D3Tagless m => Chainable -> m (Maybe D3Selection_)
update transition = do
  (D3State state) <- get  

  joinSelection_ <- join state.model $ Join {
      element   : Text
    , key       : DatumIsKey
    , selection : SelectionName "letter-group"
    , projection: unsafeCoerce -- null projection
    , behaviour : enterUpdateExit transition
  }

  pure joinSelection_

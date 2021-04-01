module D3.Examples.GUP where

import D3.Attributes.Instances (Datum, Index, datumIsChar, indexIsNumber)
import D3.Attributes.Sugar (classed, fill, fontSize, height, remove, strokeColor, strokeOpacity, text, viewBox, width, with, x, y)
import D3.Selection (Chainable, D3Selection, D3State(..), D3_Node(..), Element(..), EnterUpdateExit, node_) 
import Prelude (class Bind, bind, negate, pure, ($), (*), (+), (<<<), (<>))

import Control.Monad.State (class MonadState, get)
import D3.Interpreter.Tagless (class D3Tagless, Keys(..), append, hook, join)
import Data.String.CodeUnits (singleton)


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
    [ classed "enter"
    , fill "green"
    , x offsetXByIndex
    , y 0.0
    , text textFromDatum
    , fontSize 48.0
    ]  <> 
    (t `with` [ y 200.0 ] )

  , update: [ classed "update", fill "gray" ] <> (t `with` [ x offsetXByIndex ] ) 

  , exit:   [ classed "exit", fill "brown" ]  <> (t `with` [ y 400.0, remove ])
  }

-- TODO we can actually potentially return _much_ more structured output, selection tree etc
enter :: ∀ m. (D3Tagless m) => m D3Selection 
enter = do -- modelData is already in stateT   
  root <- hook "div#root"
  svg  <- append $ D3_Node Svg svgAttributes
  append $ node_ Group

-- TODO we can actually return much more structured output, selection tree etc
update :: forall m. Bind m => MonadState D3State m => D3Tagless m => Chainable -> m D3Selection
update t = do
  (D3State _ letters) <- get  
  joinSelection <- join Text DatumIsKey letters (enterUpdateExit t)

  pure joinSelection

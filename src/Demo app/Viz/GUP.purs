module D3.Examples.GUP where

import D3.Attributes.Sugar

import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import D3.Selection (SelectionAttribute)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach, openSelection, setAttributes, updateJoin)
import Data.String.CodeUnits (singleton)
import Effect.Aff (Milliseconds(..))
import Prelude (bind, discard, pure, ($), (*), (+), (<<<))
import Unsafe.Coerce (unsafeCoerce)

-- | ====================================================================================
-- | Simple-as-can-be example of the more complex Join which allows for new data to be
-- | entered, existing data to be updated and disappearing data to be removed
-- | ====================================================================================
-- SNIPPET
-- Name: GUP
type Model = Array Char

-- in the interests of brevity these unsafe functions are defined here with the "script"
-- however, in a larger program both Model and Unsafe would be their own modules
datumIsChar :: Datum_ -> Char
datumIsChar = unsafeCoerce

indexIsNumber :: Index_ -> Number
indexIsNumber = unsafeCoerce

keyFunction :: Datum_ -> Index_ -- for this very simple example, the data (Char) can be used directly as the key
keyFunction = unsafeCoerce

exGeneralUpdatePattern :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m ((Array Char) -> m D3Selection_)
exGeneralUpdatePattern selector = do 
  root           <- attach selector
  svg            <- appendTo root Svg [ viewBox 0.0 0.0 650.0 650.0, classed "d3svg gup" ]
  letterGroup    <- appendTo svg Group []
  
  pure $ \letters -> do
    enterSelection   <- openSelection letterGroup "text"
    updateSelections <- updateJoin enterSelection Text letters keyFunction
    setAttributes updateSelections.exit exit
    setAttributes updateSelections.update update

    newlyEntered     <- appendTo updateSelections.enter Text []
    setAttributes newlyEntered enter
    
    pure newlyEntered

  where 
    transition :: SelectionAttribute
    transition = transitionWithDuration $ Milliseconds 2000.0

    xFromIndex :: Datum_ -> Index_ -> Number
    xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0) -- letters enter at this position, and then must transition to new position on each update

    enter = [ classed  "enter"
            , fill     "green"
            , x        xFromIndex
            , y        0.0
            , text     (singleton <<< datumIsChar)
            , fontSize 96.0 ]  
          `andThen` (transition `to` [ y 200.0 ]) 

    update =  [ classed "update", fill "gray", y 200.0 ] 
              `andThen` (transition `to` [ x xFromIndex ] ) 

    exit =  [ classed "exit", fill "brown"] 
            `andThen` (transition `to` [ y 400.0, remove ])
-- TEPPINS

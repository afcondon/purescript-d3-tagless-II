module D3.Viz.GUP where

import PSD3.Internal.Attributes.Sugar

import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, openSelection, setAttributes, updateJoin)
import D3.Viz.GUP.Unsafe (coerceDatumToChar, coerceIndexToNumber, coerceDatumToKey)
import Data.String.CodeUnits (singleton)
import Effect.Aff (Milliseconds(..))
import Prelude (bind, discard, pure, ($), (*), (+), (<<<))

-- | ====================================================================================
-- | Simple-as-can-be example of the more complex Join which allows for new data to be
-- | entered, existing data to be updated and disappearing data to be removed
-- | ====================================================================================
-- Snippet_Start
-- Name: GUP
type Model = Array Char

-- | Accessor record for working with bound data
datum_ ::
  { char :: Datum_ -> Char
  , indexNum :: Index_ -> Number
  }
datum_ =
  { char: coerceDatumToChar
  , indexNum: coerceIndexToNumber
  }

exGeneralUpdatePattern :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m ((Array Char) -> m D3Selection_)
exGeneralUpdatePattern selector = do 
  root           <- attach selector
  svg            <- appendTo root Svg [ viewBox 0.0 100.0 800.0 350.0, classed "d3svg gup" ]
  letterGroup    <- appendTo svg Group []
  
  pure $ \letters -> do
    enterSelection   <- openSelection letterGroup "text"
    updateSelections <- updateJoin enterSelection Text letters coerceDatumToKey
    setAttributes updateSelections.exit exit
    setAttributes updateSelections.update update

    newlyEntered     <- appendTo updateSelections.enter Text []
    setAttributes newlyEntered enter

    pure newlyEntered

  where
    transition :: SelectionAttribute
    transition = transitionWithDuration $ Milliseconds 2000.0

    xFromIndex :: Datum_ -> Index_ -> Number
    xFromIndex _ i = 50.0 + (datum_.indexNum i * 48.0) -- letters enter at this position, and then must transition to new position on each update

    enter = [ classed  "enter"
            , fill     "green"
            , x        xFromIndex
            , y        0.0
            , text     (singleton <<< datum_.char)
            , fontSize 60.0 ]
          `andThen` (transition `to` [ y 200.0 ])

    update =  [ classed "update", fill "gray", y 200.0 ]
              `andThen` (transition `to` [ x xFromIndex ] )

    exit =  [ classed "exit", fill "brown"]
            `andThen` (transition `to` [ y 400.0, remove ])
-- Snippet_End

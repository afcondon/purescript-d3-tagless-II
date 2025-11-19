module D3.Viz.GUP where

import PSD3.Internal.Attributes.Sugar

import PSD3.Internal.Types (D3Selection_, Element(..), Index_, Selector)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, openSelection, setAttributes, updateJoin)
import D3.Viz.GUP.Unsafe (charToKey, coerceIndexToNumber)
import Data.String.CodeUnits (singleton)
import Effect.Aff (Milliseconds(..))
import Prelude (Unit, bind, discard, pure, ($), (*), (+))

-- | ====================================================================================
-- | Simple-as-can-be example of the more complex Join which allows for new data to be
-- | entered, existing data to be updated and disappearing data to be removed
-- | ====================================================================================
type Model = Array Char

-- PHANTOM TYPE SUCCESS: No more datum_ boilerplate!
-- After updateJoin, the selections are typed as D3Selection_ Char
-- So lambdas can use d :: Char directly!

-- | creates the SVG and a <g> within it to hold the letters
-- | returns a function which can be called repeatedly to generate each sequence of new and exiting letters
exGeneralUpdatePattern :: forall m. SelectionM D3Selection_ m => Selector (D3Selection_ Unit) -> m ((Array Char) -> m (D3Selection_ Char))
exGeneralUpdatePattern selector = do
  root           <- attach selector
  svg            <- appendTo root Svg [ viewBox 0.0 100.0 800.0 350.0, classed "d3svg gup" ]
  letterGroup    <- appendTo svg Group []

  pure $ \letters -> do
    enterSelection   <- openSelection letterGroup "text"
    updateSelections <- updateJoin enterSelection Text letters charToKey
    setAttributes updateSelections.exit exit
    setAttributes updateSelections.update update

    newlyEntered     <- appendTo updateSelections.enter Text []
    setAttributes newlyEntered enter

    pure newlyEntered

  where
    transition :: forall d. SelectionAttribute d
    transition = transitionWithDuration $ Milliseconds 2000.0

    -- Typed lambda! After updateJoin, d :: Char and i :: Index_
    xFromIndex :: Char -> Index_ -> Number
    xFromIndex _ i = 50.0 + (coerceIndexToNumber i * 48.0)

    enter = [ classed  "enter"
            , fill     "green"
            , x        xFromIndex
            , y        0.0
            , text     singleton  -- d :: Char, so just use singleton directly!
            , fontSize 60.0 ]
          `andThen` (transition `to` [ y 200.0 ])

    update =  [ classed "update", fill "gray", y 200.0 ]
              `andThen` (transition `to` [ x xFromIndex ] )

    exit =  [ classed "exit", fill "brown"]
            `andThen` (transition `to` [ y 400.0, remove ])

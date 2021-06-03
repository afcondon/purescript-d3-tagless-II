module D3.Examples.GUP where

import D3.Attributes.Sugar

import Control.Monad.Rec.Class (forever)
import D3.Attributes.Instances (datumIsChar, indexIsNumber)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Interpreter.D3 (runD3M)
import D3.Selection (ChainableS, Join(..), Keys(..), node, node_)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Prelude (class Bind, Unit, bind, discard, pure, unit, ($), (*), (+), (<$>), (<<<), (>))

runGeneralUpdatePattern :: forall m. Bind m => MonadEffect m => m (Unit -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  (Tuple update _) <- liftEffect $ runD3M script
  -- now we return a function that the component can run whenever it likes
  -- (but NB if it runs more often than every 2000 milliseconds there will be big problems)
  pure $ 
    (\_ -> do
      newletters <- liftEffect $ getLetters
      _          <- liftEffect $ runD3M (update newletters)
      log "GUP renew"
-- TODO i think delay logically belongs in the component not the script? but it is dependent on the transition length...hmmm
      delay (Milliseconds 2300.0)) -- NB this has to be a smidge longer than any transitions in the update!

-- | choose a string of random letters (no duplicates), ordered alphabetically
getLetters :: Effect (Array Char)
getLetters = do
  let 
    letters = toCharArray "abcdefghijklmnopqrstuvwxyz"
    coinToss :: Char -> Effect (Maybe Char)
    coinToss c = do
      n <- random
      pure $ if n > 0.6 then Just c else Nothing
  
  choices <- sequence $ coinToss <$> letters
  pure $ catMaybes choices

-- | ====================================================================================
-- | Simple as can be example of the more complex Join which allows for new data to be
-- | entered, existing data to be updated and disappearing data to be removed
-- | ====================================================================================
type Model = Array Char

-- | this could be inlined but we use it in two places:
-- | on first entry and then as an end point of update transition
offsetXByIndex :: Datum_ -> Index_ -> Number
offsetXByIndex datum i = 50.0 + ((indexIsNumber i) * 48.0)

script :: forall m. D3InterpreterM D3Selection_ m => m ((Array Char) -> m D3Selection_)
script = do 
  root        <- attach "div#gup"
  svg         <- append root $ node Svg [ viewBox 0.0 0.0 650.0 650.0 ]
  letterGroup <- append svg  $ node_ Group
  let transition = transitionWithDuration $ Milliseconds 2000.0
  pure $ \letters -> -- since Model is simply Array Char it can be used directly in the Join
    do 
      letterGroup <+> JoinGeneral {
          element   : Text
        , key       : UseDatumAsKey
        , "data"    : letters
        , behaviour : { 
            enter: [ classed  "enter"
                    , fill     "green"
                    , x        offsetXByIndex
                    , y        0.0
                    -- , yu (NWU { i: 0, u: Px })
                    , text     (singleton <<< datumIsChar)
                    , fontSize 48.0
                    ]  
                    `andThen` (transition `to` [ y 200.0 ]) 

          , update: [ classed "update"
                    , fill "gray"
                    , y 200.0 ] 
                    `andThen` (transition `to` [ x offsetXByIndex ] ) 

          , exit:   [ classed "exit"
                    , fill "brown"
                    ] 
                    `andThen` (transition `to` [ y 400.0, remove ])
          }
      }


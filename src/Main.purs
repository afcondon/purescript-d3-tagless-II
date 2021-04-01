module Main where

import Data.Array (catMaybes)
import Prelude (Unit, bind, liftA1, pure, ($), (<$>), (>))

import Control.Monad.Rec.Class (forever)
import D3.Attributes.Sugar (transitionWithDuration)
import D3.Examples.GUP (enter, update) as GUP
import D3.Interpreter.Tagless (runD3M)
import D3.Selection (Chainable, D3State(..), makeD3State, makeD3State')
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (random)

initialState :: D3State
initialState = makeD3State' (toCharArray "this data is ignored - FIX ME")

duration :: Milliseconds
duration = Milliseconds 2000.0

t :: Chainable
t = transitionWithDuration duration

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

main :: Effect Unit
main = launchAff_  do
  (D3State _ circles) <- liftEffect $ liftA1 snd $ runD3M GUP.enter initialState

  forever $ do
    letters <- liftEffect $ getLetters
    _ <- liftEffect $ runD3M (GUP.update t) (makeD3State letters circles)
    delay duration

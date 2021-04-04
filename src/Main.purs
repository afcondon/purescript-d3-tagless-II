module Main where

import Affjax (Error, printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Rec.Class (forever)
import D3.Attributes.Sugar (transitionWithDuration)
import D3.Examples.GUP (enter, update) as GUP
import D3.Examples.Tree as Tree
import D3.Interpreter.Tagless (runD3M)
import D3.Selection (Chainable, makeD3State', setData)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Prelude (Unit, bind, discard, liftA1, pure, unit, ($), (*>), (<$>), (>))
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

-- TODO could preload this as a named transition? is that cleaner or more obscure?
transition :: Milliseconds -> Chainable
transition = transitionWithDuration

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

-- TODO read file in Main and pass in Model, probably
readTreeFromFileContents :: forall r. Tuple Number Number -> Either Error { body ∷ String | r } -> Either Error (Tree.Model String)
readTreeFromFileContents (Tuple width _) (Right { body } ) = Right $ Tree.makeModel width (Tree.readJSONJS_ body)
readTreeFromFileContents _               (Left error)      = Left error

main :: Effect Unit
main = launchAff_  do
  widthHeight    <- liftEffect getWindowWidthHeight

  log "Radial tree example"
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"
  case readTreeFromFileContents widthHeight treeJSON of
    (Left error) -> liftEffect $ log $ printError error

    (Right treeModel) -> liftEffect $ runD3M Tree.enter (makeD3State' treeModel) *> pure unit

  log "General Update Pattern example"
  letters <- liftEffect $ getLetters
  state   <- liftEffect $ liftA1 snd $ runD3M GUP.enter (makeD3State' letters)
  let duration = Milliseconds 2000.0
  forever $ do -- need to fork here or nothing else will run
    newletters <- liftEffect $ getLetters
    _          <- liftEffect $ runD3M 
                                (GUP.update $ transition duration)
                                (setData newletters state)
    delay (Milliseconds 2300.0)


  

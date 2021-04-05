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
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
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


-- TODO read file in Main and pass in Model, probably
readTreeFromFileContents :: forall r. Tuple Number Number -> Either Error { body ∷ String | r } -> Either Error (Tree.Model String)
readTreeFromFileContents (Tuple width _) (Right { body } ) = Right $ Tree.makeModel width (Tree.readJSONJS_ body)
readTreeFromFileContents _               (Left error)      = Left error

drawTree :: Aff Unit
drawTree = do
  log "Radial tree example"
  widthHeight   <- liftEffect getWindowWidthHeight
  treeJSON      <- AJAX.get ResponseFormat.string "http://localhost:1234/flare-2.json"

  case readTreeFromFileContents widthHeight treeJSON of
    (Left error)      -> liftEffect $ log $ printError error
    (Right treeModel) -> liftEffect $ runD3M Tree.enter (makeD3State' treeModel) *> pure unit


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

runGeneralUpdatePattern :: Aff Unit
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  let transition = transitionWithDuration $ Milliseconds 2000.0
  letters      <- liftEffect $ getLetters
  state        <- liftEffect $ liftA1 snd $ runD3M GUP.enter (makeD3State' letters)
  forever $ do
    newletters <- liftEffect $ getLetters
    _          <- liftEffect $ runD3M 
                                (GUP.update transition)
                                (setData newletters state)
    delay (Milliseconds 2300.0)

main :: Effect Unit
main = launchAff_  do
  _ <- forkAff runGeneralUpdatePattern
  drawTree



  

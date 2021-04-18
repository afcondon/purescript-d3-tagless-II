module D3.Examples.GUP where

import D3.Attributes.Sugar

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum, Index, datumIsChar, indexIsNumber)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, attach, runD3M, (<+>))
import D3.Selection (Chainable, D3Selection_, Element(..), EnterUpdateExit, Join(..), Keys(..), node, node_)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Prelude (Unit, bind, discard, negate, pure, ($), (*), (+), (<$>), (<<<), (<>), (>))


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
  (Tuple update state) <- liftEffect $ runD3M enter []
  forever $ do
    newletters <- liftEffect $ getLetters
    _          <- liftEffect $ runD3M (update transition) newletters
    delay (Milliseconds 2300.0)

svgAttributes :: Array Chainable
svgAttributes = [ -- TODO remove totally meaningless attributes only here to demo, fix viewbox width etc too
    width   1000.0
  , height  1000.0
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
enterUpdateExit transition =
  { enter:  
    [ classed  "enter"
    , fill     "green" -- TODO use PureScript color types
    , x        offsetXByIndex
    , y        0.0
    -- , yu (NWU { i: 0, u: Px })
    , text     textFromDatum
    , fontSize 48.0
    ]  <> 
    (transition `with` [ y 200.0 ] )

  , update: [ classed "update", fill "gray", y 200.0 ] <> (transition `with` [ x offsetXByIndex ] ) 

  , exit:   [ classed "exit",   fill "brown" ] <> (transition `with` [ y 400.0, remove ])
  }

enter :: forall m. 
  MonadState (Array Char) m => 
  D3Tagless m => 
  m (Chainable -> m D3Selection_)
enter = do 
  root    <- attach "div#gup"
  svg     <- appendTo root $ node Svg svgAttributes -- TODO attributes first a la hologen
  letterS <- appendTo svg  $ node_ Group
  pure $ \transition -> do
              letters <- get 
              -- we don't have to apply a projection in this case cause the data is already array
              letterS <+> JoinGeneral {
                  element   : Text
                , key       : DatumIsUnique
                , "data"    : letters
                , behaviour : enterUpdateExit transition
              }

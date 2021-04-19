module D3.Examples.GUP where

import D3.Attributes.Sugar

import Control.Monad.Rec.Class (forever)
import D3.Attributes.Instances (Datum, Index, datumIsChar, indexIsNumber)
import D3.Interpreter.Tagless (class D3Tagless, append, attach, runD3M, (<+>))
import D3.Selection (Chainable, D3Selection_, Element(..), Join(..), Keys(..), node, node_)
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
import Prelude (Unit, bind, discard, negate, pure, ($), (*), (+), (<$>), (<<<), (>))


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
  (Tuple update _) <- liftEffect $ runD3M enter
  forever $ do
    newletters <- liftEffect $ getLetters
    _          <- liftEffect $ runD3M (update newletters)
    delay (Milliseconds 2300.0) -- NB this has to be a smidge longer than any transitions in the update!

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

enter :: forall m. D3Tagless m => m ((Array Char) -> m D3Selection_)
enter = do 
  root        <- attach "div#gup"
  svg         <- append root $ node Svg svgAttributes -- TODO attributes first a la hologen
  letterGroup <- append svg  $ node_ Group            -- the container for the Join
  let transition = transitionWithDuration $ Milliseconds 2000.0
  pure $ \letters -> -- we don't have to do anything to this 'cause the data is already Array Char
    do 
      letterGroup <+> JoinGeneral {
          element   : Text
        , key       : UseDatumAsKey
        , "data"    : letters
        , behaviour : { 
            enter: [ classed  "enter"
                    , fill     "green"                -- TODO use PureScript color types
                    , x        offsetXByIndex
                    , y        0.0
                    -- , yu (NWU { i: 0, u: Px })
                    , text     textFromDatum
                    , fontSize 48.0
                    ]  `andThen` (transition `to` [ y 200.0 ]) 

          , update: [ classed "update", fill "gray", y 200.0 ] `andThen` (transition `to` [ x offsetXByIndex ] ) 

          , exit:   [ classed "exit",   fill "brown" ] `andThen` (transition `to` [ y 400.0, remove ])
          }
      }


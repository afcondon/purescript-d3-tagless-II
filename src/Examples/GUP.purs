module D3.Examples.GUP where

import D3.Attributes.Sugar

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum, Index, NWU(..), UnitType(..), datumIsChar, indexIsNumber)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, attach, join, runD3M)
import D3.Selection (Chainable, D3Selection_, D3State(..), D3_Node(..), Element(..), EnterUpdateExit, Join(..), Keys(..), SelectionName(..), identityProjection, makeD3State', node, node_)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Prelude (class Bind, Unit, bind, discard, liftA1, negate, pure, ($), (*), (+), (<$>), (<<<), (<>), (>))
import Unsafe.Coerce (unsafeCoerce)


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
  letters                <- liftEffect $ getLetters
  (Tuple updateFn state) <- liftEffect $ runD3M enter makeD3State'
  forever $ do
    newletters <- liftEffect $ getLetters
    _          <- liftEffect $ runD3M (updateFn transition newletters) state
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

enter :: ∀ m. MonadState (D3State (Array Char)) m => D3Tagless m => m (Chainable -> Array Char -> m D3Selection_) 
enter = do 
  root    <- attach "div#gup"
  svg     <- appendTo root $ node Svg svgAttributes -- TODO attributes first a la hologen
  letterS <- appendTo svg  $ node_ Group
  pure $ update letterS

update :: forall m. Bind m => D3Tagless m => D3Selection_ -> Chainable -> Array Char -> m D3Selection_
update selection transition model = do
  joinSelection_ <- join model $ JoinGeneral {
      element   : Text
    , key       : DatumIsUnique
    , hook      : selection
    , projection: unsafeCoerce -- TODO ADT to hide this under IdentityProjection
    , behaviour : enterUpdateExit transition
  }
  pure joinSelection_

module Stories.GUP where

import Prelude 

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState)
import D3.Examples.GUP as GUP
import Data.Array (catMaybes)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Effect.Random (random)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import D3.Interpreter.D3 (runD3M)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = StartGUP
  | RestartGUP
  | PauseGUP
  | KillGUP

data Status = NotInitialized | Running | Paused
instance showStatus :: Show Status where
  show NotInitialized = "Not yet initialized"
  show Running = "GUP is running"
  show Paused  = "GUP is paused"
  
type State = { 
    value  :: Status
  , fiber  :: Maybe (Fiber Unit)
  , update :: Maybe (Array Char -> Aff Unit)
}

component :: forall m. MonadAff m => H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction }
  }
  where

  initialState :: State
  initialState = { value: NotInitialized, fiber: Nothing, update: Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.id "gup" ]
      [ HH.h3_
          [ HH.text "General Update Pattern" ]
      , HH.div_
          [ HH.text $ show state.value ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const StartGUP ]
              [ HH.text "Start" ]
          ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const PauseGUP ]
              [ HH.text "Pause" ]
          ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const RestartGUP ]
              [ HH.text "Restart" ]
          ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const KillGUP ]
              [ HH.text "Remove" ]
          ]
      ]

runGeneralUpdatePattern :: forall m. Bind m => MonadEffect m => m (Array Char -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  (Tuple update _) <- liftEffect $ runD3M GUP.script
  -- the script sets up the SVG and returns a function that the component can run whenever it likes
  -- (but NB if it runs more often than every 2000 milliseconds there will be big problems)
  pure (\letters -> liftEffect $ runD3M (update letters) *> pure unit )

runUpdate :: (Array Char -> Aff Unit) -> Aff Unit
runUpdate update = do
  letters <- liftEffect $ getLetters
  update letters
  delay (Milliseconds 2300.0)
  where
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


handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction StartGUP = do
    updateFn <- runGeneralUpdatePattern

    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn

    H.modify_ (\state -> state { value = Running, fiber = Just fiber, update = Just updateFn })

handleAction PauseGUP = do
    fiber <- H.gets _.fiber
    _ <- case fiber of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Cancel fiber to suspend computation") fiber
    H.modify_ (\state -> state { value = Paused, fiber = Nothing })

handleAction RestartGUP = do
    update <- H.gets _.update
    case update of
      Nothing -> pure unit
      (Just updateFn) -> do
        fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
        H.modify_ (\state -> state { value = Running, fiber = Just fiber })

handleAction KillGUP = do
    fiber <- H.gets _.fiber
    _ <- case fiber of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
    H.modify_ (\state -> state { value = NotInitialized, fiber = Nothing, update = Nothing })

module Component.GeneralUpdatePattern where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import D3.Viz.TreeAPI.V3GUPDemoFriendly as GUP

type State =
  { status :: Status
  , fiber :: Maybe (Fiber Unit)
  , update :: Maybe (Array Char -> Aff Unit)
  }

data Status = Running | Paused

derive instance eqStatus :: Eq Status

instance showStatus :: Show Status where
  show Running = "Running"
  show Paused = "Paused"

data Action
  = Initialize
  | ToggleStatus
  | Finalize

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { status: Paused
        , fiber: Nothing
        , update: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "gup-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "General Update Pattern" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "A simple example of D3's enter/update/exit pattern with animated letters." ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "control-button")
            , HE.onClick \_ -> ToggleStatus
            ]
            [ HH.text $ show state.status ]
        ]

    , HH.div
        [ HP.class_ (HH.ClassName "svg-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "What This Demonstrates" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Enter/Update/Exit pattern - letters appear, move, and disappear" ]
            , HH.li_ [ HH.text "Green letters are entering (new data)" ]
            , HH.li_ [ HH.text "Gray letters are updating (existing data, new position)" ]
            , HH.li_ [ HH.text "Brown letters are exiting (removed from data)" ]
            , HH.li_ [ HH.text "All transitions are smoothly animated" ]
            ]
        , HH.h3_ [ HH.text "Code Pattern" ]
        , HH.pre [ HP.class_ (HH.ClassName "code-snippet") ]
            [ HH.text """openSelection <- openSelection letterGroup "text"
updateSelections <- updateJoin openSelection Text letters charToKey

-- Handle each phase with transitions
setAttributes updateSelections.exit [classed "exit", ...]
setAttributes updateSelections.update [classed "update", ...]

newlyEntered <- appendTo updateSelections.enter Text []
setAttributes newlyEntered [classed "enter", ...]""" ]
        ]
    ]

runGeneralUpdatePattern :: forall m. Bind m => MonadAff m => m (Array Char -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example (Friendly DSL)"
  update <- liftEffect $ GUP.initFriendlyGUP "div.svg-container"
  pure (\letters -> liftEffect $ update letters)

-- | Choose a string of random letters (no duplicates), ordered alphabetically
getLetters :: Aff (Array Char)
getLetters = liftEffect do
  let
    letters = toCharArray "abcdefghijklmnopqrstuvwxyz"
    coinToss :: Char -> _ (Maybe Char)
    coinToss c = do
      n <- random
      pure $ if n > 0.6 then Just c else Nothing

  choices <- sequence $ coinToss <$> letters
  pure $ catMaybes choices

runUpdate :: (Array Char -> Aff Unit) -> Aff Unit
runUpdate update = do
  letters <- getLetters
  update letters
  delay (Milliseconds 2300.0)

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "GeneralUpdatePattern: Initializing"
    updateFn <- runGeneralUpdatePattern
    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
    H.modify_ \s -> s { status = Running, fiber = Just fiber, update = Just updateFn }
    log "GeneralUpdatePattern: Initialized successfully"

  ToggleStatus -> do
    state <- H.get
    case state.status of
      Running -> do
        log "Pausing updates"
        case state.fiber of
          Nothing -> pure unit
          Just fiber -> do
            H.liftAff $ killFiber (error "Cancel fiber to suspend computation") fiber
        H.modify_ \s -> s { status = Paused, fiber = Nothing }
      Paused -> do
        log "Starting updates"
        case state.update of
          Nothing -> pure unit
          Just updateFn -> do
            fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
            H.modify_ \s -> s { status = Running, fiber = Just fiber }

  Finalize -> do
    log "GeneralUpdatePattern: Finalizing"
    state <- H.get
    case state.fiber of
      Nothing -> pure unit
      Just fiber -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
    H.modify_ \s -> s { status = Paused, fiber = Nothing, update = Nothing }

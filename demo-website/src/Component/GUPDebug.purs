-- | GUP Debug Component
-- |
-- | Standalone debug page for testing GUP (General Update Pattern) with
-- | the Les Misérables force-directed graph. This isolates the GUPDemo
-- | visualization for cleaner debugging without noise from other demos.
module Component.GUPDebug where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.DataLoader (simpleLoadJSON)
import Unsafe.Coerce (unsafeCoerce)
import D3.Viz.LesMisV3.Model as LesMisModel
import D3.Viz.LesMisV3.GUPDemo as GUPDemo

-- =============================================================================
-- Component State
-- =============================================================================

type State =
  { gupState :: Maybe (Ref GUPDemo.LesMisGUPState)
  , cleanup :: Maybe (Effect Unit)
  }

data Action
  = Initialize
  | Finalize
  | AddNodes
  | RemoveNodes
  | ResetFull
  | TestGUPAPI

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { gupState: Nothing
      , cleanup: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Les Misérables data
    lesMisJson <- H.liftAff $ simpleLoadJSON "./data/miserables.json"
    let rawModel :: LesMisModel.LesMisRawModel
        rawModel = unsafeCoerce lesMisJson
        model = LesMisModel.processRawModel rawModel

    -- Initialize GUP demo in the container
    gupState <- liftEffect $ GUPDemo.initGUPDemo model "#gup-viz-container"
    H.modify_ _ { gupState = Just gupState }

  Finalize -> do
    state <- H.get
    -- Cleanup if needed (GUPDemo doesn't have explicit cleanup, simulation stops on its own)
    pure unit

  AddNodes -> do
    state <- H.get
    case state.gupState of
      Nothing -> pure unit
      Just stateRef -> liftEffect $ GUPDemo.addRandomNodes 5 stateRef

  RemoveNodes -> do
    state <- H.get
    case state.gupState of
      Nothing -> pure unit
      Just stateRef -> liftEffect $ GUPDemo.removeRandomNodes 5 stateRef

  ResetFull -> do
    state <- H.get
    case state.gupState of
      Nothing -> pure unit
      Just stateRef -> liftEffect $ GUPDemo.resetToFull stateRef

  TestGUPAPI -> do
    state <- H.get
    case state.gupState of
      Nothing -> pure unit
      Just stateRef -> liftEffect $ GUPDemo.testGUPAPI stateRef

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "gup-debug-page" ] ]
    [ HH.h1_ [ HH.text "GUP Debug - Les Misérables" ]
    , HH.p_ [ HH.text "Standalone debug page for testing General Update Pattern with force simulation." ]
    , HH.p_ [ HH.text "Watch the console for detailed GUP API output when clicking 'Test GUP API'." ]

    -- Visualization container
    , HH.div
        [ HP.id "gup-viz-container"
        , HP.classes [ HH.ClassName "viz-container" ]
        , HP.style "width: 100%; height: 600px; border: 1px solid #ccc; background: #fafafa;"
        ]
        []

    -- Button row
    , HH.div
        [ HP.classes [ HH.ClassName "button-row" ]
        , HP.style "margin-top: 1rem; display: flex; gap: 0.5rem; flex-wrap: wrap;"
        ]
        [ HH.button
            [ HP.classes [ HH.ClassName "debug-button" ]
            , HP.style "padding: 0.5rem 1rem; cursor: pointer;"
            , HE.onClick \_ -> AddNodes
            ]
            [ HH.text "Add 5 Nodes" ]
        , HH.button
            [ HP.classes [ HH.ClassName "debug-button" ]
            , HP.style "padding: 0.5rem 1rem; cursor: pointer;"
            , HE.onClick \_ -> RemoveNodes
            ]
            [ HH.text "Remove 5 Nodes" ]
        , HH.button
            [ HP.classes [ HH.ClassName "debug-button", HH.ClassName "secondary" ]
            , HP.style "padding: 0.5rem 1rem; cursor: pointer;"
            , HE.onClick \_ -> ResetFull
            ]
            [ HH.text "Reset All" ]
        , HH.button
            [ HP.classes [ HH.ClassName "debug-button", HH.ClassName "primary" ]
            , HP.style "padding: 0.5rem 1rem; cursor: pointer; background: #4CAF50; color: white; border: none;"
            , HE.onClick \_ -> TestGUPAPI
            ]
            [ HH.text "Test GUP API (console)" ]
        ]

    -- Instructions
    , HH.div
        [ HP.style "margin-top: 2rem; padding: 1rem; background: #f0f0f0; border-radius: 4px;" ]
        [ HH.h3_ [ HH.text "How to Use" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Add/Remove Nodes: Triggers GUP transitions (green=enter, gray=update, brown=exit)" ]
            , HH.li_ [ HH.text "Reset All: Restores the full dataset" ]
            , HH.li_ [ HH.text "Test GUP API: Logs detailed enter/update/exit info to browser console" ]
            , HH.li_ [ HH.text "Drag nodes to reposition them; the simulation will respond" ]
            ]
        ]
    ]

module PSD3.WealthHealth.WealthHealth where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PSD3.WealthHealth.Actions (Action(..))
import PSD3.WealthHealth.Data (loadNationsData, getNationAtYear)
import PSD3.WealthHealth.HTML (renderControlPanel, renderLegend)
import PSD3.WealthHealth.Rendering (renderVisualization, updateVisualization)
import PSD3.WealthHealth.State (State, initialState)
import PSD3.WealthHealth.Types (WealthHealthModel, NationPoint, regionName)

-- | Wealth & Health visualization component
component :: forall q i o m. MonadAff m => MonadEffect m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- | Render the visualization
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "wealth-health-page" ] ]
    [ -- Header
      HH.header
        [ HP.classes [ HH.ClassName "wealth-health-header" ] ]
        [ HH.h1_ [ HH.text "The Wealth & Health of Nations" ]
        , HH.p_ [ HH.text "An animated exploration of income, life expectancy, and population across 180 nations from 1800 to 2009" ]
        ]

    -- Main content area
    , HH.div
        [ HP.classes [ HH.ClassName "wealth-health-content" ] ]
        [ -- Left panel: controls
          HH.aside
            [ HP.classes [ HH.ClassName "wealth-health-sidebar" ] ]
            [ renderControlPanel state
            , renderLegend
            ]

        -- Center: visualization
        , HH.main
            [ HP.classes [ HH.ClassName "wealth-health-visualization" ] ]
            [ case state.model of
                Nothing ->
                  HH.div
                    [ HP.classes [ HH.ClassName "wealth-health-loading" ] ]
                    [ HH.text "Loading data..." ]
                Just model ->
                  HH.div
                    [ HP.classes [ HH.ClassName "wealth-health-viz-wrapper" ] ]
                    [ HH.div
                        [ HP.id "wealth-health-viz"
                        , HP.classes [ HH.ClassName "wealth-health-viz-container" ] ]
                        []  -- D3 will render SVG here
                    , renderTooltip state model
                    ]
            ]
        ]
    ]

-- | Render tooltip for hovered nation
renderTooltip :: forall m. State -> WealthHealthModel -> H.ComponentHTML Action () m
renderTooltip state model =
  case state.hoveredNation of
    Nothing -> HH.text ""
    Just nationName ->
      -- Find the nation data for the current year
      case getNationData nationName state.currentYear model of
        Nothing -> HH.text ""
        Just nation ->
          HH.div
            [ HP.classes [ HH.ClassName "wealth-health-tooltip" ] ]
            [ HH.h4_ [ HH.text nation.name ]
            , HH.div_ [ HH.text $ "Region: " <> regionName nation.region ]
            , HH.div_ [ HH.text $ "Income: $" <> formatNumber nation.income ]
            , HH.div_ [ HH.text $ "Population: " <> formatPopulation nation.population ]
            , HH.div_ [ HH.text $ "Life Expectancy: " <> formatNumber nation.lifeExpectancy <> " years" ]
            ]

-- Helper to get nation data for a specific nation and year
getNationData :: String -> Int -> WealthHealthModel -> Maybe NationPoint
getNationData nationName year model = do
  nationRawData <- find (\n -> n.name == nationName) model.nations
  getNationAtYear year nationRawData

-- | Format a number with commas
formatNumber :: Number -> String
formatNumber n =
  show $ floor n  -- TODO: Add proper formatting with commas

-- | Format population (in millions)
formatPopulation :: Number -> String
formatPopulation n =
  let millions = n / 1000000.0
  in show (floor millions) <> "M"

-- | Handle actions
handleAction :: forall o m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load data
    result <- H.liftAff loadNationsData
    case result of
      Left err -> do
        -- TODO: Show error to user
        liftEffect $ log $ "Error loading data: " <> err
        pure unit
      Right model -> do
        -- Create hover event subscriptions
        { emitter: hoverEmitter, listener: hoverListener } <- H.liftEffect HS.create
        void $ H.subscribe hoverEmitter

        -- Store the listener for use by JavaScript callbacks
        H.modify_ _ { hoverListener = Just hoverListener }

        handleAction (DataLoaded model)

  DataLoaded model -> do
    state <- H.get
    H.modify_ _ { model = Just model }
    -- Set initial year to the minimum year in the dataset
    H.modify_ _ { currentYear = model.yearRange.min }

    -- Create hover callbacks that notify the subscription
    case state.hoverListener of
      Nothing -> pure unit
      Just listener -> do
        let onHover = \name -> HS.notify listener (HoverNation (Just name))
        let onLeave = \_ -> HS.notify listener (HoverNation Nothing)
        liftEffect $ renderVisualization "wealth-health-viz" model.yearRange.min model onHover onLeave

  DataLoadFailed err -> do
    -- TODO: Show error message in UI
    liftEffect $ log $ "Data load failed: " <> err
    pure unit

  SetYear year -> do
    H.modify_ _ { currentYear = year }
    handleAction Render

  TogglePlay -> do
    state <- H.get
    let newPlaying = not state.playing
    H.modify_ _ { playing = newPlaying }

    when newPlaying do
      -- Calculate interval based on animation speed (years per second)
      -- If speed is 5 years/sec, interval should be 200ms per year
      let intervalMs = 1000.0 / state.animationSpeed

      -- Subscribe to timer
      { emitter, listener } <- H.liftEffect HS.create
      subscriptionId <- H.subscribe emitter

      -- Store subscription ID so we can unsubscribe later
      H.modify_ _ { animationSubscriptionId = Just subscriptionId }

      -- Start the timer
      void $ H.liftAff $ Aff.forkAff $ forever do
        Aff.delay (Milliseconds intervalMs)
        H.liftEffect $ HS.notify listener Tick

    when (not newPlaying) do
      -- Unsubscribe from timer
      state' <- H.get
      case state'.animationSubscriptionId of
        Nothing -> pure unit
        Just subId -> do
          H.unsubscribe subId
          H.modify_ _ { animationSubscriptionId = Nothing }

  Tick -> do
    state <- H.get
    case state.model of
      Nothing -> pure unit
      Just model -> do
        let nextYear = state.currentYear + 1
        if nextYear > model.yearRange.max then
          -- Loop back to beginning
          H.modify_ _ { currentYear = model.yearRange.min }
        else
          H.modify_ _ { currentYear = nextYear }

        handleAction Render

  HoverNation nationName -> do
    H.modify_ _ { hoveredNation = nationName }

  ToggleNationSelection nation -> do
    H.modify_ \s ->
      let
        newSelected =
          if Set.member nation s.selectedNations
          then Set.delete nation s.selectedNations
          else Set.insert nation s.selectedNations
      in
        s { selectedNations = newSelected }

  SetAnimationSpeed speed -> do
    H.modify_ _ { animationSpeed = speed }

  Render -> do
    -- Update D3 visualization with transitions
    state <- H.get
    case state.model, state.hoverListener of
      Just model, Just listener -> do
        let onHover = \name -> HS.notify listener (HoverNation (Just name))
        let onLeave = \_ -> HS.notify listener (HoverNation Nothing)
        liftEffect $ updateVisualization "wealth-health-viz" state.currentYear model onHover onLeave
      _, _ -> pure unit

-- FFI imports that would be needed for D3 rendering
foreign import log :: String -> Effect Unit

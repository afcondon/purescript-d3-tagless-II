module Component.WealthHealth.WealthHealth where

import Prelude

import Control.Monad.Rec.Class (forever)
import D3.Viz.TreeAPI.WealthHealthDraw as Draw
import Data.Array (find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Component.WealthHealth.Actions (Action(..))
import Component.WealthHealth.Data (getAllNationsAtYear, getNationAtYear, loadNationsData)
import Component.WealthHealth.HTML (renderControlPanel, renderLegend, formatNumber, formatPopulation)
import Component.WealthHealth.State (State, LabelMode(..), initialState)
import Component.WealthHealth.Types (NationPoint, WealthHealthModel, regionColor, regionName)

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
    [ -- Page Header
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
                Just _ ->
                  HH.div
                    [ HP.classes [ HH.ClassName "wealth-health-viz-wrapper" ] ]
                    [ HH.div
                        [ HP.id "wealth-health-viz"
                        , HP.classes
                            [ HH.ClassName "wealth-health-viz-container"
                            , HH.ClassName $ case state.labelMode of
                                AlwaysShow -> "labels-always-show"
                                OnHoverOnly -> ""
                            ]
                        ]
                        []  -- PSD3 will render SVG here
                    , renderTooltip state
                    ]
            ]
        ]

    -- Credits section
    , HH.footer
        [ HP.classes [ HH.ClassName "wealth-health-credits" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "wealth-health-credits__tribute" ] ]
            [ HH.text "In memory of "
            , HH.a
                [ HP.href "https://youtu.be/usdJgEwMinM"
                , HP.target "_blank"
                , HP.classes [ HH.ClassName "wealth-health-credits__link" ]
                ]
                [ HH.text "Hans Rosling" ]
            , HH.text " (1948-2017), whose famous TED talk inspired this visualization."
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "wealth-health-credits__adaptation" ] ]
            [ HH.text "D3 adaptation by "
            , HH.a
                [ HP.href "https://observablehq.com/@mbostock/the-wealth-health-of-nations"
                , HP.target "_blank"
                , HP.classes [ HH.ClassName "wealth-health-credits__link" ]
                ]
                [ HH.text "Mike Bostock" ]
            , HH.text ". PureScript implementation demonstrates the PS<$>D3 library."
            ]
        ]
    ]

-- | Render tooltip for hovered nation (Halogen-based, backup to library tooltip)
renderTooltip :: forall m. State -> H.ComponentHTML Action () m
renderTooltip state =
  case state.hoveredNation of
    Nothing -> HH.text ""
    Just nationName ->
      case state.model of
        Nothing -> HH.text ""
        Just model ->
          case getNationData nationName state.currentYear model of
            Nothing -> HH.text ""
            Just nation ->
              HH.div
                [ HP.classes [ HH.ClassName "wealth-health-tooltip-halogen" ] ]
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

-- | Convert NationPoint to visualization data with color
nationPointToDrawData :: NationPoint -> Draw.NationPoint
nationPointToDrawData np =
  { name: np.name
  , income: np.income
  , population: np.population
  , lifeExpectancy: np.lifeExpectancy
  , regionColor: regionColor np.region
  , region: regionName np.region
  }

-- | Handle actions
handleAction :: forall o m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load data
    result <- H.liftAff loadNationsData
    case result of
      Left err -> do
        liftEffect $ Console.log $ "Error loading data: " <> err
        pure unit
      Right model -> do
        handleAction (DataLoaded model)

  DataLoaded model -> do
    H.modify_ _ { model = Just model }
    H.modify_ _ { currentYear = model.yearRange.min }

    -- Give Halogen time to render the DOM with the viz container
    H.liftAff $ Aff.delay (Milliseconds 100.0)

    -- Initialize the visualization and store the update function
    updateFn <- liftEffect $ Draw.initWealthHealth "#wealth-health-viz"
    H.modify_ _ { vizUpdateFn = Just updateFn }

    -- Draw initial visualization
    handleAction Render

  DataLoadFailed err -> do
    liftEffect $ Console.log $ "Data load failed: " <> err
    pure unit

  SetYear year -> do
    H.modify_ _ { currentYear = year }
    handleAction Render

  TogglePlay -> do
    state <- H.get
    let newPlaying = not state.playing
    H.modify_ _ { playing = newPlaying }

    when newPlaying do
      let intervalMs = 1000.0 / state.animationSpeed
      { emitter, listener } <- H.liftEffect HS.create
      subscriptionId <- H.subscribe emitter
      H.modify_ _ { animationSubscriptionId = Just subscriptionId }

      void $ H.liftAff $ Aff.forkAff $ forever do
        Aff.delay (Milliseconds intervalMs)
        H.liftEffect $ HS.notify listener Tick

    when (not newPlaying) do
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

  ToggleLabels -> do
    H.modify_ \s ->
      s { labelMode = case s.labelMode of
            AlwaysShow -> OnHoverOnly
            OnHoverOnly -> AlwaysShow
        }
    handleAction Render

  Render -> do
    state <- H.get
    case state.model, state.vizUpdateFn of
      Just model, Just updateFn -> do
        let nations = getAllNationsAtYear state.currentYear model
        let drawData = map nationPointToDrawData nations
        liftEffect $ updateFn drawData
      _, _ -> pure unit

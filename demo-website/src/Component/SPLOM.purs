-- | SPLOM Component
-- |
-- | Brushable Scatterplot Matrix visualization for the Palmer Penguins dataset.
-- | Demonstrates d3-brush for interactive cross-filtering across multiple views.
module Component.SPLOM where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import D3.Viz.SPLOM.Data (loadPenguins)
import D3.Viz.SPLOM.Types (Penguin, NumericDimension(..), allDimensions)
import D3.Viz.SPLOM.Render (renderSPLOM, clearSPLOMBrush, getSelectedCount, getTotalCount, SPLOMHandle)

-- | Component state
type State =
  { penguins :: Array Penguin
  , loading :: Boolean
  , error :: Maybe String
  , splomHandle :: Maybe SPLOMHandle
  , selectedCount :: Int
  , totalCount :: Int
  }

-- | Component actions
data Action
  = Initialize
  | ClearBrush
  | UpdateCounts

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { penguins: []
      , loading: true
      , error: Nothing
      , splomHandle: Nothing
      , selectedCount: 0
      , totalCount: 0
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall w. State -> HH.HTML w Action
render state =
  HH.div
    [ HP.classes [ HH.ClassName "splom-page" ] ]
    [ -- Site Navigation
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Brushable SPLOM"
        }

    -- Main content
    , HH.div
        [ HP.classes [ HH.ClassName "splom-layout" ] ]
        [ -- Info panel
          HH.div
            [ HP.classes [ HH.ClassName "splom-panel" ] ]
            [ HH.h2_ [ HH.text "Palmer Penguins" ]
            , HH.p
                [ HP.classes [ HH.ClassName "splom-description" ] ]
                [ HH.text "Explore relationships between penguin measurements using a scatterplot matrix. "
                , HH.text "Click and drag to brush-select points in any cell. Selected points highlight across all views."
                ]

            -- Selection info
            , if state.totalCount > 0
                then HH.div
                  [ HP.classes [ HH.ClassName "splom-stats" ] ]
                  [ HH.span_ [ HH.text $ show state.selectedCount <> " / " <> show state.totalCount <> " penguins" ]
                  , if state.selectedCount < state.totalCount
                      then HH.button
                        [ HP.classes [ HH.ClassName "splom-clear-btn" ]
                        , HE.onClick \_ -> ClearBrush
                        ]
                        [ HH.text "Clear Selection" ]
                      else HH.text ""
                  ]
                else HH.text ""

            -- Legend description
            , HH.div
                [ HP.classes [ HH.ClassName "splom-legend-info" ] ]
                [ HH.h4_ [ HH.text "Species" ]
                , HH.ul_
                    [ HH.li_ [ HH.span [ HP.style "color: #1f77b4" ] [ HH.text "●" ], HH.text " Adélie" ]
                    , HH.li_ [ HH.span [ HP.style "color: #ff7f0e" ] [ HH.text "●" ], HH.text " Gentoo" ]
                    , HH.li_ [ HH.span [ HP.style "color: #2ca02c" ] [ HH.text "●" ], HH.text " Chinstrap" ]
                    ]
                ]

            -- Instructions
            , HH.div
                [ HP.classes [ HH.ClassName "splom-instructions" ] ]
                [ HH.h4_ [ HH.text "How to Use" ]
                , HH.ul_
                    [ HH.li_ [ HH.text "Drag to create a brush selection" ]
                    , HH.li_ [ HH.text "Points outside selection fade" ]
                    , HH.li_ [ HH.text "Diagonal shows dimension labels" ]
                    , HH.li_ [ HH.text "Click elsewhere to clear" ]
                    ]
                ]
            ]

        -- Visualization
        , HH.div
            [ HP.classes [ HH.ClassName "splom-viz" ] ]
            [ -- Loading state
              if state.loading
                then HH.div
                  [ HP.classes [ HH.ClassName "splom-loading" ] ]
                  [ HH.text "Loading penguins..." ]
                else HH.text ""

            -- Error state
            , case state.error of
                Just err -> HH.div
                  [ HP.classes [ HH.ClassName "splom-error" ] ]
                  [ HH.text $ "Error: " <> err ]
                Nothing -> HH.text ""

            -- SPLOM container
            , HH.div
                [ HP.id "splom-container"
                , HP.classes [ HH.ClassName "splom-svg-container" ]
                ]
                []
            ]
        ]
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect $ Console.log "Loading penguins..."
    result <- liftAff loadPenguins

    case result of
      Left err -> do
        liftEffect $ Console.log $ "Failed to load penguins: " <> err
        H.modify_ _ { loading = false, error = Just err }

      Right penguins -> do
        liftEffect $ Console.log $ "Loaded " <> show (length penguins) <> " penguins"
        H.modify_ _ { penguins = penguins, loading = false }

        -- Render SPLOM
        handle <- liftEffect $ renderSPLOM "#splom-container" penguins allDimensions
        total <- liftEffect $ getTotalCount handle
        H.modify_ _ { splomHandle = Just handle, totalCount = total, selectedCount = total }

  ClearBrush -> do
    state <- H.get
    case state.splomHandle of
      Nothing -> pure unit
      Just handle -> do
        liftEffect $ clearSPLOMBrush handle
        H.modify_ _ { selectedCount = state.totalCount }

  UpdateCounts -> do
    state <- H.get
    case state.splomHandle of
      Nothing -> pure unit
      Just handle -> do
        count <- liftEffect $ getSelectedCount handle
        H.modify_ _ { selectedCount = count }

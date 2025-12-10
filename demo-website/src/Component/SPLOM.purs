-- | SPLOM Component
-- |
-- | Brushable Scatterplot Matrix visualization for the Palmer Penguins dataset.
-- | Demonstrates d3-brush for interactive cross-filtering across multiple views.
-- |
-- | This component uses the pure PureScript SPLOM implementation which demonstrates:
-- | - PSD3.Scale for scales
-- | - PSD3v2.Brush for brush interaction
-- | - Tree API for declarative rendering
-- |
-- | State is managed by Halogen, not by Effect.Ref.
module Component.SPLOM where

import Prelude

import Data.Array (length, find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PSD3.Scale (ContinuousScale, invert)
import PSD3.Shared.SiteNav as SiteNav
import PSD3v2.Brush (BrushSelection(..))
import D3.Viz.SPLOM.Data (loadPenguins)
import D3.Viz.SPLOM.Types (Penguin, NumericDimension)
import D3.Viz.SPLOM.SPLOM as SPLOM

-- | Component state - SPLOM state is stored directly here
type State =
  { penguins :: Array Penguin
  , loading :: Boolean
  , error :: Maybe String
  , splomState :: Maybe SPLOM.SPLOMState
  }

-- | Component actions
data Action
  = Initialize
  | ClearBrush
  | BrushMove BrushSelection SPLOM.Cell
  | BrushEnd BrushSelection SPLOM.Cell

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { penguins: []
      , loading: true
      , error: Nothing
      , splomState: Nothing
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
            , case state.splomState of
                Just splomSt ->
                  let
                    selectedCount = SPLOM.getSelectedCount splomSt
                    totalCount = SPLOM.getTotalCount splomSt
                  in
                    HH.div
                      [ HP.classes [ HH.ClassName "splom-stats" ] ]
                      [ HH.span_ [ HH.text $ show selectedCount <> " / " <> show totalCount <> " penguins" ]
                      , if selectedCount < totalCount
                          then HH.button
                            [ HP.classes [ HH.ClassName "splom-clear-btn" ]
                            , HE.onClick \_ -> ClearBrush
                            ]
                            [ HH.text "Clear Selection" ]
                          else HH.text ""
                      ]
                Nothing -> HH.text ""

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

            -- Credits
            , HH.div
                [ HP.classes [ HH.ClassName "splom-credits" ] ]
                [ HH.p_
                    [ HH.text "The scatterplot matrix (SPLOM) shows pairwise correlations for multi-dimensional data; "
                    , HH.text "each cell is a scatterplot where x encodes the column's dimension and y encodes the row's dimension. "
                    , HH.text "This matrix shows "
                    , HH.a
                        [ HP.href "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0090081"
                        , HP.target "_blank"
                        ]
                        [ HH.text "Kristen Gorman's data" ]
                    , HH.text " on penguins near Palmer Station in Antarctica."
                    ]
                , HH.p
                    [ HP.classes [ HH.ClassName "splom-source" ] ]
                    [ HH.text "Based on "
                    , HH.a
                        [ HP.href "https://observablehq.com/@d3/brushable-scatterplot-matrix"
                        , HP.target "_blank"
                        ]
                        [ HH.text "Observable's Brushable Scatterplot Matrix" ]
                    , HH.text " by Mike Bostock."
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

        -- Create initial SPLOM state (pure function)
        let splomState = SPLOM.initialState "#splom-container" penguins

        H.modify_ _ { penguins = penguins, loading = false, splomState = Just splomState }

        -- Render the visualization
        liftEffect $ SPLOM.renderSPLOM splomState

        -- Set up brush callbacks using Halogen subscriptions
        emitter <- setupBrushCallbacks splomState
        void $ H.subscribe emitter

  ClearBrush -> do
    state <- H.get
    case state.splomState of
      Nothing -> pure unit
      Just splomSt -> do
        -- Clear the visual brush selection (the D3 brush rect)
        liftEffect $ SPLOM.clearBrushSelection splomSt.brushHandles

        -- Update state to clear selection
        let newSplomState = splomSt { selection = Nothing, activeBrushCell = Nothing }
        H.modify_ _ { splomState = Just newSplomState }

        -- Update point visibility (don't re-render, preserves brushes)
        liftEffect $ SPLOM.updatePointVisibility newSplomState

  BrushMove selection cell -> do
    state <- H.get
    case state.splomState of
      Nothing -> pure unit
      Just splomSt -> do
        -- Convert pixel selection to data bounds
        let newSelection = case selection of
              Selection2D sel -> do
                xScale <- findScaleForDim cell.dimX splomSt.scales
                yScale <- findScaleForDim cell.dimY splomSt.scales
                xMin <- invert xScale sel.x0
                xMax <- invert xScale sel.x1
                yMin <- invert yScale sel.y0
                yMax <- invert yScale sel.y1
                Just { dimX: cell.dimX, dimY: cell.dimY, xMin, xMax, yMin, yMax }
              NoSelection -> Nothing
              _ -> splomSt.selection

        -- Update state (don't clear other brushes - causes event cascade)
        let newSplomState = splomSt { selection = newSelection, activeBrushCell = Just cell }
        H.modify_ _ { splomState = Just newSplomState }

        -- Update point visibility (fast, preserves brushes)
        liftEffect $ SPLOM.updatePointVisibility newSplomState

  BrushEnd selection _cell -> do
    case selection of
      NoSelection -> do
        -- Brush was cleared - update point visibility
        state <- H.get
        case state.splomState of
          Nothing -> pure unit
          Just splomSt -> do
            let newSplomState = splomSt { selection = Nothing, activeBrushCell = Nothing }
            H.modify_ _ { splomState = Just newSplomState }
            liftEffect $ SPLOM.updatePointVisibility newSplomState
      _ -> pure unit  -- Selection active, already handled in BrushMove

-- | Set up brush callbacks that dispatch Halogen actions
setupBrushCallbacks :: forall o m. MonadAff m => SPLOM.SPLOMState -> H.HalogenM State Action () o m (HS.Emitter Action)
setupBrushCallbacks splomState = do
  { emitter, listener } <- liftEffect HS.create

  -- Attach brushes with callbacks
  handles <- liftEffect $ SPLOM.attachBrushes
    splomState
    (\selection cell -> HS.notify listener (BrushMove selection cell))
    (\selection cell -> HS.notify listener (BrushEnd selection cell))

  -- Update state with brush handles
  H.modify_ \s -> case s.splomState of
    Nothing -> s
    Just st -> s { splomState = Just (st { brushHandles = handles }) }

  pure emitter

-- | Helper to find scale for a dimension
findScaleForDim :: NumericDimension -> Array SPLOM.DimensionScale -> Maybe ContinuousScale
findScaleForDim dim scales =
  map _.scale $ find (\s -> s.dimension == dim) scales

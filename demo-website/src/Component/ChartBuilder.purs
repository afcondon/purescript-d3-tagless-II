module Component.ChartBuilder
  ( component
  ) where

import Prelude

import Component.ChartBuilder.Data (allDatasets, getDataset)
import Component.ChartBuilder.Rendering (renderAstVisualization, renderChartPlaceholder)
import Component.ChartBuilder.Templates (getRecipe, getExplanation, generalTips)
import Component.ChartBuilder.Types (Action(..), ChartType(..), Dataset, DatasetId(..), ParseResult, State, chartTypeLabel, datasetIdLabel, parseDatasetId)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import EmmetParser.Converter (convertToTree)
import EmmetParser.Parser (parseEmmet)
import EmmetParser.Validator (validate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- =============================================================================
-- State
-- =============================================================================

initialState :: forall i. i -> State
initialState _ =
  { emmetInput: ""
  , selectedChartType: BarChart
  , selectedDataset: FruitSales
  , parseResult: Nothing
  , showRecipe: true
  }

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load the initial recipe for BarChart
    let recipe = getRecipe BarChart
    H.modify_ _ { emmetInput = recipe }
    handleAction (UpdateEmmet recipe)

  UpdateEmmet input -> do
    H.modify_ _ { emmetInput = input }
    if String.null input
      then H.modify_ _ { parseResult = Nothing }
      else do
        let parseResult = parseEmmet input
        case parseResult of
          Left err -> do
            H.modify_ _ { parseResult = Just (Left err) }
          Right expr -> do
            let validationResult = validate expr
            case validationResult of
              Left err -> H.modify_ _ { parseResult = Just (Right (Left err)) }
              Right validExpr -> do
                let tree = convertToTree validExpr
                H.modify_ _ { parseResult = Just (Right (Right tree)) }
                -- Render AST visualization (placeholder for v1)
                renderAstVisualization
                -- Render chart preview with current dataset
                state <- H.get
                case getDataset state.selectedDataset of
                  Just dataset -> renderChartPlaceholder dataset
                  Nothing -> pure unit

  SelectChartType chartType -> do
    H.modify_ _ { selectedChartType = chartType }
    -- Don't auto-load recipe, let user click "Load Recipe" button

  SelectDataset datasetId -> do
    H.modify_ _ { selectedDataset = datasetId }
    -- Re-render chart with new dataset if we have a valid tree
    state <- H.get
    case state.parseResult of
      Just (Right (Right _tree)) ->
        case getDataset datasetId of
          Just dataset -> renderChartPlaceholder dataset
          Nothing -> pure unit
      _ -> pure unit

  CopyRecipe -> do
    state <- H.get
    let recipe = getRecipe state.selectedChartType
    liftEffect $ copyToClipboard recipe

  LoadRecipe chartType -> do
    let recipe = getRecipe chartType
    H.modify_ _ { emmetInput = recipe, selectedChartType = chartType }
    handleAction (UpdateEmmet recipe)

  ResetToRecipe -> do
    state <- H.get
    let recipe = getRecipe state.selectedChartType
    H.modify_ _ { emmetInput = recipe }
    handleAction (UpdateEmmet recipe)

  ToggleRecipe -> do
    H.modify_ \s -> s { showRecipe = not s.showRecipe }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.classes [ HH.ClassName "chart-builder-page" ] ]
    [ -- Header with navigation
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Interactive Chart Builder"
        }

    , HH.main [ HP.classes [ HH.ClassName "chart-builder-content" ] ]
        [ -- Title and chart type tabs
          HH.div [ HP.classes [ HH.ClassName "builder-header" ] ]
            [ HH.h1 [ HP.classes [ HH.ClassName "builder-title" ] ]
                [ HH.text "Interactive Chart Builder" ]
            , HH.p [ HP.classes [ HH.ClassName "builder-subtitle" ] ]
                [ HH.text "Learn Emmet syntax by building charts with real data" ]
            , HH.div [ HP.classes [ HH.ClassName "chart-type-tabs" ] ]
                [ renderTab BarChart
                , renderTab LineChart
                , renderTab ScatterChart
                ]
            ]

        , -- Three-column layout
          HH.div [ HP.classes [ HH.ClassName "builder-panels" ] ]
            [ -- Left: Recipe
              renderRecipePanel state

            , -- Center: Emmet Editor
              renderEditorPanel state

            , -- Right: Visualization + AST
              renderPreviewPanel state
            ]

        , -- Bottom: Dataset selector and tips
          HH.div [ HP.classes [ HH.ClassName "builder-footer" ] ]
            [ renderDatasetSelector state
            , renderTips
            ]
        ]
    ]
  where
    renderTab :: ChartType -> H.ComponentHTML Action () m
    renderTab chartType =
      HH.button
        [ HP.classes
            [ HH.ClassName "chart-type-tab"
            , HH.ClassName if state.selectedChartType == chartType then "active" else ""
            ]
        , HE.onClick \_ -> SelectChartType chartType
        ]
        [ HH.text (chartTypeLabel chartType) ]

-- =============================================================================
-- Recipe Panel
-- =============================================================================

renderRecipePanel :: forall m. State -> H.ComponentHTML Action () m
renderRecipePanel state =
  HH.div [ HP.classes [ HH.ClassName "recipe-panel" ] ]
    [ HH.div [ HP.classes [ HH.ClassName "panel-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "panel-title" ] ]
            [ HH.text "Recipe" ]
        ]
    , HH.pre [ HP.classes [ HH.ClassName "recipe-code" ] ]
        [ HH.code_ [ HH.text (getRecipe state.selectedChartType) ] ]
    , HH.div [ HP.classes [ HH.ClassName "recipe-actions" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "button", HH.ClassName "button-primary" ]
            , HE.onClick \_ -> CopyRecipe
            ]
            [ HH.text "📋 Copy Recipe" ]
        , HH.button
            [ HP.classes [ HH.ClassName "button", HH.ClassName "button-secondary" ]
            , HE.onClick \_ -> LoadRecipe state.selectedChartType
            ]
            [ HH.text "↓ Load to Editor" ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "recipe-explanation" ] ]
        [ HH.p [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "How it works:" ]
        , HH.pre [ HP.classes [ HH.ClassName "explanation-text" ] ]
            [ HH.text (getExplanation state.selectedChartType) ]
        ]
    ]

-- =============================================================================
-- Editor Panel
-- =============================================================================

renderEditorPanel :: forall m. State -> H.ComponentHTML Action () m
renderEditorPanel state =
  HH.div [ HP.classes [ HH.ClassName "editor-panel" ] ]
    [ HH.div [ HP.classes [ HH.ClassName "panel-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "panel-title" ] ]
            [ HH.text "Try It Yourself" ]
        ]
    , HH.textarea
        [ HP.classes [ HH.ClassName "emmet-editor" ]
        , HP.value state.emmetInput
        , HP.placeholder "Paste recipe and experiment..."
        , HP.rows 8
        , HP.spellcheck false
        , HE.onValueInput UpdateEmmet
        ]
    , renderParseStatus state.parseResult
    , HH.div [ HP.classes [ HH.ClassName "editor-controls" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "button", HH.ClassName "button-secondary" ]
            , HE.onClick \_ -> ResetToRecipe
            ]
            [ HH.text "↺ Reset to Recipe" ]
        ]
    ]

-- =============================================================================
-- Preview Panel
-- =============================================================================

renderPreviewPanel :: forall m. State -> H.ComponentHTML Action () m
renderPreviewPanel state =
  HH.div [ HP.classes [ HH.ClassName "preview-panel" ] ]
    [ HH.div [ HP.classes [ HH.ClassName "panel-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "panel-title" ] ]
            [ HH.text "Visualization" ]
        ]
    , HH.div
        [ HP.id "chart-preview"
        , HP.classes [ HH.ClassName "chart-preview" ]
        ]
        [ HH.div [ HP.classes [ HH.ClassName "preview-placeholder" ] ]
            [ HH.text "Chart will appear here" ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "panel-header", HH.ClassName "ast-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "panel-title" ] ]
            [ HH.text "AST Structure" ]
        ]
    , HH.div
        [ HP.id "ast-tree-preview"
        , HP.classes [ HH.ClassName "ast-preview" ]
        ]
        [ HH.div [ HP.classes [ HH.ClassName "preview-placeholder" ] ]
            [ HH.text "AST tree will appear here" ]
        ]
    ]

-- =============================================================================
-- Parse Status Display
-- =============================================================================

renderParseStatus :: forall w i. Maybe ParseResult -> HH.HTML w i
renderParseStatus = case _ of
  Nothing ->
    HH.div [ HP.classes [ HH.ClassName "parse-status", HH.ClassName "status-empty" ] ]
      [ HH.text "Enter an expression to see results" ]

  Just (Left parseError) ->
    HH.div [ HP.classes [ HH.ClassName "parse-status", HH.ClassName "status-error" ] ]
      [ HH.strong_ [ HH.text "Parse Error: " ]
      , HH.text (show parseError)
      ]

  Just (Right (Left validationError)) ->
    HH.div [ HP.classes [ HH.ClassName "parse-status", HH.ClassName "status-error" ] ]
      [ HH.strong_ [ HH.text "Validation Error: " ]
      , HH.text (show validationError)
      ]

  Just (Right (Right _tree)) ->
    HH.div [ HP.classes [ HH.ClassName "parse-status", HH.ClassName "status-success" ] ]
      [ HH.text "✓ Valid expression" ]

-- =============================================================================
-- Dataset Selector
-- =============================================================================

renderDatasetSelector :: forall m. State -> H.ComponentHTML Action () m
renderDatasetSelector state =
  HH.div [ HP.classes [ HH.ClassName "dataset-selector" ] ]
    [ HH.label [ HP.classes [ HH.ClassName "dataset-label" ] ]
        [ HH.text "Dataset: " ]
    , HH.select
        [ HP.classes [ HH.ClassName "dataset-select" ]
        , HE.onValueChange (SelectDataset <<< parseDatasetId)
        ]
        (map renderDatasetOption allDatasets)
    , case getDataset state.selectedDataset of
        Nothing -> HH.text ""
        Just dataset ->
          HH.span [ HP.classes [ HH.ClassName "dataset-description" ] ]
            [ HH.text dataset.description ]
    ]
  where
    renderDatasetOption :: Dataset -> H.ComponentHTML Action () m
    renderDatasetOption dataset =
      HH.option
        [ HP.value (show dataset.id)
        , HP.selected (state.selectedDataset == dataset.id)
        ]
        [ HH.text dataset.name ]

-- =============================================================================
-- Tips Section
-- =============================================================================

renderTips :: forall w i. HH.HTML w i
renderTips =
  HH.div [ HP.classes [ HH.ClassName "tips-section" ] ]
    [ HH.strong [ HP.classes [ HH.ClassName "tips-title" ] ]
        [ HH.text "💡 Tips:" ]
    , HH.ul [ HP.classes [ HH.ClassName "tips-list" ] ]
        (map renderTip generalTips)
    ]
  where
    renderTip :: String -> HH.HTML w i
    renderTip tip =
      HH.li [ HP.classes [ HH.ClassName "tip-item" ] ]
        [ HH.text tip ]

-- =============================================================================
-- Foreign Imports (Clipboard)
-- =============================================================================

foreign import copyToClipboard :: String -> Effect Unit

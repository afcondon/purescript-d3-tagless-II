module PSD3.CodeAtlas.CodeAtlas where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.CodeAtlas.Actions (Action(..))
import PSD3.CodeAtlas.Data (loadDeclarations, loadFunctionCalls, loadModules)
import PSD3.CodeAtlas.State (State, initialState)
import PSD3.CodeAtlas.Tabs.Declarations as DeclarationsTab
import PSD3.CodeAtlas.Tabs.ModuleGraph as ModuleGraphTab
import PSD3.CodeAtlas.Tabs.InteractiveGraph as InteractiveGraphTab
import PSD3.CodeAtlas.Types (AtlasTab(..))
import PSD3.Interpreter.D3 (runWithD3_Simulation)

-- | Code Atlas component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- | Render the component
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "code-atlas-page" ] ]
    [ -- Header
      HH.header
        [ HP.classes [ HH.ClassName "code-atlas-header" ] ]
        [ HH.h1_ [ HH.text "Code Atlas" ]
        , HH.p
            [ HP.classes [ HH.ClassName "code-atlas-subtitle" ] ]
            [ HH.text "Explore the codebase through declarations, function calls, and type dependencies" ]
        ]

    -- Tabs
    , HH.div
        [ HP.classes [ HH.ClassName "code-atlas-tabs" ] ]
        [ renderTab DeclarationsTab state.activeTab
        , renderTab VisualizationTab state.activeTab
        , renderTab InteractiveGraphTab state.activeTab
        ]

    -- Content
    , HH.div
        [ HP.classes [ HH.ClassName "code-atlas-content" ] ]
        [ case state.loading of
            true ->
              HH.div
                [ HP.classes [ HH.ClassName "code-atlas-loading" ] ]
                [ HH.text "Loading codebase data..." ]

            false -> case state.error of
              Just err ->
                HH.div
                  [ HP.classes [ HH.ClassName "code-atlas-error" ] ]
                  [ HH.h3_ [ HH.text "Error Loading Data" ]
                  , HH.p_ [ HH.text err ]
                  , HH.p_ [ HH.text "Make sure you've generated the data files:" ]
                  , HH.pre_ [ HH.text "npm run build\nnode scripts/generate-spago-data.js" ]
                  ]

              Nothing -> case state.activeTab of
                DeclarationsTab ->
                  DeclarationsTab.render state

                VisualizationTab ->
                  HH.div
                    [ HP.classes [ HH.ClassName "module-graph-container" ] ]
                    [ HH.div
                        [ HP.classes [ HH.ClassName "svg-container" ] ]
                        []
                    ]

                InteractiveGraphTab ->
                  HH.div
                    [ HP.classes [ HH.ClassName "interactive-graph-container" ] ]
                    [ HH.div
                        [ HP.classes [ HH.ClassName "svg-container" ] ]
                        []
                    ]
        ]
    ]

-- | Render a single tab
renderTab :: forall m. AtlasTab -> AtlasTab -> H.ComponentHTML Action () m
renderTab tab activeTab =
  HH.button
    [ HP.classes
        [ HH.ClassName "code-atlas-tab"
        , HH.ClassName if tab == activeTab then "code-atlas-tab--active" else ""
        ]
    , HE.onClick \_ -> SetActiveTab tab
    ]
    [ HH.text $ show tab ]

-- | Handle actions
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load all three data files in parallel
    declarationsResult <- H.liftAff loadDeclarations
    functionCallsResult <- H.liftAff loadFunctionCalls
    modulesResult <- H.liftAff loadModules

    case declarationsResult, functionCallsResult, modulesResult of
      Right decls, Right calls, Right modules -> do
        H.modify_ _ { loading = false }
        handleAction (DataLoaded decls calls modules)

      Left err, _, _ -> do
        H.modify_ _ { loading = false, error = Just err }

      _, Left err, _ -> do
        H.modify_ _ { loading = false, error = Just err }

      _, _, Left err -> do
        H.modify_ _ { loading = false, error = Just err }

  DataLoaded declarationsData functionCallsData moduleGraphData -> do
    H.modify_ _
      { declarationsData = Just declarationsData
      , functionCallsData = Just functionCallsData
      , moduleGraphData = Just moduleGraphData
      }

  DataLoadFailed err -> do
    H.modify_ _ { error = Just err, loading = false }

  SetActiveTab tab -> do
    H.modify_ _ { activeTab = tab }

    -- Draw visualizations when switching tabs
    case tab of
      VisualizationTab -> do
        state <- H.get
        case state.moduleGraphData of
          Just graphData ->
            runWithD3_Simulation do
              ModuleGraphTab.drawModuleGraph graphData "div.svg-container"
          Nothing -> pure unit

      InteractiveGraphTab -> do
        state <- H.get
        case state.moduleGraphData of
          Just graphData ->
            runWithD3_Simulation do
              InteractiveGraphTab.drawInteractiveGraph graphData "div.svg-container"
          Nothing -> pure unit

      _ -> pure unit

  SetSearchQuery query -> do
    H.modify_ _ { searchQuery = query }

  SetKindFilter filter -> do
    H.modify_ _ { selectedKindFilter = filter }

  SetModuleFilter filter -> do
    H.modify_ _ { selectedModuleFilter = filter }

  SetSourceFilter filter -> do
    H.modify_ _ { selectedSourceFilter = filter }

  ClearFilters -> do
    H.modify_ _
      { searchQuery = ""
      , selectedKindFilter = Nothing
      , selectedModuleFilter = Nothing
      , selectedSourceFilter = Nothing
      }

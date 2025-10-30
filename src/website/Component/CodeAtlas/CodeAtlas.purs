module PSD3.CodeAtlas.CodeAtlas where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PSD3.CodeAtlas.Actions (Action(..))
import PSD3.CodeAtlas.Data (loadDeclarations, loadFunctionCalls, loadModules)
import PSD3.CodeAtlas.State (State, initialState)
import PSD3.CodeAtlas.Tabs.Declarations as DeclarationsTab
import PSD3.CodeAtlas.Tabs.ModuleGraph as ModuleGraphTab
import PSD3.CodeAtlas.Tabs.InteractiveGraph as InteractiveGraphTab
import PSD3.CodeAtlas.Tabs.ExpandableBubbles as ExpandableBubblesTab
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
        , renderTab ExpandableBubblesTab state.activeTab
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

              Nothing ->
                HH.div []
                  [ renderTabContent state
                  , renderFloatingPanels state
                  ]
        ]
    ]

renderTabContent :: forall m. State -> H.ComponentHTML Action () m
renderTabContent state =
  case state.activeTab of
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

    ExpandableBubblesTab ->
      HH.div
        [ HP.classes [ HH.ClassName "expandable-bubbles-container" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "svg-container" ] ]
            []
        ]

-- | Render floating panels (only for ExpandableBubblesTab)
renderFloatingPanels :: forall m. State -> H.ComponentHTML Action () m
renderFloatingPanels state =
  if state.activeTab == ExpandableBubblesTab
    then HH.div_
      [ renderLegendPanel
      , renderDetailsPanel state
      ]
    else HH.text ""

-- | Render the legend panel (top-right)
renderLegendPanel :: forall m. H.ComponentHTML Action () m
renderLegendPanel =
  HH.div
    [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "editorial" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
        [ HH.text "Declaration Types" ]
    , HH.div
        [ HP.classes [ HH.ClassName "legend-items" ] ]
        [ renderLegendItem "Functions/Values" "#2196F3"
        , renderLegendItem "Foreign Functions" "#00BCD4"
        , renderLegendItem "Data Types" "#4CAF50"
        , renderLegendItem "Type Classes" "#9C27B0"
        , renderLegendItem "Type Synonyms" "#FF9800"
        , renderLegendItem "Instances" "#E91E63"
        ]
    ]

-- | Render a single legend item
renderLegendItem :: forall m. String -> String -> H.ComponentHTML Action () m
renderLegendItem label color =
  HH.div
    [ HP.classes [ HH.ClassName "legend-item" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "legend-color" ]
        , HP.style $ "background-color: " <> color
        ]
        []
    , HH.span
        [ HP.classes [ HH.ClassName "legend-label" ] ]
        [ HH.text label ]
    ]

-- | Render the details panel (bottom-right, conditional)
renderDetailsPanel :: forall m. State -> H.ComponentHTML Action () m
renderDetailsPanel state =
  case state.hoveredModule of
    Nothing -> HH.text ""
    Just info ->
      HH.div
        [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--bottom-right", HH.ClassName "editorial" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
            [ HH.text info.moduleName ]
        , HH.div
            [ HP.classes [ HH.ClassName "details-section" ] ]
            [ HH.h4_ [ HH.text "Dependencies" ]
            , if Array.length info.dependencies == 0
                then HH.p [ HP.classes [ HH.ClassName "empty-list" ] ] [ HH.text "(none)" ]
                else HH.ul_
                  (info.dependencies <#> \dep ->
                    HH.li_ [ HH.text dep ]
                  )
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "details-section" ] ]
            [ HH.h4_ [ HH.text "Depended On By" ]
            , if Array.length info.dependedOnBy == 0
                then HH.p [ HP.classes [ HH.ClassName "empty-list" ] ] [ HH.text "(none)" ]
                else HH.ul_
                  (info.dependedOnBy <#> \dep ->
                    HH.li_ [ HH.text dep ]
                  )
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

    -- Trigger visualization for the active tab
    state <- H.get
    handleAction (SetActiveTab state.activeTab)

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

      ExpandableBubblesTab -> do
        state <- H.get
        case state.moduleGraphData, state.declarationsData, state.functionCallsData of
          Just graphData, Just declsData, Just callsData -> do
            -- Create a Ref to hold pending actions from D3 event handlers
            pendingActionRef <- liftEffect $ Ref.new Nothing

            -- Create callbacks that write to the Ref
            let callbacks =
                  { onShowModuleDetails: \moduleName dependencies dependedOnBy ->
                      Ref.write (Just $ ShowModuleDetails { moduleName, dependencies, dependedOnBy }) pendingActionRef
                  , onHideModuleDetails:
                      Ref.write (Just HideModuleDetails) pendingActionRef
                  }

            -- Create a subscription that polls the Ref for pending actions
            { emitter, listener } <- liftEffect HS.create
            _ <- H.subscribe emitter

            -- Start polling loop in background
            void $ H.fork $ H.liftAff $ pollForActions pendingActionRef listener

            -- Draw the visualization
            runWithD3_Simulation do
              ExpandableBubblesTab.drawExpandableBubbles graphData declsData callsData "div.svg-container" callbacks

          _, _, _ -> pure unit

      _ -> pure unit

    where
      -- Poll the Ref for pending actions and emit them
      pollForActions ref listener = do
        delay (Milliseconds 16.0)  -- Poll at ~60fps
        pendingAction <- liftEffect $ Ref.read ref
        case pendingAction of
          Just action -> do
            liftEffect $ Ref.write Nothing ref
            liftEffect $ HS.notify listener action
          Nothing -> pure unit
        pollForActions ref listener  -- Continue polling

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

  ShowModuleDetails info -> do
    H.modify_ _ { hoveredModule = Just info }

  HideModuleDetails -> do
    H.modify_ _ { hoveredModule = Nothing }

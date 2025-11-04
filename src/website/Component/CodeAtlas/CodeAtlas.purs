module PSD3.CodeAtlas.CodeAtlas where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE
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
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

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
    [ -- Navigation Header
      TutorialNav.renderHeader CodeAtlas

    -- Header
    , HH.header
        [ HP.classes [ HH.ClassName "code-atlas-header" ] ]
        [ HH.p
            [ HP.classes [ HH.ClassName "code-atlas-subtitle" ] ]
            [ HH.text "Explore the codebase through declarations, function calls, and type dependencies" ]
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
                HH.div
                  [ HP.classes [ HH.ClassName "code-atlas-container" ] ]
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

-- | Render floating panels (always show control panel, conditionally show others)
renderFloatingPanels :: forall m. State -> H.ComponentHTML Action () m
renderFloatingPanels state =
  HH.div_
    [ renderControlPanel state
    , if state.activeTab == ExpandableBubblesTab
        then HH.div_
          [ renderLegendPanel
          , renderDetailsPanel state
          ]
        else HH.text ""
    , renderContextMenu state
    ]

-- | Render the control panel (top-left) with view tabs
renderControlPanel :: forall m. State -> H.ComponentHTML Action () m
renderControlPanel state =
  HH.div
    [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "editorial" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
        [ HH.text "Views" ]
    , HH.div
        [ HP.classes [ HH.ClassName "control-panel-tabs" ] ]
        [ renderTab DeclarationsTab state.activeTab
        , renderTab VisualizationTab state.activeTab
        , renderTab InteractiveGraphTab state.activeTab
        , renderTab ExpandableBubblesTab state.activeTab
        ]
    , if state.activeTab == VisualizationTab
        then HH.div
          [ HP.classes [ HH.ClassName "control-group" ] ]
          [ HH.h4_ [ HH.text "Layout" ]
          , HH.div
              [ HP.classes [ HH.ClassName "layout-toggle-container" ] ]
              [ HH.button
                  [ HP.classes [ HH.ClassName "layout-toggle-button" ]
                  , HE.onClick \_ -> ToggleGridLayout
                  ]
                  [ HH.text if state.isGridLayout then "Switch to Force Layout" else "Switch to Grid Layout" ]
              , HH.p
                  [ HP.classes [ HH.ClassName "helper-text" ] ]
                  [ HH.text "Toggle between force-directed and grid layouts" ]
              ]
          ]
        else if state.activeTab == ExpandableBubblesTab
          then HH.div
            [ HP.classes [ HH.ClassName "control-group" ] ]
            [ HH.h4_ [ HH.text "Spotlight Mode" ]
            , HH.div
                [ HP.classes [ HH.ClassName "spotlight-toggle-container" ] ]
                [ HH.label
                    [ HP.classes [ HH.ClassName "spotlight-toggle" ] ]
                    [ HH.input
                        [ HP.type_ HP.InputCheckbox
                        , HP.checked state.spotlightModeActive
                        , HP.disabled (not state.spotlightModeActive)
                        , HE.onClick \_ -> ResetToOverview
                        ]
                    , HH.span
                        [ HP.classes [ HH.ClassName "toggle-slider" ] ]
                        []
                    ]
                , HH.p
                    [ HP.classes [ HH.ClassName "helper-text" ] ]
                    [ HH.text if state.spotlightModeActive
                        then "Click to return to overview"
                        else "Click a module to spotlight its dependencies"
                    ]
                ]
            ]
          else HH.text ""
    ]

-- | Render the legend panel (bottom-right)
renderLegendPanel :: forall m. H.ComponentHTML Action () m
renderLegendPanel =
  HH.div
    [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--bottom-right", HH.ClassName "editorial" ] ]
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

-- | Render the details panel (top-right, conditional)
renderDetailsPanel :: forall m. State -> H.ComponentHTML Action () m
renderDetailsPanel state =
  case state.hoveredModule of
    Nothing -> HH.text ""
    Just info ->
      HH.div
        [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "editorial" ] ]
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

-- | Render the context menu (conditional, positioned at click)
renderContextMenu :: forall m. State -> H.ComponentHTML Action () m
renderContextMenu state =
  case state.contextMenu of
    Nothing -> HH.text ""
    Just info ->
      let
        isCurrentSpotlight = state.currentSpotlightModule == Just info.moduleName
        inSpotlightMode = state.spotlightModeActive
        _ = unsafePerformEffect $ Console.log $ "Context menu for " <> info.moduleName <> " - inSpotlightMode: " <> show inSpotlightMode <> ", isCurrentSpotlight: " <> show isCurrentSpotlight <> ", currentSpotlightModule: " <> show state.currentSpotlightModule

        -- Determine keyboard handler based on context
        keyHandler = case inSpotlightMode, isCurrentSpotlight of
          false, _ -> \event ->  -- Not in spotlight mode
            case KE.key event of
              "s" -> SpotlightModuleFromMenu info.moduleName
              "S" -> SpotlightModuleFromMenu info.moduleName
              "Escape" -> HideContextMenu
              _ -> HideContextMenu

          true, true -> \event ->  -- In spotlight mode, clicking current spotlight module
            case KE.key event of
              "b" -> ResetToOverview
              "B" -> ResetToOverview
              "Escape" -> HideContextMenu
              _ -> HideContextMenu

          true, false -> \event ->  -- In spotlight mode, clicking different module
            case KE.key event of
              "a" -> AddDepsToSpotlight info.moduleName
              "A" -> AddDepsToSpotlight info.moduleName
              "f" -> MakeFocusModule info.moduleName
              "F" -> MakeFocusModule info.moduleName
              "Escape" -> HideContextMenu
              _ -> HideContextMenu

        -- Determine menu items based on context
        menuItems = case inSpotlightMode, isCurrentSpotlight of
          false, _ ->  -- Not in spotlight mode
            [ renderContextMenuItem "Spotlight Module" "S" (SpotlightModuleFromMenu info.moduleName)
            ]

          true, true ->  -- In spotlight mode, clicking current spotlight module
            [ renderContextMenuItem "Unspotlight" "B" ResetToOverview
            ]

          true, false ->  -- In spotlight mode, clicking different module
            [ renderContextMenuItem "Add Deps" "A" (AddDepsToSpotlight info.moduleName)
            , renderContextMenuItem "Make Focus" "F" (MakeFocusModule info.moduleName)
            ]
      in
        HH.div_
          [ -- Transparent overlay to capture clicks outside the menu
            HH.div
              [ HP.classes [ HH.ClassName "context-menu-overlay" ]
              , HE.onClick \_ -> HideContextMenu
              ]
              []
          , -- The actual context menu
            HH.div
              [ HP.classes [ HH.ClassName "context-menu", HH.ClassName "editorial" ]
              , HP.style $ "left: " <> show info.x <> "px; top: " <> show info.y <> "px"
              , HP.tabIndex 0  -- Make focusable for keyboard events
              , HE.onKeyDown keyHandler
              ]
              [ HH.div
                  [ HP.classes [ HH.ClassName "context-menu__header" ] ]
                  [ HH.text info.moduleName ]
              , HH.div
                  [ HP.classes [ HH.ClassName "context-menu__items" ] ]
                  menuItems
              ]
          ]

-- | Render a single context menu item
renderContextMenuItem :: forall m. String -> String -> Action -> H.ComponentHTML Action () m
renderContextMenuItem label shortcut action =
  HH.button
    [ HP.classes [ HH.ClassName "context-menu__item" ]
    , HE.onClick \_ -> action
    ]
    [ HH.span
        [ HP.classes [ HH.ClassName "context-menu__label" ] ]
        [ HH.text label ]
    , HH.span
        [ HP.classes [ HH.ClassName "context-menu__shortcut" ] ]
        [ HH.text shortcut ]
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
            -- Create a Ref to hold a queue of pending actions from D3 event handlers
            pendingActionsRef <- liftEffect $ Ref.new ([] :: Array Action)

            -- Create callbacks that append to the queue
            let callbacks =
                  { onShowModuleDetails: \moduleName dependencies dependedOnBy ->
                      Ref.modify_ (_ <> [ShowModuleDetails { moduleName, dependencies, dependedOnBy }]) pendingActionsRef
                  , onHideModuleDetails:
                      Ref.modify_ (_ <> [HideModuleDetails]) pendingActionsRef
                  , onEnableSpotlightMode:
                      Ref.modify_ (_ <> [EnableSpotlightMode]) pendingActionsRef
                  , onDisableSpotlightMode:
                      Ref.modify_ (_ <> [DisableSpotlightMode]) pendingActionsRef
                  , onShowContextMenu: \moduleName x y ->
                      Ref.modify_ (_ <> [ShowContextMenu { moduleName, x, y }]) pendingActionsRef
                  , onSpotlightFunctionsReady: \spotlightFn addDepsFn makeFocusFn resetFn ->
                      Ref.modify_ (_ <> [SpotlightFunctionsReady { spotlight: spotlightFn, addDeps: addDepsFn, makeFocus: makeFocusFn, reset: resetFn }]) pendingActionsRef
                  , onSetCurrentSpotlightModule: \moduleId ->
                      Ref.modify_ (_ <> [SetCurrentSpotlightModule moduleId]) pendingActionsRef
                  }

            -- Create a subscription that polls the Ref for pending actions
            { emitter, listener } <- liftEffect HS.create
            _ <- H.subscribe emitter

            -- Start polling loop in background
            void $ H.fork $ H.liftAff $ pollForActions pendingActionsRef listener

            -- Draw the visualization
            runWithD3_Simulation do
              ExpandableBubblesTab.drawExpandableBubbles graphData declsData callsData "div.svg-container" callbacks

            -- Process all pending actions that were queued during visualization setup
            -- This ensures functions are registered before user interaction
            let processAllPending = do
                  pendingActions <- liftEffect $ Ref.read pendingActionsRef
                  case Array.uncons pendingActions of
                    Just { head: action, tail: rest } -> do
                      liftEffect $ Ref.write rest pendingActionsRef
                      handleAction action
                      processAllPending  -- Process next action
                    Nothing -> pure unit
            processAllPending

          _, _, _ -> pure unit

      _ -> pure unit

    where
      -- Poll the Ref for pending actions and emit them
      pollForActions ref listener = do
        delay (Milliseconds 16.0)  -- Poll at ~60fps
        pendingActions <- liftEffect $ Ref.read ref
        case Array.uncons pendingActions of
          Just { head: action, tail: rest } -> do
            liftEffect $ Ref.write rest ref
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

  ResetToOverview -> do
    -- Close the context menu immediately
    H.modify_ _ { contextMenu = Nothing }
    -- Call the reset spotlight function if available
    state <- H.get
    case state.resetSpotlightFunction of
      Nothing -> do
        -- Fallback: just update state and redraw
        liftEffect $ Console.log "Reset spotlight function not ready yet, using fallback"
        H.modify_ _ { hoveredModule = Nothing, spotlightModeActive = false, currentSpotlightModule = Nothing }
        handleAction (SetActiveTab state.activeTab)
      Just resetFn -> do
        liftEffect $ Console.log "Calling reset spotlight function"
        liftEffect resetFn
        -- State will be updated by the DisableSpotlightMode callback

  EnableSpotlightMode -> do
    H.modify_ _ { spotlightModeActive = true }

  DisableSpotlightMode -> do
    H.modify_ _ { spotlightModeActive = false, currentSpotlightModule = Nothing, hoveredModule = Nothing }

  ShowContextMenu info -> do
    H.modify_ _ { contextMenu = Just info }

  HideContextMenu -> do
    H.modify_ _ { contextMenu = Nothing }

  SpotlightModuleFromMenu moduleName -> do
    -- Close the context menu
    H.modify_ _ { contextMenu = Nothing }
    -- Trigger spotlight by calling the spotlight function
    state <- H.get
    case state.spotlightFunction of
      Nothing -> liftEffect $ Console.log "Spotlight function not ready yet"
      Just spotlightFn -> liftEffect $ spotlightFn moduleName

  AddDepsToSpotlight moduleName -> do
    -- Close the context menu
    H.modify_ _ { contextMenu = Nothing }
    -- Add deps by calling the add deps function
    state <- H.get
    case state.addDepsFunction of
      Nothing -> liftEffect $ Console.log "Add deps function not ready yet"
      Just addDepsFn -> liftEffect $ addDepsFn moduleName

  MakeFocusModule moduleName -> do
    -- Close the context menu
    H.modify_ _ { contextMenu = Nothing }
    -- Make focus by calling the make focus function
    state <- H.get
    case state.makeFocusFunction of
      Nothing -> liftEffect $ Console.log "Make focus function not ready yet"
      Just makeFocusFn -> liftEffect $ makeFocusFn moduleName

  SpotlightFunctionsReady fns -> do
    H.modify_ _
      { spotlightFunction = Just fns.spotlight
      , addDepsFunction = Just fns.addDeps
      , makeFocusFunction = Just fns.makeFocus
      , resetSpotlightFunction = Just fns.reset
      }

  SetCurrentSpotlightModule moduleId -> do
    liftEffect $ Console.log $ "SetCurrentSpotlightModule called with: " <> show moduleId
    H.modify_ _ { currentSpotlightModule = moduleId }
    state <- H.get
    liftEffect $ Console.log $ "State after update - spotlightModeActive: " <> show state.spotlightModeActive <> ", currentSpotlightModule: " <> show state.currentSpotlightModule

  ToggleGridLayout -> do
    -- Toggle the grid layout state
    state <- H.get
    let newGridState = not state.isGridLayout
    H.modify_ _ { isGridLayout = newGridState }
    liftEffect $ Console.log $ "Grid layout toggled to: " <> show newGridState

    -- Trigger the transition on the module graph
    runWithD3_Simulation do
      ModuleGraphTab.transitionToGridLayout newGridState

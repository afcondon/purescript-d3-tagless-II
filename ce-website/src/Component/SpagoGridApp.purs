-- | Halogen wrapper for Code Explorer
-- | Provides minimal container for the visualization with embedded NarrativePanel
module Component.SpagoGridApp where

import Prelude

import Component.CallGraphPopup as CallGraphPopup
import Component.NarrativePanel as NarrativePanel
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber, floor)
import Data.Loader as Loader
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import CodeExplorer.Explorer as Explorer
import CodeExplorer.ViewState (ViewState(..), OverviewView(..), toOverview, viewDescription, isDetail)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PSD3.Scale (interpolateTurbo)
import Type.Proxy (Proxy(..))

-- | Component state
type State =
  { initialized :: Boolean
  , error :: Maybe String
  , viewState :: ViewState
  , pendingView :: Maybe ViewState  -- For waypoint transitions
  , navigationStack :: Array ViewState  -- History for back navigation
  , focusInfo :: Explorer.FocusInfo  -- Focus state for neighborhood views
  , packagePalette :: Array NarrativePanel.ColorEntry
  , projectName :: String
  , projectId :: Int
  , projects :: Array NarrativePanel.ProjectInfo
  , moduleNames :: Array String -- All module names for search
  }

-- | Child slots
type Slots =
  ( narrativePanel :: H.Slot NarrativePanel.Query NarrativePanel.Output Unit
  , callGraphPopup :: H.Slot CallGraphPopup.Query CallGraphPopup.Output Unit
  )

_narrativePanel :: Proxy "narrativePanel"
_narrativePanel = Proxy

_callGraphPopup :: Proxy "callGraphPopup"
_callGraphPopup = Proxy

-- | Actions
data Action
  = Initialize
  | ViewStateChanged ViewState
  | TransitionComplete  -- Scene transition finished (for waypoint chaining)
  | FocusChanged Explorer.FocusInfo  -- Focus state changed (entering neighborhood)
  | NavigationPush ViewState  -- Push view to navigation stack
  | ModelLoaded Explorer.ModelInfo
  | PackagePaletteChanged (Array NarrativePanel.ColorEntry)
  | ProjectsLoaded (Array NarrativePanel.ProjectInfo)
  | SwitchProject Int String
  | NarrativePanelOutput NarrativePanel.Output
  | CallGraphPopupOutput CallGraphPopup.Output
  | ShowCallGraphPopup String String
  | HideCallGraphPopup

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { initialized: false
        , error: Nothing
        , viewState: Overview TreemapView -- Start with Treemap overview
        , pendingView: Nothing  -- No pending waypoint transition
        , navigationStack: []  -- Empty history
        , focusInfo: { focusedNodeId: Nothing, fullNodes: [], originView: Nothing }
        , packagePalette: []
        , projectName: NarrativePanel.defaultProjectName
        , projectId: 1
        , projects: []
        , moduleNames: []
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "explorer-app") ]
    [ -- Narrative Panel component (top-left)
      HH.slot _narrativePanel unit NarrativePanel.component
        { viewState: state.viewState
        , packagePalette: state.packagePalette
        , projectName: state.projectName
        , projectId: state.projectId
        , projects: state.projects
        , moduleNames: state.moduleNames
        }
        NarrativePanelOutput

    -- Call Graph Popup component (modal overlay)
    , HH.slot _callGraphPopup unit CallGraphPopup.component unit CallGraphPopupOutput

    -- Error message
    , case state.error of
        Just err -> HH.div
          [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--bottom-right", HH.ClassName "error-message" ] ]
          [ HH.text err ]
        Nothing -> HH.text ""

    -- Visualization container
    , HH.div [ HP.id "viz" ] []
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    log "[SpagoGridApp] Initializing with callback-based Explorer..."

    -- Create emitter/listener pair for Explorer callbacks
    { emitter, listener } <- liftEffect HS.create

    -- Subscribe to Explorer events
    void $ H.subscribe emitter

    -- Create callbacks that notify the listener
    let
      callbacks :: Explorer.ExplorerCallbacks
      callbacks =
        { onViewStateChanged: \viewState -> HS.notify listener (ViewStateChanged viewState)
        , onModelLoaded: \modelInfo -> HS.notify listener (ModelLoaded modelInfo)
        , onShowCallGraphPopup: \moduleName declarationName -> HS.notify listener (ShowCallGraphPopup moduleName declarationName)
        , onHideCallGraphPopup: HS.notify listener HideCallGraphPopup
        , onTransitionComplete: HS.notify listener TransitionComplete
        , onFocusChanged: \focusInfo -> HS.notify listener (FocusChanged focusInfo)
        , onNavigationPush: \viewState -> HS.notify listener (NavigationPush viewState)
        }

    -- Initialize Explorer with callbacks
    liftEffect $ Explorer.initExplorerWithCallbacks "#viz" callbacks
    H.modify_ _ { initialized = true }
    log "[SpagoGridApp] Explorer initialized with callbacks"

    -- Load projects list asynchronously
    void $ H.fork do
      projectsResult <- H.liftAff Loader.fetchProjects
      case projectsResult of
        Right projects -> do
          let projectInfos = map (\p -> { id: p.id, name: p.name }) projects
          log $ "[SpagoGridApp] Loaded " <> show (Array.length projects) <> " projects"
          handleAction (ProjectsLoaded projectInfos)
        Left err ->
          log $ "[SpagoGridApp] Failed to load projects: " <> err

  ViewStateChanged newViewState -> do
    state <- H.get
    when (state.viewState /= newViewState) do
      log $ "[SpagoGridApp] ViewState changed to: " <> viewDescription newViewState
      H.modify_ _ { viewState = newViewState }
      void $ H.tell _narrativePanel unit (NarrativePanel.SetViewState newViewState)
      -- When entering a Detail view, get and send the origin view to NarrativePanel
      when (isDetail newViewState) do
        originView <- liftEffect $ Explorer.getOriginView
        log $ "[SpagoGridApp] Entering detail view, origin: " <> show originView
        void $ H.tell _narrativePanel unit (NarrativePanel.SetOriginView originView)

  PackagePaletteChanged newPalette -> do
    state <- H.get
    when (state.packagePalette /= newPalette) do
      log $ "[SpagoGridApp] Package palette changed (" <> show (Array.length newPalette) <> " packages)"
      H.modify_ _ { packagePalette = newPalette }
      void $ H.tell _narrativePanel unit (NarrativePanel.SetPackagePalette newPalette)

  ProjectsLoaded projectInfos -> do
    log $ "[SpagoGridApp] Setting projects in state"
    H.modify_ _ { projects = projectInfos }

  SwitchProject newProjectId newProjectName -> do
    state <- H.get
    when (state.projectId /= newProjectId) do
      log $ "[SpagoGridApp] Switching to project: " <> newProjectName
      H.modify_ _ { projectId = newProjectId, projectName = newProjectName, packagePalette = [] }
      -- Reset to Treemap overview
      let resetViewState = Overview TreemapView
      liftEffect $ Explorer.setViewState resetViewState
      H.modify_ _ { viewState = resetViewState }
      liftEffect $ Explorer.reloadWithProject newProjectId

  ModelLoaded modelInfo -> do
    state <- H.get
    when (Array.null state.packagePalette && modelInfo.packageCount > 0) do
      log $ "[SpagoGridApp] Model loaded with " <> show modelInfo.packageCount <> " packages"
      let palette = Array.mapWithIndex mkColorEntry (Array.replicate modelInfo.packageCount unit)
      H.modify_ _ { packagePalette = palette }
      void $ H.tell _narrativePanel unit (NarrativePanel.SetPackagePalette palette)
    -- Fetch module names for search
    moduleNames <- liftEffect $ Explorer.getModuleNames
    log $ "[SpagoGridApp] Loaded " <> show (Array.length moduleNames) <> " module names for search"
    H.modify_ _ { moduleNames = moduleNames }
    where
    mkColorEntry idx _ =
      let
        t = numMod (toNumber idx * 0.618033988749895) 1.0
      in
        { name: "Package " <> show (idx + 1), color: interpolateTurbo t }
    numMod a b = a - b * toNumber (floor (a / b))

  NarrativePanelOutput output ->
    case output of
      -- User clicked one of the four view icons
      -- Implements waypoint logic: some transitions need intermediate steps
      NarrativePanel.ViewSelected newOverview -> do
        state <- H.get
        log $ "[SpagoGridApp] View selected: " <> show newOverview <> " (from: " <> viewDescription state.viewState <> ")"
        let newViewState = toOverview newOverview

        -- Determine current overview (if any)
        let currentOverview = case state.viewState of
              Overview ov -> Just ov
              Detail _ -> Nothing

        -- Check if waypoint transition is needed
        case currentOverview, newOverview of
          -- Tree/Force → Topo: go through Treemap first
          Just from, TopoView | isModuleCentric from -> do
            log "[SpagoGridApp] Waypoint: modules → Treemap → Topo"
            H.modify_ _ { viewState = Overview TreemapView, pendingView = Just newViewState }
            liftEffect $ Explorer.setViewState (Overview TreemapView)

          -- Topo → Tree/Force: go through Treemap first
          Just TopoView, to | isModuleCentric to -> do
            log "[SpagoGridApp] Waypoint: Topo → Treemap → modules"
            H.modify_ _ { viewState = Overview TreemapView, pendingView = Just newViewState }
            liftEffect $ Explorer.setViewState (Overview TreemapView)

          -- Any → Force: go through RadialTree first
          Just from, ForceView | from /= ForceView -> do
            log "[SpagoGridApp] Waypoint: RadialTree → Force"
            H.modify_ _ { pendingView = Just newViewState }
            liftEffect $ Explorer.executeRadialTreeWaypoint

          -- Direct transition (no waypoint needed)
          _, _ -> do
            H.modify_ _ { viewState = newViewState, pendingView = Nothing }
            liftEffect $ Explorer.setViewState newViewState
        where
        -- Is this view module-centric (shows modules)?
        isModuleCentric TreeView = true
        isModuleCentric ForceView = true
        isModuleCentric _ = false

      -- User clicked the back button in detail view
      NarrativePanel.BackRequested -> do
        log "[SpagoGridApp] Back requested from detail view"
        state <- H.get
        case Array.uncons state.navigationStack of
          Nothing -> do
            log "[SpagoGridApp] Navigation stack was empty, falling back to Treemap"
            let fallbackView = Overview TreemapView
            H.modify_ _ { viewState = fallbackView, focusInfo = { focusedNodeId: Nothing, fullNodes: [], originView: Nothing } }
            liftEffect $ Explorer.setViewState fallbackView
          Just { head: previousView, tail: remainingStack } -> do
            log $ "[SpagoGridApp] Navigating back to: " <> viewDescription previousView
            H.modify_ _ { navigationStack = remainingStack, viewState = previousView }
            -- Restore using focus info from Halogen state
            liftEffect $ Explorer.restoreFromFocus state.focusInfo previousView
            -- Clear focus info if returning to overview
            case previousView of
              Overview _ -> H.modify_ _ { focusInfo = { focusedNodeId: Nothing, fullNodes: [], originView: Nothing } }
              Detail _ -> pure unit

      NarrativePanel.ProjectSelected newProjectId -> do
        log $ "[SpagoGridApp] Project selected: " <> show newProjectId
        state <- H.get
        case Array.find (\p -> p.id == newProjectId) state.projects of
          Just project -> handleAction (SwitchProject newProjectId project.name)
          Nothing -> log $ "[SpagoGridApp] Unknown project ID: " <> show newProjectId

      -- User clicked a neighborhood view type toggle (Bubbles/Chord/Matrix)
      NarrativePanel.NeighborhoodViewTypeSelected viewType -> do
        log $ "[SpagoGridApp] Neighborhood view type selected: " <> show viewType
        liftEffect $ Explorer.setNeighborhoodViewType viewType

      -- User selected a module from search
      NarrativePanel.ModuleSearchSelected moduleName -> do
        log $ "[SpagoGridApp] Module search selected: " <> moduleName
        success <- liftEffect $ Explorer.navigateToModuleByName moduleName
        when (not success) do
          log $ "[SpagoGridApp] Module not found: " <> moduleName

  CallGraphPopupOutput output ->
    case output of
      CallGraphPopup.PopupClosed ->
        log "[SpagoGridApp] Call graph popup closed"
      CallGraphPopup.NavigateToFunction moduleName declarationName -> do
        log $ "[SpagoGridApp] Navigate to function: " <> moduleName <> "." <> declarationName

  ShowCallGraphPopup moduleName declarationName -> do
    log $ "[SpagoGridApp] Showing call graph popup for: " <> moduleName <> "." <> declarationName
    void $ H.tell _callGraphPopup unit (CallGraphPopup.ShowPopup moduleName declarationName)

  HideCallGraphPopup -> do
    log "[SpagoGridApp] Hiding call graph popup"
    void $ H.tell _callGraphPopup unit CallGraphPopup.HidePopup

  TransitionComplete -> do
    -- Scene transition completed, check if there's a pending waypoint target
    state <- H.get
    case state.pendingView of
      Just targetView -> do
        log $ "[SpagoGridApp] Waypoint complete, continuing to: " <> viewDescription targetView
        H.modify_ _ { pendingView = Nothing, viewState = targetView }
        -- Now execute the target view (all waypoints already done)
        liftEffect $ Explorer.setViewState targetView
      Nothing ->
        -- No pending view, transition complete normally
        pure unit

  FocusChanged newFocusInfo -> do
    log $ "[SpagoGridApp] Focus changed, focusedNodeId: " <> show newFocusInfo.focusedNodeId
    H.modify_ _ { focusInfo = newFocusInfo }

  NavigationPush viewToPush -> do
    log $ "[SpagoGridApp] Navigation push: " <> viewDescription viewToPush
    H.modify_ \s -> s { navigationStack = Array.cons viewToPush s.navigationStack }

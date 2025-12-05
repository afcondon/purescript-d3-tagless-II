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
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Engine.Explorer as Explorer
import Engine.Explorer (SceneId(..))
import Engine.ViewState (ViewState(..), ScopeFilter(..))
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
  , packagePalette :: Array NarrativePanel.ColorEntry
  , projectName :: String
  , projectId :: Int
  , projects :: Array NarrativePanel.ProjectInfo
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
  | ModelLoaded Explorer.ModelInfo  -- New: callback from Explorer when model loads
  | PackagePaletteChanged (Array NarrativePanel.ColorEntry)
  | ProjectsLoaded (Array NarrativePanel.ProjectInfo)
  | SwitchProject Int String  -- Project ID and name
  | NarrativePanelOutput NarrativePanel.Output
  | CallGraphPopupOutput CallGraphPopup.Output
  | ShowCallGraphPopup String String  -- moduleName, declarationName
  | HideCallGraphPopup

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { initialized: false
        , error: Nothing
        , viewState: Treemap ProjectAndLibraries
        , packagePalette: []
        , projectName: NarrativePanel.defaultProjectName
        , projectId: 1  -- Default to first project (psd3)
        , projects: []
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
      -- Scene-aware color key that switches based on ViewState
      HH.slot _narrativePanel unit NarrativePanel.component
        { viewState: state.viewState
        , packagePalette: state.packagePalette
        , projectName: state.projectName
        , projectId: state.projectId
        , projects: state.projects
        }
        NarrativePanelOutput

    -- Call Graph Popup component (modal overlay)
    , HH.slot _callGraphPopup unit CallGraphPopup.component unit CallGraphPopupOutput

    -- Error message (bottom-right to avoid collision with narrative panel)
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

    -- Subscribe to Explorer events - maps Action directly through the emitter
    void $ H.subscribe emitter

    -- Create callbacks that notify the listener
    let callbacks :: Explorer.ExplorerCallbacks
        callbacks =
          { onViewStateChanged: \viewState -> HS.notify listener (ViewStateChanged viewState)
          , onModelLoaded: \modelInfo -> HS.notify listener (ModelLoaded modelInfo)
          , onShowCallGraphPopup: \moduleName declarationName -> HS.notify listener (ShowCallGraphPopup moduleName declarationName)
          , onHideCallGraphPopup: HS.notify listener HideCallGraphPopup
          }

    -- Initialize Explorer with callbacks (replaces polling!)
    liftEffect $ Explorer.initExplorerWithCallbacks "#viz" callbacks
    H.modify_ _ { initialized = true }
    log "[SpagoGridApp] Explorer initialized with callbacks (no polling!)"

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
      log $ "[SpagoGridApp] ViewState changed to: " <> showViewState newViewState
      H.modify_ _ { viewState = newViewState }
      -- Query the NarrativePanel to update
      log $ "[SpagoGridApp] Sending SetViewState to NarrativePanel"
      void $ H.tell _narrativePanel unit (NarrativePanel.SetViewState newViewState)

  PackagePaletteChanged newPalette -> do
    state <- H.get
    when (state.packagePalette /= newPalette) do
      log $ "[SpagoGridApp] Package palette changed (" <> show (Array.length newPalette) <> " packages)"
      H.modify_ _ { packagePalette = newPalette }
      -- Query the NarrativePanel to update
      void $ H.tell _narrativePanel unit (NarrativePanel.SetPackagePalette newPalette)

  ProjectsLoaded projectInfos -> do
    log $ "[SpagoGridApp] Setting projects in state"
    H.modify_ _ { projects = projectInfos }

  SwitchProject newProjectId newProjectName -> do
    state <- H.get
    when (state.projectId /= newProjectId) do
      log $ "[SpagoGridApp] Switching to project: " <> newProjectName <> " (id: " <> show newProjectId <> ")"
      -- Update state
      H.modify_ _ { projectId = newProjectId, projectName = newProjectName, packagePalette = [] }
      -- Reset ViewState to top level Treemap
      let resetViewState = Treemap ProjectAndLibraries
      liftEffect $ Ref.write resetViewState Explorer.globalViewStateRef
      H.modify_ _ { viewState = resetViewState }
      -- Reload Explorer with new project data
      liftEffect $ Explorer.reloadWithProject newProjectId

  ModelLoaded modelInfo -> do
    -- Generate package palette from model info
    state <- H.get
    when (Array.null state.packagePalette && modelInfo.packageCount > 0) do
      log $ "[SpagoGridApp] Model loaded with " <> show modelInfo.packageCount <> " packages"
      let palette = Array.mapWithIndex mkColorEntry (Array.replicate modelInfo.packageCount unit)
      H.modify_ _ { packagePalette = palette }
      void $ H.tell _narrativePanel unit (NarrativePanel.SetPackagePalette palette)
    where
    mkColorEntry idx _ =
      let t = numMod (toNumber idx * 0.618033988749895) 1.0
      in { name: "Package " <> show (idx + 1), color: interpolateTurbo t }
    numMod a b = a - b * toNumber (floor (a / b))

  NarrativePanelOutput output ->
    case output of
      NarrativePanel.ControlChanged controlId newValue -> do
        log $ "[SpagoGridApp] Control changed: " <> controlId <> " -> " <> newValue
        -- Get current view state from Halogen state and pass to handler
        state <- H.get
        let newView = applyControlChange controlId newValue state.viewState
        -- Update Halogen state with new view
        H.modify_ _ { viewState = newView }
        -- Forward to Explorer's scene manager
        liftEffect $ handleControlChangeFromPanel newView

      NarrativePanel.BackClicked -> do
        log "[SpagoGridApp] Back button clicked"
        liftEffect handleBackFromPanel

      NarrativePanel.ProjectSelected newProjectId -> do
        log $ "[SpagoGridApp] Project selected: " <> show newProjectId
        -- Look up project name from projects list
        state <- H.get
        case Array.find (\p -> p.id == newProjectId) state.projects of
          Just project -> handleAction (SwitchProject newProjectId project.name)
          Nothing -> log $ "[SpagoGridApp] Unknown project ID: " <> show newProjectId

  CallGraphPopupOutput output ->
    case output of
      CallGraphPopup.PopupClosed ->
        log "[SpagoGridApp] Call graph popup closed"
      CallGraphPopup.NavigateToFunction moduleName declarationName -> do
        log $ "[SpagoGridApp] Navigate to function: " <> moduleName <> "." <> declarationName
        -- The popup handles navigation internally by reloading itself

  ShowCallGraphPopup moduleName declarationName -> do
    log $ "[SpagoGridApp] Showing call graph popup for: " <> moduleName <> "." <> declarationName
    void $ H.tell _callGraphPopup unit (CallGraphPopup.ShowPopup moduleName declarationName)

  HideCallGraphPopup -> do
    log "[SpagoGridApp] Hiding call graph popup"
    void $ H.tell _callGraphPopup unit CallGraphPopup.HidePopup

-- | Forward control change to Explorer's scene manager
-- | Takes the new ViewState (already computed by caller from Halogen state)
handleControlChangeFromPanel :: ViewState -> Effect Unit
handleControlChangeFromPanel newView = do
  -- Update the global ref (still needed for Explorer internals)
  Ref.write newView Explorer.globalViewStateRef

  -- Update node colors to match new view (must happen before scene transition)
  Explorer.updateNodeColors newView

  -- Trigger scene transition via global state ref
  mStateRef <- Ref.read Explorer.globalStateRef
  case mStateRef of
    Just stateRef -> do
      case newView of
        Treemap _ ->
          pure unit  -- Treemap is static, no scene transition
        TreeLayout _ _ ->
          Explorer.goToScene TreeForm stateRef
        ForceLayout _ _ ->
          Explorer.goToScene TreeRun stateRef
        _ -> pure unit
    Nothing -> pure unit

-- | Apply control change to ViewState
applyControlChange :: String -> String -> ViewState -> ViewState
applyControlChange "layout" newLayout currentView =
  let scope = getScopeFromView currentView
  in case newLayout of
    "treemap" -> Treemap scope
    "tree" -> TreeLayout scope "PSD3.Main"  -- Default root module
    "force" -> ForceLayout scope "PSD3.Main"  -- Default root module
    _ -> currentView

applyControlChange "scope" newScope currentView =
  let scope = if newScope == "project" then ProjectOnly else ProjectAndLibraries
  in case currentView of
    Treemap _ -> Treemap scope
    TreeLayout _ root -> TreeLayout scope root
    ForceLayout _ root -> ForceLayout scope root
    other -> other

applyControlChange _ _ view = view

-- | Extract scope from any ViewState
getScopeFromView :: ViewState -> ScopeFilter
getScopeFromView (Treemap scope) = scope
getScopeFromView (TreeLayout scope _) = scope
getScopeFromView (ForceLayout scope _) = scope
getScopeFromView _ = ProjectAndLibraries

-- | Forward back button to Explorer (uses navigation stack)
handleBackFromPanel :: Effect Unit
handleBackFromPanel = do
  _ <- Explorer.navigateBack
  pure unit

-- | Helper to show ViewState for logging
showViewState :: ViewState -> String
showViewState (Treemap _) = "Treemap"
showViewState (TreeLayout _ _) = "TreeLayout"
showViewState (ForceLayout _ _) = "ForceLayout"
showViewState (Neighborhood name) = "Neighborhood(" <> name <> ")"
showViewState (FunctionCalls name) = "FunctionCalls(" <> name <> ")"

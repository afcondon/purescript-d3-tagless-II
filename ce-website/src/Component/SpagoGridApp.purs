-- | Halogen wrapper for Code Explorer
-- | Provides minimal container for the visualization with embedded NarrativePanel
module Component.SpagoGridApp where

import Prelude

import Component.NarrativePanel as NarrativePanel
import Data.Array as Array
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Engine.Explorer as Explorer
import Engine.ViewState (ViewState(..), ScopeFilter(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Scale (interpolateTurbo)
import Type.Proxy (Proxy(..))

-- | Component state
type State =
  { initialized :: Boolean
  , error :: Maybe String
  , viewState :: ViewState
  , packagePalette :: Array NarrativePanel.ColorEntry
  }

-- | Child slots
type Slots = ( narrativePanel :: H.Slot NarrativePanel.Query NarrativePanel.Output Unit )

_narrativePanel :: Proxy "narrativePanel"
_narrativePanel = Proxy

-- | Actions
data Action
  = Initialize
  | ViewStateChanged ViewState
  | PackagePaletteChanged (Array NarrativePanel.ColorEntry)
  | NarrativePanelOutput NarrativePanel.Output

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { initialized: false
        , error: Nothing
        , viewState: PackageGrid ProjectAndLibraries
        , packagePalette: []
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
        }
        NarrativePanelOutput

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
    log "[SpagoGridApp] Initializing..."

    -- Initialize the explorer (loads data via legacy endpoints)
    liftEffect $ Explorer.initExplorer "#viz"
    H.modify_ _ { initialized = true }

    -- Start polling loop for ViewState and palette changes
    void $ H.fork pollLoop

  ViewStateChanged newViewState -> do
    state <- H.get
    when (state.viewState /= newViewState) do
      log $ "[SpagoGridApp] ViewState changed"
      H.modify_ _ { viewState = newViewState }
      -- Query the NarrativePanel to update
      void $ H.tell _narrativePanel unit (NarrativePanel.SetViewState newViewState)

  PackagePaletteChanged newPalette -> do
    state <- H.get
    when (state.packagePalette /= newPalette) do
      log $ "[SpagoGridApp] Package palette changed (" <> show (Array.length newPalette) <> " packages)"
      H.modify_ _ { packagePalette = newPalette }
      -- Query the NarrativePanel to update
      void $ H.tell _narrativePanel unit (NarrativePanel.SetPackagePalette newPalette)

  NarrativePanelOutput output ->
    case output of
      NarrativePanel.ControlChanged controlId newValue -> do
        log $ "[SpagoGridApp] Control changed: " <> controlId <> " -> " <> newValue
        -- Forward to Explorer's handleControlChange
        liftEffect $ handleControlChangeFromPanel controlId newValue

      NarrativePanel.BackClicked -> do
        log "[SpagoGridApp] Back button clicked"
        liftEffect handleBackFromPanel

-- | Poll loop that checks for ViewState and palette changes
pollLoop :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
pollLoop = do
  -- Read current state from Explorer's global refs
  viewState <- liftEffect $ Ref.read Explorer.globalViewStateRef
  modelInfo <- liftEffect $ Ref.read Explorer.globalModelInfoRef

  -- Check if ViewState changed
  state <- H.get
  when (state.viewState /= viewState) do
    handleAction (ViewStateChanged viewState)

  -- Check if palette needs updating (only once when data loads)
  when (Array.null state.packagePalette && modelInfo.packageCount > 0) do
    let palette = Array.mapWithIndex mkColorEntry (Array.replicate modelInfo.packageCount unit)
    handleAction (PackagePaletteChanged palette)

  -- Continue polling
  H.liftAff $ delay (Milliseconds 100.0)
  pollLoop
  where
  mkColorEntry idx _ =
    let t = numMod (toNumber idx * 0.618033988749895) 1.0
    in { name: "Package " <> show (idx + 1), color: interpolateTurbo t }

  numMod a b = a - b * toNumber (floor (a / b))

-- | Forward control change to Explorer
-- | This is called when the user clicks a TangleJS-style control
handleControlChangeFromPanel :: String -> String -> Effect Unit
handleControlChangeFromPanel controlId newValue = do
  -- Import and call Explorer's handleControlChange
  -- Since we don't have direct access, we'll update the ViewState ref directly
  -- and let Explorer's control callback handle the scene transition
  currentView <- Ref.read Explorer.globalViewStateRef
  let newView = applyControlChange controlId newValue currentView
  Ref.write newView Explorer.globalViewStateRef
  -- Trigger scene transition via global state ref
  mStateRef <- Ref.read Explorer.globalStateRef
  case mStateRef of
    Just stateRef -> do
      case controlId, newView of
        "layout", PackageGrid _ ->
          Explorer.goToScene "GridRun" stateRef
        "layout", ModuleOrbit _ ->
          Explorer.goToScene "OrbitRun" stateRef
        "layout", DependencyTree _ ->
          Explorer.goToScene "TreeRun" stateRef
        _, _ -> pure unit
    Nothing -> pure unit

-- | Apply control change to ViewState
applyControlChange :: String -> String -> ViewState -> ViewState
applyControlChange "layout" newLayout currentView =
  case newLayout, currentView of
    "grid", PackageGrid scope -> PackageGrid scope
    "grid", ModuleOrbit scope -> PackageGrid scope
    "grid", DependencyTree scope -> PackageGrid scope
    "orbit", PackageGrid scope -> ModuleOrbit scope
    "orbit", ModuleOrbit scope -> ModuleOrbit scope
    "orbit", DependencyTree scope -> ModuleOrbit scope
    "tree", PackageGrid scope -> DependencyTree scope
    "tree", ModuleOrbit scope -> DependencyTree scope
    "tree", DependencyTree scope -> DependencyTree scope
    _, other -> other

applyControlChange "scope" newScope currentView =
  let scope = if newScope == "project" then ProjectOnly else ProjectAndLibraries
  in case currentView of
    PackageGrid _ -> PackageGrid scope
    ModuleOrbit _ -> ModuleOrbit scope
    DependencyTree _ -> DependencyTree scope
    other -> other

applyControlChange _ _ view = view

-- | Forward back button to Explorer
handleBackFromPanel :: Effect Unit
handleBackFromPanel = do
  mStateRef <- Ref.read Explorer.globalStateRef
  case mStateRef of
    Just _stateRef -> do
      -- Reset to full view
      Ref.write (PackageGrid ProjectAndLibraries) Explorer.globalViewStateRef
    Nothing -> pure unit

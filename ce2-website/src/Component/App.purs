-- | Code Explorer v2 - Main Application Component
-- |
-- | Architecture:
-- | - Halogen owns ALL application state (view, navigation, focus, etc.)
-- | - Explorer is stateless - just renders what Halogen tells it to
-- | - D3 mutable data is managed by library code (opaque types)
-- | - All callbacks flow through Halogen (except drag/zoom which are D3 internal)
module Component.App where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Loader as Loader
import ViewState (ViewState(..), OverviewView(..), viewDescription)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Types (SimNode)
import Viz.Treemap as Treemap
import Viz.TreeView as TreeView
import Viz.ForceGraph as ForceGraph
import Data.TreeLayout (findRootModule)
import DataViz.Layout.Hierarchy.TreeStyle as TreeStyle
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Types
-- =============================================================================

-- | Highlight state - single source of truth for what's highlighted and why
data HighlightState
  = NoHighlight
  | HoverHighlight Int    -- Hovering over a node (by ID)

derive instance eqHighlightState :: Eq HighlightState

-- | Application state - ALL state lives here, Explorer is stateless
type State =
  { -- Lifecycle
    phase :: AppPhase

    -- View state (current view, navigation history)
  , viewState :: ViewState
  , navigationStack :: Array ViewState
  , pendingView :: Maybe ViewState  -- For waypoint transitions

    -- Focus state (for neighborhood drill-down)
  , focusedNodeId :: Maybe Int
  , originView :: Maybe OverviewView

    -- Highlight state - unified across all views
  , highlightState :: HighlightState

    -- Model data (loaded from API)
  , modelData :: Maybe ModelData

    -- Animation state
  , treeAnimation :: Maybe TreeView.AnimationState
  , treeRootId :: Maybe Int              -- Current tree root (Nothing = use main module)
  , treeStyle :: TreeStyle.TreeStyle     -- Bundles orientation + matching link path

    -- Force view state
  , forceAnimation :: Maybe ForceGraph.AnimationState
  , forceHandle :: Maybe ForceGraph.SimulationHandle

    -- Callback ref for D3 event -> Halogen action bridge
  , clickEmitter :: Maybe (HS.Listener Action)

    -- UI state
  , packageCount :: Int  -- For color palette
  , projectId :: Int
  , projectName :: String
  }

-- | Application lifecycle phases
data AppPhase
  = Loading
  | Ready
  | Error String

derive instance eqAppPhase :: Eq AppPhase

-- | Loaded model data (immutable after load)
-- | We store the full LoadedModel from the Loader
type ModelData = Loader.LoadedModel

-- | Actions
data Action
  = Initialize
  | DataLoaded ModelData
  | DataFailed String
  | SwitchToTreemap
  | SwitchToTree
  | SwitchToForce
  | FocusSubtree Int  -- Click on a module to show its subtree (horizontal)
  | HandleHover (Maybe SimNode)  -- Hover events routed through Halogen
  | AnimationTick
  | ForceAnimationTick  -- Separate tick for force animation

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { phase: Loading
  , viewState: Overview TreemapView
  , navigationStack: []
  , pendingView: Nothing
  , focusedNodeId: Nothing
  , originView: Nothing
  , highlightState: NoHighlight
  , modelData: Nothing
  , treeAnimation: Nothing
  , treeRootId: Nothing
  , treeStyle: TreeStyle.verticalTree
  , forceAnimation: Nothing
  , forceHandle: Nothing
  , clickEmitter: Nothing
  , packageCount: 0
  , projectId: 1
  , projectName: "Loading..."
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "explorer-app") ]
    [ -- Status display with view switcher
      HH.div
        [ HP.class_ (HH.ClassName "status-panel") ]
        [ HH.h2_ [ HH.text "Code Explorer v2" ]
        , HH.p_ [ HH.text $ "Phase: " <> showPhase state.phase ]
        , HH.p_ [ HH.text $ "View: " <> viewDescription state.viewState ]
        , case state.modelData of
            Nothing -> HH.p_ [ HH.text "No data loaded" ]
            Just model -> HH.p_
              [ HH.text $ "Loaded: " <> show model.moduleCount <> " modules, "
                  <> show model.packageCount <> " packages"
              ]
        -- View switcher buttons
        , HH.div
            [ HP.class_ (HH.ClassName "view-switcher") ]
            [ HH.button
                [ HE.onClick \_ -> SwitchToTreemap
                , HP.class_ (HH.ClassName $ if isTreemapView state.viewState then "active" else "")
                ]
                [ HH.text "Treemap" ]
            , HH.button
                [ HE.onClick \_ -> SwitchToTree
                , HP.class_ (HH.ClassName $ if isTreeView state.viewState then "active" else "")
                ]
                [ HH.text "Tree" ]
            , HH.button
                [ HE.onClick \_ -> SwitchToForce
                , HP.class_ (HH.ClassName $ if isForceView state.viewState then "active" else "")
                ]
                [ HH.text "Force" ]
            ]
        ]

    -- Visualization container (D3 will render into this)
    , HH.div [ HP.id "viz" ] []
    ]

showPhase :: AppPhase -> String
showPhase Loading = "Loading..."
showPhase Ready = "Ready"
showPhase (Error msg) = "Error: " <> msg

isTreemapView :: ViewState -> Boolean
isTreemapView (Overview TreemapView) = true
isTreemapView _ = false

isTreeView :: ViewState -> Boolean
isTreeView (Overview TreeView) = true
isTreeView _ = false

isForceView :: ViewState -> Boolean
isForceView (Overview ForceView) = true
isForceView _ = false

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[App] Initializing..."

    -- Set up subscription for D3 click events -> Halogen actions
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    H.modify_ _ { clickEmitter = Just listener }

    -- Load model data
    void $ H.fork do
      result <- H.liftAff $ Loader.loadModel
      case result of
        Left err -> handleAction (DataFailed err)
        Right loaded -> handleAction (DataLoaded loaded)

  DataLoaded model -> do
    log $ "[App] Data loaded: " <> show model.moduleCount <> " modules"
    state <- H.get
    H.modify_ _
      { phase = Ready
      , modelData = Just model
      , packageCount = model.packageCount
      }

    -- Render the treemap visualization
    let config :: Treemap.Config
        config =
          { containerSelector: "#viz"
          , viewState: state.viewState
          , packageCount: model.packageCount
          , packages: model.packages
          }

    -- Build callbacks with emitter for click -> action bridge
    let callbacks = makeTreemapCallbacks state.clickEmitter

    liftEffect $ Treemap.render config callbacks model.nodes
    log "[App] Treemap rendered"

  DataFailed err -> do
    log $ "[App] Data load failed: " <> err
    H.modify_ _ { phase = Error err }

  SwitchToTreemap -> do
    log "[App] Switching to Treemap view"
    state <- H.get
    -- Clean up any existing force simulation
    case state.forceHandle of
      Just handle -> liftEffect handle.cleanup
      Nothing -> pure unit

    case state.modelData of
      Nothing -> pure unit
      Just model -> do
        H.modify_ _ { viewState = Overview TreemapView, treeAnimation = Nothing, treeRootId = Nothing, forceHandle = Nothing }

        let config :: Treemap.Config
            config =
              { containerSelector: "#viz"
              , viewState: Overview TreemapView
              , packageCount: model.packageCount
              , packages: model.packages
              }

        let callbacks = makeTreemapCallbacks state.clickEmitter

        liftEffect $ Treemap.render config callbacks model.nodes
        log "[App] Treemap rendered"

  SwitchToTree -> do
    log "[App] SwitchToTree clicked"
    state <- H.get
    -- Clean up any existing force simulation
    case state.forceHandle of
      Just handle -> liftEffect handle.cleanup
      Nothing -> pure unit

    case state.modelData of
      Nothing -> log "[App] No model data, skipping"
      Just model -> do
        log "[App] Initializing animation state"
        -- Use main module as root, vertical style (default tree view)
        let rootId = findRootModule model.nodes
        H.modify_ _
          { viewState = Overview TreeView
          , treeAnimation = Just TreeView.initAnimation
          , treeRootId = Just rootId
          , treeStyle = TreeStyle.verticalTree  -- Bundled with matching bezier
          , forceHandle = Nothing
          }

        log "[App] Starting animation loop"
        startAnimationLoop

  SwitchToForce -> do
    log "[App] SwitchToForce clicked"
    state <- H.get
    -- Clean up any existing force simulation
    case state.forceHandle of
      Just handle -> liftEffect handle.cleanup
      Nothing -> pure unit

    case state.modelData of
      Nothing -> log "[App] No model data, skipping"
      Just _model -> do
        log "[App] Starting force animation (radial tree growth)"
        H.modify_ _
          { viewState = Overview ForceView
          , treeAnimation = Nothing
          , treeRootId = Nothing
          , forceAnimation = Just ForceGraph.initAnimation
          , forceHandle = Nothing
          }

        log "[App] Starting force animation loop"
        startForceAnimationLoop

  FocusSubtree nodeId -> do
    log $ "[App] FocusSubtree on node " <> show nodeId
    state <- H.get
    case state.modelData of
      Nothing -> pure unit
      Just _model -> do
        -- Reset to treemap positions first (fixes additive tree bug)
        liftEffect $ TreeView.resetToTreemap "#viz"

        -- Switch to horizontal tree with clicked node as root
        H.modify_ _
          { viewState = Overview TreeView
          , treeAnimation = Just TreeView.initAnimation
          , treeRootId = Just nodeId
          , treeStyle = TreeStyle.horizontalTree  -- Bundled with matching bezier
          }

        log "[App] Starting horizontal subtree animation"
        startAnimationLoop

  AnimationTick -> do
    state <- H.get
    case state.treeAnimation, state.modelData, state.treeRootId of
      Just anim, Just model, Just rootId | TreeView.isAnimating anim -> do
        -- Render FIRST with current animation state (before ticking)
        let config :: TreeView.Config
            config =
              { containerSelector: "#viz"
              , style: state.treeStyle  -- Bundled orientation + link path
              , rootId: rootId
              }

        animWithLayout <- liftEffect $ TreeView.renderAnimated config anim model.nodes model.links

        -- THEN advance animation for next frame
        let newAnim = TreeView.tickAnimation animationDelta animWithLayout
        H.modify_ _ { treeAnimation = Just newAnim }

        -- Continue loop if still animating
        when (TreeView.isAnimating newAnim) do
          startAnimationLoop

      _, _, _ -> pure unit

  ForceAnimationTick -> do
    state <- H.get
    case state.forceAnimation, state.modelData of
      Just anim, Just model | ForceGraph.isAnimating anim -> do
        -- Find root for config
        let rootId = findRootModule model.nodes
        let config :: ForceGraph.Config
            config =
              { containerSelector: "#viz"
              , packageCount: model.packageCount
              , rootId: rootId
              }

        -- Render current frame
        result <- liftEffect $ ForceGraph.renderAnimated config anim model.nodes model.links

        -- Advance animation for next frame
        let newAnim = ForceGraph.tickAnimation animationDelta result.animState
        H.modify_ _ { forceAnimation = Just newAnim }

        -- Check if we got a simulation handle (phase 3 started)
        case result.simHandle of
          Just handle -> do
            log "[App] Force simulation started, storing handle"
            H.modify_ _ { forceHandle = Just handle }
          Nothing -> pure unit

        -- Continue loop if still animating
        when (ForceGraph.isAnimating newAnim) do
          startForceAnimationLoop

      _, _ -> pure unit

  HandleHover mNode -> do
    state <- H.get
    -- Decide whether to apply highlighting based on current view
    case state.viewState of
      Overview TreeView ->
        -- In tree view: ignore hover highlighting (tree classification takes priority)
        pure unit
      Overview ForceView ->
        -- In force view: ignore hover highlighting for now
        pure unit
      Overview TreemapView ->
        -- In treemap view: apply hover highlighting
        case mNode of
          Just n -> do
            H.modify_ _ { highlightState = HoverHighlight n.id }
            liftEffect $ Treemap.highlightDependencies "#viz" n
          Nothing -> do
            H.modify_ _ { highlightState = NoHighlight }
            liftEffect $ Treemap.clearHighlight "#viz"
      _ ->
        -- Other views: default behavior (apply highlighting)
        case mNode of
          Just n -> do
            H.modify_ _ { highlightState = HoverHighlight n.id }
            liftEffect $ Treemap.highlightDependencies "#viz" n
          Nothing -> do
            H.modify_ _ { highlightState = NoHighlight }
            liftEffect $ Treemap.clearHighlight "#viz"

-- | Animation speed (progress per tick)
-- | At ~60fps with 16ms delay: 0.02 = 50 ticks = ~0.8 seconds
animationDelta :: Tick.TickDelta
animationDelta = 0.02

-- | Start the animation loop by forking an action after a short delay
startAnimationLoop :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
startAnimationLoop = do
  -- Use a simple fork with delay to simulate requestAnimationFrame
  void $ H.fork do
    H.liftAff $ Aff.delay (Milliseconds 16.0)
    handleAction AnimationTick

-- | Start the force animation loop
startForceAnimationLoop :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
startForceAnimationLoop = do
  void $ H.fork do
    H.liftAff $ Aff.delay (Milliseconds 16.0)
    handleAction ForceAnimationTick

-- | Build treemap callbacks with click/hover -> action bridge
-- | Uses the subscription listener to emit actions to Halogen
makeTreemapCallbacks :: Maybe (HS.Listener Action) -> Treemap.Callbacks
makeTreemapCallbacks mListener =
  { onNodeClick: \node -> case mListener of
      Just listener -> HS.notify listener (FocusSubtree node.id)
      Nothing -> pure unit
  , onNodeHover: \mNode -> case mListener of
      Just listener -> HS.notify listener (HandleHover mNode)
      Nothing -> pure unit
  }

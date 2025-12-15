-- | Code Explorer v2 - Main Application Component
-- |
-- | Halogen-First Architecture:
-- | - ALL state lives here in Halogen
-- | - Visualization modules (Treemap, TreeView, ForceGraph) are STATELESS
-- | - D3 mutable data is managed by library code (opaque types)
-- | - Simulation ticks flow through Halogen subscriptions
-- |
-- | Force View Animation Phases:
-- | 1. TreeGrowth: Nodes animate from center to radial tree positions
-- | 2. LinkMorph: Bezier curves morph into straight lines
-- | 3. ForceActive: Force simulation takes over, positions updated via ticks
module Component.App where

import Prelude

import Control.Monad (void, when)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Loader as Loader
import Data.TreeLayout as TreeLayout
import Data.TreeLayout (findRootModule)
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
import DataViz.Layout.Hierarchy.TreeStyle as TreeStyle
import PSD3.Transition.Tick as Tick
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Events (SimulationEvent(..), defaultCallbacks)
import PSD3.ForceEngine.Halogen (subscribeToSimulation)

-- =============================================================================
-- Types
-- =============================================================================

-- | The three phases of force view animation
data ForcePhase
  = TreeGrowth       -- Phase 1: Grow radial tree from center
  | LinkMorph        -- Phase 2: Morph bezier links to straight lines
  | ForceActive      -- Phase 3: Force simulation running

derive instance eqForcePhase :: Eq ForcePhase

instance showForcePhase :: Show ForcePhase where
  show TreeGrowth = "TreeGrowth"
  show LinkMorph = "LinkMorph"
  show ForceActive = "ForceActive"

-- | Highlight state - single source of truth for what's highlighted and why
data HighlightState
  = NoHighlight
  | HoverHighlight Int    -- Hovering over a node (by ID)

derive instance eqHighlightState :: Eq HighlightState

-- | Application state - ALL state lives here
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

    -- Tree animation state
  , treeAnimation :: Maybe TreeView.AnimationState
  , treeRootId :: Maybe Int              -- Current tree root (Nothing = use main module)
  , treeStyle :: TreeStyle.TreeStyle     -- Bundles orientation + matching link path

    -- Force view state (Halogen-owned)
  , forcePhase :: Maybe ForcePhase          -- Current phase (Nothing = not in force view)
  , forceProgress :: Tick.Progress          -- 0.0 to 1.0 within current phase
  , forceLayout :: Maybe TreeLayout.TreeLayoutResult  -- Computed once, reused
  , simulation :: Maybe ForceGraph.ForceSimulation    -- Opaque handle to D3 simulation
  , forceShowProjectOnly :: Boolean         -- Toggle: show only "my-project" modules

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
type ModelData = Loader.LoadedModel

-- | Actions
data Action
  = Initialize
  | DataLoaded ModelData
  | DataFailed String
  | SwitchToTreemap
  | SwitchToTree
  | SwitchToForce
  | FocusSubtree Int          -- Click on a module to show its subtree (horizontal)
  | HandleHover (Maybe SimNode)  -- Hover events routed through Halogen
  | AnimationTick             -- Tree animation tick
  | ForceAnimationTick        -- Force phases 1-2 animation tick (Halogen-driven)
  | ForceSimTick              -- Force phase 3 tick (D3 simulation-driven)
  | ToggleForceProjectOnly    -- Toggle between all modules and project-only

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
  , forcePhase: Nothing
  , forceProgress: 0.0
  , forceLayout: Nothing
  , simulation: Nothing
  , forceShowProjectOnly: false
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
        , case state.forcePhase of
            Just fp -> HH.p_ [ HH.text $ "Force phase: " <> show fp ]
            Nothing -> HH.text ""
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
        -- Force view filter toggle (only shown when in force view, same row as view switcher)
        , if isForceView state.viewState
            then HH.div
              [ HP.class_ (HH.ClassName "view-switcher") ]
              [ HH.button
                  [ HE.onClick \_ -> ToggleForceProjectOnly
                  , HP.class_ (HH.ClassName $ if not state.forceShowProjectOnly then "active" else "")
                  ]
                  [ HH.text "All" ]
              , HH.button
                  [ HE.onClick \_ -> ToggleForceProjectOnly
                  , HP.class_ (HH.ClassName $ if state.forceShowProjectOnly then "active" else "")
                  ]
                  [ HH.text "Project" ]
              ]
            else HH.text ""
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
    cleanupForceSimulation

    case state.modelData of
      Nothing -> pure unit
      Just model -> do
        H.modify_ _
          { viewState = Overview TreemapView
          , treeAnimation = Nothing
          , treeRootId = Nothing
          , forcePhase = Nothing
          , forceLayout = Nothing
          , simulation = Nothing
          }

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
    cleanupForceSimulation

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
          , treeStyle = TreeStyle.verticalTree
          , forcePhase = Nothing
          , forceLayout = Nothing
          , simulation = Nothing
          }

        log "[App] Starting animation loop"
        startAnimationLoop

  SwitchToForce -> do
    log "[App] SwitchToForce clicked"
    state <- H.get
    -- Clean up any existing force simulation
    cleanupForceSimulation

    case state.modelData of
      Nothing -> log "[App] No model data, skipping"
      Just model -> do
        log "[App] Starting force animation (radial tree growth)"

        -- Compute layout once (will be reused through all phases)
        let rootId = findRootModule model.nodes
        let layout = ForceGraph.computeLayout rootId model.nodes model.links
        log $ "[App] Layout computed: " <> show (length layout.treeNodes) <> " nodes in tree"

        -- Add tree links to DOM
        liftEffect $ ForceGraph.addTreeLinks "#viz" layout

        H.modify_ _
          { viewState = Overview ForceView
          , treeAnimation = Nothing
          , treeRootId = Nothing
          , forcePhase = Just TreeGrowth
          , forceProgress = 0.0
          , forceLayout = Just layout
          , simulation = Nothing
          }

        log "[App] Starting force animation loop (phase 1: TreeGrowth)"
        startForceAnimationLoop
    where
      length = Array.length

  FocusSubtree nodeId -> do
    log $ "[App] FocusSubtree on node " <> show nodeId
    state <- H.get
    -- Clean up any existing force simulation
    cleanupForceSimulation

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
          , treeStyle = TreeStyle.horizontalTree
          , forcePhase = Nothing
          , forceLayout = Nothing
          , simulation = Nothing
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
              , style: state.treeStyle
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
    case state.forcePhase, state.forceLayout, state.modelData of
      Just phase, Just layout, Just model -> do
        -- Advance progress
        let newProgress = min 1.0 (state.forceProgress + animationDelta)

        case phase of
          TreeGrowth -> do
            -- Render tree growth animation
            liftEffect $ ForceGraph.renderTreeGrowthPhase "#viz" layout state.forceProgress

            if newProgress >= 1.0
              then do
                -- Transition to LinkMorph
                log "[App] Phase 1 complete, transitioning to LinkMorph"
                H.modify_ _ { forcePhase = Just LinkMorph, forceProgress = 0.0 }
                startForceAnimationLoop
              else do
                H.modify_ _ { forceProgress = newProgress }
                startForceAnimationLoop

          LinkMorph -> do
            -- Render link morph animation
            liftEffect $ ForceGraph.renderLinkMorphPhase "#viz" layout state.forceProgress

            if newProgress >= 1.0
              then do
                -- Transition to ForceActive - create and start simulation
                log "[App] Phase 2 complete, transitioning to ForceActive"
                startForceSimulation layout model
              else do
                H.modify_ _ { forceProgress = newProgress }
                startForceAnimationLoop

          ForceActive ->
            -- This shouldn't happen - ForceActive uses SimTick, not AnimationTick
            pure unit

      _, _, _ -> pure unit

  ForceSimTick -> do
    -- Tick from D3 simulation - update positions
    state <- H.get
    case state.simulation, state.forcePhase of
      Just sim, Just ForceActive -> do
        -- Get LIVE nodes from simulation
        currentNodes <- liftEffect $ Sim.getNodes sim

        -- Render using live positions
        liftEffect $ ForceGraph.renderForcePhase "#viz" currentNodes

      _, _ -> pure unit

  ToggleForceProjectOnly -> do
    log "[App] Toggling force project-only filter"
    state <- H.get
    case state.simulation, state.modelData, state.forceLayout of
      Just sim, Just model, Just layout -> do
        let newProjectOnly = not state.forceShowProjectOnly

        -- Filter nodes based on toggle
        -- Use the root module's package as the "project" package
        let rootId = TreeLayout.findRootModule model.nodes
            rootPackage = findPackageOfNode rootId model.nodes
        let filteredNodes = if newProjectOnly
              then Array.filter (\n -> n.package == rootPackage) model.nodes
              else model.nodes

        -- Filter links to only those between filtered nodes
        let nodeIds = Array.fromFoldable $ map _.id filteredNodes
            filteredLinks = Array.filter
              (\l -> Array.elem l.source nodeIds && Array.elem l.target nodeIds)
              model.links

        log $ "[App] Project-only: " <> show newProjectOnly
            <> " -> " <> show (Array.length filteredNodes) <> " nodes, "
            <> show (Array.length filteredLinks) <> " links"

        -- Recompute layout for filtered nodes
        let rootId = TreeLayout.findRootModule filteredNodes
            newLayout = ForceGraph.computeLayout rootId filteredNodes filteredLinks

        -- Add new tree links to DOM
        liftEffect $ ForceGraph.addTreeLinks "#viz" newLayout

        -- Create new simulation with filtered data
        callbacks <- liftEffect defaultCallbacks
        newSim <- liftEffect $ ForceGraph.createSimulationWithCallbacks
          callbacks newLayout filteredNodes filteredLinks

        -- Subscribe to new simulation
        emitter <- liftEffect $ subscribeToSimulation newSim
        void $ H.subscribe $ emitter <#> \event -> case event of
          Tick -> ForceSimTick
          _ -> ForceSimTick

        -- Start the new simulation
        liftEffect $ Sim.start newSim

        -- Cleanup old simulation
        liftEffect $ ForceGraph.cleanupSimulation sim

        H.modify_ _
          { forceShowProjectOnly = newProjectOnly
          , forceLayout = Just newLayout
          , simulation = Just newSim
          }

      _, _, _ -> pure unit

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

-- =============================================================================
-- Force Simulation Helpers
-- =============================================================================

-- | Start the force simulation (phase 3)
-- | Creates simulation with callbacks and subscribes to ticks
startForceSimulation
  :: forall output m. MonadAff m
  => TreeLayout.TreeLayoutResult
  -> ModelData
  -> H.HalogenM State Action () output m Unit
startForceSimulation layout model = do
  log "[App] Creating force simulation with Halogen callbacks"

  -- Create callbacks for Halogen subscription
  callbacks <- liftEffect defaultCallbacks

  -- Create simulation (nodes positioned from layout)
  sim <- liftEffect $ ForceGraph.createSimulationWithCallbacks callbacks layout model.nodes model.links

  -- Subscribe to simulation events via Halogen subscription
  emitter <- liftEffect $ subscribeToSimulation sim
  void $ H.subscribe $ emitter <#> \event -> case event of
    Tick -> ForceSimTick
    _ -> ForceSimTick  -- Map all events to tick for now

  -- Start the simulation
  liftEffect $ Sim.start sim

  -- Update state
  H.modify_ _
    { forcePhase = Just ForceActive
    , forceProgress = 0.0
    , simulation = Just sim
    }

  log "[App] Force simulation started and subscribed"

-- | Clean up any existing force simulation
cleanupForceSimulation
  :: forall output m. MonadAff m
  => H.HalogenM State Action () output m Unit
cleanupForceSimulation = do
  state <- H.get
  case state.simulation of
    Just sim -> do
      liftEffect $ ForceGraph.cleanupSimulation sim
      log "[App] Force simulation cleaned up"
    Nothing -> pure unit

-- =============================================================================
-- Animation Helpers
-- =============================================================================

-- | Animation speed (progress per tick)
-- | At ~60fps with 16ms delay: 0.02 = 50 ticks = ~0.8 seconds
animationDelta :: Tick.TickDelta
animationDelta = 0.02

-- | Start the tree animation loop by forking an action after a short delay
startAnimationLoop :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
startAnimationLoop = do
  void $ H.fork do
    H.liftAff $ Aff.delay (Milliseconds 16.0)
    handleAction AnimationTick

-- | Start the force animation loop (phases 1-2, Halogen-driven)
startForceAnimationLoop :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
startForceAnimationLoop = do
  void $ H.fork do
    H.liftAff $ Aff.delay (Milliseconds 16.0)
    handleAction ForceAnimationTick

-- =============================================================================
-- Callback Helpers
-- =============================================================================

-- | Build treemap callbacks with click/hover -> action bridge
makeTreemapCallbacks :: Maybe (HS.Listener Action) -> Treemap.Callbacks
makeTreemapCallbacks mListener =
  { onNodeClick: \node -> case mListener of
      Just listener -> HS.notify listener (FocusSubtree node.id)
      Nothing -> pure unit
  , onNodeHover: \mNode -> case mListener of
      Just listener -> HS.notify listener (HandleHover mNode)
      Nothing -> pure unit
  }

-- | Find the package of a node by ID
findPackageOfNode :: Int -> Array SimNode -> String
findPackageOfNode nodeId nodes =
  case Array.find (\n -> n.id == nodeId) nodes of
    Just node -> node.package
    Nothing -> "unknown"

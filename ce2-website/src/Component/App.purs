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
import Viz.Treemap as Treemap
import Viz.TreeView as TreeView
import PSD3.Transition.Tick as Tick

-- =============================================================================
-- Types
-- =============================================================================

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

    -- Model data (loaded from API)
  , modelData :: Maybe ModelData

    -- Animation state
  , treeAnimation :: Maybe TreeView.AnimationState

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
  | AnimationTick

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
  , modelData: Nothing
  , treeAnimation: Nothing
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

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[App] Initializing..."

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

        callbacks :: Treemap.Callbacks
        callbacks =
          { onNodeClick: \_ -> pure unit  -- TODO: handle clicks
          , onNodeHover: \mNode -> case mNode of
              Nothing -> Treemap.clearHighlight "#viz"
              Just n -> Treemap.highlightDependencies "#viz" n
          }

    liftEffect $ Treemap.render config callbacks model.nodes
    log "[App] Treemap rendered"

  DataFailed err -> do
    log $ "[App] Data load failed: " <> err
    H.modify_ _ { phase = Error err }

  SwitchToTreemap -> do
    log "[App] Switching to Treemap view"
    state <- H.get
    case state.modelData of
      Nothing -> pure unit
      Just model -> do
        H.modify_ _ { viewState = Overview TreemapView, treeAnimation = Nothing }

        let config :: Treemap.Config
            config =
              { containerSelector: "#viz"
              , viewState: Overview TreemapView
              , packageCount: model.packageCount
              , packages: model.packages
              }

            callbacks :: Treemap.Callbacks
            callbacks =
              { onNodeClick: \_ -> pure unit
              , onNodeHover: \mNode -> case mNode of
                  Nothing -> Treemap.clearHighlight "#viz"
                  Just n -> Treemap.highlightDependencies "#viz" n
              }

        liftEffect $ Treemap.render config callbacks model.nodes
        log "[App] Treemap rendered"

  SwitchToTree -> do
    log "[App] SwitchToTree clicked"
    state <- H.get
    case state.modelData of
      Nothing -> log "[App] No model data, skipping"
      Just _model -> do
        log "[App] Initializing animation state"
        -- Initialize animation and update state
        H.modify_ _
          { viewState = Overview TreeView
          , treeAnimation = Just TreeView.initAnimation
          }

        log "[App] Starting animation loop"
        -- Start animation loop
        startAnimationLoop

  AnimationTick -> do
    state <- H.get
    case state.treeAnimation, state.modelData of
      Just anim, Just model | TreeView.isAnimating anim -> do
        -- Render FIRST with current animation state (before ticking)
        -- This ensures isFirstFrame is true on the first call
        -- renderAnimated returns updated state with cached tree layout
        let config :: TreeView.Config
            config = { containerSelector: "#viz" }

        animWithLayout <- liftEffect $ TreeView.renderAnimated config anim model.nodes model.links

        -- THEN advance animation for next frame
        let newAnim = TreeView.tickAnimation animationDelta animWithLayout
        H.modify_ _ { treeAnimation = Just newAnim }

        -- Continue loop if still animating
        when (TreeView.isAnimating newAnim) do
          startAnimationLoop

      _, _ -> pure unit

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

module Component.LesMisGUP where

-- | Fully declarative Les Misérables force-directed graph demo
-- |
-- | Demonstrates the "impossible to mess up" declarative pattern:
-- | - Scene-based architecture with complete scene specifications
-- | - Automatic link filtering (zero boilerplate)
-- | - Clean scene switching (just change the scene config)
-- | - Auto-cycling through different visualizations
-- |
-- | This is the clean, simple way to build D3 visualizations in PureScript!

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Data.Array (length)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import D3.Viz.LesMis.LesMisGUP as LesMisGUP
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import PSD3.Data.Node (D3Link_Swizzled)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3v2.Interpreter.D3v2 as D3v2
import PSD3v2.Selection.Types (SEmpty)
import Web.DOM.Element (Element)

-- | Application state
type State =
  { simulation :: D3SimulationState_ LesMisSimNode
  , model :: Maybe LesMisRawModel
  , selections :: Maybe
      { nodesGroup :: D3v2.D3v2Selection_ SEmpty Element LesMisSimNode
      , linksGroup :: D3v2.D3v2Selection_ SEmpty Element D3Link_Swizzled
      }
  , currentScene :: SceneType
  , autoCycling :: Boolean
  }

-- | Scene types for easy switching
data SceneType
  = FullGraph
  | FilteredGraph Int  -- minimum group
  | Grid Number        -- grid spacing
  | Phylotaxis

derive instance eqSceneType :: Eq SceneType

-- | User actions
data Action
  = Initialize
  | SwitchToScene SceneType
  | ToggleAutoCycle
  | AutoCycleNext

-- | Forces configuration for LesMis
forces :: { center :: Force LesMisSimNode, collision :: Force LesMisSimNode, links :: Force LesMisSimNode, manyBodyNeg :: Force LesMisSimNode }
forces =
  { manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  , collision: createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]
  , center: createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links: createLinkForce allNodes []
  }

forceLibrary :: Map.Map String (Force LesMisSimNode)
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

activeForces :: Set.Set Label
activeForces = Set.fromFoldable [ "many body negative", "collision", "center", linksForceName_ ]

-- | Halogen component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { simulation: initialSimulationState forceLibrary
        , model: Nothing
        , selections: Nothing
        , currentScene: FullGraph
        , autoCycling: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- | Render the UI
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "lesmis-gup-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Les Misérables - Declarative Scene-Based Visualization" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Fully declarative force-directed graph using the scene-based pattern. Zero boilerplate, automatic link filtering, clean scene switching." ]
        ]

    , HH.div [ HP.class_ (HH.ClassName "controls-panel") ]
        [ HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.h3_ [ HH.text "Scene Selection" ]
            , HH.p_ [ HH.text "Switch between different visualization scenes:" ]
            , HH.div [ HP.class_ (HH.ClassName "button-group") ]
                [ renderSceneButton state FullGraph "Full Graph" "Force-directed with all 77 nodes"
                , renderSceneButton state (FilteredGraph 5) "Filtered (≥5)" "Show only nodes with group ≥ 5"
                , renderSceneButton state (Grid 50.0) "Grid Layout" "Nodes arranged in a grid (pinned)"
                , renderSceneButton state Phylotaxis "Phylotaxis" "Sunflower spiral pattern (pinned)"
                ]
            ]

        , HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.h3_ [ HH.text "Auto-Cycling" ]
            , HH.p_ [ HH.text "Automatically cycle through all scenes:" ]
            , HH.div [ HP.class_ (HH.ClassName "button-group") ]
                [ HH.button
                    [ HP.class_ (HH.ClassName $ "control-button" <> if state.autoCycling then " active" else "")
                    , HE.onClick \_ -> ToggleAutoCycle
                    ]
                    [ HH.text if state.autoCycling then "⏸ Stop Auto-Cycle" else "▶ Start Auto-Cycle" ]
                ]
            , if state.autoCycling
                then HH.p [ HP.class_ (HH.ClassName "info-text") ]
                  [ HH.text "Switching scenes every 4 seconds..." ]
                else HH.text ""
            ]
        ]

    , HH.div
        [ HP.id "lesmis-gup-viz"
        , HP.class_ (HH.ClassName "viz-container")
        ]
        []

    , HH.div [ HP.class_ (HH.ClassName "info-panel") ]
        [ HH.h3_ [ HH.text "Declarative Pattern Benefits" ]
        , HH.ul_
            [ HH.li_ [ HH.text "✅ Scene-based architecture - complete specifications, no partial updates" ]
            , HH.li_ [ HH.text "✅ Automatic link filtering - library ensures consistency" ]
            , HH.li_ [ HH.text "✅ Zero boilerplate - no manual GUP steps, no wrapper types" ]
            , HH.li_ [ HH.text "✅ Impossible to mess up - scene configs are validated at compile time" ]
            , HH.li_ [ HH.text "✅ Clean scene switching - just one function call" ]
            ]
        , HH.h3_ [ HH.text "Current Scene" ]
        , HH.p_ [ HH.text $ sceneDescription state.currentScene ]
        ]
    ]

-- | Render a scene button with active state
renderSceneButton :: forall m. State -> SceneType -> String -> String -> H.ComponentHTML Action () m
renderSceneButton state sceneType label description =
  HH.button
    [ HP.class_ (HH.ClassName $ "control-button" <> if state.currentScene == sceneType then " active" else "")
    , HE.onClick \_ -> SwitchToScene sceneType
    , HP.title description
    ]
    [ HH.text label ]

-- | Get description of a scene
sceneDescription :: SceneType -> String
sceneDescription = case _ of
  FullGraph -> "Full force-directed graph with all 77 nodes"
  FilteredGraph minGroup -> "Filtered graph showing only nodes with group ≥ " <> show minGroup
  Grid spacing -> "Grid layout with " <> show spacing <> "px spacing (pinned)"
  Phylotaxis -> "Phylotaxis (sunflower spiral) layout (pinned)"

-- | Handle user actions
handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "LesMisGUP: Initializing declarative visualization"
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Les Misérables data
    lesMisResponse <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    case lesMisResponse of
      Left _ -> do
        log "LesMisGUP: Failed to load data"

      Right response -> do
        let model = readGraphFromFileContents (Right response)
        log $ "LesMisGUP: Loaded " <> show (length model.nodes) <> " nodes and " <> show (length model.links) <> " links"
        H.modify_ _ { model = Just model }

        -- Get simulation state
        state <- H.get

        -- Draw initial visualization using declarative pattern
        Tuple result newState <- H.liftEffect $ D3v2.runD3v2SimM state do
          let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
          LesMisGUP.drawLesMisGUP forcesArray activeForces model "#lesmis-gup-viz"

        -- Store selections and update simulation state
        H.put newState
        H.modify_ _ { selections = Just { nodesGroup: result.nodesGroup, linksGroup: result.linksGroup } }

        log "LesMisGUP: Initial visualization complete"

  SwitchToScene sceneType -> do
    log $ "LesMisGUP: Switching to scene: " <> sceneDescription sceneType
    state <- H.get

    case state.model, state.selections of
      Just model, Just sels -> do
        -- Run the scene switch in the D3v2SimM monad
        let selections = { nodes: sels.nodesGroup, links: sels.linksGroup }
        Tuple _ newState <- H.liftEffect $ D3v2.runD3v2SimM state do
          case sceneType of
            FullGraph ->
              LesMisGUP.switchToFullGraph selections model activeForces

            FilteredGraph minGroup ->
              LesMisGUP.switchToFilteredGraph selections model minGroup activeForces

            Grid spacing ->
              LesMisGUP.switchToGrid selections model spacing

            Phylotaxis ->
              LesMisGUP.switchToPhylotaxis selections model

        -- Update simulation state and current scene
        H.put newState
        H.modify_ _ { currentScene = sceneType }

      _, _ ->
        log "LesMisGUP: Cannot switch scene - no model or selections available"

  ToggleAutoCycle -> do
    state <- H.get
    let newState = not state.autoCycling
    H.modify_ _ { autoCycling = newState }

    if newState then do
      log "LesMisGUP: Starting auto-cycle"
      -- Start the auto-cycle loop
      handleAction AutoCycleNext
    else
      log "LesMisGUP: Stopping auto-cycle"

  AutoCycleNext -> do
    state <- H.get

    -- Only continue if auto-cycling is enabled
    when state.autoCycling do
      -- Determine next scene
      let nextScene = case state.currentScene of
            FullGraph -> FilteredGraph 5
            FilteredGraph 5 -> Grid 50.0
            Grid _ -> Phylotaxis
            Phylotaxis -> FullGraph
            _ -> FullGraph

      -- Switch to next scene
      handleAction (SwitchToScene nextScene)

      -- Wait 4 seconds then cycle again
      H.liftAff $ delay (Milliseconds 4000.0)
      handleAction AutoCycleNext

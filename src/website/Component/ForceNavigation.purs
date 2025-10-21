module PSD3.ForceNavigator where

import Prelude

import Control.Monad.State (class MonadState, get)
import D3.Attributes.Sugar (onMouseEventEffectful)
import D3.Data.Types (MouseEvent(..))
import D3.Node (D3_SimulationNode(..))
import D3.Selection (SelectionAttribute)
import D3.Simulation.Types (SimVariable(..), getStatusMap)
import D3.Viz.ForceNavigator.Data (navigationData)
import D3.Viz.ForceNavigator.Model (NodeType(..))
import D3Tagless.Capabilities (actualizeForces, setConfigVariable, start, stop)
import D3Tagless.Instance.Simulation (evalEffectSimulation, runWithD3_Simulation)
import Data.Array (filter)
import Data.Lens (use, (.=))
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (null)
import Data.Set as Set
import Debug (spy)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import PSD3.ForceNavigator.Actions (Action(..), VizEvent(..))
import PSD3.ForceNavigator.Draw as Draw
import PSD3.ForceNavigator.Forces (forceLibrary)
import PSD3.ForceNavigator.State (State, _callback, _expandedNodes, _openSelections, initialState, visibleLinks, visibleNodes)

data FN_Output = 
    GoToExample String
  | OpenNewTab

component :: forall query m. MonadAff m => H.Component query Unit FN_Output m
component = H.mkComponent
  { initialState: const $ initialState forceLibrary -- all forces enabled from the get go
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize = Just Finalize
    }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [] []  -- Empty render - all visualization is done via D3

-- | Create a callback for simulation events
simulationEvent :: HS.Listener Action -> SelectionAttribute
simulationEvent l = onMouseEventEffectful MouseClick (\e d t -> liftEffect $ HS.notify l (EventFromVizualization (Draw.getVizEventFromClick e d t)))

handleAction :: forall m.
  MonadAff m =>
  Action ->
  HalogenM State Action () FN_Output m Unit
handleAction = case _ of

  Initialize -> do
    -- Initialize SVG structure
    openSels <- evalEffectSimulation Draw.initialize
    _openSelections .= Just openSels

    -- Create subscription for viz events
    { emitter, listener } <- liftEffect $ HS.create
    -- now hook up this emitter so that Halogen actions will be triggered from the emitter
    void $ H.subscribe emitter
    _callback .= (simulationEvent listener)
    pure unit

  Finalize -> pure unit

  EventFromVizualization (NodeClick nodeId) -> do
    -- Find the clicked node to determine its type
    H.liftEffect $ log "got an event from the force layout"
    let (D3SimNode clickedNode) = case filter (\(D3SimNode n) -> n.id == nodeId) navigationData.nodes of
          [node] -> node
          _ -> D3SimNode { id: "", label: "", nodeType: Section, category: Nothing, children: Nothing, url: Nothing, external: Nothing, description: Nothing, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0, fx: null, fy: null }

    case clickedNode.nodeType of
      -- Toggle expansion for sections with children
      Section | isJust clickedNode.children -> handleAction $ ToggleExpansion nodeId
      -- Navigate for examples
      Example -> handleAction $ NavigateToExample nodeId
      -- Navigate for sections with URLs
      Section | isJust clickedNode.url -> case clickedNode.url, clickedNode.external of
        -- Just url, Just ext -> handleAction $ NavigateToUrl url ext
        -- Just url, Nothing -> handleAction $ NavigateToUrl url false
        _, _ -> pure unit -- TODO removed pending solution with purescript libraries
      -- Do nothing for other node types
      _ -> pure unit

  ToggleExpansion nodeId -> do
    expanded <- use _expandedNodes
    let newExpanded = if Set.member nodeId expanded
                      then Set.delete nodeId expanded
                      else Set.insert nodeId expanded
    _expandedNodes .= newExpanded

    -- Re-run simulation with updated expansion
    -- TODO: Get callback from somewhere - for now use dummy
    runSimulation 

  NavigateToExample exampleId -> do -- WIP TODO we just raise this to the parent, not implement it here
    -- Navigate to example page
    H.raise $ spy "Raising a GoToExample event on the parent component" $ GoToExample exampleId

runSimulation :: forall m.
  MonadEffect m =>
  MonadState State m =>
  m Unit
runSimulation = do
  state <- get
  openSels <- use _openSelections
  expanded <- use _expandedNodes
  callback <- use _callback

  -- Filter data based on expansion state
  let visible = visibleNodes expanded navigationData.nodes
      links = visibleLinks visible navigationData.links
      model = { nodes: visible, links: links }
      -- attributesWithCallback = sceneAttributes { circles = callback : sceneAttributes.circles }


  runWithD3_Simulation do
      stop
      actualizeForces (getStatusMap forceLibrary)
      -- Draw.updateSimulation { nodes: Nothing, links: Nothing } 
      setConfigVariable $ Alpha 1.0
      start

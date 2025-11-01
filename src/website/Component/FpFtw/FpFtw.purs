module PSD3.FpFtw where

import Prelude

import D3.Viz.FpFtw.MapQuartet as MapQuartet
import D3.Viz.FpFtw.TopologicalSort as TopologicalSort
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import PSD3.FpFtw.Actions (Action(..))
import PSD3.FpFtw.HTML (render)
import PSD3.FpFtw.State (State, initialState)
import PSD3.Interpreter.D3 (eval_D3M, runWithD3_Simulation)

-- | FP FTW component - Functional Programming examples
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- | Handle actions
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    log "FP FTW: Initializing..."

    -- Generate randomized Map quartet data
    log "FP FTW: Generating Map quartet data..."
    quartetData <- H.liftEffect MapQuartet.generateMapQuartet
    log "FP FTW: Map quartet data generated"

    -- Draw the Map quartet visualization
    log "FP FTW: Drawing Map quartet..."
    _ <- H.liftEffect $ eval_D3M $ MapQuartet.drawMapQuartet quartetData "div.map-quartet-viz"
    log "FP FTW: Map quartet drawn"

    -- Draw the build pipeline topological sort first (for explanation)
    log "FP FTW: Drawing build pipeline topological sort..."
    _ <- H.liftEffect $ eval_D3M $ TopologicalSort.drawTopologicalSort TopologicalSort.buildPipelineTasks "div.topological-sort-viz"
    log "FP FTW: Build pipeline drawn"

    -- Load and visualize LesMiserables graph with force-directed layout
    log "FP FTW: Loading LesMiserables graph..."
    lesMisResult <- H.liftAff TopologicalSort.loadLesMisGraph
    case lesMisResult of
      Left err -> do
        log $ "FP FTW: Failed to load LesMis: " <> err
      Right lesMisGraph -> do
        log "FP FTW: LesMiserables graph loaded!"
        let lesMisTasks = TopologicalSort.lesMisToTasks lesMisGraph
        log $ "FP FTW: Converted to " <> show (Array.length lesMisTasks) <> " tasks"
        -- Draw it using force-directed layout with fy-fixed layers
        log "FP FTW: Drawing LesMis topological layers with force simulation..."
        runWithD3_Simulation do
          TopologicalSort.drawTopologicalForceDirected lesMisTasks "div.lesmis-layers-viz"
        log "FP FTW: LesMis topological layers drawn!"
        pure unit

    pure unit

  SelectExample exampleId -> do
    H.modify_ _ { currentExample = exampleId }

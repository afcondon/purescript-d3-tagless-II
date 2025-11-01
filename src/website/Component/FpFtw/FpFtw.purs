module PSD3.FpFtw where

import Prelude

import D3.Viz.FpFtw.MapQuartet as MapQuartet
import D3.Viz.FpFtw.TopologicalSort as TopologicalSort
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import PSD3.FpFtw.Actions (Action(..))
import PSD3.FpFtw.HTML (render)
import PSD3.FpFtw.State (State, initialState)
import PSD3.Interpreter.D3 (eval_D3M)

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

    -- Draw the topological sort visualization
    log "FP FTW: Drawing topological sort..."
    _ <- H.liftEffect $ eval_D3M $ TopologicalSort.drawTopologicalSort TopologicalSort.buildPipelineTasks "div.topological-sort-viz"
    log "FP FTW: Topological sort drawn"

    pure unit

  SelectExample exampleId -> do
    H.modify_ _ { currentExample = exampleId }

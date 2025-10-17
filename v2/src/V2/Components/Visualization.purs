module V2.Components.Visualization where

import Prelude

import D3.Examples.Charts.Model as Charts
import D3.Examples.LineChart as LineChart
import D3.Examples.BarChart as BarChart
import D3.Examples.ScatterPlot as ScatterPlot
import D3.Examples.ChordDiagram as ChordDiagram
import D3.Examples.ThreeLittleCircles as ThreeLittleCircles
import D3Tagless.Instance.Selection (eval_D3M)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import V2.Types (ExampleId)
import Data.Maybe (Maybe(..))

-- | Input for the Visualization component
type Input = {
  exampleId :: ExampleId
}

-- | No slots needed
type Slots :: forall k. Row k
type Slots = ()

-- | State just tracks the example ID
type State = {
  exampleId :: ExampleId,
  containerId :: String
}

-- | Actions
data Action = Initialize

-- | Component definition
component :: forall q o m. MonadAff m => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: Input -> State
initialState input =
  { exampleId: input.exampleId
  , containerId: "viz-container-" <> input.exampleId
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "visualization" ]
    , HP.id state.containerId
    ]
    []

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let selector = "#" <> state.containerId

    case state.exampleId of
      -- Basic Charts
      "line-chart" -> do
        _ <- liftEffect $ eval_D3M $ LineChart.draw Charts.sineWaveData selector
        pure unit

      "bar-chart" -> do
        _ <- liftEffect $ eval_D3M $ BarChart.draw Charts.monthlySales selector
        pure unit

      "scatter-plot" -> do
        _ <- liftEffect $ eval_D3M $ ScatterPlot.draw Charts.scatterData selector
        pure unit

      "scatter-quartet" -> do
        _ <- liftEffect $ eval_D3M $ ScatterPlot.drawQuartet Charts.anscombesQuartet selector
        pure unit

      -- Advanced Layouts
      "chord-diagram" -> do
        _ <- liftEffect $ eval_D3M $ ChordDiagram.draw ChordDiagram.exampleMatrix ChordDiagram.exampleLabels selector
        pure unit

      -- Interactive Examples
      "three-little-circles" -> do
        _ <- liftEffect $ eval_D3M $ ThreeLittleCircles.drawThreeCircles selector
        pure unit

      _ -> do
        -- Other examples not yet implemented:
        -- bubble-chart, sankey, tree (need file loading or complex state)
        -- gup, les-mis (need Halogen integration for interactivity)
        -- meta-tree, print-tree, spago (need different interpreters/setup)
        pure unit

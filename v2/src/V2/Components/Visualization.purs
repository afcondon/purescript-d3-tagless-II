module V2.Components.Visualization where

import Prelude

import D3.Data.Tree (TreeLayout(..), TreeType(..))
import D3.Examples.Charts.Model as Charts
import D3.Examples.LineChart as LineChart
import D3.Examples.BarChart as BarChart
import D3.Examples.ScatterPlot as ScatterPlot
import D3.Examples.ChordDiagram as ChordDiagram
import D3.Examples.ThreeLittleCircles as ThreeLittleCircles
import D3.Examples.Tree.Configure as Tree
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import D3Tagless.Instance.Selection (eval_D3M)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import V2.Types (ExampleId)
import Data.Maybe (Maybe(..))
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- | Input for the Visualization component
type Input = {
  exampleId :: ExampleId
}

-- | No slots needed
type Slots :: forall k. Row k
type Slots = ()

-- | State tracks the example ID and optional text output (for print-tree)
type State = {
  exampleId :: ExampleId,
  containerId :: String,
  textOutput :: Maybe String
}

-- | Actions
data Action
  = Initialize
  | HighlightCode

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
  , textOutput: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "visualization" ]
    , HP.id state.containerId
    ]
    -- If we have text output (for print-tree), display it as pre/code with Prism highlighting
    ( case state.textOutput of
        Just text ->
          [ HH.pre
              [ HP.classes [ HH.ClassName "visualization__text-output", HH.ClassName "line-numbers" ] ]
              [ HH.code
                  [ HP.classes [ HH.ClassName "language-javascript" ]
                  , HP.id ("generated-js-" <> state.exampleId)
                  ]
                  [ HH.text text ]
              ]
          ]
        Nothing -> []
    )

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

      -- String Interpreter (print-tree)
      "print-tree" -> do
        -- Load tree data from JSON
        treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case treeJSON of
          Left _err -> pure unit  -- Handle error silently for now
          Right tree -> do
            -- Create tree model
            treeModel <- H.liftAff $ makeModel TidyTree Radial tree
            -- Get string representation using printer interpreter
            textRep <- H.liftAff $ Tree.getPrintTree treeModel
            -- Store in state for rendering
            H.modify_ _ { textOutput = Just textRep }
            -- Trigger syntax highlighting
            handleAction HighlightCode
        pure unit

      _ -> do
        -- Other examples not yet implemented:
        -- bubble-chart, sankey, tree (need file loading or complex state)
        -- gup, les-mis (need Halogen integration for interactivity)
        -- meta-tree, spago (need different interpreters/setup)
        pure unit

  HighlightCode -> do
    state <- H.get
    -- Only highlight if we have text output (for print-tree example)
    case state.textOutput of
      Just _ -> do
        -- Call Prism.highlightElement() via FFI
        H.liftEffect do
          win <- window
          htmlDoc <- document win
          let doc = toDocument htmlDoc
          let node = toNonElementParentNode doc
          maybeEl <- getElementById ("generated-js-" <> state.exampleId) node
          traverse_ highlightElement maybeEl
      Nothing -> pure unit

-- | FFI function to trigger Prism highlighting
foreign import highlightElement :: forall a. a -> Effect Unit

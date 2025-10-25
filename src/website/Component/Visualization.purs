module PSD3.Visualization where

import Prelude

import PSD3.Data.Tree (TreeLayout(..), TreeType(..))
import D3.Viz.Charts.Model as Charts
import D3.Viz.LineChart as LineChart
import D3.Viz.BarChart as BarChart
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.ChordDiagram as ChordDiagram
import D3.Viz.BubbleChart as BubbleChart
import D3.Viz.Sankey.Model as Sankey
import D3.Viz.SankeyDiagram as SankeyDiagram
import D3.Viz.GUP as GUP
import D3.Viz.ThreeLittleCircles as ThreeLittleCircles
import D3.Viz.Tree.Configure as Tree
import D3.Viz.TreeSimple as TreeSimple
import PSD3.Internal.Hierarchical (getTreeViaAJAX, makeModel)
import PSD3.Internal.Sankey.Types (initialSankeyLayoutState)
import PSD3.Interpreter.D3 (eval_D3M, runD3M)
import PSD3.Interpreter.D3 (eval_D3M_Sankey)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Types (ExampleId)
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

      "bubble-chart" -> do
        jsonData <- H.liftAff BubbleChart.loadFlareData
        _ <- liftEffect $ eval_D3M $ BubbleChart.draw jsonData selector
        pure unit

      "sankey" -> do
        let sankeyState = { sankeyLayout: initialSankeyLayoutState }
        _ <- liftEffect $ eval_D3M_Sankey sankeyState $ SankeyDiagram.draw Sankey.energyData selector
        pure unit

      "tree-horizontal" -> do
        treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case treeJSON of
          Left _err -> pure unit
          Right json -> do
            _ <- liftEffect $ eval_D3M $ TreeSimple.drawTreeHorizontal json selector
            pure unit

      "tree-vertical" -> do
        treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case treeJSON of
          Left _err -> pure unit
          Right json -> do
            _ <- liftEffect $ eval_D3M $ TreeSimple.drawTreeVertical json selector
            pure unit

      "tree-radial" -> do
        treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
        case treeJSON of
          Left _err -> pure unit
          Right json -> do
            _ <- liftEffect $ eval_D3M $ TreeSimple.drawTreeRadial json selector
            pure unit

      -- Interactive Examples
      "three-little-circles" -> do
        _ <- liftEffect $ eval_D3M $ ThreeLittleCircles.drawThreeCircles selector
        pure unit

      -- General Update Pattern (animated)
      "gup" -> do
        -- Set up the GUP visualization and get the update function
        update <- liftEffect $ eval_D3M $ GUP.exGeneralUpdatePattern selector
        -- Run a few updates to demonstrate the pattern
        H.liftAff do
          -- First update with some letters
          letters1 <- liftEffect getRandomLetters
          _ <- liftEffect $ runD3M (update letters1)
          delay (Milliseconds 2300.0)
          -- Second update with different letters
          letters2 <- liftEffect getRandomLetters
          _ <- liftEffect $ runD3M (update letters2)
          delay (Milliseconds 2300.0)
          -- Third update
          letters3 <- liftEffect getRandomLetters
          _ <- liftEffect $ runD3M (update letters3)
          pure unit
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

-- | Helper function to generate random letters for GUP example
-- | Choose a string of random letters (no duplicates), ordered alphabetically
getRandomLetters :: Effect (Array Char)
getRandomLetters = do
  let letters = toCharArray "abcdefghijklmnopqrstuvwxyz"
      coinToss :: Char -> Effect (Maybe Char)
      coinToss c = do
        n <- random
        pure $ if n > 0.6 then Just c else Nothing

  choices <- sequence $ coinToss <$> letters
  pure $ catMaybes choices

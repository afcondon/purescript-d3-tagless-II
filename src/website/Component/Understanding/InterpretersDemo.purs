module PSD3.Understanding.InterpretersDemo where -- Understanding

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, cx, cy, fill, fillOpacity, height, radius, viewBox, width)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Element(..))
import PSD3.Data.Node (NodeID)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Interpreter.MetaTree (D3MetaTreeM, MetaTreeSelection, runMetaTree, scriptTreeToJSON)
import PSD3.Interpreter.String (D3PrinterM, StringSelection, runPrinter)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)

-- | Sample data points for scatter plot
type DataPoint = { x :: Number, y :: Number }

sampleData :: Array DataPoint
sampleData =
  [ { x: 50.0, y: 100.0 }
  , { x: 120.0, y: 80.0 }
  , { x: 200.0, y: 150.0 }
  , { x: 280.0, y: 120.0 }
  , { x: 350.0, y: 180.0 }
  , { x: 50.0, y: 200.0 }
  , { x: 150.0, y: 160.0 }
  , { x: 250.0, y: 220.0 }
  , { x: 320.0, y: 190.0 }
  , { x: 380.0, y: 250.0 }
  , { x: 90.0, y: 140.0 }
  , { x: 170.0, y: 110.0 }
  , { x: 230.0, y: 170.0 }
  , { x: 310.0, y: 210.0 }
  , { x: 360.0, y: 240.0 }
  , { x: 110.0, y: 90.0 }
  , { x: 190.0, y: 130.0 }
  , { x: 270.0, y: 200.0 }
  , { x: 330.0, y: 180.0 }
  , { x: 370.0, y: 220.0 }
  ]

-- | The sample visualization DSL code for D3 rendering
scatterPlotD3 :: forall m. Bind m => MonadEffect m => SelectionM D3Selection_ m => m Unit
scatterPlotD3 = do
  (root :: D3Selection_ Unit) <- attach "div.scatterplot-viz"
  svg <- appendTo root Svg
    [ classed "simple-scatterplot"
    , width 400.0
    , height 300.0
    , viewBox 0.0 0.0 400.0 300.0
    ]
  dotsGroup <- appendTo svg Group [ classed "dots" ]

  _ <- traverse_ (\pt -> do
    _ <- appendTo dotsGroup Circle
      [ cx pt.x
      , cy pt.y
      , radius 4.0
      , fill "steelblue"
      , fillOpacity 0.7
      ]
    pure unit
  ) sampleData

  pure unit

-- | The sample visualization for String interpreter (generates D3 code)
scatterPlotString :: forall d. D3PrinterM (StringSelection d)
scatterPlotString = do
  (root :: StringSelection d) <- attach "div.scatterplot-viz"
  svg <- appendTo root Svg
    [ classed "simple-scatterplot"
    , width 400.0
    , height 300.0
    , viewBox 0.0 0.0 400.0 300.0
    ]
  dotsGroup <- appendTo svg Group [ classed "dots" ]

  _ <- traverse_ (\pt -> do
    _ <- appendTo dotsGroup Circle
      [ cx pt.x
      , cy pt.y
      , radius 4.0
      , fill "steelblue"
      , fillOpacity 0.7
      ]
    pure unit
  ) sampleData

  pure dotsGroup

-- | The sample visualization for MetaTree interpreter (builds AST)
scatterPlotMeta :: forall d. D3MetaTreeM (MetaTreeSelection d)
scatterPlotMeta = do
  (root :: MetaTreeSelection d) <- attach "div.scatterplot-viz"
  svg <- appendTo root Svg
    [ classed "simple-scatterplot"
    , width 400.0
    , height 300.0
    , viewBox 0.0 0.0 400.0 300.0
    ]
  dotsGroup <- appendTo svg Group [ classed "dots" ]

  _ <- traverse_ (\pt -> do
    _ <- appendTo dotsGroup Circle
      [ cx pt.x
      , cy pt.y
      , radius 4.0
      , fill "steelblue"
      , fillOpacity 0.7
      ]
    pure unit
  ) sampleData

  pure root -- Return the root selection

-- | Run the visualization through the String interpreter to generate D3 code
generateD3Code :: Effect String
generateD3Code = do
  Tuple _ output <- runPrinter scatterPlotString ""
  pure output

-- | Run the visualization through the MetaTree interpreter to get AST
generateMetaTree :: Effect TreeJson_
generateMetaTree = do
  Tuple _ tree <- runMetaTree scatterPlotMeta
  pure $ scriptTreeToJSON tree

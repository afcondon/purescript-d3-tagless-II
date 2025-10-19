module PSD3.Data.CodeFiles where

import Prelude
import PSD3.Types (ExampleId)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))

-- | Map example IDs to their code file names in v1/code-examples/
codeFileMap :: Map.Map ExampleId String
codeFileMap = Map.fromFoldable
  [ Tuple "line-chart" "LineChartDraw"
  , Tuple "bar-chart" "BarChartDraw"
  , Tuple "scatter-plot" "ScatterPlotDraw"
  , Tuple "scatter-quartet" "ScatterPlotQuartet"
  , Tuple "chord-diagram" "ChordDiagramDraw"
  , Tuple "bubble-chart" "BubbleChartDraw"
  , Tuple "sankey" "SankeyDraw"
  , Tuple "tree" "TreeDraw"
  , Tuple "tree-horizontal" "TreeHorizontalDraw"
  , Tuple "tree-vertical" "TreeVerticalDraw"
  , Tuple "tree-radial" "TreeRadialDraw"
  , Tuple "three-little-circles" "3LC"
  , Tuple "gup" "GUP"
  , Tuple "les-mis" "LesMisScript"
  , Tuple "meta-tree" "MetaTreeDraw"
  , Tuple "print-tree" "PrintTreeHandleActions"
  , Tuple "spago" "LesMisScript"  -- Placeholder, uses similar code
  ]

-- | Get the code file name for an example
getCodeFile :: ExampleId -> Maybe String
getCodeFile exampleId = Map.lookup exampleId codeFileMap

-- | Get the full URL for an example's code file
getCodeFileUrl :: ExampleId -> Maybe String
getCodeFileUrl exampleId = do
  filename <- getCodeFile exampleId
  pure $ "./code-examples/" <> filename

-- | Get the visualization URL for an example
-- | (V1 removed - all examples now in V2)
getVisualizationUrl :: ExampleId -> Maybe String
getVisualizationUrl _ = Nothing

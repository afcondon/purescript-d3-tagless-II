module V2.Data.CodeFiles where

import Prelude
import V2.Types (ExampleId)
import Data.Maybe (Maybe)
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
  , Tuple "sankey-diagram" "SankeyDraw"
  , Tuple "tree-layout" "TreeDraw"
  , Tuple "three-little-circles" "3LC"
  , Tuple "general-update-pattern" "GUP"
  , Tuple "les-miserables" "LesMisScript"
  , Tuple "meta-tree" "MetaTreeDraw"
  , Tuple "print-tree" "PrintTreeHandleActions"
  , Tuple "spago-explorer" "LesMisScript"  -- Placeholder, uses similar code
  ]

-- | Get the code file name for an example
getCodeFile :: ExampleId -> Maybe String
getCodeFile exampleId = Map.lookup exampleId codeFileMap

-- | Get the full URL for an example's code file
getCodeFileUrl :: ExampleId -> Maybe String
getCodeFileUrl exampleId = do
  filename <- getCodeFile exampleId
  pure $ "./code-examples/" <> filename

-- | Map example IDs to their V1 visualization URLs
visualizationUrlMap :: Map.Map ExampleId String
visualizationUrlMap = Map.fromFoldable
  [ Tuple "line-chart" "../v1/index.html#line-chart"
  , Tuple "bar-chart" "../v1/index.html#bar-chart"
  , Tuple "scatter-plot" "../v1/index.html#scatter-plot"
  , Tuple "scatter-quartet" "../v1/index.html#scatter-quartet"
  , Tuple "chord-diagram" "../v1/index.html#chord-diagram"
  , Tuple "bubble-chart" "../v1/index.html#bubble-chart"
  , Tuple "sankey-diagram" "../v1/index.html#sankey-diagram"
  , Tuple "tree-layout" "../v1/index.html#tree-layout"
  , Tuple "three-little-circles" "../v1/index.html#three-little-circles"
  , Tuple "general-update-pattern" "../v1/index.html#general-update-pattern"
  , Tuple "les-miserables" "../v1/index.html#les-miserables"
  , Tuple "meta-tree" "../v1/index.html#metatree"
  , Tuple "print-tree" "../v1/index.html#string-generator"
  , Tuple "spago-explorer" "../v1/index.html#spago-explorer"
  ]

-- | Get the visualization URL for an example
getVisualizationUrl :: ExampleId -> Maybe String
getVisualizationUrl exampleId = Map.lookup exampleId visualizationUrlMap

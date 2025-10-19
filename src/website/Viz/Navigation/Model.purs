module D3.Viz.Navigation.Model where

import Prelude
import D3.Node (D3Link, D3_SimulationNode, D3_VxyFxy, D3_XY)
import Data.Maybe (Maybe)
import Type.Row (type (+))

-- | Node types in the navigation graph
data NodeType = Center | Section | Example | Feature

derive instance eqNodeType :: Eq NodeType
derive instance ordNodeType :: Ord NodeType

instance showNodeType :: Show NodeType where
  show Center = "center"
  show Section = "section"
  show Example = "example"
  show Feature = "feature"

-- | Categories for example nodes
data Category = BasicChart | AdvancedLayout | Interactive | Interpreter | Application

derive instance eqCategory :: Eq Category
derive instance ordCategory :: Ord Category

instance showCategory :: Show Category where
  show BasicChart = "basic-chart"
  show AdvancedLayout = "advanced-layout"
  show Interactive = "interactive"
  show Interpreter = "interpreter"
  show Application = "application"

-- | The extra/model-specific data for navigation nodes
type NavigationNodeData row = (
  id :: String
, label :: String
, nodeType :: NodeType
, category :: Maybe Category
, children :: Maybe (Array String)
, url :: Maybe String
, external :: Maybe Boolean
, description :: Maybe String
| row
)

-- | Navigation node as used in PureScript simulation
type NavigationSimNode = D3_SimulationNode (NavigationNodeData + D3_XY + D3_VxyFxy + ())

-- | Navigation node as a bare record (what D3 returns in callbacks)
type NavigationSimRecord = Record (D3_XY + D3_VxyFxy + NavigationNodeData + ())

-- | Link data (no extra data needed beyond source/target)
type NavigationLinkData = ()

-- | Link between nodes
type NavigationGraphLinkObj = { source :: NavigationSimRecord, target :: NavigationSimRecord | NavigationLinkData }

-- | The raw model with string-based links
type NavigationRawModel = {
  links :: Array (D3Link String NavigationLinkData)
, nodes :: Array NavigationSimNode
}

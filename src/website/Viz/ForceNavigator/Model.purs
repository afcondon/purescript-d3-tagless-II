module D3.Viz.ForceNavigator.Model where

import Prelude
import PSD3.Data.Node (D3Link_Unswizzled, SimulationNode, D3_VxyFxy, D3_XY)
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
type NavigationNodeRow = (
  id :: String
, label :: String
, nodeType :: NodeType
, category :: Maybe Category
, children :: Maybe (Array String)
, url :: Maybe String
, external :: Maybe Boolean
, description :: Maybe String
)

-- | Navigation node as used in simulation (phantom type friendly!)
type NavigationSimNode = SimulationNode NavigationNodeRow

-- | Legacy alias - now equivalent to NavigationSimNode
type NavigationSimRecord = NavigationSimNode

-- | Link data (no extra data needed beyond source/target)
type NavigationLinkData = ()

-- | Link between nodes
type NavigationGraphLinkObj = { source :: NavigationSimRecord, target :: NavigationSimRecord | NavigationLinkData }

-- | The raw model with string-based links (UNSWIZZLED)
type NavigationRawModel = {
  links :: Array D3Link_Unswizzled
, nodes :: Array NavigationSimNode
}

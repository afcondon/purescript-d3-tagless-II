module Stories.Spago.Actions where


import D3.Attributes.Instances (Label)
import D3.Data.Tree (TreeLayout)
import D3.Data.Types (Datum_)
import D3.Examples.Spago.Draw.Attributes (SpagoSceneAttributes)
import D3.Examples.Spago.Files (NodeType, SpagoGraphLinkID)
import D3.Examples.Spago.Model (SpagoSimNode)
import D3.Node (NodeID)
import D3.Simulation.Types (SimVariable)

data Scene = PackageGrid | PackageGraph | ModuleTree TreeLayout | LayerSwarm
data StyleChange = TopLevelCSS String | GraphStyle SpagoSceneAttributes
data FilterData = LinkShowFilter (SpagoGraphLinkID -> Boolean)
                | LinkForceFilter (Datum_ -> Boolean) -- because this is post- putting in the DOM, it's a filter on the opaque type
                | NodeFilter (SpagoSimNode -> Boolean)
data Action
  = Initialize
  | Finalize
  | Scene Scene
  | ToggleForce Label
  | Filter FilterData
  | ChangeStyling StyleChange
  | ChangeSimConfig SimVariable
  | StopSim
  | StartSim
  | EventFromVizualization VizEvent
  | ToggleChildrenOfNode NodeID
  | SpotlightNode NodeID
  | UnToggleChildrenOfNode NodeID

data VizEvent = NodeClick NodeType NodeID 
-- to be added:
-- | DragFinished
-- | LinkClick
-- and whatever other events are meaningful in terms of the graphics objects and DOM events on them
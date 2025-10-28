module PSD3.CodeExplorer.Actions where


import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Data.Tree (TreeLayout)
import PSD3.Internal.Types (Datum_)
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes)
import D3.Viz.Spago.Files (NodeType, SpagoGraphLinkID)
import D3.Viz.Spago.Model (SpagoSimNode)
import PSD3.Data.Node (NodeID)
import PSD3.Internal.Simulation.Types (SimVariable)

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
  | TagHalogen  -- Tag all Halogen-related packages
  | ClearTags   -- Clear all tags

data VizEvent = NodeClick NodeType NodeID 
-- to be added:
-- | DragFinished
-- | LinkClick
-- and whatever other events are meaningful in terms of the graphics objects and DOM events on them
module PSD3.CodeExplorer.Actions where

import Prelude

import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes)
import D3.Viz.Spago.Files (NodeType, SpagoLink)
import D3.Viz.Spago.GitMetrics (ColorByOption)
import D3.Viz.Spago.Model (SpagoSimNode)
import PSD3.Data.Node (NodeID)
import PSD3.Data.Tree (TreeLayout)
import PSD3.Internal.Attributes.Instances (Label)
import PSD3.Internal.Simulation.Types (SimVariable)

data Scene = EmptyScene | PackageGrid | PackageGraph | ModuleTree TreeLayout | LayerSwarm
derive instance eqScene :: Eq Scene
derive instance ordScene :: Ord Scene

instance showScene :: Show Scene where
  show PackageGrid = "PackageGrid"
  show PackageGraph = "PackageGraph"
  show (ModuleTree layout) = "ModuleTree " <> show layout
  show LayerSwarm = "LayerSwarm"
  show EmptyScene = "EmptyScene"
data StyleChange :: forall k. k -> Type
data StyleChange d = TopLevelCSS String | GraphStyle SpagoSceneAttributes
data FilterData = LinkShowFilter (SpagoLink -> Boolean)
                | LinkForceFilter (SpagoLink -> Boolean)  -- Now using typed link instead of Datum_
                | NodeFilter (SpagoSimNode -> Boolean)
data Action :: forall k. k -> Type
data Action d
  = Initialize
  | Finalize
  | Scene Scene
  | ToggleForce Label
  | Filter FilterData
  | ChangeStyling (StyleChange d)
  | ChangeSimConfig SimVariable
  | ChangeColorBy ColorByOption
  | StopSim
  | StartSim
  | EventFromVizualization VizEvent
  | ToggleChildrenOfNode NodeID
  | SpotlightNode NodeID
  | UnToggleChildrenOfNode NodeID
  | TagHalogen  -- Tag all Halogen-related packages
  | ClearTags   -- Clear all tags
  | DismissWelcome  -- Dismiss welcome overlay
  -- Git replay actions
  | StartReplay
  | StopReplay
  | ResetReplay
  | ReplayTick  -- Internal action for replay updates
  -- Tree revelation actions (Act One)
  | SetRevelationStep Int

data VizEvent = NodeClick NodeType NodeID 
-- to be added:
-- | DragFinished
-- | LinkClick
-- and whatever other events are meaningful in terms of the graphics objects and DOM events on them
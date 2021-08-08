module Stories.Spago.Actions where


import D3.Attributes.Instances (Label)
import D3.Data.Tree (TreeLayout)
import D3.Examples.Spago.Files (SpagoGraphLinkID)
import D3.Examples.Spago.Model (SpagoSimNode)
import D3.Simulation.Types (SimVariable)

data Scene = PackageGrid | PackageGraph | ModuleTree TreeLayout
data FilterData = LinkFilter (SpagoGraphLinkID -> Boolean)
                | NodeFilter (SpagoSimNode -> Boolean)
data Action
  = Initialize
  | Finalize
  | Scene Scene
  | ToggleForce Label
  | Filter FilterData
  | ChangeStyling String
  | ChangeSimConfig SimVariable
  | StopSim
  | StartSim

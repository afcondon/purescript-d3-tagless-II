module Stories.Spago.State where


import D3.Examples.Spago.Files (SpagoGraphLinkID)
import D3.Examples.Spago.Model (SpagoModel, SpagoSimNode)
import D3.Simulation.Types (D3SimulationState_)
import Data.Maybe (Maybe)
  
type State = {
  -- the model should actually be a component, probably a hook so that it can be constructed by this component and not be a Maybe
    model           :: Maybe SpagoModel 
  -- the Halogen app controls what is given to the simulation by giving filtered and maybe modified versions
  -- of the nodes and links in the model to the Draw functions
  , activeLinks     :: Array SpagoGraphLinkID
  , activeNodes     :: Array SpagoSimNode
  , activeForces    :: Array String
  -- governing class on the SVG means we can completely change the look of the vis (and not have to think about this at D3 level)
  , svgClass        :: String 
  -- the simulationState manages the Forces, Selections, Ticks & simulation parameters
  , simulationState :: D3SimulationState_
}


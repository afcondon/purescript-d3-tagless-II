module D3.Examples.Force where

import Affjax (Error)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, fill, radius, strokeColor, strokeOpacity, strokeWidth, viewBox)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join, runD3M)
import D3.Layouts.Simulation (D3ForceLink_, D3ForceNode_, D3Simulation_, DragBehavior(..), Force(..), ForceName(..), ForceType(..), defaultConfigSimulation, initSimulation_, putForcesInSimulation, putLinksInSimulation_, putNodesInSimulation_, startSimulation_)
import D3.Selection (D3Selection_, D3State(..), Element(..), Join(..), Keys(..), SelectionName(..), enterOnly, makeD3State', makeProjection, node)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Prelude (class Bind, Unit, bind, discard, pure, unit, ($), (*>), (/))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> Model
readGraphFromFileContents (Right { body } ) = readJSONJS body
readGraphFromFileContents (Left err)        = { links: [], nodes: [] }

drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  widthHeight <- liftEffect getWindowWidthHeight

  forceJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
  let graph      = readGraphFromFileContents forceJSON
  
  liftEffect $ runD3M (enter widthHeight) (makeD3State' { links: graph.links, nodes: graph.nodes }) *> pure unit

  pure unit



-- this is the model used by this particular "chart" (ie force layout simulation)
type NodeExtension = (group :: Number) -- any extra fields beyond what's required of all ForceLayout nodes
type LinkExtension = (value :: Number) -- empty row, this simulation doesn't yet have extra stuff in the links
type GraphNode = D3ForceNode_ NodeExtension
type GraphLink = D3ForceLink_ NodeExtension LinkExtension
type Model = { links :: Array GraphLink, nodes :: Array GraphNode }


colorByGroup :: Datum -> String
colorByGroup datum = d3SchemeCategory10JS d.group
  where
    d = datumIsGraphNode datum

linkWidth :: Datum -> Number
linkWidth datum = sqrt d.value
  where
    d = datumIsGraphLink datum

-- | recipe for this force layout graph
enter :: forall m. Bind m => D3Tagless m => MonadState (D3State Model) m => Tuple Number Number -> m D3Selection_ -- going to actually be a simulation right? 
enter (Tuple width height) = do
  root  <- hook "div#force"
  svg   <- appendTo root "svg-tree" (node Svg [ viewBox 0.0 0.0 width height ] )
  links <- appendTo svg "links-group" (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodes <- appendTo svg "nodes-group" (node Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  (D3State state) <- get
  let s   = initSimulation state.model (\model -> model.nodes) (\model -> model.links)

  linkJoinSelection_ <- join state.model $ Join {
      element   : Line
    , key       : DatumIsKey
    , hook      : SelectionName "links-group"
    , projection: makeProjection (\model -> model.links) -- NB!!!! side-effects links into sim
    , behaviour : enterOnly [ strokeWidth linkWidth ]
  }

  nodeJoinSelection <- join state.model $ Join {
      element   : Circle
    , key       : DatumIsKey
    , hook      : SelectionName "nodes-group"
    , projection: makeProjection (\model -> model.nodes) -- NB!!!! side-effects nodes into sim
    , behaviour : enterOnly [ radius 5.0, fill colorByGroup ]
  }

  let _ = startSimulation_ s

  pure svg
  

-- | definition of the particular Simulation that we are going to run
-- initSimulation :: forall model. model -> (model -> D3Data_) -> (model -> D3Data_) -> D3Simulation_
initSimulation :: forall t61 t70 t75. t61 -> (t61 -> t70) -> (t61 -> t75) -> D3Simulation_
initSimulation model nodeProjection linkProjection = do
  let nodes      =  nodeProjection model
      links      =  linkProjection model
      simulation = initSimulation_ defaultConfigSimulation
      _          = simulation `putNodesInSimulation_` (unsafeCoerce nodes)
      _          = simulation `putLinksInSimulation_` (unsafeCoerce links)
      _          = simulation `putForcesInSimulation` [ Force (ForceName "charge") ForceMany, centerForce 800.0 900.0 ]
  simulation
  --     { 
  --     label : "simulation" -- TODO stringy label
  --   , config: defaultConfigSimulation
  --   , forces:  
  --   , nodes : [] 
  --   , links : []
  --   , tick  : identity
  --   , drag  : const unit
  -- }

centerForce :: Number -> Number -> Force
centerForce width height = Force (ForceName "center") $ ForceCenter (width / 2.0) (height / 2.0)

-- | function to build the tick function, quite tricky
-- myTickMap :: TickMap Model
-- myTickMap = fromFoldable
--   [ Tuple "link" [ x1 (\d -> d.source.x)
--                  , y1 (\d -> d.source.y)
--                  , x2 (\d -> d.target.x)
--                  , y2 (\d -> d.target.y) ]
--   , Tuple "node" [ cx (\d -> d.x)
--                  , cy (\d -> d.y) ]
--   ]

-- | utility functions and boilerplate
myDrag :: DragBehavior
myDrag = DefaultDrag "node" "simulation"

makeModel :: Array GraphLink -> Array GraphNode -> Model
makeModel links nodes = { links, nodes }

-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas
datumIsGraphLink :: Datum -> GraphLink
datumIsGraphLink = unsafeCoerce
datumIsGraphNode :: Datum -> GraphNode
datumIsGraphNode = unsafeCoerce


-- drag = 
--   d3Drag "node" simulation {
--       start: dragstarted
--     , drag:  dragged
--     , end:   dragended
--   }

-- attachTickMap :: (Unit -> Unit) -> Simulation -> Unit
-- attachTickMap tick simulation = 

type Scale = Number -> String
foreign import readJSONJS :: String -> Model -- TODO no error handling at all here RN
foreign import d3SchemeCategory10JS :: Scale -- not modelling the scale / domain distinction yet

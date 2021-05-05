module D3.Examples.Simulation.LesMiserables where

import Affjax (Error)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, cx, cy, fill, getWindowWidthHeight, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, x1, x2, y1, y2)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, join)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (D3ForceLink_, D3ForceNode_, Force(..), ForceName(..), ForceType(..), initSimulation, makeCenterForce, startSimulation_)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (D3Selection_, DragBehavior(..), Element(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Prelude (class Bind, Unit, bind, discard, pure, unit, ($))
import Unsafe.Coerce (unsafeCoerce)

-- this is the model used by this particular "chart" (ie force layout simulation)
-- *********************************************************************************************************************
-- NOTA BENE - these types are a _lie_ as stated in that the Nodes / Links are mutable and are changed when you put them
-- into the simulation, the types given here represent their form AFTER D3 has mutated them
-- *********************************************************************************************************************
type NodeExtension = (group :: Number) -- any extra fields beyond what's required of all ForceLayout nodes
type LinkExtension = (value :: Number) -- empty row, this simulation doesn't yet have extra stuff in the links
type GraphNode = D3ForceNode_ Int NodeExtension
type GraphLink = D3ForceLink_ Int NodeExtension LinkExtension
type Model = { links :: Array GraphLink, nodes :: Array GraphNode }

drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  widthHeight <- liftEffect getWindowWidthHeight
  forceJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
  let graph = readGraphFromFileContents forceJSON
  
  -- type qualification necessary here because we're discarding the result of enter
  (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (enter widthHeight graph)
  -- in contrast, string version of interpreter doesn't need qualification here because we use result
  printedScript <- liftEffect $ runPrinter (enter widthHeight graph) "Force Layout Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit

-- TODO no error handling at all here RN (OTOH - performant!!)
foreign import readJSONJS :: String -> Model 

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> Model
readGraphFromFileContents (Right { body } ) = readJSONJS body
readGraphFromFileContents (Left err)        = { links: [], nodes: [] } -- TODO exceptions dodged using empty Model



-- | recipe for this force layout graph
enter :: forall m link node selection r. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  { links :: Array link, nodes :: Array node | r } -> 
  m selection -- TODO is it right to return selection_ instead of simulation_? does it matter? 
enter (Tuple w h) model = do
  root       <- attach "div#force"
  svg        <- root `append` (node Svg   [ viewBox 0.0 0.0 1000.0 1000.0 ] )
  linksGroup <- svg  `append` (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  let forces      = [ makeCenterForce 500.0 500.0
                    , Force (ForceName "charge") ForceMany ]
      simulation_ = initSimulation forces model model.nodes model.links

  links <- join linksGroup $ JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ strokeWidth linkWidth ]
    , simulation: simulation_ -- following config fields are extras for simulation
    , tickName  : "links"
    , onTick    : [ x1 setX1, y1 setY1, x2 setX2, y2 setY2 ]
    , onDrag    : SimulationDrag NoDrag
  }

  nodes <- join nodesGroup $ JoinSimulation {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : model.nodes
    , behaviour : [ radius 5.0, fill colorByGroup ]
    , simulation: simulation_  -- following config fields are extras for simulation
    , tickName  : "nodes"
    , onTick    : [ cx setCx, cy setCy ]
    , onDrag    : SimulationDrag DefaultDrag
  }
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }

  let _ = startSimulation_ simulation_

  pure svg'

-- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas (i think)
datumIsGraphLink :: Datum -> GraphLink
datumIsGraphLink = unsafeCoerce
datumIsGraphNode :: Datum -> GraphNode
datumIsGraphNode = unsafeCoerce

colorByGroup :: Datum -> String
colorByGroup datum = d3SchemeCategory10N_ d.group
  where
    d = datumIsGraphNode datum

linkWidth :: Datum -> Number
linkWidth datum = sqrt d.value
  where
    d = datumIsGraphLink datum

setX1 :: Datum -> Number
setX1 datum = d.source.x
  where
    d = datumIsGraphLink datum
setY1 :: Datum -> Number
setY1 datum = d.source.y
  where
    d = datumIsGraphLink datum
setX2 :: Datum -> Number
setX2 datum = d.target.x
  where
    d = datumIsGraphLink datum
setY2 :: Datum -> Number
setY2 datum = d.target.y
  where
    d = datumIsGraphLink datum
setCx :: Datum -> Number
setCx datum = d.x
  where
    d = datumIsGraphNode datum
setCy :: Datum -> Number
setCy datum = d.y
  where
    d = datumIsGraphNode datum

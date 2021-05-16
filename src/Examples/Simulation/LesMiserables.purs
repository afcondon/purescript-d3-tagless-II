module D3.Examples.Simulation.LesMiserables where

import D3.Data.File.LesMiserables
import D3.Node

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, cx, cy, fill, getWindowWidthHeight, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, Datum_, Element(..))
import D3.FFI (startSimulation_)
import D3.FFI.Config (defaultConfigSimulation, defaultForceLinkConfig, defaultForceManyConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, join)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (Force(..), ForceType(..), initSimulation)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (DragBehavior(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
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

drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  widthHeight <- liftEffect getWindowWidthHeight
  forceJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
  let graph = readGraphFromFileContents forceJSON
  
  -- type qualification necessary here because we're discarding the result of enter
  (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (graphScript widthHeight graph)
  -- in contrast, string version of interpreter doesn't need qualification here because we use result
  printedScript <- liftEffect $ runPrinter (graphScript widthHeight graph) "Force Layout Script"
  log $ snd printedScript
  log $ fst printedScript
  pure unit


-- | recipe for this force layout graph
graphScript :: forall linkdata  m r selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  { links :: Array (D3_Link NodeID linkdata), nodes :: Array LesMisNodeData | r } -> 
  m selection
graphScript (Tuple w h) model = do
  root       <- attach "div#force"
  svg        <- root `append` (node Svg   [ viewBox 0.0 0.0 1000.0 1000.0 ] )
  linksGroup <- svg  `append` (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  let forces =  [ Force $ ForceCenter { name: "center", cx: 500.0, cy: 500.0, strength: 1.0 }
                , Force $ ForceManyBody (defaultForceManyConfig "charge")
                , Force $ ForceLink     (defaultForceLinkConfig "links" model.links (\d i -> d.id))
                ]
      simulation = initSimulation forces model.nodes defaultConfigSimulation

  links <- join linksGroup $ JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links -- NB the links are still just { source :: NodeID, target :: NodeID, value :: Number } at this point
    , behaviour : [ strokeWidth linkWidth ]
    , simulation: simulation.simulation -- following config fields are extras for JoinSimulation

    , tickName  : "links"
    , onTick    : [ x1 getSourceX, y1 getSourceY, x2 getTargetX, y2 getTargetY ]
    , onDrag    : SimulationDrag NoDrag
  }

  nodes <- join nodesGroup $ JoinSimulation {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : simulation.nodes
    , behaviour : [ radius 5.0, fill colorByGroup ]
    , simulation: simulation.simulation  -- following config fields are extras for JoinSimulation

    , tickName  : "nodes"
    , onTick    : [ cx getNodeX, cy getNodeY ]
    , onDrag    : SimulationDrag DefaultDrag
  }
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }

  let _ = startSimulation_ simulation.simulation

  pure svg'

-- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas (i think)

colorByGroup :: Datum_ -> String
colorByGroup datum = d3SchemeCategory10N_ d.group
  where
    -- d = datumIsLesMisGraphNode_ datum
    d = unsafeCoerce datum

linkWidth :: Datum_ -> Number
linkWidth datum = sqrt v
  where
    v = (unsafeCoerce datum).value -- TODO rewrite more specific coercion of type safety cannot be achieved
    -- d = unsafeCoerce $ spy "linkWidth datum" datum


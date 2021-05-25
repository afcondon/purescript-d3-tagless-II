module D3.Examples.LesMiserables where

import Utility

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, cx, cy, fill, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Selection_, Datum_, Element(..))
import D3.Examples.LesMiserables.File (LesMisModel, readGraphFromFileContents)
import D3.FFI (D3ForceHandle_, configSimulation_, getLinks_, initSimulation_, putForcesInSimulation_, setLinks_, setNodes_, startSimulation_, stopSimulation_)
import D3.FFI.Config (defaultConfigSimulation, defaultForceLinkConfig, defaultForceLinkConfigEmpty, defaultForceManyConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, join, on)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (Force(..), createForce)
import D3.Node (getNodeX, getNodeY, getSourceX, getSourceY, getTargetX, getTargetY)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (cons)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, (/), ($), (<$>))
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

lesMisForces :: Array D3ForceHandle_
lesMisForces = createForce <$>
    [ ForceCenter { name: "center", cx: 500.0, cy: 500.0, strength: 1.0 }
    , ForceManyBody (defaultForceManyConfig "charge")
    ]

-- | recipe for this force layout graph
graphScript :: forall  m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  LesMisModel -> 
  m selection
graphScript widthheight model = do
  let columns = 3.0
      width   = (fst widthheight) / columns
      height  = (snd widthheight) / columns
  root       <- attach "div#force"
  svg        <- root `append` (node Svg    [ viewBox (-width / 2.0) (-height / 2.0) width height ] )
  centerDot  <- svg  `append` (node Circle [ radius 20.0, fill "red", x (width / 2.0), y (height / 2.0) ])
  linksGroup <- svg  `append` (node Group  [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group  [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` model.nodes 
      _          = simulation `putForcesInSimulation_` lesMisForces
      _          = setLinks_ simulation model.links (\d i -> d.id)

  linksSelection <- join linksGroup $ Join {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links -- NB the links are still just { source :: NodeID, target :: NodeID, value :: Number } at this point
    , behaviour : [ strokeWidth linkWidth ]
  }

  nodesSelection <- join nodesGroup $ Join {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ radius 5.0, fill colorByGroup ]
  }
  _ <- linksSelection `on` Tick { name: "links", simulation, chain: [ x1 getSourceX, y1 getSourceY, x2 getTargetX, y2 getTargetY ]}
  _ <- nodesSelection `on` Tick { name: "nodes", simulation, chain: [ cx getNodeX, cy getNodeY  ]}
  _ <- nodesSelection `on` Drag DefaultDrag

  _ <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: height, right: width }
                          , scale     : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                          , qualifier : "tree"
                          , target    : SelfTarget
                          }

  pure svg

-- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas (i think)

colorByGroup :: Datum_ -> String
colorByGroup datum = d3SchemeCategory10N_ d.group
  where
    d = unsafeCoerce datum

linkWidth :: Datum_ -> Number
linkWidth datum = sqrt v
  where
    v = (unsafeCoerce datum).value -- TODO rewrite more specific coercion if type safety cannot be achieved


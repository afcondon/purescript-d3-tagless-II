module D3.Examples.Force where

import D3.Layouts.Simulation

import Affjax (Error)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum)
import D3.Attributes.Sugar (classed, cursor, cx, cy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, width, x1, x2, y1, y2)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, attach, join, runD3M)
import D3.Scales (d3SchemeCategory10_)
import D3.Selection (D3Selection_, D3Simulation_, DragBehavior(..), Element(..), Join(..), Keys(..), ScaleExtent(..), SelectionName(..), ZoomExtent(..), attachZoom, makeProjection, node)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Prelude (class Bind, Unit, bind, discard, pure, unit, ($), (*>))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win    <- window
  width  <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

-- TODO no error handling at all here RN (OTOH - performant!!)
foreign import readJSONJS :: String -> Model 

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> Model
readGraphFromFileContents (Right { body } ) = readJSONJS body
readGraphFromFileContents (Left err)        = { links: [], nodes: [] }


-- this is the model used by this particular "chart" (ie force layout simulation)
type NodeExtension = (group :: Number) -- any extra fields beyond what's required of all ForceLayout nodes
type LinkExtension = (value :: Number) -- empty row, this simulation doesn't yet have extra stuff in the links
type GraphNode = D3ForceNode_ NodeExtension
type GraphLink = D3ForceLink_ NodeExtension LinkExtension
type Model = { links :: Array GraphLink, nodes :: Array GraphNode }

drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  widthHeight <- liftEffect getWindowWidthHeight
  forceJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
  let graph = readGraphFromFileContents forceJSON
  
  liftEffect $ runD3M (enter widthHeight) graph
    *> pure unit

-- | recipe for this force layout graph
enter :: forall m link node r. 
  Bind m => 
  D3Tagless m => 
  MonadState { links :: Array link, nodes :: Array node | r } m => 
  Tuple Number Number -> m D3Selection_ -- going to actually be a simulation right? 
enter (Tuple w h) = do
  model      <- get -- this gets the input model but actually we're going to act on a transformed version of that
  root       <- attach "div#force"
  svg        <- root `appendTo` (node Svg   [ width w, height h, viewBox 0.0 0.0 w h ] )
  linksGroup <- svg  `appendTo` (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `appendTo` (node Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  let forces      = [ makeCenterForce w h
                    , Force (ForceName "charge") ForceMany ]
      simulation_ = initSimulation forces model model.nodes model.links

  links <- join linksGroup $ JoinSimulation {
      element   : Line
    , key       : DatumIsUnique
    , "data"    : model.links
    , behaviour : [ strokeWidth linkWidth ]
    , simulation: simulation_ -- following config fields are extras for simulation
    , tickName  : "links"
    , onTick    : [ x1 setX1, y1 setY1, x2 setX2, y2 setY2 ]
    , onDrag    : NoDrag
  }

  nodes <- join nodesGroup $ JoinSimulation {
      element   : Circle
    , key       : DatumIsUnique
    , "data"    : model.nodes
    , behaviour : [ radius 5.0, fill colorByGroup ]
    , simulation: simulation_  -- following config fields are extras for simulation
    , tickName  : "nodes"
    , onTick    : [ cx setCx, cy setCy ]
    , onDrag    : DefaultDrag
  }
  
  let _ = svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 1 8 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            }

  let _ = startSimulation_ simulation_

  pure svg

-- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas (i think)
datumIsGraphLink :: Datum -> GraphLink
datumIsGraphLink = unsafeCoerce
datumIsGraphNode :: Datum -> GraphNode
datumIsGraphNode = unsafeCoerce

colorByGroup :: Datum -> String
colorByGroup datum = d3SchemeCategory10_ d.group
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

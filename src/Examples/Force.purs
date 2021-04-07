module D3.Examples.Force where

import D3.Attributes.Sugar (classed, cx, cy, fill, height, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, width, x1, x2, y1, y2)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, ($), (*>), (/), (<$>))

import Affjax (Error)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, applyChainable, hook, join, runD3M)
import D3.Layouts.Simulation (D3ForceLink_, D3ForceNode_, DragBehavior(..), Force(..), ForceName(..), ForceType(..), defaultConfigSimulation, initSimulation_, putForcesInSimulation, setLinks_, startSimulation_)
import D3.Selection (Chainable, D3Selection_, D3Simulation_, D3State(..), Element(..), Join(..), Keys(..), SelectionName(..), makeD3State', makeProjection, node)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  width <- innerWidth win
  height <- innerHeight win
  pure $ Tuple (toNumber width) (toNumber height)

foreign import readJSONJS :: String -> Model -- TODO no error handling at all here RN

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
  let graph      = readGraphFromFileContents forceJSON
  
  -- liftEffect $ runD3M (enter widthHeight) (makeD3State' { links: graph.links, nodes: graph.nodes }) *> pure unit
  liftEffect $ runD3M (enter widthHeight) (makeD3State' graph) *> pure unit

  pure unit

-- | recipe for this force layout graph
enter :: forall m. Bind m => D3Tagless m => MonadState (D3State Model) m => Tuple Number Number -> m D3Selection_ -- going to actually be a simulation right? 
enter (Tuple w h) = do
  root  <- hook "div#force"
  let cx = negate $ w / 2.0
      cy = negate $ h / 2.0
  svg   <- appendTo root "svg-tree" (node Svg [ width w, height h, viewBox cx cy w h ] )
  links_ <- appendTo svg "links-group" (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodes_ <- appendTo svg "nodes-group" (node Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  (D3State state) <- get
  let simulation = initSimulation state.model (\model -> model.nodes) (\model -> model.links)

  maybeLinks_ <- join state.model $ JoinSimulation {
      element   : Line
    , key       : DatumIsKey
    , hook      : SelectionName "links-group"
    , projection: makeProjection (\model -> model.links)
    , behaviour : [ strokeWidth linkWidth ]
    -- extras for simulation elements
    , simulation
    , onTick    : (\_ -> do
                    let _ = (applyChainable links_) <$> linkTick
                        _ = spy "Tick function: " simulation
                    unit)
  }

  maybeNodes_ <- join state.model $ JoinSimulation {
      element   : Circle
    , key       : DatumIsKey
    , hook      : SelectionName "nodes-group"
    , projection: makeProjection (\model -> model.nodes)
    , behaviour : [ radius 5.0, fill colorByGroup ]
    -- extras for simulation elements
    , simulation
    , onTick    : (\_ -> do
                    let _ = (applyChainable nodes_) <$> nodeTick
                        _ = spy "Tick function: " simulation
                    unit)
  }
          
  let _ = startSimulation_ simulation

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
colorByGroup datum = d3SchemeCategory10JS d.group
  where
    d = datumIsGraphNode datum

linkWidth :: Datum -> Number
linkWidth datum = sqrt d.value
  where
    d = datumIsGraphLink datum

linkTick :: Array Chainable
linkTick = 
  [ x1 setX1
  , y1 setY1
  , x2 setX2
  , y2 setY2 ]

nodeTick :: Array Chainable
nodeTick =
  [ cx setCx
  , cy setCy ]

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
setY2 datum = d.target.x
  where
    d = datumIsGraphLink datum
setCx :: Datum -> Number
setCx datum = d.x
  where
    d = datumIsGraphNode datum
setCy :: Datum -> Number
setCy datum = d.x
  where
    d = datumIsGraphNode datum

-- | definition of the particular Simulation that we are going to run
initSimulation :: forall model node link. model -> (model -> node) -> (model -> link) -> D3Simulation_
initSimulation model nodeProjection linkProjection = do
  let nodes_     = unsafeCoerce $ nodeProjection model
      links_     = unsafeCoerce $ linkProjection model
      simulation = initSimulation_ nodes_ defaultConfigSimulation
      _ = simulation `setLinks_` links_
      -- _ = simulation `putForcesInSimulation` [ Force (ForceName "charge") ForceMany, centerForce 800.0 900.0 ]
      _ = simulation `putForcesInSimulation` [ centerForce 800.0 900.0 ]
  simulation

centerForce :: Number -> Number -> Force
centerForce width height = Force (ForceName "center") $ ForceCenter (width / 2.0) (height / 2.0)

-- | utility functions and boilerplate
myDrag :: DragBehavior
myDrag = DefaultDrag "node" "simulation"

makeModel :: Array GraphLink -> Array GraphNode -> Model
makeModel links nodes = { links, nodes }


-- TODO next lines belong in dedicated Scales module
type Scale = Number -> String 
foreign import d3SchemeCategory10JS :: Scale -- not modelling the scale / domain distinction yet

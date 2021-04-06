module D3.Examples.Force where

import D3.Attributes.Sugar
import Prelude

import Affjax (Error)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Datum)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, applyChainable, hook, join, runD3M)
import D3.Layouts.Simulation (D3ForceLink_, D3ForceNode_, D3Simulation_, DragBehavior(..), Force(..), ForceName(..), ForceType(..), defaultConfigSimulation, initSimulation_, onTick_, putForcesInSimulation, setLinks_, setNodes_, startSimulation_)
import D3.Selection (Chainable, D3Selection, D3Selection_, D3State(..), Element(..), Join(..), Keys(..), SelectionName(..), enterOnly, makeD3State', makeProjection, node)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
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

  maybeLinks_ <- join state.model $ Join {
      element   : Line
    , key       : DatumIsKey
    , hook      : SelectionName "links-group"
    , projection: makeProjection (\model -> model.links)
    , behaviour : enterOnly [ strokeWidth linkWidth ]
    -- would like to have the tick function for this selection in here
  }

  maybeNodes_ <- join state.model $ Join {
      element   : Circle
    , key       : DatumIsKey
    , hook      : SelectionName "nodes-group"
    , projection: makeProjection (\model -> model.nodes)
    , behaviour : enterOnly [ radius 5.0, fill colorByGroup ]
    -- would like to have the tick function for this selection in here
  }

  let _ = case maybeLinks_, maybeNodes_ of
            (Just links_), (Just nodes_) -> 
              s `onTick_` (\sn -> do
                              let _ = (applyChainable links_) <$> linkTick
                                  _ = (applyChainable nodes_) <$> nodeTick
                                  _ = spy "Tick function: " sn
                              sn)
            _, _ -> s -- TODO let's really make an effort to make this work by extending Join to JoinSim
          
      _ = startSimulation_ s

  pure svg

linkTick :: Array Chainable
linkTick = 
  [ x1 (\d -> (datumIsGraphLink d).source.x)
  , y1 (\d -> (datumIsGraphLink d).source.y)
  , x2 (\d -> (datumIsGraphLink d).target.x)
  , y2 (\d -> (datumIsGraphLink d).target.y) ]

nodeTick :: Array Chainable
nodeTick =
  [ cx (\d -> (datumIsGraphNode d).x)
  , cy (\d -> (datumIsGraphNode d).y) ]

-- | definition of the particular Simulation that we are going to run
-- initSimulation :: forall model. model -> (model -> D3Data_) -> (model -> D3Data_) -> D3Simulation_
initSimulation :: forall t61 t70 t75. t61 -> (t61 -> t70) -> (t61 -> t75) -> D3Simulation_
initSimulation model nodeProjection linkProjection = do
  let nodes      =  nodeProjection model
      links      =  linkProjection model
      simulation = initSimulation_ defaultConfigSimulation
      -- the projection functions for the join are not sufficient for the simulation
      -- of course, the makeProjection is an unsafeCoerce anyway so it's a moot point
      -- TODO revisit whole area when functionally complete and try for a type-safe expression of both
      _          = simulation `setNodes_` (unsafeCoerce nodes)
      _          = simulation `setLinks_` (unsafeCoerce links)
      _          = simulation `putForcesInSimulation` [ Force (ForceName "charge") ForceMany, centerForce 800.0 900.0 ]
  simulation

centerForce :: Number -> Number -> Force
centerForce width height = Force (ForceName "center") $ ForceCenter (width / 2.0) (height / 2.0)

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

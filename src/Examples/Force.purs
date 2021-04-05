module D3.Examples.Force where

import D3.Attributes.Sugar
import D3.Layouts.Tree

import Affjax (Error, printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Attribute(..), Datum, toAttr)
import D3.Interpreter.Tagless (class D3Tagless, appendTo, hook, join, runD3M)
import D3.Selection (Chainable(..), D3Data_, D3Selection_, D3State(..), Element(..), Join(..), Keys(..), SelectionName(..), EnterUpdateExit, makeD3State', makeProjection, node)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (pi, sqrt)
import Prelude (class Bind, Unit, bind, discard, negate, pure, show, unit, ($), (*), (*>), (-), (/), (<), (<>), (==), (>=))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

type Scale = Number -> String
foreign import readJSONJS :: String -> Model -- TODO no error handling at all here RN

readModelFromFileContents :: forall r. Either Error { body ∷ String | r } -> Model
readModelFromFileContents (Right { body } ) = readJSONJS body
readModelFromFileContents (Left err)        = { links: [], nodes: [] }

foreign import d3SchemeCategory10JS :: Scale -- not modelling the scale / domain distinction yet

-- this is the model used by this particular "chart" (ie force layout simulation)
type Model = { links :: Array GraphLink, nodes :: Array GraphNode }

type GraphNode = SimulationNodeRow { group :: Number }
type GraphLink = { id :: ID, source :: ID, target :: ID, value :: Number }

-- | express the additions that D3 makes in terms of rows for clarity and DRY
-- after the GraphLink type has been bound in D3 it is changed to the following
type D3GraphLink = { id :: ID, source :: GraphNode, target :: GraphNode, value :: Number }

colorByGroup :: Datum -> String
colorByGroup datum = d3SchemeCategory10JS d.group
  where
    d = unsafeCoerce datum

-- | recipe for this force layout graph
enter :: forall m. Bind m => D3Tagless m => MonadState (D3State Model) m => m D3Selection_
enter = do
  root <- hook "div#force"
  svg  <- appendTo root "svg-tree" (node Svg [ viewBox 0.0 0.0 width height ] )
  links <- appendTo svg "links-group" (node Group [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodes <- appendTo svg "nodes-group" (node Group [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  (D3State state) <- get

  linkJoinSelection_ <- join state.model $ Join {
      element   : Line
    , key       : DatumIsKey
    , hook      : SelectionName "links-group"
    , projection: makeProjection (\model -> model.links)
    , behaviour : { enter: [ strokeWidth (\d -> sqrt d.value) ], update: [], exit: [] }
  }

  nodeJoinSelection <- join state.model $ Join {
      element   : Circle
    , key       : DatumIsKey
    , hook      : SelectionName "nodes-group"
    , projection: makeProjection (\model -> model.nodes)
    , behaviour : { enter: [ radius 5.0, fill colorByGroup ]}
  }
  pure svg
  

-- | definition of the particular Simulation that we are going to run
simulationConfig :: Simulation
simulationConfig =
  Simulation { 
      label: "simulation" -- TODO stringy label
    , config: defaultConfigSimulation
    , forces: [ Force "charge" ForceMany, centerForce 800.0 900.0 ] 
    , nodes: []
    , links: []
    , tick: identity
    , drag: const unit
  }

centerForce :: Number -> Number -> Force
centerForce width height = Force "center" $ ForceCenter (width / 2.0) (height / 2.0)

-- | function to build the tick function, quite tricky
myTickMap :: TickMap Model
myTickMap = fromFoldable
  [ Tuple "link" [ NumberAttr "x1" (\d -> (unsafeCoerce d).source.x)
                 , NumberAttr "y1" (\d -> (unsafeCoerce d).source.y)
                 , NumberAttr "x2" (\d -> (unsafeCoerce d).target.x)
                 , NumberAttr "y2" (\d -> (unsafeCoerce d).target.y) ]
  , Tuple "node" [ NumberAttr "cx" (\d -> (unsafeCoerce d).x)
                 , NumberAttr "cy" (\d -> (unsafeCoerce d).y) ]
  ]

-- | utility functions and boilerplate
myDrag :: DragBehavior
myDrag = DefaultDrag "node" "simulation"

makeModel :: Array GraphLink -> Array GraphNode -> Model
makeModel links nodes = { links, nodes }

-- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- which projection of the "Model" is active in each Join varies so we can't have both strong
-- static type representations AND lightweight syntax with JS compatible lambdas
datumIsGraphLink :: Datum -> D3GraphLink
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


module D3.Examples.LesMiserables where

import D3.Examples.LesMiserables.Types

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, cx, cy, fill, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, Datum_, Element(..))
import D3.Examples.LesMis.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.Examples.LesMiserables.File (readGraphFromFileContents)
import D3.FFI (configSimulation_, initSimulation_, setLinks_, setNodes_)
import D3.Interpreter (class D3InterpreterM, append, attach, join, on)
import D3.Interpreter.D3 (runD3M)
import D3.Interpreter.String (runPrinter)
import D3.Layouts.Simulation (Force(..), ForceType(..), putEachForceInSimulation)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (defaultConfigSimulation)
import D3.Simulation.Config as F
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Int (toNumber)
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, (<<<), ($), (/))
import Utility (getWindowWidthHeight)

link_ :: { source :: Datum_
            -> { fx :: Nullable Number
               , fy :: Nullable Number
               , group :: Int
               , index :: Int
               , vx :: Number
               , vy :: Number
               , x :: Number
               , y :: Number
               }
, target :: Datum_
            -> { fx :: Nullable Number
               , fy :: Nullable Number
               , group :: Int
               , index :: Int
               , vx :: Number
               , vy :: Number
               , x :: Number
               , y :: Number
               }
, value :: Datum_ -> Number
}
link_ = {
    source: (\d -> (unboxD3SimLink d).source)
  , target: (\d -> (unboxD3SimLink d).target)
  , value:  (\d -> (unboxD3SimLink d).value)
}

datum_ :: { colorByGroup :: Datum_ -> String
, group :: Datum_ -> Int
, index :: Datum_ -> Int
, x :: Datum_ -> Number
, y :: Datum_ -> Number
}
datum_ = {
-- direct accessors to fields of the datum (BOILERPLATE)
    index : (\d -> (unboxD3SimNode d).index)
  , x     : (\d -> (unboxD3SimNode d).x)
  , y     : (\d -> (unboxD3SimNode d).y)
  , group : (\d -> (unboxD3SimNode d).group)

  , colorByGroup:
      (\d -> d3SchemeCategory10N_ (toNumber $ datum_.group d))
}

drawGraph :: Aff Unit
drawGraph = do
  log "Force layout example"
  widthHeight <- liftEffect getWindowWidthHeight
  forceJSON   <- AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
  let graph = readGraphFromFileContents forceJSON
  
  (_ :: Tuple D3Selection_ Unit) <- liftEffect $ runD3M (graphScript widthHeight graph)

  (Tuple selection result) <- liftEffect $ runPrinter (graphScript widthHeight graph) "Force Layout Script"
  log $ result
  log $ selection
  pure unit

lesMisForces :: Array Force
lesMisForces = 
    [ Force "center" ForceCenter  [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
    , Force "charge" ForceManyBody  []
    ]

-- | recipe for this force layout graph
graphScript :: forall  m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  LesMisRawModel -> 
  m selection
graphScript widthheight model = do
  let columns = 3.0
      rows    = 2.0
      width   = (fst widthheight) / columns
      height  = (snd widthheight) / rows
  root       <- attach "div#force"
  svg        <- root `append` (node Svg    [ viewBox (-width / 2.0) (-height / 2.0) width height ] )
  linksGroup <- svg  `append` (node Group  [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `append` (node Group  [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` model.nodes 
      _          = simulation `putEachForceInSimulation` lesMisForces
      _          = setLinks_ simulation model.links (\d i -> d.id)

  linksSelection <- join linksGroup $ Join {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links -- NB the links are still just { source :: NodeID, target :: NodeID, value :: Number } at this point
    , behaviour : [ strokeWidth (sqrt <<< link_.value) ]
  }
  nodesSelection <- join nodesGroup $ Join {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ radius 5.0, fill datum_.colorByGroup ]
  }

  _ <- linksSelection `on` Tick { name: "links", simulation, chain: [ x1 (_.x <<< link_.source)
                                                                    , y1 (_.y <<< link_.source)
                                                                    , x2 (_.x <<< link_.target)
                                                                    , y2 (_.y <<< link_.target)
                                                                    ]}
  _ <- nodesSelection `on` Tick { name: "nodes", simulation, chain: [ cx datum_.x, cy datum_.y  ]}
  _ <- nodesSelection `on` Drag DefaultDrag

  _ <- svg `on`  Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: height, right: width }
                      , scale     : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                      , name : "LesMis"
                      }
  pure svg

module D3.Examples.LesMiserables where

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import D3.Attributes.Sugar (classed, cx, cy, fill, radius, strokeColor, strokeOpacity, strokeWidth, viewBox, x1, x2, y1, y2)
import D3.Data.Types (D3Selection_, Datum_, Element(..), Selector)
import D3.Examples.LesMis.Unsafe (unboxD3SimLink, unboxD3SimNode)
import D3.Examples.LesMiserables.File (readGraphFromFileContents)
import D3.Examples.LesMiserables.Types (LesMisRawModel)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce)
import D3.Simulation.Types (Force, ForceType(..), SimulationState_, Step(..), initialSimulationState)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, addTickFunction, appendElement, attach, join, on, setLinks, setNodes)
import D3Tagless.Instance.Simulation (run_D3M_Simulation)
import Data.Int (toNumber)
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Math (sqrt)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, ($), (/), (<<<))
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

-- | recipe for this force layout graph
graphScript :: forall  m. 
  Bind m => 
  MonadEffect m =>
  SimulationM D3Selection_ m =>
  LesMisRawModel -> Selector D3Selection_ -> m Unit
graphScript model selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  (root :: D3Selection_) <- attach selector
  svg        <- root `appendElement` (node Svg [ viewBox (-w / 2.0) (-h / 2.0) w h
                                               , classed "lesmis" ] )
  linksGroup <- svg  `appendElement` (node Group  [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  `appendElement` (node Group  [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])
  
  simulationNodes <- setNodes model.nodes 
  simulationLinks <- setLinks model.links
  
  linksSelection <- join linksGroup $ Join {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : simulationLinks
    , behaviour : [ strokeWidth (sqrt <<< link_.value) ]
  }
  nodesSelection <- join nodesGroup $ Join {
      element   : Circle
    , key       : UseDatumAsKey
    , "data"    : simulationNodes
    , behaviour : [ radius 5.0, fill datum_.colorByGroup ]
  }

  addTickFunction "nodes" $ Step nodesSelection [ cx datum_.x, cy datum_.y  ]
  addTickFunction "links" $ Step linksSelection [ x1 (_.x <<< link_.source)
                                                , y1 (_.y <<< link_.source)
                                                , x2 (_.x <<< link_.target)
                                                , y2 (_.y <<< link_.target)
                                                ]
  -- _ <- nodesSelection `on` Drag DefaultDrag

  _ <- svg `on`  Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                      , scale     : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                      , name : "LesMis"
                      }
  pure unit -- svg

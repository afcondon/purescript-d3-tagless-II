module D3.Examples.Spago.Draw where

import Control.Monad.State (class MonadState)
import D3.Attributes.Sugar (classed, cursor, fill, height, onMouseEvent, radius, remove, strokeColor, text, textAnchor, transform', viewBox, width, x, x1, x2, y, y1, y2)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Selection_, D3Simulation_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Files (SpagoDataRow, SpagoGraphLinkID, SpagoLinkData)
import D3.Examples.Spago.Model (SpagoSimNode, cancelSpotlight_, datum_, link_, toggleSpotlight, tree_datum_)
import D3.Examples.Spago.Unsafe (spagoNodeKeyFunction)
import D3.FFI (D3ForceHandle_, keyIsID, setLinks_, setNodes_)
import D3.Layouts.Hierarchical (horizontalLink', radialLink, verticalLink)
import D3.Node (D3Link, D3_SimulationNode, NodeID)
import D3.Selection (Behavior(..), ChainableS, DragBehavior(..), Join(..), node, node_)
import D3.Simulation.Forces (getLinkForceHandle)
import D3.Simulation.Types (D3SimulationState_, Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, SimDataCooked, SimDataRaw, addTickFunction, attach, loadSimData, modifySelection, on, simulationHandle)
import D3Tagless.Capabilities as D3
import Data.Lens (modifying)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, discard, negate, pure, unit, ($), (/), (<<<))
import Stories.Spago.Lenses (_activeForces, _class)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- TODO this is a problem once extracted from "script", leads to undefined in D3.js
enterLinks :: forall t339. Array t339
enterLinks = [] -- [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"

enterAttrs :: D3Simulation_ -> Array ChainableS
enterAttrs simulation_ = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  , onMouseEvent MouseClick (\e d _ -> toggleSpotlight e simulation_ d)
  ]

updateAttrs :: forall t1. t1 -> Array ChainableS
updateAttrs _ = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

-- | Some examples of pre-packaged attribute sets available to the app maker
circleAttrs1 :: Array ChainableS
circleAttrs1 = [ 
    radius datum_.radius
  , fill datum_.colorByGroup
]

circleAttrs2 :: Array ChainableS
circleAttrs2 = [
    radius 3.0
  , fill datum_.colorByUsage
]

labelsAttrs1 :: Array ChainableS
labelsAttrs1 = [ 
    classed "label"
  , x 0.2
  , y datum_.positionLabel
  , textAnchor "middle"
  , text datum_.indexAndID
  -- , text datum_.name
]

-- TODO x and y position for label would also depend on "hasChildren", need to get "tree" data into nodes
labelsAttrsH :: Array ChainableS
labelsAttrsH = [ 
    classed "label"
  , x 4.0
  , y 2.0
  , textAnchor (tree_datum_.textAnchor Horizontal)
  , text datum_.name
]

graphAttrs :: { circle :: Array ChainableS , labels :: Array ChainableS }
graphAttrs = { 
    circle: circleAttrs1
  , labels: labelsAttrs1 
}

treeAttrs :: { circle :: Array ChainableS, labels :: Array ChainableS }
treeAttrs  = {
    circle: circleAttrs2
  , labels: labelsAttrsH
}

-- | recipe for this force layout graph
setup :: forall m selection row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: D3SimulationState_, svgClass :: String | row } m =>
  SelectionM selection m =>
  SimulationM selection m =>
  m selection
setup = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  simulation_ <- simulationHandle -- needed for click handler to stop / start simulation
  root        <- attach "div.svg-container"
  svg         <- root D3.+ (node Svg  [ viewBox (-w / 2.1) (-h / 2.05) w h 
                                      -- , preserveAspectRatio $ AspectRatio XMid YMid Meet 
                                      , classed "overlay"
                                      , width w, height h
                                      , cursor "grab"
                                      , onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation_) ] )
  inner    <- svg D3.+ (node_ Group)
  _        <- inner `on` Drag DefaultDrag
  -- because the zoom event is picked up by `svg` but applied to `inner` has to come after creation of `inner`
  _        <- svg `on` Zoom  {  extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                              , scale  : ScaleExtent 0.1 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                              , name   : "spago"
                              , target : inner
                              }

  linksGroup  <- inner  D3.+ (node Group [ classed "links" ])
  nodesGroup  <- inner  D3.+ (node Group [ classed "nodes" ])
  nodes <- nodesGroup D3.<+> PreJoin "g.node"
  links <- linksGroup D3.<+> PreJoin "line.link"
  
  modifying _class toUpper
  pure root

coerceLinks :: forall id r d. Array (D3Link id r) -> Array (D3Link (D3_SimulationNode d) r) 
coerceLinks links = unsafeCoerce links


updateSimulation :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Maybe (SimDataRaw D3Selection_ SpagoDataRow SpagoLinkData NodeID) ->
  { circle :: Array ChainableS, labels :: Array ChainableS } -> 
  m Unit
updateSimulation Nothing _ = pure unit
updateSimulation (Just simdata) attrs = do
  simulation_ <- simulationHandle
  simData     <- loadSimData simdata

  -- first the nodes
  let joinNodes = UpdateJoin Group simData.data.nodes keyIsID
                  { enter : enterAttrs simulation_
                  , update: updateAttrs simulation_
                  , exit  : [ remove ] 
                  }
                  
  nodesSelection <- simData.selections.nodes D3.<+> joinNodes
  
  -- now the links
  let joinLinks = UpdateJoin Line simData.data.links keyIsID
                    { enter : [ classed link_.linkClass, strokeColor link_.color ]
                    , update: [ classed "graphlinkSimUpdate" ]
                    , exit  : [ remove ]
                    }
  linksSelection <- simData.selections.links D3.<+> joinLinks

  circle <- nodesSelection D3.+ (node Circle attrs.circle)
  _      <- circle `on` Drag DefaultDrag -- TODO needs to ACTUALLY drag the parent transform, not this circle
  labels <- nodesSelection D3.+ (node Text attrs.labels) 
  
  addTickFunction "nodes" $
    Step nodesSelection [ transform' datum_.translateNode ]
  addTickFunction "links" $
    Step linksSelection [ x1 (_.x <<< link_.source), y1 (_.y <<< link_.source), x2 (_.x <<< link_.target), y2 (_.y <<< link_.target) ]
  
  -- modifying { nodes: nodesSelection, links: linksSelection } -- return the selections
  pure unit


{-
updateGraphLinks :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  m Unit
updateGraphLinks links = do
  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"
    
  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      -- TODO the links need valid IDs too if they are to do general update pattern, probably best to actually make them when making the model
      linksSelection <- linksGroup D3.<+> 
                        UpdateJoinWithKeyFunction
                        Line
                        links
                        { enter: [ classed link_.linkClass, strokeColor link_.color ]
                        , update: [ classed "graphlinkSimUpdate" ]
                        , exit: [ remove ] }
                        spagoLinkKeyFunction_

      addTickFunction "links" $ Step linksSelection linkTick
      addSelection "graphlinksSelection" linksSelection
      _ <- setLinks links datum_.indexFunction -- NB this is the model-defined way of getting the index function for the NodeID -> object reference swizzling that D3 does when you set the links
      pure unit

  pure unit
  
updateGraphLinks' :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  m Unit
updateGraphLinks' links = do
  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"
    
  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      linksSelection <- linksGroup D3.<+> 
                        UpdateJoinWithKeyFunction
                        Line
                        links
                        { enter: [ classed link_.linkClass, strokeColor link_.color ]
                        , update: [ classed "graphlinkUpdate" ]
                        , exit: [ remove ] }
                        spagoLinkKeyFunction_

      addTickFunction "links" $ Step linksSelection linkTick
      addSelection "graphlinksSelection" linksSelection

  pure unit
  
updateTreeLinks :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulationState :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  TreeLayout -> 
  m Unit
updateTreeLinks links layout = do
  linksInSimulation <- setLinks links datum_.indexFunction

  let linkPath =
        case layout of
          Horizontal -> horizontalLink' -- the ' is because current library default horizontalLink flips x&y (tree examples written that way, should be changed)
          Radial     -> radialLink datum_.x datum_.y
          Vertical   -> verticalLink

  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"
  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      linksSelection <- linksGroup D3.<+> 
                        UpdateJoinWithKeyFunction
                        Path
                        linksInSimulation
                        { enter: [ classed link_.linkClass, strokeColor link_.color, linkPath ]
                        , update: [ classed "treelinkUpdate" ]
                        , exit: [ remove ] }
                        spagoLinkKeyFunction_
                        
      addTickFunction "links" $ Step linksSelection linkTick
      addSelection "treelinksSelection" linksSelection

  pure unit
-}
  
-- removeNamedSelection :: forall m row. 
--   Bind m => 
--   MonadEffect m =>
--   MonadState { simulationState :: D3SimulationState_ | row } m =>
--   SelectionM D3Selection_ m =>
--   SimulationM D3Selection_ m =>
--   String -> 
--   m Unit
-- removeNamedSelection name = do
--   (maybeSelection :: Maybe D3Selection_) <- getSelection name
--   case maybeSelection of
--     Nothing -> pure unit
--     (Just selection) -> do
--       modifySelection selection [ remove ]

--   pure unit
  
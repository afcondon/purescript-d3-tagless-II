module D3.Examples.Spago.Draw where

import Control.Monad.State (class MonadState)
import D3.Attributes.Sugar (classed, cursor, fill, height, onMouseEvent, radius, remove, strokeColor, text, textAnchor, transform', viewBox, width, x, x1, x2, y, y1, y2)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Selection_, D3Simulation_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Model (cancelSpotlight_, datum_, link_, toggleSpotlight, tree_datum_)
import D3.FFI (keyIsID_)
import D3.Selection (Behavior(..), DragBehavior(..), SelectionAttribute)
import D3.Simulation.Types (Step(..))
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import D3Tagless.Capabilities (class SelectionM, class SimulationM, Staging, addTickFunction, appendTo, attach, carryOverSimState, getLinks, getNodes, mergeSelections, on, openSelection, setAttributes, setNodes, simulationHandle, updateJoin)
import Data.Lens (modifying)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Bind, Unit, bind, const, discard, negate, pure, unit, ($), (/), (<<<))
import Stories.Spago.State (State) as Spago
import Stories.Spago.State (_enterselections, _links, _nodes, _staging)
import Utility (getWindowWidthHeight)

-- TODO this is a problem once extracted from "script", leads to undefined in D3.js
enterLinks :: forall t339. Array t339
enterLinks = [] -- [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"

enterAttrs :: D3Simulation_ -> Array SelectionAttribute
enterAttrs simulation_ = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  , onMouseEvent MouseClick (\e d _ -> toggleSpotlight e simulation_ d)
  ]

updateAttrs :: forall t1. t1 -> Array SelectionAttribute
updateAttrs _ = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

-- | Some examples of pre-packaged attribute sets available to the app maker
circleAttrs1 :: Array SelectionAttribute
circleAttrs1 = [ 
    radius datum_.radius
  , fill datum_.colorByGroup
]

circleAttrs2 :: Array SelectionAttribute
circleAttrs2 = [
    radius 3.0
  , fill datum_.colorByUsage
]

labelsAttrs1 :: Array SelectionAttribute
labelsAttrs1 = [ 
    classed "label"
  , x 0.2
  , y datum_.positionLabel
  , textAnchor "middle"
  , text datum_.indexAndID
  -- , text datum_.name
]

-- TODO x and y position for label would also depend on "hasChildren", need to get "tree" data into nodes
labelsAttrsH :: Array SelectionAttribute
labelsAttrsH = [ 
    classed "label"
  , x 4.0
  , y 2.0
  , textAnchor (tree_datum_.textAnchor Horizontal)
  , text datum_.name
]

graphSceneAttributes :: { circle :: Array SelectionAttribute , labels :: Array SelectionAttribute }
graphSceneAttributes = { 
    circle: circleAttrs1
  , labels: labelsAttrs1 
}

treeSceneAttributes :: { circle :: Array SelectionAttribute, labels :: Array SelectionAttribute }
treeSceneAttributes  = {
    circle: circleAttrs2
  , labels: labelsAttrsH
}

svgAttrs :: D3Simulation_ -> Number -> Number -> Array SelectionAttribute
svgAttrs sim w h = [ viewBox (-w / 2.1) (-h / 2.05) w h 
                    -- , preserveAspectRatio $ AspectRatio XMid YMid Meet 
                    , classed "overlay"
                    , width w, height h
                    , cursor "grab"
                    , onMouseEvent MouseClick (\e d t -> cancelSpotlight_ sim) ]

-- | recipe for this force layout graph
initialize :: forall m.
  Bind m => MonadEffect m => SimulationM D3Selection_ m => SelectionM D3Selection_ m => MonadState Spago.State m => m Unit
initialize = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  sim <- simulationHandle -- needed for click handler to stop / start simulation
  root <- attach "div.svg-container" -- typeclass here determined by D3Selection_ in SimulationM

  svg   <- appendTo root Svg (svgAttrs sim w h) 
  inner <- appendTo svg  Group []
  _     <- inner `on` Drag DefaultDrag
  _     <- svg   `on` Zoom { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                           , scale  : ScaleExtent 0.1 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                           , name   : "spago"
                           , target : inner
                           }
  -- create the <g>'s to hold the nodes and links and pass these selection onward
  -- so that the data can be joined here each time it is changed
  nodesGroup <- appendTo inner Group [ classed "nodes" ]
  modifying (_staging <<< _enterselections <<< _nodes) (const $ Just nodesGroup)

  linksGroup <- appendTo inner Group [ classed "links" ]
  modifying (_staging <<< _enterselections <<< _links) (const $ Just linksGroup)
  
updateSimulation :: forall m d r id. 
  Bind m => 
  MonadEffect m =>
  MonadState Spago.State m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  (Staging D3Selection_ d r id) ->
  { circle :: Array SelectionAttribute, labels :: Array SelectionAttribute } -> 
  m Unit
updateSimulation staging@{ selections: { nodes: Just nodesGroup, links: Just linkSelection }} attrs = do
  node <- openSelection nodesGroup "g" -- node.selectAll("g"), this call and updateJoin and append all have to match FIX THIS
  -- this will change all the object refs so a defensive copy is needed if join is to work
  mergedData  <- carryOverSimState node staging.rawdata keyIsID_ 
  -- let dataForJoin = staging.rawdata.nodes 
  let dataForJoin = mergedData.updatedNodeData
  simulation_ <- simulationHandle
  -- first the nodedata
  node'            <- updateJoin node Group dataForJoin keyIsID_
  nodeEnter        <- appendTo node'.enter Group [] -- fill in the empty slots in the selection with the new data
  circlesSelection <- appendTo nodeEnter Circle attrs.circle
  labelsSelection  <- appendTo nodeEnter Text attrs.labels
  setAttributes nodeEnter $ enterAttrs simulation_ -- now you can set attributes on these newly entered elements without triggering exception
  setAttributes node'.exit [ remove ]
  setAttributes node'.update $ updateAttrs simulation_
  _ <- circlesSelection `on` Drag DefaultDrag -- TODO needs to ACTUALLY drag the parent transform, not this circle as per DefaultDrag
    -- now the linkData
  linksEnter <- openSelection linkSelection "line.link"
  -- linksUpdateSelections <- updateJoin linksEnter Line mergedData.atedLinkData keyIsID_
  -- newlyEnteredLinks     <- appendTo linksUpdateSelections.enter Group [] -- fill in the empty slots in the selection with the new data
  -- _                     <- setAttributes newlyEnteredLinks [ classed link_.linkClass, strokeColor link_.color ]
  -- _                     <- setAttributes linksUpdateSelections.update  [ classed "graphlinkSimUpdate" ]
  -- _                     <- setAttributes linksUpdateSelections.exit    [ remove ]  
  -- addTickFunction "links" $
  --   Step linksUpdateSelections.update [ x1 (_.x <<< link_.source), y1 (_.y <<< link_.source), x2 (_.x <<< link_.target), y2 (_.y <<< link_.target) ]
  mergedSelection <- mergeSelections node'.enter node'.update  -- merged enter and update becomes the `node` selection for next pass
  setNodes dataForJoin
  -- addTickFunction "nodes" $ -- NB the position of the <g> is updated, not the <circle> and <text> within it
  --   Step mergedSelection [ transform' datum_.translateNode ]
  -- modifying (_staging <<< _enterselections <<< _nodes) (const $ Just mergedSelection)
  -- modifying (_staging <<< _enterselections <<< _links) (const $ Just linksUpdateSelections.update)

updateSimulation _ _ = pure unit -- something's gone badly wrong, one or both selections are missing


{-
updateGraphLinks :: forall m row. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
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
      linksSelection <- linksGroup D3.<-> 
                        SplitJoinCloseWithKeyFunction
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
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  Array SpagoGraphLinkID ->
  m Unit
updateGraphLinks' links = do
  (maybeLinksGroup :: Maybe D3Selection_) <- getSelection "linksGroup"
    
  case maybeLinksGroup of
    Nothing -> pure unit
    (Just linksGroup) -> do
      linksSelection <- linksGroup D3.<-> 
                        SplitJoinCloseWithKeyFunction
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
  MonadState { simulation :: D3SimulationState_ | row } m =>
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
      linksSelection <- linksGroup D3.<-> 
                        SplitJoinCloseWithKeyFunction
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
--   MonadState { simulation :: D3SimulationState_ | row } m =>
--   SelectionM D3Selection_ m =>
--   SimulationM D3Selection_ m =>
--   String -> 
--   m Unit
-- removeNamedSelection name = do
--   (maybeSelection :: Maybe D3Selection_) <- getSelection name
--   case maybeSelection of
--     Nothing -> pure unit
--     (Just selection) -> do
--       setAttributes selection [ remove ]

--   pure unit
  
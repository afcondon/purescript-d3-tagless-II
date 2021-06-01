module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, onMouseEvent, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Simulation_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Attributes
import D3.Examples.Spago.Model 
import D3.FFI (configSimulation_, initSimulation_, setLinks_, setNodes_, startSimulation_)
import D3.Interpreter (class D3InterpreterM, append, attach, on, (<+>))
import D3.Layouts.Simulation (Force(..), ForceType(..), putEachForceInSimulation)
import D3.Node (D3_Link(..), NodeID, getSourceX, getSourceY, getTargetX, getTargetY)
import D3.Selection (Behavior(..), DragBehavior(..), Join(..), Keys(..), node)
import D3.Simulation.Config (defaultConfigSimulation)
import D3.Simulation.Config as F
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Array (filter)
import Data.Number (infinity)
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, unit, (/), (<$>), (<>), (==))
import Unsafe.Coerce (unsafeCoerce)

spagoForces :: Array Force
spagoForces =  
  [ Force "charge"  ForceManyBody [ F.strength (-60.0), F.theta 0.9, F.distanceMin 1.0, F.distanceMax infinity ]
  , Force "x"       ForceX        [ F.strength 0.1, F.x 0.0 ]
  , Force "y"       ForceY        [ F.strength 0.1, F.y 0.0 ]
  , Force "center"  ForceCenter   [ F.strength 0.5, F.x 0.0, F.y 0.0 ]
  , Force "collide" ForceCollide  [ F.strength 1.0, F.radius datum.collideRadius, F.iterations 1.0 ]
  , Force "radial"  ForceRadial   [ F.strength 0.1, F.x 0.0, F.y 0.0, F.radius 800.0 ]
  ]
      
-- | recipe for this force layout graph
graphScript :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SpagoModel ->
  m selection
graphScript (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h 
                                           , classed "graph"] )
  linksGroup <- svg  `append` (node Group  [ classed "links", strokeColor "#999" ])
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` model.nodes
      _          = simulation `putEachForceInSimulation` spagoForces
      _          = setLinks_ simulation model.links (\d i -> d.id)

  linksSelection <- linksGroup <+> Join {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ classed link.linkClass ] -- default invisible in CSS unless marked "visible"
  }
  nodesSelection <- nodesGroup <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed datum.nodeClass, transform' datum.translateNode ]
  }

  circle  <- nodesSelection `append` (node Circle [ radius datum.radius
                                                  , fill datum.colorByGroup
                                                  -- , on MouseEnter (\e d t -> stopSimulation_ simulation) 
                                                  , onMouseEvent MouseClick (\e d t -> startSimulation_ simulation)
                                                  , onMouseEvent MouseEnter (\e d t -> highlightNeighborhood simulation linksGroup (unsafeCoerce model) (datum.id d))
                                                  , onMouseEvent MouseLeave (\e d t -> unhighlightNeighborhood simulation linksGroup (unsafeCoerce model))
                                                  ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label",  x 0.2, y datum.positionLabel, text datum.name]) 
  
  _ <- linksSelection `on` Tick { name: "links", simulation, chain: [ x1 getSourceX, y1 getSourceY, x2 getTargetX, y2 getTargetY ]}
  _ <- nodesSelection `on` Tick { name: "nodes", simulation, chain: [ classed datum.nodeClass, transform' datum.translateNode ]}
  _ <- nodesSelection `on` Drag DefaultDrag
  _ <- svg `on` Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                     , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                     , name : "spago"
                     }
  pure svg


-- | no sigs on these because they're currently called using unsafeCoerce to account for the fact that the link IDs
-- | have been swizzled for their underlying objects
-- TODO remove need for unsafeCoerce on the model that's passed here
highlightNeighborhood simulation linkselection { links, prunedTreeLinks } nodeId = markAsSpotlit_ nodeId simulation linkselection (sourceLinks <> targetLinks) sources targets
  where
    allLinks    = links <> prunedTreeLinks
    sourceLinks = filter (\(D3_Link l) -> l.target.id == nodeId) allLinks
    targetLinks = filter (\(D3_Link l) -> l.source.id == nodeId) allLinks
    sources     = (\(D3_Link l) -> l.source.id) <$> sourceLinks
    targets     = (\(D3_Link l) -> l.target.id) <$> targetLinks

unhighlightNeighborhood simulation linkselection { links } = removeSpotlight_ simulation linkselection links -- we're caching all the selections on the JS side, simply reversing what we've done in highlight

foreign import markAsSpotlit_   :: forall link selection. NodeID -> D3Simulation_ -> selection -> Array link -> Array NodeID -> Array NodeID -> Unit
foreign import removeSpotlight_ :: forall link selection.           D3Simulation_ -> selection -> Array link -> Unit
foreign import toggleSimulation_ :: D3Simulation_ -> Unit
foreign import unfixElementsNode :: forall element. element -> Unit -- TODO not a forall, actually a DOM element
foreign import refixElementsNode :: forall element. element -> Unit
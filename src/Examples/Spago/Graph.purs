module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, on, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Data.Types (D3Simulation_, Element(..), MouseEvent(..))
import D3.Examples.Spago.Attributes (chooseRadius, chooseRadiusFn, colorByGroup, linkClass, nodeClass, positionLabel, setX1, setX2, setY1, setY2, translateNode)
import D3.Examples.Spago.Model (SpagoModel, getIdFromSpagoSimNode, getNameFromSpagoSimNode)
import D3.FFI (D3ForceHandle_, configSimulation_, getLinks_, initSimulation_, putForcesInSimulation_, setLinks_, setNodes_, stopSimulation_)
import D3.FFI.Config (defaultConfigSimulation, defaultForceCenterConfig, defaultForceCollideConfig, defaultForceLinkConfig, defaultForceManyConfig, defaultForceRadialFixedConfig, defaultForceXConfig, defaultForceYConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Layouts.Simulation (Force(..), createForce)
import D3.Node (D3_Link(..), NodeID, getSourceX, getSourceY, getTargetX, getTargetY)
import D3.Selection (DragBehavior(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (cons, filter)
import Data.Tuple (Tuple(..))
import Prelude (class Bind, Unit, bind, negate, pure, unit, ($), (/), (<$>), (<>), (==))
import Unsafe.Coerce (unsafeCoerce)

spagoForces :: Array D3ForceHandle_
spagoForces = createForce <$> 
  [ ForceManyBody    $ (defaultForceManyConfig "charge")   { strength = -100.0 }
  , ForceX           $ (defaultForceXConfig "x")           { strength = 0.1 }
  , ForceY           $ (defaultForceYConfig "y")           { strength = 0.1 }
  , ForceCenter      $ (defaultForceCenterConfig "center") { strength = -1.0 }
  , ForceCollide     $  defaultForceCollideConfig "collide"        (\d -> chooseRadiusFn d)
  , ForceRadialFixed $ defaultForceRadialFixedConfig "radial" 800.0
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
  svg        <- root `append` (node Svg    [ viewBox (-w / 2.0) (-h / 2.0) w h ] )
  centerDot  <- svg  `append` (node Circle [ radius 20.0, fill "red", x (w / 2.0), y h ])
  linksGroup <- svg  `append` (node Group  [ classed "links", strokeColor "#999" ])
  nodesGroup <- svg  `append` (node Group  [ classed "nodes" ])

  let simulation = initSimulation_ unit
      _          = simulation `configSimulation_` defaultConfigSimulation
      nodes      = simulation `setNodes_` model.nodes
      _          = simulation `putForcesInSimulation_` spagoForces
      _          = setLinks_ simulation model.links (\d i -> d.id)

  linksSelection <- linksGroup <+> JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ classed linkClass ] -- default invisible in CSS unless marked "visible"
    , simulation: simulation
    , tickName  : "links"
    , onTick    : [ x1 getSourceX, y1 getSourceY, x2 getTargetX, y2 getTargetY ] -- is this tick function working on links that are removed and then added back 
    , onDrag    : SimulationDrag NoDrag
  }

  nodesSelection <- nodesGroup <+> JoinSimulation {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : nodes
    , behaviour : [ classed nodeClass, transform' translateNode ]
    , simulation: simulation
    , tickName  : "nodes"
    , onTick    : [ transform' translateNode  ]
    , onDrag    : SimulationDrag DefaultDrag
  }

  circle  <- nodesSelection `append` (node Circle [ radius (chooseRadius model.maps.path_2_LOC) 
                                                  , fill (colorByGroup model.maps.id_2_Package)
                                                  -- , on MouseEnter (\e d t -> stopSimulation_ simulation) 
                                                  , on MouseClick (\e d t -> toggleSimulation_ simulation)
                                                  , on MouseEnter (\e d t -> highlightNeighborhood simulation linksGroup (unsafeCoerce model) (getIdFromSpagoSimNode d))
                                                  , on MouseLeave (\e d t -> unhighlightNeighborhood simulation linksGroup (unsafeCoerce model))
                                                  ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label",  x 0.2, y (positionLabel model.maps.path_2_LOC), text getNameFromSpagoSimNode]) 
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }
  pure svg'


-- | no sigs on these because they're currently called using unsafeCoerce to account for the fact that the link IDs
-- | have been swizzled for their underlying objects

highlightNeighborhood simulation linkselection { links, prunedTreeLinks } nodeId = markAsSpotlit_ nodeId simulation linkselection (sourceLinks <> targetLinks) sources targets
  where
    allLinks = links <> prunedTreeLinks
    sourceLinks = filter (\(D3_Link l) -> l.target.id == nodeId) allLinks
    targetLinks = filter (\(D3_Link l) -> l.source.id == nodeId) allLinks
    sources = (\(D3_Link l) -> l.source.id) <$> sourceLinks
    targets = (\(D3_Link l) -> l.target.id) <$> targetLinks

unhighlightNeighborhood simulation linkselection { links } = removeSpotlight_ simulation linkselection links -- we're caching all the selections on the JS side, simply reversing what we've done in highlight

foreign import markAsSpotlit_   :: forall link selection. NodeID -> D3Simulation_ -> selection -> Array link -> Array NodeID -> Array NodeID -> Unit
foreign import removeSpotlight_ :: forall link selection.           D3Simulation_ -> selection -> Array link -> Unit
foreign import toggleSimulation_ :: D3Simulation_ -> Unit
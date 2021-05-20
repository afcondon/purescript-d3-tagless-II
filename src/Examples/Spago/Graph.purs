module D3.Examples.Spago.Graph where

import D3.Attributes.Sugar (classed, fill, on, radius, strokeColor, text, transform', viewBox, x, x1, x2, y, y1, y2)
import D3.Examples.Spago.Model 
import D3.Data.Types (Datum_, Element(..), Index_, MouseEvent(..))
import D3.FFI.Config (defaultConfigSimulation, defaultForceCenterConfig, defaultForceCollideConfig, defaultForceLinkConfig, defaultForceManyConfig, defaultForceXConfig, defaultForceYConfig)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Layouts.Simulation (Force(..), ForceType(..), initSimulation)
import D3.Node (D3_Link(..), D3_SimulationNode(..), NodeID, getNodeX, getNodeY, getSourceX, getSourceY, getTargetX, getTargetY)
import D3.Scales (d3SchemeCategory10N_)
import D3.Selection (DragBehavior(..), Join(..), Keys(..), SimulationDrag(..), node)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..), ZoomTarget(..))
import Data.Array (cons, foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Math (sqrt) as Math
import Prelude (class Bind, Unit, bind, negate, pure, show, unit, ($), (+), (/), (<>), (==))
import Unsafe.Coerce (unsafeCoerce)

-- | recipe for this force layout graph
graphScript :: forall m selection. 
  Bind m => 
  D3InterpreterM selection m => 
  Tuple Number Number ->
  SpagoModel -> 
  m selection
graphScript (Tuple w h) model = do
  root       <- attach "div#spago"
  svg        <- root `append` (node Svg   [ viewBox (-w / 2.0) (-h / 2.0) w h ] )
  centerDot  <- svg  `append` (node Circle [ radius 20.0, fill "red", x (w / 2.0), y h ])
  linksGroup <- svg  `append` (node Group [ classed "links", strokeColor "#999" ])
  nodesGroup <- svg  `append` (node Group [ classed "nodes" ])

  let forces      = [ Force $ ForceManyBody    $ (defaultForceManyConfig "charge") { strength = -100.0 }
                    , Force $ ForceCollide     $  defaultForceCollideConfig "collide" (\d -> chooseRadiusFn d)
                    , Force $ ForceX           $ (defaultForceXConfig "x") { strength = 0.05 }
                    , Force $ ForceY           $ (defaultForceYConfig "y") { strength = 0.05 }
                    , Force $ ForceCenter      $ (defaultForceCenterConfig "center") { strength = -1.0 }
                    , Force $ ForceLink        $ (defaultForceLinkConfig "links" model.links (\d -> d.id))
                    -- , Force $ ForceRadialFixed $ defaultForceRadialFixedConfig "radial" 500.0
                    ]
      { simulation, nodes } = initSimulation forces model.nodes defaultConfigSimulation
      -- _ = pinNodeMatchingPredicate nodes (\(D3SimNode n) -> n.name == "Main") 0.0 0.0
      -- _ = stopSimulation_ simulation

  linksSelection <- linksGroup <+> JoinSimulation {
      element   : Line
    , key       : UseDatumAsKey
    , "data"    : model.links
    , behaviour : [ classed linkClass ] -- default invisible in CSS unless marked "visible"
    , simulation: simulation
    , tickName  : "links"
    , onTick    : [ x1 setX1, y1 setY1, x2 setX2, y2 setY2 ]
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

  circle  <- nodesSelection `append` (node Circle [ radius (chooseRadius model.path2LOCMap) 
                                                  , fill (colorByGroup model.id2PackageIDMap)
                                                  -- , on MouseEnter (\e d t -> stopSimulation_ simulation) 
                                                  -- , on MouseLeave (\e d t -> startSimulation_ simulation)
                                                  , on MouseEnter (\e d t -> highlightNeighborhood (unsafeCoerce model) (getIdFromSpagoSimNode d))
                                                  , on MouseLeave (\e d t -> unhighlightNeighborhood (unsafeCoerce model) (getIdFromSpagoSimNode d))
                                                  ]) 
  labels' <- nodesSelection `append` (node Text [ classed "label",  x 0.2, y (positionLabel model.path2LOCMap), text getNameFromSpagoSimNode]) 
  
  svg' <- svg `attachZoom`  { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                            , scale     : ScaleExtent 0.2 2.0 -- wonder if ScaleExtent ctor could be range operator `..`
                            , qualifier : "tree"
                            , target    : SelfTarget
                            }
  pure svg'


-- | no sigs on these because they're currently called using unsafeCoerce to account for the fact that the link IDs
-- | have been swizzled for their underlying objects
highlightNeighborhood { links } nodeId = markAsSpotlit_ nodeId sources targets
  where
    sources = foldl (\acc (D3_Link l) -> if l.target.id == nodeId then (cons l.source.id acc) else acc) [] links
    targets = foldl (\acc (D3_Link l) -> if l.source.id == nodeId then (cons l.target.id acc) else acc) [] links

unhighlightNeighborhood { links } nodeId = removeSpotlight_ unit
  where
    sources = foldl (\acc l -> if l.target.id == nodeId then (cons l.source.id acc) else acc) [] links
    targets = foldl (\acc l -> if l.source.id == nodeId then (cons l.target.id acc) else acc) [] links

foreign import markAsSpotlit_   :: NodeID -> Array NodeID -> Array NodeID -> Unit
foreign import removeSpotlight_ :: Unit -> Unit


-- -- this is boilerplate but...typed attribute setters facilitate typeclass based conversions
-- -- we give the chart our Model type but behind the scenes it is mutated by D3 and additionally
-- -- which projection of the "Model" is active in each Join varies so we can't have both strong
-- -- static type representations AND lightweight syntax with JS compatible lambdas (i think)
-- -- TODO move coerce for well defined (ie shared) types to FFI, try to use Row machinery to eliminate need for this or tighten up the type safety

chooseRadius :: Map String Number -> Datum_ -> Number
chooseRadius locMap datum = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  case d.nodetype of
    IsModule   -> Math.sqrt (fromMaybe 10.0 $ M.lookup d.path locMap)
    IsPackage -> packageRadius

positionLabel :: Map String Number -> Datum_ -> Number
positionLabel locMap datum = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  case d.nodetype of
    IsModule -> negate $ Math.sqrt (fromMaybe 10.0 $ M.lookup d.path locMap)
    IsPackage -> 0.0

chooseRadiusFn :: Datum_ -> Index_ -> Number
chooseRadiusFn datum index = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  case d.nodetype of
    IsModule  -> moduleRadius
    IsPackage -> packageRadius + packageForceRadius

nodeClass :: Datum_ -> String
nodeClass datum = do
  let (D3SimNode d) = datumIsSpagoSimNode datum
  show d.nodetype

linkClass :: Datum_ -> String
linkClass datum = do
  let (D3_Link d) = datumIsSpagoLink datum
  show d.linktype

translateNode :: Datum_ -> String
translateNode datum = "translate(" <> show x <> "," <> show y <> ")"
  where 
    d = datumIsGraphNode datum
    (x :: Number) = (unsafeCoerce datum).x
    (y :: Number) = (unsafeCoerce datum).y

colorByGroup :: M.Map NodeID NodeID -> Datum_ -> String
colorByGroup packageMap datum = d3SchemeCategory10N_ (toNumber $ fromMaybe 0 packageID)
  where
    (D3SimNode d) = unsafeCoerce datum
    packageID     = M.lookup d.id packageMap


setX1 :: Datum_ -> Number
setX1 = getSourceX
setY1 :: Datum_ -> Number
setY1 = getSourceY
setX2 :: Datum_ -> Number
setX2 = getTargetX
setY2 :: Datum_ -> Number
setY2 = getTargetY
setCx :: Datum_ -> Number
setCx = getNodeX
setCy :: Datum_ -> Number
setCy = getNodeY




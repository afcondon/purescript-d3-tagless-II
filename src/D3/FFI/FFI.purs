module D3.FFI where

-- brings together ALL of the wrapped D3js functions and FFI / native types
-- probably should break it up again when it's more feature complete (ie to match D3 modules). Maybe.

import D3.Node

import D3.Data.Tree (TreeJson_, TreeLayoutFn_, TreeType(..))
import D3.Data.Types (D3Data_, D3Selection_, D3Simulation_, Datum_, Element, Index_, PointXY, Selector, Transition, ZoomConfigDefault_, ZoomConfig_)
import Data.Array (find)
import Data.Function.Uncurried (Fn2)
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable)
import Prelude (Unit, unit, ($), (<$>))

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js zoom module       *********************************************
-- | *********************************************************************************************************************
foreign import data ZoomBehavior_ :: Type  -- the zoom behavior, provided to Event Handler
foreign import d3AttachZoom_              :: D3Selection_ -> ZoomConfig_        -> D3Selection_
foreign import d3AttachZoomDefaultExtent_ :: D3Selection_ -> ZoomConfigDefault_ -> D3Selection_
foreign import showAttachZoomDefaultExtent_ :: forall selection. selection -> ZoomConfigDefault_ -> selection
foreign import showAttachZoom_              :: forall selection. selection -> ZoomConfig_ -> selection

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for Selection & Transition  ********************************************
-- | *********************************************************************************************************************

foreign import d3SelectAllInDOM_     :: Selector    -> D3Selection_
foreign import d3SelectFirstInDOM_   :: Selector    -> D3Selection_
foreign import d3SelectionSelectAll_ :: Selector    -> D3Selection_ -> D3Selection_
foreign import d3SelectionSelect_    :: Selector    -> D3Selection_ -> D3Selection_
foreign import d3SelectionIsEmpty_   :: D3Selection_ -> Boolean
foreign import d3EnterAndAppend_     :: String      -> D3Selection_ -> D3Selection_
foreign import d3Append_             :: String      -> D3Selection_ -> D3Selection_

foreign import d3Exit_               :: D3Selection_ -> D3Selection_

-- Removes the selected elements from the document. Returns this selection (the
-- removed elements) which are now detached from the DOM. There is not currently a
-- dedicated API to add removed elements back to the document; however, you can
-- pass a function to selection.append or selection.insert to re-add elements.
foreign import d3RemoveSelection_    :: D3Selection_ -> D3Selection_

foreign import d3FilterSelection_    :: D3Selection_ -> Selector   -> D3Selection_
foreign import d3OrderSelection_     :: D3Selection_ -> D3Selection_
foreign import d3RaiseSelection_     :: D3Selection_ -> D3Selection_
foreign import d3LowerSelection_     :: D3Selection_ -> D3Selection_
foreign import d3SortSelection_      :: forall d. D3Selection_ -> (d -> d -> Int) -> D3Selection_

foreign import d3Data_               :: forall d. Array d -> D3Selection_ -> D3Selection_

type ComputeKeyFunction_ = Datum_ -> Index_
foreign import d3KeyFunction_        :: forall d. Array d -> ComputeKeyFunction_ -> D3Selection_ -> D3Selection_

-- we'll coerce everything to this type if we can validate attr lambdas against provided data
-- ... and we'll also just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr :: Type 
-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are 
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_ :: String -> D3Selection -> ???? -- solve the ???? as needed later
foreign import d3AddTransition_ :: D3Selection_ -> Transition -> D3Selection_ -- this is the PS transition record

foreign import d3SetAttr_       :: String -> D3Attr -> D3Selection_ -> D3Selection_
foreign import d3SetText_       :: D3Attr -> D3Selection_ -> D3Selection_
foreign import d3SetProperty_   :: D3Attr -> D3Selection_ -> D3Selection_
foreign import d3SetHTML_       :: D3Attr -> D3Selection_ -> D3Selection_

foreign import emptyD3Data_ :: D3Data_ -- probably just null, could this be monoid too??? ie Last (Maybe D3Data_)

foreign import defaultDrag_ :: D3Selection_ -> D3Selection_
foreign import disableDrag_ :: D3Selection_ -> D3Selection_

-- show functions that are used for the string version of the interpreter and also for debugging inside Selection.js
foreign import showSelectAllInDOM_  :: forall selection. Selector -> String -> selection
foreign import showSelectAll_       :: forall selection. Selector -> String -> selection -> selection
foreign import showEnterAndAppend_  :: forall selection. Element -> selection -> selection
foreign import showExit_            :: forall selection. selection -> selection
foreign import showAddTransition_   :: forall selection. selection -> Transition -> selection 
foreign import showRemoveSelection_ :: forall selection. selection -> selection
foreign import showAppend_          :: forall selection. Element -> selection -> selection
foreign import showKeyFunction_     :: forall selection d. Array d -> ComputeKeyFunction_ -> selection -> selection
foreign import showData_            :: forall selection d. Array d -> selection -> selection
foreign import showSetAttr_         :: forall selection. String -> D3Attr -> selection -> selection
foreign import showSetText_         :: forall selection. D3Attr -> selection -> selection
foreign import showSetHTML_         :: forall selection. D3Attr -> selection -> selection
foreign import showSetProperty_     :: forall selection. D3Attr -> selection -> selection
foreign import showSetOrdering_     :: forall selection. String -> selection -> selection
foreign import selectionOn_         :: forall selection callback. selection -> String -> callback -> selection  


-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js Simulation module  *********************************************
-- | *********************************************************************************************************************
-- | foreign types associated with Force Layout Simulation

type GraphModel_ link node = { links :: Array link, nodes :: Array node }
foreign import data D3ForceHandle_     :: Type
foreign import data CustomForceConfig_ :: Type

-- | a record to initialize / configure simulations
type SimulationConfig_ = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
    , running       :: Boolean
}

foreign import initSimulation_         ::                  SimulationConfig_ -> D3Simulation_
foreign import configSimulation_       :: D3Simulation_ -> SimulationConfig_   -> D3Simulation_

foreign import getNodes_               :: forall d.   D3Simulation_ -> Array (D3_SimulationNode d)
foreign import setNodes_               :: forall d.   D3Simulation_ -> Array (D3_SimulationNode d) -> Array (D3_SimulationNode d)

-- removing forces should be done by simulation manager and doesn't need more indirection
-- foreign import removeForceByName_  :: D3Simulation_ -> String -> D3Simulation_

foreign import setLinks_               :: forall d r datum. D3Simulation_ -> Array (D3_Link d r) -> (datum -> Index_ -> Number) -> D3ForceHandle_
foreign import getLinks_               :: forall d r. D3ForceHandle_ -> Array (D3_Link d r)

foreign import startSimulation_        :: D3Simulation_ -> Unit
foreign import stopSimulation_         :: D3Simulation_ -> Unit

foreign import pinNode_   :: forall d. Number -> Number -> D3_SimulationNode d -> Unit
foreign import unpinNode_ :: forall d. D3_SimulationNode d -> Unit

-- NB mutating function
pinNode :: forall d. D3_SimulationNode d -> PointXY -> D3_SimulationNode d
pinNode node p = do
  let _ = pinNode_ p.x p.y node
  node -- NB mutated value, fx / fy have been set

pinNodeMatchingPredicate :: forall d. Array (D3_SimulationNode d) -> ((D3_SimulationNode d) -> Boolean) -> Number -> Number -> Unit
pinNodeMatchingPredicate nodes predicate fx fy = fromMaybe unit $ (pinNode_ fx fy) <$> (find predicate nodes)


-- TODO this all has to change completely to work within Tagless 
-- foreign import data NativeSelection :: Type -- just temporarily defined to allow foreign functions to pass
-- foreign import addAttrFnToTick_           :: D3Selection_ -> D3Attr -> Unit
foreign import onTick_                :: D3Simulation_ -> String -> (Unit -> Unit) -> Unit
foreign import disableTick_           :: D3Simulation_ -> String -> Unit
foreign import defaultSimulationDrag_ :: D3Selection_ -> D3Simulation_ -> Unit
foreign import setAlpha_              :: D3Simulation_ -> Number -> Unit
foreign import setAlphaMin_           :: D3Simulation_ -> Number -> Unit
foreign import setAlphaDecay_         :: D3Simulation_ -> Number -> Unit
foreign import setAlphaTarget_        :: D3Simulation_ -> Number -> Unit
foreign import setVelocityDecay_      :: D3Simulation_ -> Number -> Unit

-- implementations / wrappers for the Force ADT
foreign import forceCenter_       :: Unit -> D3ForceHandle_
foreign import forceCollideFn_    :: Unit -> D3ForceHandle_
foreign import forceMany_         :: Unit -> D3ForceHandle_
foreign import forceRadial_       :: Unit -> D3ForceHandle_
foreign import forceX_            :: Unit -> D3ForceHandle_
foreign import forceY_            :: Unit -> D3ForceHandle_
foreign import forceLink_         :: forall r. Array (D3_Link NodeID r) -> D3ForceHandle_
foreign import forceCustom_       :: Unit -> D3ForceHandle_
foreign import dummyForceHandle_  :: D3ForceHandle_ -- used for fixed "forces", is null under the hood

foreign import setForceRadius_      :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceStrength_    :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceCx_          :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceCy_          :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceTheta_       :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceDistanceMin_ :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceDistanceMax_ :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceIterations_  :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceX_           :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceY_           :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_
foreign import setForceDistance_    :: D3ForceHandle_ -> D3Attr -> D3ForceHandle_

foreign import putForceInSimulation_ :: D3Simulation_ -> String -> D3ForceHandle_ -> D3Simulation_
foreign import applyFixForceInSimulationXY_ :: D3Simulation_ -> String -> (Datum_ -> PointXY) -> D3Simulation_ 
foreign import applyFixForceInSimulationX_  :: D3Simulation_ -> String -> (Datum_ -> Number)  -> D3Simulation_ 
foreign import applyFixForceInSimulationY_  :: D3Simulation_ -> String -> (Datum_ -> Number)  -> D3Simulation_ 
foreign import setAsNullForceInSimulation_ :: D3Simulation_ -> String -> D3Simulation_
-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js Hierarchy module  *********************************************
-- | *********************************************************************************************************************

-- this is an opaque type behind which hides the data type of the Purescript tree that was converted
foreign import data RecursiveD3TreeNode :: Type
-- this is the Purescript Tree after processing in JS to remove empty child fields from leaves etc
-- need to ensure that this structure is encapsulated in libraries (ie by moving this code)
foreign import data D3TreeLike_         :: Type -- covers both trees and clusters
foreign import data D3SortComparator_   :: Type -- a number such that n < 0 => a > b, n > 0 => b > a, n == 0 undef'd
foreign import data D3Hierarchical_     :: Type

foreign import hierarchyFromJSON_       :: forall d. TreeJson_ -> D3_TreeNode d
-- TODO now that these different hierarchy rows are composed at type level, polymorphic functions should be written
foreign import treeSortForCirclePack_   :: forall d. D3CirclePackRow d -> D3CirclePackRow d
foreign import treeSortForTreeMap_      :: forall d. D3TreeMapRow d -> D3TreeMapRow d
foreign import treeSortForTree_         :: forall d. D3_TreeNode d -> D3_TreeNode d
foreign import treeSortForTree_Spago    :: forall d. D3_TreeNode d -> D3_TreeNode d

-- next some functions to make attributes, types are a bit sloppy here
foreign import hasChildren_             :: Datum_ -> Boolean -- really only meaningful when Datum_ when is a D3HierarchicalNode_

-- the full API for hierarchical nodes:
-- TODO these should all be operating on cooked tree type, however that is to be done
foreign import descendants_     :: forall r. D3_TreeNode r -> Array (D3_TreeNode r)
foreign import find_            :: forall r. D3_TreeNode r -> (Datum_ -> Boolean) -> Nullable (D3_TreeNode r)
foreign import links_           :: forall d r1 r2. D3_TreeNode r1 -> Array (D3_Link d r2)
foreign import ancestors_       :: forall r. D3_TreeNode r -> Array (D3_TreeNode r)
foreign import leaves_          :: forall r. D3_TreeNode r -> Array (D3_TreeNode r)
foreign import path_            :: forall r. D3_TreeNode r -> D3_TreeNode r -> Array (D3_TreeNode r)

getLayout :: TreeType -> TreeLayoutFn_
getLayout layout = do
  case layout of
    TidyTree   -> getTreeLayoutFn_ unit
    Dendrogram -> getClusterLayoutFn_ unit

foreign import getTreeLayoutFn_       :: Unit -> TreeLayoutFn_
foreign import getClusterLayoutFn_    :: Unit -> TreeLayoutFn_

foreign import runLayoutFn_           :: forall r. TreeLayoutFn_ -> D3_TreeNode r -> D3_TreeNode r
foreign import treeSetSize_           :: TreeLayoutFn_ -> Array Number -> TreeLayoutFn_
foreign import treeSetNodeSize_       :: TreeLayoutFn_ -> Array Number -> TreeLayoutFn_
foreign import treeSetSeparation_     :: forall d. TreeLayoutFn_ -> (Fn2 (D3_TreeNode d) (D3_TreeNode d) Number) -> TreeLayoutFn_
foreign import treeMinMax_            :: forall d. D3_TreeNode d -> { xMin :: Number, xMax :: Number, yMin :: Number, yMax :: Number }
-- foreign import sum_                :: D3HierarchicalNode_ -> (Datum_ -> Number) -> D3HierarchicalNode_ -- alters the tree!!!!
-- from docs:  <<if you only want leaf nodes to have internal value, then return zero for any node with children. 
-- For example, as an alternative to node.count:
--        root.sum(function(d) { return d.value ? 1 : 0; });
-- foreign import count_              :: D3HierarchicalNode_ -> D3HierarchicalNode_ -- NB alters the tree!!!
-- foreign import sort_               :: D3HierarchicalNode_ -> (D3HierarchicalNode_ -> D3HierarchicalNode_ -> D3SortComparator_)
-- foreign import each_ -- breadth first traversal
-- foreign import eachAfter_ 
-- foreign import eachBefore_
-- foreign import deepCopy_ -- copies (sub)tree but shares data with clone !!!
foreign import sharesParent_          :: forall r. (D3_TreeNode r) -> (D3_TreeNode r) -> Boolean

foreign import linkHorizontal_        :: (Datum_ -> String) 
foreign import linkVertical_          :: (Datum_ -> String) 
foreign import linkClusterHorizontal_ :: Number -> (Datum_ -> String) 
foreign import linkClusterVertical_   :: Number -> (Datum_ -> String) 
foreign import linkRadial_            :: (Datum_ -> Number) -> (Datum_ -> Number) -> (Datum_ -> String)
foreign import autoBox_               :: Datum_ -> Array Number

-- accessors for fields of D3HierarchicalNode, only valid if layout has been done, hence the _XY version of node
-- REVIEW maybe accessors aren't needed if you can ensure type safety
foreign import hNodeDepth_  :: forall r. D3_TreeNode r -> Number
foreign import hNodeHeight_ :: forall r. D3_TreeNode r -> Number
foreign import hNodeX_      :: forall r. D3_TreeNode r -> Number
foreign import hNodeY_      :: forall r. D3_TreeNode r -> Number

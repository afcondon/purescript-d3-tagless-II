module PSD3.Internal.FFI where

-- brings together ALL of the wrapped D3js functions and FFI / native types
-- probably should break it up again when it's more feature complete (ie to match D3 modules). Maybe.

import PSD3.Data.Node

import PSD3.Data.Tree (TreeJson_, TreeLayoutFn_, TreeType(..))
import PSD3.Internal.Types (D3Data_, D3Selection_, D3Simulation_, Datum_, Index_, PointXY, Selector, Transition, ZoomConfigDefault_, ZoomConfig_)
import Data.Function.Uncurried (Fn2)
import Data.Nullable (Nullable)
import Effect (Effect)
import Prelude (Unit, unit)

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js zoom module       *********************************************
-- | *********************************************************************************************************************
foreign import data ZoomBehavior_ :: Type  -- the zoom behavior, provided to Event Handler
foreign import d3AttachZoom_              :: forall d. D3Selection_ d -> ZoomConfig_ d        -> D3Selection_ d
foreign import d3AttachZoomDefaultExtent_ :: forall d. D3Selection_ d -> ZoomConfigDefault_ d -> D3Selection_ d
foreign import showAttachZoomDefaultExtent_ :: forall selection d. selection -> ZoomConfigDefault_ d -> selection
foreign import showAttachZoom_              :: forall selection d. selection -> ZoomConfig_ d -> selection

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for Selection & Transition  ********************************************
-- | *********************************************************************************************************************

foreign import d3SelectAllInDOM_     :: forall d. Selector (D3Selection_ d) -> D3Selection_ d
foreign import d3SelectFirstInDOM_   :: forall d. Selector (D3Selection_ d) -> D3Selection_ d
foreign import d3SelectionSelectAll_ :: forall d. Selector (D3Selection_ d) -> D3Selection_ d -> D3Selection_ d
foreign import d3SelectionSelect_    :: forall d. Selector (D3Selection_ d) -> D3Selection_ d -> D3Selection_ d
foreign import d3SelectionIsEmpty_   :: forall d. D3Selection_ d -> Boolean
foreign import d3GetSelectionData_   :: forall d. D3Selection_ d -> Array Datum_
foreign import d3EnterAndAppend_     :: forall d. String -> D3Selection_ d -> D3Selection_ d
foreign import d3Append_             :: forall d. String -> D3Selection_ d -> D3Selection_ d
foreign import d3MergeSelectionWith_ :: forall d. D3Selection_ d -> D3Selection_ d -> D3Selection_ d

-- these next two are for getting Enter and Exit selections during an update
foreign import d3GetEnterSelection_ :: forall d. D3Selection_ d -> D3Selection_ d
foreign import d3GetExitSelection_  :: forall d. D3Selection_ d -> D3Selection_ d

-- Removes the selected elements from the document. Returns this selection (the
-- removed elements) which are now detached from the DOM. There is not currently a
-- dedicated API to add removed elements back to the document; however, you can
-- pass a function to selection.append or selection.insert to re-add elements.
foreign import d3RemoveSelection_    :: forall d. D3Selection_ d -> D3Selection_ d

foreign import d3FilterSelection_    :: forall d. D3Selection_ d -> Selector (D3Selection_ d) -> D3Selection_ d
foreign import d3OrderSelection_     :: forall d. D3Selection_ d -> D3Selection_ d
foreign import d3RaiseSelection_     :: forall d. D3Selection_ d -> D3Selection_ d
foreign import d3LowerSelection_     :: forall d. D3Selection_ d -> D3Selection_ d
foreign import d3SortSelection_      :: forall d. D3Selection_ d -> (d -> d -> Int) -> D3Selection_ d

foreign import getIndexFromDatum_    :: Datum_ -> Int

foreign import d3Data_               :: forall d. Array d -> D3Selection_ d -> D3Selection_ d
type ComputeKeyFunction_ = Datum_ -> Index_
foreign import keyIsID_           :: ComputeKeyFunction_
foreign import keyIsSourceTarget_ :: ComputeKeyFunction_ -- used for links in simulation
-- REVIEW the returned D3Selection_ here is the full enter, update, exit type of selection
-- which we haven't really modelled in PureScript (opaque type) but maybe it will turn out that we
-- needed to all along
foreign import d3DataWithKeyFunction_ :: forall d1 d2. Array d2 -> ComputeKeyFunction_ -> D3Selection_ d1 -> D3Selection_ d2
foreign import d3DataWithFunction_ :: forall d. (Datum_ -> Array Datum_) -> ComputeKeyFunction_ -> D3Selection_ d -> D3Selection_ d

-- we'll coerce everything to this type if we can validate attr lambdas against provided data
-- ... and we'll also just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr_ :: Type 
-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_ :: String -> D3Selection -> ???? -- solve the ???? as needed later
foreign import d3AddTransition_ :: forall d. D3Selection_ d -> Transition -> D3Selection_ d -- this is the PS transition record

foreign import d3SetAttr_       :: forall d. String -> D3Attr_ -> D3Selection_ d -> D3Selection_ d
foreign import d3SetText_       :: forall d. D3Attr_ -> D3Selection_ d -> D3Selection_ d
foreign import d3SetProperty_   :: forall d. D3Attr_ -> D3Selection_ d -> D3Selection_ d
foreign import d3SetHTML_       :: forall d. D3Attr_ -> D3Selection_ d -> D3Selection_ d

foreign import emptyD3Data_ :: D3Data_ -- probably just null, could this be monoid too??? ie Last (Maybe D3Data_)

foreign import data D3DragFunction_ :: Type
foreign import simulationDrag_ :: forall d. String -> D3Selection_ d -> D3Simulation_ -> D3DragFunction_ -> D3Selection_ d
foreign import simdrag_  :: D3DragFunction_
foreign import simdragHorizontal_ :: D3DragFunction_
foreign import disableDrag_ :: forall d. D3Selection_ d -> D3Selection_ d

foreign import highlightConnectedNodes_ :: forall d. D3Selection_ d -> Array String -> Unit
foreign import clearHighlights_ :: forall d. D3Selection_ d -> Unit
foreign import unpinAllNodes_ :: D3Simulation_ -> Unit
foreign import updateBubbleRadii_ :: D3Simulation_ -> (Boolean -> Int -> Number) -> Unit
foreign import updateNodeExpansion_ :: forall declsData callsData. D3Simulation_ -> (Boolean -> Int -> Number) -> declsData -> callsData -> Datum_ -> Unit
foreign import unsafeSetField_ :: forall a. String -> a -> Datum_ -> Effect Unit

foreign import expandNodeById_ :: forall declsData callsData. D3Simulation_ -> (Boolean -> Int -> Number) -> declsData -> callsData -> String -> Boolean -> Effect Unit
foreign import addModuleArrowMarker_ :: forall d. D3Selection_ d -> Effect Unit
foreign import drawInterModuleDeclarationLinks_ :: forall declsData callsData d. D3Selection_ d -> (Boolean -> Int -> Number) -> declsData -> callsData -> Effect Unit
foreign import filterToConnectedNodes_ :: D3Simulation_ -> (Datum_ -> Index_) -> Array String -> Unit

foreign import selectionOn_         :: forall selection callback. selection -> String -> callback -> selection  


-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js Simulation module  *********************************************
-- | *********************************************************************************************************************

-- links force is very special, can't (easily) manage multiple named versions. 
-- consistency of naming of link force is established with top level name
foreign import linksForceName_ :: String

-- | foreign types associated with Force Layout Simulation
type GraphModel_ link node = { links :: Array link, nodes :: Array node }
foreign import data D3ForceHandle_     :: Type
foreign import data CustomForceConfig_ :: Type

-- | a record to initialize / configure simulations
type SimulationVariables = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
}

foreign import initSimulation_         ::                  SimulationVariables -> (Datum_ -> Index_) -> D3Simulation_
foreign import configSimulation_       :: D3Simulation_ -> SimulationVariables -> D3Simulation_
foreign import readSimulationVariables_ :: D3Simulation_ -> SimulationVariables

foreign import d3PreserveSimulationPositions_ ::
  forall d row.
  D3Selection_ d ->
  Array (SimulationNode row) ->
  (Datum_ -> Index_) ->
  Array (SimulationNode row)
foreign import d3PreserveLinkReferences_ ::
  forall d.
  D3Selection_ d ->
  Array D3Link_Unswizzled ->
  Array D3Link_Unswizzled

foreign import getIDsFromNodes_ :: forall d id. Array (SimulationNode d) -> (Datum_ -> Index_) -> Array id

foreign import getNodes_ :: forall d.   D3Simulation_ -> Array (SimulationNode d)
foreign import setNodes_ :: forall d.   D3Simulation_ -> Array (SimulationNode d) -> Array (SimulationNode d)
-- setLinks will do the swizzling AND prune any links that have source or target that is not in [nodes]
foreign import setLinks_ ::
  D3Simulation_ ->
  Array D3Link_Swizzled ->
  Unit
foreign import swizzleLinks_ ::
  forall d.
  Array D3Link_Unswizzled ->
  Array (SimulationNode d) ->
  (Datum_ -> Index_) ->
  Array D3Link_Swizzled

foreign import getLinkID_              :: (Datum_ -> Index_) -> Datum_ -> Index_
foreign import getLinkIDs_             :: forall id. (Datum_ -> Index_) -> D3Link_Unswizzled -> { sourceID :: id, targetID :: id }
foreign import unsetLinks_             :: D3Simulation_ -> D3Simulation_

foreign import getLinksFromForce_      :: D3ForceHandle_ -> Array D3Link_Unswizzled
foreign import getLinksFromSimulation_ :: D3Simulation_ -> Array D3Link_Swizzled

foreign import startSimulation_        :: D3Simulation_ -> Unit
foreign import stopSimulation_         :: D3Simulation_ -> Unit

foreign import setInSimNodeFlag_     :: forall d. SimulationNode d -> Unit
foreign import unsetInSimNodeFlag_   :: forall d. SimulationNode d -> Unit

-- following functions modify the node and return it - meant to be used on staged data which will then be re-entered to sim
foreign import pinNode_              :: forall d. Number -> Number -> SimulationNode d -> SimulationNode d
foreign import pinNamedNode_         :: forall d. String -> Number -> Number -> SimulationNode d -> SimulationNode d
foreign import pinTreeNode_          :: forall d. SimulationNode d -> SimulationNode d -- modifies fx/fy
foreign import unpinNode_            :: forall d. SimulationNode d -> SimulationNode d -- set fx/fy to null

-- NB mutating function
-- pinNode :: forall d. SimulationNode d -> PointXY -> SimulationNode d
-- pinNode node p = do
--   let _ = pinNode_ p.x p.y node
--   node -- NB mutated value, fx / fy have been set
-- NB mutating function
setInSimNodeFlag :: forall d. SimulationNode d -> SimulationNode d
setInSimNodeFlag node = do
  let _ = setInSimNodeFlag_ node
  node -- NB mutated value, inSim now true
unsetInSimNodeFlag :: forall d. SimulationNode d -> SimulationNode d
unsetInSimNodeFlag node = do
  let _ = unsetInSimNodeFlag_ node
  node -- NB mutated value, inSim now false

-- TODO this all has to change completely to work within Tagless 
-- foreign import data NativeSelection :: Type -- just temporarily defined to allow foreign functions to pass
-- foreign import addAttrFnToTick_           :: D3Selection_ -> D3Attr_ -> Unit
foreign import onTick_                :: D3Simulation_ -> String -> (Unit -> Unit) -> Unit
foreign import disableTick_           :: D3Simulation_ -> String -> Unit
foreign import defaultNodeTick_       :: forall d. String -> D3Simulation_ -> D3Selection_ d -> Unit
foreign import defaultLinkTick_       :: forall d. String -> D3Simulation_ -> D3Selection_ d -> Unit
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
foreign import forceLink_         :: Unit -> D3ForceHandle_ -- actually created in initSimulation where keyFunction is provided too
foreign import forceCustom_       :: Unit -> D3ForceHandle_
foreign import dummyForceHandle_  :: D3ForceHandle_ -- used for fixed "forces", is null under the hood

foreign import setForceRadius_      :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceStrength_    :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceCx_          :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceCy_          :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceTheta_       :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceDistanceMin_ :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceDistanceMax_ :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceIterations_  :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceX_           :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceY_           :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setForceDistance_    :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_
foreign import setLinksKeyFunction_ :: D3ForceHandle_ -> D3Attr_ -> D3ForceHandle_

foreign import putForceInSimulation_        :: D3Simulation_ -> String -> D3ForceHandle_ -> D3Simulation_
-- foreign import restartLinksForceInSimulation_ :: D3Simulation_ -> D3ForceHandle_ -> Array Datum_ -> D3Simulation_
-- foreign import putForceInSimulationWithFilter_ :: D3Simulation_ -> String -> (Datum_ -> Boolean) -> D3ForceHandle_ -> D3Simulation_
foreign import lookupForceByName_           :: D3Simulation_ -> String -> Nullable D3ForceHandle_
foreign import removeFixForceXY_            :: D3Simulation_ -> (Datum_ -> Boolean) -> D3Simulation_
foreign import removeFixForceX_             :: D3Simulation_ -> (Datum_ -> Boolean) -> D3Simulation_
foreign import removeFixForceY_             :: D3Simulation_ -> (Datum_ -> Boolean) -> D3Simulation_
foreign import applyFixForceInSimulationXY_ :: D3Simulation_ -> String -> (Datum_ -> Index_ -> PointXY) -> (Datum_ -> Boolean) -> D3Simulation_ 
foreign import applyFixForceInSimulationX_  :: D3Simulation_ -> String -> (Datum_ -> Index_ -> { x :: Number})  -> (Datum_ -> Boolean) -> D3Simulation_ 
foreign import applyFixForceInSimulationY_  :: D3Simulation_ -> String -> (Datum_ -> Index_ -> { y :: Number})  -> (Datum_ -> Boolean) -> D3Simulation_ 
foreign import setAsNullForceInSimulation_  :: D3Simulation_ -> String -> D3Simulation_
-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js Hierarchy module  *********************************************
-- | *********************************************************************************************************************

-- this is an opaque type behind which hides the data type of the Purescript tree that was converted
foreign import data RecursiveD3TreeNode_ :: Type
-- this is the Purescript Tree after processing in JS to remove empty child fields from leaves etc
-- =======================================================================================
-- | REMOVED: Old D3 hierarchy FFI functions
-- | We now use pure PureScript hierarchy layouts (PSD3.Layout.Hierarchy.*)
-- | These old FFI functions are no longer needed:
-- |   - hierarchyFromJSON_, descendants_, links_, runLayoutFn_, etc.
-- |   - treeSortForCirclePack_, treeSortForTreeMap_, treeSortForTree_
-- |   - hasChildren_, getHierarchyValue_, getHierarchyChildren_, getHierarchyParent_
-- |   - getTreeLayoutFn_, getClusterLayoutFn_, treeSetSize_, treeSetSeparation_, etc.
-- |   - hNodeDepth_, hNodeHeight_, hNodeX_, hNodeY_, hNodeR_
-- =======================================================================================

-- Kept: cloneTreeJson_ is still used for creating copies of tree data
foreign import cloneTreeJson_           :: TreeJson_ -> TreeJson_

-- Kept: Link generators are still used for rendering connections
foreign import linkHorizontal_        :: (Datum_ -> String)
foreign import linkHorizontal2_       :: (Datum_ -> String)
foreign import linkVertical_          :: (Datum_ -> String)
foreign import linkClusterHorizontal_ :: Number -> (Datum_ -> String)
foreign import linkClusterVertical_   :: Number -> (Datum_ -> String)
foreign import linkRadial_            :: (Datum_ -> Number) -> (Datum_ -> Number) -> (Datum_ -> String)
foreign import autoBox_               :: Datum_ -> Array Number

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js Chord module          *********************************************
-- | *********************************************************************************************************************
foreign import data ChordLayout_ :: Type
foreign import data ChordGenerator_ :: Type
foreign import data RibbonGenerator_ :: Type
foreign import data ArcGenerator_ :: Type

foreign import chordLayout_         :: Array (Array Number) -> ChordLayout_
foreign import chordGroups_         :: ChordLayout_ -> Array Datum_
foreign import chordArray_          :: ChordLayout_ -> Array Datum_
foreign import ribbonGenerator_     :: Unit -> RibbonGenerator_
foreign import arcGenerator_        :: Unit -> ArcGenerator_
foreign import ribbonPath_          :: RibbonGenerator_ -> Datum_ -> String
foreign import arcPath_             :: ArcGenerator_ -> Datum_ -> String
foreign import setRibbonRadius_     :: RibbonGenerator_ -> Number -> RibbonGenerator_
foreign import setArcInnerRadius_   :: ArcGenerator_ -> Number -> ArcGenerator_
foreign import setArcOuterRadius_   :: ArcGenerator_ -> Number -> ArcGenerator_

-- | *********************************************************************************************************************
-- | REMOVED: Old D3 hierarchy layout FFI functions (Pack, Treemap, Partition)
-- | We now use pure PureScript implementations in PSD3.Layout.Hierarchy.*
-- | These old FFI functions are no longer needed:
-- |   - packLayout_, packSetSize_, packSetPadding_, runPackLayout_
-- |   - treemapLayout_, treemapSetSize_, treemapSetPadding_, runTreemapLayout_
-- |   - partitionLayout_, partitionSetSize_, partitionSetPadding_, runPartitionLayout_
-- |   - treeSortForCirclePack_, treeSortForTreeMap_, treeSortForPartition_
-- |   - hNodeX0_, hNodeY0_, hNodeX1_, hNodeY1_, hNodeR_
-- | *********************************************************************************************************************

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for Details Panel manipulation  *************************************
-- | *********************************************************************************************************************
foreign import showDetailsPanel_ :: forall d. D3Selection_ d -> Effect Unit
foreign import hideDetailsPanel_ :: forall d. D3Selection_ d -> Effect Unit
foreign import setDetailsModuleName_ :: forall d. D3Selection_ d -> String -> Effect Unit
foreign import populateDetailsList_ :: forall d. D3Selection_ d -> Array String -> Effect Unit
foreign import showModuleLabels_ :: forall d. D3Selection_ d -> Effect Unit
foreign import hideModuleLabels_ :: forall d. D3Selection_ d -> Effect Unit
foreign import switchToSpotlightForces_ :: D3Simulation_ -> Effect Unit
foreign import switchToCompactForces_ :: D3Simulation_ -> Effect Unit
foreign import resetNodeFilter_ :: D3Simulation_ -> Effect Unit
foreign import restoreAllNodes_ :: forall d1 d2. D3Simulation_ -> D3Selection_ d1 -> D3Selection_ d2 -> Array Datum_ -> Array Datum_ -> (Boolean -> Int -> Number) -> (Datum_ -> Index_) -> Effect Unit

module PSD3.Internal.FFI
  ( -- Selection FFI
    d3SelectAllInDOM_
  , d3SelectFirstInDOM_
  , d3SelectionSelectAll_
  , d3SelectionSelect_
  , d3SelectionIsEmpty_
  , d3GetSelectionData_
  , d3EnterAndAppend_
  , d3Append_
  , d3MergeSelectionWith_
  , d3GetEnterSelection_
  , d3GetExitSelection_
  , d3RemoveSelection_
  , d3FilterSelection_
  , d3OrderSelection_
  , d3RaiseSelection_
  , d3LowerSelection_
  , d3SortSelection_
  , getIndexFromDatum_
  , d3Data_
  , ComputeKeyFunction_
  , keyIsID_
  , keyIsSourceTarget_
  , swizzledLinkKey_
  , d3DataWithKeyFunction_
  , d3DataWithFunction_
  , D3Attr_
  , d3SetAttr_
  , d3SetText_
  , d3SetProperty_
  , d3SetHTML_
  , D3DragFunction_
  , simulationDrag_
  , simdrag_
  , simdragHorizontal_
  , disableDrag_
  , selectionOn_
  -- Force FFI (used by psd3-simulation via Config.Apply)
  , linksForceName_
  , D3ForceHandle_
  , SimulationVariables
  , configSimulation_
  , readSimulationVariables_
  , onTick_
  , disableTick_
  , startSimulation_
  , stopSimulation_
  , setAlpha_
  , setAlphaMin_
  , setAlphaDecay_
  , setAlphaTarget_
  , setVelocityDecay_
  -- Force creation and parameter FFI
  , forceCenter_
  , forceCollideFn_
  , forceMany_
  , forceRadial_
  , forceX_
  , forceY_
  , forceLink_
  , forceCustom_
  , dummyForceHandle_
  , CustomForceConfig_
  , setForceRadius_
  , setForceStrength_
  , setForceCx_
  , setForceCy_
  , setForceTheta_
  , setForceDistanceMin_
  , setForceDistanceMax_
  , setForceIterations_
  , setForceX_
  , setForceY_
  , setForceDistance_
  , setLinksKeyFunction_
  , putForceInSimulation_
  , lookupForceByName_
  , setAsNullForceInSimulation_
  -- Hierarchy FFI
  , RecursiveD3TreeNode_
  , cloneTreeJson_
  , linkHorizontal_
  , linkHorizontal2_
  , linkVertical_
  , linkClusterHorizontal_
  , linkClusterVertical_
  , linkRadial_
  , autoBox_
  -- Chord FFI
  , ChordLayout_
  , ChordGenerator_
  , RibbonGenerator_
  , ArcGenerator_
  , chordLayout_
  , chordLayoutWithPadAngle_
  , chordGroups_
  , chordArray_
  , ribbonGenerator_
  , arcGenerator_
  , ribbonPath_
  , arcPath_
  , setRibbonRadius_
  , setArcInnerRadius_
  , setArcOuterRadius_
  ) where

-- brings together ALL of the wrapped D3js functions and FFI / native types
-- (Simulation node-specific FFI now lives in psd3-simulation)

import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, D3Simulation_, Datum_, Index_, Selector)
import Data.Nullable (Nullable)
import Prelude (Unit)

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for Selection                ********************************************
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
type ComputeKeyFunction_ d key = d -> key
foreign import keyIsID_           :: forall d. ComputeKeyFunction_ d Index_
foreign import keyIsSourceTarget_ :: forall d. ComputeKeyFunction_ d Index_ -- used for links in simulation
foreign import swizzledLinkKey_   :: forall d. ComputeKeyFunction_ d String -- key function for swizzled links (extracts source/target IDs)
-- REVIEW the returned D3Selection_ here is the full enter, update, exit type of selection
-- which we haven't really modelled in PureScript (opaque type) but maybe it will turn out that we
-- needed to all along
-- Key function can return any type (String, Int, etc), gets coerced to Index_ for D3
foreign import d3DataWithKeyFunction_ :: forall d1 d2 key. Array d2 -> ComputeKeyFunction_ d2 key -> D3Selection_ d1 -> D3Selection_ d2
foreign import d3DataWithFunction_ :: forall d. (Datum_ -> Array Datum_) -> ComputeKeyFunction_ Datum_ Index_ -> D3Selection_ d -> D3Selection_ d

-- we'll coerce everything to this type if we can validate attr lambdas against provided data
-- ... and we'll also just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr_ :: Type
foreign import d3SetAttr_       :: forall d. String -> D3Attr_ -> D3Selection_ d -> D3Selection_ d
foreign import d3SetText_       :: forall d. D3Attr_ -> D3Selection_ d -> D3Selection_ d
foreign import d3SetProperty_   :: forall d. D3Attr_ -> D3Selection_ d -> D3Selection_ d
foreign import d3SetHTML_       :: forall d. D3Attr_ -> D3Selection_ d -> D3Selection_ d

foreign import data D3DragFunction_ :: Type
foreign import simulationDrag_ :: forall d. String -> D3Selection_ d -> D3Simulation_ -> D3DragFunction_ -> D3Selection_ d
foreign import simdrag_  :: D3DragFunction_
foreign import simdragHorizontal_ :: D3DragFunction_
foreign import disableDrag_ :: forall d. D3Selection_ d -> D3Selection_ d

foreign import selectionOn_         :: forall selection callback. selection -> String -> callback -> selection


-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js Force Module (no SimulationNode) ******************************
-- | *********************************************************************************************************************

-- links force is very special, can't (easily) manage multiple named versions.
-- consistency of naming of link force is established with top level name
foreign import linksForceName_ :: String

-- | foreign types associated with Force Layout Simulation
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

foreign import configSimulation_       :: D3Simulation_ -> SimulationVariables -> D3Simulation_
foreign import readSimulationVariables_ :: D3Simulation_ -> SimulationVariables

foreign import startSimulation_        :: D3Simulation_ -> Unit
foreign import stopSimulation_         :: D3Simulation_ -> Unit

foreign import onTick_                :: D3Simulation_ -> String -> (Unit -> Unit) -> Unit
foreign import disableTick_           :: D3Simulation_ -> String -> Unit
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
foreign import lookupForceByName_           :: D3Simulation_ -> String -> Nullable D3ForceHandle_
foreign import setAsNullForceInSimulation_  :: D3Simulation_ -> String -> D3Simulation_

-- | *********************************************************************************************************************
-- | ***************************   FFI signatures for D3js Hierarchy module  *********************************************
-- | *********************************************************************************************************************

-- Opaque type for D3 hierarchy nodes
foreign import data RecursiveD3TreeNode_ :: Type

-- Tree JSON cloning for creating copies of tree data
foreign import cloneTreeJson_           :: TreeJson_ -> TreeJson_

-- Link generators for rendering connections
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

foreign import chordLayout_            :: Array (Array Number) -> ChordLayout_
foreign import chordLayoutWithPadAngle_ :: Array (Array Number) -> Number -> ChordLayout_
foreign import chordGroups_            :: ChordLayout_ -> Array Datum_
foreign import chordArray_             :: ChordLayout_ -> Array Datum_
foreign import ribbonGenerator_        :: Unit -> RibbonGenerator_
foreign import arcGenerator_           :: Unit -> ArcGenerator_
foreign import ribbonPath_             :: RibbonGenerator_ -> Datum_ -> String
foreign import arcPath_                :: ArcGenerator_ -> Datum_ -> String
foreign import setRibbonRadius_        :: RibbonGenerator_ -> Number -> RibbonGenerator_
foreign import setArcInnerRadius_      :: ArcGenerator_ -> Number -> ArcGenerator_
foreign import setArcOuterRadius_      :: ArcGenerator_ -> Number -> ArcGenerator_

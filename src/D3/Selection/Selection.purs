module D3.Selection where

import Prelude hiding (append,join)

import D3.Attributes.Instances (Attribute, Datum, Index)
import Data.Maybe.Last (Last)
import Effect.Aff (Milliseconds)
import Unsafe.Coerce (unsafeCoerce)


type Selector = String 

data Element = Div | Svg | Circle | Line | Group | Text | Path | Rect
instance showElement :: Show Element where
  show Div    = "div"
  show Svg    = "svg"
  show Circle = "circle"
  show Line   = "line"
  show Group  = "g"
  show Text   = "text"
  show Path   = "path"
  show Rect   = "rect"
  
-- (Opaque) foreign types generated for (ie unsafeCoerce), or by (ie returned selections), D3 
foreign import data D3Data_       :: Type 
foreign import data D3Selection_  :: Type
foreign import data D3Simulation_ :: Type -- has to be declared here to avoid cycle with Simulation.purs
foreign import data D3Transition_ :: Type -- not clear yet if we need to distinguish from Selection
foreign import data D3DomNode     :: Type -- not yet used but may be needed, ex. in callbacks
foreign import data D3This        :: Type -- not yet used but may be needed, ex. in callbacks

foreign import d3SelectAllInDOM_     :: Selector    -> D3Selection_ -- NB passed D3Selection is IGNORED
foreign import d3SelectionSelectAll_ :: Selector    -> D3Selection_ -> D3Selection_
foreign import d3EnterAndAppend_     :: String      -> D3Selection_ -> D3Selection_
foreign import d3Append_             :: String      -> D3Selection_ -> D3Selection_

foreign import d3Exit_               :: D3Selection_ -> D3Selection_
foreign import d3RemoveSelection_    :: D3Selection_ -> D3Selection_

foreign import d3Data_               :: forall d. Array d -> D3Selection_ -> D3Selection_
foreign import d3KeyFunction_        :: forall d. Array d -> ComputeKeyFunction_ -> D3Selection_ -> D3Selection_

-- we'll coerce everything to this type if we can validate attr lambdas against provided data
-- ... and we'll also just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr :: Type 
-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are 
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_ :: String -> D3Selection -> ???? -- solve the ???? as needed later
foreign import d3AddTransition_ :: D3Selection_ -> Transition -> D3Selection_ -- this is the PS transition record
foreign import d3SetAttr_       :: String      -> D3Attr -> D3Selection_ -> D3Selection_
foreign import d3SetText_       :: D3Attr      -> D3Selection_ -> D3Selection_

foreign import emptyD3Data_ :: D3Data_ -- probably just null, could this be monoid too??? ie Last (Maybe D3Data_)

type D3Group      = Array D3DomNode

type D3Selection  = Last D3Selection_
type ComputeKeyFunction_ = Datum -> Index
data Keys = ComputeKey ComputeKeyFunction_ | UseDatumAsKey
-- TODO hide the "unsafeCoerce/makeProjection" in a smart constructor
type Projection = forall model. (model -> D3Data_)
identityProjection :: Projection
identityProjection model = unsafeCoerce (\d -> d)

makeProjection :: forall model model'. (model -> model') -> (model -> D3Data_)
makeProjection = unsafeCoerce

data DragBehavior = 
    DefaultDrag
  | NoDrag
  | CustomDrag (D3Selection_ -> Unit)

data SimulationDrag = SimulationDrag DragBehavior

foreign import defaultDrag_ :: D3Selection_ -> D3Selection_
foreign import disableDrag_ :: D3Selection_ -> D3Selection_

type JoinParams d r = -- the 
  { element    :: Element -- what we're going to insert in the DOM
  , key        :: Keys    -- how D3 is going to identify data so that 
  , "data"     :: Array d -- the data we're actually joining at this point
 
| r
  }
-- TODO the type parameter d here is an impediment to the meta interpreter, possible rethink ?
data Join d = Join           (JoinParams d (behaviour   :: Array Chainable))
            | JoinGeneral    (JoinParams d (behaviour   :: EnterUpdateExit)) -- what we're going to do for each set (enter, exit, update) each refresh of data
            | JoinSimulation (JoinParams d (behaviour   :: Array Chainable
                                          , onTick      :: Array Chainable
                                          , tickName    :: String
                                          , onDrag      :: SimulationDrag
                                          , simulation  :: D3Simulation_)) -- simulation joins are a bit different
newtype SelectionName = SelectionName String
derive instance eqSelectionName  :: Eq SelectionName
derive instance ordSelectionName :: Ord SelectionName

data D3_Node = D3_Node Element (Array Chainable)

instance showD3_Node :: Show D3_Node where
  show (D3_Node e cs) = "D3Node: " <> show e

-- sugar for appending with no attributes
node :: Element -> (Array Chainable) -> D3_Node
node e a = D3_Node e a

node_ :: Element -> D3_Node
node_ e = D3_Node e []

-- Transition types
type EasingTime = Number
type D3EasingFn = EasingTime -> EasingTime -- easing function maps 0-1 to 0-1 in some way with 0 -> 0, 1 -> 1
data EasingFunction = 
    DefaultCubic
  | EasingFunction D3EasingFn
  | EasingFactory (Datum -> Int -> D3Group -> D3This -> D3EasingFn)

-- TODO make this a Newtype and give it monoid instance
type Transition = { name     :: String
                  , delay    :: Milliseconds-- can also be a function, ie (\d -> f d)
                  , duration :: Milliseconds -- can also be a function, ie (\d -> f d)
                  , easing   :: EasingFunction
}

data Chainable =  AttrT Attribute
                | TextT Attribute -- we can't narrow it to String here but helper function will do that
                | TransitionT (Array Chainable) Transition -- the array is set situationally
                | RemoveT
  -- other candidates for this ADT include
                -- | WithUnit Attribute UnitType
                -- | On
                -- | Merge
                
type EnterUpdateExit = {
    enter  :: Array Chainable
  , update :: Array Chainable
  , exit   :: Array Chainable
}

enterOnly :: Array Chainable -> EnterUpdateExit
enterOnly as = { enter: as, update: [], exit: [] }

-- show functions that are used for the string version of the interpreter and also for debugging inside Selection.js
foreign import showSelectAllInDOM_          :: forall selection. Selector -> String -> selection
foreign import showSelectAll_               :: forall selection. Selector -> String -> selection -> selection
foreign import showEnterAndAppend_          :: forall selection. Element -> selection -> selection
foreign import showExit_                    :: forall selection. selection -> selection
foreign import showAddTransition_           :: forall selection. selection -> Transition -> selection 
foreign import showRemoveSelection_         :: forall selection. selection -> selection
foreign import showAppend_                  :: forall selection. Element -> selection -> selection
foreign import showKeyFunction_             :: forall selection d. Array d -> ComputeKeyFunction_ -> selection -> selection
foreign import showData_                    :: forall selection d. Array d -> selection -> selection
foreign import showSetAttr_                 :: forall selection. String -> D3Attr -> selection -> selection
foreign import showSetText_                 :: forall selection. D3Attr -> selection -> selection

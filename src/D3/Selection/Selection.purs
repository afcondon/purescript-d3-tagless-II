module D3.Selection where

import Prelude hiding (append,join)

import D3.Attributes.Instances (Attribute, Datum, Index)
import Data.Map (Map, empty)
import Data.Maybe.Last (Last)
import Effect.Aff (Milliseconds)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (Event)


type Selector = String 

data Element = Div | Svg | Circle | Line | Group | Text | Path
instance showElement :: Show Element where
  show Div    = "div"
  show Svg    = "svg"
  show Circle = "circle"
  show Line   = "line"
  show Group  = "g"
  show Text   = "text"
  show Path   = "path"
  
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

foreign import d3Data_               :: forall model. model -> (model -> D3Data_) -> D3Selection_ -> D3Selection_
foreign import d3KeyFunction_        :: forall model. model -> (model -> D3Data_)   -> ComputeKeyunction_ -> D3Selection_ -> D3Selection_

-- we'll coerce everything to this type if we can validate attr lambdas against provided data
-- ... and we'll also just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr :: Type 
-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are 
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_ :: String -> D3Selection -> ???? -- solve the ???? as needed later
foreign import d3AddTransition :: D3Selection_ -> Transition -> D3Selection_ -- this is the PS transition record
foreign import d3SetAttr_      :: String      -> D3Attr -> D3Selection_ -> D3Selection_
foreign import d3SetText_      :: D3Attr      -> D3Selection_ -> D3Selection_

foreign import emptyD3Data_ :: D3Data_ -- probably just null, could this be monoid too??? ie Last (Maybe D3Data_)

type D3Group      = Array D3DomNode

type D3Selection  = Last D3Selection_
type ComputeKeyunction_ = Datum -> Index
data Keys = ComputeKey ComputeKeyunction_ | DatumIsUnique
-- TODO hide the "unsafeCoerce/makeProjection" in a smart constructor
type Projection = forall model. (model -> D3Data_)
identityProjection :: Projection
identityProjection model = unsafeCoerce (\d -> d)

makeProjection :: forall model model'. (model -> model') -> (model -> D3Data_)
makeProjection = unsafeCoerce

data DragBehavior = DefaultDrag | NoDrag | CustomDrag (D3Selection_ -> Unit)

type JoinParams model r = 
  { element    :: Element           -- what we're going to insert in the DOM
  , key        :: Keys              -- how D3 is going to identify data so that 
  , hook       :: D3Selection_
  , projection :: model -> D3Data_  -- the join might operate on some subset or transformation of the data}
  | r
  }
data Join model = Join           (JoinParams model (behaviour   :: Array Chainable))
                | JoinGeneral    (JoinParams model (behaviour   :: EnterUpdateExit)) -- what we're going to do for each set (enter, exit, update) each refresh of data
                | JoinSimulation (JoinParams model (behaviour   :: Array Chainable
                                                   , onTick     :: Array Chainable
                                                   , tickName   :: String
                                                   , onDrag     :: DragBehavior
                                                   , simulation :: D3Simulation_)) -- simulation joins are a bit different
newtype SelectionName = SelectionName String
derive instance eqSelectionName  :: Eq SelectionName
derive instance ordSelectionName :: Ord SelectionName
data D3State model = D3State {
    active           :: D3Selection
  , namedSelections  :: Map SelectionName D3Selection_
  , namedTransitions :: Map SelectionName D3Selection_
  , namedJoins       :: Map SelectionName (Join model)
}

makeD3State' :: ∀ model. D3State model
makeD3State' = D3State { active: mempty, namedSelections: empty, namedTransitions: empty, namedJoins: empty }

makeD3State :: ∀ model. D3Selection -> D3State model
makeD3State selection = D3State { active: selection, namedSelections: empty, namedTransitions: empty, namedJoins: empty }

data D3_Node = D3_Node Element (Array Chainable)

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
                -- | On
                -- | Merge
                
type EnterUpdateExit = {
    enter  :: Array Chainable
  , update :: Array Chainable
  , exit   :: Array Chainable
}

enterOnly :: Array Chainable -> EnterUpdateExit
enterOnly as = { enter: as, update: [], exit: [] }


-- stuff related to zoom functionality
type ZoomConfig = {
    extent    :: ZoomExtent
  , scale     :: ScaleExtent
  , qualifier :: String -- zoom.foo
  -- this is the full list of values and their defaults
-- filter = defaultFilter,
-- extent = defaultExtent,
-- constrain = defaultConstrain,
-- wheelDelta = defaultWheelDelta,
-- touchable = defaultTouchable,
-- scaleExtent = [0, Infinity],
-- translateExtent = [[-Infinity, -Infinity], [Infinity, Infinity]],
-- duration = 250,
-- interpolate = interpolateZoom,
-- listeners = dispatch("start", "zoom", "end"),
-- touchstarting,
-- touchfirst,
-- touchending,
-- touchDelay = 500,
-- wheelDelay = 150,
-- clickDistance2 = 0,
-- tapDistance = 10;
}
type ZoomConfig_ = {
    extent      :: Array (Array Number)
  , scaleExtent :: Array Int
  , qualifier   :: String
}
type ZoomConfigDefault_ = {
    scaleExtent :: Array Int
  , qualifier   :: String
}
foreign import data ZoomBehavior_ :: Type  -- the zoom behavior, provided to Event Handler
data ScaleExtent   = ScaleExtent Int Int
zoomRange = ScaleExtent
zoomExtent = ZoomExtent
data ZoomExtent    = DefaultZoomExtent 
                   | ZoomExtent { top :: Number, left :: Number, bottom :: Number, right :: Number }
                  --  | ExtentFunction (Datum -> Array (Array Number))
data ZoomType      = ZoomStart | ZoomEnd | ZoomZoom
data ZoomTransform = ZoomTransform { k :: Number, tx :: Number, ty :: Number }
type ZoomEvent     = {
    target      :: ZoomBehavior_
  , type        :: ZoomType
  , transform   :: ZoomTransform
  , sourceEvent :: Event
}

foreign import d3AttachZoom_              :: D3Selection_ -> ZoomConfig_        -> D3Selection_
foreign import d3AttachZoomDefaultExtent_ :: D3Selection_ -> ZoomConfigDefault_ -> D3Selection_

attachZoom :: D3Selection_ -> ZoomConfig -> D3Selection_
attachZoom selection config = do
  let 
    (ScaleExtent smallest largest) = config.scale
  
  -- sticking to the rules of no ADT's on the JS side we case on the ZoomExtent here
  case config.extent of
    DefaultZoomExtent -> 
      d3AttachZoomDefaultExtent_ selection {
        scaleExtent: [ smallest, largest ]
      , qualifier  : config.qualifier
      } 

    (ZoomExtent ze)   -> do
      d3AttachZoom_ selection { 
        extent     : [ [ ze.left, ze.top ], [ ze.right, ze.bottom ] ]
      , scaleExtent: [ smallest, largest ]
      , qualifier  : config.qualifier
      }

    --  (ExtentFunction f) -> selection -- TODO write this case
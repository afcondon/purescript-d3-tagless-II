module D3.Selection where

import D3.Attributes.Instances (Attribute, Listener_, attrLabel)
import D3.Data.Types (D3Selection_, D3Simulation_, Element, MouseEvent, Transition)
import D3.FFI (ComputeKeyFunction_)
import Data.Maybe.Last (Last)
import Prelude (class Eq, class Ord, class Show, Unit, show, (<>))

type D3Selection  = Last D3Selection_
data Keys = ComputeKey ComputeKeyFunction_ | UseDatumAsKey

data DragBehavior = 
    DefaultDrag
  | NoDrag
  | CustomDrag (D3Selection_ -> Unit)

data Behavior = Tick { chain :: Array Chainable, name :: String, simulation  :: D3Simulation_}
              | Drag DragBehavior
      -- TODO | Zoom ZoomBehavior

type JoinParams d r = -- the 
  { element    :: Element -- what we're going to insert in the DOM
  , key        :: Keys    -- how D3 is going to identify data so that 
  , "data"     :: Array d -- the data we're actually joining at this point
| r
  }
-- TODO the type parameter d here is an impediment to the meta interpreter, possible rethink ?
data Join d = Join           (JoinParams d (behaviour   :: Array Chainable))
            | JoinGeneral    (JoinParams d (behaviour   :: EnterUpdateExit)) -- what we're going to do for each set (enter, exit, update) each refresh of data

-- TODO presumably the D3Simulation_ is type parameter to allow for 

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


data Chainable =  AttrT Attribute
                | TextT Attribute -- we can't narrow it to String here but helper function will do that
                | TransitionT (Array Chainable) Transition -- the array is set situationally
                | RemoveT
                | OnT MouseEvent Listener_
  -- other candidates for this ADT include
                -- | WithUnit Attribute UnitType
                -- | Merge
                
type EnterUpdateExit = {
    enter  :: Array Chainable
  , update :: Array Chainable
  , exit   :: Array Chainable
}

enterOnly :: Array Chainable -> EnterUpdateExit
enterOnly as = { enter: as, update: [], exit: [] }

instance showChainable :: Show Chainable where
  show (AttrT attr)      = attrLabel attr
  show (TextT _)         = "text"
  show (TransitionT _ _) = ""
  show RemoveT           = ""
  show (OnT event _)     = show event
module D3.Selection where

import D3.Attributes.Instances (Attribute, Listener_, attrLabel)
import D3.Data.Types (D3Selection_, D3Simulation_, Datum_, Element, MouseEvent, Transition)
import D3.FFI (ComputeKeyFunction_)
import D3.Zoom (ZoomConfig)
import Data.Maybe.Last (Last)
import Prelude (class Eq, class Ord, class Show, Unit, show, (<>))

type D3Selection  = Last D3Selection_
data Keys = ComputeKey ComputeKeyFunction_ | UseDatumAsKey

data DragBehavior = 
    DefaultDrag
  | NoDrag
  | CustomDrag (D3Selection_ -> Unit)

data Behavior = Drag DragBehavior
              | Zoom ZoomConfig 

type JoinParams d r = -- the 
  { element    :: Element -- what we're going to insert in the DOM
  , key        :: Keys    -- how D3 is going to identify data so that 
  , "data"     :: Array d -- the data we're actually joining at this point
| r
  }
-- TODO the type parameter d here is an impediment to the meta interpreter, possible rethink ?
data Join d = Join           (JoinParams d (behaviour   :: Array ChainableS))
            | JoinGeneral    (JoinParams d (behaviour   :: EnterUpdateExit)) -- what we're going to do for each set (enter, exit, update) each refresh of data

-- TODO presumably the D3Simulation_ is type parameter to allow for 

newtype SelectionName = SelectionName String
derive instance eqSelectionName  :: Eq SelectionName
derive instance ordSelectionName :: Ord SelectionName

data D3_Node = D3_Node Element (Array ChainableS)

instance showD3_Node :: Show D3_Node where
  show (D3_Node e cs) = "D3Node: " <> show e

-- sugar for appending with no attributes
node :: Element -> (Array ChainableS) -> D3_Node
node e a = D3_Node e a

node_ :: Element -> D3_Node
node_ e = D3_Node e []

data OrderingAttribute = Order | Sort (Datum_ -> Datum_ -> Int) | Raise | Lower

data ChainableS =  
    AttrT Attribute
  | TextT Attribute -- we can't narrow it to String here but helper function will do that
  | HTMLT Attribute -- we can't narrow it to String here but helper function will do that
  | PropertyT Attribute -- this might motivate adding a Boolean flavor of Attribute, eg for checkbox "checked"

  | OrderingT OrderingAttribute

  | TransitionT (Array ChainableS) Transition -- the array is set situationally

  | RemoveT

  | OnT MouseEvent Listener_

  -- other candidates for this ADT include
                -- | WithUnit Attribute UnitType
                -- | Merge
                
type EnterUpdateExit = {
    enter  :: Array ChainableS
  , update :: Array ChainableS
  , exit   :: Array ChainableS
}

enterOnly :: Array ChainableS -> EnterUpdateExit
enterOnly as = { enter: as, update: [], exit: [] }

instance showChainableS :: Show ChainableS where
  show (AttrT attr)      = "chainable: attr " <> attrLabel attr
  show (TextT _)         = "chainable: text"
  show (HTMLT attr)      = "chainable: html" <> attrLabel attr
  show (PropertyT attr)  = "chainable: property" <> attrLabel attr

  show (TransitionT _ _) = "chainable: transition"

  show RemoveT           = "chainable: remove"
  -- show (ForceT _)        = "chainable: force attr"
  show (OnT event _)     = show event

  show (OrderingT attr)  = "chainable: ordering" <> show attr


instance showOrderingAttribute :: Show OrderingAttribute where
  show Order    = "Order"
  show Raise    = "Raise"
  show Lower    = "Lower"
  show (Sort _) = "Sort"


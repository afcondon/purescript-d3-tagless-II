module D3.Selection where

import D3.FFI

import D3.Attributes.Instances (Attribute(..), Listener_, attrLabel, unbox)
import D3.Data.Types (D3Selection_, Datum_, Element, MouseEvent, Transition)
import D3.Zoom (ZoomConfig)
import Data.Array (foldl)
-- import Data.Maybe.Last (Last)
import Prelude (class Eq, class Ord, class Show, Unit, show, (<>))

-- type D3Selection  = Last D3Selection_

data DragBehavior = 
    DefaultDrag
  | NoDrag
  | CustomDrag (D3Selection_ -> Unit)

data Behavior = Drag DragBehavior
              | Zoom ZoomConfig 

data Join d = Join Element (Array d) (Array ChainableS)
            | UpdateJoin Element (Array d) EnterUpdateExit
            | JoinWithKeyFunction Element (Array d) (Array ChainableS) ComputeKeyFunction_
            | UpdateJoinWithKeyFunction Element (Array d) EnterUpdateExit ComputeKeyFunction_

computeKeyWith :: forall d.  ComputeKeyFunction_ -> Join d -> Join d
computeKeyWith keyFunction = 
  case _ of
    Join e ds cs                        -> JoinWithKeyFunction e ds cs keyFunction
    UpdateJoin e ds cs                  -> UpdateJoinWithKeyFunction e ds cs keyFunction
    JoinWithKeyFunction e ds cs _       -> JoinWithKeyFunction e ds cs keyFunction
    UpdateJoinWithKeyFunction e ds cs _ -> UpdateJoinWithKeyFunction e ds cs keyFunction

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


applyChainableSD3 :: D3Selection_ -> ChainableS -> D3Selection_
applyChainableSD3 selection_ (AttrT (ToAttribute label attr)) = 
  d3SetAttr_ label (unbox attr) selection_

-- NB only protection against non-text attribute for Text field is in the helper function
-- and similarly for Property and HTML
applyChainableSD3 selection_ (TextT (ToAttribute label attr))     = d3SetText_    (unbox attr) selection_ 
applyChainableSD3 selection_ (PropertyT (ToAttribute label attr)) = d3SetProperty_ (unbox attr) selection_ 
applyChainableSD3 selection_ (HTMLT (ToAttribute label attr))     = d3SetHTML_     (unbox attr) selection_ 

-- NB this remove call will have no effect on elements with active or pending transitions
-- and this gives rise to very counter-intuitive misbehaviour as subsequent enters clash with 
-- elements that should have been removed
-- also NB "selection" here will often be a "transition" but this distinction won't matter (i think)
-- TODO remove is not like other chainables, in fact it's not chainable since it returns unit
applyChainableSD3 selection_ RemoveT = do
  let _ = d3RemoveSelection_ selection_ 
  selection_

-- for transition in D3 we must use .call(selection, transition) so that chain continues
-- in this interpreter it's enought to just return the selection instead of the transition
applyChainableSD3 selection_ (TransitionT chain transition) = do
  let tHandler = d3AddTransition_ selection_ transition
      _        = foldl applyChainableSD3 tHandler chain
  selection_ -- NB we return selection, not transition

applyChainableSD3 selection_ (OnT event listener) = selectionOn_ selection_ (show event) listener

applyChainableSD3 selection_ (OrderingT oAttr) =
  case oAttr of
    Order          -> d3OrderSelection_ selection_
    (Sort compare) -> d3SortSelection_ selection_ compare
    Raise          -> d3RaiseSelection_ selection_
    Lower          -> d3LowerSelection_ selection_

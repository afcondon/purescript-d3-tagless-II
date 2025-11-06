module PSD3.Internal.Selection.Types where

import PSD3.Internal.FFI

import Data.Array (foldl)
import PSD3.Internal.Attributes.Instances (AttributeSetter(..), Label, Listener_, EffectfulListener_, attributeLabel, unboxAttr)
import PSD3.Internal.Types (D3Selection_, Datum_, MouseEvent, Transition)
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomConfig, ZoomExtent(..))
import Prelude (class Eq, class Ord, class Show, show, (<>))

-- type D3Selection  = Last D3Selection_

data DragBehavior = 
    DefaultDrag
  | NoDrag
  | CustomDrag Label D3DragFunction_

data Behavior selection = Drag DragBehavior
                        | Zoom (ZoomConfig selection)

-- | Default zoom configuration
-- | Allows zooming from 10% to 4x the original size
defaultZoomConfig :: forall selection. Number -> Number -> selection -> ZoomConfig selection
defaultZoomConfig w h target =
  { extent : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
  , scale  : ScaleExtent 0.1 4.0 
  , name   : "default"
  , target
  }

newtype SelectionName = SelectionName String
derive instance eqSelectionName  :: Eq SelectionName
derive instance ordSelectionName :: Ord SelectionName

-- data D3_Node = D3_Node Element (Array SelectionAttribute)

-- instance showD3_Node :: Show D3_Node where
--   show (D3_Node e _) = "D3Node: " <> show e

-- -- sugar for appending WITH attributes
-- node :: Element -> (Array SelectionAttribute) -> D3_Node
-- node e a = D3_Node e a

-- -- sugar for appending with NO attributes
-- node_ :: Element -> D3_Node
-- node_ e = D3_Node e []

data OrderingAttribute d = Order | Sort (d -> d -> Int) | Raise | Lower

data SelectionAttribute d =
    AttrT AttributeSetter
  | TextT AttributeSetter -- we can't narrow it to String here but helper function will do that
  | HTMLT AttributeSetter -- we can't narrow it to String here but helper function will do that
  | PropertyT AttributeSetter -- this might motivate adding a Boolean flavor of Attribute, eg for checkbox "checked"

  | OrderingT (OrderingAttribute d)

  | TransitionT (Array (SelectionAttribute d)) Transition -- the array is set situationally

  | RemoveT

  | OnT MouseEvent Listener_
  | OnT' MouseEvent EffectfulListener_
                
instance (Show d) => Show (SelectionAttribute d) where
  show (AttrT attr)      = "chainable: attr " <> attributeLabel attr
  show (TextT _)         = "chainable: text"
  show (HTMLT attr)      = "chainable: html" <> attributeLabel attr
  show (PropertyT attr)  = "chainable: property" <> attributeLabel attr

  show (TransitionT _ _) = "chainable: transition"

  show RemoveT           = "chainable: remove"
  -- show (ForceT _)        = "chainable: force attr"
  show (OnT event _)     = show event
  show (OnT' event _)     = show event

  show (OrderingT attr)  = "chainable: ordering" <> show attr


instance showOrderingAttribute :: Show (OrderingAttribute d) where
  show Order    = "Order"
  show Raise    = "Raise"
  show Lower    = "Lower"
  show (Sort _) = "Sort"


applySelectionAttributeD3 :: forall d. D3Selection_ d -> SelectionAttribute d -> D3Selection_ d
applySelectionAttributeD3 selection_ (AttrT (AttributeSetter label attr)) = 
  d3SetAttr_ label (unboxAttr attr) selection_

-- NB only protection against non-text attribute for Text field is in the helper function
-- and similarly for Property and HTML
applySelectionAttributeD3 selection_ (TextT (AttributeSetter _ attr))     = d3SetText_    (unboxAttr attr) selection_ 
applySelectionAttributeD3 selection_ (PropertyT (AttributeSetter _ attr)) = d3SetProperty_ (unboxAttr attr) selection_ 
applySelectionAttributeD3 selection_ (HTMLT (AttributeSetter _ attr))     = d3SetHTML_     (unboxAttr attr) selection_ 

-- NB this remove call will have no effect on elements with active or pending transitions
-- and this gives rise to very counter-intuitive misbehaviour as subsequent enters clash with 
-- elements that should have been removed
-- also NB "selection" here will often be a "transition" but this distinction won't matter (i think)
-- the remove function returns the removed selection which allows it to be cached amongst other things
applySelectionAttributeD3 selection_ RemoveT = do
  let removed_ = d3RemoveSelection_ selection_ 
  removed_

-- for transition in D3 we must use .call(selection, transition) so that chain continues
-- in this interpreter it's enought to just return the selection instead of the transition
applySelectionAttributeD3 selection_ (TransitionT chain transition) = do
  let tHandler = d3AddTransition_ selection_ transition
      _        = foldl applySelectionAttributeD3 tHandler chain
  selection_ -- NB we return selection, not transition

applySelectionAttributeD3 selection_ (OnT event listener) = selectionOn_ selection_ (show event) listener
applySelectionAttributeD3 selection_ (OnT' event listener) = selectionOn_ selection_ (show event) listener

applySelectionAttributeD3 selection_ (OrderingT oAttr) =
  case oAttr of
    Order          -> d3OrderSelection_ selection_
    (Sort compare) -> d3SortSelection_  selection_ compare
    Raise          -> d3RaiseSelection_ selection_
    Lower          -> d3LowerSelection_ selection_

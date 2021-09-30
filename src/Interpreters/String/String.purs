module D3Tagless.Capabilities.String where

import D3.Selection

import Control.Monad.State (class MonadState, StateT, modify_, runStateT)
import D3.Attributes.Instances (AttributeSetter(..), unboxAttr)
import D3.Data.Types (D3Selection_, Element, Selector, Transition)
import D3.FFI (ComputeKeyFunction_, D3Attr)
import D3Tagless.Capabilities (class SelectionM, mergeSelections, selectUnder)
import Data.Array (foldl)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, discard, pure, show, unit, (<>))

-- TODO s/Effect/Identity
newtype D3PrinterM a = D3PrinterM (StateT String Effect a)

runPrinter :: D3PrinterM String -> String -> Effect (Tuple String String)
runPrinter (D3PrinterM state) initialString = runStateT state initialString

derive newtype instance functorD3PrinterM     :: Functor           D3PrinterM
derive newtype instance applyD3PrinterM       :: Apply             D3PrinterM
derive newtype instance applicativeD3PrinterM :: Applicative       D3PrinterM
derive newtype instance bindD3PrinterM        :: Bind              D3PrinterM
derive newtype instance monadD3PrinterM       :: Monad             D3PrinterM
derive newtype instance monadStateD3PrinterM  :: MonadState String D3PrinterM 
derive newtype instance monadEffD3PrinterM    :: MonadEffect       D3PrinterM

instance d3Tagless :: SelectionM String D3PrinterM where
  attach selector = do
    modify_ (\s -> s <> "\nattaching to " <> selector <> " in DOM\n" ) -- TODO good case for (+=) ?
    pure "attach"

  selectUnder selection selector = do
    modify_ (\s -> s <> "\nsub-selection " <> selector <> "\n" )
    pure "attach"

  appendTo selection element attributes = do
    let attributeString = foldl applySelectionAttributeString selection attributes
    modify_ (\s -> s <> "\nappending "    <> show element <> " to " <> selection <> "\n" <> attributeString)
    pure "append"

  filterSelection selection selector = do
    modify_ (\s -> s <> "\nfiltering selection using " <> show selector <> "\n" )
    pure "filter"

  mergeSelections a b = do
    modify_ (\s -> s <> "merging selections" <> "\n" )
    pure "merge"

  setAttributes selection attributes = do
    let attributeString = foldl applySelectionAttributeString selection attributes
    modify_ (\s -> s <> "\nmodifying " <> selection <> "\n" <> attributeString)
    pure unit

  on selection (Drag drag) = do
    modify_ (\s -> s <> "\nadding drag behavior to " <> selection)
    pure unit
  on selection (Zoom zoom) = do
    modify_ (\s -> s <> "\nadding drag behavior to " <> selection)
    pure unit

  openSelection selection selector = do
    modify_ (\s -> s <> "\nopening a selection using " <> show selector)
    pure "openSelection"

  simpleJoin selection e ds k = do
    modify_ (\s -> s <> "\nentering a "   <> show e <> " for each datum" )
    pure "join"

  updateJoin selection e ds k = do
    modify_ (\s -> s <> "\nentering a "   <> show e <> " for each datum" )
    pure { enter: "enter", exit: "exit", update: "update" }
      


applySelectionAttributeString :: String -> SelectionAttribute -> String
applySelectionAttributeString selection  = 
  case _ of 
    (AttrT (AttributeSetter label attr))     -> showSetAttr_ label (unboxAttr attr) selection
    (TextT (AttributeSetter label text))     -> showSetText_       (unboxAttr text) selection
    (PropertyT (AttributeSetter label text)) -> showSetProperty_   (unboxAttr text) selection
    (HTMLT (AttributeSetter label text))     -> showSetHTML_       (unboxAttr text) selection

    RemoveT       -> showRemoveSelection_ selection

    (OrderingT o) -> showSetOrdering_ (show o) selection

    (TransitionT chain transition) -> do 
      let tString = showAddTransition_ selection transition
      foldl applySelectionAttributeString tString chain
    -- (ForceT (AttributeSetter label attr)) -> showSetAttr_ label (unboxAttr attr) selection -- might need custom one for forces

    (OnT event listener) -> do
      show "event handler for " <> show event <> " has been set"
    (OnT' event listener) -> do
      show "effectful event handler for " <> show event <> " has been set"

-- REVIEW surely these must all be s/D3Selection/String/ here ???
foreign import showSelectAllInDOM_  :: forall selection. Selector D3Selection_ -> String -> selection
foreign import showSelectAll_       :: forall selection. Selector D3Selection_ -> String -> selection -> selection
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

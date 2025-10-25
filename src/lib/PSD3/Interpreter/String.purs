module D3Tagless.Capabilities.String where

import D3.Selection

import Control.Monad.State (class MonadState, StateT, modify_, runStateT)
import D3.Attributes.Instances (AttributeSetter(..), unboxAttr)
import D3.Data.Types (D3Selection_, Element, Selector, Transition)
import D3.FFI (ComputeKeyFunction_, D3Attr)
import D3Tagless.Capabilities (class SelectionM)
import Data.Array (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), indexOf, drop)
import Data.String.CodeUnits (length)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, discard, pure, show, unit, ($), (+), (<>))

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
    let code = showSelectAllInDOM_ selector
    modify_ (\s -> s <> code)
    pure code

  selectUnder selection selector = do
    let code = showSelectAll_ selector selection
    modify_ (\s -> s <> "\n  ." <> code)
    pure code

  appendTo selection element attributes = do
    let appendCode = showAppend_ element selection
        attributeString = foldl applySelectionAttributeString appendCode attributes
    modify_ (\s -> s <> "\n\nconst /* TODO: varName */ = " <> appendCode <> attributeString <> ";")
    pure appendCode

  filterSelection selection selector = do
    let code = selection <> ".filter(" <> show selector <> ")"
    modify_ (\s -> s <> "\n  ." <> code)
    pure code

  mergeSelections a b = do
    let code = a <> ".merge(" <> b <> ")"
    modify_ (\s -> s <> "\n  ." <> code)
    pure code

  setAttributes selection attributes = do
    let attributeString = foldl (\acc attr -> acc <> "\n  ." <> applySelectionAttributeString selection attr) "" attributes
    modify_ (\s -> s <> attributeString)
    pure unit

  on selection (Drag drag) = do
    modify_ (\s -> s <> "\n  .call(d3.drag())")
    pure unit
  on selection (Zoom zoom) = do
    modify_ (\s -> s <> "\n  .call(d3.zoom())")
    pure unit

  openSelection selection selector = do
    let code = showSelectAll_ selector selection
    modify_ (\s -> s <> "\n\nconst /* TODO: varName */ = " <> code <> ";")
    pure code

  simpleJoin selection e ds k = do
    let dataCode = showData_ ds selection
        joinCode = dataCode <> "\n  .join(" <> show e <> ")"
    modify_ (\s -> s <> "\n\nconst /* TODO: varName */ = " <> joinCode <> ";")
    pure joinCode

  updateJoin selection e ds k = do
    let dataCode = showData_ ds selection
        enterCode = dataCode <> "\n  .enter().append(" <> show e <> ")"
        exitCode = dataCode <> "\n  .exit().remove()"
    modify_ (\s -> s <> "\n\n// Update pattern\nconst update = " <> dataCode <> ";\nconst enter = " <> enterCode <> ";\nconst exit = " <> exitCode <> ";")
    pure { enter: enterCode, exit: exitCode, update: dataCode }
      


applySelectionAttributeString :: String -> SelectionAttribute -> String
applySelectionAttributeString selection  =
  case _ of
    (AttrT (AttributeSetter label attr))     -> trimSelectionPrefix $ showSetAttr_ label (unboxAttr attr) selection
    (TextT (AttributeSetter label text))     -> trimSelectionPrefix $ showSetText_       (unboxAttr text) selection
    (PropertyT (AttributeSetter label text)) -> trimSelectionPrefix $ showSetProperty_   (unboxAttr text) selection
    (HTMLT (AttributeSetter label text))     -> trimSelectionPrefix $ showSetHTML_       (unboxAttr text) selection

    RemoveT       -> trimSelectionPrefix $ showRemoveSelection_ selection

    (OrderingT o) -> trimSelectionPrefix $ showSetOrdering_ (show o) selection

    (TransitionT chain transition) -> do
      let tString = showAddTransition_ selection transition
          trimmed = trimSelectionPrefix tString
      foldl (\acc attr -> acc <> "\n  ." <> trimSelectionPrefix (applySelectionAttributeString tString attr)) trimmed chain
    -- (ForceT (AttributeSetter label attr)) -> showSetAttr_ label (unboxAttr attr) selection -- might need custom one for forces

    (OnT event _listener) -> do
      "on(" <> show event <> ", function(d) { /* event handler */ })"
    (OnT' event _listener) -> do
      "on(" <> show event <> ", function(d) { /* event handler */ })"

-- Helper to remove the selection prefix from generated code for chaining
trimSelectionPrefix :: String -> String
trimSelectionPrefix str =
  case indexOf (Pattern ".") str of
    Just idx -> drop (idx + 1) str
    Nothing -> str

-- FFI functions that generate JavaScript code strings
foreign import showSelectAllInDOM_  :: Selector D3Selection_ -> String
foreign import showSelectAll_       :: Selector D3Selection_ -> String -> String
foreign import showEnterAndAppend_  :: Element -> String -> String
foreign import showExit_            :: String -> String
foreign import showAddTransition_   :: String -> Transition -> String
foreign import showRemoveSelection_ :: String -> String
foreign import showAppend_          :: Element -> String -> String
foreign import showKeyFunction_     :: forall d. Array d -> ComputeKeyFunction_ -> String -> String
foreign import showData_            :: forall d. Array d -> String -> String
foreign import showSetAttr_         :: String -> D3Attr -> String -> String
foreign import showSetText_         :: D3Attr -> String -> String
foreign import showSetHTML_         :: D3Attr -> String -> String
foreign import showSetProperty_     :: D3Attr -> String -> String
foreign import showSetOrdering_     :: String -> String -> String

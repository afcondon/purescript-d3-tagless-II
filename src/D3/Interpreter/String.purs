module D3.Interpreter.String where

import D3.Selection

import Control.Monad.State (class MonadState, StateT, modify_, runStateT)
import D3.Attributes.Instances (class ToAttr, Attribute(..), unbox)
import D3.FFI (showAddTransition_, showRemoveSelection_, showSetAttr_, showSetHTML_, showSetOrdering_, showSetProperty_, showSetText_)
import D3.Interpreter (class D3InterpreterM)
import Data.Array (foldl)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, discard, pure, show, (<>))

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

instance d3Tagless :: D3InterpreterM String D3PrinterM where
  attach selector = do
    modify_ (\s -> s <> "\nattaching to " <> selector <> " in DOM" )
    pure "attach"
  append selection (D3_Node element attributes) = do
    let attributeString = foldl applyChainableSString selection attributes
    modify_ (\s -> s <> "\nappending "    <> show element <> " to " <> selection <> "\n" <> attributeString)
    pure "append"
  filter selection selector = do
    modify_ (\s -> s <> "\nfiltering selection using " <> show selector)
    pure "filter"
  modify selection attributes = do
    let attributeString = foldl applyChainableSString selection attributes
    modify_ (\s -> s <> "\nmodifying " <> selection <> "\n" <> attributeString)
    pure "modify"
  join selection (Join j) = do
    let attributeString = foldl applyChainableSString selection j.behaviour
    modify_ (\s -> s <> "\nentering a "   <> show j.element <> " for each datum" )
    pure "join"
  join selection (JoinGeneral j) = do
    let enterAttributes  = foldl applyChainableSString selection j.behaviour.enter
        exitAttributes   = foldl applyChainableSString selection j.behaviour.exit
        updateAttributes = foldl applyChainableSString selection j.behaviour.update
    modify_ (\s -> s <> "\n\tenter behaviour: " <> enterAttributes)
    modify_ (\s -> s <> "\n\tupdate behaviour: " <> updateAttributes)
    modify_ (\s -> s <> "\n\texit behaviour: " <> exitAttributes)
    pure "join"
  on selection (Drag drag) = do
    modify_ (\s -> s <> "\nadding drag behavior to " <> selection)
    pure "addDrag"
  on selection (Zoom zoom) = do
    modify_ (\s -> s <> "\nadding drag behavior to " <> selection)
    pure "addZoom"
  on selection (Tick tick) = do
    modify_ (\s -> s <> "\nadding tick behavior to " <> selection)
    pure "addTick"


applyChainableSString :: String -> ChainableS -> String
applyChainableSString selection  = 
  case _ of 
    (AttrT (ToAttribute label attr))     -> showSetAttr_ label (unbox attr) selection
    (TextT (ToAttribute label text))     -> showSetText_       (unbox text) selection
    (PropertyT (ToAttribute label text)) -> showSetProperty_   (unbox text) selection
    (HTMLT (ToAttribute label text))     -> showSetHTML_       (unbox text) selection

    RemoveT       -> showRemoveSelection_ selection

    (OrderingT o) -> showSetOrdering_ (show o) selection

    (TransitionT chain transition) -> do 
      let tString = showAddTransition_ selection transition
      foldl applyChainableSString tString chain
    -- (ForceT (ToAttribute label attr)) -> showSetAttr_ label (unbox attr) selection -- might need custom one for forces

    (OnT event listener) -> do
      show "event handler for " <> show event <> " has been set"


module D3Tagless.Capabilities.String where

import D3.Selection

import Control.Monad.State (class MonadState, StateT, modify_, runStateT)
import D3.Attributes.Instances (class ToAttr, AttributeSetter(..), unboxAttr)
import D3.FFI (showAddTransition_, showRemoveSelection_, showSetAttr_, showSetHTML_, showSetOrdering_, showSetProperty_, showSetText_)
import D3Tagless.Capabilities (class SelectionM)
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
    modify_ (\s -> s <> "\nattaching to " <> selector <> " in DOM" )
    pure "attach"

  appendElement selection (D3_Node element attributes) = do
    let attributeString = foldl applyChainableSString selection attributes
    modify_ (\s -> s <> "\nappending "    <> show element <> " to " <> selection <> "\n" <> attributeString)
    pure "append"

  filterSelection selection selector = do
    modify_ (\s -> s <> "\nfiltering selection using " <> show selector)
    pure "filter"

  modifySelection selection attributes = do
    let attributeString = foldl applyChainableSString selection attributes
    modify_ (\s -> s <> "\nmodifying " <> selection <> "\n" <> attributeString)
    pure unit

  join selection (Join e ds cs) = do
    let attributeString = foldl applyChainableSString selection cs
    modify_ (\s -> s <> "\nentering a "   <> show e <> " for each datum" )
    pure "join"
  join selection (UpdateJoin e ds eux) = do
    let enterAttributes  = foldl applyChainableSString selection eux.enter
        exitAttributes   = foldl applyChainableSString selection eux.exit
        updateAttributes = foldl applyChainableSString selection eux.update
    modify_ (\s -> s <> "\n\tenter behaviour: " <> enterAttributes)
    modify_ (\s -> s <> "\n\tupdate behaviour: " <> updateAttributes)
    modify_ (\s -> s <> "\n\texit behaviour: " <> exitAttributes)
    pure "join"
  join selection (JoinWithKeyFunction e ds cs fn) = do
    let attributeString = foldl applyChainableSString selection cs
    modify_ (\s -> s <> "\nentering a "   <> show e <> " for each datum" )
    pure "join"
  join selection (UpdateJoinWithKeyFunction e ds eux fn) = do
    let enterAttributes  = foldl applyChainableSString selection eux.enter
        exitAttributes   = foldl applyChainableSString selection eux.exit
        updateAttributes = foldl applyChainableSString selection eux.update
    modify_ (\s -> s <> "\n\tenter behaviour: " <> enterAttributes)
    modify_ (\s -> s <> "\n\tupdate behaviour: " <> updateAttributes)
    modify_ (\s -> s <> "\n\texit behaviour: " <> exitAttributes)
    pure "join"
      
  on selection (Drag drag) = do
    modify_ (\s -> s <> "\nadding drag behavior to " <> selection)
    pure unit
  on selection (Zoom zoom) = do
    modify_ (\s -> s <> "\nadding drag behavior to " <> selection)
    pure unit


applyChainableSString :: String -> ChainableS -> String
applyChainableSString selection  = 
  case _ of 
    (AttrT (AttributeSetter label attr))     -> showSetAttr_ label (unboxAttr attr) selection
    (TextT (AttributeSetter label text))     -> showSetText_       (unboxAttr text) selection
    (PropertyT (AttributeSetter label text)) -> showSetProperty_   (unboxAttr text) selection
    (HTMLT (AttributeSetter label text))     -> showSetHTML_       (unboxAttr text) selection

    RemoveT       -> showRemoveSelection_ selection

    (OrderingT o) -> showSetOrdering_ (show o) selection

    (TransitionT chain transition) -> do 
      let tString = showAddTransition_ selection transition
      foldl applyChainableSString tString chain
    -- (ForceT (AttributeSetter label attr)) -> showSetAttr_ label (unboxAttr attr) selection -- might need custom one for forces

    (OnT event listener) -> do
      show "event handler for " <> show event <> " has been set"


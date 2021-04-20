module D3.Interpreter.String where

import D3.Interpreter (class D3Tagless)
import D3.Selection (Chainable(..), D3_Node(..), Join(..), showAddTransition_, showRemoveSelection_, showSetAttr_, showSetText_)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, discard, pure, show, (<>))

import Control.Monad.State (class MonadState, StateT, modify_, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import Data.Array (foldl)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)

newtype D3PrinterM a = D3PrinterM (StateT String Effect a) -- TODO s/Effect/Identity

runPrinter :: D3PrinterM String -> String -> Effect (Tuple String String) -- TODO s/Effect/Identity
runPrinter (D3PrinterM state) initialString = runStateT state initialString

derive newtype instance functorD3PrinterM     :: Functor           D3PrinterM
derive newtype instance applyD3PrinterM       :: Apply             D3PrinterM
derive newtype instance applicativeD3PrinterM :: Applicative       D3PrinterM
derive newtype instance bindD3PrinterM        :: Bind              D3PrinterM
derive newtype instance monadD3PrinterM       :: Monad             D3PrinterM
derive newtype instance monadStateD3PrinterM  :: MonadState String D3PrinterM 
derive newtype instance monadEffD3PrinterM    :: MonadEffect       D3PrinterM

instance d3Tagless :: D3Tagless String D3PrinterM where
  attach selector = do
    modify_ (\s -> s <> "\nattaching to " <> selector <> " in DOM" )
    pure "attach"
  append selection (D3_Node element attributes) = do
    let attributeString = foldl applyChainableString selection attributes
    modify_ (\s -> s <> "\nappending "    <> show element <> " to " <> selection <> "\n" <> attributeString)
    pure "append"
  join selection (Join j) = do
    modify_ (\s -> s <> "\nentering a "   <> show j.element <> " for each datum" )
    pure "join"
  join selection (JoinGeneral j) = do
    modify_ (\s -> s <> "\nentering a "   <> show j.element <> " for each datum" )
    pure "join"
  join selection (JoinSimulation j) = do
    modify_ (\s -> s <> "\nentering a "   <> show j.element <> " for each datum" )
    pure "join"
  attachZoom selection zoomConfig = do
    modify_ (\s -> s <> "\nattaching a zoom handler to " <> selection)
    pure "attachZoom"


applyChainableString :: String -> Chainable -> String
applyChainableString selection  = 
  case _ of 
    (AttrT (Attribute label attr)) -> showSetAttr_ label (unbox attr) selection
    (TextT (Attribute label attr)) -> showSetText_ (unbox attr) selection  -- TODO unboxText surely?
    RemoveT                        -> showRemoveSelection_ selection
    (TransitionT chain transition) -> do 
      let tString = showAddTransition_ selection transition
      foldl applyChainableString tString chain


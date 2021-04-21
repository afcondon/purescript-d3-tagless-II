module D3.Interpreter.String where

import Control.Monad.State (class MonadState, StateT, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Interpreter (class D3Tagless)
import D3.Layouts.Hierarchical (Tree(..))
import D3.Selection (Chainable(..), D3_Node(..), Join(..), showAddTransition_, showRemoveSelection_, showSetAttr_, showSetText_)
import Data.Array (foldl)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, pure)

type MetaTreeNode = { nodeName :: String, attributes :: Array Chainable }
type MetaTree = Tree MetaTreeNode
newtype D3MetaTreeM a = D3MetaTreeM (StateT MetaTree Effect a)

initialMetaTree :: MetaTree
initialMetaTree = (Node { nodeName: "root", attributes: [] } [] )

runMetaTree :: D3MetaTreeM MetaTree -> Effect (Tuple MetaTree MetaTree) 
runMetaTree (D3MetaTreeM state) = runStateT state initialMetaTree

derive newtype instance functorD3MetaTreeM     :: Functor             D3MetaTreeM
derive newtype instance applyD3MetaTreeM       :: Apply               D3MetaTreeM
derive newtype instance applicativeD3MetaTreeM :: Applicative         D3MetaTreeM
derive newtype instance bindD3MetaTreeM        :: Bind                D3MetaTreeM
derive newtype instance monadD3MetaTreeM       :: Monad               D3MetaTreeM
derive newtype instance monadStateD3MetaTreeM  :: MonadState MetaTree D3MetaTreeM 
derive newtype instance monadEffD3MetaTreeM    :: MonadEffect         D3MetaTreeM

instance d3Tagless :: D3Tagless String D3MetaTreeM where
  attach selector = do
    -- modify_ (\s -> s <> "\nattaching to " <> selector <> " in DOM" )
    pure "attach"
  append selection (D3_Node element attributes) = do
    let attributeString = foldl applyChainableString selection attributes
    -- modify_ (\s -> s <> "\nappending "    <> show element <> " to " <> selection <> "\n" <> attributeString)
    pure "append"
  join selection (Join j) = do
    let attributeString = foldl applyChainableString selection j.behaviour
    -- modify_ (\s -> s <> "\nentering a "   <> show j.element <> " for each datum" )
    pure "join"
  join selection (JoinGeneral j) = do
    let enterAttributes  = foldl applyChainableString selection j.behaviour.enter
        exitAttributes   = foldl applyChainableString selection j.behaviour.exit
        updateAttributes = foldl applyChainableString selection j.behaviour.update
    -- modify_ (\s -> s <> "\n\tenter behaviour: " <> enterAttributes)
    -- modify_ (\s -> s <> "\n\tupdate behaviour: " <> updateAttributes)
    -- modify_ (\s -> s <> "\n\texit behaviour: " <> exitAttributes)
    pure "join"
  join selection (JoinSimulation j) = do
    let attributeString = foldl applyChainableString selection j.behaviour
    -- modify_ (\s -> s <> "\nentering a "   <> show j.element <> " for each datum" )
    pure "join"
  attachZoom selection zoomConfig = do
    -- modify_ (\s -> s <> "\nattaching a zoom handler to " <> selection)
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


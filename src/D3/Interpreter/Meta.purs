module D3.Interpreter.MetaTree where

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Interpreter (class D3InterpreterM)
import D3.Layouts.Hierarchical (Tree(..))
import D3.Selection (Chainable(..), D3_Node(..), Element, EnterUpdateExit, Join(..), Keys, ZoomConfig, Transition, showAddTransition_, showRemoveSelection_, showSetAttr_, showSetText_)
import Data.Array (foldl)
import Data.Map (Map, empty, insert)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, discard, bind, mempty, pure, (+))

type NodeID = Int
data MetaTreeNode = 
    Empty
  | AttachNode String
  | AppendNode Element
  -- TODO if datum type can be peeled off the Join type, just store the Join directly
  | JoinSimpleNode     NodeID Element Keys (Array Chainable)
  | JoinGeneralNode    NodeID Element Keys EnterUpdateExit
  | JoinSimulationNode NodeID Element Keys (Array Chainable)
  | ZoomNode ZoomConfig
  | AttrNode Chainable
  | TransitionNode Transition

type MetaTree = Map NodeID MetaTreeNode
data ScriptTree = ScriptTree Int MetaTree

newtype D3MetaTreeM a = D3MetaTreeM (StateT ScriptTree Effect a)

initialMetaTree :: ScriptTree
initialMetaTree = ScriptTree 0 empty 

-- runMetaTree :: D3MetaTreeM MetaTree -> Effect (Tuple MetaTree MetaTree) 
runMetaTree :: D3MetaTreeM NodeID -> Effect (Tuple NodeID ScriptTree)
runMetaTree (D3MetaTreeM state) = runStateT state initialMetaTree

derive newtype instance functorD3MetaTreeM     :: Functor             D3MetaTreeM
derive newtype instance applyD3MetaTreeM       :: Apply               D3MetaTreeM
derive newtype instance applicativeD3MetaTreeM :: Applicative         D3MetaTreeM
derive newtype instance bindD3MetaTreeM        :: Bind                D3MetaTreeM
derive newtype instance monadD3MetaTreeM       :: Monad               D3MetaTreeM
derive newtype instance monadStateD3MetaTreeM  :: MonadState ScriptTree D3MetaTreeM 
derive newtype instance monadEffD3MetaTreeM    :: MonadEffect         D3MetaTreeM

insertInScriptTree :: ScriptTree -> MetaTreeNode -> ScriptTree
insertInScriptTree (ScriptTree id nodeMap) newNode = ScriptTree (id + 1) (insert id newNode nodeMap)

insertAttributeInScriptTree :: ScriptTree -> Chainable -> ScriptTree
insertAttributeInScriptTree (ScriptTree id nodeMap) attr = ScriptTree (id + 1) (insert id (AttrNode attr) nodeMap)

instance d3Tagless :: D3InterpreterM NodeID D3MetaTreeM where
  attach selector = do
    (ScriptTree id _) <- get
    modify_ (\s -> insertInScriptTree s (AttachNode selector))
    pure (id + 1)

  append selection (D3_Node element attributes) = do
    (ScriptTree id _) <- get
    modify_ (\s -> insertInScriptTree s (AppendNode element))
    modify_ (\s -> foldl insertAttributeInScriptTree s attributes)
    (ScriptTree nextId _) <- get
    pure nextId

  join selection theJoin = do
    (ScriptTree id _) <- get
    case theJoin of
      (Join j) ->
        modify_ (\s -> insertInScriptTree s (JoinSimpleNode id j.element j.key j.behaviour))
      (JoinGeneral j) ->
        modify_ (\s -> insertInScriptTree s (JoinGeneralNode id j.element j.key j.behaviour))
      (JoinSimulation j) ->
        modify_ (\s -> insertInScriptTree s (JoinSimulationNode id j.element j.key j.behaviour))
    
    pure (id + 1)

  attachZoom selection zoomConfig = do
    (ScriptTree id _) <- get
    modify_ (\s -> insertInScriptTree s (ZoomNode zoomConfig))
    pure (id + 1)


applyChainableString :: String -> Chainable -> String
applyChainableString selection  = 
  case _ of 
    (AttrT (Attribute label attr)) -> showSetAttr_ label (unbox attr) selection
    (TextT (Attribute label attr)) -> showSetText_ (unbox attr) selection  -- TODO unboxText surely?
    RemoveT                        -> showRemoveSelection_ selection
    (TransitionT chain transition) -> do 
      let tString = showAddTransition_ selection transition
      foldl applyChainableString tString chain


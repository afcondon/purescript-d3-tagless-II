module D3.Interpreter.MetaTree where

import Prelude

import Control.Monad.State (class MonadState, StateT, State, get, modify_, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Interpreter (class D3InterpreterM)
import D3.Layouts.Hierarchical (Tree(..))
import D3.Selection (Chainable(..), D3_Node(..), Element, EnterUpdateExit, Join(..), Keys, ZoomConfig, Transition, showAddTransition_, showRemoveSelection_, showSetAttr_, showSetText_)
import Data.Array (foldl)
import Data.Map (Map, empty, insert)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)

type NodeID = Int
data MetaTreeNode = 
    Empty
  | AttachNode String
  | AppendNode Element
  -- TODO if datum type can be peeled off the Join type, just store the Join directly
  | JoinSimpleNode     Element Keys (Array Chainable)
  | JoinGeneralNode    Element Keys EnterUpdateExit
  | JoinSimulationNode Element Keys (Array Chainable)
  -- the next nodes are for nodes that are attributes and transitions and zooms which are all handled differently
  | ZoomNode ZoomConfig
  | AttrNode Chainable -- actually only Attr and Text
  | TransitionNode (Array Chainable) Transition
  | RemoveNode

type MetaTree = Map NodeID MetaTreeNode
data ScriptTree = ScriptTree Int MetaTree

newtype D3MetaTreeM a = D3MetaTreeM (StateT ScriptTree Effect a)

initialMetaTree :: ScriptTree
initialMetaTree = ScriptTree 0 empty 

-- runMetaTree :: D3MetaTreeM MetaTree -> Effect (Tuple MetaTree MetaTree) 
runMetaTree :: D3MetaTreeM NodeID -> Effect (Tuple NodeID ScriptTree)
runMetaTree (D3MetaTreeM state) = runStateT state initialMetaTree

derive newtype instance functorD3MetaTreeM     :: Functor               D3MetaTreeM
derive newtype instance applyD3MetaTreeM       :: Apply                 D3MetaTreeM
derive newtype instance applicativeD3MetaTreeM :: Applicative           D3MetaTreeM
derive newtype instance bindD3MetaTreeM        :: Bind                  D3MetaTreeM
derive newtype instance monadD3MetaTreeM       :: Monad                 D3MetaTreeM
derive newtype instance monadStateD3MetaTreeM  :: MonadState ScriptTree D3MetaTreeM 
derive newtype instance monadEffD3MetaTreeM    :: MonadEffect           D3MetaTreeM

insertInScriptTree :: MetaTreeNode -> D3MetaTreeM NodeID
-- returns the number of the next node
insertInScriptTree transition@(TransitionNode chain config) = do
  (ScriptTree id nodeMap) <- get
  modify_ (\s -> ScriptTree (id + 1) (insert id transition nodeMap))
  _ <- traverse insertAttributeInScriptTree chain
  (ScriptTree nextId _) <- get
  pure nextId
insertInScriptTree newNode = do
  (ScriptTree id nodeMap) <- get
  modify_ (\s -> ScriptTree (id + 1) (insert id newNode nodeMap))
  (ScriptTree nextId _) <- get
  pure (id + 1)

insertAttributeInScriptTree :: Chainable -> D3MetaTreeM NodeID
insertAttributeInScriptTree = 
  case _ of 
      -- simple attributes are just nodes
      attr@(AttrT _) -> insertInScriptTree (AttrNode attr)
      text@(TextT _) -> insertInScriptTree (AttrNode text)
      RemoveT        -> insertInScriptTree RemoveNode
-- the transition attribute is an exception, it can have further (Array Chainable)
      transition@(TransitionT chain config) ->
        insertInScriptTree (TransitionNode chain config)
  


instance d3Tagless :: D3InterpreterM NodeID D3MetaTreeM where
  attach selector = insertInScriptTree (AttachNode selector)

  append selection (D3_Node element attributes) = do
    _ <- insertInScriptTree (AppendNode element)
    _ <- traverse insertAttributeInScriptTree attributes
    (ScriptTree nextId _) <- get
    pure nextId

  join selection =
    case _ of
      (Join j)           -> insertInScriptTree (JoinSimpleNode     j.element j.key j.behaviour)
      (JoinGeneral j)    -> insertInScriptTree (JoinGeneralNode    j.element j.key j.behaviour)
      (JoinSimulation j) -> insertInScriptTree (JoinSimulationNode j.element j.key j.behaviour)

  attachZoom selection zoomConfig = pure 666 -- insertInScriptTree (ZoomNode zoomConfig)


applyChainableString :: String -> Chainable -> String
applyChainableString selection  = 
  case _ of 
    (AttrT (Attribute label attr)) -> showSetAttr_ label (unbox attr) selection
    (TextT (Attribute label attr)) -> showSetText_ (unbox attr) selection  -- TODO unboxText surely?
    RemoveT                        -> showRemoveSelection_ selection
    (TransitionT chain transition) -> do 
      let tString = showAddTransition_ selection transition
      foldl applyChainableString tString chain


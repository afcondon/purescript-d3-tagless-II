module D3.Interpreter.MetaTree where

import Prelude

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import D3.Attributes.Instances (Attribute(..), MouseEvent, unbox)
import D3.Interpreter (class D3InterpreterM)
import D3.Layouts.Hierarchical (TreeJson_)
import D3.Selection (Chainable(..), D3_Node(..), DragBehavior, Element, EnterUpdateExit, Join(..), Keys, Transition, showAddTransition_, showRemoveSelection_, showSetAttr_, showSetText_)
import D3.Zoom (ZoomConfig)
import Data.Array (filter, foldl, (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
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
  | ZoomNode (ZoomConfig NodeID)
  | DragNode DragBehavior -- TODO make chainable
  | AttrNode Chainable -- actually only Attr and Text
  | OnEventNode MouseEvent
  | TransitionNode (Array Chainable) Transition
  | RemoveNode

instance showMetaTreeNode :: Show MetaTreeNode where -- super primitive implementation to get started
  show Empty                      = "Empty"
  show RemoveNode                 = "RemoveNode"
  show (AttachNode _)             = "AttachNode"
  show (AppendNode _)             = "AppendNode"
  show (JoinSimpleNode _ _ _)     = "JoinSimpleNode"
  show (JoinGeneralNode _ _ _)    = "JoinGeneralNode"
  show (JoinSimulationNode _ _ _) = "JoinSimulationNode"
  show (ZoomNode _)               = "ZoomNode"
  show (DragNode _)               = "DragNode"
  show (AttrNode _)               = "AttrNode"
  show (OnEventNode _)            = "OnEventNode"
  show (TransitionNode _ _)       = "TransitionNode"

type MetaTreeMap = Map NodeID MetaTreeNode
data ScriptTree = ScriptTree Int MetaTreeMap (Array (Tuple NodeID NodeID))

newtype D3MetaTreeM a = D3MetaTreeM (StateT ScriptTree Effect a)

-- this is the type that we will give to JS, prune out the empty child arrays and then pass to d3.hierarchy
newtype MetaTreeNode_ = MetaTreeNode { name :: String, children :: Array MetaTreeNode_ }

scriptTreeToJSON :: ScriptTree ->  TreeJson_
scriptTreeToJSON (ScriptTree _ nodeMap links) = pruneEmptyChildren $ go 0
  where
    go :: NodeID -> MetaTreeNode_
    go id = do
      let children = snd <$> filter (\(Tuple parentID nodeID) -> parentID == id) links
          nodeName = show $ fromMaybe Empty $ lookup id nodeMap
      MetaTreeNode { name: nodeName, children: go <$> children }

foreign import pruneEmptyChildren :: MetaTreeNode_ -> TreeJson_

initialMetaTree :: ScriptTree
initialMetaTree = ScriptTree 0 empty []

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

insertInScriptTree :: NodeID -> MetaTreeNode -> D3MetaTreeM Unit
-- returns the number of the next node
insertInScriptTree parentID transition@(TransitionNode chain config) = do
  (ScriptTree id nodeMap links) <- get
  modify_ (\s -> ScriptTree (id + 1) (insert id transition nodeMap) ((Tuple parentID (id+1)) : links))
  _ <- traverse (insertAttributeInScriptTree parentID) chain
  pure unit

insertInScriptTree parentID newNode = do
  (ScriptTree id nodeMap links) <- get
  modify_ (\s -> ScriptTree (id + 1) (insert id newNode nodeMap) ((Tuple parentID (id+1)) : links))
  pure unit

insertAttributeInScriptTree :: NodeID -> Chainable -> D3MetaTreeM Unit
insertAttributeInScriptTree parentID = 
  case _ of 
      -- simple attributes are just nodes
      attr@(AttrT _)       -> insertInScriptTree parentID (AttrNode attr)
      text@(TextT _)       -> insertInScriptTree parentID (AttrNode text)
      (OnT event listener) -> insertInScriptTree parentID (OnEventNode event)
      RemoveT              -> insertInScriptTree parentID RemoveNode
-- the transition attribute is an exception, it can have further (Array Chainable)
      transition@(TransitionT chain config) ->
        insertInScriptTree parentID (TransitionNode chain config)
  


instance d3Tagless :: D3InterpreterM NodeID D3MetaTreeM where
  attach selector = do
    insertInScriptTree 0 (AttachNode selector) -- TODO this could actually be a multiple insert
    pure 1

  append nodeID (D3_Node element attributes) = do
    insertInScriptTree nodeID (AppendNode element)
    (ScriptTree id _ _) <- get
    _ <- traverse (insertAttributeInScriptTree id) attributes
    pure id -- this is the id of the AppendNode itself

  join nodeID (Join j)          = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (JoinSimpleNode j.element j.key j.behaviour)
    pure id
  join nodeID (JoinGeneral j)   = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (JoinGeneralNode j.element j.key j.behaviour)
    pure id
  join nodeID (JoinSimulation j)= do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (JoinSimulationNode j.element j.key j.behaviour)
    pure id

  attachZoom nodeID zoomConfig = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (ZoomNode zoomConfig)
    pure id

  onDrag nodeID behavior = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (DragNode behavior)
    pure id


-- applyChainableString :: String -> Chainable -> String
-- applyChainableString selection  = 
--   case _ of 
--     (AttrT (Attribute label attr)) -> showSetAttr_ label (unbox attr) selection
--     (TextT (Attribute label attr)) -> showSetText_ (unbox attr) selection  -- TODO unboxText surely?
--     RemoveT                        -> showRemoveSelection_ selection
--     (TransitionT chain transition) -> do 
--       let tString = showAddTransition_ selection transition
--       foldl applyChainableString tString chain


module PSD3.Interpreter.MetaTree where

import PSD3.Data.Node

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (Element, MouseEvent, Transition, Selector)
import PSD3.Internal.FFI (ComputeKeyFunction_)
import PSD3.Internal.Selection.Types (Behavior(..), SelectionAttribute(..), OrderingAttribute(..))
import PSD3.Capabilities.Selection (class SelectionM)
import Data.Array (filter, (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, bind, discard, pure, show, unit, ($), (+), (<$>), (<>), (==))

-- REVIEW this whole module should be re-written cleanly to match the final Final Tagless language for Selection and Simulation, it's really just a placeholder
-- but also serves to validate the ability of the interpreter to run in multiple monads

-- TODO fix interpreter build up of AST, not correct currently as attrs become children (just look at example in browser to see problem)
data D3GrammarNode = 
    Empty
  | AttachNode String
  | SelectUnderNode String
  | AppendNode Element
  | FilterNode String
  | ModifyNode (Array SelectionAttribute)
  -- TODO if datum type can be peeled off the Join type, just store the Join directly
  | JoinSimpleNode     Element (Array SelectionAttribute)
  | UpdateJoinNode     Element
  | OpenJoinNode (Selector NodeID)
  | JoinSimpleWithKeyFunctionNode Element ComputeKeyFunction_
  | SplitJoinCloseWithKeyFunctionNode Element ComputeKeyFunction_
  -- the next nodes are for nodes that are attributes and transitions and zooms which are all handled differently
  | OnNode (Behavior NodeID) -- TODO make chainable
  | AttrNode SelectionAttribute -- actually only Attr and Text
  | OrderNode String
  | OnEventNode MouseEvent
  | TransitionNode (Array SelectionAttribute) Transition
  | RemoveNode

instance showD3GrammarNode :: Show D3GrammarNode where -- super primitive implementation to get started
  show Empty                      = "Empty"
  show RemoveNode                 = "Remove"
  show (AttachNode _)             = "Attach"
  show (SelectUnderNode _)        = "SelectUnder"
  show (AppendNode _)             = "Append"
  show (FilterNode _)             = "Filter"
  show (ModifyNode _)             = "Modify"

  show (JoinSimpleNode _ _)       = "JoinSimple"
  show (UpdateJoinNode _)       = "JoinGeneral"
  show (OpenJoinNode s)           = "OpenJoin" <> s
  show (JoinSimpleWithKeyFunctionNode _ _) = "JoinSimple"
  show (SplitJoinCloseWithKeyFunctionNode _ _) = "JoinGeneral"

  show (AttrNode _)               = "Attr"
  show (OrderNode _)              = "Order"
  show (OnEventNode _)            = "OnEvent"
  show (TransitionNode _ _)       = "Transition"

  show (OnNode (Zoom _))          = "Zoom"
  show (OnNode (Drag _))          = "Drag"

showAsSymbol :: D3GrammarNode -> { name :: String, symbol :: String, param1 :: String,              param2 :: String }
showAsSymbol = 
  case _ of
    Empty                      ->  { name: "Empty"         , symbol: ""    , param1: "",           param2: "" }
    RemoveNode                 ->  { name: "Remove"        , symbol: "x"   , param1: "",           param2: "" }
    (AttachNode s)             ->  { name: "Attach"        , symbol: "a"   , param1: "",           param2: "" }
    (SelectUnderNode s)        ->  { name: "SelectUnder"   , symbol: "s"   , param1: tag s,        param2: "" }
    (AppendNode e)             ->  { name: "Append"        , symbol: "+"   , param1: tag $ show e, param2: "" }
    (FilterNode s)             ->  { name: "Filter"        , symbol: "/"   , param1: tag s,        param2: "" }
    (ModifyNode as)            ->  { name: "Modify"        , symbol: "->"  , param1: "",           param2: "" }
    (JoinSimpleNode e _)       ->  { name: "JoinSimple"    , symbol: "<+>" , param1: tag $ show e, param2: "" }
    (UpdateJoinNode e)       ->  { name: "SplitJoinClose"    , symbol: "<+>" , param1: tag $ show e, param2: "" }
    (OpenJoinNode s)           ->  { name: "SplitJoinClose"    , symbol: "<+>" , param1: tag $ s, param2: "" }
    (JoinSimpleWithKeyFunctionNode e _) ->  { name: "JoinSimpleK" , symbol: "<+>" , param1: tag $ show e, param2: "" }
    (SplitJoinCloseWithKeyFunctionNode e _) ->  { name: "SplitJoinCloseK" , symbol: "<+>" , param1: tag $ show e, param2: "" }
    (OnNode (Zoom _))          ->  { name: "Zoom"          , symbol: "z"   , param1: "",           param2: "" }
    (OnNode (Drag _))          ->  { name: "Drag"          , symbol: "drag", param1: "",           param2: "" }
    (AttrNode c)               ->  { name: "Attr"          , symbol: "attr", param1: show c,       param2: "" }
    (OrderNode c)              ->  { name: "Order"         , symbol: "order", param1: show c,       param2: "" }
    (OnEventNode _)            ->  { name: "OnEvent"       , symbol: "on"  , param1: "",           param2: "" }
    (TransitionNode _ _)       ->  { name: "Transition"    , symbol: "T"   , param1: "",           param2: "" }

tag :: String -> String
tag s = "<" <> s <> ">"

type MetaTreeMap = Map NodeID D3GrammarNode
data ScriptTree = ScriptTree Int MetaTreeMap (Array (Tuple NodeID NodeID))

newtype D3MetaTreeM a = D3MetaTreeM (StateT ScriptTree Effect a)

-- this is the type that we will give to JS, prune out the empty child arrays and then pass to d3.hierarchy
newtype D3GrammarNode_ = D3GrammarNode { 
    name     :: String
  , symbol   :: String
  , children :: Array D3GrammarNode_ 
  , param1   :: String
  , param2   :: String
}

scriptTreeToJSON :: ScriptTree -> TreeJson_
scriptTreeToJSON (ScriptTree _ nodeMap links) = pruneEmptyChildren $ go 0
  where
    go :: NodeID -> D3GrammarNode_
    go id = do
      let children = snd <$> filter (\(Tuple parentID nodeID) -> parentID == id) links

          { name, symbol, param1, param2 }
                   = showAsSymbol $ fromMaybe Empty $ lookup id nodeMap

      D3GrammarNode { name, symbol, param1, param2, children: go <$> children }

foreign import pruneEmptyChildren :: D3GrammarNode_ -> TreeJson_

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

insertInScriptTree :: NodeID -> D3GrammarNode -> D3MetaTreeM Unit
-- returns the number of the next node
insertInScriptTree parentID transition@(TransitionNode chain config) = do
  (ScriptTree id nodeMap links) <- get
  modify_ (\s -> ScriptTree (id + 1) (insert id transition nodeMap) ((Tuple parentID (id+1)) : links))
  -- _ <- traverse (insertAttributeInScriptTree parentID) chain
  pure unit

insertInScriptTree parentID newNode = do
  (ScriptTree id nodeMap links) <- get
  modify_ (\s -> ScriptTree (id + 1) (insert id newNode nodeMap) ((Tuple parentID (id+1)) : links))
  pure unit

insertAttributeInScriptTree :: NodeID -> SelectionAttribute -> D3MetaTreeM Unit
insertAttributeInScriptTree parentID = 
  case _ of 
      -- simple attributes are just nodes
      attr@(AttrT _)       -> insertInScriptTree parentID (AttrNode attr)
      text@(TextT _)       -> insertInScriptTree parentID (AttrNode text)
      text@(PropertyT _)   -> insertInScriptTree parentID (AttrNode text)
      text@(HTMLT _)       -> insertInScriptTree parentID (AttrNode text)

      RemoveT              -> insertInScriptTree parentID RemoveNode

      (OrderingT o) ->
        case o of
          Order    -> insertInScriptTree parentID (OrderNode "order")
          (Sort _) -> insertInScriptTree parentID (OrderNode "sort")
          Raise    -> insertInScriptTree parentID (OrderNode "raise")
          Lower    -> insertInScriptTree parentID (OrderNode "lower")

      -- the transition attribute is an exception, it can have further (Array SelectionAttribute)
      transition@(TransitionT chain config) ->
        insertInScriptTree parentID (TransitionNode chain config)

      (OnT event listener) -> insertInScriptTree parentID (OnEventNode event)
      (OnT' event listener) -> insertInScriptTree parentID (OnEventNode event) -- NB this is the effectful branch, doesn't matter to String interpreter tho

      -- attr@(ForceT _)       -> insertInScriptTree parentID (AttrNode attr) -- TODO specialize for Force attributes if needed
  


instance d3Tagless :: SelectionM NodeID D3MetaTreeM where
  appendTo nodeID element attributes = do
    insertInScriptTree nodeID (AppendNode element)
    (ScriptTree id _ _) <- get -- TODO write a lens to get at the ID and all this code will be much shorter
    -- _ <- traverse (insertAttributeInScriptTree id) attributes
    pure id -- this is the id of the AppendNode itself

  selectUnder nodeID selector = do
    insertInScriptTree nodeID (SelectUnderNode selector)
    (ScriptTree id _ _) <- get
    pure id

  attach selector = do
    insertInScriptTree 0 (AttachNode selector) -- TODO this could actually be a multiple insert
    pure 1

  filterSelection nodeID selector = do
    insertInScriptTree nodeID (FilterNode selector)
    (ScriptTree id _ _) <- get
    pure id

  mergeSelections a b = do
    -- insertInScriptTree nodeID (MergeNode a b)
    (ScriptTree id _ _) <- get
    pure id

  setAttributes nodeID attributes = do
    insertInScriptTree nodeID (ModifyNode attributes)
    pure unit

  on nodeID behavior = do
    insertInScriptTree nodeID (OnNode behavior) 
    pure unit

  openSelection selection selector = do
    -- insertInScriptTree nodeID (OpenNode selector)
    (ScriptTree id _ _) <- get
    pure id

  simpleJoin nodeID e ds k          = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (JoinSimpleWithKeyFunctionNode e k)
    pure id
  updateJoin nodeID e ds k          = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (UpdateJoinNode e)
    pure { enter: id, exit: id, update: id }

-- applySelectionAttributeString :: String -> SelectionAttribute -> String
-- applySelectionAttributeString selection  = 
--   case _ of 
--     (AttrT (AttributeSetter label attr)) -> showSetAttr_ label (unboxAttr attr) selection
--     (TextT (AttributeSetter label text)) -> showSetText_ (unboxText text) selection 
--     RemoveT                        -> showRemoveSelection_ selection
--     (TransitionT chain transition) -> do 
--       let tString = showAddTransition_ selection transition
--       foldl applySelectionAttributeString tString chain


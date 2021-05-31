module D3.Interpreter.MetaTree where

import D3.Node

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import D3.Data.Tree (TreeJson_)
import D3.Data.Types (Element, MouseEvent, Transition)
import D3.Interpreter (class D3InterpreterM)
import D3.Selection (Behavior(..), ChainableS(..), D3_Node(..), DragBehavior, EnterUpdateExit, Join(..), Keys)
import D3.Zoom (ZoomConfig)
import Data.Array (filter, (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, bind, discard, pure, show, unit, ($), (+), (<$>), (<>), (==))

-- TODO fix interpreter build up of AST, not correct currently as attrs become children (just look at example in browser to see problem)
data D3GrammarNode = 
    Empty
  | AttachNode String
  | AppendNode Element
  | FilterNode String
  | ModifyNode (Array ChainableS)
  -- TODO if datum type can be peeled off the Join type, just store the Join directly
  | JoinSimpleNode     Element Keys (Array ChainableS)
  | JoinGeneralNode    Element Keys EnterUpdateExit
  -- the next nodes are for nodes that are attributes and transitions and zooms which are all handled differently
  | OnNode Behavior -- TODO make chainable
  | AttrNode ChainableS -- actually only Attr and Text
  | OnEventNode MouseEvent
  | TransitionNode (Array ChainableS) Transition
  | RemoveNode

instance showD3GrammarNode :: Show D3GrammarNode where -- super primitive implementation to get started
  show Empty                      = "Empty"
  show RemoveNode                 = "Remove"
  show (AttachNode _)             = "Attach"
  show (AppendNode _)             = "Append"
  show (FilterNode _)             = "Filter"
  show (ModifyNode _)             = "Modify"
  show (JoinSimpleNode _ _ _)     = "JoinSimple"
  show (JoinGeneralNode _ _ _)    = "JoinGeneral"
  show (AttrNode _)               = "Attr"
  show (OnEventNode _)            = "OnEvent"
  show (TransitionNode _ _)       = "Transition"

  show (OnNode (Zoom _))          = "Zoom"
  show (OnNode (Drag _))          = "Drag"
  show (OnNode (Tick _))          = "Tick"

showAsSymbol :: D3GrammarNode -> { name :: String, symbol :: String, param1 :: String,              param2 :: String }
showAsSymbol = 
  case _ of
    Empty                      ->  { name: "Empty"         , symbol: ""    , param1: "",           param2: "" }
    RemoveNode                 ->  { name: "Remove"        , symbol: "x"   , param1: "",           param2: "" }
    (AttachNode s)             ->  { name: "Attach"        , symbol: "a"   , param1: "",           param2: "" }
    (AppendNode e)             ->  { name: "Append"        , symbol: "+"   , param1: tag $ show e, param2: "" }
    (FilterNode s)             ->  { name: "Filter"        , symbol: "/"   , param1: tag s,        param2: "" }
    (ModifyNode as)            ->  { name: "Modify"        , symbol: "->"  , param1: "",           param2: "" }
    (JoinSimpleNode e _ _)     ->  { name: "JoinSimple"    , symbol: "<+>" , param1: tag $ show e, param2: "" }
    (JoinGeneralNode e _ _)    ->  { name: "JoinGeneral"   , symbol: "<+>" , param1: "",           param2: "" }
    (OnNode (Zoom _))          ->  { name: "Zoom"          , symbol: "z"   , param1: "",           param2: "" }
    (OnNode (Drag _))          ->  { name: "Drag"          , symbol: "drag", param1: "",           param2: "" }
    (OnNode (Tick _))          ->  { name: "Tick"          , symbol: "tick", param1: "",           param2: "" }
    (AttrNode c)               ->  { name: "Attr"          , symbol: "attr", param1: show c,       param2: "" }
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

insertAttributeInScriptTree :: NodeID -> ChainableS -> D3MetaTreeM Unit
insertAttributeInScriptTree parentID = 
  case _ of 
      -- simple attributes are just nodes
      attr@(AttrT _)       -> insertInScriptTree parentID (AttrNode attr)
      text@(TextT _)       -> insertInScriptTree parentID (AttrNode text)

      RemoveT              -> insertInScriptTree parentID RemoveNode

      -- the transition attribute is an exception, it can have further (Array ChainableS)
      transition@(TransitionT chain config) ->
        insertInScriptTree parentID (TransitionNode chain config)

      (OnT event listener) -> insertInScriptTree parentID (OnEventNode event)

      -- attr@(ForceT _)       -> insertInScriptTree parentID (AttrNode attr) -- TODO specialize for Force attributes if needed
  


instance d3Tagless :: D3InterpreterM NodeID D3MetaTreeM where
  attach selector = do
    insertInScriptTree 0 (AttachNode selector) -- TODO this could actually be a multiple insert
    pure 1

  append nodeID (D3_Node element attributes) = do
    insertInScriptTree nodeID (AppendNode element)
    (ScriptTree id _ _) <- get
    -- _ <- traverse (insertAttributeInScriptTree id) attributes
    pure id -- this is the id of the AppendNode itself

  filter nodeID selector = do
    insertInScriptTree nodeID (FilterNode selector)
    (ScriptTree id _ _) <- get
    pure id

  modify nodeID attributes = do
    insertInScriptTree nodeID (ModifyNode attributes)
    (ScriptTree id _ _) <- get
    pure id

  join nodeID (Join j)          = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (JoinSimpleNode j.element j.key j.behaviour)
    pure id
  join nodeID (JoinGeneral j)   = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (JoinGeneralNode j.element j.key j.behaviour)
    pure id

  on nodeID behavior = do
    (ScriptTree id _ _) <- get
    insertInScriptTree nodeID (OnNode behavior) 
    pure id

-- applyChainableSString :: String -> ChainableS -> String
-- applyChainableSString selection  = 
--   case _ of 
--     (AttrT (Attribute label attr)) -> showSetAttr_ label (unbox attr) selection
--     (TextT (Attribute label attr)) -> showSetText_ (unbox attr) selection  -- TODO unboxText surely?
--     RemoveT                        -> showRemoveSelection_ selection
--     (TransitionT chain transition) -> do 
--       let tString = showAddTransition_ selection transition
--       foldl applyChainableSString tString chain


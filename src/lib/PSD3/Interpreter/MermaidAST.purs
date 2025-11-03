module PSD3.Interpreter.MermaidAST where

import PSD3.Internal.Selection.Types
import PSD3.Data.Node (NodeID)

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import PSD3.Internal.Attributes.Instances (AttributeSetter(..))
import PSD3.Internal.Types (Element, Transition)
import PSD3.Internal.FFI (ComputeKeyFunction_)
import PSD3.Capabilities.Selection (class SelectionM)
import Data.Array (length, foldl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

-- | State for generating Mermaid diagram
-- | Contains the node counter and accumulated Mermaid syntax
type MermaidState =
  { nodeCounter :: Int
  , mermaidCode :: String
  }

newtype MermaidASTM a = MermaidASTM (StateT MermaidState Effect a)

-- | Escape quotes in labels for Mermaid by replacing with single quotes
escapeLabel :: String -> String
escapeLabel = replaceAll (Pattern "\"") (Replacement "'")

runMermaidAST :: MermaidASTM NodeID -> Effect String
runMermaidAST (MermaidASTM state) = do
  Tuple _ finalState <- runStateT state initialState
  pure $ "graph TD\n" <> finalState.mermaidCode
  where
    initialState = { nodeCounter: 0, mermaidCode: "" }

derive newtype instance functorMermaidASTM     :: Functor           MermaidASTM
derive newtype instance applyMermaidASTM       :: Apply             MermaidASTM
derive newtype instance applicativeMermaidASTM :: Applicative       MermaidASTM
derive newtype instance bindMermaidASTM        :: Bind              MermaidASTM
derive newtype instance monadMermaidASTM       :: Monad             MermaidASTM
derive newtype instance monadStateMermaidASTM  :: MonadState MermaidState MermaidASTM
derive newtype instance monadEffMermaidASTM    :: MonadEffect       MermaidASTM

-- | Add a node to the Mermaid diagram and return its ID
addNode :: String -> MermaidASTM NodeID
addNode label = do
  state <- get
  let nodeId = state.nodeCounter
      nodeName = "n" <> show nodeId
      escapedLabel = escapeLabel label
      line = "    " <> nodeName <> "[\"" <> escapedLabel <> "\"]\n"
  modify_ (\s -> s { nodeCounter = s.nodeCounter + 1, mermaidCode = s.mermaidCode <> line })
  pure nodeId

-- | Add an edge between two nodes
addEdge :: NodeID -> NodeID -> String -> MermaidASTM Unit
addEdge fromId toId label = do
  let fromName = "n" <> show fromId
      toName = "n" <> show toId
      line = "    " <> fromName <> " -->|" <> label <> "| " <> toName <> "\n"
  modify_ (\s -> s { mermaidCode = s.mermaidCode <> line })

instance mermaidTagless :: SelectionM NodeID MermaidASTM where
  attach selector = do
    nodeId <- addNode ("select(\"" <> selector <> "\")")
    pure nodeId

  selectUnder parentId selector = do
    nodeId <- addNode ("selectAll(\"" <> selector <> "\")")
    addEdge parentId nodeId "selectAll"
    pure nodeId

  appendTo parentId element attributes = do
    nodeId <- addNode ("append(\"" <> show element <> "\")")
    addEdge parentId nodeId "append"
    -- Add attribute nodes
    _ <- foldl (\acc attr -> acc *> addAttributeNode nodeId attr) (pure unit) attributes
    pure nodeId

  filterSelection selectionId selector = do
    nodeId <- addNode ("filter(\"" <> selector <> "\")")
    addEdge selectionId nodeId "filter"
    pure nodeId

  mergeSelections aId bId = do
    nodeId <- addNode "merge"
    addEdge aId nodeId "a"
    addEdge bId nodeId "b"
    pure nodeId

  setAttributes selectionId attributes = do
    _ <- foldl (\acc attr -> acc *> addAttributeNode selectionId attr) (pure unit) attributes
    pure unit

  on selectionId (Drag _drag) = do
    nodeId <- addNode "drag()"
    addEdge selectionId nodeId "call"
    pure unit

  on selectionId (Zoom _zoom) = do
    nodeId <- addNode "zoom()"
    addEdge selectionId nodeId "call"
    pure unit

  openSelection selectionId selector = do
    nodeId <- addNode ("selectAll(\"" <> selector <> "\")")
    addEdge selectionId nodeId "open"
    pure nodeId

  simpleJoin selectionId element ds _k = do
    -- Data node
    dataNodeId <- addNode ("data([" <> show (length ds) <> " items])")
    addEdge selectionId dataNodeId "data"
    -- Join node
    joinNodeId <- addNode ("join(\"" <> show element <> "\")")
    addEdge dataNodeId joinNodeId "join"
    pure joinNodeId

  nestedJoin selectionId element _extractChildren _k = do
    -- Data node with nested function
    dataNodeId <- addNode "data(fn)"
    addEdge selectionId dataNodeId "data"
    -- Join node
    joinNodeId <- addNode ("join(\"" <> show element <> "\")")
    addEdge dataNodeId joinNodeId "join"
    pure joinNodeId

  updateJoin selectionId element ds _k = do
    -- Data node
    dataNodeId <- addNode ("data([" <> show (length ds) <> " items])")
    addEdge selectionId dataNodeId "data"

    -- Enter node
    enterNodeId <- addNode "enter()"
    addEdge dataNodeId enterNodeId "enter"
    enterAppendId <- addNode ("append(\"" <> show element <> "\")")
    addEdge enterNodeId enterAppendId "append"

    -- Exit node
    exitNodeId <- addNode "exit()"
    addEdge dataNodeId exitNodeId "exit"
    exitRemoveId <- addNode "remove()"
    addEdge exitNodeId exitRemoveId "remove"

    pure { enter: enterAppendId, exit: exitRemoveId, update: dataNodeId }

-- | Add an attribute node to the diagram
addAttributeNode :: NodeID -> SelectionAttribute -> MermaidASTM Unit
addAttributeNode parentId attr = do
  case attr of
    (AttrT (AttributeSetter label _)) -> do
      nodeId <- addNode ("attr(\"" <> label <> "\")")
      addEdge parentId nodeId "attr"

    (TextT (AttributeSetter _label _)) -> do
      nodeId <- addNode "text(...)"
      addEdge parentId nodeId "text"

    (PropertyT (AttributeSetter label _)) -> do
      nodeId <- addNode ("property(\"" <> label <> "\")")
      addEdge parentId nodeId "property"

    (HTMLT (AttributeSetter _label _)) -> do
      nodeId <- addNode "html(...)"
      addEdge parentId nodeId "html"

    RemoveT -> do
      nodeId <- addNode "remove()"
      addEdge parentId nodeId "remove"

    (OrderingT o) -> do
      nodeId <- addNode (show o)
      addEdge parentId nodeId "order"

    (TransitionT chain _transition) -> do
      nodeId <- addNode "transition()"
      addEdge parentId nodeId "transition"
      -- Add chained attributes
      _ <- foldl (\acc a -> acc *> addAttributeNode nodeId a) (pure unit) chain
      pure unit

    (OnT event _listener) -> do
      nodeId <- addNode ("on(\"" <> show event <> "\")")
      addEdge parentId nodeId "on"

    (OnT' event _listener) -> do
      nodeId <- addNode ("on(\"" <> show event <> "\")")
      addEdge parentId nodeId "on"

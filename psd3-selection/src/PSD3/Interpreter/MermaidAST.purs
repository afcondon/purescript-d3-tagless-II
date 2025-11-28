module PSD3.Interpreter.MermaidAST where

import Prelude (class Monad, Unit, bind, discard, pure, show, unit, ($), (+), (<>))

import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import Data.Array (foldl, snoc)
import Data.Foldable (class Foldable)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import PSD3.Capabilities.Selection (class SelectionM)
import PSD3.Data.Node (NodeID)
import PSD3.Internal.Attributes.Instances (AttributeSetter(..))
import PSD3.Internal.Selection.Types (Behavior, SelectionAttribute(..))
import PSD3.Internal.Types (Datum_, Element, Index_, Selector)

-- | Phantom-type-aware selection type for Mermaid interpreter
-- | The type parameter d tracks the datum type (for compatibility with phantom types)
-- | but is not used at runtime (Mermaid just tracks node IDs)
newtype MermaidSelection d = MermaidSelection NodeID

-- | State for generating Mermaid diagram
-- | Contains the node counter and accumulated Mermaid syntax
type MermaidState =
  { nodeCounter :: Int
  , mermaidCode :: String
  , nodeStyles :: Array (Tuple Int String)  -- Track node IDs and their style classes
  }

newtype MermaidASTM a = MermaidASTM (StateT MermaidState Effect a)

-- | Escape quotes in labels for Mermaid by replacing with single quotes
escapeLabel :: String -> String
escapeLabel = replaceAll (Pattern "\"") (Replacement "'")

runMermaidAST :: forall d. MermaidASTM (MermaidSelection d) -> Effect String
runMermaidAST (MermaidASTM state) = do
  Tuple _ finalState <- runStateT state initialState
  let themeConfig = "%%{init: {'theme':'base', 'themeVariables': {'fontFamily':'monospace'}, 'look':'handDrawn', 'flowchart':{'curve':'basis'}}}%%\n"
  let styleDefinitions = generateStyleDefinitions finalState.nodeStyles
  pure $ themeConfig <> "graph TD\n" <> finalState.mermaidCode <> styleDefinitions
  where
    initialState = { nodeCounter: 0, mermaidCode: "", nodeStyles: [] }

derive newtype instance functorMermaidASTM     :: Functor           MermaidASTM
derive newtype instance applyMermaidASTM       :: Apply             MermaidASTM
derive newtype instance applicativeMermaidASTM :: Applicative       MermaidASTM
derive newtype instance bindMermaidASTM        :: Bind              MermaidASTM
derive newtype instance monadMermaidASTM       :: Monad             MermaidASTM
derive newtype instance monadStateMermaidASTM  :: MonadState MermaidState MermaidASTM
derive newtype instance monadEffMermaidASTM    :: MonadEffect       MermaidASTM

-- | Add a node to the Mermaid diagram with a style class and return its ID
addNode :: String -> String -> MermaidASTM NodeID
addNode label styleClass = do
  state <- get
  let nodeId = state.nodeCounter
      nodeName = "n" <> show nodeId
      escapedLabel = escapeLabel label
      line = "    " <> nodeName <> "[\"" <> escapedLabel <> "\"]:::" <> styleClass <> "\n"
      newStyles = snoc state.nodeStyles (Tuple nodeId styleClass)
  modify_ (\s -> s { nodeCounter = s.nodeCounter + 1, mermaidCode = s.mermaidCode <> line, nodeStyles = newStyles })
  pure nodeId

-- | Add an edge between two nodes (no label for cleaner diagrams)
addEdge :: NodeID -> NodeID -> MermaidASTM Unit
addEdge fromId toId = do
  let fromName = "n" <> show fromId
      toName = "n" <> show toId
      line = "    " <> fromName <> " --> " <> toName <> "\n"
  modify_ (\s -> s { mermaidCode = s.mermaidCode <> line })

-- | Generate style class definitions for Mermaid
-- | Using vibrant colors from Spectral scheme (matching grouped bar chart)
generateStyleDefinitions :: Array (Tuple Int String) -> String
generateStyleDefinitions _ =
  "\n    %% Style definitions\n" <>
  "    classDef selectOp fill:#abdda4,stroke:#66c2a5,stroke-width:2px\n" <>
  "    classDef appendOp fill:#fee08b,stroke:#fdae61,stroke-width:2px\n" <>
  "    classDef joinOp fill:#f46d43,stroke:#d53e4f,stroke-width:2px\n" <>
  "    classDef attrOp fill:#e6f598,stroke:#abdda4,stroke-width:2px\n" <>
  "    classDef controlOp fill:#66c2a5,stroke:#3288bd,stroke-width:2px\n" <>
  "    classDef transitionOp fill:#fdae61,stroke:#f46d43,stroke-width:2px\n"

instance mermaidTagless :: SelectionM MermaidSelection MermaidASTM where
  attach selector = do
    nodeId <- addNode ("select(\"" <> selector <> "\")") "selectOp"
    pure (MermaidSelection nodeId)

  selectUnder (MermaidSelection parentId) selector = do
    nodeId <- addNode ("selectAll(\"" <> selector <> "\")") "selectOp"
    addEdge parentId nodeId
    pure (MermaidSelection nodeId)

  appendTo (MermaidSelection parentId) element attributes = do
    nodeId <- addNode ("append(\"" <> show element <> "\")") "appendOp"
    addEdge parentId nodeId
    -- Add coalesced attributes node
    addAttributesNode nodeId attributes
    pure (MermaidSelection nodeId)

  filterSelection (MermaidSelection selectionId) selector = do
    nodeId <- addNode ("filter(\"" <> selector <> "\")") "selectOp"
    addEdge selectionId nodeId
    pure (MermaidSelection nodeId)

  mergeSelections (MermaidSelection aId) (MermaidSelection bId) = do
    nodeId <- addNode "merge" "selectOp"
    addEdge aId nodeId
    addEdge bId nodeId
    pure (MermaidSelection nodeId)

  setAttributes (MermaidSelection selectionId) attributes = do
    addAttributesNode selectionId attributes
    pure unit

  on (MermaidSelection selectionId) (Drag _drag) = do
    nodeId <- addNode "drag()" "controlOp"
    addEdge selectionId nodeId
    pure unit

  on (MermaidSelection selectionId) (Zoom _zoom) = do
    nodeId <- addNode "zoom()" "controlOp"
    addEdge selectionId nodeId
    pure unit

  openSelection (MermaidSelection selectionId) selector = do
    nodeId <- addNode ("selectAll(\"" <> selector <> "\")") "selectOp"
    addEdge selectionId nodeId
    pure (MermaidSelection nodeId)

  simpleJoin (MermaidSelection selectionId) element ds _k = do
    -- Collapsed simpleJoin node
    joinNodeId <- addNode ("simpleJoin(\"" <> show element <> "\", [" <> show (length ds) <> " items])") "joinOp"
    addEdge selectionId joinNodeId
    pure (MermaidSelection joinNodeId)

  nestedJoin (MermaidSelection selectionId) element _extractChildren _k = do
    -- Collapsed nestedJoin node
    joinNodeId <- addNode ("nestedJoin(\"" <> show element <> "\", fn)") "joinOp"
    addEdge selectionId joinNodeId
    pure (MermaidSelection joinNodeId)

  updateJoin (MermaidSelection selectionId) element ds _k = do
    -- Collapsed updateJoin node
    joinNodeId <- addNode ("updateJoin(\"" <> show element <> "\", [" <> show (length ds) <> " items])") "joinOp"
    addEdge selectionId joinNodeId

    -- Enter node
    enterNodeId <- addNode "enter()" "controlOp"
    addEdge joinNodeId enterNodeId
    enterAppendId <- addNode ("append(\"" <> show element <> "\")") "appendOp"
    addEdge enterNodeId enterAppendId

    -- Exit node
    exitNodeId <- addNode "exit()" "controlOp"
    addEdge joinNodeId exitNodeId
    exitRemoveId <- addNode "remove()" "controlOp"
    addEdge exitNodeId exitRemoveId

    pure { enter: MermaidSelection enterAppendId, exit: MermaidSelection exitRemoveId, update: MermaidSelection joinNodeId }

-- | Format a single attribute for display
formatAttribute :: forall d. SelectionAttribute d -> String
formatAttribute attr = case attr of
  (AttrT (AttributeSetter label _)) -> label
  (TextT (AttributeSetter _label _)) -> "text"
  (PropertyT (AttributeSetter label _)) -> "prop:" <> label
  (HTMLT (AttributeSetter _label _)) -> "html"
  RemoveT -> "remove"
  (OrderingT o) -> show o
  (TransitionT chain _transition) -> "transition -> [" <> formatAttributeList chain <> "]"
  (OnT event _listener) -> "on(" <> show event <> ")"
  (OnT' event _listener) -> "on(" <> show event <> ")"

-- | Format a list of attributes as a comma-separated string
formatAttributeList :: forall d. Array (SelectionAttribute d) -> String
formatAttributeList attrs =
  case attrs of
    [] -> ""
    _ -> foldl (\acc attr -> if acc == "" then formatAttribute attr else acc <> ", " <> formatAttribute attr) "" attrs

-- | Add a coalesced attributes node to the diagram
addAttributesNode :: forall d. NodeID -> Array (SelectionAttribute d) -> MermaidASTM Unit
addAttributesNode parentId attributes =
  case attributes of
    [] -> pure unit
    _ -> do
      let attrList = formatAttributeList attributes
      nodeId <- addNode ("attrs: [" <> attrList <> "]") "attrOp"
      addEdge parentId nodeId

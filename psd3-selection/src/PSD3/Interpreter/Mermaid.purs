module PSD3.Interpreter.Mermaid
  ( NodeID
  , MermaidTreeState
  , MermaidTreeM(..)
  , escapeLabel
  , addNode
  , addEdge
  , showElement
  , formatAttribute
  , formatAttributeList
  , renderTree
  , generateStyleDefinitions
  , runMermaidTree
  ) where

import Prelude

import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Array (foldl, length, uncons)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), isJust)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttrSource(..))
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree(..))

-- | Node ID for tracking connections in Mermaid diagram
type NodeID = Int

-- | State for generating Mermaid diagram from Tree API
type MermaidTreeState =
  { nodeCounter :: Int
  , mermaidCode :: String
  }

newtype MermaidTreeM a = MermaidTreeM (StateT MermaidTreeState Effect a)

derive newtype instance functorMermaidTreeM :: Functor MermaidTreeM
derive newtype instance applyMermaidTreeM :: Apply MermaidTreeM
derive newtype instance applicativeMermaidTreeM :: Applicative MermaidTreeM
derive newtype instance bindMermaidTreeM :: Bind MermaidTreeM
derive newtype instance monadMermaidTreeM :: Monad MermaidTreeM

-- | Escape quotes in labels for Mermaid
escapeLabel :: String -> String
escapeLabel = replaceAll (Pattern "\"") (Replacement "'")

-- | Add a node to the Mermaid diagram with a style class
addNode :: String -> String -> MermaidTreeM NodeID
addNode label styleClass = MermaidTreeM do
  state <- get
  let nodeId = state.nodeCounter
      nodeName = "n" <> show nodeId
      escapedLabel = escapeLabel label
      line = "    " <> nodeName <> "[\"" <> escapedLabel <> "\"]:::" <> styleClass <> "\n"
  modify_ (\s -> s { nodeCounter = s.nodeCounter + 1, mermaidCode = s.mermaidCode <> line })
  pure nodeId

-- | Add an edge between two nodes
addEdge :: NodeID -> NodeID -> MermaidTreeM Unit
addEdge fromId toId = MermaidTreeM do
  let fromName = "n" <> show fromId
      toName = "n" <> show toId
      line = "    " <> fromName <> " --> " <> toName <> "\n"
  modify_ (\s -> s { mermaidCode = s.mermaidCode <> line })

-- | Format element type for display
showElement :: ElementType -> String
showElement SVG = "svg"
showElement Group = "g"
showElement Circle = "circle"
showElement Rect = "rect"
showElement Line = "line"
showElement Path = "path"
showElement Text = "text"
showElement Div = "div"
showElement Span = "span"
showElement Table = "table"
showElement Tr = "tr"
showElement Td = "td"
showElement Th = "th"
showElement Thead = "thead"
showElement Tbody = "tbody"
showElement Defs = "defs"
showElement LinearGradient = "linearGradient"
showElement Stop = "stop"
showElement PatternFill = "pattern"

-- | Format a single attribute for display
formatAttribute :: forall d. Attribute d -> String
formatAttribute = case _ of
  StaticAttr (AttributeName name) _ -> name
  DataAttr (AttributeName name) src _ -> formatAttrSource name src
  IndexedAttr (AttributeName name) src _ -> formatAttrSource name src

-- | Format attribute source for Mermaid display
formatAttrSource :: String -> AttrSource -> String
formatAttrSource name = case _ of
  UnknownSource -> name <> "(d)"
  StaticSource _ -> name
  FieldSource f -> name <> "=d." <> f
  ExprSource e -> name <> "=(" <> e <> ")"
  IndexSource -> name <> "=i"

-- | Format a list of attributes as a comma-separated string
formatAttributeList :: forall d. Array (Attribute d) -> String
formatAttributeList attrs =
  case attrs of
    [] -> ""
    _ -> foldl (\acc attr -> if acc == "" then formatAttribute attr else acc <> ", " <> formatAttribute attr) "" attrs

-- | Render a tree node to Mermaid
renderTree :: forall datum. Tree datum -> Maybe NodeID -> MermaidTreeM NodeID
renderTree tree parentId = case tree of
  Node node -> do
    -- Create node with name if present, otherwise anonymous
    let nodeLabel = case node.name of
          Just name -> "<" <> showElement node.elemType <> " \"" <> name <> "\">"
          Nothing -> showElement node.elemType
        attrSuffix = if length node.attrs > 0
          then " + [" <> formatAttributeList node.attrs <> "]"
          else ""
        fullLabel = nodeLabel <> attrSuffix

    nodeId <- addNode fullLabel "elementNode"

    -- Connect to parent if present
    case parentId of
      Just pid -> addEdge pid nodeId
      Nothing -> pure unit

    -- Render children
    _ <- traverseWithParent nodeId node.children
    pure nodeId

  Join joinSpec -> do
    -- Create join node
    let joinLabel = "JOIN \"" <> joinSpec.name <> "\" (" <> joinSpec.key <> ") [" <> show (length joinSpec.joinData) <> " items]"
    joinId <- addNode joinLabel "joinNode"

    -- Connect to parent if present
    case parentId of
      Just pid -> addEdge pid joinId
      Nothing -> pure unit

    -- Create template node (just show one instance)
    templateLabel <- addNode "template(datum) →" "templateNode"
    addEdge joinId templateLabel

    -- Render one instance of the template (if data exists)
    case uncons joinSpec.joinData of
      Nothing -> pure unit
      Just { head: firstDatum } -> do
        let templateTree = joinSpec.template firstDatum
        _ <- renderTree templateTree (Just templateLabel)
        pure unit

    pure joinId

  NestedJoin nestedSpec -> do
    -- Create nested join node
    let joinLabel = "NESTED JOIN \"" <> nestedSpec.name <> "\" (" <> nestedSpec.key <> ") [" <> show (length nestedSpec.joinData) <> " items]"
    joinId <- addNode joinLabel "nestedJoinNode"

    -- Connect to parent if present
    case parentId of
      Just pid -> addEdge pid joinId
      Nothing -> pure unit

    -- Create decompose node
    decomposeId <- addNode "decompose(datum) →" "decomposeNode"
    addEdge joinId decomposeId

    -- Create template node
    templateId <- addNode "template(innerDatum) →" "templateNode"
    addEdge decomposeId templateId

    -- Render one instance of the template (if data exists)
    case uncons nestedSpec.joinData of
      Nothing -> pure unit
      Just { head: firstDatum } -> do
        let innerData = nestedSpec.decompose firstDatum
        case uncons innerData of
          Nothing -> pure unit
          Just { head: firstInner } -> do
            let templateTree = nestedSpec.template firstInner
            _ <- renderTree templateTree (Just templateId)
            pure unit

    pure joinId

  UpdateJoin sceneSpec -> do
    -- Create update join node with GUP info
    let behaviorInfo =
          (if isJust sceneSpec.behaviors.enter then "E" else "") <>
          (if isJust sceneSpec.behaviors.update then "U" else "") <>
          (if isJust sceneSpec.behaviors.exit then "X" else "")
        joinLabel = "UPDATE JOIN \"" <> sceneSpec.name <> "\" (" <> sceneSpec.key <> ") ["
                    <> show (length sceneSpec.joinData) <> " items] {" <> behaviorInfo <> "}"
    joinId <- addNode joinLabel "updateJoinNode"

    -- Connect to parent if present
    case parentId of
      Just pid -> addEdge pid joinId
      Nothing -> pure unit

    -- Create template node
    templateLabel <- addNode "template(datum) →" "templateNode"
    addEdge joinId templateLabel

    -- Render one instance of the template (if data exists)
    case uncons sceneSpec.joinData of
      Nothing -> pure unit
      Just { head: firstDatum } -> do
        let templateTree = sceneSpec.template firstDatum
        _ <- renderTree templateTree (Just templateLabel)
        pure unit

    pure joinId

  UpdateNestedJoin sceneNestedSpec -> do
    -- Create update nested join node with GUP info
    let behaviorInfo =
          (if isJust sceneNestedSpec.behaviors.enter then "E" else "") <>
          (if isJust sceneNestedSpec.behaviors.update then "U" else "") <>
          (if isJust sceneNestedSpec.behaviors.exit then "X" else "")
        joinLabel = "UPDATE NESTED JOIN \"" <> sceneNestedSpec.name <> "\" (" <> sceneNestedSpec.key <> ") ["
                    <> show (length sceneNestedSpec.joinData) <> " items] {" <> behaviorInfo <> "}"
    joinId <- addNode joinLabel "updateNestedJoinNode"

    -- Connect to parent if present
    case parentId of
      Just pid -> addEdge pid joinId
      Nothing -> pure unit

    -- Create decompose node
    decomposeId <- addNode "decompose(datum) →" "decomposeNode"
    addEdge joinId decomposeId

    -- Create template node
    templateLabel <- addNode "template(innerDatum) →" "templateNode"
    addEdge decomposeId templateLabel

    -- Render one instance of the template (if data exists)
    case uncons sceneNestedSpec.joinData of
      Nothing -> pure unit
      Just { head: firstDatum } -> do
        let innerData = sceneNestedSpec.decompose firstDatum
        case uncons innerData of
          Nothing -> pure unit
          Just { head: firstInner } -> do
            let templateTree = sceneNestedSpec.template firstInner
            _ <- renderTree templateTree (Just templateLabel)
            pure unit

    pure joinId

  where
    traverseWithParent :: NodeID -> Array (Tree datum) -> MermaidTreeM Unit
    traverseWithParent pid children = do
      _ <- traverse (\child -> renderTree child (Just pid)) children
      pure unit

-- | Generate style class definitions for Mermaid
generateStyleDefinitions :: String
generateStyleDefinitions =
  "\n    %% Style definitions\n" <>
  "    classDef elementNode fill:#e6f598,stroke:#abdda4,stroke-width:2px\n" <>
  "    classDef joinNode fill:#f46d43,stroke:#d53e4f,stroke-width:2px\n" <>
  "    classDef nestedJoinNode fill:#fdae61,stroke:#f46d43,stroke-width:2px\n" <>
  "    classDef templateNode fill:#abdda4,stroke:#66c2a5,stroke-width:2px\n" <>
  "    classDef decomposeNode fill:#fee08b,stroke:#fdae61,stroke-width:2px\n"

-- | Run the Mermaid tree interpreter and generate diagram code
runMermaidTree :: forall datum. Tree datum -> Effect String
runMermaidTree tree = do
  let (MermaidTreeM state) = renderTree tree Nothing
  Tuple _ finalState <- runStateT state initialState
  let themeConfig = "%%{init: {'theme':'base', 'themeVariables': {'fontFamily':'monospace'}, 'look':'handDrawn', 'flowchart':{'curve':'basis'}}}%%\n"
  pure $ themeConfig <> "graph TD\n" <> finalState.mermaidCode <> generateStyleDefinitions
  where
    initialState = { nodeCounter: 0, mermaidCode: "" }

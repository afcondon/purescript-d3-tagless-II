-- | TreeBuilder3 AST Importer: PSD3.AST -> TreeBuilder tree
-- |
-- | Converts an existing PSD3.AST.Tree into the TreeBuilder3 representation,
-- | enabling visualization and editing of existing ASTs.
-- |
-- | This is the reverse of Converter.purs:
-- | - Converter: TreeBuilder tree -> AST.Tree (for code generation)
-- | - ASTImporter: AST.Tree -> TreeBuilder tree (for visualization/editing)
-- |
-- | Key insight: We lose function implementations but preserve AttrSource metadata,
-- | which is enough to reconstruct the code representation.
module TreeBuilder3.ASTImporter
  ( astToBuilderTree
  , ImportState
  , runImport
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, modify)
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tree (Tree) as DT
import Control.Comonad.Cofree (mkCofree)
import PSD3.AST as AST
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Internal.Behavior.Types (Behavior(..))
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..), BehaviorKind(..), DatumType(..))

-- | State for generating unique IDs during import
type ImportState = { nextId :: Int }

-- | Run the import with fresh state, returning the tree
runImport :: forall datum. AST.Tree datum -> DT.Tree TreeNode
runImport ast = evalState (astToBuilderTree ast 0) { nextId: 1 }

-- | Convert an AST.Tree to a TreeBuilder3 tree
-- | Takes the current depth for layout purposes
astToBuilderTree :: forall datum. AST.Tree datum -> Int -> State ImportState (DT.Tree TreeNode)
astToBuilderTree ast depth = case ast of

  AST.Node node -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId (NodeElem node.elemType) node.name Nothing depth

    -- Convert attrs to child nodes
    attrChildren <- convertAttrs node.attrs (depth + 1)

    -- Convert behaviors to child nodes
    behaviorChildren <- convertBehaviors node.behaviors (depth + 1)

    -- Recursively convert structural children
    structChildren <- convertChildren node.children (depth + 1)

    -- Combine all children: attrs first, then behaviors, then structural
    let allChildren = attrChildren <> behaviorChildren <> structChildren

    pure $ mkCofree treeNode (List.fromFoldable allChildren)

  AST.Join joinSpec -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId NodeJoin (Just joinSpec.name) (Just joinSpec.key) depth

    -- Convert template (evaluate with placeholder to get the tree structure)
    -- Since we don't have actual data, we create a dummy and hope AttrSource captures what we need
    templateChildren <- case Array.head joinSpec.joinData of
      Just firstDatum -> do
        let templateTree = joinSpec.template firstDatum
        templateChild <- astToBuilderTree templateTree (depth + 1)
        pure [templateChild]
      Nothing -> pure []

    pure $ mkCofree treeNode (List.fromFoldable templateChildren)

  AST.NestedJoin nestedSpec -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId NodeNestedJoin (Just nestedSpec.name) (Just nestedSpec.key) depth

    templateChildren <- case Array.head nestedSpec.joinData of
      Just firstDatum ->
        case Array.head (nestedSpec.decompose firstDatum) of
          Just innerDatum -> do
            let templateTree = nestedSpec.template innerDatum
            templateChild <- astToBuilderTree templateTree (depth + 1)
            pure [templateChild]
          Nothing -> pure []
      Nothing -> pure []

    pure $ mkCofree treeNode (List.fromFoldable templateChildren)

  AST.UpdateJoin sceneSpec -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId NodeUpdateJoin (Just sceneSpec.name) (Just sceneSpec.key) depth

    -- For UpdateJoin, the template element contains the GUP phases as children
    templateChildren <- case Array.head sceneSpec.joinData of
      Just firstDatum -> do
        let templateTree = sceneSpec.template firstDatum
        -- Convert template, then add GUP phases as children of the template element
        templateWithGUP <- addGUPPhasesToTemplate templateTree sceneSpec.behaviors (depth + 1)
        pure [templateWithGUP]
      Nothing -> pure []

    pure $ mkCofree treeNode (List.fromFoldable templateChildren)

  AST.UpdateNestedJoin sceneNestedSpec -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId NodeUpdateNestedJoin (Just sceneNestedSpec.name) (Just sceneNestedSpec.key) depth

    templateChildren <- case Array.head sceneNestedSpec.joinData of
      Just firstDatum ->
        case Array.head (sceneNestedSpec.decompose firstDatum) of
          Just innerDatum -> do
            let templateTree = sceneNestedSpec.template innerDatum
            templateWithGUP <- addGUPPhasesToTemplate templateTree sceneNestedSpec.behaviors (depth + 1)
            pure [templateWithGUP]
          Nothing -> pure []
      Nothing -> pure []

    pure $ mkCofree treeNode (List.fromFoldable templateChildren)

  AST.ConditionalRender { cases } -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId NodeConditionalRender Nothing Nothing depth
    -- Each case is a function, can't import details
    pure $ mkCofree treeNode List.Nil

  AST.LocalCoordSpace { child } -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId NodeLocalCoordSpace Nothing Nothing depth
    childTree <- astToBuilderTree child (depth + 1)
    pure $ mkCofree treeNode (List.singleton childTree)

-- | Add GUP phase nodes (Enter/Update/Exit) as children of a template element
addGUPPhasesToTemplate :: forall datum. AST.Tree datum -> AST.GUPBehaviors datum -> Int -> State ImportState (DT.Tree TreeNode)
addGUPPhasesToTemplate templateAst behaviors depth = case templateAst of
  AST.Node node -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId (NodeElem node.elemType) node.name Nothing depth

    -- Convert element's own attrs and behaviors
    attrChildren <- convertAttrs node.attrs (depth + 1)
    behaviorChildren <- convertBehaviors node.behaviors (depth + 1)
    structChildren <- convertChildren node.children (depth + 1)

    -- Add GUP phase children
    gupChildren <- convertGUPBehaviors behaviors (depth + 1)

    let allChildren = attrChildren <> behaviorChildren <> gupChildren <> structChildren
    pure $ mkCofree treeNode (List.fromFoldable allChildren)

  -- For non-Node templates, just convert normally (shouldn't happen in practice)
  other -> astToBuilderTree other depth

-- | Convert GUP behaviors to Enter/Update/Exit nodes with their attrs
convertGUPBehaviors :: forall datum. AST.GUPBehaviors datum -> Int -> State ImportState (Array (DT.Tree TreeNode))
convertGUPBehaviors behaviors depth = do
  enterNode <- convertPhase NodeEnter behaviors.enter depth
  updateNode <- convertPhase NodeUpdate behaviors.update depth
  exitNode <- convertPhase NodeExit behaviors.exit depth
  pure $ Array.catMaybes [enterNode, updateNode, exitNode]

-- | Convert a single GUP phase to a node (if it has content)
convertPhase :: forall datum. DslNodeType -> Maybe (AST.PhaseBehavior datum) -> Int -> State ImportState (Maybe (DT.Tree TreeNode))
convertPhase phaseType maybeBehavior depth = case maybeBehavior of
  Nothing -> pure Nothing
  Just behavior -> do
    nodeId <- freshId
    let treeNode = mkTreeNode nodeId phaseType Nothing Nothing depth
    attrChildren <- convertAttrs behavior.attrs (depth + 1)
    pure $ Just $ mkCofree treeNode (List.fromFoldable attrChildren)

-- | Convert an array of Attributes to TreeNode children
convertAttrs :: forall datum. Array (Attribute datum) -> Int -> State ImportState (Array (DT.Tree TreeNode))
convertAttrs attrs depth = do
  Array.foldM (\acc attr -> do
    attrNode <- convertAttr attr depth
    pure $ acc <> [attrNode]
  ) [] attrs

-- | Convert a single Attribute to a TreeNode
convertAttr :: forall datum. Attribute datum -> Int -> State ImportState (DT.Tree TreeNode)
convertAttr attr depth = do
  nodeId <- freshId
  let attrKind = attributeToAttrKind attr
  let treeNode = mkTreeNode nodeId (NodeAttr attrKind) Nothing Nothing depth
  pure $ mkCofree treeNode List.Nil

-- | Convert an Attribute to AttrKind (extracting source metadata)
attributeToAttrKind :: forall datum. Attribute datum -> AttrKind
attributeToAttrKind = case _ of
  StaticAttr (AttributeName name) value ->
    AttrStatic name (showAttrValue value)

  DataAttr (AttributeName name) source _ ->
    case source of
      FieldSource field -> AttrField name field
      ExprSource expr -> AttrExpr name expr
      StaticSource s -> AttrStatic name s
      _ -> AttrExpr name "d.<?>"  -- Unknown source

  IndexedAttr (AttributeName name) _ _ ->
    AttrIndex name

-- | Convert an array of Behaviors to TreeNode children
convertBehaviors :: forall datum. Array (Behavior datum) -> Int -> State ImportState (Array (DT.Tree TreeNode))
convertBehaviors behaviors depth = do
  Array.foldM (\acc behavior -> do
    behaviorNode <- convertBehavior behavior depth
    pure $ acc <> [behaviorNode]
  ) [] behaviors

-- | Convert a single Behavior to a TreeNode
convertBehavior :: forall datum. Behavior datum -> Int -> State ImportState (DT.Tree TreeNode)
convertBehavior behavior depth = do
  nodeId <- freshId
  let behaviorKind = behaviorToBehaviorKind behavior
  let treeNode = mkTreeNode nodeId (NodeBehavior behaviorKind) Nothing Nothing depth
  pure $ mkCofree treeNode List.Nil

-- | Convert a Behavior to BehaviorKind
behaviorToBehaviorKind :: forall datum. Behavior datum -> BehaviorKind
behaviorToBehaviorKind = case _ of
  Zoom _ -> BehaviorZoom
  Drag _ -> BehaviorDrag
  ClickWithDatum _ -> BehaviorClick
  MouseEnter _ -> BehaviorHover
  _ -> BehaviorClick  -- Default for other behaviors

-- | Convert children recursively
convertChildren :: forall datum. Array (AST.Tree datum) -> Int -> State ImportState (Array (DT.Tree TreeNode))
convertChildren children depth = do
  Array.foldM (\acc child -> do
    childNode <- astToBuilderTree child depth
    pure $ acc <> [childNode]
  ) [] children

-- | Generate a fresh unique ID
freshId :: State ImportState Int
freshId = do
  state <- get
  _ <- modify \s -> s { nextId = s.nextId + 1 }
  pure state.nextId

-- | Create a TreeNode with default position (layout will fix this)
-- | datumType starts as TypeUnit; propagateTypes will compute actual types
mkTreeNode :: Int -> DslNodeType -> Maybe String -> Maybe String -> Int -> TreeNode
mkTreeNode id nodeType name key depth =
  { id
  , nodeType
  , name
  , key
  , datumType: TypeUnit -- Will be computed by propagateTypes
  , x: 0.0
  , y: 0.0
  , depth
  }

-- | Show an AttributeValue as a string
showAttrValue :: AttributeValue -> String
showAttrValue = case _ of
  StringValue s -> s
  NumberValue n -> show n
  BooleanValue b -> show b

-- | TreeBuilder3 Converter: TreeBuilder tree -> PSD3.AST
-- |
-- | Converts the uniform Tree TreeNode representation used by TreeBuilder3
-- | into the heterogeneous PSD3.AST.Tree that interpreters understand.
-- |
-- | Key differences between representations:
-- | - TreeBuilder3: Uniform nodes, attrs/behaviors are child nodes
-- | - PSD3.AST: Heterogeneous ADT, attrs/behaviors are inside Node records
-- |
-- | This module bridges that gap, enabling live code generation from the builder.
module TreeBuilder3.Converter
  ( builderTreeToAST
  , ASTConversionError(..)
  ) where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tree (Tree) as DT
import PSD3.AST as AST
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Internal.Behavior.Types (Behavior(Zoom, Drag, ClickWithDatum, MouseEnter), defaultZoom, defaultDrag, ScaleExtent(..))
import PSD3.Internal.Selection.Types (ElementType)

-- Import the TreeBuilder3 types (they're in App.purs)
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..), BehaviorKind(..))

-- | Conversion errors
data ASTConversionError
  = EmptyTree
  | PendingNodeFound String  -- Node still awaiting user input
  | InvalidStructure String  -- Grammar violation

instance Show ASTConversionError where
  show EmptyTree = "Empty tree"
  show (PendingNodeFound msg) = "Pending node found: " <> msg
  show (InvalidStructure msg) = "Invalid structure: " <> msg

-- | Convert a TreeBuilder3 tree to PSD3.AST
-- |
-- | The resulting AST has `Unit` as datum type since TreeBuilder3 doesn't
-- | deal with actual data - it's just building tree structure.
-- | For code generation, we use placeholder data.
builderTreeToAST :: DT.Tree TreeNode -> Either ASTConversionError (AST.Tree Unit)
builderTreeToAST dtree =
  let
    node = head dtree
    children = tail dtree
  in
    convertNode node children

-- | Convert a single node and its children
convertNode :: TreeNode -> List (DT.Tree TreeNode) -> Either ASTConversionError (AST.Tree Unit)
convertNode node children = case node.nodeType of

  -- Element nodes become AST.Node
  NodeElem elemType -> do
    let childList = Array.fromFoldable children
    let { attrs, behaviors, structuralChildren } = partitionChildren childList
    convertedAttrs <- convertAttrs attrs
    convertedBehaviors <- convertBehaviors behaviors
    convertedChildren <- convertStructuralChildren structuralChildren
    pure $ AST.Node
      { name: Nothing  -- TreeBuilder3 doesn't track names yet
      , elemType
      , attrs: convertedAttrs
      , behaviors: convertedBehaviors
      , children: convertedChildren
      }

  -- Join nodes
  NodeJoin -> do
    let childList = Array.fromFoldable children
    case Array.head childList of
      Nothing ->
        -- Empty join - valid but no template yet
        pure $ AST.Join
          { name: "join"
          , key: "g"
          , joinData: []
          , template: \_ -> AST.elem AST.Group []
          }
      Just templateChild -> do
        templateAST <- builderTreeToAST templateChild
        pure $ AST.Join
          { name: "join"
          , key: getKeyFromTemplate templateChild
          , joinData: [unit]  -- Placeholder data
          , template: \_ -> templateAST
          }

  NodeNestedJoin -> do
    let childList = Array.fromFoldable children
    case Array.head childList of
      Nothing ->
        pure $ AST.NestedJoin
          { name: "nestedJoin"
          , key: "g"
          , joinData: []
          , decompose: \_ -> []
          , template: \_ -> AST.elem AST.Group []
          }
      Just templateChild -> do
        templateAST <- builderTreeToAST templateChild
        pure $ AST.NestedJoin
          { name: "nestedJoin"
          , key: getKeyFromTemplate templateChild
          , joinData: [unit]
          , decompose: \_ -> [unit]
          , template: \_ -> templateAST
          }

  NodeUpdateJoin -> do
    let childList = Array.fromFoldable children
    let { gupPhases, templateChild } = partitionUpdateJoinChildren childList
    case templateChild of
      Nothing ->
        pure $ AST.UpdateJoin
          { name: "updateJoin"
          , key: "g"
          , joinData: []
          , template: \_ -> AST.elem AST.Group []
          , keyFn: Nothing
          , behaviors: { enter: Nothing, update: Nothing, exit: Nothing }
          }
      Just tc -> do
        templateAST <- builderTreeToAST tc
        behaviors <- convertGUPPhases gupPhases
        pure $ AST.UpdateJoin
          { name: "updateJoin"
          , key: getKeyFromTemplate tc
          , joinData: [unit]
          , template: \_ -> templateAST
          , keyFn: Nothing
          , behaviors
          }

  NodeUpdateNestedJoin -> do
    let childList = Array.fromFoldable children
    let { gupPhases, templateChild } = partitionUpdateJoinChildren childList
    case templateChild of
      Nothing ->
        pure $ AST.UpdateNestedJoin
          { name: "updateNestedJoin"
          , key: "g"
          , joinData: []
          , decompose: \_ -> []
          , template: \_ -> AST.elem AST.Group []
          , behaviors: { enter: Nothing, update: Nothing, exit: Nothing }
          }
      Just tc -> do
        templateAST <- builderTreeToAST tc
        behaviors <- convertGUPPhases gupPhases
        pure $ AST.UpdateNestedJoin
          { name: "updateNestedJoin"
          , key: getKeyFromTemplate tc
          , joinData: [unit]
          , decompose: \_ -> [unit]
          , template: \_ -> templateAST
          , behaviors
          }

  -- Attr nodes shouldn't be at top level
  NodeAttr _ -> Left $ InvalidStructure "Attr node at top level"

  -- Behavior nodes shouldn't be at top level
  NodeBehavior _ -> Left $ InvalidStructure "Behavior node at top level"

  -- GUP phase nodes shouldn't be at top level
  NodeEnter -> Left $ InvalidStructure "Enter node at top level"
  NodeUpdate -> Left $ InvalidStructure "Update node at top level"
  NodeExit -> Left $ InvalidStructure "Exit node at top level"

  -- Pending nodes are incomplete
  PendingElement -> Left $ PendingNodeFound "Element awaiting type"
  PendingAttr -> Left $ PendingNodeFound "Attr awaiting name"
  PendingAttrValue name -> Left $ PendingNodeFound $ "Attr '" <> name <> "' awaiting value"
  PendingBehavior -> Left $ PendingNodeFound "Behavior awaiting type"

-- | Partition children into attrs, behaviors, and structural (element/join) children
partitionChildren :: Array (DT.Tree TreeNode) ->
  { attrs :: Array (DT.Tree TreeNode)
  , behaviors :: Array (DT.Tree TreeNode)
  , structuralChildren :: Array (DT.Tree TreeNode)
  }
partitionChildren children =
  foldl categorize { attrs: [], behaviors: [], structuralChildren: [] } children
  where
  categorize acc child =
    let node = head child
    in case node.nodeType of
      NodeAttr _ -> acc { attrs = acc.attrs <> [child] }
      NodeBehavior _ -> acc { behaviors = acc.behaviors <> [child] }
      _ -> acc { structuralChildren = acc.structuralChildren <> [child] }

-- | Partition UpdateJoin children into GUP phases and template
partitionUpdateJoinChildren :: Array (DT.Tree TreeNode) ->
  { gupPhases :: { enter :: Maybe (DT.Tree TreeNode), update :: Maybe (DT.Tree TreeNode), exit :: Maybe (DT.Tree TreeNode) }
  , templateChild :: Maybe (DT.Tree TreeNode)
  }
partitionUpdateJoinChildren children =
  foldl categorize
    { gupPhases: { enter: Nothing, update: Nothing, exit: Nothing }
    , templateChild: Nothing
    }
    children
  where
  categorize acc child =
    let node = head child
    in case node.nodeType of
      NodeEnter -> acc { gupPhases = acc.gupPhases { enter = Just child } }
      NodeUpdate -> acc { gupPhases = acc.gupPhases { update = Just child } }
      NodeExit -> acc { gupPhases = acc.gupPhases { exit = Just child } }
      NodeElem _ -> acc { templateChild = Just child }
      _ -> acc  -- Ignore other children

-- | Convert GUP phase nodes to PhaseBehavior records
convertGUPPhases ::
  { enter :: Maybe (DT.Tree TreeNode)
  , update :: Maybe (DT.Tree TreeNode)
  , exit :: Maybe (DT.Tree TreeNode)
  } ->
  Either ASTConversionError (AST.GUPBehaviors Unit)
convertGUPPhases phases = do
  enter <- convertPhaseBehavior phases.enter
  update <- convertPhaseBehavior phases.update
  exit <- convertPhaseBehavior phases.exit
  pure { enter, update, exit }

-- | Convert a single GUP phase node to PhaseBehavior
convertPhaseBehavior :: Maybe (DT.Tree TreeNode) -> Either ASTConversionError (Maybe (AST.PhaseBehavior Unit))
convertPhaseBehavior Nothing = Right Nothing
convertPhaseBehavior (Just phaseTree) = do
  let children = Array.fromFoldable $ tail phaseTree
  let attrChildren = Array.filter (\c -> isAttrNode (head c).nodeType) children
  attrs <- convertAttrs attrChildren
  if Array.null attrs
    then Right Nothing
    else Right $ Just { attrs, transition: Nothing }

-- | Check if a node type is an attr
isAttrNode :: DslNodeType -> Boolean
isAttrNode (NodeAttr _) = true
isAttrNode (PendingAttr) = true
isAttrNode (PendingAttrValue _) = true
isAttrNode _ = false

-- | Convert attr child nodes to PSD3 Attributes
convertAttrs :: Array (DT.Tree TreeNode) -> Either ASTConversionError (Array (Attribute Unit))
convertAttrs attrTrees = do
  Array.foldM convertAttr [] attrTrees
  where
  convertAttr acc attrTree =
    let node = head attrTree
    in case node.nodeType of
      NodeAttr kind -> Right $ acc <> [attrKindToAttribute kind]
      PendingAttr -> Left $ PendingNodeFound "Attr awaiting name"
      PendingAttrValue name -> Left $ PendingNodeFound $ "Attr '" <> name <> "' awaiting value"
      _ -> Right acc  -- Skip non-attr nodes

-- | Convert AttrKind to a PSD3 Attribute
attrKindToAttribute :: AttrKind -> Attribute Unit
attrKindToAttribute = case _ of
  AttrStatic name value ->
    StaticAttr (AttributeName name) (StringValue value)
  AttrField name field ->
    DataAttr (AttributeName name) (FieldSource field) (\_ -> StringValue $ "d." <> field)
  AttrExpr name expr ->
    DataAttr (AttributeName name) (ExprSource expr) (\_ -> StringValue expr)
  AttrIndex name ->
    IndexedAttr (AttributeName name) IndexSource (\_ _ -> NumberValue 0.0)

-- | Convert behavior child nodes to PSD3 Behaviors
convertBehaviors :: Array (DT.Tree TreeNode) -> Either ASTConversionError (Array (Behavior Unit))
convertBehaviors behaviorTrees = do
  Array.foldM convertBehavior [] behaviorTrees
  where
  convertBehavior acc behaviorTree =
    let node = head behaviorTree
    in case node.nodeType of
      NodeBehavior kind -> Right $ acc <> [behaviorKindToBehavior kind]
      PendingBehavior -> Left $ PendingNodeFound "Behavior awaiting type"
      _ -> Right acc

-- | Convert BehaviorKind to a PSD3 Behavior (placeholder implementations)
behaviorKindToBehavior :: BehaviorKind -> Behavior Unit
behaviorKindToBehavior = case _ of
  BehaviorZoom -> Zoom $ defaultZoom (ScaleExtent 0.1 10.0) ".zoom-group"
  BehaviorDrag -> Drag defaultDrag
  BehaviorClick -> ClickWithDatum (\_ -> pure unit)
  BehaviorHover -> MouseEnter (\_ -> pure unit)  -- Hover represented as MouseEnter

-- | Recursively convert structural children (elements and joins)
convertStructuralChildren :: Array (DT.Tree TreeNode) -> Either ASTConversionError (Array (AST.Tree Unit))
convertStructuralChildren children =
  Array.foldM (\acc child -> do
    converted <- builderTreeToAST child
    pure $ acc <> [converted]
  ) [] children

-- | Extract element key from template tree (for join's key field)
getKeyFromTemplate :: DT.Tree TreeNode -> String
getKeyFromTemplate template =
  let node = head template
  in case node.nodeType of
    NodeElem elemType -> elementTypeToKey elemType
    _ -> "g"  -- Default to group

-- | Convert ElementType to D3 join key string
elementTypeToKey :: ElementType -> String
elementTypeToKey = case _ of
  AST.SVG -> "svg"
  AST.Group -> "g"
  AST.Circle -> "circle"
  AST.Rect -> "rect"
  AST.Path -> "path"
  AST.Line -> "line"
  AST.Text -> "text"
  AST.Div -> "div"
  AST.Span -> "span"
  AST.Table -> "table"
  AST.Tbody -> "tbody"
  AST.Thead -> "thead"
  AST.Tr -> "tr"
  AST.Td -> "td"
  AST.Th -> "th"
  AST.Defs -> "defs"
  AST.LinearGradient -> "linearGradient"
  AST.Stop -> "stop"

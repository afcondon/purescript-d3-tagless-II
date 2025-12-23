module EmmetParser.Converter
  ( convert
  , convertToTree
  ) where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tree (Tree, mkTree)
import EmmetParser.Types (EmmetExpr(..), EmmetNode(..), ElementType(..), JoinType(..), Attribute(..))
import PSD3.Internal.Selection.Types (ElementType(..)) as PSD3
import TreeBuilder3.TypePropagation (builtinTypes)
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..), DatumType(..))
import Data.Map as Map

-- | Converter state tracking IDs and depth
type ConverterState =
  { nextId :: Int
  , depth :: Int
  }

-- | Result of conversion
type ConvertResult =
  { trees :: Array (Tree TreeNode)
  , state :: ConverterState
  }

-- | Convert an Emmet expression to a TreeBuilder3 tree
-- | Returns a tree with an SVG root and the converted structure as children
convertToTree :: EmmetExpr -> Tree TreeNode
convertToTree expr =
  let
    -- Create SVG root node (ID 0)
    svgRoot =
      { id: 0
      , nodeType: NodeElem PSD3.SVG
      , name: Just "svg"
      , key: Nothing
      , datumType: TypeUnit
      , x: 0.0
      , y: 0.0
      , depth: 0
      }

    -- Convert the expression starting at ID 1, depth 1
    result = convertExpr { nextId: 1, depth: 1 } expr
  in
    mkTree svgRoot (List.fromFoldable result.trees)

-- | Convert an Emmet expression to a tree (internal)
convert :: EmmetExpr -> Array (Tree TreeNode)
convert expr =
  let result = convertExpr { nextId: 1, depth: 1 } expr
  in result.trees

-- | Convert an expression with state
convertExpr :: ConverterState -> EmmetExpr -> ConvertResult
convertExpr state = case _ of
  Single node children ->
    convertSingle state node children

  Sibling e1 e2 -> do
    let r1 = convertExpr state e1
    let r2 = convertExpr r1.state e2
    { trees: Array.concat [r1.trees, r2.trees]
    , state: r2.state
    }

  Repeat expr n ->
    convertRepeat state expr n

-- | Convert a single node with children
convertSingle :: ConverterState -> EmmetNode -> Array EmmetExpr -> ConvertResult
convertSingle state node childExprs =
  let
    nodeId = state.nextId
    treeNode = createTreeNode state.depth nodeId node
    childState = { nextId: state.nextId + 1, depth: state.depth + 1 }

    convertChildren :: ConverterState -> Array EmmetExpr -> ConvertResult
    convertChildren st exprs =
      Array.foldl
        (\acc expr ->
          let result = convertExpr acc.state expr
          in { trees: Array.concat [acc.trees, result.trees]
             , state: result.state
             }
        )
        { trees: [], state: st }
        exprs
  in
    case node of
      ElemNode _ attrs _ ->
        let
          attrResult = convertAttributes childState attrs
          elemResult = convertChildren attrResult.state childExprs
          allChildren = Array.concat [attrResult.trees, elemResult.trees]
        in
          { trees: [mkTree treeNode (List.fromFoldable allChildren)]
          , state: elemResult.state
          }

      JoinNode _ typeName attrs _ ->
        let
          attrResult = convertAttributes childState attrs
          elemResult = convertChildren attrResult.state childExprs
          allChildren = Array.concat [attrResult.trees, elemResult.trees]
          typedNode = treeNode { datumType = lookupType typeName }
        in
          { trees: [mkTree typedNode (List.fromFoldable allChildren)]
          , state: elemResult.state
          }

-- | Convert attributes to badge nodes
convertAttributes :: ConverterState -> Array Attribute -> ConvertResult
convertAttributes state attrs =
  Array.foldl
    (\acc attr ->
      let attrNode = createAttributeNode acc.state.depth acc.state.nextId attr
      in { trees: Array.snoc acc.trees (mkTree attrNode List.Nil)
         , state: { nextId: acc.state.nextId + 1, depth: acc.state.depth }
         }
    )
    { trees: [], state }
    attrs

-- | Convert a repeated expression
convertRepeat :: ConverterState -> EmmetExpr -> Int -> ConvertResult
convertRepeat state expr n =
  let
    indices = Array.range 0 (n - 1)
  in
    Array.foldl
      (\acc _ ->
        let result = convertExpr acc.state expr
        in { trees: Array.concat [acc.trees, result.trees]
           , state: result.state
           }
      )
      { trees: [], state }
      indices

-- | Create a TreeNode from an EmmetNode
createTreeNode :: Int -> Int -> EmmetNode -> TreeNode
createTreeNode depth nodeId = case _ of
  ElemNode elemType _attrs name ->
    { id: nodeId
    , nodeType: NodeElem (convertElementType elemType)
    , name
    , key: Nothing
    , datumType: TypeUnit  -- Will be updated by type propagation
    , x: 0.0
    , y: 0.0
    , depth
    }

  JoinNode joinType _typeName _attrs name ->
    { id: nodeId
    , nodeType: convertJoinType joinType
    , name
    , key: Nothing
    , datumType: TypeUnknown  -- Will be updated below
    , x: 0.0
    , y: 0.0
    , depth
    }

-- | Create an attribute badge node
createAttributeNode :: Int -> Int -> Attribute -> TreeNode
createAttributeNode depth nodeId attr =
  { id: nodeId
  , nodeType: NodeAttr (convertAttribute attr)
  , name: Nothing
  , key: Nothing
  , datumType: TypeUnit
  , x: 0.0
  , y: 0.0
  , depth
  }

-- | Convert EmmetParser ElementType to PSD3 ElementType
convertElementType :: ElementType -> PSD3.ElementType
convertElementType = case _ of
  EGroup -> PSD3.Group
  ECircle -> PSD3.Circle
  ERect -> PSD3.Rect
  EPath -> PSD3.Path
  ELine -> PSD3.Line
  EText -> PSD3.Text

-- | Convert EmmetParser JoinType to DslNodeType
convertJoinType :: JoinType -> DslNodeType
convertJoinType = case _ of
  SimpleJoin -> NodeJoin
  NestedJoin -> NodeNestedJoin
  UpdateJoin -> NodeUpdateJoin
  UpdateNestedJoin -> NodeUpdateNestedJoin

-- | Convert EmmetParser Attribute to TreeBuilder3 AttrKind
convertAttribute :: Attribute -> AttrKind
convertAttribute = case _ of
  StaticAttr name value -> AttrStatic name value
  FieldAttr name field -> AttrField name field
  IndexAttr name -> AttrIndex name

-- | Look up a type by name in the builtin types
lookupType :: String -> DatumType
lookupType typeName =
  case Map.lookup typeName builtinTypes of
    Just dtype -> dtype
    Nothing -> TypeUnknown

module EmmetParser.Printer
  ( printEmmet
  , treeToEmmet
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tree (Tree)
import Control.Comonad.Cofree (head, tail) as Cofree
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..), DatumType(..))
import PSD3.Internal.Selection.Types (ElementType(..))

-- | Convert a Tree TreeNode back to Emmet syntax
-- | This is the inverse of parseEmmet >>> convertToTree
-- | Note: convertToTree wraps everything in an SVG root, so we skip it
printEmmet :: Tree TreeNode -> String
printEmmet tree =
  let
    node = Cofree.head tree
    children = Array.fromFoldable $ Cofree.tail tree
  in
    -- Skip the SVG wrapper and print its children directly
    case node.nodeType of
      NodeElem SVG ->
        let structuralChildren = filterStructuralChildren children
        in case Array.length structuralChildren of
          0 -> ""
          1 -> case Array.head structuralChildren of
            Nothing -> ""
            Just child -> treeToEmmet child
          _ ->
            -- Multiple top-level siblings
            intercalate "+" (map treeToEmmet structuralChildren)
      _ ->
        treeToEmmet tree

-- | Convert tree to Emmet expression string
treeToEmmet :: Tree TreeNode -> String
treeToEmmet tree =
  let
    node = Cofree.head tree
    children = Array.fromFoldable $ Cofree.tail tree
  in
    case node.nodeType of
      -- Element nodes
      NodeElem elemType ->
        let
          elemChar = elementTypeToChar elemType
          attrs = collectAttributes children
          attrsStr = if Array.null attrs then "" else "[" <> intercalate "," attrs <> "]"
          structuralChildren = filterStructuralChildren children
        in
          elemChar <> attrsStr <> formatChildren structuralChildren

      -- Join nodes
      NodeJoin ->
        let
          typeName = extractTypeName node.datumType
          attrs = collectAttributes children
          attrsStr = if Array.null attrs then "" else "[" <> intercalate "," attrs <> "]"
          structuralChildren = filterStructuralChildren children
        in
          "j(" <> typeName <> ")" <> attrsStr <> formatChildren structuralChildren

      NodeNestedJoin ->
        let
          typeName = extractTypeName node.datumType
          attrs = collectAttributes children
          attrsStr = if Array.null attrs then "" else "[" <> intercalate "," attrs <> "]"
          structuralChildren = filterStructuralChildren children
        in
          "n(" <> typeName <> ")" <> attrsStr <> formatChildren structuralChildren

      NodeUpdateJoin ->
        let
          typeName = extractTypeName node.datumType
          attrs = collectAttributes children
          attrsStr = if Array.null attrs then "" else "[" <> intercalate "," attrs <> "]"
          structuralChildren = filterStructuralChildren children
        in
          "u(" <> typeName <> ")" <> attrsStr <> formatChildren structuralChildren

      NodeUpdateNestedJoin ->
        let
          typeName = extractTypeName node.datumType
          attrs = collectAttributes children
          attrsStr = if Array.null attrs then "" else "[" <> intercalate "," attrs <> "]"
          structuralChildren = filterStructuralChildren children
        in
          "x(" <> typeName <> ")" <> attrsStr <> formatChildren structuralChildren

      -- Skip badge nodes (attributes, behaviors) - they're handled by parent
      NodeAttr _ -> ""
      NodeBehavior _ -> ""

      -- Skip GUP phases, pending states
      _ -> ""

-- | Convert element type to single character
-- | Only supports simple Emmet element types (g, c, r, p, l, t)
-- | Other element types map to 'g' (group) as a fallback
elementTypeToChar :: ElementType -> String
elementTypeToChar = case _ of
  Circle -> "c"
  Rect -> "r"
  Path -> "p"
  Line -> "l"
  Text -> "t"
  _ -> "g"  -- SVG, Group, Defs, and all others map to 'g'

-- | Extract type name from DatumType
extractTypeName :: DatumType -> String
extractTypeName = case _ of
  TypeRecord name _ -> name
  TypeArray (TypeRecord name _) -> name
  _ -> "Unknown"

-- | Collect attribute strings from badge children
collectAttributes :: Array (Tree TreeNode) -> Array String
collectAttributes children =
  Array.mapMaybe nodeToAttribute children

-- | Convert an attribute node to its string representation
nodeToAttribute :: Tree TreeNode -> Maybe String
nodeToAttribute tree =
  let node = Cofree.head tree
  in case node.nodeType of
    NodeAttr attrKind -> Just $ formatAttribute attrKind
    _ -> Nothing

-- | Format an attribute kind as Emmet syntax
formatAttribute :: AttrKind -> String
formatAttribute = case _ of
  AttrStatic name value -> name <> "=" <> value
  AttrField name field -> name <> ":" <> field
  AttrIndex name -> name <> "@index"
  AttrExpr _ _ -> "" -- Not supported in Emmet yet

-- | Filter out badge children to get only structural children
filterStructuralChildren :: Array (Tree TreeNode) -> Array (Tree TreeNode)
filterStructuralChildren children =
  Array.filter isStructuralNode children

-- | Check if a node is structural (not a badge)
isStructuralNode :: Tree TreeNode -> Boolean
isStructuralNode tree =
  let node = Cofree.head tree
  in case node.nodeType of
    NodeAttr _ -> false
    NodeBehavior _ -> false
    _ -> true

-- | Format children with appropriate separators
-- | Single child: >child
-- | Multiple children (siblings): >child1+child2+child3
formatChildren :: Array (Tree TreeNode) -> String
formatChildren children = case Array.length children of
  0 -> ""
  1 -> case Array.head children of
    Nothing -> ""
    Just child -> ">" <> treeToEmmet child
  _ ->
    -- Multiple children are siblings
    ">" <> intercalate "+" (map treeToEmmet children)

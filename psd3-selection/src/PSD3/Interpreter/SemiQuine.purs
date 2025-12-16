-- | SemiQuine Interpreter
-- |
-- | Converts a BuilderTree to compilable PureScript code using the Tree API.
-- | Generates code that uses T.named, T.elem, T.withChild, and T.joinData.
-- |
-- | Named "SemiQuine" because it generates PureScript code from a data structure
-- | that represents PureScript code - a quine-ish concept, but not a true quine.
-- |
-- | Two variants:
-- | - `treeToCode` (this module): works on BuilderTree (fully inspectable)
-- | - `TreeToCode.treeToCode`: works on actual Tree datum (evaluates functions)
-- |   Import separately: `import PSD3.Interpreter.SemiQuine.TreeToCode`
module PSD3.Interpreter.SemiQuine
  ( treeToCode
  , module PSD3.Interpreter.SemiQuine.Types
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import PSD3.Interpreter.SemiQuine.Types (AttributeBinding, AttributeChoice(..), BuilderNode, BuilderTree(..), NodeId)

-- Re-export types
import PSD3.Interpreter.SemiQuine.Types (BuilderTree(..), BuilderNode, NodeId, AttributeChoice(..), AttributeBinding, emptyNode, defaultAttributesFor) as PSD3.Interpreter.SemiQuine.Types

-- =============================================================================
-- Code Generation
-- =============================================================================

-- | Convert a BuilderTree to compilable PureScript code
-- | Now generates v3Attr syntax for true round-trip capability
treeToCode :: BuilderTree -> String
treeToCode tree = String.joinWith "\n" $
  [ "-- Generated Tree API code (v3Attr)"
  , ""
  , "import PSD3.AST as T"
  , "import PSD3.Internal.Selection.Types (ElementType(..))"
  , "import PSD3.Expr.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)"
  , "import PSD3.Expr.Expr (lit, str)"
  , ""
  , "tree :: T.Tree Datum"
  , "tree ="
  ] <> indentLines 2 (nodeToCode tree)

-- Note: v3Attr imports are fixed (no per-attribute imports needed)

-- | Convert a tree node to code lines
nodeToCode :: BuilderTree -> Array String
nodeToCode = case _ of
  BNode node children ->
    case node.name of
      -- Named node
      Just name ->
        if Array.null children
          then namedLeafCode node name
          else namedBranchCode node name children
      -- Anonymous node
      Nothing ->
        if Array.null children
          then elemLeafCode node
          else elemBranchCode node children

  BDataJoin join ->
    joinCode join

-- | Named leaf node: T.named SVG "name" [attrs]
namedLeafCode :: BuilderNode -> String -> Array String
namedLeafCode node name =
  [ "T.named " <> elementType node.elementType <> " \"" <> name <> "\""
  , "  " <> attrsArray node.attributes
  ]

-- | Named branch node with children
namedBranchCode :: BuilderNode -> String -> Array BuilderTree -> Array String
namedBranchCode node name children =
  [ "T.named " <> elementType node.elementType <> " \"" <> name <> "\""
  , "  " <> attrsArray node.attributes
  ] <> childrenCode children

-- | Anonymous leaf: T.elem Circle [attrs]
elemLeafCode :: BuilderNode -> Array String
elemLeafCode node =
  [ "T.elem " <> elementType node.elementType
  , "  " <> attrsArray node.attributes
  ]

-- | Anonymous branch with children
elemBranchCode :: BuilderNode -> Array BuilderTree -> Array String
elemBranchCode node children =
  [ "T.elem " <> elementType node.elementType
  , "  " <> attrsArray node.attributes
  ] <> childrenCode children

-- | Generate children code using withChild or withChildren
childrenCode :: Array BuilderTree -> Array String
childrenCode children =
  case Array.length children of
    1 -> case Array.head children of
      Just child ->
        [ "  `T.withChild`" ] <>
        indentLines 4 (wrapInParens (nodeToCode child))
      Nothing -> []
    _ ->
      [ "  `T.withChildren`" ] <>
      [ "    [ " <> String.joinWith "\n    , " (map (String.joinWith " " <<< nodeToCode) children) <> "\n    ]" ]

-- | Data join code: T.joinData "name" "key" data $ \d -> ...
joinCode :: { id :: Int, name :: String, elementType :: String, template :: BuilderNode, expanded :: Boolean } -> Array String
joinCode join =
  [ "T.joinData \"" <> join.name <> "\" \"" <> join.elementType <> "\" data_ $ \\d ->"
  , "  T.elem " <> elementType join.elementType
  , "    " <> templateAttrsArray join.template.attributes
  ]

-- | Generate element type constructor
elementType :: String -> String
elementType = case _ of
  "svg" -> "SVG"
  "group" -> "Group"
  "circle" -> "Circle"
  "rect" -> "Rect"
  "line" -> "Line"
  "text" -> "Text"
  "path" -> "Path"
  t -> t

-- | Generate attributes array (with line breaks for readability)
attrsArray :: Array AttributeBinding -> String
attrsArray attrs =
  if Array.length attrs <= 2
    then "[ " <> String.joinWith ", " (map attrCode attrs) <> " ]"
    else "[ " <> String.joinWith "\n  , " (map attrCode attrs) <> "\n  ]"

-- | Generate template attributes (with datum accessors, line breaks)
templateAttrsArray :: Array AttributeBinding -> String
templateAttrsArray attrs =
  if Array.length attrs <= 2
    then "[ " <> String.joinWith ", " (map templateAttrCode attrs) <> " ]"
    else "[ " <> String.joinWith "\n    , " (map templateAttrCode attrs) <> "\n    ]"

-- | Generate a single attribute using v3Attr syntax
attrCode :: AttributeBinding -> String
attrCode binding = choiceToV3Attr binding.attrName binding.choice

-- | Generate template attribute (for data joins, using datum accessors)
templateAttrCode :: AttributeBinding -> String
templateAttrCode binding = templateChoiceToV3Attr binding.attrName binding.choice

-- | Determine if an attribute is string-typed (for choosing v3AttrStr vs v3Attr)
isStringAttr :: String -> Boolean
isStringAttr = case _ of
  "fill" -> true
  "stroke" -> true
  "text" -> true
  "textContent" -> true
  "class" -> true
  "id" -> true
  "text-anchor" -> true
  "font-family" -> true
  _ -> false

-- | Convert an AttributeChoice to v3Attr expression (for static nodes)
choiceToV3Attr :: String -> AttributeChoice -> String
choiceToV3Attr attrName = case _ of
  ConstantNumber n ->
    "v3Attr \"" <> attrName <> "\" (lit " <> show n <> ")"
  ConstantString s ->
    "v3AttrStr \"" <> attrName <> "\" (str \"" <> s <> "\")"
  FromField field ->
    if isStringAttr attrName
      then "v3AttrFnStr \"" <> attrName <> "\" (_." <> field <> " :: Datum -> String)"
      else "v3AttrFn \"" <> attrName <> "\" (_." <> field <> " :: Datum -> Number)"
  IndexBased ->
    "v3AttrFn \"" <> attrName <> "\" (\\d -> toNumber d.index)"
  Computed expr ->
    -- For computed expressions, wrap in v3Attr with the expression
    if isStringAttr attrName
      then "v3AttrStr \"" <> attrName <> "\" (str (" <> expr <> "))"
      else "v3Attr \"" <> attrName <> "\" (lit (" <> expr <> "))"

-- | Convert an AttributeChoice for template (datum accessor) using v3Attr
templateChoiceToV3Attr :: String -> AttributeChoice -> String
templateChoiceToV3Attr attrName = case _ of
  ConstantNumber n ->
    "v3Attr \"" <> attrName <> "\" (lit " <> show n <> ")"
  ConstantString s ->
    "v3AttrStr \"" <> attrName <> "\" (str \"" <> s <> "\")"
  FromField field ->
    if isStringAttr attrName
      then "v3AttrFnStr \"" <> attrName <> "\" (_." <> field <> " :: Datum -> String)"
      else "v3AttrFn \"" <> attrName <> "\" (_." <> field <> " :: Datum -> Number)"
  IndexBased ->
    "v3AttrFn \"" <> attrName <> "\" (\\d -> toNumber d.index)"
  Computed expr ->
    if isStringAttr attrName
      then "v3AttrFnStr \"" <> attrName <> "\" (\\d -> " <> expr <> ")"
      else "v3AttrFn \"" <> attrName <> "\" (\\d -> " <> expr <> ")"

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Indent lines by n spaces
indentLines :: Int -> Array String -> Array String
indentLines n = map (\line -> if String.null line then "" else spaces n <> line)

-- | Generate n spaces
spaces :: Int -> String
spaces n = String.joinWith "" (Array.replicate n " ")

-- | Wrap code lines in parentheses if multiple lines
wrapInParens :: Array String -> Array String
wrapInParens lines = case Array.length lines of
  1 -> lines
  _ -> case Array.head lines, Array.last lines of
    Just first, Just last ->
      [ "(" <> first ] <>
      (Array.drop 1 (Array.dropEnd 1 lines)) <>
      [ last <> ")" ]
    _, _ -> lines

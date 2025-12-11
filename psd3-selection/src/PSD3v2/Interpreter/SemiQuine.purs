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
-- |   Import separately: `import PSD3v2.Interpreter.SemiQuine.TreeToCode`
module PSD3v2.Interpreter.SemiQuine
  ( treeToCode
  , module PSD3v2.Interpreter.SemiQuine.Types
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import PSD3v2.Interpreter.SemiQuine.Types (AttributeBinding, AttributeChoice(..), BuilderNode, BuilderTree(..), NodeId)

-- Re-export types
import PSD3v2.Interpreter.SemiQuine.Types (BuilderTree(..), BuilderNode, NodeId, AttributeChoice(..), AttributeBinding, emptyNode, defaultAttributesFor) as PSD3v2.Interpreter.SemiQuine.Types

-- =============================================================================
-- Code Generation
-- =============================================================================

-- | Convert a BuilderTree to compilable PureScript code
treeToCode :: BuilderTree -> String
treeToCode tree = String.joinWith "\n" $
  [ "-- Generated Tree API code"
  , ""
  , "import PSD3v2.VizTree.Tree as T"
  , "import PSD3v2.Selection.Types (ElementType(..))"
  , "import PSD3v2.Attribute.Types (" <> attributeImports tree <> ")"
  , ""
  , "tree :: T.Tree Datum"
  , "tree ="
  ] <> indentLines 2 (nodeToCode tree)

-- | Collect all unique attribute names used in the tree for imports
attributeImports :: BuilderTree -> String
attributeImports tree =
  let attrs = collectAttrs tree
      unique = Array.nub attrs
  in String.joinWith ", " unique

collectAttrs :: BuilderTree -> Array String
collectAttrs = case _ of
  BNode node children ->
    map attrImportName node.attributes <>
    Array.concat (map collectAttrs children)
  BDataJoin join ->
    map attrImportName join.template.attributes

attrImportName :: AttributeBinding -> String
attrImportName binding = case binding.attrName of
  "cx" -> "cx"
  "cy" -> "cy"
  "r" -> "radius"
  "x" -> "x"
  "y" -> "y"
  "x1" -> "x1"
  "y1" -> "y1"
  "x2" -> "x2"
  "y2" -> "y2"
  "width" -> "width"
  "height" -> "height"
  "fill" -> "fill"
  "stroke" -> "stroke"
  "stroke-width" -> "strokeWidth"
  "opacity" -> "opacity"
  "text" -> "textContent"
  "font-size" -> "fontSize"
  name -> name

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

-- | Generate attributes array
attrsArray :: Array AttributeBinding -> String
attrsArray attrs =
  "[ " <> String.joinWith ", " (map attrCode attrs) <> " ]"

-- | Generate template attributes (with datum accessors)
templateAttrsArray :: Array AttributeBinding -> String
templateAttrsArray attrs =
  "[ " <> String.joinWith ", " (map templateAttrCode attrs) <> " ]"

-- | Generate a single attribute
attrCode :: AttributeBinding -> String
attrCode binding =
  attrFn binding.attrName <> " " <> choiceToValue binding.choice

-- | Generate template attribute (for data joins, using datum accessors)
templateAttrCode :: AttributeBinding -> String
templateAttrCode binding =
  attrFn binding.attrName <> " " <> templateChoiceToValue binding.choice

-- | Map attribute names to PureScript attribute functions
attrFn :: String -> String
attrFn = case _ of
  "cx" -> "cx"
  "cy" -> "cy"
  "r" -> "radius"
  "x" -> "x"
  "y" -> "y"
  "x1" -> "x1"
  "y1" -> "y1"
  "x2" -> "x2"
  "y2" -> "y2"
  "width" -> "width"
  "height" -> "height"
  "fill" -> "fill"
  "stroke" -> "stroke"
  "stroke-width" -> "strokeWidth"
  "opacity" -> "opacity"
  "text" -> "textContent"
  "font-size" -> "fontSize"
  name -> name

-- | Convert an AttributeChoice to a value expression (for static nodes)
choiceToValue :: AttributeChoice -> String
choiceToValue = case _ of
  ConstantNumber n -> show n
  ConstantString s -> "\"" <> s <> "\""
  FromField field -> "d." <> field  -- Will need data in scope
  IndexBased -> "d.index"
  Computed name -> name

-- | Convert an AttributeChoice for template (datum accessor)
templateChoiceToValue :: AttributeChoice -> String
templateChoiceToValue = case _ of
  ConstantNumber n -> show n
  ConstantString s -> "\"" <> s <> "\""
  FromField field -> "d." <> field
  IndexBased -> "d.index"
  Computed name -> name

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

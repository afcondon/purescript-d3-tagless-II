-- | BuilderTree → PureScript DSL Code Generator
-- |
-- | Unlike TreeToCode (which works on opaque Tree datum functions),
-- | this works directly on BuilderTree where we have full knowledge
-- | of AttributeChoice - so we can generate proper DSL syntax.
-- |
-- | Target output format:
-- | ```purescript
-- | T.named SVG "svg"
-- |   [ evalAttr "width" (lit 300.0)
-- |   , evalAttr "height" (lit 300.0)
-- |   ]
-- |   `T.withChild`
-- |     (T.joinData "cells" "rect" data_ $ \d ->
-- |       T.elem Rect
-- |         [ fnAttr "x" (\d -> d.x)
-- |         , fnAttr "fill" (\d -> d.color)
-- |         ])
-- | ```
module TreeBuilder.BuilderToCode
  ( builderToCode
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import PSD3.Interpreter.SemiQuine.Types (BuilderTree(..), AttributeChoice(..), AttributeBinding)

-- | Convert a BuilderTree to PureScript DSL code
builderToCode :: BuilderTree -> String
builderToCode tree = String.joinWith "\n" $ treeLines tree 0

-- | Generate code lines for a tree node
treeLines :: BuilderTree -> Int -> Array String
treeLines tree indentLevel = case tree of
  BNode node children ->
    let
      -- Element constructor
      elemCode = case node.name of
        Just name -> "T.named " <> showElemType node.elementType <> " \"" <> name <> "\""
        Nothing -> "T.elem " <> showElemType node.elementType

      -- Children
      childrenCode = case Array.length children of
        0 -> []
        1 -> case Array.head children of
          Just child ->
            [ indent indentLevel <> "  `T.withChild`" ] <>
            [ indent (indentLevel + 2) <> "(" ] <>
            treeLines child (indentLevel + 2) <>
            [ indent (indentLevel + 2) <> ")" ]
          Nothing -> []
        _ ->
          [ indent indentLevel <> "  `T.withChildren`"
          , indent (indentLevel + 2) <> "["
          ] <>
          childrenWithCommas children (indentLevel + 2) <>
          [ indent (indentLevel + 2) <> "]" ]
    in
      [ indent indentLevel <> elemCode ] <>
      attrsLines node.attributes indentLevel <>
      childrenCode

  BDataJoin join ->
    let
      -- Template element
      templateElemCode = "T.elem " <> showElemType join.template.elementType
    in
      [ indent indentLevel <> "(T.joinData \"" <> join.name <> "\" \"" <> join.elementType <> "\" data_ $ \\d ->"
      , indent (indentLevel + 1) <> templateElemCode
      ] <>
      attrsLines join.template.attributes (indentLevel + 1) <>
      [ indent indentLevel <> ")" ]

  where
  -- Render children with commas between them
  childrenWithCommas :: Array BuilderTree -> Int -> Array String
  childrenWithCommas children ind =
    Array.concat $ Array.mapWithIndex (\i child ->
      let
        lines = treeLines child 0  -- Start at 0 indent, we'll add our own
        prefix = if i == 0 then " " else ", "
      in case Array.uncons lines of
        Just { head, tail } ->
          [ indent ind <> prefix <> head ] <>
          map (\l -> indent ind <> "  " <> l) tail
        Nothing -> []
    ) children

-- | Convert attributes to code lines
attrsLines :: Array AttributeBinding -> Int -> Array String
attrsLines attrs indentLevel =
  if Array.null attrs
    then [ indent (indentLevel + 1) <> "[]" ]
    else
      [ indent (indentLevel + 1) <> "[ " <> firstAttr ] <>
      restAttrs <>
      [ indent (indentLevel + 1) <> "]" ]
  where
  attrStrings = map attrToCode attrs
  firstAttr = case Array.head attrStrings of
    Just a -> a
    Nothing -> ""
  restAttrs = case Array.tail attrStrings of
    Just rest -> map (\a -> indent (indentLevel + 1) <> ", " <> a) rest
    Nothing -> []

-- | Convert attributes array to single-line code (for simple cases)
attrsToCode :: Array AttributeBinding -> String
attrsToCode attrs =
  if Array.null attrs
    then "[]"
    else "[ " <> String.joinWith ", " (map attrToCode attrs) <> " ]"

-- | Convert a single attribute binding to DSL code (Friendly DSL syntax)
-- | This generates code that matches the Prism.js semantic highlighting patterns
attrToCode :: AttributeBinding -> String
attrToCode binding = case binding.choice of
  ConstantNumber n ->
    -- Use Friendly sugar: width $ num 300.0
    attrFn binding.attrName <> " $ num " <> show n

  ConstantString s ->
    -- Use Friendly sugar: fill $ text "blue"
    attrFn binding.attrName <> " $ text \"" <> s <> "\""

  FromField fieldName ->
    -- Use Friendly sugar: cx $ field @"x"
    attrFn binding.attrName <> " $ field @\"" <> fieldName <> "\""

  IndexBased ->
    -- Use Friendly sugar: x $ index `timesN` 50.0
    attrFn binding.attrName <> " $ index"

  Computed expr ->
    -- Computed expressions - try to preserve readable form
    attrFn binding.attrName <> " $ " <> exprToFriendly expr

-- | Convert attribute name to Friendly sugar function
-- | Maps SVG attribute names to their Friendly DSL equivalents
attrFn :: String -> String
attrFn = case _ of
  -- Position
  "x" -> "x"
  "y" -> "y"
  "cx" -> "cx"
  "cy" -> "cy"
  "x1" -> "x1"
  "y1" -> "y1"
  "x2" -> "x2"
  "y2" -> "y2"
  "dx" -> "dx"
  "dy" -> "dy"
  -- Size
  "width" -> "width"
  "height" -> "height"
  "r" -> "r"
  -- Style
  "fill" -> "fill"
  "stroke" -> "stroke"
  "stroke-width" -> "strokeWidth"
  "opacity" -> "opacity"
  -- Text
  "text-anchor" -> "textAnchor"
  "dominant-baseline" -> "dominantBaseline"
  "font-size" -> "fontSize"
  "font-family" -> "fontFamily"
  "textContent" -> "textContent"
  "text" -> "textContent"  -- alias
  -- Path
  "d" -> "path"
  "transform" -> "transform"
  -- Fallback: use attr function for unknown attributes
  other -> "attr \"" <> other <> "\""

-- | Convert computed expression to Friendly DSL
-- | Tries to parse simple patterns into DSL combinators
exprToFriendly :: String -> String
exprToFriendly expr = expr  -- For now, pass through as-is

-- | Show element type as constructor
showElemType :: String -> String
showElemType = case _ of
  "svg" -> "SVG"
  "group" -> "Group"
  "g" -> "Group"
  "circle" -> "Circle"
  "rect" -> "Rect"
  "path" -> "Path"
  "line" -> "Line"
  "text" -> "Text"
  "div" -> "Div"
  "span" -> "Span"
  "table" -> "Table"
  "tbody" -> "Tbody"
  "thead" -> "Thead"
  "tr" -> "Tr"
  "td" -> "Td"
  "th" -> "Th"
  "defs" -> "Defs"
  other -> other  -- Pass through unknown types

-- | Generate indentation
indent :: Int -> String
indent n = String.joinWith "" (Array.replicate n "  ")

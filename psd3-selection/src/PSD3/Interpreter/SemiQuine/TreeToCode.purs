-- | SemiQuine Tree Interpreter
-- |
-- | Converts a Tree datum directly to PureScript code.
-- | Unlike the BuilderTree version, this works on actual Tree API trees.
-- |
-- | Limitations:
-- | - DataAttr/IndexedAttr functions are opaque - we can show they exist
-- |   and optionally evaluate them with sample data, but can't recover
-- |   the original expression (e.g., "d.x" vs "50.0")
-- | - Template functions in Join are evaluated with sample data to reveal structure
module PSD3.Interpreter.SemiQuine.TreeToCode
  ( treeToCode
  , treeToCodeWithSample
  ) where

import Prelude

import Data.Array (head, length, null, replicate) as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree(..))

-- | Convert a Tree to PureScript code (no sample data - dynamic attrs shown as placeholders)
treeToCode :: forall datum. Tree datum -> String
treeToCode tree = String.joinWith "\n" $ treeLines Nothing tree 0

-- | Convert a Tree to PureScript code with sample datum for evaluating dynamic attrs
treeToCodeWithSample :: forall datum. datum -> Tree datum -> String
treeToCodeWithSample sample tree = String.joinWith "\n" $ treeLines (Just sample) tree 0

-- | Generate code lines for a tree node
treeLines :: forall datum. Maybe datum -> Tree datum -> Int -> Array String
treeLines maybeSample tree indentLevel = case tree of

  Node node ->
    let
      elemCode = case node.name of
        Just name -> "T.named " <> showElemType node.elemType <> " \"" <> name <> "\""
        Nothing -> "T.elem " <> showElemType node.elemType

      attrsCode = attrsToCode maybeSample node.attrs

      childrenCode = case Array.length node.children of
        0 -> []
        1 -> case Array.head node.children of
          Just child ->
            [ indent indentLevel <> "  `T.withChild`" ] <>
            treeLines maybeSample child (indentLevel + 2)
          Nothing -> []
        _ ->
          [ indent indentLevel <> "  `T.withChildren`"
          , indent indentLevel <> "    [ " <> String.joinWith ("\n" <> indent indentLevel <> "    , ")
              (map (\c -> String.joinWith " " (treeLines maybeSample c 0)) node.children)
          , indent indentLevel <> "    ]"
          ]
    in
      [ indent indentLevel <> elemCode
      , indent indentLevel <> "  " <> attrsCode
      ] <> childrenCode

  Join joinSpec ->
    let
      -- Try to evaluate template with first datum
      templateCode = case Array.head joinSpec.joinData of
        Just firstDatum ->
          let templateTree = joinSpec.template firstDatum
          in treeLines (Just firstDatum) templateTree (indentLevel + 1)
        Nothing ->
          [ indent (indentLevel + 1) <> "-- (no data to evaluate template)" ]
    in
      [ indent indentLevel <> "T.joinData \"" <> joinSpec.name <> "\" \"" <> joinSpec.key <> "\" data_ $ \\d ->"
      ] <> templateCode

  NestedJoin nestedSpec ->
    let
      -- Try to evaluate with first datum decomposed
      templateCode = case Array.head nestedSpec.joinData of
        Just firstDatum ->
          let innerData = nestedSpec.decompose firstDatum
          in case Array.head innerData of
            Just firstInner ->
              let templateTree = nestedSpec.template firstInner
              in treeLines (Just firstInner) templateTree (indentLevel + 1)
            Nothing ->
              [ indent (indentLevel + 1) <> "-- (no inner data to evaluate template)" ]
        Nothing ->
          [ indent (indentLevel + 1) <> "-- (no data to evaluate template)" ]
    in
      [ indent indentLevel <> "T.nestedJoin \"" <> nestedSpec.name <> "\" \"" <> nestedSpec.key <> "\" data_ decompose $ \\inner ->"
      ] <> templateCode

  SceneJoin sceneSpec ->
    let
      behaviors =
        (if hasEnter sceneSpec then "enter " else "") <>
        (if hasUpdate sceneSpec then "update " else "") <>
        (if hasExit sceneSpec then "exit" else "")

      templateCode = case Array.head sceneSpec.joinData of
        Just firstDatum ->
          let templateTree = sceneSpec.template firstDatum
          in treeLines (Just firstDatum) templateTree (indentLevel + 1)
        Nothing ->
          [ indent (indentLevel + 1) <> "-- (no data to evaluate template)" ]
    in
      [ indent indentLevel <> "T.sceneJoin \"" <> sceneSpec.name <> "\" \"" <> sceneSpec.key <> "\" data_"
      , indent indentLevel <> "  -- behaviors: " <> behaviors
      , indent indentLevel <> "  $ \\d ->"
      ] <> templateCode

  SceneNestedJoin sceneNestedSpec ->
    [ indent indentLevel <> "T.sceneNestedJoin \"" <> sceneNestedSpec.name <> "\" \"" <> sceneNestedSpec.key <> "\" data_ decompose"
    , indent indentLevel <> "  -- (scene nested join - template omitted)"
    ]

  where
    hasEnter spec = case spec.enterBehavior of
      Just _ -> true
      Nothing -> false
    hasUpdate spec = case spec.updateBehavior of
      Just _ -> true
      Nothing -> false
    hasExit spec = case spec.exitBehavior of
      Just _ -> true
      Nothing -> false

-- | Convert attributes array to code
attrsToCode :: forall datum. Maybe datum -> Array (Attribute datum) -> String
attrsToCode maybeSample attrs =
  if Array.null attrs
    then "[]"
    else "[ " <> String.joinWith ", " (map (attrToCode maybeSample) attrs) <> " ]"

-- | Convert a single attribute to code
attrToCode :: forall datum. Maybe datum -> Attribute datum -> String
attrToCode maybeSample attr = case attr of
  StaticAttr (AttributeName name) value ->
    attrFnName name <> " " <> showAttrValue value

  DataAttr (AttributeName name) src fn ->
    case maybeSample of
      Just sample ->
        let evaluated = fn sample
        in attrFnName name <> " " <> attrSourceToCode src <> " {- → " <> showAttrValue evaluated <> " -}"
      Nothing ->
        attrFnName name <> " " <> attrSourceToCode src

  IndexedAttr (AttributeName name) src fn ->
    case maybeSample of
      Just sample ->
        let evaluated = fn sample 0
        in attrFnName name <> " " <> attrSourceToCode src <> " {- → " <> showAttrValue evaluated <> " -}"
      Nothing ->
        attrFnName name <> " " <> attrSourceToCode src

-- | Convert AttrSource to PureScript code representation
attrSourceToCode :: AttrSource -> String
attrSourceToCode = case _ of
  UnknownSource -> "d.<?>"
  StaticSource _ -> "<?>" -- Static values should use StaticAttr, not this
  FieldSource f -> "(field @\"" <> f <> "\")"
  ExprSource e -> "(" <> e <> ")" -- The expression string
  IndexSource -> "index"

-- | Map attribute names to PureScript function names
attrFnName :: String -> String
attrFnName = case _ of
  "r" -> "radius"
  "stroke-width" -> "strokeWidth"
  "fill-opacity" -> "fillOpacity"
  "stroke-opacity" -> "strokeOpacity"
  "stroke-dasharray" -> "strokeDasharray"
  "font-size" -> "fontSize"
  "font-family" -> "fontFamily"
  "font-weight" -> "fontWeight"
  "text-anchor" -> "textAnchor"
  "dominant-baseline" -> "dominantBaseline"
  "stop-color" -> "stopColor"
  "gradientUnits" -> "gradientUnits"
  name -> name

-- | Show element type as constructor
showElemType :: ElementType -> String
showElemType = case _ of
  SVG -> "SVG"
  Group -> "Group"
  Circle -> "Circle"
  Rect -> "Rect"
  Path -> "Path"
  Line -> "Line"
  Text -> "Text"
  Div -> "Div"
  Span -> "Span"
  Table -> "Table"
  Tbody -> "Tbody"
  Thead -> "Thead"
  Tr -> "Tr"
  Td -> "Td"
  Th -> "Th"
  Defs -> "Defs"
  LinearGradient -> "LinearGradient"
  Stop -> "Stop"

-- | Show attribute value
showAttrValue :: AttributeValue -> String
showAttrValue = case _ of
  StringValue s -> "\"" <> s <> "\""
  NumberValue n -> show n
  BooleanValue b -> show b

-- | Generate indentation
indent :: Int -> String
indent n = String.joinWith "" (Array.replicate n "  ")

-- | SemiQuine Tree Interpreter
-- |
-- | Converts a Tree datum directly to PureScript code.
-- | Unlike the BuilderTree version, this works on actual Tree API trees.
-- |
-- | The generated code uses Friendly DSL syntax designed for Prism.js highlighting:
-- | - `width $ num 300.0` instead of `evalAttr "width" (lit 300.0)`
-- | - `fill $ field @"color"` instead of `fnAttr "fill" (\d -> d.color)`
-- |
-- | AttrSource metadata enables recovering original expressions from opaque functions.
module PSD3.Interpreter.SemiQuine.TreeToCode
  ( treeToCode
  , treeToCodeWithSample
  ) where

import Prelude

import Data.Array (concat, drop, head, length, mapWithIndex, null, replicate, zipWith) as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree(..), GUPBehaviors, PhaseBehavior)

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
      -- Element constructor line
      elemCode = case node.name of
        Just name -> "T.named " <> showElemType node.elemType <> " \"" <> name <> "\""
        Nothing -> "T.elem " <> showElemType node.elemType

      -- Attributes as multi-line array
      attrsLines = attrsToLines maybeSample node.attrs (indentLevel + 1)

      -- Children
      childrenCode = case Array.length node.children of
        0 -> []
        1 -> case Array.head node.children of
          Just child ->
            [ indent indentLevel <> "`T.withChild`" ] <>
            treeLines maybeSample child (indentLevel + 1)
          Nothing -> []
        _ ->
          [ indent indentLevel <> "`T.withChildren`"
          , indent (indentLevel + 1) <> "["
          ] <>
          childrenWithCommas maybeSample node.children (indentLevel + 1) <>
          [ indent (indentLevel + 1) <> "]" ]
    in
      [ indent indentLevel <> elemCode ] <>
      attrsLines <>
      childrenCode

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

  UpdateJoin sceneSpec ->
    let
      maybeDatum = Array.head sceneSpec.joinData

      templateCode = case maybeDatum of
        Just firstDatum ->
          let templateTree = sceneSpec.template firstDatum
          in treeLines (Just firstDatum) templateTree (indentLevel + 1)
        Nothing ->
          [ indent (indentLevel + 1) <> "-- (no data to evaluate template)" ]

      behaviorsCode = gupBehaviorsToLines maybeDatum sceneSpec.behaviors (indentLevel + 1)
    in
      [ indent indentLevel <> "T.updateJoin \"" <> sceneSpec.name <> "\" \"" <> sceneSpec.key <> "\" data_"
      , indent indentLevel <> "  (\\d ->"
      ] <> templateCode <>
      [ indent indentLevel <> "  )"
      ] <> behaviorsCode

  UpdateNestedJoin sceneNestedSpec ->
    let
      maybeDatum = case Array.head sceneNestedSpec.joinData of
        Just outer -> Array.head (sceneNestedSpec.decompose outer)
        Nothing -> Nothing

      templateCode = case maybeDatum of
        Just innerDatum ->
          let templateTree = sceneNestedSpec.template innerDatum
          in treeLines (Just innerDatum) templateTree (indentLevel + 1)
        Nothing ->
          [ indent (indentLevel + 1) <> "-- (no data to evaluate template)" ]

      behaviorsCode = gupBehaviorsToLines maybeDatum sceneNestedSpec.behaviors (indentLevel + 1)
    in
      [ indent indentLevel <> "T.updateNestedJoin \"" <> sceneNestedSpec.name <> "\" \"" <> sceneNestedSpec.key <> "\" data_ decompose"
      , indent indentLevel <> "  (\\inner ->"
      ] <> templateCode <>
      [ indent indentLevel <> "  )"
      ] <> behaviorsCode

-- | Render children with proper comma formatting
childrenWithCommas :: forall datum. Maybe datum -> Array (Tree datum) -> Int -> Array String
childrenWithCommas maybeSample children ind =
  Array.concat $ Array.zipWith renderChild (Array.replicate (Array.length children) unit) (indexedChildren children)
  where
  indexedChildren cs = Array.mapWithIndex (\i c -> { idx: i, child: c }) cs

  renderChild _ { idx, child } =
    let
      childLines = treeLines maybeSample child 0
      prefix = if idx == 0 then "  " else ", "
    in case Array.head childLines of
      Just firstLine ->
        [ indent ind <> prefix <> firstLine ] <>
        map (\l -> indent (ind + 1) <> l) (dropFirst childLines)
      Nothing -> []

  dropFirst arr = case Array.length arr of
    0 -> []
    1 -> []
    _ -> Array.drop 1 arr

-- | Convert attributes array to multi-line code
attrsToLines :: forall datum. Maybe datum -> Array (Attribute datum) -> Int -> Array String
attrsToLines maybeSample attrs indentLevel =
  if Array.null attrs
    then [ indent indentLevel <> "[]" ]
    else
      let attrStrings = map (attrToCode maybeSample) attrs
      in case Array.head attrStrings of
        Just first ->
          [ indent indentLevel <> "[ " <> first ] <>
          map (\a -> indent indentLevel <> ", " <> a) (Array.drop 1 attrStrings) <>
          [ indent indentLevel <> "]" ]
        Nothing -> [ indent indentLevel <> "[]" ]

-- | Convert a single attribute to code (Friendly DSL syntax for Prism highlighting)
attrToCode :: forall datum. Maybe datum -> Attribute datum -> String
attrToCode maybeSample attr = case attr of
  StaticAttr (AttributeName name) value ->
    -- Friendly DSL: width $ num 300.0, fill $ text "blue"
    attrFnName name <> " $ " <> staticValueToCode value

  DataAttr (AttributeName name) src fn ->
    case maybeSample of
      Just sample ->
        let evaluated = fn sample
        -- Show source expression with evaluated result as comment
        in attrFnName name <> " $ " <> attrSourceToCode src <> " {- → " <> showAttrValue evaluated <> " -}"
      Nothing ->
        attrFnName name <> " $ " <> attrSourceToCode src

  IndexedAttr (AttributeName name) src fn ->
    case maybeSample of
      Just sample ->
        let evaluated = fn sample 0
        in attrFnName name <> " $ " <> attrSourceToCode src <> " {- → " <> showAttrValue evaluated <> " -}"
      Nothing ->
        attrFnName name <> " $ " <> attrSourceToCode src

-- | Convert static value to Friendly DSL code
staticValueToCode :: AttributeValue -> String
staticValueToCode = case _ of
  StringValue s -> "text \"" <> s <> "\""
  NumberValue n -> "num " <> show n
  BooleanValue b -> "bool " <> show b

-- | Convert AttrSource to Friendly DSL code representation
-- | Designed to match Prism.js highlighting patterns
attrSourceToCode :: AttrSource -> String
attrSourceToCode = case _ of
  UnknownSource -> "d.<?>"
  StaticSource s -> "num " <> s
  FieldSource f -> "field @\"" <> f <> "\""
  ExprSource e -> e
  IndexSource -> "index"

-- | Map attribute names to Friendly DSL function names
attrFnName :: String -> String
attrFnName = case _ of
  -- Friendly DSL uses 'r' directly, not 'radius'
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
  PatternFill -> "PatternFill"

-- | Show attribute value for display in comments
showAttrValue :: AttributeValue -> String
showAttrValue = case _ of
  StringValue s -> "\"" <> s <> "\""
  NumberValue n -> show n
  BooleanValue b -> show b

-- | Generate indentation (2 spaces per level)
indent :: Int -> String
indent n = String.joinWith "" (Array.replicate n "  ")

-- | Convert GUP behaviors to code lines
gupBehaviorsToLines :: forall datum. Maybe datum -> GUPBehaviors datum -> Int -> Array String
gupBehaviorsToLines maybeSample behaviors ind =
  [ indent ind <> "{ enter: " <> phaseToCode maybeSample behaviors.enter
  , indent ind <> ", update: " <> phaseToCode maybeSample behaviors.update
  , indent ind <> ", exit: " <> phaseToCode maybeSample behaviors.exit
  , indent ind <> "}"
  ]

-- | Convert a single phase behavior to code
phaseToCode :: forall datum. Maybe datum -> Maybe (PhaseBehavior datum) -> String
phaseToCode _ Nothing = "Nothing"
phaseToCode maybeSample (Just phase) =
  if Array.null phase.attrs
    then "Nothing"
    else "Just { attrs: " <> attrsInlineToCode maybeSample phase.attrs <> ", transition: Nothing }"

-- | Convert attributes to inline code (for phase behaviors)
attrsInlineToCode :: forall datum. Maybe datum -> Array (Attribute datum) -> String
attrsInlineToCode maybeSample attrs =
  "[ " <> String.joinWith ", " (map (attrToCode maybeSample) attrs) <> " ]"

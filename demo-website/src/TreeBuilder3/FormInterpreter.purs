-- | Form Interpreter: AST -> Interactive Halogen Form
-- |
-- | Converts an AST.Tree to an interactive form where names and values
-- | can be edited inline. Changes emit actions that allow reconstructing
-- | the AST with updated values.
-- |
-- | Editable parts:
-- | - Element names (named SVG "svg" -> the "svg")
-- | - Join names ("nodes" in joinData "nodes")
-- | - Join keys ("circle" in joinData _ "circle")
-- | - Static attribute values (numbers, strings)
-- |
-- | Read-only parts:
-- | - Structure keywords (T.elem, T.joinData, etc.)
-- | - Element types (SVG, Circle, etc.)
-- | - Lambda syntax
module TreeBuilder3.FormInterpreter
  ( astToForm
  , FormAction(..)
  , FormPath
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as Data.String
import Data.String (length) as String
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.AST as AST
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Internal.Selection.Types (ElementType(..))

-- | Path to a value in the AST (for identifying which field changed)
type FormPath = Array String

-- | Actions emitted by the form
data FormAction
  = NameChanged FormPath String      -- Named element/join name changed
  | KeyChanged FormPath String       -- Join key changed
  | StaticValueChanged FormPath String  -- Static attr value changed

-- | Convert an AST to an interactive form
-- | Returns HTML with input fields for editable parts
astToForm :: forall datum w. AST.Tree datum -> HH.HTML w FormAction
astToForm tree = HH.div
  [ HP.class_ (HH.ClassName "ast-form") ]
  [ HH.pre
      [ HP.class_ (HH.ClassName "ast-form-code") ]
      (treeToFormLines [] tree 0)
  ]

-- | Generate form lines for a tree node
treeToFormLines :: forall datum w. FormPath -> AST.Tree datum -> Int -> Array (HH.HTML w FormAction)
treeToFormLines path tree indentLevel = case tree of

  AST.Node node ->
    let
      ind = indent indentLevel
      elemCode = case node.name of
        Just name ->
          [ HH.text (ind <> "T.named " <> showElemType node.elemType <> " ")
          , nameInput (path <> ["name"]) name
          , HH.text "\n"
          ]
        Nothing ->
          [ HH.text (ind <> "T.elem " <> showElemType node.elemType <> "\n") ]

      attrsLines = attrsToFormLines (path <> ["attrs"]) node.attrs (indentLevel + 1)

      childrenLines = case Array.length node.children of
        0 -> []
        1 -> case Array.head node.children of
          Just child ->
            [ HH.text (ind <> "`T.withChild`\n") ] <>
            treeToFormLines (path <> ["children", "0"]) child (indentLevel + 1)
          Nothing -> []
        _ ->
          [ HH.text (ind <> "`T.withChildren`\n")
          , HH.text (indent (indentLevel + 1) <> "[\n")
          ] <>
          childrenToFormLines (path <> ["children"]) node.children (indentLevel + 1) <>
          [ HH.text (indent (indentLevel + 1) <> "]\n") ]
    in
      elemCode <> attrsLines <> childrenLines

  AST.Join joinSpec ->
    let
      ind = indent indentLevel
      templateLines = case Array.head joinSpec.joinData of
        Just firstDatum ->
          let templateTree = joinSpec.template firstDatum
          in treeToFormLines (path <> ["template"]) templateTree (indentLevel + 1)
        Nothing ->
          [ HH.text (indent (indentLevel + 1) <> "-- (no data)\n") ]
    in
      [ HH.text (ind <> "T.joinData ")
      , nameInput (path <> ["name"]) joinSpec.name
      , HH.text " "
      , keyInput (path <> ["key"]) joinSpec.key
      , HH.text " data_ $ \\d ->\n"
      ] <> templateLines

  AST.NestedJoin nestedSpec ->
    let
      ind = indent indentLevel
      templateLines = case Array.head nestedSpec.joinData of
        Just firstDatum ->
          case Array.head (nestedSpec.decompose firstDatum) of
            Just innerDatum ->
              let templateTree = nestedSpec.template innerDatum
              in treeToFormLines (path <> ["template"]) templateTree (indentLevel + 1)
            Nothing -> [ HH.text (indent (indentLevel + 1) <> "-- (no inner data)\n") ]
        Nothing ->
          [ HH.text (indent (indentLevel + 1) <> "-- (no data)\n") ]
    in
      [ HH.text (ind <> "T.nestedJoin ")
      , nameInput (path <> ["name"]) nestedSpec.name
      , HH.text " "
      , keyInput (path <> ["key"]) nestedSpec.key
      , HH.text " data_ decompose $ \\inner ->\n"
      ] <> templateLines

  AST.UpdateJoin sceneSpec ->
    let
      ind = indent indentLevel
      templateLines = case Array.head sceneSpec.joinData of
        Just firstDatum ->
          let templateTree = sceneSpec.template firstDatum
          in treeToFormLines (path <> ["template"]) templateTree (indentLevel + 1)
        Nothing ->
          [ HH.text (indent (indentLevel + 1) <> "-- (no data)\n") ]
    in
      [ HH.text (ind <> "T.updateJoin ")
      , nameInput (path <> ["name"]) sceneSpec.name
      , HH.text " "
      , keyInput (path <> ["key"]) sceneSpec.key
      , HH.text " data_\n"
      , HH.text (ind <> "  (\\d ->\n")
      ] <> templateLines <>
      [ HH.text (ind <> "  )\n")
      , HH.text (ind <> "  { enter: ..., update: ..., exit: ... }\n")
      ]

  AST.UpdateNestedJoin sceneNestedSpec ->
    let
      ind = indent indentLevel
      templateLines = case Array.head sceneNestedSpec.joinData of
        Just firstDatum ->
          case Array.head (sceneNestedSpec.decompose firstDatum) of
            Just innerDatum ->
              let templateTree = sceneNestedSpec.template innerDatum
              in treeToFormLines (path <> ["template"]) templateTree (indentLevel + 1)
            Nothing -> [ HH.text (indent (indentLevel + 1) <> "-- (no inner data)\n") ]
        Nothing ->
          [ HH.text (indent (indentLevel + 1) <> "-- (no data)\n") ]
    in
      [ HH.text (ind <> "T.updateNestedJoin ")
      , nameInput (path <> ["name"]) sceneNestedSpec.name
      , HH.text " "
      , keyInput (path <> ["key"]) sceneNestedSpec.key
      , HH.text " data_ decompose\n"
      , HH.text (ind <> "  (\\inner ->\n")
      ] <> templateLines <>
      [ HH.text (ind <> "  )\n")
      , HH.text (ind <> "  { enter: ..., update: ..., exit: ... }\n")
      ]

-- | Render a name input field
nameInput :: forall w. FormPath -> String -> HH.HTML w FormAction
nameInput path value = HH.input
  [ HP.type_ HP.InputText
  , HP.class_ (HH.ClassName "ast-form-name-input")
  , HP.value value
  , HE.onValueInput (NameChanged path)
  , HP.attr (HH.AttrName "size") (show (max 4 (String.length value + 1)))
  ]

-- | Render a key input field
keyInput :: forall w. FormPath -> String -> HH.HTML w FormAction
keyInput path value = HH.input
  [ HP.type_ HP.InputText
  , HP.class_ (HH.ClassName "ast-form-key-input")
  , HP.value value
  , HE.onValueInput (KeyChanged path)
  , HP.attr (HH.AttrName "size") (show (max 3 (String.length value + 1)))
  ]

-- | Render a static value input field
staticValueInput :: forall w. FormPath -> String -> HH.HTML w FormAction
staticValueInput path value = HH.input
  [ HP.type_ HP.InputText
  , HP.class_ (HH.ClassName "ast-form-value-input")
  , HP.value value
  , HE.onValueInput (StaticValueChanged path)
  , HP.attr (HH.AttrName "size") (show (max 3 (String.length value + 2)))
  ]

-- | Convert attributes to form lines
attrsToFormLines :: forall datum w. FormPath -> Array (Attribute datum) -> Int -> Array (HH.HTML w FormAction)
attrsToFormLines path attrs indentLevel =
  if Array.null attrs
    then [ HH.text (indent indentLevel <> "[]\n") ]
    else
      [ HH.text (indent indentLevel <> "[ ") ] <>
      Array.concat (Array.mapWithIndex (attrToFormLine path indentLevel) attrs) <>
      [ HH.text (indent indentLevel <> "]\n") ]

-- | Convert a single attribute to form elements
attrToFormLine :: forall datum w. FormPath -> Int -> Int -> Attribute datum -> Array (HH.HTML w FormAction)
attrToFormLine path indentLevel idx attr =
  let
    prefix = if idx == 0 then "" else indent indentLevel <> ", "
    attrPath = path <> [show idx]
  in case attr of
    StaticAttr (AttributeName name) value ->
      [ HH.text prefix
      , HH.text (attrFnName name <> " $ ")
      , staticValueInput (attrPath <> ["value"]) (showStaticValue value)
      , HH.text "\n"
      ]

    DataAttr (AttributeName name) source _ ->
      [ HH.text prefix
      , HH.text (attrFnName name <> " $ " <> showAttrSource source <> "\n")
      ]

    IndexedAttr (AttributeName name) _ _ ->
      [ HH.text prefix
      , HH.text (attrFnName name <> " $ index\n")
      ]

-- | Convert children to form lines with proper comma formatting
childrenToFormLines :: forall datum w. FormPath -> Array (AST.Tree datum) -> Int -> Array (HH.HTML w FormAction)
childrenToFormLines path children indentLevel =
  Array.concat $ Array.mapWithIndex renderChild children
  where
  renderChild idx child =
    let
      prefix = if idx == 0 then "  " else ", "
      childPath = path <> [show idx]
    in
      [ HH.text (indent indentLevel <> prefix) ] <>
      treeToFormLines childPath child 0

-- | Show element type
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

-- | Show static attribute value
showStaticValue :: AttributeValue -> String
showStaticValue = case _ of
  StringValue s -> "text \"" <> s <> "\""
  NumberValue n -> "num " <> show n
  BooleanValue b -> "bool " <> show b

-- | Show attr source
showAttrSource :: AttrSource -> String
showAttrSource = case _ of
  UnknownSource -> "d.<?>"
  StaticSource s -> "num " <> s
  FieldSource f -> "field @\"" <> f <> "\""
  ExprSource e -> e
  IndexSource -> "index"

-- | Map attribute names to Friendly DSL function names
attrFnName :: String -> String
attrFnName = case _ of
  "stroke-width" -> "strokeWidth"
  "fill-opacity" -> "fillOpacity"
  "stroke-opacity" -> "strokeOpacity"
  "font-size" -> "fontSize"
  "font-family" -> "fontFamily"
  "text-anchor" -> "textAnchor"
  name -> name

-- | Generate indentation
indent :: Int -> String
indent n = joinWith "" (Array.replicate n "  ")
  where
  joinWith = Data.String.joinWith

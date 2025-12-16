-- | TreeBuilder → Tree Converter
-- |
-- | Converts a BuilderTree (editable UI representation) to an actual Tree SampleDatum
-- | that can be interpreted by multiple interpreters:
-- | - MetaAST: Shows tree structure
-- | - SemiQuine/TreeToCode: Generates PureScript code
-- | - D3: Renders to SVG
-- |
-- | This is the KEY converter that makes the educational demo work:
-- | One Tree definition → Multiple interpretations
module TreeBuilder.ToTree
  ( builderToTree
  , builderToTreeWithData
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import PSD3.AST as T
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3.Internal.Selection.Types (ElementType(..))
import TreeBuilder.Types (SampleDatum)
import PSD3.Interpreter.SemiQuine.Types (BuilderTree(..), BuilderNode, AttributeChoice(..), AttributeBinding)

-- | Convert a BuilderTree to a Tree SampleDatum
-- | Uses an empty array for joinData by default (for structure inspection)
builderToTree :: BuilderTree -> T.Tree SampleDatum
builderToTree tree = builderToTreeWithData tree []

-- | Convert a BuilderTree to a Tree SampleDatum with provided sample data
builderToTreeWithData :: BuilderTree -> Array SampleDatum -> T.Tree SampleDatum
builderToTreeWithData tree sampleData = case tree of
  BNode node children ->
    T.Node
      { name: node.name
      , elemType: parseElementType node.elementType
      , attrs: map bindingToAttribute node.attributes
      , behaviors: []
      , children: map (\c -> builderToTreeWithData c sampleData) children
      }

  BDataJoin join ->
    T.Join
      { name: join.name
      , key: join.elementType
      , joinData: sampleData
      , template: \_ ->
          T.Node
            { name: Nothing
            , elemType: parseElementType join.template.elementType
            , attrs: map bindingToAttribute join.template.attributes
            , behaviors: []
            , children: []
            }
      }

-- | Convert element type string to ElementType
parseElementType :: String -> ElementType
parseElementType = case _ of
  "svg" -> SVG
  "group" -> Group
  "g" -> Group
  "circle" -> Circle
  "rect" -> Rect
  "line" -> Line
  "path" -> Path
  "text" -> Text
  "defs" -> Defs
  "div" -> Div
  "span" -> Span
  "table" -> Table
  "tbody" -> Tbody
  "thead" -> Thead
  "tr" -> Tr
  "td" -> Td
  "th" -> Th
  _ -> Group -- Default fallback

-- | Convert an AttributeBinding to an Attribute SampleDatum
bindingToAttribute :: AttributeBinding -> Attribute SampleDatum
bindingToAttribute binding =
  choiceToAttribute (AttributeName binding.attrName) binding.choice

-- | Convert an AttributeChoice to an Attribute SampleDatum
choiceToAttribute :: AttributeName -> AttributeChoice -> Attribute SampleDatum
choiceToAttribute name choice = case choice of
  ConstantNumber n ->
    StaticAttr name (NumberValue n)

  ConstantString s ->
    StaticAttr name (StringValue s)

  FromField field ->
    DataAttr name (getFieldValue field)

  IndexBased ->
    IndexedAttr name (\_ idx -> NumberValue (toNumber idx))

  Computed expr ->
    -- For computed expressions, evaluate them as best we can
    -- This is simplified - real expressions would need proper parsing
    DataAttr name (evalComputedExpr expr)

-- | Get a field value from a SampleDatum as an AttributeValue
-- | This is the key function that makes field references work
getFieldValue :: String -> SampleDatum -> AttributeValue
getFieldValue field datum = case field of
  -- Numeric fields
  "x" -> NumberValue datum.x
  "y" -> NumberValue datum.y
  "cx" -> NumberValue datum.cx
  "cy" -> NumberValue datum.cy
  "rx" -> NumberValue datum.rx
  "ry" -> NumberValue datum.ry
  "sx" -> NumberValue datum.sx
  "sy" -> NumberValue datum.sy
  "radius" -> NumberValue datum.radius
  "width" -> NumberValue datum.width
  "height" -> NumberValue datum.height
  "value" -> NumberValue datum.value
  "index" -> NumberValue (toNumber datum.index)
  -- String fields
  "color" -> StringValue datum.color
  "label" -> StringValue datum.label
  "name" -> StringValue datum.name
  -- Default fallback
  _ -> StringValue ""

-- | Get numeric field value (for computations)
getNumericField :: String -> SampleDatum -> Number
getNumericField field datum = case field of
  "x" -> datum.x
  "y" -> datum.y
  "cx" -> datum.cx
  "cy" -> datum.cy
  "rx" -> datum.rx
  "ry" -> datum.ry
  "sx" -> datum.sx
  "sy" -> datum.sy
  "radius" -> datum.radius
  "width" -> datum.width
  "height" -> datum.height
  "value" -> datum.value
  "index" -> toNumber datum.index
  _ -> 0.0

-- | Evaluate a computed expression
-- | Supports simple patterns like:
-- | - "d.x + 10"
-- | - "d.x * d.width"
-- | - "d.y - 5"
evalComputedExpr :: String -> SampleDatum -> AttributeValue
evalComputedExpr expr datum =
  -- Try each pattern in order
  case parseAddExpr expr datum of
    Just result -> NumberValue result
    Nothing -> case parseSubExpr expr datum of
      Just result -> NumberValue result
      Nothing -> case parseMulExpr expr datum of
        Just result -> NumberValue result
        Nothing -> case parseDivExpr expr datum of
          Just result -> NumberValue result
          Nothing -> case parseFieldRef expr datum of
            Just result -> result
            Nothing -> StringValue expr -- Fallback: treat as literal string

-- | Parse "d.field + N" or "d.field1 + d.field2"
parseAddExpr :: String -> SampleDatum -> Maybe Number
parseAddExpr s datum = do
  let parts = String.split (String.Pattern " + ") s
  case Array.length parts of
    2 -> do
      part1 <- Array.head parts
      part2 <- Array.last parts
      field1 <- String.stripPrefix (String.Pattern "d.") part1
      -- Try as field + number first
      case Number.fromString part2 of
        Just n -> Just (getNumericField field1 datum + n)
        Nothing -> do
          -- Try as field + field
          field2 <- String.stripPrefix (String.Pattern "d.") part2
          Just (getNumericField field1 datum + getNumericField field2 datum)
    _ -> Nothing

-- | Parse "d.field - N"
parseSubExpr :: String -> SampleDatum -> Maybe Number
parseSubExpr s datum = do
  let parts = String.split (String.Pattern " - ") s
  case Array.length parts of
    2 -> do
      part1 <- Array.head parts
      part2 <- Array.last parts
      field1 <- String.stripPrefix (String.Pattern "d.") part1
      n <- Number.fromString part2
      Just (getNumericField field1 datum - n)
    _ -> Nothing

-- | Parse "d.field * N" or "d.field1 * d.field2"
parseMulExpr :: String -> SampleDatum -> Maybe Number
parseMulExpr s datum = do
  let parts = String.split (String.Pattern " * ") s
  case Array.length parts of
    2 -> do
      part1 <- Array.head parts
      part2 <- Array.last parts
      field1 <- String.stripPrefix (String.Pattern "d.") part1
      case Number.fromString part2 of
        Just n -> Just (getNumericField field1 datum * n)
        Nothing -> do
          field2 <- String.stripPrefix (String.Pattern "d.") part2
          Just (getNumericField field1 datum * getNumericField field2 datum)
    _ -> Nothing

-- | Parse "d.field / N"
parseDivExpr :: String -> SampleDatum -> Maybe Number
parseDivExpr s datum = do
  let parts = String.split (String.Pattern " / ") s
  case Array.length parts of
    2 -> do
      part1 <- Array.head parts
      part2 <- Array.last parts
      field1 <- String.stripPrefix (String.Pattern "d.") part1
      n <- Number.fromString part2
      Just (getNumericField field1 datum / n)
    _ -> Nothing

-- | Parse "d.field" simple field reference
parseFieldRef :: String -> SampleDatum -> Maybe AttributeValue
parseFieldRef s datum = do
  field <- String.stripPrefix (String.Pattern "d.") s
  Just (getFieldValue field datum)

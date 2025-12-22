module PSD3.Interpreter.English
  ( runEnglish
  , describeTree
  , describeAttribute
  , showElemType
  , showAttrName
  , showAttrValue
  , indent
  ) where

-- | English description interpreter for TreeAPI
-- | Translates tree structure into English description of HOW it's built

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (length, null)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (snd)
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree(..))

-- | Run the English interpreter on a tree
runEnglish :: forall datum. Tree datum -> String
runEnglish tree = snd $ runWriter $ describeTree tree 0

-- | Describe a tree with indentation level
describeTree :: forall datum. Tree datum -> Int -> Writer String Unit
describeTree tree level = case tree of
  Node { name, elemType, attrs, children } -> do
    case name of
      Just n -> tell $ indent level <> "Create a " <> showElemType elemType <> " named \"" <> n <> "\""
      Nothing -> tell $ indent level <> "Create a " <> showElemType elemType
    when (not $ null attrs) do
      tell " with attributes:\n"
      for_ attrs \attr -> tell $ indent (level + 1) <> describeAttribute attr <> "\n"
    when (not $ null children) do
      tell $ indent level <> "Then add " <> show (length children) <> " child element(s):\n"
      for_ children \child -> describeTree child (level + 1)

  Join { name, key, joinData, template } -> do
    tell $ indent level <> "For each of " <> show (length joinData) <> " data items, "
    tell $ "create a " <> key <> " (join named \"" <> name <> "\")"
    tell " using data-driven attributes\n"
    -- We can't actually evaluate the template function, so just note it exists
    tell $ indent (level + 1) <> "(Template function defined for creating elements from data)\n"

  NestedJoin { name, key, joinData, template } -> do
    tell $ indent level <> "For each of " <> show (length joinData) <> " nested data items, "
    tell $ "create a " <> key <> " (nested join named \"" <> name <> "\")"
    tell " using data-driven attributes\n"
    tell $ indent (level + 1) <> "(Nested template function defined)\n"

  UpdateJoin { name, key, joinData, behaviors } -> do
    tell $ indent level <> "For each of " <> show (length joinData) <> " data items, "
    tell $ "create/update/remove " <> key <> " (update join named \"" <> name <> "\")"
    tell " with General Update Pattern:\n"
    when (isJust behaviors.enter)
      $ tell
      $ indent (level + 1) <> "- Enter: elements appear with initial attributes and transition\n"
    when (isJust behaviors.update)
      $ tell
      $ indent (level + 1) <> "- Update: elements transition to new state\n"
    when (isJust behaviors.exit)
      $ tell
      $ indent (level + 1) <> "- Exit: elements transition out then removed\n"
    tell $ indent (level + 1) <> "(Template function defined for creating elements from data)\n"

  UpdateNestedJoin { name, key, joinData, behaviors } -> do
    tell $ indent level <> "For each of " <> show (length joinData) <> " nested data items, "
    tell $ "decompose and create/update/remove " <> key <> " (update nested join named \"" <> name <> "\")"
    tell " with General Update Pattern:\n"
    when (isJust behaviors.enter)
      $ tell
      $ indent (level + 1) <> "- Enter: elements appear with initial attributes and transition\n"
    when (isJust behaviors.update)
      $ tell
      $ indent (level + 1) <> "- Update: elements transition to new state\n"
    when (isJust behaviors.exit)
      $ tell
      $ indent (level + 1) <> "- Exit: elements transition out then removed\n"
    tell $ indent (level + 1) <> "(Decompose and template functions defined)\n"

-- | Describe an attribute in English
describeAttribute :: forall datum. Attribute datum -> String
describeAttribute (StaticAttr name value) =
  "Set " <> showAttrName name <> " to " <> showAttrValue value

describeAttribute (DataAttr name src _) =
  "Set " <> showAttrName name <> " " <> describeSource src

describeAttribute (IndexedAttr name src _) =
  "Set " <> showAttrName name <> " " <> describeSource src

-- | Describe attribute source in English
describeSource :: AttrSource -> String
describeSource = case _ of
  UnknownSource -> "based on data (dynamic)"
  StaticSource s -> "to " <> s
  FieldSource f -> "from data field '" <> f <> "'"
  ExprSource e -> "computed as " <> e
  IndexSource -> "from element index"

-- | Show element type as English
showElemType :: ElementType -> String
showElemType SVG = "SVG container"
showElemType Group = "group"
showElemType Circle = "circle"
showElemType Rect = "rectangle"
showElemType Path = "path"
showElemType Line = "line"
showElemType Text = "text"
showElemType Div = "div"
showElemType Span = "span"
showElemType Table = "table"
showElemType Tbody = "table body"
showElemType Thead = "table head"
showElemType Tr = "table row"
showElemType Td = "table cell"
showElemType Th = "table header"
showElemType Defs = "definitions"
showElemType LinearGradient = "linear gradient"
showElemType Stop = "gradient stop"
showElemType PatternFill = "fill pattern"

-- | Show attribute name
showAttrName :: AttributeName -> String
showAttrName (AttributeName name) = name

-- | Show attribute value
showAttrValue :: AttributeValue -> String
showAttrValue (StringValue s) = "\"" <> s <> "\""
showAttrValue (NumberValue n) = show n
showAttrValue (BooleanValue b) = show b

-- | Create indentation
indent :: Int -> String
indent 0 = ""
indent n = "  " <> indent (n - 1)

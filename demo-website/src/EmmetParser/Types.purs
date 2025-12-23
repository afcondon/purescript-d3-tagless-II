module EmmetParser.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as SCU

-- | Emmet-specific AST (intermediate representation)
-- | This is parsed from Emmet syntax and converted to TreeBuilder3's TreeNode format
data EmmetNode
  = ElemNode ElementType (Array Attribute) (Maybe String)
  | JoinNode JoinType String (Array Attribute) (Maybe String)

derive instance eqEmmetNode :: Eq EmmetNode

-- | Element types matching TreeBuilder3's element types
data ElementType
  = EGroup
  | ECircle
  | ERect
  | EPath
  | ELine
  | EText

derive instance eqElementType :: Eq ElementType

instance showElementType :: Show ElementType where
  show = case _ of
    EGroup -> "Group"
    ECircle -> "Circle"
    ERect -> "Rect"
    EPath -> "Path"
    ELine -> "Line"
    EText -> "Text"

-- | Join types for data binding
data JoinType
  = SimpleJoin        -- j(Type) - simple data join
  | NestedJoin        -- n(Type) - type-decomposing join
  | UpdateJoin        -- u(Type) - GUP join (enter/update/exit)
  | UpdateNestedJoin  -- x(Type) - GUP + type decomposition

derive instance eqJoinType :: Eq JoinType

instance showJoinType :: Show JoinType where
  show = case _ of
    SimpleJoin -> "Join"
    NestedJoin -> "NestedJoin"
    UpdateJoin -> "UpdateJoin"
    UpdateNestedJoin -> "UpdateNestedJoin"

-- | Attributes for simple builder (restricted to static, field, and index)
data Attribute
  = StaticAttr String String      -- name, literal value
  | FieldAttr String String       -- name, field accessor
  | IndexAttr String              -- name (uses index parameter)

derive instance eqAttribute :: Eq Attribute

instance showAttribute :: Show Attribute where
  show = case _ of
    StaticAttr name val -> name <> "=" <> val
    FieldAttr name field -> name <> ":" <> field
    IndexAttr name -> name <> "@index"

-- | Emmet expression tree structure
data EmmetExpr
  = Single EmmetNode (Array EmmetExpr)         -- node with children
  | Sibling EmmetExpr EmmetExpr                -- expr + expr
  | Repeat EmmetExpr Int                       -- expr * N

derive instance eqEmmetExpr :: Eq EmmetExpr

instance showEmmetExpr :: Show EmmetExpr where
  show = case _ of
    Single node children ->
      "Single " <> showNode node <> " " <> show children
    Sibling e1 e2 ->
      "Sibling (" <> show e1 <> ") (" <> show e2 <> ")"
    Repeat expr n ->
      "Repeat (" <> show expr <> ") " <> show n
    where
      showNode = case _ of
        ElemNode typ attrs name ->
          show typ <> showAttrs attrs <> showName name
        JoinNode jtyp tname attrs name ->
          show jtyp <> "(" <> tname <> ")" <> showAttrs attrs <> showName name

      showAttrs [] = ""
      showAttrs attrs = "[" <> String.joinWith "," (map show attrs) <> "]"

      showName Nothing = ""
      showName (Just n) = "#" <> n

-- | Parser errors
data ParseError
  = UnexpectedChar Char Int                     -- char, position
  | UnexpectedEnd String                        -- expected
  | UnknownElement String Int                   -- element, position
  | UnknownJoinType String Int                  -- join type, position
  | InvalidAttribute String Int                 -- message, position
  | UnbalancedParens Int                        -- position
  | EmptyExpression
  | InvalidTypeName String Int                  -- type name, position

derive instance eqParseError :: Eq ParseError

instance showParseError :: Show ParseError where
  show = formatError

-- | Format parse errors for user display
formatError :: ParseError -> String
formatError = case _ of
  UnexpectedChar c pos ->
    "Unexpected character '" <> SCU.singleton c <> "' at position " <> show pos

  UnexpectedEnd expected ->
    "Unexpected end of input, expected " <> expected

  UnknownElement elem pos ->
    "Unknown element '" <> elem <> "' at position " <> show pos <>
    "\nValid elements: g (group), c (circle), r (rect), p (path), l (line), t (text)"

  UnknownJoinType jtyp pos ->
    "Unknown join type '" <> jtyp <> "' at position " <> show pos <>
    "\nValid join types: j (join), n (nested), u (update), x (update nested)"

  InvalidAttribute msg pos ->
    "Invalid attribute at position " <> show pos <> ": " <> msg

  UnbalancedParens pos ->
    "Unbalanced parentheses at position " <> show pos

  EmptyExpression ->
    "Empty expression"

  InvalidTypeName tname pos ->
    "Invalid type name '" <> tname <> "' at position " <> show pos <>
    "\nValid types: Point, Node, Country, Letter, Board, Row, Cell"

-- | Validation errors (semantic errors after parsing)
data ValidationError
  = InvalidType { typeName :: String, validTypes :: Array String }
  | InvalidField { fieldName :: String, typeName :: String, validFields :: Array String }
  | InvalidAttrName { attrName :: String, validAttrs :: Array String }
  | JoinWithoutTemplate { position :: Int }
  | NestedJoinWithScalarType { typeName :: String }

derive instance eqValidationError :: Eq ValidationError

instance showValidationError :: Show ValidationError where
  show = case _ of
    InvalidType { typeName, validTypes } ->
      "Invalid type '" <> typeName <> "'. Valid types: " <> String.joinWith ", " validTypes

    InvalidField { fieldName, typeName, validFields } ->
      "Field '" <> fieldName <> "' does not exist on type " <> typeName <>
      ". Valid fields: " <> String.joinWith ", " validFields

    InvalidAttrName { attrName, validAttrs } ->
      "Invalid attribute name '" <> attrName <> "'. Valid attributes: " <> String.joinWith ", " validAttrs

    JoinWithoutTemplate { position } ->
      "Join at position " <> show position <> " must have a child element (template)"

    NestedJoinWithScalarType { typeName } ->
      "Cannot use nested join with scalar type '" <> typeName <> "'. " <>
      "Nested joins require array types (Board, Row, etc.)"

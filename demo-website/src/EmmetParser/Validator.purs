module EmmetParser.Validator
  ( validate
  , validateTypeName
  , validateFieldName
  , validateAttributeName
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import EmmetParser.Types (EmmetExpr(..), EmmetNode(..), JoinType(..), Attribute(..), ValidationError(..))
import TreeBuilder3.TypePropagation (builtinTypes)
import TreeBuilder3.Types (datumTypeFields)

-- | Valid type names
validTypeNames :: Array String
validTypeNames = ["Point", "Node", "Link", "Country", "Letter", "Cell", "Row", "Board"]

-- | Array type names (for nested joins)
arrayTypeNames :: Array String
arrayTypeNames = ["Board", "Row"]

-- | Valid attribute names (common SVG attributes)
validAttributeNames :: Array String
validAttributeNames =
  [ "cx", "cy", "x", "y", "r", "radius"
  , "fill", "f"
  , "stroke", "s"
  , "width", "w"
  , "height", "h"
  , "transform"
  , "stroke-width", "k"
  , "opacity", "o"
  , "d"  -- path data
  , "x1", "y1", "x2", "y2"  -- line coordinates
  ]

-- | Validate an Emmet expression
validate :: EmmetExpr -> Either ValidationError EmmetExpr
validate expr = do
  validateExpr expr
  pure expr

validateExpr :: EmmetExpr -> Either ValidationError Unit
validateExpr = case _ of
  Single node children -> do
    validateNode node
    -- Validate children
    traverse_ validateExpr children

  Sibling e1 e2 -> do
    validateExpr e1
    validateExpr e2

  Repeat expr _ ->
    validateExpr expr

validateNode :: EmmetNode -> Either ValidationError Unit
validateNode = case _ of
  ElemNode _ attrs _ ->
    traverse_ validateAttribute attrs

  JoinNode joinType typeName attrs _ -> do
    -- Validate type name
    validateTypeName typeName
    -- Validate that nested joins use array types
    when (joinType == NestedJoin || joinType == UpdateNestedJoin) do
      unless (Array.elem typeName arrayTypeNames) do
        Left $ NestedJoinWithScalarType { typeName }
    -- Validate attributes
    traverse_ validateAttribute attrs

validateAttribute :: Attribute -> Either ValidationError Unit
validateAttribute = case _ of
  StaticAttr name _ ->
    validateAttributeName name

  FieldAttr name _fieldName -> do
    validateAttributeName name
    -- Note: We can't validate field names without knowing the type context
    -- This would require threading type information through validation
    -- For now, we just ensure the attribute name is valid
    pure unit

  IndexAttr name ->
    validateAttributeName name

  OpaqueAttr name _opaqueType -> do
    validateAttributeName name
    -- Opaque attributes are valid - they'll be substituted during round-trip
    pure unit

-- | Validate that a type name is recognized
validateTypeName :: String -> Either ValidationError Unit
validateTypeName typeName =
  if Array.elem typeName validTypeNames
    then Right unit
    else Left $ InvalidType
      { typeName
      , validTypes: validTypeNames
      }

-- | Validate that a field exists on a type
validateFieldName :: String -> String -> Either ValidationError Unit
validateFieldName typeName fieldName = do
  -- Look up type
  case Map.lookup typeName builtinTypes of
    Nothing ->
      Left $ InvalidType { typeName, validTypes: validTypeNames }

    Just datumType -> do
      let fields = datumTypeFields datumType
      let fieldNames = map _.name fields
      if Array.elem fieldName fieldNames
        then Right unit
        else Left $ InvalidField
          { fieldName
          , typeName
          , validFields: fieldNames
          }

-- | Validate that an attribute name is recognized
validateAttributeName :: String -> Either ValidationError Unit
validateAttributeName attrName =
  if Array.elem attrName validAttributeNames
    then Right unit
    else Left $ InvalidAttrName
      { attrName
      , validAttrs: validAttributeNames
      }

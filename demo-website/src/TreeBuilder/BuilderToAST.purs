-- | BuilderTree → Enhanced AST
-- |
-- | Produces an AST representation that includes actual attribute details,
-- | not just counts. Works from BuilderTree where we have AttributeChoice info.
module TreeBuilder.BuilderToAST
  ( BuilderAST(..)
  , AttrAST(..)
  , builderToAST
  ) where

import Prelude

import Data.Maybe (Maybe)
import PSD3.Interpreter.SemiQuine.Types (BuilderTree(..), AttributeChoice(..), AttributeBinding)

-- | Attribute AST - shows what kind of binding each attribute has
data AttrAST
  = ConstNumAttr String Number      -- attr = 300.0
  | ConstStrAttr String String      -- attr = "red"
  | FieldAttr String String         -- attr = d.x
  | IndexAttr String                -- attr = index
  | ComputedAttr String String      -- attr = d.x + 10

derive instance Eq AttrAST

-- | Enhanced AST with full attribute info
data BuilderAST
  = NodeAST
      { name :: Maybe String
      , elemType :: String
      , attrs :: Array AttrAST
      , children :: Array BuilderAST
      }
  | JoinAST
      { name :: String
      , elemType :: String
      , templateAttrs :: Array AttrAST
      }

derive instance Eq BuilderAST

-- | Convert BuilderTree to enhanced AST
builderToAST :: BuilderTree -> BuilderAST
builderToAST tree = case tree of
  BNode node children ->
    NodeAST
      { name: node.name
      , elemType: node.elementType
      , attrs: map bindingToAST node.attributes
      , children: map builderToAST children
      }

  BDataJoin join ->
    JoinAST
      { name: join.name
      , elemType: join.template.elementType
      , templateAttrs: map bindingToAST join.template.attributes
      }

-- | Convert an AttributeBinding to AttrAST
bindingToAST :: AttributeBinding -> AttrAST
bindingToAST binding = case binding.choice of
  ConstantNumber n -> ConstNumAttr binding.attrName n
  ConstantString s -> ConstStrAttr binding.attrName s
  FromField field -> FieldAttr binding.attrName field
  IndexBased -> IndexAttr binding.attrName
  Computed expr -> ComputedAttr binding.attrName expr


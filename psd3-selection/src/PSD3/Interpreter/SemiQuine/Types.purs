-- | SemiQuine Types
-- |
-- | Core types for the tree builder / code generator.
-- | These represent the editable form of a visualization tree
-- | before it's interpreted back into PureScript code.
module PSD3.Interpreter.SemiQuine.Types
  ( BuilderTree(..)
  , BuilderNode
  , NodeId
  , AttributeChoice(..)
  , AttributeBinding
  , emptyNode
  , defaultAttributesFor
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- | Unique identifier for nodes in the builder tree
type NodeId = Int

-- | How an attribute value is determined.
-- | This is the key abstraction that avoids runtime PureScript compilation.
data AttributeChoice
  = FromField String          -- ^ Read from datum field (e.g., "x", "color")
  | ConstantNumber Number     -- ^ Fixed number value
  | ConstantString String     -- ^ Fixed string value
  | IndexBased                -- ^ Use the datum's index
  | Computed String           -- ^ Named computation (e.g., "scaled_x", "category_color")

derive instance eqAttributeChoice :: Eq AttributeChoice

instance showAttributeChoice :: Show AttributeChoice where
  show (FromField f) = "_.\"" <> f <> "\""
  show (ConstantNumber n) = show n
  show (ConstantString s) = "\"" <> s <> "\""
  show IndexBased = "_.index"
  show (Computed c) = c

-- | An attribute binding: which attribute + how to get its value
type AttributeBinding =
  { attrName :: String           -- e.g., "cx", "fill", "x"
  , choice :: AttributeChoice    -- how to get the value
  }

-- | A node in the builder tree (editable form)
type BuilderNode =
  { id :: NodeId
  , elementType :: String        -- "circle", "rect", "text", "group", "svg", "line"
  , name :: Maybe String         -- Optional name for selection retrieval
  , attributes :: Array AttributeBinding
  , expanded :: Boolean          -- UI state: is this node expanded in tree view?
  }

-- | The builder tree structure
data BuilderTree
  = BNode BuilderNode (Array BuilderTree)  -- Node with children
  | BDataJoin                               -- Data join point
      { id :: NodeId
      , name :: String
      , elementType :: String
      , template :: BuilderNode            -- Template node for each datum
      , expanded :: Boolean
      }

-- Manual Eq instance (can't derive due to record type alias)
instance eqBuilderTree :: Eq BuilderTree where
  eq (BNode n1 c1) (BNode n2 c2) = n1.id == n2.id && c1 == c2
  eq (BDataJoin j1) (BDataJoin j2) = j1.id == j2.id
  eq _ _ = false

-- | Create an empty node of a given type
emptyNode :: NodeId -> String -> BuilderNode
emptyNode nid elemType =
  { id: nid
  , elementType: elemType
  , name: Nothing
  , attributes: defaultAttributesFor elemType
  , expanded: true
  }

-- | Default attributes based on element type
defaultAttributesFor :: String -> Array AttributeBinding
defaultAttributesFor = case _ of
  "circle" ->
    [ { attrName: "cx", choice: FromField "x" }
    , { attrName: "cy", choice: FromField "y" }
    , { attrName: "r", choice: FromField "radius" }
    , { attrName: "fill", choice: FromField "color" }
    ]
  "rect" ->
    [ { attrName: "x", choice: FromField "x" }
    , { attrName: "y", choice: FromField "y" }
    , { attrName: "width", choice: FromField "width" }
    , { attrName: "height", choice: FromField "height" }
    , { attrName: "fill", choice: FromField "color" }
    ]
  "text" ->
    [ { attrName: "x", choice: FromField "x" }
    , { attrName: "y", choice: FromField "y" }
    , { attrName: "text", choice: FromField "label" }
    , { attrName: "fill", choice: ConstantString "#333" }
    ]
  "line" ->
    [ { attrName: "x1", choice: ConstantNumber 0.0 }
    , { attrName: "y1", choice: ConstantNumber 0.0 }
    , { attrName: "x2", choice: FromField "x" }
    , { attrName: "y2", choice: FromField "y" }
    , { attrName: "stroke", choice: FromField "color" }
    ]
  "group" -> []
  "svg" ->
    [ { attrName: "width", choice: ConstantNumber 400.0 }
    , { attrName: "height", choice: ConstantNumber 300.0 }
    ]
  _ -> []

-- | TreeBuilder3 Types
-- |
-- | Shared types used by TreeBuilder3 App and Converter modules.
module TreeBuilder3.Types
  ( TreeNode
  , DslNodeType(..)
  , AttrKind(..)
  , BehaviorKind(..)
  , nodeLabel
  , nodeKeyHints
  -- Datum types
  , DatumType(..)
  , PrimType(..)
  , FieldDef
  , datumTypeLabel
  , datumTypeFields
  ) where

import Prelude

import Data.Maybe (Maybe)
import PSD3.Internal.Selection.Types (ElementType(..))

-- | The different AST node types from the PSD3 grammar
-- | Includes "pending" states for nodes awaiting further input
data DslNodeType
  = NodeElem ElementType -- Element node (SVG, Group, Circle, etc.)
  | NodeJoin -- Simple data join
  | NodeNestedJoin -- Type-decomposing join
  | NodeUpdateJoin -- GUP join
  | NodeUpdateNestedJoin -- GUP + type decomposition
  | NodeAttr AttrKind -- Attribute (fully specified)
  | NodeBehavior BehaviorKind -- Behavior (fully specified)
  -- GUP selection phases (auto-created under UpdateJoin templates)
  | NodeEnter -- Enter selection (new elements)
  | NodeUpdate -- Update selection (existing elements)
  | NodeExit -- Exit selection (removed elements)
  -- Pending states - awaiting further input
  | PendingElement -- Awaiting element type (s,g,c,r,p,l,t,d)
  | PendingAttr -- Awaiting attr name (c,x,y,r,f,s,w,h,t)
  | PendingAttrValue String -- Has attr name, awaiting value type (l,f,e,i)
  | PendingBehavior -- Awaiting behavior type (z,d,c,h)

derive instance Eq DslNodeType
derive instance Ord DslNodeType

-- | Attribute kinds
data AttrKind
  = AttrStatic String String -- name, value
  | AttrField String String -- name, field
  | AttrExpr String String -- name, expr
  | AttrIndex String -- name (uses index)

derive instance Eq AttrKind
derive instance Ord AttrKind

-- | Behavior kinds
data BehaviorKind = BehaviorZoom | BehaviorDrag | BehaviorClick | BehaviorHover

derive instance Eq BehaviorKind
derive instance Ord BehaviorKind

-- =============================================================================
-- Datum Types (for phantom type visualization)
-- =============================================================================

-- | Primitive types that can appear in records
data PrimType
  = TNumber
  | TString
  | TInt
  | TBoolean

derive instance Eq PrimType
derive instance Ord PrimType

-- | A field in a record type
type FieldDef =
  { name :: String
  , typ :: PrimType
  }

-- | The datum type at a point in the tree
-- | This represents what type `d` has in expressions like `F.field "x"`
data DatumType
  = TypeUnit                              -- No datum (root, before any join)
  | TypeRecord String (Array FieldDef)    -- Named record: "Point" { x :: Number, y :: Number }
  | TypeArray DatumType                   -- Array of a type (for nested joins)
  | TypeUnknown                           -- Type not yet specified (joins without type)

derive instance Eq DatumType

-- Can't derive Ord for recursive types easily, so manual instance
instance Ord DatumType where
  compare TypeUnit TypeUnit = EQ
  compare TypeUnit _ = LT
  compare _ TypeUnit = GT
  compare TypeUnknown TypeUnknown = EQ
  compare TypeUnknown _ = LT
  compare _ TypeUnknown = GT
  compare (TypeRecord n1 _) (TypeRecord n2 _) = compare n1 n2
  compare (TypeRecord _ _) _ = LT
  compare _ (TypeRecord _ _) = GT
  compare (TypeArray t1) (TypeArray t2) = compare t1 t2

-- | Get a display label for a datum type
datumTypeLabel :: DatumType -> String
datumTypeLabel TypeUnit = "Unit"
datumTypeLabel (TypeRecord name _) = name
datumTypeLabel (TypeArray inner) = "Array " <> datumTypeLabel inner
datumTypeLabel TypeUnknown = "?"

-- | Get the fields available for a datum type (empty for non-records)
datumTypeFields :: DatumType -> Array FieldDef
datumTypeFields (TypeRecord _ fields) = fields
datumTypeFields _ = []

-- | Our tree node data
type TreeNode =
  { id :: Int
  , nodeType :: DslNodeType
  , name :: Maybe String     -- Element/join name (e.g., "svg", "nodes")
  , key :: Maybe String      -- Join key (e.g., "circle", "line")
  , datumType :: DatumType   -- The datum type at this node (phantom type)
  , x :: Number
  , y :: Number
  , depth :: Int
  }

-- | Get label for a DSL node type
nodeLabel :: DslNodeType -> String
nodeLabel (NodeElem SVG) = "SVG"
nodeLabel (NodeElem Group) = "Group"
nodeLabel (NodeElem Circle) = "Circle"
nodeLabel (NodeElem Rect) = "Rect"
nodeLabel (NodeElem Path) = "Path"
nodeLabel (NodeElem Line) = "Line"
nodeLabel (NodeElem Text) = "Text"
nodeLabel (NodeElem Defs) = "Defs"
nodeLabel (NodeElem _) = "Element" -- Other element types
nodeLabel NodeJoin = "Join"
nodeLabel NodeNestedJoin = "NestedJoin"
nodeLabel NodeUpdateJoin = "UpdateJoin"
nodeLabel NodeUpdateNestedJoin = "UpdateNestedJoin"
nodeLabel (NodeAttr (AttrStatic name _)) = name
nodeLabel (NodeAttr (AttrField name _)) = name
nodeLabel (NodeAttr (AttrExpr name _)) = name
nodeLabel (NodeAttr (AttrIndex name)) = name
nodeLabel (NodeBehavior BehaviorZoom) = "Zoom"
nodeLabel (NodeBehavior BehaviorDrag) = "Drag"
nodeLabel (NodeBehavior BehaviorClick) = "Click"
nodeLabel (NodeBehavior BehaviorHover) = "Hover"
-- GUP selection phases
nodeLabel NodeEnter = "Enter"
nodeLabel NodeUpdate = "Update"
nodeLabel NodeExit = "Exit"
-- Pending types - show "?" to indicate awaiting input
nodeLabel PendingElement = "Element?"
nodeLabel PendingAttr = "Attr?"
nodeLabel (PendingAttrValue name) = "attr:" <> name <> "?"
nodeLabel PendingBehavior = "Behavior?"

-- | Get valid key hints for a node type (shown next to selected node)
-- | Grammar-constrained: only shows keys that are valid for this node type
nodeKeyHints :: DslNodeType -> String
nodeKeyHints PendingElement = "[g,c,r,p,l,t,d]" -- No SVG - that's root only
nodeKeyHints PendingAttr = "[c,x,y,r,f,s,w,h,t]"
nodeKeyHints (PendingAttrValue _) = "[l,f,e,i]"
nodeKeyHints PendingBehavior = "[z,d,c,h]"
-- Resolved nodes - element-specific hints
nodeKeyHints (NodeElem SVG) = "[e,j,n,s,x,a,b]" -- SVG (root): can have all children
nodeKeyHints (NodeElem Group) = "[e,j,n,s,x,a,b]" -- Group: can have all children
nodeKeyHints (NodeElem Defs) = "[a]" -- Defs: only attrs (simplified)
nodeKeyHints (NodeElem Circle) = "[a,b]" -- Circle: leaf - only attrs/behaviors
nodeKeyHints (NodeElem Rect) = "[a,b]" -- Rect: leaf
nodeKeyHints (NodeElem Path) = "[a,b]" -- Path: leaf
nodeKeyHints (NodeElem Line) = "[a,b]" -- Line: leaf
nodeKeyHints (NodeElem Text) = "[a,b]" -- Text: leaf
nodeKeyHints (NodeElem _) = "[a,b]" -- Other elements: leaf by default
-- Join nodes
nodeKeyHints NodeJoin = "[e]" -- Joins can only have element template
nodeKeyHints NodeNestedJoin = "[e]"
nodeKeyHints NodeUpdateJoin = "[e]"
nodeKeyHints NodeUpdateNestedJoin = "[e]"
-- Attr/Behavior nodes
nodeKeyHints (NodeAttr _) = "[a]" -- Attrs can add sibling attrs
nodeKeyHints (NodeBehavior _) = "[b]" -- Behaviors can add sibling behaviors
-- GUP selection phases - can have attrs
nodeKeyHints NodeEnter = "[a]"
nodeKeyHints NodeUpdate = "[a]"
nodeKeyHints NodeExit = "[a]"

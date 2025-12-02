-- | BubblePack rendering for module internals
-- |
-- | Converts module declarations into hierarchical bubble packs:
-- | - Outer circle: Module
-- | - Middle circles: Categories (typeClass, data, typeSynonym, externData, alias, value)
-- | - Inner circles: Individual declarations
module Engine.BubblePack
  ( renderModulePack
  , ModulePackData
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Loader (Declaration, DeclarationsMap)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object
import PSD3.Layout.Hierarchy.Pack (HierarchyData(..), PackNode(..), defaultPackConfig, hierarchy, pack)
import Types (SimNode)

-- | FFI for rendering bubble pack with proper data binding
foreign import renderBoundBubblePack_
  :: String           -- Container selector
  -> SimNode          -- Node (bound to group as __data__)
  -> Array PackCircle -- Pack circles
  -> Number           -- Center offset
  -> (PackCircle -> Effect String) -- Category color function
  -> Effect Unit

-- | FFI for rendering re-export (umbrella) modules
foreign import renderReexportModule_
  :: String  -- Container selector
  -> SimNode -- Node (bound to group as __data__)
  -> Boolean -- isReexport (true = re-export, false = empty)
  -> Effect Unit

-- | Render a re-export or empty module
renderReexportModule :: SimNode -> Boolean -> Effect Unit
renderReexportModule node isReexport =
  renderReexportModule_ "#explorer-nodes" node isReexport

-- | Flat pack circle data for FFI
type PackCircle =
  { x :: Number
  , y :: Number
  , r :: Number
  , depth :: Int
  , data_ :: String
  }

-- | Data for a module pack (position + declarations)
type ModulePackData =
  { node :: SimNode
  , declarations :: Array Declaration
  }

-- | Category colors
categoryColor :: String -> String
categoryColor "typeClass" = "#9467bd"   -- purple
categoryColor "data" = "#2ca02c"         -- green
categoryColor "typeSynonym" = "#17becf"  -- cyan
categoryColor "externData" = "#bcbd22"   -- yellow-green
categoryColor "alias" = "#7f7f7f"        -- gray
categoryColor "value" = "#1f77b4"        -- blue
categoryColor _ = "#cccccc"

-- | Build HierarchyData from module declarations
-- | Structure: Module -> [Category -> [Declaration]]
buildModuleHierarchy :: String -> Array Declaration -> HierarchyData String
buildModuleHierarchy moduleName decls =
  let
    -- Group declarations by kind
    grouped = groupByKind decls

    -- Build category children
    categoryChildren = Array.mapMaybe buildCategory grouped
  in
    HierarchyData
      { data_: moduleName
      , value: Nothing  -- Value is sum of children
      , children: if Array.null categoryChildren
                  then Nothing
                  else Just categoryChildren
      }

-- | Group declarations by kind
groupByKind :: Array Declaration -> Array { kind :: String, decls :: Array Declaration }
groupByKind decls =
  let
    -- Get unique kinds
    kinds = Array.nub $ map _.kind decls

    -- Group by each kind
    groups = map (\k -> { kind: k, decls: Array.filter (\d -> d.kind == k) decls }) kinds
  in
    -- Filter out empty groups and sort by count (descending)
    Array.sortWith (\g -> negate $ Array.length g.decls) $
      Array.filter (\g -> not $ Array.null g.decls) groups

-- | Build a category node with declaration children
buildCategory :: { kind :: String, decls :: Array Declaration } -> Maybe (HierarchyData String)
buildCategory { kind, decls } =
  if Array.null decls then Nothing
  else Just $ HierarchyData
    { data_: kind
    , value: Nothing
    , children: Just $ map buildDeclaration decls
    }

-- | Build a leaf declaration node
buildDeclaration :: Declaration -> HierarchyData String
buildDeclaration decl = HierarchyData
  { data_: decl.title
  , value: Just 1.0  -- Each declaration has value 1
  , children: Nothing
  }

-- | Get all nodes from pack tree (recursive)
getAllPackNodes :: forall a. PackNode a -> Array (PackNode a)
getAllPackNodes node@(PackNode n) =
  if Array.null n.children then [ node ]
  else [ node ] <> (n.children >>= getAllPackNodes)

-- | Render a module as a bubble pack
-- | Returns the pack's radius for layout purposes
renderModulePack :: DeclarationsMap -> SimNode -> Effect Number
renderModulePack declarationsMap node = do
  let moduleName = node.name
  let decls = fromMaybe [] $ Object.lookup moduleName declarationsMap

  if Array.null decls then do
    -- No declarations - likely a re-export module
    -- Detect: has dependencies but no declarations = umbrella/re-export module
    let isReexport = not (Array.null node.targets)
    if isReexport
      then log $ "[BubblePack] Re-export module: " <> moduleName <> " (depends on " <> show (Array.length node.targets) <> " modules)"
      else log $ "[BubblePack] Empty module: " <> moduleName

    -- Render a distinctive visual for re-export modules
    renderReexportModule node isReexport
    pure node.r
  else do
    log $ "[BubblePack] Rendering " <> moduleName <> " with " <> show (Array.length decls) <> " declarations"

    -- Build hierarchy
    let hierData = buildModuleHierarchy moduleName decls

    -- Convert to PackNode
    let packRoot = hierarchy hierData

    -- Apply pack layout - size based on original node radius
    let packSize = node.r * 3.0  -- Make pack bigger than original circle
    let config = defaultPackConfig
          { size = { width: packSize, height: packSize }
          , padding = 1.0
          }
    let packed = pack config packRoot

    -- Get all nodes for rendering (as flat PackCircle records)
    let allNodes = getAllPackNodes packed
    let packCircles = map packNodeToCircle allNodes

    -- Center offset for positioning circles relative to group origin
    let centerOffset = packSize / 2.0

    -- Render using FFI with proper data binding
    renderBoundBubblePack_ "#explorer-nodes" node packCircles centerOffset getPackFillEffect

    -- Return the pack's radius
    let PackNode rootData = packed
    pure rootData.r

-- | Convert PackNode to flat PackCircle for FFI
packNodeToCircle :: PackNode String -> PackCircle
packNodeToCircle (PackNode n) =
  { x: n.x
  , y: n.y
  , r: n.r
  , depth: n.depth
  , data_: n.data_
  }

-- | Get fill color as Effect (for FFI callback)
getPackFillEffect :: PackCircle -> Effect String
getPackFillEffect pc = pure $ getPackFill pc

-- | Get fill color based on depth
getPackFill :: PackCircle -> String
getPackFill pc = case pc.depth of
  0 -> "#333"  -- Module (outer) - dark
  1 -> categoryColor pc.data_  -- Category
  _ -> categoryColor "value"  -- Declaration inherits parent category color (simplified)


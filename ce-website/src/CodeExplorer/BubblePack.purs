-- | BubblePack rendering for module internals
-- |
-- | Converts module declarations into hierarchical bubble packs:
-- | - Outer circle: Module
-- | - Middle circles: Categories (typeClass, data, typeSynonym, externData, alias, value)
-- | - Inner circles: Individual declarations
module CodeExplorer.BubblePack
  ( renderModulePack
  , renderModulePackWithCallbacks
  , ModulePackData
  , highlightCallGraph
  , clearCallGraphHighlight
  , drawFunctionEdges
  , clearFunctionEdges
  , drawModuleEdges
  , highlightModuleCallGraph
  , clearBubblePacks
  , ModuleEdge
  , DeclarationClickCallback
  , DeclarationHoverCallback
  , ModuleHoverCallback
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Loader (Declaration, DeclarationsMap)
import Data.ColorPalette (PaletteType(..), getCategoryColor)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object
import DataViz.Layout.Hierarchy.Pack (HierarchyData(..), PackNode(..), defaultPackConfig, hierarchy, pack)
import Types (SimNode)

-- | Callback type for declaration clicks
-- | Parameters: moduleName -> declarationName -> kind -> Effect Unit
type DeclarationClickCallback = String -> String -> String -> Effect Unit

-- | Callback type for declaration hover
-- | Parameters: moduleName -> declarationName -> kind -> Effect Unit
type DeclarationHoverCallback = String -> String -> String -> Effect Unit

-- | Callback type for module (outer circle) hover
-- | Parameters: moduleName -> Effect Unit
type ModuleHoverCallback = String -> Effect Unit

-- | FFI for rendering bubble pack with proper data binding
foreign import renderBoundBubblePack_
  :: String -- Container selector
  -> SimNode -- Node (bound to group as __data__)
  -> Array PackCircle -- Pack circles
  -> Number -- Center offset
  -> (PackCircle -> Effect String) -- Category color function
  -> DeclarationClickCallback -- On declaration click (module, declaration, kind) -> Effect Unit
  -> DeclarationHoverCallback -- On declaration hover (module, declaration, kind) -> Effect Unit
  -> Effect Unit -- On declaration leave -> Effect Unit
  -> ModuleHoverCallback -- On module hover (moduleName) -> Effect Unit
  -> Effect Unit

-- | FFI for highlighting call graph at function level
-- | Parameters are now full qualified names like "Module.funcName"
foreign import highlightCallGraph_
  :: String -- Source function (e.g. "Module.func")
  -> Array String -- Caller functions (e.g. ["Caller.fn1", "Caller.fn2"])
  -> Array String -- Callee functions (e.g. ["Called.fn1"])
  -> Effect Unit

-- | FFI for drawing temporary edges between function circles
foreign import drawFunctionEdges_
  :: String -- Source function
  -> Array String -- Caller functions
  -> Array String -- Callee functions
  -> Effect Unit

-- | FFI for clearing function edges
foreign import clearFunctionEdges_ :: Effect Unit

-- | Edge type for module-level highlighting
type ModuleEdge =
  { source :: String -- "Module.func"
  , target :: String -- "Module.func"
  , isOutgoing :: Boolean -- true if source module calls target
  }

-- | FFI for drawing all edges from a module at once
foreign import drawModuleEdges_ :: Array ModuleEdge -> Effect Unit

-- | FFI for highlighting all functions in a module
foreign import highlightModuleCallGraph_
  :: String -- Module name
  -> Array String -- Functions in the hovered module
  -> Array String -- Caller functions (from other modules)
  -> Array String -- Callee functions (in other modules)
  -> Effect Unit

-- | FFI for clearing call graph highlights
foreign import clearCallGraphHighlight_ :: Effect Unit

-- | FFI for rendering re-export (umbrella) modules
foreign import renderReexportModule_
  :: String -- Container selector
  -> SimNode -- Node (bound to group as __data__)
  -> Boolean -- isReexport (true = re-export, false = empty)
  -> Effect Unit

-- | Render a re-export or empty module
renderReexportModule :: SimNode -> Boolean -> Effect Unit
renderReexportModule node isReexport =
  renderReexportModule_ "#explorer-nodes" node isReexport

-- | FFI for clearing all bubble packs
foreign import clearBubblePacks_ :: Effect Unit

-- | Clear all bubble packs from the DOM
clearBubblePacks :: Effect Unit
clearBubblePacks = clearBubblePacks_

-- | Flat pack circle data for FFI
type PackCircle =
  { x :: Number
  , y :: Number
  , r :: Number
  , depth :: Int
  , data_ :: String
  , category :: String -- Parent category (for coloring declarations)
  }

-- | Data for a module pack (position + declarations)
type ModulePackData =
  { node :: SimNode
  , declarations :: Array Declaration
  }

-- | Get color for a category using the current palette
-- | For now, we use DeclarationTypes palette
-- | TODO: Make this dynamic based on active palette selection
categoryColor :: String -> String
categoryColor = getCategoryColor DeclarationTypes

-- | Get intense color for a category (currently same as base color)
-- | The Tableau10 palette already has good saturation
categoryColorIntense :: String -> String
categoryColorIntense = getCategoryColor DeclarationTypes

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
      , value: Nothing -- Value is sum of children
      , children:
          if Array.null categoryChildren then Nothing
          else Just categoryChildren
      }

-- | Group declarations by kind (single-pass O(n) algorithm using Object)
groupByKind :: Array Declaration -> Array { kind :: String, decls :: Array Declaration }
groupByKind decls =
  let
    -- Build kind -> declarations map in single pass
    addToGroup :: Object.Object (Array Declaration) -> Declaration -> Object.Object (Array Declaration)
    addToGroup m d = Object.alter (Just <<< maybe [ d ] (_ `Array.snoc` d)) d.kind m

    kindMap = Array.foldl addToGroup Object.empty decls

    -- Convert to array of records
    groups = Object.foldMap (\k ds -> [ { kind: k, decls: ds } ]) kindMap
  in
    -- Sort by count (descending)
    Array.sortWith (\g -> negate $ Array.length g.decls) groups

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
  , value: Just 1.0 -- Each declaration has value 1
  , children: Nothing
  }

-- | Get all nodes from pack tree with category tracking (recursive)
-- | At depth 1, the node's data_ IS the category name
-- | At depth 2+, we pass down the parent category
getAllPackNodesWithCategory :: PackNode String -> String -> Array { node :: PackNode String, category :: String }
getAllPackNodesWithCategory node@(PackNode n) parentCategory =
  let
    -- At depth 1, this node IS a category, so use its data_ as category for children
    thisCategory = if n.depth == 1 then n.data_ else parentCategory
    childResults = n.children >>= (\child -> getAllPackNodesWithCategory child thisCategory)
  in
    [ { node, category: thisCategory } ] <> childResults

-- | Default no-op callbacks for backwards compatibility
noOpClickCallback :: DeclarationClickCallback
noOpClickCallback _ _ _ = pure unit

noOpHoverCallback :: DeclarationHoverCallback
noOpHoverCallback _ _ _ = pure unit

noOpLeaveCallback :: Effect Unit
noOpLeaveCallback = pure unit

noOpModuleHoverCallback :: ModuleHoverCallback
noOpModuleHoverCallback _ = pure unit

-- | Highlight individual function circles based on call graph relationships
-- | Takes full qualified names like "Module.funcName"
highlightCallGraph :: String -> Array String -> Array String -> Effect Unit
highlightCallGraph = highlightCallGraph_

-- | Draw temporary edges between function circles
drawFunctionEdges :: String -> Array String -> Array String -> Effect Unit
drawFunctionEdges = drawFunctionEdges_

-- | Clear function edges
clearFunctionEdges :: Effect Unit
clearFunctionEdges = clearFunctionEdges_

-- | Draw all edges from a module at once
drawModuleEdges :: Array ModuleEdge -> Effect Unit
drawModuleEdges = drawModuleEdges_

-- | Highlight all functions in a module and their connections
highlightModuleCallGraph :: String -> Array String -> Array String -> Array String -> Effect Unit
highlightModuleCallGraph = highlightModuleCallGraph_

-- | Clear call graph highlighting
clearCallGraphHighlight :: Effect Unit
clearCallGraphHighlight = clearCallGraphHighlight_

-- | Render a module as a bubble pack (without callbacks)
-- | Returns the pack's radius for layout purposes
renderModulePack :: DeclarationsMap -> SimNode -> Effect Number
renderModulePack declarationsMap node =
  renderModulePackWithCallbacks declarationsMap noOpClickCallback noOpHoverCallback noOpLeaveCallback noOpModuleHoverCallback node

-- | Render a module as a bubble pack with click and hover handlers
-- | Returns the pack's radius for layout purposes
renderModulePackWithCallbacks
  :: DeclarationsMap
  -> DeclarationClickCallback
  -> DeclarationHoverCallback
  -> Effect Unit -- onLeave callback
  -> ModuleHoverCallback -- onModuleHover callback
  -> SimNode
  -> Effect Number
renderModulePackWithCallbacks declarationsMap onDeclClick onDeclHover onDeclLeave onModuleHover node = do
  let moduleName = node.name
  let decls = fromMaybe [] $ Object.lookup moduleName declarationsMap

  if Array.null decls then do
    -- No declarations - likely a re-export module
    -- Detect: has dependencies but no declarations = umbrella/re-export module
    let isReexport = not (Array.null node.targets)
    if isReexport then log $ "[BubblePack] Re-export module: " <> moduleName <> " (depends on " <> show (Array.length node.targets) <> " modules)"
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
    -- Larger multiplier for neighborhood view where detail needs to be visible
    let packSize = node.r * 5.0 -- Make pack significantly bigger for readability
    let
      config = defaultPackConfig
        { size = { width: packSize, height: packSize }
        , padding = 1.5 -- Slightly more padding between circles
        }
    let packed = pack config packRoot

    -- Get all nodes for rendering with category tracking
    let allNodesWithCategory = getAllPackNodesWithCategory packed ""
    let packCircles = map packNodeToCircleWithCategory allNodesWithCategory

    -- Center offset for positioning circles relative to group origin
    let centerOffset = packSize / 2.0

    -- Render using FFI with proper data binding, click, hover, and module hover callbacks
    renderBoundBubblePack_ "#explorer-nodes" node packCircles centerOffset getPackFillEffect onDeclClick onDeclHover onDeclLeave onModuleHover

    -- Return the pack's radius
    let PackNode rootData = packed
    pure rootData.r

-- | Convert PackNode to flat PackCircle for FFI (with category)
packNodeToCircleWithCategory :: { node :: PackNode String, category :: String } -> PackCircle
packNodeToCircleWithCategory { node: PackNode n, category } =
  { x: n.x
  , y: n.y
  , r: n.r
  , depth: n.depth
  , data_: n.data_
  , category
  }

-- | Get fill color as Effect (for FFI callback)
getPackFillEffect :: PackCircle -> Effect String
getPackFillEffect pc = pure $ getPackFill pc

-- | Get fill color based on depth
-- | Depth 0 (module): dark gray
-- | Depth 1 (category): category color
-- | Depth 2+ (declaration): intense version of parent category color
getPackFill :: PackCircle -> String
getPackFill pc = case pc.depth of
  0 -> "#333" -- Module (outer) - dark
  1 -> categoryColor pc.data_ -- Category
  _ -> categoryColorIntense pc.category -- Declaration uses intense parent category color


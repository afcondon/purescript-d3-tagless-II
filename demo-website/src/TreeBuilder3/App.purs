module TreeBuilder3.App
  ( component
  ) where

-- | TreeBuilder3: DSL Grammar Tree Builder
-- |
-- | An interactive demo for building PSD3 AST trees:
-- | - Keyboard-driven with sub-menus for node types
-- | - Each AST node type has a distinct color
-- | - Supports: Node, Join, NestedJoin, SceneJoin, SceneNestedJoin
-- | - Node children include Attr and Behavior nodes
-- | - Sub-menus for ElementType, AttrName, AttrValue, Behavior
-- |
-- | See GRAMMAR.md for the full grammar specification.

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.List as Data.List
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Link (linkBezierVertical)
import DataViz.Layout.Hierarchy.Tree (defaultTreeConfig, tree)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PSD3.Internal.Behavior.FFI (attachZoom_)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..))
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (SEmpty, ElementType(..))
import PSD3.Expr.Integration (evalAttr, evalAttrStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Expr.Friendly (textContent) as Friendly
import PSD3.AST as T
import PSD3.Transform (clearContainer)
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (focus)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

-- =============================================================================
-- DSL Node Types
-- =============================================================================

-- | The different AST node types from the PSD3 grammar
-- | Includes "pending" states for nodes awaiting further input
data DslNodeType
  = NodeElem ElementType -- Element node (SVG, Group, Circle, etc.)
  | NodeJoin -- Simple data join
  | NodeNestedJoin -- Type-decomposing join
  | NodeSceneJoin -- GUP join
  | NodeSceneNestedJoin -- GUP + type decomposition
  | NodeAttr AttrKind -- Attribute (fully specified)
  | NodeBehavior BehaviorKind -- Behavior (fully specified)
  -- GUP selection phases (auto-created under SceneJoin templates)
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

-- | Get color for a DSL node type
nodeColor :: DslNodeType -> String
nodeColor (NodeElem _) = "#6B7280" -- Gray
nodeColor NodeJoin = "#E2D24A" -- Yellow
nodeColor NodeNestedJoin = "#D4A017" -- Gold
nodeColor NodeSceneJoin = "#4A90E2" -- Blue
nodeColor NodeSceneNestedJoin = "#9B4AE2" -- Purple
nodeColor (NodeAttr _) = "#4AE24A" -- Green
nodeColor (NodeBehavior _) = "#E27A4A" -- Orange
-- GUP selection phases (classic GUP demo colors)
nodeColor NodeEnter = "#2CA02C" -- Green (enter = new)
nodeColor NodeUpdate = "#7F7F7F" -- Gray (update = existing)
nodeColor NodeExit = "#8C564B" -- Brown (exit = removed)
-- Pending types - lighter/desaturated versions
nodeColor PendingElement = "#9CA3AF" -- Light gray
nodeColor PendingAttr = "#86EFAC" -- Light green
nodeColor (PendingAttrValue _) = "#86EFAC" -- Light green
nodeColor PendingBehavior = "#FDBA74" -- Light orange

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
nodeLabel NodeSceneJoin = "SceneJoin"
nodeLabel NodeSceneNestedJoin = "SceneNestedJoin"
nodeLabel (NodeAttr (AttrStatic name _)) = "attr:" <> name
nodeLabel (NodeAttr (AttrField name _)) = "attr:" <> name
nodeLabel (NodeAttr (AttrExpr name _)) = "attr:" <> name
nodeLabel (NodeAttr (AttrIndex name)) = "attr:" <> name
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
nodeKeyHints NodeSceneJoin = "[e]"
nodeKeyHints NodeSceneNestedJoin = "[e]"
-- Attr/Behavior nodes
nodeKeyHints (NodeAttr _) = "[a]" -- Attrs can add sibling attrs
nodeKeyHints (NodeBehavior _) = "[b]" -- Behaviors can add sibling behaviors
-- GUP selection phases - can have attrs
nodeKeyHints NodeEnter = "[a]"
nodeKeyHints NodeUpdate = "[a]"
nodeKeyHints NodeExit = "[a]"

-- =============================================================================
-- State Types
-- =============================================================================

-- | Our tree node data
type TreeNode =
  { id :: Int
  , nodeType :: DslNodeType
  , x :: Number
  , y :: Number
  , depth :: Int
  }

-- | Node data for D3 rendering
type RenderNode =
  { id :: Int
  , nodeType :: DslNodeType
  , x :: Number
  , y :: Number
  , depth :: Int
  , color :: String
  , strokeWidth :: Number
  , label :: String
  , keyHints :: String -- Valid key hints to show (e.g., "[e,j,n,s,x,a,b]")
  , isSelected :: Boolean -- Whether this node is currently selected
  , showAsStack :: Boolean -- Whether to draw as deck-of-cards (join templates + GUP phases)
  }

-- | Link data for rendering
type LinkData =
  { id :: String
  , sourceX :: Number
  , sourceY :: Number
  , targetX :: Number
  , targetY :: Number
  }

-- | Component state
type State =
  { userTree :: Tree TreeNode
  , selectedNodeId :: Maybe Int
  , nextId :: Int
  , clickListener :: Maybe (HS.Listener Action)
  }

-- | Actions
data Action
  = Initialize
  | NodeClicked Int
  | HandleKeyDown KeyboardEvent
  | RenderTree

-- =============================================================================
-- Initial State
-- =============================================================================

initialTree :: Tree TreeNode
initialTree = mkTree
  { id: 0
  , nodeType: NodeElem SVG
  , x: 0.0
  , y: 0.0
  , depth: 0
  }
  Nil

initialState :: State
initialState =
  { userTree: initialTree
  , selectedNodeId: Just 0 -- Start with root selected
  , nextId: 1
  , clickListener: Nothing
  }

-- =============================================================================
-- Tree Operations
-- =============================================================================

-- isLeaf :: forall a. Tree a -> Boolean
-- isLeaf t = case tail t of
--   Nil -> true
--   _ -> false

findParentId :: Int -> Tree TreeNode -> Maybe Int
findParentId targetId t =
  let
    val = head t
    children = tail t
    childIds = map (\c -> (head c).id) children
  in
    if Array.elem targetId (Array.fromFoldable childIds) then Just val.id
    else Array.foldl
      ( \acc c -> case acc of
          Just pid -> Just pid
          Nothing -> findParentId targetId c
      )
      Nothing
      (Array.fromFoldable children)

getChildrenIds :: Int -> Tree TreeNode -> Array Int
getChildrenIds targetId t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId then map (\c -> (head c).id) (Array.fromFoldable children)
    else Array.foldl (\acc c -> if Array.null acc then getChildrenIds targetId c else acc)
      []
      (Array.fromFoldable children)

-- | Check if a node's parent is a Join type (making it a "template" node)
isJoinChild :: Int -> Tree TreeNode -> Boolean
isJoinChild nodeId t = case findParentId nodeId t of
  Nothing -> false
  Just parentId -> case findNodeById parentId t of
    Nothing -> false
    Just parent -> isJoinType parent.nodeType

-- | Check if a node's parent is a Scene Join type (GUP - has enter/update/exit)
isSceneJoinChild :: Int -> Tree TreeNode -> Boolean
isSceneJoinChild nodeId t = case findParentId nodeId t of
  Nothing -> false
  Just parentId -> case findNodeById parentId t of
    Nothing -> false
    Just parent -> isSceneJoinType parent.nodeType

-- | Check if a node type is a Join variant
isJoinType :: DslNodeType -> Boolean
isJoinType NodeJoin = true
isJoinType NodeNestedJoin = true
isJoinType NodeSceneJoin = true
isJoinType NodeSceneNestedJoin = true
isJoinType _ = false

-- | Check if a node type is a Scene Join (GUP) variant
isSceneJoinType :: DslNodeType -> Boolean
isSceneJoinType NodeSceneJoin = true
isSceneJoinType NodeSceneNestedJoin = true
isSceneJoinType _ = false

addChildToNode :: Int -> TreeNode -> Tree TreeNode -> Tree TreeNode
addChildToNode targetId newChild t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId then mkTree val (children <> (mkTree newChild Nil : Nil))
    else mkTree val (map (addChildToNode targetId newChild) children)

flattenTree :: Tree TreeNode -> Array TreeNode
flattenTree = Array.fromFoldable

makeLinks :: Tree TreeNode -> Array LinkData
makeLinks t =
  let
    val = head t
    children = tail t
    childLinks = Array.fromFoldable children >>= \child ->
      let
        childVal = head child
      in
        [ { id: show val.id <> "->" <> show childVal.id
          , sourceX: val.x
          , sourceY: val.y
          , targetX: childVal.x
          , targetY: childVal.y
          }
        ]
    grandchildLinks = Array.fromFoldable children >>= makeLinks
  in
    childLinks <> grandchildLinks

applyLayout :: Tree TreeNode -> Tree TreeNode
applyLayout t =
  let
    config = defaultTreeConfig
      { size = { width: 700.0, height: 600.0 }
      , minSeparation = 2.0
      , layerSeparation = Just 60.0 -- Fixed 60px between layers (2.5x node height)
      }
  in
    tree config t

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- | Ref label for the container div (for focusing)
containerRef :: H.RefLabel
containerRef = H.RefLabel "tree-builder3-main"

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "tree-builder3-container")
    , HP.tabIndex 0
    , HP.ref containerRef
    , HE.onKeyDown HandleKeyDown
    ]
    [ HH.h2_ [ HH.text "DSL Grammar Tree Builder" ]
    , HH.p
        [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.text "Click to select, arrows to navigate. Key hints shown next to selected node." ]
    , HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-svg-container")
        , HP.id "tree-builder3-container"
        ]
        []
    ]

-- =============================================================================
-- Event Handling
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- liftEffect HS.create
    H.modify_ _ { clickListener = Just listener }
    void $ H.subscribe emitter
    handleAction RenderTree
    liftEffect setupZoom
    -- Focus the container so it receives keyboard events immediately
    maybeElem <- H.getHTMLElementRef containerRef
    for_ maybeElem \elem -> liftEffect $ focus elem

  NodeClicked nodeId -> do
    state <- H.get
    case state.selectedNodeId of
      Just selectedId | selectedId == nodeId ->
        H.modify_ \s -> s { selectedNodeId = Nothing }
      _ ->
        H.modify_ \s -> s { selectedNodeId = Just nodeId }
    handleAction RenderTree

  HandleKeyDown event -> do
    let keyName = KE.key event
    handleNoMenuKey keyName -- All key handling is context-based on selected node

  RenderTree -> do
    state <- H.get
    case state.clickListener of
      Just listener -> liftEffect $ renderTreeViz state listener
      Nothing -> pure unit

-- | Handle keys - context depends on selected node's type
handleNoMenuKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleNoMenuKey keyName = do
  state <- H.get

  -- Navigation always works
  case keyName of
    "ArrowUp" -> navigateUp
    "ArrowDown" -> navigateDown
    "ArrowLeft" -> navigateLeft
    "ArrowRight" -> navigateRight
    _ ->
      -- Other keys depend on selected node's type
      case getSelectedNodeType state of
        Nothing -> pure unit -- No selection, no action
        Just nodeType -> handleKeyForNodeType nodeType keyName

-- | Handle key based on the selected node's type
handleKeyForNodeType :: forall output m. MonadAff m => DslNodeType -> String -> H.HalogenM State Action () output m Unit
handleKeyForNodeType PendingElement keyName = handlePendingElementKey keyName
handleKeyForNodeType PendingAttr keyName = handlePendingAttrKey keyName
handleKeyForNodeType (PendingAttrValue attrName) keyName = handlePendingAttrValueKey attrName keyName
handleKeyForNodeType PendingBehavior keyName = handlePendingBehaviorKey keyName
-- Resolved nodes - grammar-constrained handlers (element-specific)
handleKeyForNodeType (NodeElem elemType) keyName = handleElementKey elemType keyName
handleKeyForNodeType NodeJoin keyName = handleJoinKey keyName
handleKeyForNodeType NodeNestedJoin keyName = handleJoinKey keyName
handleKeyForNodeType NodeSceneJoin keyName = handleJoinKey keyName
handleKeyForNodeType NodeSceneNestedJoin keyName = handleJoinKey keyName
handleKeyForNodeType (NodeAttr _) keyName = handleAttrKey keyName
handleKeyForNodeType (NodeBehavior _) keyName = handleBehaviorKey keyName
-- GUP phase nodes - can add attrs
handleKeyForNodeType NodeEnter keyName = handleGupPhaseKey keyName
handleKeyForNodeType NodeUpdate keyName = handleGupPhaseKey keyName
handleKeyForNodeType NodeExit keyName = handleGupPhaseKey keyName

-- | Handle keys for Element nodes - element-type specific
handleElementKey :: forall output m. MonadAff m => ElementType -> String -> H.HalogenM State Action () output m Unit
handleElementKey elemType keyName = case elemType of
  -- Container elements (SVG, Group) - can have all children
  SVG -> handleContainerElementKey keyName
  Group -> handleContainerElementKey keyName
  -- Defs - only attrs (simplified)
  Defs -> handleDefsKey keyName
  -- Leaf elements - only attrs and behaviors
  _ -> handleLeafElementKey keyName

-- | Handle keys for container elements (SVG, Group) - can have all children
handleContainerElementKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleContainerElementKey keyName = case keyName of
  "e" -> addNodeOfType PendingElement
  "j" -> addNodeOfType NodeJoin
  "n" -> addNodeOfType NodeNestedJoin
  "s" -> addNodeOfType NodeSceneJoin
  "x" -> addNodeOfType NodeSceneNestedJoin
  "a" -> addNodeOfType PendingAttr
  "b" -> addNodeOfType PendingBehavior
  _ -> pure unit

-- | Handle keys for Defs - only attrs
handleDefsKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleDefsKey keyName = case keyName of
  "a" -> addNodeOfType PendingAttr
  _ -> pure unit

-- | Handle keys for leaf elements (Circle, Rect, etc.) - only attrs and behaviors
handleLeafElementKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleLeafElementKey keyName = case keyName of
  "a" -> addNodeOfType PendingAttr
  "b" -> addNodeOfType PendingBehavior
  _ -> pure unit

-- | Handle keys for Join nodes - can only add element template (and only one)
handleJoinKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleJoinKey keyName = do
  state <- H.get
  case keyName of
    "e" -> do
      -- Only allow adding template if Join doesn't already have one
      for_ state.selectedNodeId \selectedId -> do
        let children = getChildrenIds selectedId state.userTree
        when (Array.null children) do
          addNodeOfType PendingElement
    _ -> pure unit

-- | Handle keys for resolved Attr nodes - can add sibling attrs
handleAttrKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleAttrKey keyName = case keyName of
  "a" -> addSiblingOfType PendingAttr
  _ -> pure unit

-- | Handle keys for resolved Behavior nodes - can add sibling behaviors
handleBehaviorKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleBehaviorKey keyName = case keyName of
  "b" -> addSiblingOfType PendingBehavior
  _ -> pure unit

-- | Handle keys for GUP phase nodes (Enter/Update/Exit) - can add attrs
handleGupPhaseKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleGupPhaseKey keyName = case keyName of
  "a" -> addNodeOfType PendingAttr
  _ -> pure unit

-- | Handle keys for pending element node
-- | Note: SVG (s) is not allowed - SVG is root only
handlePendingElementKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handlePendingElementKey keyName = case keyName of
  "g" -> resolveSelectedNode (NodeElem Group)
  "c" -> resolveSelectedNode (NodeElem Circle)
  "r" -> resolveSelectedNode (NodeElem Rect)
  "p" -> resolveSelectedNode (NodeElem Path)
  "l" -> resolveSelectedNode (NodeElem Line)
  "t" -> resolveSelectedNode (NodeElem Text)
  "d" -> resolveSelectedNode (NodeElem Defs)
  "Escape" -> deleteSelectedNode -- Cancel pending node
  _ -> pure unit

-- | Handle keys for pending attr node (selecting attr name)
handlePendingAttrKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handlePendingAttrKey keyName = case keyName of
  "c" -> resolveSelectedNode (PendingAttrValue "cx")
  "x" -> resolveSelectedNode (PendingAttrValue "x")
  "y" -> resolveSelectedNode (PendingAttrValue "y")
  "r" -> resolveSelectedNode (PendingAttrValue "r")
  "f" -> resolveSelectedNode (PendingAttrValue "fill")
  "s" -> resolveSelectedNode (PendingAttrValue "stroke")
  "w" -> resolveSelectedNode (PendingAttrValue "width")
  "h" -> resolveSelectedNode (PendingAttrValue "height")
  "t" -> resolveSelectedNode (PendingAttrValue "transform")
  "Escape" -> deleteSelectedNode
  _ -> pure unit

-- | Handle keys for pending attr value (selecting value type)
handlePendingAttrValueKey :: forall output m. MonadAff m => String -> String -> H.HalogenM State Action () output m Unit
handlePendingAttrValueKey attrName keyName = case keyName of
  "l" -> resolveSelectedNode (NodeAttr (AttrStatic attrName "100"))
  "f" -> resolveSelectedNode (NodeAttr (AttrField attrName "value"))
  "e" -> resolveSelectedNode (NodeAttr (AttrExpr attrName "d.x + 10"))
  "i" -> resolveSelectedNode (NodeAttr (AttrIndex attrName))
  "Escape" -> deleteSelectedNode
  _ -> pure unit

-- | Handle keys for pending behavior node
handlePendingBehaviorKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handlePendingBehaviorKey keyName = case keyName of
  "z" -> resolveSelectedNode (NodeBehavior BehaviorZoom)
  "d" -> resolveSelectedNode (NodeBehavior BehaviorDrag)
  "c" -> resolveSelectedNode (NodeBehavior BehaviorClick)
  "h" -> resolveSelectedNode (NodeBehavior BehaviorHover)
  "Escape" -> deleteSelectedNode
  _ -> pure unit

-- | Delete the selected node (for canceling pending nodes)
deleteSelectedNode :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
deleteSelectedNode = do
  state <- H.get
  for_ state.selectedNodeId \selectedId -> do
    -- Don't delete root
    when (selectedId /= 0) do
      -- Find parent and select it
      let parentId = findParentId selectedId state.userTree
      let newTree = removeNodeById selectedId state.userTree
      H.modify_ \s -> s
        { userTree = newTree
        , selectedNodeId = parentId
        }
      handleAction RenderTree

-- | Remove a node by id from the tree
removeNodeById :: Int -> Tree TreeNode -> Tree TreeNode
removeNodeById targetId t =
  let
    val = head t
    children = tail t
    filteredChildren = Data.List.filter (\child -> (head child).id /= targetId) children
  in
    mkTree val (map (removeNodeById targetId) filteredChildren)

-- | Add a node of the given type as child of selected node
-- | Add a node as child of selected, and select the new node
addNodeOfType :: forall output m. MonadAff m => DslNodeType -> H.HalogenM State Action () output m Unit
addNodeOfType nodeType = do
  state <- H.get
  for_ state.selectedNodeId \selectedId -> do
    let newId = state.nextId
    let newChild = { id: newId, nodeType, x: 0.0, y: 0.0, depth: 0 }
    let newTree = addChildToNode selectedId newChild state.userTree
    H.modify_ \s -> s
      { userTree = newTree
      , nextId = s.nextId + 1
      , selectedNodeId = Just newId -- Select the new node
      }
    handleAction RenderTree

-- | Add a node as sibling of selected (adds to parent)
addSiblingOfType :: forall output m. MonadAff m => DslNodeType -> H.HalogenM State Action () output m Unit
addSiblingOfType nodeType = do
  state <- H.get
  for_ state.selectedNodeId \selectedId -> do
    -- Find parent of selected node
    case findParentId selectedId state.userTree of
      Nothing -> pure unit -- Root has no parent, can't add sibling
      Just parentId -> do
        let newId = state.nextId
        let newChild = { id: newId, nodeType, x: 0.0, y: 0.0, depth: 0 }
        let newTree = addChildToNode parentId newChild state.userTree
        H.modify_ \s -> s
          { userTree = newTree
          , nextId = s.nextId + 1
          , selectedNodeId = Just newId -- Select the new sibling
          }
        handleAction RenderTree

-- | Get the node type of the selected node
getSelectedNodeType :: State -> Maybe DslNodeType
getSelectedNodeType state = do
  selectedId <- state.selectedNodeId
  node <- findNodeById selectedId state.userTree
  pure node.nodeType

-- | Find a node by id in the tree
findNodeById :: Int -> Tree TreeNode -> Maybe TreeNode
findNodeById targetId t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId then Just val
    else Array.foldl (\acc child -> acc <|> findNodeById targetId child) Nothing (Array.fromFoldable children)

-- | Update a node's type in the tree
updateNodeType :: Int -> DslNodeType -> Tree TreeNode -> Tree TreeNode
updateNodeType targetId newType t =
  let
    val = head t
    children = tail t
    newVal = if val.id == targetId then val { nodeType = newType } else val
  in
    mkTree newVal (map (updateNodeType targetId newType) children)

-- | Update the selected node's type (for resolving pending nodes)
-- | If resolving an Element under a SceneJoin, auto-create Enter/Update/Exit children
resolveSelectedNode :: forall output m. MonadAff m => DslNodeType -> H.HalogenM State Action () output m Unit
resolveSelectedNode newType = do
  state <- H.get
  for_ state.selectedNodeId \selectedId -> do
    -- First, update the node type
    let treeWithType = updateNodeType selectedId newType state.userTree

    -- Check if this is an Element being resolved under a SceneJoin
    let isElementUnderScene = case newType of
          NodeElem _ -> isSceneJoinChild selectedId state.userTree
          _ -> false

    -- If so, auto-create Enter/Update/Exit children
    if isElementUnderScene then do
      let enterId = state.nextId
      let updateId = state.nextId + 1
      let exitId = state.nextId + 2
      let enterNode = { id: enterId, nodeType: NodeEnter, x: 0.0, y: 0.0, depth: 0 }
      let updateNode = { id: updateId, nodeType: NodeUpdate, x: 0.0, y: 0.0, depth: 0 }
      let exitNode = { id: exitId, nodeType: NodeExit, x: 0.0, y: 0.0, depth: 0 }
      let treeWithEnter = addChildToNode selectedId enterNode treeWithType
      let treeWithUpdate = addChildToNode selectedId updateNode treeWithEnter
      let treeWithExit = addChildToNode selectedId exitNode treeWithUpdate
      H.modify_ \s -> s
        { userTree = treeWithExit
        , nextId = s.nextId + 3
        }
    else
      H.modify_ \s -> s { userTree = treeWithType }

    handleAction RenderTree

-- | Navigation helpers
navigateUp :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
navigateUp = do
  state <- H.get
  case state.selectedNodeId of
    Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }
    Just selectedId ->
      case findParentId selectedId state.userTree of
        Just parentId -> H.modify_ \s -> s { selectedNodeId = Just parentId }
        Nothing -> pure unit
  handleAction RenderTree

navigateDown :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
navigateDown = do
  state <- H.get
  case state.selectedNodeId of
    Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }
    Just selectedId ->
      case Array.head (getChildrenIds selectedId state.userTree) of
        Just childId -> H.modify_ \s -> s { selectedNodeId = Just childId }
        Nothing -> pure unit
  handleAction RenderTree

navigateLeft :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
navigateLeft = do
  state <- H.get
  case state.selectedNodeId of
    Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }
    Just selectedId ->
      case findParentId selectedId state.userTree of
        Nothing -> pure unit
        Just parentId ->
          let
            siblings = getChildrenIds parentId state.userTree
            currentIdx = Array.elemIndex selectedId siblings
          in
            case currentIdx of
              Just idx | idx > 0 ->
                case Array.index siblings (idx - 1) of
                  Just prevId -> H.modify_ \s -> s { selectedNodeId = Just prevId }
                  Nothing -> pure unit
              _ -> pure unit
  handleAction RenderTree

navigateRight :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
navigateRight = do
  state <- H.get
  case state.selectedNodeId of
    Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }
    Just selectedId ->
      case findParentId selectedId state.userTree of
        Nothing -> pure unit
        Just parentId ->
          let
            siblings = getChildrenIds parentId state.userTree
            currentIdx = Array.elemIndex selectedId siblings
          in
            case currentIdx of
              Just idx ->
                case Array.index siblings (idx + 1) of
                  Just nextId -> H.modify_ \s -> s { selectedNodeId = Just nextId }
                  Nothing -> pure unit
              Nothing -> pure unit
  handleAction RenderTree

-- =============================================================================
-- D3/PSD3 Rendering
-- =============================================================================

selectedStrokeWidth :: Number
selectedStrokeWidth = 4.0

normalStrokeWidth :: Number
normalStrokeWidth = 2.0

renderTreeViz :: State -> HS.Listener Action -> Effect Unit
renderTreeViz state listener = do
  clearContainer "#tree-builder3-container"

  let positioned = applyLayout state.userTree
  let nodes = flattenTree positioned
  let links = makeLinks positioned

  let renderNodes = map (toRenderNode state) nodes

  let svgWidth = 900.0
  let svgHeight = 600.0

  -- Center the tree horizontally in the SVG
  -- For a single node, minX == maxX, so we center on that point
  let
    firstX = case Array.head nodes of
      Just n -> n.x
      Nothing -> 0.0
  let minX = Array.foldl (\acc n -> min acc n.x) firstX nodes
  let maxX = Array.foldl (\acc n -> max acc n.x) firstX nodes
  let centerX = (minX + maxX) / 2.0
  let offsetX = (svgWidth / 2.0) - centerX
  let offsetY = 60.0

  runD3v2M do
    container <- select "#tree-builder3-container" :: _ (D3v2Selection_ SEmpty Element Unit)

    let
      linksTree :: T.Tree LinkData
      linksTree =
        T.named SVG "svg"
          [ evalAttr "width" (lit svgWidth)
          , evalAttr "height" (lit svgHeight)
          , evalAttrStr "viewBox" (str ("0 0 " <> show svgWidth <> " " <> show svgHeight))
          , evalAttrStr "id" (str "tree-builder3-svg")
          ]
          `T.withChild`
            ( T.named Group "zoomGroup"
                [ evalAttrStr "class" (str "zoom-group") ]
                `T.withChild`
                  ( T.named Group "linksGroup"
                      [ evalAttrStr "class" (str "links") ]
                      `T.withChild`
                        ( T.joinData "linkPaths" "path" links $ \link ->
                            T.elem Path
                              [ evalAttrStr "d"
                                  ( str
                                      ( linkBezierVertical
                                          (link.sourceX + offsetX)
                                          (link.sourceY + offsetY)
                                          (link.targetX + offsetX)
                                          (link.targetY + offsetY)
                                      )
                                  )
                              , evalAttrStr "fill" (str "none")
                              , evalAttrStr "stroke" (str "#888")
                              , evalAttr "stroke-width" (lit 2.0)
                              ]
                        )
                  )
            )

    linksSelections <- renderTree container linksTree
    zoomGroupSel <- liftEffect $ reselectD3v2 "zoomGroup" linksSelections

    -- Helper to create a stacked "punch card" rect at a given offset
    let
      stackedRect :: Number -> Number -> String -> T.Tree RenderNode
      stackedRect dx dy color =
        T.elem Rect
          [ evalAttr "x" (lit (-40.0 + dx))
          , evalAttr "y" (lit (-12.0 + dy))
          , evalAttr "width" (lit 80.0)
          , evalAttr "height" (lit 24.0)
          , evalAttr "rx" (lit 4.0)
          , evalAttrStr "fill" (str color)
          , evalAttrStr "stroke" (str "#333")
          , evalAttr "stroke-width" (lit 1.5)
          ]

    let
      nodesTree :: T.Tree RenderNode
      nodesTree =
        T.named Group "nodesGroup"
          [ evalAttrStr "class" (str "nodes") ]
          `T.withChild`
            ( T.joinData "nodeGroups" "g" renderNodes $ \node ->
                T.elem Group
                  [ evalAttrStr "transform" (str ("translate(" <> show (node.x + offsetX) <> "," <> show (node.y + offsetY) <> ")"))
                  , evalAttrStr "cursor" (str "pointer")
                  ]
                  `T.withBehaviors`
                    [ ClickWithDatum \n -> HS.notify listener (NodeClicked n.id) ]
                  `T.withChildren`
                    ( -- Stacked "punch card" rects for join templates + GUP phases (drawn back-to-front)
                      ( if node.showAsStack then
                          [ stackedRect 9.0 9.0 node.color -- Back card
                          , stackedRect 6.0 6.0 node.color -- Middle card
                          , stackedRect 3.0 3.0 node.color -- Front-ish card
                          ]
                        else []
                      )
                        <>
                          [ -- Main/front rect for the node
                            T.elem Rect
                              [ evalAttr "x" (lit (-40.0))
                              , evalAttr "y" (lit (-12.0))
                              , evalAttr "width" (lit 80.0)
                              , evalAttr "height" (lit 24.0)
                              , evalAttr "rx" (lit 4.0)
                              , evalAttrStr "fill" (str node.color)
                              , evalAttrStr "stroke" (str "#333")
                              , evalAttr "stroke-width" (lit node.strokeWidth)
                              ]
                          , -- Label text
                            T.elem Text
                              [ evalAttr "x" (lit 0.0)
                              , evalAttr "y" (lit 4.0)
                              , evalAttrStr "text-anchor" (str "middle")
                              , evalAttrStr "fill" (str "white")
                              , evalAttrStr "font-size" (str "11px")
                              , evalAttrStr "font-weight" (str "bold")
                              , Friendly.textContent (str node.label)
                              ]
                          , -- Key hints (shown to right of selected node)
                            T.elem Text
                              [ evalAttr "x" (lit 50.0) -- Right of the node rect
                              , evalAttr "y" (lit 4.0)
                              , evalAttrStr "text-anchor" (str "start")
                              , evalAttrStr "fill" (str "#666")
                              , evalAttrStr "font-size" (str "10px")
                              , evalAttrStr "font-family" (str "monospace")
                              , evalAttrStr "opacity" (str (if node.isSelected then "1" else "0"))
                              , Friendly.textContent (str node.keyHints)
                              ]
                          ]
                    )
            )

    _ <- renderTree zoomGroupSel nodesTree

    pure unit

toRenderNode :: State -> TreeNode -> RenderNode
toRenderNode state node =
  let
    selected = state.selectedNodeId == Just node.id
    -- Show as stack: join templates OR GUP phase nodes (Enter/Update/Exit)
    isGupPhase = case node.nodeType of
      NodeEnter -> true
      NodeUpdate -> true
      NodeExit -> true
      _ -> false
    shouldStack = isJoinChild node.id state.userTree || isGupPhase
  in
    { id: node.id
    , nodeType: node.nodeType
    , x: node.x
    , y: node.y
    , depth: node.depth
    , color: nodeColor node.nodeType
    , strokeWidth: if selected then selectedStrokeWidth else normalStrokeWidth
    , label: nodeLabel node.nodeType
    , keyHints: computeKeyHints state.userTree node
    , isSelected: selected
    , showAsStack: shouldStack
    }

-- | Compute key hints dynamically based on tree state
-- | Accounts for constraints like "Join can only have one template"
computeKeyHints :: Tree TreeNode -> TreeNode -> String
computeKeyHints tree node = case node.nodeType of
  -- Join types: show [e] only if no template yet, otherwise []
  NodeJoin -> joinHints node.id
  NodeNestedJoin -> joinHints node.id
  NodeSceneJoin -> joinHints node.id
  NodeSceneNestedJoin -> joinHints node.id
  -- All other types use static hints
  _ -> nodeKeyHints node.nodeType
  where
  joinHints nodeId =
    let
      children = getChildrenIds nodeId tree
    in
      if Array.null children then "[e]" else "[]"

setupZoom :: Effect Unit
setupZoom = do
  doc <- window >>= document
  let parentNode = Document.toParentNode (toDocument doc)
  maybeSvg <- querySelector (QuerySelector "#tree-builder3-svg") parentNode
  for_ maybeSvg \svg -> do
    let ScaleExtent minScale maxScale = ScaleExtent 0.5 4.0
    _ <- attachZoom_ svg minScale maxScale ".zoom-group"
    pure unit

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
import Data.Maybe (Maybe(..))
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
import PSD3.Expr.Integration (v3Attr, v3AttrStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Expr.Friendly (textContent) as Friendly
import PSD3.AST as T
import PSD3.Transform (clearContainer)
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

-- =============================================================================
-- DSL Node Types
-- =============================================================================

-- | The different AST node types from the PSD3 grammar
data DslNodeType
  = NodeElem ElementType  -- Element node (SVG, Group, Circle, etc.)
  | NodeJoin              -- Simple data join
  | NodeNestedJoin        -- Type-decomposing join
  | NodeSceneJoin         -- GUP join
  | NodeSceneNestedJoin   -- GUP + type decomposition
  | NodeAttr AttrKind     -- Attribute
  | NodeBehavior BehaviorKind  -- Behavior

derive instance Eq DslNodeType
derive instance Ord DslNodeType

-- | Attribute kinds
data AttrKind
  = AttrStatic String String  -- name, value
  | AttrField String String   -- name, field
  | AttrExpr String String    -- name, expr
  | AttrIndex String          -- name (uses index)

derive instance Eq AttrKind
derive instance Ord AttrKind

-- | Behavior kinds
data BehaviorKind = BehaviorZoom | BehaviorDrag | BehaviorClick | BehaviorHover

derive instance Eq BehaviorKind
derive instance Ord BehaviorKind

-- | Get color for a DSL node type
nodeColor :: DslNodeType -> String
nodeColor (NodeElem _) = "#6B7280"      -- Gray
nodeColor NodeJoin = "#E2D24A"          -- Yellow
nodeColor NodeNestedJoin = "#D4A017"    -- Gold
nodeColor NodeSceneJoin = "#4A90E2"     -- Blue
nodeColor NodeSceneNestedJoin = "#9B4AE2" -- Purple
nodeColor (NodeAttr _) = "#4AE24A"      -- Green
nodeColor (NodeBehavior _) = "#E27A4A"  -- Orange

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
nodeLabel (NodeElem _) = "Element"  -- Other element types
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

-- =============================================================================
-- Menu Types
-- =============================================================================

-- | Menu state - which menu is currently open
data MenuState
  = NoMenu
  | TopLevelMenu          -- e, j, n, s, x, a, b
  | ElementTypeMenu       -- After pressing 'e'
  | AttrNameMenu          -- After pressing 'a'
  | AttrValueMenu String  -- After selecting attr name
  | BehaviorMenu          -- After pressing 'b'

derive instance Eq MenuState

-- | Menu item with key, label, and optional description
type MenuItem =
  { key :: String
  , label :: String
  , description :: String
  }

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
  , menuState :: MenuState
  , menuIndex :: Int  -- Currently highlighted menu item
  , clickListener :: Maybe (HS.Listener Action)
  }

-- | Actions
data Action
  = Initialize
  | NodeClicked Int
  | HandleKeyDown KeyboardEvent
  | RenderTree
  | CloseMenu

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
  , selectedNodeId: Just 0  -- Start with root selected
  , nextId: 1
  , menuState: NoMenu
  , menuIndex: 0
  , clickListener: Nothing
  }

-- =============================================================================
-- Menu Definitions
-- =============================================================================

topLevelMenuItems :: Array MenuItem
topLevelMenuItems =
  [ { key: "e", label: "Element", description: "Add element node (SVG, Circle, etc.)" }
  , { key: "j", label: "Join", description: "Add data join" }
  , { key: "n", label: "NestedJoin", description: "Add type-decomposing join" }
  , { key: "s", label: "SceneJoin", description: "Add GUP join" }
  , { key: "x", label: "SceneNestedJoin", description: "Add GUP + decomposition" }
  , { key: "a", label: "Attr", description: "Add attribute" }
  , { key: "b", label: "Behavior", description: "Add behavior" }
  ]

elementTypeMenuItems :: Array MenuItem
elementTypeMenuItems =
  [ { key: "s", label: "SVG", description: "Root SVG container" }
  , { key: "g", label: "Group", description: "Grouping element <g>" }
  , { key: "c", label: "Circle", description: "Circle element" }
  , { key: "r", label: "Rect", description: "Rectangle element" }
  , { key: "p", label: "Path", description: "Path element" }
  , { key: "l", label: "Line", description: "Line element" }
  , { key: "t", label: "Text", description: "Text element" }
  , { key: "d", label: "Defs", description: "Definitions (gradients, etc.)" }
  ]

attrNameMenuItems :: Array MenuItem
attrNameMenuItems =
  [ { key: "x", label: "x/cx", description: "Horizontal position" }
  , { key: "y", label: "y/cy", description: "Vertical position" }
  , { key: "w", label: "width", description: "Width" }
  , { key: "h", label: "height", description: "Height" }
  , { key: "r", label: "r", description: "Radius" }
  , { key: "f", label: "fill", description: "Fill color" }
  , { key: "s", label: "stroke", description: "Stroke color" }
  , { key: "o", label: "opacity", description: "Opacity (0-1)" }
  ]

attrValueMenuItems :: Array MenuItem
attrValueMenuItems =
  [ { key: "l", label: "Lit", description: "Static literal value" }
  , { key: "f", label: "Field", description: "Data field (d.field)" }
  , { key: "e", label: "Expr", description: "Expression" }
  , { key: "i", label: "Index", description: "Element index" }
  ]

behaviorMenuItems :: Array MenuItem
behaviorMenuItems =
  [ { key: "z", label: "Zoom", description: "Pan and zoom" }
  , { key: "d", label: "Drag", description: "Draggable" }
  , { key: "c", label: "Click", description: "Click handler" }
  , { key: "h", label: "Hover", description: "Hover handler" }
  ]

-- =============================================================================
-- Tree Operations
-- =============================================================================

-- isLeaf :: forall a. Tree a -> Boolean
-- isLeaf t = case tail t of
--   Nil -> true
--   _ -> false

findParentId :: Int -> Tree TreeNode -> Maybe Int
findParentId targetId t =
  let val = head t
      children = tail t
      childIds = map (\c -> (head c).id) children
  in if Array.elem targetId (Array.fromFoldable childIds)
     then Just val.id
     else Array.foldl (\acc c -> case acc of
       Just pid -> Just pid
       Nothing -> findParentId targetId c) Nothing (Array.fromFoldable children)

getChildrenIds :: Int -> Tree TreeNode -> Array Int
getChildrenIds targetId t =
  let val = head t
      children = tail t
  in if val.id == targetId
     then map (\c -> (head c).id) (Array.fromFoldable children)
     else Array.foldl (\acc c -> if Array.null acc then getChildrenIds targetId c else acc)
                       [] (Array.fromFoldable children)

addChildToNode :: Int -> TreeNode -> Tree TreeNode -> Tree TreeNode
addChildToNode targetId newChild t =
  let val = head t
      children = tail t
  in if val.id == targetId
     then mkTree val (children <> (mkTree newChild Nil : Nil))
     else mkTree val (map (addChildToNode targetId newChild) children)

flattenTree :: Tree TreeNode -> Array TreeNode
flattenTree = Array.fromFoldable

makeLinks :: Tree TreeNode -> Array LinkData
makeLinks t =
  let val = head t
      children = tail t
      childLinks = Array.fromFoldable children >>= \child ->
        let childVal = head child
        in [ { id: show val.id <> "->" <> show childVal.id
             , sourceX: val.x
             , sourceY: val.y
             , targetX: childVal.x
             , targetY: childVal.y
             } ]
      grandchildLinks = Array.fromFoldable children >>= makeLinks
  in childLinks <> grandchildLinks

applyLayout :: Tree TreeNode -> Tree TreeNode
applyLayout t =
  let config = defaultTreeConfig
        { size = { width: 700.0, height: 600.0 }
        , minSeparation = 2.0
        , layerSeparation = Just 60.0  -- Fixed 60px between layers (2.5x node height)
        }
  in tree config t

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

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "tree-builder3-container")
    , HP.tabIndex 0
    , HE.onKeyDown HandleKeyDown
    ]
    [ HH.h2_ [ HH.text "DSL Grammar Tree Builder" ]
    , HH.p
        [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.text $ case state.menuState of
            NoMenu -> "Press a key to add node: e=Element, j=Join, n=NestedJoin, s=SceneJoin, x=SceneNestedJoin, a=Attr, b=Behavior"
            TopLevelMenu -> "Select node type (arrows + Enter, or press key)"
            ElementTypeMenu -> "Select element type: s=SVG, g=Group, c=Circle, r=Rect, p=Path, l=Line, t=Text, d=Defs"
            AttrNameMenu -> "Select attribute: x=x/cx, y=y/cy, w=width, h=height, r=radius, f=fill, s=stroke, o=opacity"
            AttrValueMenu name -> "Attr '" <> name <> "': l=Lit, f=Field, e=Expr, i=Index"
            BehaviorMenu -> "Select behavior: z=Zoom, d=Drag, c=Click, h=Hover"
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-layout") ]
        [ -- Tree visualization container
          HH.div
            [ HP.class_ (HH.ClassName "tree-builder3-svg-container")
            , HP.id "tree-builder3-container"
            ]
            []
        , -- Menu panel (shown when menu is open)
          renderMenu state
        ]
    ]

renderMenu :: forall m. State -> H.ComponentHTML Action () m
renderMenu state =
  case state.menuState of
    NoMenu -> HH.text ""
    TopLevelMenu -> renderMenuPanel "Add Node" topLevelMenuItems state.menuIndex
    ElementTypeMenu -> renderMenuPanel "Element Type" elementTypeMenuItems state.menuIndex
    AttrNameMenu -> renderMenuPanel "Attribute Name" attrNameMenuItems state.menuIndex
    AttrValueMenu _ -> renderMenuPanel "Attribute Value" attrValueMenuItems state.menuIndex
    BehaviorMenu -> renderMenuPanel "Behavior" behaviorMenuItems state.menuIndex

renderMenuPanel :: forall m. String -> Array MenuItem -> Int -> H.ComponentHTML Action () m
renderMenuPanel title items selectedIndex =
  HH.div
    [ HP.class_ (HH.ClassName "menu-panel") ]
    [ HH.h3_ [ HH.text title ]
    , HH.ul
        [ HP.class_ (HH.ClassName "menu-items") ]
        (Array.mapWithIndex renderMenuItem items)
    , HH.p
        [ HP.class_ (HH.ClassName "menu-hint") ]
        [ HH.text "Arrow keys to navigate, Enter to select, Escape to cancel" ]
    ]
  where
  renderMenuItem idx item =
    HH.li
      [ HP.class_ (HH.ClassName $ if idx == selectedIndex then "menu-item selected" else "menu-item") ]
      [ HH.span [ HP.class_ (HH.ClassName "menu-key") ] [ HH.text $ "[" <> item.key <> "]" ]
      , HH.span [ HP.class_ (HH.ClassName "menu-label") ] [ HH.text item.label ]
      , HH.span [ HP.class_ (HH.ClassName "menu-desc") ] [ HH.text item.description ]
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

  NodeClicked nodeId -> do
    state <- H.get
    case state.selectedNodeId of
      Just selectedId | selectedId == nodeId ->
        H.modify_ \s -> s { selectedNodeId = Nothing }
      _ ->
        H.modify_ \s -> s { selectedNodeId = Just nodeId }
    handleAction RenderTree

  CloseMenu -> do
    H.modify_ \s -> s { menuState = NoMenu, menuIndex = 0 }

  HandleKeyDown event -> do
    state <- H.get
    let keyName = KE.key event

    case state.menuState of
      NoMenu -> handleNoMenuKey keyName
      TopLevelMenu -> handleTopLevelMenuKey keyName
      ElementTypeMenu -> handleElementTypeMenuKey keyName
      AttrNameMenu -> handleAttrNameMenuKey keyName
      AttrValueMenu attrName -> handleAttrValueMenuKey attrName keyName
      BehaviorMenu -> handleBehaviorMenuKey keyName

  RenderTree -> do
    state <- H.get
    case state.clickListener of
      Just listener -> liftEffect $ renderTreeViz state listener
      Nothing -> pure unit

-- | Handle keys when no menu is open
handleNoMenuKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleNoMenuKey keyName = do
  state <- H.get

  case keyName of
    -- Navigation
    "ArrowUp" -> navigateUp
    "ArrowDown" -> navigateDown
    "ArrowLeft" -> navigateLeft
    "ArrowRight" -> navigateRight

    -- Open menus for adding nodes (only when something selected)
    "e" -> when (state.selectedNodeId /= Nothing) $ H.modify_ \s -> s { menuState = ElementTypeMenu, menuIndex = 0 }
    "j" -> addNodeOfType NodeJoin
    "n" -> addNodeOfType NodeNestedJoin
    "s" -> addNodeOfType NodeSceneJoin
    "x" -> addNodeOfType NodeSceneNestedJoin
    "a" -> when (state.selectedNodeId /= Nothing) $ H.modify_ \s -> s { menuState = AttrNameMenu, menuIndex = 0 }
    "b" -> when (state.selectedNodeId /= Nothing) $ H.modify_ \s -> s { menuState = BehaviorMenu, menuIndex = 0 }

    _ -> pure unit

-- | Handle keys in top-level menu
handleTopLevelMenuKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleTopLevelMenuKey keyName = do
  state <- H.get
  let maxIndex = Array.length topLevelMenuItems - 1

  case keyName of
    "Escape" -> handleAction CloseMenu
    "ArrowUp" -> H.modify_ \s -> s { menuIndex = max 0 (s.menuIndex - 1) }
    "ArrowDown" -> H.modify_ \s -> s { menuIndex = min maxIndex (s.menuIndex + 1) }
    "Enter" -> selectTopLevelMenuItem state.menuIndex
    "e" -> H.modify_ \s -> s { menuState = ElementTypeMenu, menuIndex = 0 }
    "j" -> addNodeOfType NodeJoin *> handleAction CloseMenu
    "n" -> addNodeOfType NodeNestedJoin *> handleAction CloseMenu
    "s" -> addNodeOfType NodeSceneJoin *> handleAction CloseMenu
    "x" -> addNodeOfType NodeSceneNestedJoin *> handleAction CloseMenu
    "a" -> H.modify_ \s -> s { menuState = AttrNameMenu, menuIndex = 0 }
    "b" -> H.modify_ \s -> s { menuState = BehaviorMenu, menuIndex = 0 }
    _ -> pure unit

selectTopLevelMenuItem :: forall output m. MonadAff m => Int -> H.HalogenM State Action () output m Unit
selectTopLevelMenuItem idx = case idx of
  0 -> H.modify_ \s -> s { menuState = ElementTypeMenu, menuIndex = 0 }
  1 -> addNodeOfType NodeJoin *> handleAction CloseMenu
  2 -> addNodeOfType NodeNestedJoin *> handleAction CloseMenu
  3 -> addNodeOfType NodeSceneJoin *> handleAction CloseMenu
  4 -> addNodeOfType NodeSceneNestedJoin *> handleAction CloseMenu
  5 -> H.modify_ \s -> s { menuState = AttrNameMenu, menuIndex = 0 }
  6 -> H.modify_ \s -> s { menuState = BehaviorMenu, menuIndex = 0 }
  _ -> pure unit

-- | Handle keys in element type menu
handleElementTypeMenuKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleElementTypeMenuKey keyName = do
  state <- H.get
  let maxIndex = Array.length elementTypeMenuItems - 1

  case keyName of
    "Escape" -> handleAction CloseMenu
    "ArrowUp" -> H.modify_ \s -> s { menuIndex = max 0 (s.menuIndex - 1) }
    "ArrowDown" -> H.modify_ \s -> s { menuIndex = min maxIndex (s.menuIndex + 1) }
    "Enter" -> selectElementType state.menuIndex
    "s" -> addNodeOfType (NodeElem SVG) *> handleAction CloseMenu
    "g" -> addNodeOfType (NodeElem Group) *> handleAction CloseMenu
    "c" -> addNodeOfType (NodeElem Circle) *> handleAction CloseMenu
    "r" -> addNodeOfType (NodeElem Rect) *> handleAction CloseMenu
    "p" -> addNodeOfType (NodeElem Path) *> handleAction CloseMenu
    "l" -> addNodeOfType (NodeElem Line) *> handleAction CloseMenu
    "t" -> addNodeOfType (NodeElem Text) *> handleAction CloseMenu
    "d" -> addNodeOfType (NodeElem Defs) *> handleAction CloseMenu
    _ -> pure unit

selectElementType :: forall output m. MonadAff m => Int -> H.HalogenM State Action () output m Unit
selectElementType idx = do
  let elemKind = case idx of
        0 -> SVG
        1 -> Group
        2 -> Circle
        3 -> Rect
        4 -> Path
        5 -> Line
        6 -> Text
        _ -> Defs
  addNodeOfType (NodeElem elemKind)
  handleAction CloseMenu

-- | Handle keys in attr name menu
handleAttrNameMenuKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleAttrNameMenuKey keyName = do
  state <- H.get
  let maxIndex = Array.length attrNameMenuItems - 1

  case keyName of
    "Escape" -> handleAction CloseMenu
    "ArrowUp" -> H.modify_ \s -> s { menuIndex = max 0 (s.menuIndex - 1) }
    "ArrowDown" -> H.modify_ \s -> s { menuIndex = min maxIndex (s.menuIndex + 1) }
    "Enter" -> selectAttrName state.menuIndex
    "x" -> H.modify_ \s -> s { menuState = AttrValueMenu "x", menuIndex = 0 }
    "y" -> H.modify_ \s -> s { menuState = AttrValueMenu "y", menuIndex = 0 }
    "w" -> H.modify_ \s -> s { menuState = AttrValueMenu "width", menuIndex = 0 }
    "h" -> H.modify_ \s -> s { menuState = AttrValueMenu "height", menuIndex = 0 }
    "r" -> H.modify_ \s -> s { menuState = AttrValueMenu "r", menuIndex = 0 }
    "f" -> H.modify_ \s -> s { menuState = AttrValueMenu "fill", menuIndex = 0 }
    "s" -> H.modify_ \s -> s { menuState = AttrValueMenu "stroke", menuIndex = 0 }
    "o" -> H.modify_ \s -> s { menuState = AttrValueMenu "opacity", menuIndex = 0 }
    _ -> pure unit

selectAttrName :: forall output m. MonadAff m => Int -> H.HalogenM State Action () output m Unit
selectAttrName idx = do
  let attrName = case idx of
        0 -> "x"
        1 -> "y"
        2 -> "width"
        3 -> "height"
        4 -> "r"
        5 -> "fill"
        6 -> "stroke"
        _ -> "opacity"
  H.modify_ \s -> s { menuState = AttrValueMenu attrName, menuIndex = 0 }

-- | Handle keys in attr value menu
handleAttrValueMenuKey :: forall output m. MonadAff m => String -> String -> H.HalogenM State Action () output m Unit
handleAttrValueMenuKey attrName keyName = do
  state <- H.get
  let maxIndex = Array.length attrValueMenuItems - 1

  case keyName of
    "Escape" -> handleAction CloseMenu
    "ArrowUp" -> H.modify_ \s -> s { menuIndex = max 0 (s.menuIndex - 1) }
    "ArrowDown" -> H.modify_ \s -> s { menuIndex = min maxIndex (s.menuIndex + 1) }
    "Enter" -> selectAttrValue attrName state.menuIndex
    "l" -> addNodeOfType (NodeAttr (AttrStatic attrName "100")) *> handleAction CloseMenu
    "f" -> addNodeOfType (NodeAttr (AttrField attrName "value")) *> handleAction CloseMenu
    "e" -> addNodeOfType (NodeAttr (AttrExpr attrName "d.x + 10")) *> handleAction CloseMenu
    "i" -> addNodeOfType (NodeAttr (AttrIndex attrName)) *> handleAction CloseMenu
    _ -> pure unit

selectAttrValue :: forall output m. MonadAff m => String -> Int -> H.HalogenM State Action () output m Unit
selectAttrValue attrName idx = do
  let attrKind = case idx of
        0 -> AttrStatic attrName "100"
        1 -> AttrField attrName "value"
        2 -> AttrExpr attrName "d.x + 10"
        _ -> AttrIndex attrName
  addNodeOfType (NodeAttr attrKind)
  handleAction CloseMenu

-- | Handle keys in behavior menu
handleBehaviorMenuKey :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
handleBehaviorMenuKey keyName = do
  state <- H.get
  let maxIndex = Array.length behaviorMenuItems - 1

  case keyName of
    "Escape" -> handleAction CloseMenu
    "ArrowUp" -> H.modify_ \s -> s { menuIndex = max 0 (s.menuIndex - 1) }
    "ArrowDown" -> H.modify_ \s -> s { menuIndex = min maxIndex (s.menuIndex + 1) }
    "Enter" -> selectBehavior state.menuIndex
    "z" -> addNodeOfType (NodeBehavior BehaviorZoom) *> handleAction CloseMenu
    "d" -> addNodeOfType (NodeBehavior BehaviorDrag) *> handleAction CloseMenu
    "c" -> addNodeOfType (NodeBehavior BehaviorClick) *> handleAction CloseMenu
    "h" -> addNodeOfType (NodeBehavior BehaviorHover) *> handleAction CloseMenu
    _ -> pure unit

selectBehavior :: forall output m. MonadAff m => Int -> H.HalogenM State Action () output m Unit
selectBehavior idx = do
  let behaviorKind = case idx of
        0 -> BehaviorZoom
        1 -> BehaviorDrag
        2 -> BehaviorClick
        _ -> BehaviorHover
  addNodeOfType (NodeBehavior behaviorKind)
  handleAction CloseMenu

-- | Add a node of the given type as child of selected node
addNodeOfType :: forall output m. MonadAff m => DslNodeType -> H.HalogenM State Action () output m Unit
addNodeOfType nodeType = do
  state <- H.get
  for_ state.selectedNodeId \selectedId -> do
    let newChild = { id: state.nextId, nodeType, x: 0.0, y: 0.0, depth: 0 }
    let newTree = addChildToNode selectedId newChild state.userTree
    H.modify_ \s -> s
      { userTree = newTree
      , nextId = s.nextId + 1
      }
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
          let siblings = getChildrenIds parentId state.userTree
              currentIdx = Array.elemIndex selectedId siblings
          in case currentIdx of
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
          let siblings = getChildrenIds parentId state.userTree
              currentIdx = Array.elemIndex selectedId siblings
          in case currentIdx of
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
  let firstX = case Array.head nodes of
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
          [ v3Attr "width" (lit svgWidth)
          , v3Attr "height" (lit svgHeight)
          , v3AttrStr "viewBox" (str ("0 0 " <> show svgWidth <> " " <> show svgHeight))
          , v3AttrStr "id" (str "tree-builder3-svg")
          ]
          `T.withChild`
            ( T.named Group "zoomGroup"
                [ v3AttrStr "class" (str "zoom-group") ]
                `T.withChild`
                  ( T.named Group "linksGroup"
                      [ v3AttrStr "class" (str "links") ]
                      `T.withChild`
                        ( T.joinData "linkPaths" "path" links $ \link ->
                            T.elem Path
                              [ v3AttrStr "d" (str (linkBezierVertical
                                  (link.sourceX + offsetX)
                                  (link.sourceY + offsetY)
                                  (link.targetX + offsetX)
                                  (link.targetY + offsetY)))
                              , v3AttrStr "fill" (str "none")
                              , v3AttrStr "stroke" (str "#888")
                              , v3Attr "stroke-width" (lit 2.0)
                              ]
                        )
                  )
            )

    linksSelections <- renderTree container linksTree
    zoomGroupSel <- liftEffect $ reselectD3v2 "zoomGroup" linksSelections

    let
      nodesTree :: T.Tree RenderNode
      nodesTree =
        T.named Group "nodesGroup"
          [ v3AttrStr "class" (str "nodes") ]
          `T.withChild`
            ( T.joinData "nodeGroups" "g" renderNodes $ \node ->
                T.elem Group
                  [ v3AttrStr "transform" (str ("translate(" <> show (node.x + offsetX) <> "," <> show (node.y + offsetY) <> ")"))
                  , v3AttrStr "cursor" (str "pointer")
                  ]
                  `T.withBehaviors`
                    [ ClickWithDatum \n -> HS.notify listener (NodeClicked n.id) ]
                  `T.withChildren`
                    [ -- Background rect for the node
                      T.elem Rect
                        [ v3Attr "x" (lit (-40.0))
                        , v3Attr "y" (lit (-12.0))
                        , v3Attr "width" (lit 80.0)
                        , v3Attr "height" (lit 24.0)
                        , v3Attr "rx" (lit 4.0)
                        , v3AttrStr "fill" (str node.color)
                        , v3AttrStr "stroke" (str "#333")
                        , v3Attr "stroke-width" (lit node.strokeWidth)
                        ]
                    , -- Label text
                      T.elem Text
                        [ v3Attr "x" (lit 0.0)
                        , v3Attr "y" (lit 4.0)
                        , v3AttrStr "text-anchor" (str "middle")
                        , v3AttrStr "fill" (str "white")
                        , v3AttrStr "font-size" (str "11px")
                        , v3AttrStr "font-weight" (str "bold")
                        , Friendly.textContent (str node.label)
                        ]
                    ]
            )

    _ <- renderTree zoomGroupSel nodesTree

    pure unit

toRenderNode :: State -> TreeNode -> RenderNode
toRenderNode state node =
  let isSelected = state.selectedNodeId == Just node.id
  in
    { id: node.id
    , nodeType: node.nodeType
    , x: node.x
    , y: node.y
    , depth: node.depth
    , color: nodeColor node.nodeType
    , strokeWidth: if isSelected then selectedStrokeWidth else normalStrokeWidth
    , label: nodeLabel node.nodeType
    }

setupZoom :: Effect Unit
setupZoom = do
  doc <- window >>= document
  let parentNode = Document.toParentNode (toDocument doc)
  maybeSvg <- querySelector (QuerySelector "#tree-builder3-svg") parentNode
  for_ maybeSvg \svg -> do
    let ScaleExtent minScale maxScale = ScaleExtent 0.5 4.0
    _ <- attachZoom_ svg minScale maxScale ".zoom-group"
    pure unit

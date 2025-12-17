module TreeBuilder2.App
  ( component
  ) where

-- | TreeBuilder2: Interactive Tree Builder Demo
-- |
-- | An interactive demo where users build a tree by clicking nodes:
-- | - Start with a single blue root node
-- | - Click any node to select it (thick stroke indicates selection)
-- | - Press 'a' to add a green child, 'b' to add an orange child
-- | - Selection is retained after adding children (for adding siblings)
-- | - Arrow keys navigate the tree (up=parent, down=child, left/right=siblings)
-- | - Click selected node again to deselect
-- | - Tree uses vertical hierarchical layout with bezier links
-- | - SVG container supports zoom and pan
-- |
-- | Architecture:
-- | - Halogen manages UI state (tree data, selection, keyboard events)
-- | - PSD3/D3 renders the tree visualization into a container div

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
import PSD3.Expr.Integration (evalAttr, evalAttrStr)
import PSD3.Expr.Expr (lit, str)
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
-- Types
-- =============================================================================

-- | Node types in our tree
data NodeType = Root | TypeA | TypeB

derive instance Eq NodeType
derive instance Ord NodeType

-- | Get the color for a node type (when not selected)
nodeColor :: NodeType -> String
nodeColor Root = "#4A90E2"   -- Blue
nodeColor TypeA = "#4AE2A4"  -- Green
nodeColor TypeB = "#E27A4A"  -- Orange

-- | Selection stroke width (thick for selected, thin for normal)
selectedStrokeWidth :: Number
selectedStrokeWidth = 5.0

normalStrokeWidth :: Number
normalStrokeWidth = 2.0

-- | Our tree node data
type TreeNode =
  { id :: Int
  , nodeType :: NodeType
  , x :: Number
  , y :: Number
  , depth :: Int
  }

-- | Node data for D3 rendering (includes computed properties)
type RenderNode =
  { id :: Int
  , nodeType :: NodeType
  , x :: Number
  , y :: Number
  , depth :: Int
  , color :: String
  , strokeWidth :: Number
  , isLeaf :: Boolean
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
  { userTree :: Tree TreeNode          -- The tree structure being built
  , selectedNodeId :: Maybe Int        -- Currently selected node (if any)
  , nextId :: Int                      -- Next available node ID
  , clickListener :: Maybe (HS.Listener Action)  -- For node click events
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

-- | Initial tree with just the root node
initialTree :: Tree TreeNode
initialTree = mkTree
  { id: 0
  , nodeType: Root
  , x: 0.0
  , y: 0.0
  , depth: 0
  }
  Nil

initialState :: State
initialState =
  { userTree: initialTree
  , selectedNodeId: Nothing
  , nextId: 1
  , clickListener: Nothing
  }

-- =============================================================================
-- Tree Operations
-- =============================================================================

-- | Check if a node is a leaf (has no children)
isLeaf :: forall a. Tree a -> Boolean
isLeaf t = case tail t of
  Nil -> true
  _ -> false

-- | Find a node by ID and check if it's a leaf
isLeafById :: Int -> Tree TreeNode -> Boolean
isLeafById targetId t =
  let val = head t
      children = tail t
  in if val.id == targetId
     then isLeaf t
     else Array.any identity $ Array.fromFoldable $ map (isLeafById targetId) children

-- | Add a child to a node with the given ID
addChildToNode :: Int -> TreeNode -> Tree TreeNode -> Tree TreeNode
addChildToNode targetId newChild t =
  let val = head t
      children = tail t
  in if val.id == targetId
     then mkTree val (children <> (mkTree newChild Nil : Nil))
     else mkTree val (map (addChildToNode targetId newChild) children)

-- | Flatten tree to array of nodes
flattenTree :: Tree TreeNode -> Array TreeNode
flattenTree = Array.fromFoldable

-- | Find parent node ID for a given node
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

-- | Get children IDs of a node
getChildrenIds :: Int -> Tree TreeNode -> Array Int
getChildrenIds targetId t =
  let val = head t
      children = tail t
  in if val.id == targetId
     then map (\c -> (head c).id) (Array.fromFoldable children)
     else Array.foldl (\acc c -> if Array.null acc then getChildrenIds targetId c else acc)
                       [] (Array.fromFoldable children)

-- | Create links from tree structure
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

-- | Apply tree layout to position nodes
applyLayout :: Tree TreeNode -> Tree TreeNode
applyLayout t =
  let config = defaultTreeConfig
        { size = { width: 600.0, height: 400.0 }
        , minSeparation = 2.0
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
    [ HP.class_ (HH.ClassName "tree-builder2-container")
    , HP.tabIndex 0  -- Make focusable for keyboard events
    , HE.onKeyDown HandleKeyDown
    ]
    [ HH.h2_ [ HH.text "Interactive Tree Builder" ]
    , HH.p
        [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.text $ case state.selectedNodeId of
            Nothing -> "Click any node to select it, or use arrow keys to navigate"
            Just _ -> "Press 'a' for green child, 'b' for orange child. Arrow keys to navigate. Click to deselect."
        ]
    -- Container div for D3 to render into
    , HH.div
        [ HP.class_ (HH.ClassName "tree-builder2-svg-container")
        , HP.id "tree-builder2-container"
        ]
        []
    ]

-- =============================================================================
-- Event Handling
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Create emitter/listener for node clicks
    { emitter, listener } <- liftEffect HS.create
    H.modify_ _ { clickListener = Just listener }
    void $ H.subscribe emitter

    handleAction RenderTree
    -- Setup zoom behavior on the SVG after it's rendered
    liftEffect setupZoom

  NodeClicked nodeId -> do
    state <- H.get

    -- Allow clicking any node for navigation
    case state.selectedNodeId of
      -- If clicking the already-selected node, deselect
      Just selectedId | selectedId == nodeId ->
        H.modify_ \s -> s { selectedNodeId = Nothing }

      -- Otherwise, select this node
      _ ->
        H.modify_ \s -> s { selectedNodeId = Just nodeId }

    handleAction RenderTree

  HandleKeyDown event -> do
    state <- H.get
    let keyName = KE.key event

    case keyName of
      -- Arrow keys work even without selection (select root if nothing selected)
      "ArrowUp" -> do
        case state.selectedNodeId of
          Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }  -- Select root
          Just selectedId ->
            -- Go to parent
            case findParentId selectedId state.userTree of
              Just parentId -> H.modify_ \s -> s { selectedNodeId = Just parentId }
              Nothing -> pure unit  -- Already at root
        handleAction RenderTree

      "ArrowDown" -> do
        case state.selectedNodeId of
          Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }  -- Select root
          Just selectedId ->
            -- Go to first child
            case Array.head (getChildrenIds selectedId state.userTree) of
              Just childId -> H.modify_ \s -> s { selectedNodeId = Just childId }
              Nothing -> pure unit  -- No children (leaf node)
        handleAction RenderTree

      "ArrowLeft" -> do
        case state.selectedNodeId of
          Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }  -- Select root
          Just selectedId ->
            -- Go to previous sibling
            case findParentId selectedId state.userTree of
              Nothing -> pure unit  -- Root has no siblings
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

      "ArrowRight" -> do
        case state.selectedNodeId of
          Nothing -> H.modify_ \s -> s { selectedNodeId = Just 0 }  -- Select root
          Just selectedId ->
            -- Go to next sibling
            case findParentId selectedId state.userTree of
              Nothing -> pure unit  -- Root has no siblings
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

      -- 'a' and 'b' only work when a node is selected
      "a" ->
        for_ state.selectedNodeId \selectedId -> do
          -- Add green (TypeA) child (keep parent selected for adding siblings)
          let newChild = { id: state.nextId, nodeType: TypeA, x: 0.0, y: 0.0, depth: 0 }
          let newTree = addChildToNode selectedId newChild state.userTree
          H.modify_ \s -> s
            { userTree = newTree
            , nextId = s.nextId + 1
            }
          handleAction RenderTree

      "b" ->
        for_ state.selectedNodeId \selectedId -> do
          -- Add orange (TypeB) child (keep parent selected for adding siblings)
          let newChild = { id: state.nextId, nodeType: TypeB, x: 0.0, y: 0.0, depth: 0 }
          let newTree = addChildToNode selectedId newChild state.userTree
          H.modify_ \s -> s
            { userTree = newTree
            , nextId = s.nextId + 1
            }
          handleAction RenderTree

      _ -> pure unit

  RenderTree -> do
    state <- H.get
    case state.clickListener of
      Just listener -> liftEffect $ renderTreeViz state listener
      Nothing -> pure unit  -- No listener yet (shouldn't happen)

-- =============================================================================
-- D3/PSD3 Rendering
-- =============================================================================

-- | Render the tree visualization using PSD3
renderTreeViz :: State -> HS.Listener Action -> Effect Unit
renderTreeViz state listener = do
  -- Clear the container first
  clearContainer "#tree-builder2-container"

  -- Apply layout to get positioned tree
  let positioned = applyLayout state.userTree
  let nodes = flattenTree positioned
  let links = makeLinks positioned

  -- Convert nodes to render nodes with computed properties
  let renderNodes = map (toRenderNode state) nodes

  -- SVG dimensions
  let svgWidth = 800.0
  let svgHeight = 500.0

  -- Compute actual extent of positioned nodes to center dynamically
  let minX = Array.foldl (\acc n -> min acc n.x) 0.0 nodes
  let maxX = Array.foldl (\acc n -> max acc n.x) 0.0 nodes
  let actualWidth = maxX - minX
  -- Center the node extent in the SVG
  let offsetX = (svgWidth - actualWidth) / 2.0 - minX
  let offsetY = 80.0  -- Top margin + space for the root node (bigger margin for breathing room)

  runD3v2M do
    container <- select "#tree-builder2-container" :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Step 1: Build and render SVG with links
    let
      linksTree :: T.Tree LinkData
      linksTree =
        T.named SVG "svg"
          [ evalAttr "width" (lit svgWidth)
          , evalAttr "height" (lit svgHeight)
          , evalAttrStr "viewBox" (str ("0 0 " <> show svgWidth <> " " <> show svgHeight))
          , evalAttrStr "id" (str "tree-builder2-svg")
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
                              [ evalAttrStr "d" (str (linkBezierVertical
                                  (link.sourceX + offsetX)
                                  (link.sourceY + offsetY)
                                  (link.targetX + offsetX)
                                  (link.targetY + offsetY)))
                              , evalAttrStr "fill" (str "none")
                              , evalAttrStr "stroke" (str "#888")
                              , evalAttr "stroke-width" (lit 2.0)
                              ]
                        )
                  )
            )

    -- Render links (this creates the SVG structure)
    linksSelections <- renderTree container linksTree

    -- Step 2: Reselect the zoom group and add nodes
    zoomGroupSel <- liftEffect $ reselectD3v2 "zoomGroup" linksSelections

    -- Build nodes tree with click behavior
    let
      nodesTree :: T.Tree RenderNode
      nodesTree =
        T.named Group "nodesGroup"
          [ evalAttrStr "class" (str "nodes") ]
          `T.withChild`
            ( T.joinData "nodeCircles" "circle" renderNodes $ \node ->
                T.elem Circle
                  [ evalAttr "cx" (lit (node.x + offsetX))
                  , evalAttr "cy" (lit (node.y + offsetY))
                  , evalAttr "r" (lit 15.0)
                  , evalAttrStr "fill" (str node.color)
                  , evalAttrStr "stroke" (str "#333")
                  , evalAttr "stroke-width" (lit node.strokeWidth)
                  , evalAttrStr "cursor" (str "pointer")
                  ]
                  `T.withBehaviors`
                    [ ClickWithDatum \n -> HS.notify listener (NodeClicked n.id) ]
            )

    -- Render nodes on top of links
    _ <- renderTree zoomGroupSel nodesTree
    pure unit

-- | Convert a TreeNode to a RenderNode with computed properties
toRenderNode :: State -> TreeNode -> RenderNode
toRenderNode state node =
  let
    isSelected = state.selectedNodeId == Just node.id
    isNodeLeaf = isLeafById node.id state.userTree
  in
    { id: node.id
    , nodeType: node.nodeType
    , x: node.x
    , y: node.y
    , depth: node.depth
    , color: nodeColor node.nodeType  -- Always use node type color
    , strokeWidth: if isSelected then selectedStrokeWidth else normalStrokeWidth
    , isLeaf: isNodeLeaf
    }

-- | Setup zoom behavior on the SVG
setupZoom :: Effect Unit
setupZoom = do
  doc <- window >>= document
  let parentNode = Document.toParentNode (toDocument doc)
  maybeSvg <- querySelector (QuerySelector "#tree-builder2-svg") parentNode
  for_ maybeSvg \svg -> do
    let ScaleExtent minScale maxScale = ScaleExtent 0.5 4.0
    _ <- attachZoom_ svg minScale maxScale ".zoom-group"
    pure unit

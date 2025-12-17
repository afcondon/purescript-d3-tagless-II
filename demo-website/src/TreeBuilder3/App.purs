module TreeBuilder3.App
  ( component
  ) where

-- | TreeBuilder3: DSL Grammar Tree Builder
-- |
-- | An interactive demo for building PSD3 AST trees:
-- | - Keyboard-driven with sub-menus for node types
-- | - Each AST node type has a distinct color
-- | - Supports: Node, Join, NestedJoin, UpdateJoin, UpdateNestedJoin
-- | - Node children include Attr and Behavior nodes
-- | - Sub-menus for ElementType, AttrName, AttrValue, Behavior
-- | - Live code generation panel showing PSD3 DSL code
-- |
-- | See GRAMMAR.md for the full grammar specification.

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Int as Int
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
import PSD3.Interpreter.SemiQuine.TreeToCode (treeToCode)
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..), BehaviorKind(..), nodeColor, nodeLabel, nodeKeyHints)
import TreeBuilder3.Converter (builderTreeToAST)
import TreeBuilder3.ASTImporter (runImport)
import TreeBuilder3.FormInterpreter (astToForm, FormAction(..))
import PSD3.AST as AST
import PSD3.Expr.Friendly (cx, cy, r, fill, stroke, strokeWidth, num, text) as Attr
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
-- State Types
-- =============================================================================

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
  , nameLabel :: Maybe String -- Name to show on LHS (element name or join name)
  , keyLabel :: Maybe String -- Key to show (join key)
  , keyHints :: String -- Valid key hints to show (e.g., "[e,j,n,s,x,a,b]")
  , isSelected :: Boolean -- Whether this node is currently selected
  , showAsStack :: Boolean -- Whether to draw as deck-of-cards (join templates + GUP phases)
  , isBadge :: Boolean -- Whether to render as a small badge (attrs, behaviors)
  , badgeIndex :: Int -- Position in badge row (0, 1, 2, ...)
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
  , generatedCode :: String  -- Live generated PSD3 code
  }

-- | Actions
data Action
  = Initialize
  | NodeClicked Int
  | HandleKeyDown KeyboardEvent
  | RenderTree
  | ImportExample
  | HandleFormAction FormAction

-- =============================================================================
-- Initial State
-- =============================================================================

initialTree :: Tree TreeNode
initialTree = mkTree
  { id: 0
  , nodeType: NodeElem SVG
  , name: Nothing
  , key: Nothing
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
  , generatedCode: "-- Build a tree to see generated code"
  }

-- | Sample AST for testing import functionality
-- | A simple scatter plot structure: SVG > Group > Join(Circle[cx,cy,r,fill])
sampleAST :: AST.Tree Unit
sampleAST =
  AST.named AST.SVG "svg"
    [ Attr.fill $ Attr.text "none" ]
    `AST.withChild`
      (AST.named AST.Group "zoomGroup" []
        `AST.withChild`
          (AST.joinData "nodes" "circle" [unit] $ \_ ->
            AST.elem AST.Circle
              [ Attr.cx $ Attr.num 100.0
              , Attr.cy $ Attr.num 100.0
              , Attr.r $ Attr.num 5.0
              , Attr.fill $ Attr.text "#4a90e2"
              , Attr.stroke $ Attr.text "#333"
              , Attr.strokeWidth $ Attr.num 1.0
              ]
          )
      )

-- =============================================================================
-- Code Generation
-- =============================================================================

-- | Generate PSD3 code from the builder tree
generateCode :: Tree TreeNode -> String
generateCode builderTree =
  case builderTreeToAST builderTree of
    Left err -> "-- " <> show err
    Right ast -> treeToCode ast

-- =============================================================================
-- Tree Operations
-- =============================================================================

-- | Check if a node type should render as a badge (attr, behavior, pending variants)
isBadgeNodeType :: DslNodeType -> Boolean
isBadgeNodeType (NodeAttr _) = true
isBadgeNodeType (NodeBehavior _) = true
isBadgeNodeType PendingAttr = true
isBadgeNodeType (PendingAttrValue _) = true
isBadgeNodeType PendingBehavior = true
isBadgeNodeType _ = false

-- | Filter tree to only structural nodes (for layout calculation)
-- | Badge nodes are removed; their positions will be computed relative to parent
filterStructuralTree :: Tree TreeNode -> Tree TreeNode
filterStructuralTree t =
  let
    val = head t
    children = tail t
    -- Filter out badge children, recurse on structural children
    structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
  in
    mkTree val (map filterStructuralTree structuralChildren)

-- | Get badge children (attrs, behaviors) for a node
getBadgeChildren :: Int -> Tree TreeNode -> Array TreeNode
getBadgeChildren targetId t =
  let
    val = head t
    children = tail t
  in
    if val.id == targetId
      then Array.filter (\n -> isBadgeNodeType n.nodeType) (Array.fromFoldable (map head children))
      else Array.foldl (\acc c -> if Array.null acc then getBadgeChildren targetId c else acc)
             []
             (Array.fromFoldable children)

-- | Position badge nodes relative to their parent
-- | Returns array of (TreeNode with updated position, badge index)
positionBadges :: TreeNode -> Array TreeNode -> Array { node :: TreeNode, index :: Int }
positionBadges parent badges =
  Array.mapWithIndex (\i badge ->
    let
      -- Badges appear in a column to the right of the parent
      -- Each badge is stacked vertically with small gap
      badgeX = parent.x + 70.0  -- 70px to the right (past the 40px half-width + 30px gap)
      badgeY = parent.y + toNumber i * 25.0  -- Stack vertically, 25px apart
    in
      { node: badge { x = badgeX, y = badgeY }, index: i }
  ) badges

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

-- | Find the maximum ID in a tree (for setting nextId after import)
findMaxId :: Tree TreeNode -> Int
findMaxId t =
  let
    val = head t
    children = tail t
    childMaxes = map findMaxId (Array.fromFoldable children)
  in
    Array.foldl max val.id childMaxes

-- | Check if a node's parent is a Join type (making it a "template" node)
isJoinChild :: Int -> Tree TreeNode -> Boolean
isJoinChild nodeId t = case findParentId nodeId t of
  Nothing -> false
  Just parentId -> case findNodeById parentId t of
    Nothing -> false
    Just parent -> isJoinType parent.nodeType

-- | Check if a node's parent is a Scene Join type (GUP - has enter/update/exit)
isUpdateJoinChild :: Int -> Tree TreeNode -> Boolean
isUpdateJoinChild nodeId t = case findParentId nodeId t of
  Nothing -> false
  Just parentId -> case findNodeById parentId t of
    Nothing -> false
    Just parent -> isUpdateJoinType parent.nodeType

-- | Check if a node type is a Join variant
isJoinType :: DslNodeType -> Boolean
isJoinType NodeJoin = true
isJoinType NodeNestedJoin = true
isJoinType NodeUpdateJoin = true
isJoinType NodeUpdateNestedJoin = true
isJoinType _ = false

-- | Check if a node type is a Scene Join (GUP) variant
isUpdateJoinType :: DslNodeType -> Boolean
isUpdateJoinType NodeUpdateJoin = true
isUpdateJoinType NodeUpdateNestedJoin = true
isUpdateJoinType _ = false

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
    -- Only create links to structural children (not badges)
    structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
    childLinks = Array.fromFoldable structuralChildren >>= \child ->
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
    grandchildLinks = Array.fromFoldable structuralChildren >>= makeLinks
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
render state =
  HH.div
    [ HP.class_ (HH.ClassName "tree-builder3-container")
    , HP.tabIndex 0
    , HP.ref containerRef
    , HE.onKeyDown HandleKeyDown
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-header") ]
        [ HH.h2_ [ HH.text "DSL Grammar Tree Builder" ]
        , HH.button
            [ HP.class_ (HH.ClassName "import-example-btn")
            , HE.onClick \_ -> ImportExample
            ]
            [ HH.text "Import Example AST" ]
        ]
    , HH.p
        [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.text "Click to select, arrows to navigate. Key hints shown next to selected node." ]
    , HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-main-layout") ]
        [ -- Tree visualization panel
          HH.div
            [ HP.class_ (HH.ClassName "tree-builder3-svg-container")
            , HP.id "tree-builder3-container"
            ]
            []
        , -- Code generation panel
          HH.div
            [ HP.class_ (HH.ClassName "tree-builder3-code-panel") ]
            [ HH.h3_ [ HH.text "Generated PSD3 Code" ]
            , HH.pre
                [ HP.class_ (HH.ClassName "code-output") ]
                [ HH.code_ [ HH.text state.generatedCode ] ]
            , -- Interactive form (if AST conversion succeeds)
              case builderTreeToAST state.userTree of
                Left _ -> HH.text ""  -- Don't show form for incomplete trees
                Right ast ->
                  HH.div
                    [ HP.class_ (HH.ClassName "ast-form-container") ]
                    [ HH.h3_ [ HH.text "Interactive Editor (names editable)" ]
                    , map HandleFormAction (astToForm ast)
                    ]
            ]
        ]
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

  ImportExample -> do
    -- Import sample AST and convert to TreeBuilder tree
    let importedTree = runImport sampleAST
    -- Find the max ID in the imported tree to set nextId correctly
    let maxId = findMaxId importedTree
    H.modify_ \s -> s
      { userTree = importedTree
      , selectedNodeId = Just 0  -- Select root
      , nextId = maxId + 1
      }
    handleAction RenderTree

  HandleFormAction formAction -> do
    state <- H.get
    -- Update tree based on form action
    let newTree = case formAction of
          NameChanged path value -> updateNameAtPath path value state.userTree
          KeyChanged path value -> updateKeyAtPath path value state.userTree
          StaticValueChanged _ _ -> state.userTree  -- TODO: implement static value updates
    H.modify_ \s -> s { userTree = newTree }
    handleAction RenderTree

  RenderTree -> do
    state <- H.get
    -- Update generated code
    let newCode = generateCode state.userTree
    H.modify_ \s -> s { generatedCode = newCode }
    -- Render tree visualization
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
handleKeyForNodeType NodeUpdateJoin keyName = handleJoinKey keyName
handleKeyForNodeType NodeUpdateNestedJoin keyName = handleJoinKey keyName
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
  "s" -> addNodeOfType NodeUpdateJoin
  "x" -> addNodeOfType NodeUpdateNestedJoin
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
    let newChild = { id: newId, nodeType, name: Nothing, key: Nothing, x: 0.0, y: 0.0, depth: 0 }
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
        let newChild = { id: newId, nodeType, name: Nothing, key: Nothing, x: 0.0, y: 0.0, depth: 0 }
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

-- | Update name at a FormPath
-- | Path structure: ["name"] for root, ["children", "0", "name"] for first child, etc.
-- | "template" navigates to the first child of a join
updateNameAtPath :: Array String -> String -> Tree TreeNode -> Tree TreeNode
updateNameAtPath path newName t = case Array.uncons path of
  Nothing -> t  -- Empty path, no change
  Just { head: segment, tail: rest } ->
    case segment of
      "name" ->
        -- Update this node's name
        let val = head t
            newVal = val { name = Just newName }
        in mkTree newVal (tail t)
      "children" ->
        -- Navigate to children, next segment is index
        case Array.uncons rest of
          Just { head: idxStr, tail: restAfterIdx } ->
            case Int.fromString idxStr of
              Just idx ->
                let val = head t
                    children = tail t
                    -- Only count structural children (skip attrs/behaviors)
                    structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
                    structArray = Array.fromFoldable structuralChildren
                in case Array.index structArray idx of
                  Just targetChild ->
                    let updatedChild = updateNameAtPath restAfterIdx newName targetChild
                        -- Replace the child in the original list
                        newChildren = map (\c -> if (head c).id == (head targetChild).id then updatedChild else c) children
                    in mkTree val newChildren
                  Nothing -> t  -- Index out of bounds
              Nothing -> t  -- Invalid index
          Nothing -> t  -- Missing index
      "template" ->
        -- Template refers to first structural child (for joins)
        let val = head t
            children = tail t
            structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
        in case Data.List.head structuralChildren of
          Just firstChild ->
            let updatedChild = updateNameAtPath rest newName firstChild
                newChildren = map (\c -> if (head c).id == (head firstChild).id then updatedChild else c) children
            in mkTree val newChildren
          Nothing -> t
      _ -> t  -- Unknown segment

-- | Update key at a FormPath (similar to updateNameAtPath)
updateKeyAtPath :: Array String -> String -> Tree TreeNode -> Tree TreeNode
updateKeyAtPath path newKey t = case Array.uncons path of
  Nothing -> t
  Just { head: segment, tail: rest } ->
    case segment of
      "key" ->
        let val = head t
            newVal = val { key = Just newKey }
        in mkTree newVal (tail t)
      "children" ->
        case Array.uncons rest of
          Just { head: idxStr, tail: restAfterIdx } ->
            case Int.fromString idxStr of
              Just idx ->
                let val = head t
                    children = tail t
                    structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
                    structArray = Array.fromFoldable structuralChildren
                in case Array.index structArray idx of
                  Just targetChild ->
                    let updatedChild = updateKeyAtPath restAfterIdx newKey targetChild
                        newChildren = map (\c -> if (head c).id == (head targetChild).id then updatedChild else c) children
                    in mkTree val newChildren
                  Nothing -> t
              Nothing -> t
          Nothing -> t
      "template" ->
        let val = head t
            children = tail t
            structuralChildren = Data.List.filter (\c -> not (isBadgeNodeType (head c).nodeType)) children
        in case Data.List.head structuralChildren of
          Just firstChild ->
            let updatedChild = updateKeyAtPath rest newKey firstChild
                newChildren = map (\c -> if (head c).id == (head firstChild).id then updatedChild else c) children
            in mkTree val newChildren
          Nothing -> t
      _ -> t

-- | Update the selected node's type (for resolving pending nodes)
-- | If resolving an Element under a UpdateJoin, auto-create Enter/Update/Exit children
resolveSelectedNode :: forall output m. MonadAff m => DslNodeType -> H.HalogenM State Action () output m Unit
resolveSelectedNode newType = do
  state <- H.get
  for_ state.selectedNodeId \selectedId -> do
    -- First, update the node type
    let treeWithType = updateNodeType selectedId newType state.userTree

    -- Check if this is an Element being resolved under a UpdateJoin
    let isElementUnderScene = case newType of
          NodeElem _ -> isUpdateJoinChild selectedId state.userTree
          _ -> false

    -- If so, auto-create Enter/Update/Exit children
    if isElementUnderScene then do
      let enterId = state.nextId
      let updateId = state.nextId + 1
      let exitId = state.nextId + 2
      let enterNode = { id: enterId, nodeType: NodeEnter, name: Nothing, key: Nothing, x: 0.0, y: 0.0, depth: 0 }
      let updateNode = { id: updateId, nodeType: NodeUpdate, name: Nothing, key: Nothing, x: 0.0, y: 0.0, depth: 0 }
      let exitNode = { id: exitId, nodeType: NodeExit, name: Nothing, key: Nothing, x: 0.0, y: 0.0, depth: 0 }
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

  -- Apply layout to structural-only tree (excludes badges)
  let structuralTree = filterStructuralTree state.userTree
  let positioned = applyLayout structuralTree
  let structuralNodes = flattenTree positioned
  let links = makeLinks positioned

  -- Collect badges for each structural node and position them
  let badgeNodes = structuralNodes >>= \parent ->
        let badges = getBadgeChildren parent.id state.userTree
            positioned' = positionBadges parent badges
        in map (\b -> { node: b.node, index: b.index, parentId: parent.id }) positioned'

  -- Convert structural nodes to render nodes
  let structuralRenderNodes = map (toRenderNode state false 0) structuralNodes

  -- Convert badge nodes to render nodes
  let badgeRenderNodes = map (\b -> toRenderNode state true b.index b.node) badgeNodes

  let renderNodes = structuralRenderNodes <> badgeRenderNodes

  let svgWidth = 900.0
  let svgHeight = 600.0

  -- Center the tree horizontally in the SVG (use structural nodes for centering)
  -- For a single node, minX == maxX, so we center on that point
  let
    firstX = case Array.head structuralNodes of
      Just n -> n.x
      Nothing -> 0.0
  let minX = Array.foldl (\acc n -> min acc n.x) firstX structuralNodes
  let maxX = Array.foldl (\acc n -> max acc n.x) firstX structuralNodes
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

    -- Badge rendering helper - smaller pill-shaped nodes
    let
      badgeRect :: RenderNode -> T.Tree RenderNode
      badgeRect node =
        T.elem Rect
          [ evalAttr "x" (lit (-28.0))
          , evalAttr "y" (lit (-10.0))
          , evalAttr "width" (lit 56.0)
          , evalAttr "height" (lit 20.0)
          , evalAttr "rx" (lit 10.0) -- Pill shape
          , evalAttrStr "fill" (str node.color)
          , evalAttrStr "stroke" (str "#333")
          , evalAttr "stroke-width" (lit node.strokeWidth)
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
                    ( if node.isBadge then
                        -- Badge rendering: smaller pill + smaller label
                        [ badgeRect node
                        , T.elem Text
                            [ evalAttr "x" (lit 0.0)
                            , evalAttr "y" (lit 3.0)
                            , evalAttrStr "text-anchor" (str "middle")
                            , evalAttrStr "fill" (str "white")
                            , evalAttrStr "font-size" (str "9px")
                            , evalAttrStr "font-weight" (str "bold")
                            , Friendly.textContent (str node.label)
                            ]
                        , -- Key hints for badges (shown to right)
                          T.elem Text
                            [ evalAttr "x" (lit 40.0) -- Further right to avoid overlap
                            , evalAttr "y" (lit 3.0)
                            , evalAttrStr "text-anchor" (str "start")
                            , evalAttrStr "fill" (str "#333") -- Darker for better visibility
                            , evalAttrStr "font-size" (str "11px") -- Larger for readability
                            , evalAttrStr "font-family" (str "monospace")
                            , evalAttrStr "font-weight" (str "bold") -- Bold for emphasis
                            , evalAttrStr "opacity" (str (if node.isSelected then "1" else "0"))
                            , Friendly.textContent (str node.keyHints)
                            ]
                        ]
                      else
                        -- Regular node rendering
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
                              , -- Name label (shown to left of node)
                                T.elem Text
                                  [ evalAttr "x" (lit (-50.0)) -- Left of the node rect
                                  , evalAttr "y" (lit 4.0)
                                  , evalAttrStr "text-anchor" (str "end")
                                  , evalAttrStr "fill" (str "#4a90e2") -- Blue color for names
                                  , evalAttrStr "font-size" (str "10px")
                                  , evalAttrStr "font-family" (str "monospace")
                                  , Friendly.textContent (str (formatNameLabel node.nameLabel node.keyLabel))
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
            )

    _ <- renderTree zoomGroupSel nodesTree

    pure unit

toRenderNode :: State -> Boolean -> Int -> TreeNode -> RenderNode
toRenderNode state isBadge badgeIndex node =
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
    , nameLabel: node.name  -- Name from imported AST (element/join name)
    , keyLabel: node.key    -- Key from imported AST (join key)
    , keyHints: computeKeyHints state.userTree node
    , isSelected: selected
    , showAsStack: shouldStack
    , isBadge
    , badgeIndex
    }

-- | Format name label for display on LHS
-- | Shows "name" for named elements, "name:key" for joins
formatNameLabel :: Maybe String -> Maybe String -> String
formatNameLabel maybeName maybeKey = case maybeName, maybeKey of
  Just name, Just key -> "\"" <> name <> ":" <> key <> "\""
  Just name, Nothing -> "\"" <> name <> "\""
  Nothing, Just key -> "\"" <> key <> "\""
  Nothing, Nothing -> ""

-- | Compute key hints dynamically based on tree state
-- | Accounts for constraints like "Join can only have one template"
computeKeyHints :: Tree TreeNode -> TreeNode -> String
computeKeyHints tree node = case node.nodeType of
  -- Join types: show [e] only if no template yet, otherwise []
  NodeJoin -> joinHints node.id
  NodeNestedJoin -> joinHints node.id
  NodeUpdateJoin -> joinHints node.id
  NodeUpdateNestedJoin -> joinHints node.id
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

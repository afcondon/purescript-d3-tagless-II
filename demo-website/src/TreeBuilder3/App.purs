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

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tree (Tree, mkTree)
import DataViz.Layout.Hierarchy.Link (linkBezierVertical)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PSD3.Internal.Behavior.FFI (attachZoomWithTransform_, getZoomTransform_)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..))
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (SEmpty, ElementType(..))
import PSD3.Expr.Friendly as F
import PSD3.AST as T
import PSD3.Transform (clearContainer)
import PSD3.Interpreter.SemiQuine.TreeToCode (treeToCode)
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..), BehaviorKind(..), nodeLabel, nodeKeyHints)
import TreeBuilder3.Theme as Theme
import TreeBuilder3.TreeOps
  ( LinkData
  , findNodeById
  , findParentId
  , getChildrenIds
  , findMaxId
  , isJoinChild
  , isUpdateJoinChild
  , addChildToNode
  , removeNodeById
  , updateNodeType
  , filterStructuralTree
  , updateNameAtPath
  , updateKeyAtPath
  , getBadgeChildren
  , positionBadges
  , flattenTree
  , makeLinks
  , applyLayout
  )
import Color (toHexString)
import TreeBuilder3.Converter (builderTreeToAST)
import TreeBuilder3.ASTImporter (runImport)
import TreeBuilder3.FormInterpreter (astToForm, FormAction(..))
import PSD3.AST as AST
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

-- | Zoom transform for preserving zoom state across re-renders
type ZoomTransform = { k :: Number, x :: Number, y :: Number }

-- | Component state
type State =
  { userTree :: Tree TreeNode
  , selectedNodeId :: Maybe Int
  , nextId :: Int
  , clickListener :: Maybe (HS.Listener Action)
  , generatedCode :: String -- Live generated PSD3 code
  , zoomTransform :: ZoomTransform -- Current zoom state (preserved across renders)
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
  , zoomTransform: { k: 1.0, x: 0.0, y: 0.0 } -- Identity transform
  }

-- | Sample AST for testing import functionality
-- | A simple scatter plot structure: SVG > Group > Join(Circle[cx,cy,r,fill])
sampleAST :: AST.Tree Unit
sampleAST =
  AST.named AST.SVG "svg"
    [ F.fill $ F.text "none" ]
    `AST.withChild`
      ( AST.named AST.Group "zoomGroup" []
          `AST.withChild`
            ( AST.joinData "nodes" "circle" [ unit ] $ \_ ->
                AST.elem AST.Circle
                  [ F.cx $ F.num 100.0
                  , F.cy $ F.num 100.0
                  , F.r $ F.num 5.0
                  , F.fill $ F.hex "#4a90e2"
                  , F.stroke $ F.hex "#333333"
                  , F.strokeWidth $ F.num 1.0
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
        [ HP.class_ (HH.ClassName "tree-builder3-editors-row") ]
        [ -- Tree visualization panel (left)
          HH.div
            [ HP.class_ (HH.ClassName "tree-builder3-svg-container")
            , HP.id "tree-builder3-container"
            ]
            []
        , -- Interactive form editor (right, side by side with tree)
          case builderTreeToAST state.userTree of
            Left _ -> HH.text "" -- Don't show form for incomplete trees
            Right ast ->
              HH.div
                [ HP.class_ (HH.ClassName "tree-builder3-form-panel") ]
                [ HH.h3_ [ HH.text "Interactive Editor" ]
                , map HandleFormAction (astToForm ast)
                ]
        ]
    , -- Code generation panel (below)
      HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-code-panel") ]
        [ HH.h3_ [ HH.text "Generated PSD3 Code" ]
        , HH.pre
            [ HP.class_ (HH.ClassName "code-output") ]
            [ HH.code_ [ HH.text state.generatedCode ] ]
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
    -- Reset zoom to identity when loading new AST
    H.modify_ \s -> s
      { userTree = importedTree
      , selectedNodeId = Just 0 -- Select root
      , nextId = maxId + 1
      , zoomTransform = { k: 1.0, x: 0.0, y: 0.0 } -- Reset zoom
      }
    handleAction RenderTree

  HandleFormAction formAction -> do
    state <- H.get
    -- Update tree based on form action
    let
      newTree = case formAction of
        NameChanged path value -> updateNameAtPath path value state.userTree
        KeyChanged path value -> updateKeyAtPath path value state.userTree
        StaticValueChanged _ _ -> state.userTree -- TODO: implement static value updates
    H.modify_ \s -> s { userTree = newTree }
    handleAction RenderTree

  RenderTree -> do
    state <- H.get
    -- Save current zoom transform before clearing (if SVG exists)
    savedTransform <- liftEffect $ saveZoomTransform
    -- Store in state for use by setupZoom
    let transform = case savedTransform of
          Just t -> t
          Nothing -> state.zoomTransform
    H.modify_ \s -> s { zoomTransform = transform }
    -- Update generated code
    let newCode = generateCode state.userTree
    H.modify_ \s -> s { generatedCode = newCode }
    -- Render tree visualization
    state' <- H.get -- Get updated state with zoom transform
    case state'.clickListener of
      Just listener -> do
        liftEffect $ renderTreeViz state' listener
        liftEffect $ setupZoomWithTransform state'.zoomTransform
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

-- | Update the selected node's type (for resolving pending nodes)
-- | If resolving an Element under a UpdateJoin, auto-create Enter/Update/Exit children
resolveSelectedNode :: forall output m. MonadAff m => DslNodeType -> H.HalogenM State Action () output m Unit
resolveSelectedNode newType = do
  state <- H.get
  for_ state.selectedNodeId \selectedId -> do
    -- First, update the node type
    let treeWithType = updateNodeType selectedId newType state.userTree

    -- Check if this is an Element being resolved under a UpdateJoin
    let
      isElementUnderScene = case newType of
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


renderTreeViz :: State -> HS.Listener Action -> Effect Unit
renderTreeViz state listener = do
  clearContainer "#tree-builder3-container"

  -- Apply layout to structural-only tree (excludes badges)
  let structuralTree = filterStructuralTree state.userTree
  let positioned = applyLayout structuralTree
  let structuralNodes = flattenTree positioned
  let links = makeLinks positioned

  -- Collect badges for each structural node and position them
  let
    badgeNodes = structuralNodes >>= \parent ->
      let
        badges = getBadgeChildren parent.id state.userTree
        positioned' = positionBadges parent badges
      in
        map (\b -> { node: b.node, index: b.index, parentId: parent.id }) positioned'

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
          [ F.width (F.num svgWidth)
          , F.height (F.num svgHeight)
          , F.viewBox 0.0 0.0 svgWidth svgHeight
          , F.staticStr "id" "tree-builder3-svg"
          ]
          `T.withChild`
            ( T.named Group "zoomGroup"
                [ F.staticStr "class" "zoom-group" ]
                `T.withChild`
                  ( T.named Group "linksGroup"
                      [ F.staticStr "class" "links" ]
                      `T.withChild`
                        ( T.joinData "linkPaths" "path" links $ \link ->
                            T.elem Path
                              [ F.path $ F.text $ linkBezierVertical
                                  (link.sourceX + offsetX)
                                  (link.sourceY + offsetY)
                                  (link.targetX + offsetX)
                                  (link.targetY + offsetY)
                              , F.fill (F.text "none")
                              , F.stroke (F.color Theme.linkColor)
                              , F.strokeWidth (F.num Theme.strokeWidthLink)
                              ]
                        )
                  )
            )

    linksSelections <- renderTree container linksTree
    zoomGroupSel <- liftEffect $ reselectD3v2 "zoomGroup" linksSelections

    -- Helper to create a stacked "punch card" rect at a given offset
    let
      stackedRect :: Number -> Number -> String -> T.Tree RenderNode
      stackedRect dx' dy' color =
        T.elem Rect
          [ F.x (F.num (-40.0 + dx'))
          , F.y (F.num (-12.0 + dy'))
          , F.width (F.num 80.0)
          , F.height (F.num 24.0)
          , F.static "rx" 4.0
          , F.fill (F.text color)
          , F.stroke (F.color Theme.nodeStroke)
          , F.strokeWidth (F.num 1.5)
          ]

    -- Badge rendering helper - smaller pill-shaped nodes
    let
      badgeRect :: RenderNode -> T.Tree RenderNode
      badgeRect node =
        T.elem Rect
          [ F.x (F.num (-28.0))
          , F.y (F.num (-10.0))
          , F.width (F.num 56.0)
          , F.height (F.num 20.0)
          , F.static "rx" 10.0 -- Pill shape
          , F.fill (F.text node.color)
          , F.stroke (F.color Theme.nodeStroke)
          , F.strokeWidth (F.num node.strokeWidth)
          ]

    let
      nodesTree :: T.Tree RenderNode
      nodesTree =
        T.named Group "nodesGroup"
          [ F.staticStr "class" "nodes" ]
          `T.withChild`
            ( T.joinData "nodeGroups" "g" renderNodes $ \node ->
                T.elem Group
                  [ F.transform $ F.text $ "translate(" <> show (node.x + offsetX) <> "," <> show (node.y + offsetY) <> ")"
                  , F.staticStr "cursor" "pointer"
                  ]
                  `T.withBehaviors`
                    [ ClickWithDatum \n -> HS.notify listener (NodeClicked n.id) ]
                  `T.withChildren`
                    ( if node.isBadge then
                        -- Badge rendering: smaller pill + smaller label
                        [ badgeRect node
                        , T.elem Text
                            [ F.x (F.num 0.0)
                            , F.y (F.num 3.0)
                            , F.textAnchor (F.text "middle")
                            , F.fill (F.color Theme.nodeLabelDark)
                            , F.fontSize (F.px 9.0)
                            , F.staticStr "font-weight" "bold"
                            , F.textContent (F.text node.label)
                            ]
                        , -- Key hints for badges (shown to right)
                          T.elem Text
                            [ F.x (F.num 40.0) -- Further right to avoid overlap
                            , F.y (F.num 3.0)
                            , F.textAnchor (F.text "start")
                            , F.fill (F.color Theme.keyHintsColor) -- Darker for better visibility
                            , F.fontSize (F.px 11.0) -- Larger for readability
                            , F.fontFamily (F.text "monospace")
                            , F.staticStr "font-weight" "bold" -- Bold for emphasis
                            , F.opacity (F.text (if node.isSelected then "1" else "0"))
                            , F.textContent (F.text node.keyHints)
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
                                  [ F.x (F.num (-40.0))
                                  , F.y (F.num (-12.0))
                                  , F.width (F.num 80.0)
                                  , F.height (F.num 24.0)
                                  , F.static "rx" 4.0
                                  , F.fill (F.text node.color)
                                  , F.stroke (F.color Theme.nodeStroke)
                                  , F.strokeWidth (F.num node.strokeWidth)
                                  ]
                              , -- Label text
                                T.elem Text
                                  [ F.x (F.num 0.0)
                                  , F.y (F.num 4.0)
                                  , F.textAnchor (F.text "middle")
                                  , F.fill (F.color Theme.nodeLabelLight)
                                  , F.fontSize (F.px 11.0)
                                  , F.staticStr "font-weight" "bold"
                                  , F.textContent (F.text node.label)
                                  ]
                              , -- Name label (shown to left of node)
                                T.elem Text
                                  [ F.x (F.num (-50.0)) -- Left of the node rect
                                  , F.y (F.num 4.0)
                                  , F.textAnchor (F.text "end")
                                  , F.fill (F.color Theme.nameLabelColor) -- Blue color for names
                                  , F.fontSize (F.px 10.0)
                                  , F.fontFamily (F.text "monospace")
                                  , F.textContent (F.text (formatNameLabel node.nameLabel node.keyLabel))
                                  ]
                              , -- Key hints (shown to right of selected node)
                                T.elem Text
                                  [ F.x (F.num 50.0) -- Right of the node rect
                                  , F.y (F.num 4.0)
                                  , F.textAnchor (F.text "start")
                                  , F.fill (F.color Theme.keyHintsColor)
                                  , F.fontSize (F.px 10.0)
                                  , F.fontFamily (F.text "monospace")
                                  , F.opacity (F.text (if node.isSelected then "1" else "0"))
                                  , F.textContent (F.text node.keyHints)
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
    , color: toHexString $ Theme.nodeTypeColor node.nodeType
    , strokeWidth: if selected then Theme.strokeWidthSelected else Theme.strokeWidthNormal
    , label: nodeLabel node.nodeType
    , nameLabel: node.name -- Name from imported AST (element/join name)
    , keyLabel: node.key -- Key from imported AST (join key)
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

-- | Save the current zoom transform from the existing SVG (if any)
saveZoomTransform :: Effect (Maybe ZoomTransform)
saveZoomTransform = do
  doc <- window >>= document
  let parentNode = Document.toParentNode (toDocument doc)
  maybeSvg <- querySelector (QuerySelector "#tree-builder3-svg") parentNode
  case maybeSvg of
    Just svg -> do
      t <- getZoomTransform_ svg
      pure (Just t)
    Nothing -> pure Nothing

-- | Setup zoom with a given transform (to restore previous zoom state)
setupZoomWithTransform :: ZoomTransform -> Effect Unit
setupZoomWithTransform transform = do
  doc <- window >>= document
  let parentNode = Document.toParentNode (toDocument doc)
  maybeSvg <- querySelector (QuerySelector "#tree-builder3-svg") parentNode
  for_ maybeSvg \svg -> do
    let ScaleExtent minScale maxScale = ScaleExtent 0.5 4.0
    _ <- attachZoomWithTransform_ svg minScale maxScale ".zoom-group" transform
    pure unit

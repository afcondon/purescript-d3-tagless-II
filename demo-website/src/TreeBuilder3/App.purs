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
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tree (Tree, mkTree)
import Control.Comonad.Cofree (head, tail) as Cofree
import DataViz.Layout.Hierarchy.Link (linkBezierVertical)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PSD3.Internal.Behavior.FFI (attachZoomWithCallback_, getZoomTransform_, updateAttr_)
import PSD3.Internal.Behavior.Types (Behavior(..), ScaleExtent(..))
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (SEmpty, ElementType(..))
import PSD3.Expr.Friendly as F
import PSD3.AST as T
import PSD3.Transform (clearContainer)
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..), BehaviorKind(..), DatumType(..), nodeLabel, nodeKeyHints, datumTypeLabel)
import TreeBuilder3.TypePropagation (propagateTypes, pointType, nodeType, linkType, countryType, cellType, rowType, boardType, letterType)
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
  , updateNodeDatumType
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
import TreeBuilder3.ASTImporter (runImport)
import TreeBuilder3.FormInterpreter (FormAction(..))
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
  , keyHints :: String -- Valid key hints to show (e.g., "[e,j,n,u,x,a,b]")
  , typeLabel :: String -- Datum type annotation (e.g., "Unit", "Point", "?")
  , isSelected :: Boolean -- Whether this node is currently selected
  , showAsStack :: Boolean -- Whether to draw as deck-of-cards (join templates + GUP phases)
  , isBadge :: Boolean -- Whether to render as a small badge (attrs, behaviors)
  , badgeIndex :: Int -- Position in badge row (0, 1, 2, ...)
  }

-- | Type card data for rendering in SVG
type TypeCard =
  { name :: String -- Type name (e.g., "Point")
  , fields :: Array { name :: String, typ :: String } -- Fields to display
  , datumType :: DatumType -- The actual type for assignment
  , y :: Number -- Y position
  , isEnabled :: Boolean -- Can be clicked (Join is selected)
  , isAssigned :: Boolean -- This type is assigned to selected Join
  }

-- | Sample data identifier
data SampleDataId
  = SampleChess
  | SampleSudoku
  | SampleGo
  | SampleScatterPlot
  | SampleForceGraph
  | SampleGapminder
  | SampleGupLetters

derive instance eqSampleDataId :: Eq SampleDataId
derive instance ordSampleDataId :: Ord SampleDataId

-- | AST preset identifiers (TOP cards)
data AstPreset
  = AstEmpty       -- Empty tree for building from scratch
  | AstGrid        -- Grid layout: SVG > NestedJoin(Row) > NestedJoin(Cell) > Rect
  | AstScatter     -- Scatter plot: SVG > Join > Circle
  | AstBubble      -- Bubble chart: SVG > Join > Circle (with radius encoding)
  | AstTree        -- Tree layout: SVG > Join > Group > Path, Circle, Text
  | AstUpdateJoin  -- GUP demo: SVG > UpdateJoin > Text (enter/update/exit)

derive instance eqAstPreset :: Eq AstPreset
derive instance ordAstPreset :: Ord AstPreset

-- | Get label for AST preset
astPresetLabel :: AstPreset -> String
astPresetLabel = case _ of
  AstEmpty -> "Try Me"
  AstGrid -> "Grid"
  AstScatter -> "Scatter"
  AstBubble -> "Bubble"
  AstTree -> "Tree"
  AstUpdateJoin -> "GUP"

-- | Get description for AST preset
astPresetDescription :: AstPreset -> String
astPresetDescription = case _ of
  AstEmpty -> "Build your own!"
  AstGrid -> "Row × Column"
  AstScatter -> "X × Y points"
  AstBubble -> "Sized circles"
  AstTree -> "Hierarchical"
  AstUpdateJoin -> "Enter/Update/Exit"

-- | All available AST presets
allAstPresets :: Array AstPreset
allAstPresets = [ AstEmpty, AstGrid, AstScatter, AstBubble, AstTree, AstUpdateJoin ]

-- | Get compatible DatumTypes for an AST preset
compatibleTypesForAst :: AstPreset -> Array DatumType
compatibleTypesForAst = case _ of
  AstEmpty -> [ pointType, nodeType, countryType, boardType, letterType ] -- Try Me: all types available
  AstGrid -> [ boardType ]
  AstScatter -> [ pointType ]
  AstBubble -> [ countryType ]
  AstTree -> [ nodeType ]
  AstUpdateJoin -> [ letterType ] -- GUP demo with letters

-- | Get compatible SampleDataIds for a DatumType
compatibleDatasetsForType :: DatumType -> Array SampleDataId
compatibleDatasetsForType t
  | t == boardType = [ SampleChess, SampleSudoku, SampleGo ]
  | t == countryType = [ SampleGapminder ]
  | t == nodeType = [ SampleForceGraph ]
  | t == pointType = [ SampleScatterPlot ]
  | t == letterType = [ SampleGupLetters ]
  | otherwise = []

-- | Check if a type is compatible with the current AST
isTypeCompatibleWithAst :: AstPreset -> DatumType -> Boolean
isTypeCompatibleWithAst ast typ = Array.elem typ (compatibleTypesForAst ast)

-- | Check if a dataset is compatible with the current type
isDatasetCompatibleWithType :: Maybe DatumType -> SampleDataId -> Boolean
isDatasetCompatibleWithType Nothing _ = false
isDatasetCompatibleWithType (Just typ) dataset = Array.elem dataset (compatibleDatasetsForType typ)

-- | AST card for rendering preset options in SVG
type AstCard =
  { name :: String
  , description :: String
  , preset :: AstPreset
  , x :: Number
  , isSelected :: Boolean
  }

-- | Data card for rendering sample data options in SVG
type DataCard =
  { name :: String -- Display name (e.g., "Chess")
  , description :: String -- Brief description
  , sampleId :: SampleDataId -- ID for selection
  , y :: Number -- Y position
  , isEnabled :: Boolean -- Can be clicked (type is assigned)
  , isSelected :: Boolean -- This sample is currently selected
  }

-- | Zoom transform for preserving zoom state across re-renders
type ZoomTransform = { k :: Number, x :: Number, y :: Number }

-- | Component state
type State =
  { userTree :: Tree TreeNode
  , selectedNodeId :: Maybe Int
  , nextId :: Int
  , clickListener :: Maybe (HS.Listener Action)
  , zoomTransform :: ZoomTransform -- Current zoom state (preserved across renders)
  , selectedSampleData :: Maybe SampleDataId -- Selected sample data for visualization
  , selectedAstPreset :: AstPreset -- Currently selected AST preset
  , showTryMeInstructions :: Boolean -- Show the TryMe mode instructions popup
  }

-- | Actions
data Action
  = Initialize
  | NodeClicked Int
  | HandleKeyDown KeyboardEvent
  | RenderTree
  | ImportExample
  | HandleFormAction FormAction
  | AssignType DatumType -- Assign a type to the selected Join node
  | ZoomChanged ZoomTransform -- Zoom/pan changed, update arrow
  | SelectSampleData SampleDataId -- Select sample data to visualize
  | SelectAstPreset AstPreset -- Select an AST preset from top cards
  | DismissTryMeInstructions -- Dismiss the TryMe instructions popup

-- =============================================================================
-- Initial State & Preset Trees
-- =============================================================================

-- | Get the tree for an AST preset
presetTree :: AstPreset -> Tree TreeNode
presetTree = case _ of
  AstEmpty -> emptyTree
  AstGrid -> gridTree
  AstScatter -> scatterTree
  AstBubble -> bubbleTree
  AstTree -> hierarchyTree
  AstUpdateJoin -> gupTree

-- | Get the next ID for a preset tree (for adding new nodes)
presetNextId :: AstPreset -> Int
presetNextId = case _ of
  AstEmpty -> 1
  AstGrid -> 5
  AstScatter -> 3
  AstBubble -> 3
  AstTree -> 5
  AstUpdateJoin -> 6

-- | Empty tree: just SVG root
emptyTree :: Tree TreeNode
emptyTree = mkTree
  { id: 0
  , nodeType: NodeElem SVG
  , name: Nothing
  , key: Nothing
  , datumType: TypeUnit
  , x: 0.0
  , y: 0.0
  , depth: 0
  }
  Nil

-- | Grid tree: SVG > NestedJoin(rows) > NestedJoin(cells) > Rect
-- | For board games like Chess, Go, Sudoku
gridTree :: Tree TreeNode
gridTree = mkTree
  { id: 0, nodeType: NodeElem SVG, name: Just "svg", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
  ( Cons
      ( mkTree
          { id: 1, nodeType: NodeNestedJoin, name: Just "rows", key: Just "row", datumType: rowType, x: 0.0, y: 0.0, depth: 1 }
          ( Cons
              ( mkTree
                  { id: 2, nodeType: NodeNestedJoin, name: Just "cells", key: Just "col", datumType: cellType, x: 0.0, y: 0.0, depth: 2 }
                  ( Cons
                      ( mkTree
                          { id: 3, nodeType: NodeElem Group, name: Just "cell", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 3 }
                          ( Cons
                              ( mkTree
                                  { id: 4, nodeType: NodeElem Rect, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 4 }
                                  Nil
                              )
                              Nil
                          )
                      )
                      Nil
                  )
              )
              Nil
          )
      )
      Nil
  )

-- | Scatter tree: SVG > Join > Circle
scatterTree :: Tree TreeNode
scatterTree = mkTree
  { id: 0, nodeType: NodeElem SVG, name: Just "svg", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
  ( Cons
      ( mkTree
          { id: 1, nodeType: NodeJoin, name: Just "points", key: Just "id", datumType: pointType, x: 0.0, y: 0.0, depth: 1 }
          ( Cons
              ( mkTree
                  { id: 2, nodeType: NodeElem Circle, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 2 }
                  Nil
              )
              Nil
          )
      )
      Nil
  )

-- | Bubble tree: SVG > Join > Circle (same structure, different type)
bubbleTree :: Tree TreeNode
bubbleTree = mkTree
  { id: 0, nodeType: NodeElem SVG, name: Just "svg", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
  ( Cons
      ( mkTree
          { id: 1, nodeType: NodeJoin, name: Just "bubbles", key: Just "name", datumType: countryType, x: 0.0, y: 0.0, depth: 1 }
          ( Cons
              ( mkTree
                  { id: 2, nodeType: NodeElem Circle, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 2 }
                  Nil
              )
              Nil
          )
      )
      Nil
  )

-- | Hierarchy tree: SVG > Join > Group > Path + Circle + Text
hierarchyTree :: Tree TreeNode
hierarchyTree = mkTree
  { id: 0, nodeType: NodeElem SVG, name: Just "svg", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
  ( Cons
      ( mkTree
          { id: 1, nodeType: NodeJoin, name: Just "nodes", key: Just "id", datumType: nodeType, x: 0.0, y: 0.0, depth: 1 }
          ( Cons
              ( mkTree
                  { id: 2, nodeType: NodeElem Group, name: Just "node", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 2 }
                  ( Cons
                      ( mkTree
                          { id: 3, nodeType: NodeElem Circle, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 3 }
                          Nil
                      )
                      ( Cons
                          ( mkTree
                              { id: 4, nodeType: NodeElem Text, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 3 }
                              Nil
                          )
                          Nil
                      )
                  )
              )
              Nil
          )
      )
      Nil
  )

-- | GUP tree: SVG > UpdateJoin > Text (with Enter/Update/Exit phases)
gupTree :: Tree TreeNode
gupTree = mkTree
  { id: 0, nodeType: NodeElem SVG, name: Just "svg", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
  ( Cons
      ( mkTree
          { id: 1, nodeType: NodeUpdateJoin, name: Just "letters", key: Just "char", datumType: TypeUnit, x: 0.0, y: 0.0, depth: 1 }
          ( Cons
              ( mkTree
                  { id: 2, nodeType: NodeElem Text, name: Just "letter", key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 2 }
                  ( Cons
                      ( mkTree { id: 3, nodeType: NodeEnter, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 3 } Nil )
                      ( Cons
                          ( mkTree { id: 4, nodeType: NodeUpdate, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 3 } Nil )
                          ( Cons
                              ( mkTree { id: 5, nodeType: NodeExit, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 3 } Nil )
                              Nil
                          )
                      )
                  )
              )
              Nil
          )
      )
      Nil
  )

initialState :: State
initialState =
  { userTree: presetTree AstGrid
  , selectedNodeId: Just 1 -- Select the first Join node
  , nextId: presetNextId AstGrid
  , clickListener: Nothing
  , zoomTransform: { k: 1.0, x: 0.0, y: 0.0 } -- Identity transform
  , selectedSampleData: Just SampleChess -- Preload Chess data
  , selectedAstPreset: AstGrid -- Start with Grid preset
  , showTryMeInstructions: false -- Don't show TryMe popup initially
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
    , -- AST preset cards row (like VizMatrix top cards)
      HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-ast-cards") ]
        ( map (renderAstCard state) allAstPresets )
    , HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-editors-row") ]
        [ -- Tree visualization panel (with integrated type cards)
          HH.div
            [ HP.class_ (HH.ClassName "tree-builder3-svg-container")
            , HP.id "tree-builder3-container"
            ]
            []
        , -- Rendered output panel (visualization result)
          HH.div
            [ HP.class_ (HH.ClassName "tree-builder3-output-panel") ]
            [ HH.h3_ [ HH.text (if state.selectedAstPreset == AstEmpty then "English Description" else "Rendered Output") ]
            , renderOutput state
            ]
        ]
    , -- TryMe instructions popup overlay
      if state.showTryMeInstructions
        then renderTryMePopup
        else HH.text ""
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

  AssignType datumType -> do
    state <- H.get
    -- Only assign if a Join node is selected
    for_ state.selectedNodeId \selectedId -> do
      case findNodeById selectedId state.userTree of
        Just node | isJoinNodeType node.nodeType -> do
          let newTree = updateNodeDatumType selectedId datumType state.userTree
          -- Clear sample data selection when type changes
          H.modify_ \s -> s { userTree = newTree, selectedSampleData = Nothing }
          handleAction RenderTree
        _ -> pure unit
    where
    isJoinNodeType NodeJoin = true
    isJoinNodeType NodeNestedJoin = true
    isJoinNodeType NodeUpdateJoin = true
    isJoinNodeType NodeUpdateNestedJoin = true
    isJoinNodeType _ = false

  SelectSampleData sampleId -> do
    H.modify_ \s -> s { selectedSampleData = Just sampleId }
    handleAction RenderTree

  SelectAstPreset preset -> do
    -- Load the preset tree, reset selections, reset zoom
    let newTree = presetTree preset
    let newNextId = presetNextId preset
    H.modify_ \s -> s
      { userTree = newTree
      , selectedNodeId = if preset == AstEmpty then Just 0 else Just 1 -- Select first Join or root
      , nextId = newNextId
      , selectedAstPreset = preset
      , selectedSampleData = Nothing -- Clear sample data when changing AST
      , zoomTransform = { k: 1.0, x: 0.0, y: 0.0 } -- Reset zoom
      , showTryMeInstructions = preset == AstEmpty -- Show popup for TryMe mode
      }
    handleAction RenderTree

  DismissTryMeInstructions -> do
    H.modify_ \s -> s { showTryMeInstructions = false }

  ZoomChanged newTransform -> do
    -- Update state with new zoom transform
    H.modify_ \s -> s { zoomTransform = newTransform }
    -- Update arrow position imperatively (without re-rendering whole tree)
    state <- H.get
    liftEffect $ updateArrowPosition state

  RenderTree -> do
    state <- H.get
    -- Save current zoom transform before clearing (if SVG exists)
    savedTransform <- liftEffect $ saveZoomTransform
    -- Store in state for use by setupZoom
    let
      transform = case savedTransform of
        Just t -> t
        Nothing -> state.zoomTransform
    H.modify_ \s -> s { zoomTransform = transform }
    -- Propagate datum types through the tree
    let typedTree = propagateTypes state.userTree
    H.modify_ \s -> s { userTree = typedTree }
    -- Render tree visualization
    state' <- H.get -- Get updated state with zoom transform and types
    case state'.clickListener of
      Just listener -> do
        liftEffect $ renderTreeViz state' listener
        liftEffect $ setupZoomWithCallback state'.zoomTransform listener
        -- Render output visualization if ready
        liftEffect $ renderOutputViz state'
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
  "u" -> addNodeOfType NodeUpdateJoin
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
    let newChild = { id: newId, nodeType, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
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
        let newChild = { id: newId, nodeType, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
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
      let enterNode = { id: enterId, nodeType: NodeEnter, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
      let updateNode = { id: updateId, nodeType: NodeUpdate, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
      let exitNode = { id: exitId, nodeType: NodeExit, name: Nothing, key: Nothing, datumType: TypeUnit, x: 0.0, y: 0.0, depth: 0 }
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

  -- Check if selected node is a Join and what type it has
  let selectedJoinType = getSelectedJoinDatumType state

  -- Create type card data (all types shown, incompatible ones at 0.3 opacity)
  let typeCards = makeTypeCards state.selectedAstPreset selectedJoinType

  -- Create data card options (all datasets shown, incompatible ones at 0.3 opacity)
  let dataCards = makeDataCards selectedJoinType state.selectedSampleData

  -- Position for data cards (RHS of SVG)
  let dataCardsX = svgWidth - Theme.dataCardWidth - 10.0

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
                              , -- Type annotation (shown below node)
                                T.elem Text
                                  [ F.x (F.num 0.0)
                                  , F.y (F.num 24.0) -- Below the node rect
                                  , F.textAnchor (F.text "middle")
                                  , F.fill (F.color Theme.typeLabelColor)
                                  , F.fontSize (F.px 9.0)
                                  , F.fontFamily (F.text "monospace")
                                  , F.textContent (F.text (":: " <> node.typeLabel))
                                  ]
                              ]
                        )
                    )
            )

    _ <- renderTree zoomGroupSel nodesTree

    -- Render type cards in a fixed group (not affected by zoom)
    svgSel <- liftEffect $ reselectD3v2 "svg" linksSelections

    let
      typeCardsTree :: T.Tree TypeCard
      typeCardsTree =
        T.named Group "typeCardsGroup"
          [ F.staticStr "class" "type-cards"
          , F.transform $ F.text "translate(10, 30)" -- Fixed position, top-left
          ]
          `T.withChild`
            ( T.joinData "typeCardGroups" "g" typeCards $ \card ->
                T.elem Group
                  [ F.transform $ F.text $ "translate(0," <> show card.y <> ")"
                  , F.staticStr "cursor" (if card.isEnabled then "pointer" else "default")
                  , F.opacity (F.text (if card.isEnabled then "1" else "0.3"))
                  ]
                  `T.withBehaviors`
                    ( if card.isEnabled then [ ClickWithDatum \c -> HS.notify listener (AssignType c.datumType) ]
                      else []
                    )
                  `T.withChildren`
                    -- Header row (type name)
                    [ T.elem Rect
                        [ F.x (F.num 0.0)
                        , F.y (F.num 0.0)
                        , F.width (F.num Theme.typeCardWidth)
                        , F.height (F.num 22.0)
                        , F.static "rx" 4.0
                        , F.fill (F.color (if card.isAssigned then Theme.typeCardHighlight else Theme.typeCardHeader))
                        , F.stroke (F.color Theme.typeCardStroke)
                        , F.strokeWidth (F.num (if card.isAssigned then 3.0 else 1.5))
                        ]
                    , T.elem Text
                        [ F.x (F.num (Theme.typeCardWidth / 2.0))
                        , F.y (F.num 15.0)
                        , F.textAnchor (F.text "middle")
                        , F.fill (F.color (if card.isAssigned then Theme.nodeLabelDark else Theme.nodeLabelLight))
                        , F.fontSize (F.px 11.0)
                        , F.staticStr "font-weight" "bold"
                        , F.textContent (F.text card.name)
                        ]
                    -- Fields background
                    , T.elem Rect
                        [ F.x (F.num 0.0)
                        , F.y (F.num 22.0)
                        , F.width (F.num Theme.typeCardWidth)
                        , F.height (F.num (toNumber (Array.length card.fields) * Theme.typeCardFieldHeight + 4.0))
                        , F.static "rx" 0.0
                        , F.fill (F.color Theme.typeCardFill)
                        , F.stroke (F.color Theme.typeCardStroke)
                        , F.strokeWidth (F.num 1.0)
                        ]
                    -- Red X for incompatible (only shown when disabled)
                    , T.elem Text
                        [ F.x (F.num (Theme.typeCardWidth - 8.0))
                        , F.y (F.num 15.0)
                        , F.textAnchor (F.text "middle")
                        , F.fill (F.text (if card.isEnabled then "none" else "#E63946"))
                        , F.fontSize (F.px 14.0)
                        , F.staticStr "font-weight" "bold"
                        , F.textContent (F.text "✕")
                        ]
                    ]
            )

    _ <- renderTree svgSel typeCardsTree

    -- Now render field labels for each card (separate pass since nested joins aren't ideal here)
    -- We'll use a simple approach: render all field labels as a flat join
    let
      allFieldLabels = typeCards >>= \card ->
        Array.mapWithIndex
          ( \i field ->
              { text: field.name <> " :: " <> field.typ
              , cardY: card.y
              , fieldY: 22.0 + toNumber i * Theme.typeCardFieldHeight + 14.0
              }
          )
          card.fields

      fieldLabelsTree :: T.Tree { text :: String, cardY :: Number, fieldY :: Number }
      fieldLabelsTree =
        T.named Group "fieldLabelsGroup"
          [ F.staticStr "class" "field-labels"
          , F.transform $ F.text "translate(10, 30)" -- Same offset as type cards
          ]
          `T.withChild`
            ( T.joinData "fieldLabelTexts" "text" allFieldLabels $ \field ->
                T.elem Text
                  [ F.x (F.num 6.0)
                  , F.y (F.num (field.cardY + field.fieldY))
                  , F.textAnchor (F.text "start")
                  , F.fill (F.color Theme.typeCardFieldText)
                  , F.fontSize (F.px 9.0)
                  , F.fontFamily (F.text "monospace")
                  , F.textContent (F.text field.text)
                  ]
            )

    _ <- renderTree svgSel fieldLabelsTree

    -- Render data cards on RHS as database icons
    let
      -- Database icon dimensions
      dbWidth = 50.0
      dbHeight = 40.0
      dbEllipseRy = 8.0

      dataCardsTree :: T.Tree DataCard
      dataCardsTree =
        T.named Group "dataCardsGroup"
          [ F.staticStr "class" "data-cards"
          , F.transform $ F.text $ "translate(" <> show dataCardsX <> ", 30)"
          ]
          `T.withChild`
            ( T.joinData "dataCardGroups" "g" dataCards $ \card ->
                T.elem Group
                  [ F.transform $ F.text $ "translate(0," <> show card.y <> ")"
                  , F.staticStr "cursor" (if card.isEnabled then "pointer" else "default")
                  , F.opacity (F.text (if card.isEnabled then "1" else "0.3"))
                  ]
                  `T.withBehaviors`
                    ( if card.isEnabled then [ ClickWithDatum \c -> HS.notify listener (SelectSampleData c.sampleId) ]
                      else []
                    )
                  `T.withChildren`
                    -- Database icon: cylinder shape using Path elements
                    -- (Ellipse not available in ElementType)
                    let
                      cx = Theme.dataCardWidth / 2.0
                      rx = dbWidth / 2.0
                      ry = dbEllipseRy
                      -- SVG path for ellipse: M cx-rx,y A rx,ry 0 1,1 cx+rx,y A rx,ry 0 1,1 cx-rx,y
                      ellipsePath y' =
                        "M " <> show (cx - rx) <> " " <> show y'
                          <> " A " <> show rx <> " " <> show ry <> " 0 1 1 " <> show (cx + rx) <> " " <> show y'
                          <> " A " <> show rx <> " " <> show ry <> " 0 1 1 " <> show (cx - rx) <> " " <> show y'
                          <> " Z"
                      -- Bottom arc (only bottom half visible)
                      bottomArcPath =
                        "M " <> show (cx - rx) <> " " <> show dbHeight
                          <> " A " <> show rx <> " " <> show ry <> " 0 0 0 " <> show (cx + rx) <> " " <> show dbHeight
                    in
                    -- Body (rectangle)
                    [ T.elem Rect
                        [ F.x (F.num ((Theme.dataCardWidth - dbWidth) / 2.0))
                        , F.y (F.num dbEllipseRy)
                        , F.width (F.num dbWidth)
                        , F.height (F.num (dbHeight - dbEllipseRy))
                        , F.fill (F.color (if card.isSelected then Theme.dataCardHighlight else Theme.dataCardFill))
                        , F.stroke (F.text "none")
                        ]
                    -- Bottom arc (curve at bottom of cylinder)
                    , T.elem Path
                        [ F.path $ F.text bottomArcPath
                        , F.fill (F.text "none")
                        , F.stroke (F.color Theme.dataCardStroke)
                        , F.strokeWidth (F.num (if card.isSelected then 2.5 else 1.5))
                        ]
                    -- Side lines
                    , T.elem Line
                        [ F.x1 (F.num (cx - rx))
                        , F.y1 (F.num dbEllipseRy)
                        , F.x2 (F.num (cx - rx))
                        , F.y2 (F.num dbHeight)
                        , F.stroke (F.color Theme.dataCardStroke)
                        , F.strokeWidth (F.num (if card.isSelected then 2.5 else 1.5))
                        ]
                    , T.elem Line
                        [ F.x1 (F.num (cx + rx))
                        , F.y1 (F.num dbEllipseRy)
                        , F.x2 (F.num (cx + rx))
                        , F.y2 (F.num dbHeight)
                        , F.stroke (F.color Theme.dataCardStroke)
                        , F.strokeWidth (F.num (if card.isSelected then 2.5 else 1.5))
                        ]
                    -- Top ellipse (filled cap)
                    , T.elem Path
                        [ F.path $ F.text (ellipsePath dbEllipseRy)
                        , F.fill (F.color (if card.isSelected then Theme.dataCardHighlight else Theme.typeCardHeader))
                        , F.stroke (F.color Theme.dataCardStroke)
                        , F.strokeWidth (F.num (if card.isSelected then 2.5 else 1.5))
                        ]
                    -- Name text below icon
                    , T.elem Text
                        [ F.x (F.num (Theme.dataCardWidth / 2.0))
                        , F.y (F.num (dbHeight + 16.0))
                        , F.textAnchor (F.text "middle")
                        , F.fill (F.color Theme.dataCardStroke)
                        , F.fontSize (F.px 11.0)
                        , F.staticStr "font-weight" "bold"
                        , F.textContent (F.text card.name)
                        ]
                    -- Description text
                    , T.elem Text
                        [ F.x (F.num (Theme.dataCardWidth / 2.0))
                        , F.y (F.num (dbHeight + 28.0))
                        , F.textAnchor (F.text "middle")
                        , F.fill (F.color Theme.keyHintsColor)
                        , F.fontSize (F.px 9.0)
                        , F.textContent (F.text card.description)
                        ]
                    -- Red X for incompatible (only shown when disabled)
                    , T.elem Text
                        [ F.x (F.num (Theme.dataCardWidth - 8.0))
                        , F.y (F.num 12.0)
                        , F.textAnchor (F.text "middle")
                        , F.fill (F.text (if card.isEnabled then "none" else "#E63946"))
                        , F.fontSize (F.px 16.0)
                        , F.staticStr "font-weight" "bold"
                        , F.textContent (F.text "✕")
                        ]
                    ]
            )

    _ <- renderTree svgSel dataCardsTree

    -- Draw arrow from assigned type card to the selected Join node
    -- Only draw if a Join is selected and has a type assigned
    let
      maybeArrow = do
        -- Find the assigned type card
        assignedCard <- Array.find _.isAssigned typeCards
        -- Find the selected Join node
        selectedId <- state.selectedNodeId
        selectedNode <- Array.find (\n -> n.id == selectedId) structuralNodes
        -- Return arrow data
        pure
          { cardX: 10.0 + Theme.typeCardWidth -- Right edge of card
          , cardY: 30.0 + assignedCard.y + 11.0 -- Center of header
          , nodeX: selectedNode.x + offsetX - 40.0 -- Left edge of node
          , nodeY: selectedNode.y + offsetY -- Center of node
          }

      arrowData = case maybeArrow of
        Just arrow -> [ arrow ]
        Nothing -> []

      -- Bezier curve from type card to Join node
      arrowPath arrow =
        let
          midX = (arrow.cardX + arrow.nodeX) / 2.0
        in
          "M " <> show arrow.cardX <> " " <> show arrow.cardY
            <> " C "
            <> show midX
            <> " "
            <> show arrow.cardY
            <> " "
            <> show midX
            <> " "
            <> show arrow.nodeY
            <> " "
            <> show arrow.nodeX
            <> " "
            <> show arrow.nodeY

      arrowTree :: T.Tree { cardX :: Number, cardY :: Number, nodeX :: Number, nodeY :: Number }
      arrowTree =
        T.named Group "typeArrowGroup"
          [ F.staticStr "class" "type-arrow" ]
          `T.withChild`
            ( T.joinData "typeArrows" "g" arrowData $ \arrow ->
                T.elem Group []
                  `T.withChildren`
                    [ -- Arrow path
                      T.elem Path
                        [ F.path $ F.text $ arrowPath arrow
                        , F.fill (F.text "none")
                        , F.stroke (F.color Theme.typeCardHeader)
                        , F.strokeWidth (F.num 2.0)
                        , F.staticStr "stroke-dasharray" "5,3"
                        ]
                    , -- Arrow head (triangle at the end)
                      T.elem Path
                        [ F.path $ F.text $
                            "M " <> show (arrow.nodeX - 8.0) <> " " <> show (arrow.nodeY - 5.0)
                              <> " L "
                              <> show arrow.nodeX
                              <> " "
                              <> show arrow.nodeY
                              <> " L "
                              <> show (arrow.nodeX - 8.0)
                              <> " "
                              <> show (arrow.nodeY + 5.0)
                              <>
                                " Z"
                        , F.fill (F.color Theme.typeCardHeader)
                        , F.stroke (F.text "none")
                        ]
                    ]
            )

    _ <- renderTree svgSel arrowTree

    -- Draw arrows from selected data card to Enter/Update/Exit nodes
    -- For regular joins: arrow to the template element
    -- For update joins: arrows to Enter, Update, Exit nodes
    let
      -- Find GUP phase nodes (Enter/Update/Exit) by looking at the tree
      findGUPNodes :: Array TreeNode
      findGUPNodes = Array.filter isGUPNode structuralNodes
        where
        isGUPNode n = case n.nodeType of
          NodeEnter -> true
          NodeUpdate -> true
          NodeExit -> true
          _ -> false

      -- Find the template element (first structural child of the selected Join)
      findTemplateNode :: Maybe TreeNode
      findTemplateNode = do
        selectedId <- state.selectedNodeId
        selectedNode <- findNodeById selectedId state.userTree
        -- Check if it's an update join
        if isUpdateJoinType selectedNode.nodeType then Nothing -- For update joins, we use GUP nodes instead
        else do
          -- For regular joins, find the template (first child)
          let childIds = getChildrenIds selectedId state.userTree
          firstChildId <- Array.head childIds
          Array.find (\n -> n.id == firstChildId) structuralNodes
        where
        isUpdateJoinType NodeUpdateJoin = true
        isUpdateJoinType NodeUpdateNestedJoin = true
        isUpdateJoinType _ = false

      -- Build arrow data for data card -> node connections
      -- Include targetId for zoom-reactive updates
      dataArrowTargets :: Array { nodeBaseX :: Number, nodeBaseY :: Number, color :: String, targetId :: String }
      dataArrowTargets =
        case state.selectedSampleData of
          Nothing -> []
          Just _ ->
            -- Check if we have GUP nodes or template node
            let
              gupNodes = findGUPNodes
            in
              if Array.length gupNodes > 0 then map
                ( \n ->
                    { nodeBaseX: n.x + offsetX + 40.0 -- Right edge of node (base position)
                    , nodeBaseY: n.y + offsetY
                    , color: toHexString $ Theme.nodeTypeColor n.nodeType
                    , targetId: nodeTypeToTargetId n.nodeType
                    }
                )
                gupNodes
              else case findTemplateNode of
                Just n ->
                  [ { nodeBaseX: n.x + offsetX + 40.0
                    , nodeBaseY: n.y + offsetY
                    , color: toHexString $ Theme.dataCardStroke
                    , targetId: "template"
                    }
                  ]
                Nothing -> []

      -- Convert node type to arrow target ID
      nodeTypeToTargetId :: DslNodeType -> String
      nodeTypeToTargetId = case _ of
        NodeEnter -> "enter"
        NodeUpdate -> "update"
        NodeExit -> "exit"
        _ -> "template"

      -- Find selected data card position
      selectedDataCard = Array.find _.isSelected dataCards

      -- Build full arrow data with source position
      -- nodeBaseX/Y are pre-zoom positions, nodeX/Y are rendered positions
      dataArrowData = case selectedDataCard of
        Nothing -> []
        Just card ->
          map
            ( \target ->
                { cardX: dataCardsX -- Left edge of data card
                , cardY: 30.0 + card.y + Theme.dataCardHeight / 2.0 -- Center of card
                , nodeX: target.nodeBaseX -- For initial render (no zoom yet)
                , nodeY: target.nodeBaseY
                , color: target.color
                , targetId: target.targetId
                }
            )
            dataArrowTargets

      -- Bezier curve from data card to node (going left)
      dataArrowPath arrow =
        let
          midX = (arrow.cardX + arrow.nodeX) / 2.0
        in
          "M " <> show arrow.cardX <> " " <> show arrow.cardY
            <> " C "
            <> show midX
            <> " "
            <> show arrow.cardY
            <> " "
            <> show midX
            <> " "
            <> show arrow.nodeY
            <> " "
            <> show arrow.nodeX
            <> " "
            <> show arrow.nodeY

      dataArrowTree :: T.Tree { cardX :: Number, cardY :: Number, nodeX :: Number, nodeY :: Number, color :: String, targetId :: String }
      dataArrowTree =
        T.named Group "dataArrowGroup"
          [ F.staticStr "class" "data-arrow" ]
          `T.withChild`
            ( T.joinData "dataArrows" "g" dataArrowData $ \arrow ->
                T.elem Group
                  -- Add class based on target for zoom-reactive updates
                  [ F.staticStr "class" ("data-arrow-" <> arrow.targetId) ]
                  `T.withChildren`
                    [ -- Arrow path
                      T.elem Path
                        [ F.path $ F.text $ dataArrowPath arrow
                        , F.fill (F.text "none")
                        , F.stroke (F.text arrow.color)
                        , F.strokeWidth (F.num 2.0)
                        , F.staticStr "stroke-dasharray" "5,3"
                        ]
                    , -- Arrow head (triangle pointing left, at the node)
                      T.elem Path
                        [ F.path $ F.text $
                            "M " <> show (arrow.nodeX + 8.0) <> " " <> show (arrow.nodeY - 5.0)
                              <> " L "
                              <> show arrow.nodeX
                              <> " "
                              <> show arrow.nodeY
                              <> " L "
                              <> show (arrow.nodeX + 8.0)
                              <> " "
                              <> show (arrow.nodeY + 5.0)
                              <>
                                " Z"
                        , F.fill (F.text arrow.color)
                        , F.stroke (F.text "none")
                        ]
                    ]
            )

    _ <- renderTree svgSel dataArrowTree

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
    , typeLabel: datumTypeLabel node.datumType -- Phantom type annotation
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

-- | Setup zoom with callback to notify Halogen of zoom changes
setupZoomWithCallback :: ZoomTransform -> HS.Listener Action -> Effect Unit
setupZoomWithCallback transform listener = do
  doc <- window >>= document
  let parentNode = Document.toParentNode (toDocument doc)
  maybeSvg <- querySelector (QuerySelector "#tree-builder3-svg") parentNode
  for_ maybeSvg \svg -> do
    let ScaleExtent minScale maxScale = ScaleExtent 0.5 4.0
    let onZoom t = HS.notify listener (ZoomChanged t)
    _ <- attachZoomWithCallback_ svg minScale maxScale ".zoom-group" transform onZoom
    pure unit

-- | Update arrow position imperatively when zoom changes
-- | Recalculates arrow path using current zoom transform
updateArrowPosition :: State -> Effect Unit
updateArrowPosition state = do
  -- Calculate same offsets as renderTreeViz
  let svgWidth = 900.0
  let offsetY = 60.0

  -- Get structural tree and apply layout
  let structuralTree = filterStructuralTree state.userTree
  let positioned = applyLayout structuralTree
  let structuralNodes = flattenTree positioned

  -- Calculate centerX for offsetX
  let
    firstX = case Array.head structuralNodes of
      Just n -> n.x
      Nothing -> 0.0
  let minX = Array.foldl (\acc n -> min acc n.x) firstX structuralNodes
  let maxX = Array.foldl (\acc n -> max acc n.x) firstX structuralNodes
  let centerX = (minX + maxX) / 2.0
  let offsetX = (svgWidth / 2.0) - centerX

  -- Get the assigned type card and selected node info
  let selectedJoinType = getSelectedJoinDatumType state
  let typeCards = makeTypeCards state.selectedAstPreset selectedJoinType

  -- Find arrow data (same logic as in renderTreeViz)
  let
    maybeArrow = do
      assignedCard <- Array.find _.isAssigned typeCards
      selectedId <- state.selectedNodeId
      selectedNode <- Array.find (\n -> n.id == selectedId) structuralNodes
      pure
        { cardX: 10.0 + Theme.typeCardWidth
        , cardY: 30.0 + assignedCard.y + 11.0
        -- Base node position (before zoom)
        , nodeBaseX: selectedNode.x + offsetX - 40.0
        , nodeBaseY: selectedNode.y + offsetY
        }

  case maybeArrow of
    Just arrow -> do
      let { k, x, y } = state.zoomTransform
      -- Transform node position using zoom
      let nodeX = arrow.nodeBaseX * k + x
      let nodeY = arrow.nodeBaseY * k + y
      let midX = (arrow.cardX + nodeX) / 2.0

      -- Build arrow path
      let
        pathD =
          "M " <> show arrow.cardX <> " " <> show arrow.cardY
            <> " C "
            <> show midX
            <> " "
            <> show arrow.cardY
            <> " "
            <> show midX
            <> " "
            <> show nodeY
            <> " "
            <> show nodeX
            <> " "
            <> show nodeY

      -- Build arrowhead path
      let
        arrowheadD =
          "M " <> show (nodeX - 8.0) <> " " <> show (nodeY - 5.0)
            <> " L "
            <> show nodeX
            <> " "
            <> show nodeY
            <> " L "
            <> show (nodeX - 8.0)
            <> " "
            <> show (nodeY + 5.0)
            <>
              " Z"

      -- Update DOM elements using class selectors
      updateAttr_ ".type-arrow path:first-child" "d" pathD
      updateAttr_ ".type-arrow path:last-child" "d" arrowheadD

    Nothing -> pure unit

  -- ===== Data Arrow Updates =====
  -- Update data arrows (from data cards to GUP/template nodes)
  let { k, x, y } = state.zoomTransform
  let dataCardsX = svgWidth - Theme.dataCardWidth - 10.0

  -- Find GUP nodes (Enter/Update/Exit)
  let
    findGUPNodes :: Array TreeNode
    findGUPNodes = Array.filter isGUPNode structuralNodes
      where
      isGUPNode n = case n.nodeType of
        NodeEnter -> true
        NodeUpdate -> true
        NodeExit -> true
        _ -> false

    -- Find template node (for regular joins)
    findTemplateNode :: Maybe TreeNode
    findTemplateNode = do
      selectedId <- state.selectedNodeId
      selectedNode <- findNodeById selectedId state.userTree
      if isUpdateJoinType selectedNode.nodeType then Nothing
      else do
        let childIds = getChildrenIds selectedId state.userTree
        firstChildId <- Array.head childIds
        Array.find (\n -> n.id == firstChildId) structuralNodes
      where
      isUpdateJoinType NodeUpdateJoin = true
      isUpdateJoinType NodeUpdateNestedJoin = true
      isUpdateJoinType _ = false

    -- Build data arrow targets (same logic as renderTreeViz but with base positions)
    dataArrowTargets :: Array { nodeBaseX :: Number, nodeBaseY :: Number, targetId :: String }
    dataArrowTargets =
      case state.selectedSampleData of
        Nothing -> []
        Just _ ->
          let
            gupNodes = findGUPNodes
          in
            if Array.length gupNodes > 0 then map
              ( \n ->
                  { nodeBaseX: n.x + offsetX + 40.0
                  , nodeBaseY: n.y + offsetY
                  , targetId: nodeTypeToTargetId n.nodeType
                  }
              )
              gupNodes
            else case findTemplateNode of
              Just n ->
                [ { nodeBaseX: n.x + offsetX + 40.0
                  , nodeBaseY: n.y + offsetY
                  , targetId: "template"
                  }
                ]
              Nothing -> []

    nodeTypeToTargetId :: DslNodeType -> String
    nodeTypeToTargetId = case _ of
      NodeEnter -> "enter"
      NodeUpdate -> "update"
      NodeExit -> "exit"
      _ -> "template"

    -- Get selected data card position
    dataCards = makeDataCards selectedJoinType state.selectedSampleData
    selectedDataCard = Array.find _.isSelected dataCards

  -- Update each data arrow
  case selectedDataCard of
    Nothing -> pure unit
    Just card -> do
      let cardY = 30.0 + card.y + Theme.dataCardHeight / 2.0
      -- Update each target arrow
      for_ dataArrowTargets \target -> do
        -- Transform node position using zoom
        let nodeX = target.nodeBaseX * k + x
        let nodeY = target.nodeBaseY * k + y
        let midX = (dataCardsX + nodeX) / 2.0

        -- Build arrow path (going from right to left)
        let
          pathD =
            "M " <> show dataCardsX <> " " <> show cardY
              <> " C "
              <> show midX
              <> " "
              <> show cardY
              <> " "
              <> show midX
              <> " "
              <> show nodeY
              <> " "
              <> show nodeX
              <> " "
              <> show nodeY

        -- Build arrowhead path (pointing left)
        let
          arrowheadD =
            "M " <> show (nodeX + 8.0) <> " " <> show (nodeY - 5.0)
              <> " L "
              <> show nodeX
              <> " "
              <> show nodeY
              <> " L "
              <> show (nodeX + 8.0)
              <> " "
              <> show (nodeY + 5.0)
              <>
                " Z"

        -- Update DOM elements using target-specific selectors
        let selector = ".data-arrow-" <> target.targetId
        updateAttr_ (selector <> " path:first-child") "d" pathD
        updateAttr_ (selector <> " path:last-child") "d" arrowheadD

-- =============================================================================
-- SVG Type Cards Helpers
-- =============================================================================

-- | Get the datum type of the selected Join (if any)
getSelectedJoinDatumType :: State -> Maybe DatumType
getSelectedJoinDatumType state = case state.selectedNodeId of
  Nothing -> Nothing
  Just selectedId -> case findNodeById selectedId state.userTree of
    Nothing -> Nothing
    Just node ->
      if isJoinNodeType node.nodeType then Just node.datumType
      else Nothing
  where
  isJoinNodeType NodeJoin = true
  isJoinNodeType NodeNestedJoin = true
  isJoinNodeType NodeUpdateJoin = true
  isJoinNodeType NodeUpdateNestedJoin = true
  isJoinNodeType _ = false

-- | Create type card data for SVG rendering
-- | Types incompatible with current AST have isEnabled = false (rendered at 0.3 opacity)
makeTypeCards :: AstPreset -> Maybe DatumType -> Array TypeCard
makeTypeCards currentAst selectedJoinType =
  Array.mapWithIndex makeCard availableTypeInfos
  where
  -- Calculate card height: header (22) + fields * fieldHeight + padding (4)
  cardHeight :: Int -> Number
  cardHeight numFields = 22.0 + toNumber numFields * Theme.typeCardFieldHeight + 4.0 + Theme.typeCardSpacing

  -- Available types with their field info
  -- Record types show their fields, Array types show [ innerType ]
  availableTypeInfos =
    [ { name: "Point", typ: pointType, fields: pointFields }
    , { name: "Node", typ: nodeType, fields: nodeFields }
    , { name: "Link", typ: linkType, fields: linkFields }
    , { name: "Country", typ: countryType, fields: countryFields }
    -- Array types for nested joins (board games)
    , { name: "Cell", typ: cellType, fields: cellFields }
    , { name: "Row", typ: rowType, fields: rowFields }
    , { name: "Board", typ: boardType, fields: boardFields }
    ]

  pointFields = [ { name: "x", typ: "Number" }, { name: "y", typ: "Number" } ]
  nodeFields = [ { name: "id", typ: "String" }, { name: "x", typ: "Number" }, { name: "y", typ: "Number" }, { name: "group", typ: "Int" } ]
  linkFields = [ { name: "source", typ: "String" }, { name: "target", typ: "String" }, { name: "value", typ: "Number" } ]
  countryFields = [ { name: "name", typ: "String" }, { name: "population", typ: "Number" }, { name: "gdp", typ: "Number" }, { name: "lifeExp", typ: "Number" } ]
  -- Cell fields
  cellFields = [ { name: "row", typ: "Int" }, { name: "col", typ: "Int" }, { name: "value", typ: "String" } ]
  -- Array types show their inner type
  rowFields = [ { name: "[ ]", typ: "Cell" } ] -- Row = Array Cell
  boardFields = [ { name: "[ ]", typ: "Row" } ] -- Board = Array Row

  -- Calculate cumulative Y positions
  makeCard idx info =
    let
      -- Calculate Y by summing heights of previous cards
      prevHeights = Array.take idx availableTypeInfos
        # map (\t -> cardHeight (Array.length t.fields))
        # Array.foldl (+) 0.0
    in
      { name: info.name
      , fields: info.fields
      , datumType: info.typ
      , y: prevHeights
      , isEnabled: isTypeCompatibleWithAst currentAst info.typ
      , isAssigned: isTypeAssigned info.typ selectedJoinType
      }

  -- Check if a type matches the selected join's type
  isTypeAssigned :: DatumType -> Maybe DatumType -> Boolean
  isTypeAssigned cardType (Just selectedType) = cardType == selectedType
  isTypeAssigned _ Nothing = false

-- | Create data card options - always shows all datasets
-- | Datasets incompatible with current type have isEnabled = false (rendered at 0.3 opacity)
makeDataCards :: Maybe DatumType -> Maybe SampleDataId -> Array DataCard
makeDataCards maybeType selectedSample =
  Array.mapWithIndex mkCard allDatasets
  where
  -- All available datasets
  allDatasets =
    [ { name: "Chess", desc: "8x8 board", sampleId: SampleChess }
    , { name: "Sudoku", desc: "9x9 puzzle", sampleId: SampleSudoku }
    , { name: "Go", desc: "19x19 board", sampleId: SampleGo }
    , { name: "Gapminder", desc: "Health & wealth", sampleId: SampleGapminder }
    , { name: "Force Graph", desc: "Node network", sampleId: SampleForceGraph }
    , { name: "Scatter Plot", desc: "X/Y points", sampleId: SampleScatterPlot }
    , { name: "GUP Letters", desc: "Enter/Update/Exit", sampleId: SampleGupLetters }
    ]

  mkCard :: Int -> { name :: String, desc :: String, sampleId :: SampleDataId } -> DataCard
  mkCard idx dataset =
    { name: dataset.name
    , description: dataset.desc
    , sampleId: dataset.sampleId
    , y: toNumber idx * (Theme.dataCardHeight + Theme.dataCardSpacing)
    , isEnabled: isDatasetCompatibleWithType maybeType dataset.sampleId
    , isSelected: selectedSample == Just dataset.sampleId
    }

-- =============================================================================
-- AST Preset Cards (Halogen HTML)
-- =============================================================================

-- | Render a single AST preset card (Halogen HTML)
renderAstCard :: forall m. State -> AstPreset -> H.ComponentHTML Action () m
renderAstCard state preset =
  HH.div
    [ HP.class_ (HH.ClassName cardClasses)
    , HE.onClick \_ -> SelectAstPreset preset
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "ast-card-name") ]
        [ HH.text (astPresetLabel preset) ]
    , HH.div
        [ HP.class_ (HH.ClassName "ast-card-description") ]
        [ HH.text (astPresetDescription preset) ]
    ]
  where
  isSelected = state.selectedAstPreset == preset
  cardClasses =
    if isSelected then "ast-card ast-card-selected"
    else "ast-card"

-- =============================================================================
-- Rendered Output Panel
-- =============================================================================

-- | Render the output panel showing visualization result
-- | This will eventually render the actual visualization from the current AST + data
renderOutput :: forall m. State -> H.ComponentHTML Action () m
renderOutput state
  -- TryMe mode: show English description of the tree
  | state.selectedAstPreset == AstEmpty =
      HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-output-content english-output") ]
        [ HH.pre
            [ HP.class_ (HH.ClassName "english-description") ]
            [ HH.text (treeToEnglish state.userTree 0) ]
        ]
  -- Normal mode: show visualization status
  | otherwise =
      HH.div
        [ HP.class_ (HH.ClassName "tree-builder3-output-content") ]
        [ case getOutputStatus state of
            NoJoinSelected ->
              HH.div
                [ HP.class_ (HH.ClassName "output-placeholder") ]
                [ HH.p_ [ HH.text "Select a Join node to assign a type" ]
                , HH.p_ [ HH.text "Use keyboard: j=Join, n=NestedJoin, u=UpdateJoin" ]
                ]
            JoinNeedsType ->
              HH.div
                [ HP.class_ (HH.ClassName "output-placeholder") ]
                [ HH.p_ [ HH.text "Click a type card on the left to assign data type" ]
                ]
            TypeNeedsData ->
              HH.div
                [ HP.class_ (HH.ClassName "output-placeholder") ]
                [ HH.p_ [ HH.text "Click a data card on the right to select sample data" ]
                ]
            ReadyToRender sampleId ->
              HH.div
                [ HP.class_ (HH.ClassName "output-ready") ]
                [ HH.p_ [ HH.text $ "Ready to render: " <> sampleIdLabel sampleId ]
                , HH.div
                    [ HP.id "tree-builder3-output-svg"
                    , HP.class_ (HH.ClassName "output-svg-container")
                    ]
                    []
                ]
        ]

-- | Status of the output panel
data OutputStatus
  = NoJoinSelected
  | JoinNeedsType
  | TypeNeedsData
  | ReadyToRender SampleDataId

-- | Determine the current output status
getOutputStatus :: State -> OutputStatus
getOutputStatus state = case state.selectedNodeId of
  Nothing -> NoJoinSelected
  Just selectedId -> case findNodeById selectedId state.userTree of
    Nothing -> NoJoinSelected
    Just node ->
      if not (isJoinType node.nodeType) then NoJoinSelected
      else if node.datumType == TypeUnit || node.datumType == TypeUnknown then JoinNeedsType
      else case state.selectedSampleData of
        Nothing -> TypeNeedsData
        Just sampleId -> ReadyToRender sampleId
  where
  isJoinType NodeJoin = true
  isJoinType NodeNestedJoin = true
  isJoinType NodeUpdateJoin = true
  isJoinType NodeUpdateNestedJoin = true
  isJoinType _ = false

-- | Get label for sample data ID
sampleIdLabel :: SampleDataId -> String
sampleIdLabel = case _ of
  SampleChess -> "Chess Board"
  SampleSudoku -> "Sudoku Puzzle"
  SampleGo -> "Go Board"
  SampleScatterPlot -> "Scatter Plot"
  SampleForceGraph -> "Force Graph"
  SampleGapminder -> "Gapminder"
  SampleGupLetters -> "GUP Letters"

-- =============================================================================
-- TryMe Mode: English Description & Popup
-- =============================================================================

-- | Convert tree to English description (for TryMe mode)
treeToEnglish :: Tree TreeNode -> Int -> String
treeToEnglish tree depth =
  let
    node = Cofree.head tree
    indent = String.joinWith "" (Array.replicate depth "  ")
    children = Array.fromFoldable (Cofree.tail tree)
    childrenText = String.joinWith "\n" (map (\c -> treeToEnglish c (depth + 1)) children)
    nodeDesc = describeNode node
  in
    indent <> nodeDesc <> (if String.null childrenText then "" else "\n" <> childrenText)

-- | Describe a single node in English
describeNode :: TreeNode -> String
describeNode node = case node.nodeType of
  NodeElem SVG -> "Create an SVG container"
  NodeElem Group -> "Wrap in a group" <> maybeNamed node.name
  NodeElem Circle -> "Draw a circle"
  NodeElem Rect -> "Draw a rectangle"
  NodeElem Text -> "Add text"
  NodeElem Path -> "Draw a path"
  NodeElem Line -> "Draw a line"
  NodeElem _ -> "Create element"
  NodeJoin -> "Join data as elements" <> maybeNamed node.name <> maybeTyped node.datumType
  NodeNestedJoin -> "Nest data by grouping" <> maybeNamed node.name <> maybeTyped node.datumType
  NodeUpdateJoin -> "Use enter/update/exit pattern" <> maybeNamed node.name
  NodeUpdateNestedJoin -> "Nested update pattern" <> maybeNamed node.name
  NodeEnter -> "For entering elements:"
  NodeUpdate -> "For updating elements:"
  NodeExit -> "For exiting elements:"
  NodeAttr _ -> "Set attribute"
  NodeBehavior _ -> "Add behavior"
  PendingElement -> "(selecting element type...)"
  PendingAttr -> "(selecting attribute...)"
  PendingAttrValue _ -> "(selecting value...)"
  PendingBehavior -> "(selecting behavior...)"
  where
  maybeNamed Nothing = ""
  maybeNamed (Just n) = " (\"" <> n <> "\")"
  maybeTyped TypeUnit = ""
  maybeTyped TypeUnknown = ""
  maybeTyped (TypeRecord typeName fields) =
    let fieldNames = map (\f -> f.name) fields
    in " (" <> typeName <> ": " <> String.joinWith ", " fieldNames <> ")"
  maybeTyped (TypeArray _) = ""

-- | Render the TryMe instructions popup
renderTryMePopup :: forall m. H.ComponentHTML Action () m
renderTryMePopup =
  HH.div
    [ HP.class_ (HH.ClassName "tryme-popup-overlay")
    , HE.onClick \_ -> DismissTryMeInstructions
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "tryme-popup")
        , HE.onClick \_ -> DismissTryMeInstructions -- Clicking popup also dismisses
        ]
        [ HH.h2_ [ HH.text "Build Your Own Visualization!" ]
        , HH.div
            [ HP.class_ (HH.ClassName "tryme-content") ]
            [ HH.p_ [ HH.text "Welcome to TryMe mode! Build a visualization tree using keyboard commands." ]
            , HH.h3_ [ HH.text "Getting Started:" ]
            , HH.ol_
                [ HH.li_ [ HH.text "First, pick a data type from the left cards" ]
                , HH.li_ [ HH.text "Then, select a dataset from the right cards" ]
                , HH.li_ [ HH.text "Use keys to add nodes to your tree" ]
                ]
            , HH.h3_ [ HH.text "Key Commands:" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "J" ], HH.text " - Add a Join (binds data to elements)" ]
                , HH.li_ [ HH.strong_ [ HH.text "N" ], HH.text " - Add a NestedJoin (for nested data)" ]
                , HH.li_ [ HH.strong_ [ HH.text "G" ], HH.text " - Add a Group (container)" ]
                , HH.li_ [ HH.strong_ [ HH.text "C" ], HH.text " - Add a Circle" ]
                , HH.li_ [ HH.strong_ [ HH.text "R" ], HH.text " - Add a Rectangle" ]
                , HH.li_ [ HH.strong_ [ HH.text "T" ], HH.text " - Add Text" ]
                , HH.li_ [ HH.strong_ [ HH.text "Arrow keys" ], HH.text " - Navigate the tree" ]
                , HH.li_ [ HH.strong_ [ HH.text "Delete/Backspace" ], HH.text " - Remove selected node" ]
                ]
            , HH.p
                [ HP.class_ (HH.ClassName "tryme-dismiss") ]
                [ HH.text "Click anywhere to start building!" ]
            ]
        ]
    ]

-- =============================================================================
-- Output Visualization Rendering
-- =============================================================================

-- | Render the output visualization when state is ready
renderOutputViz :: State -> Effect Unit
renderOutputViz state
  -- TryMe mode doesn't render a visualization
  | state.selectedAstPreset == AstEmpty = pure unit
  -- Only render if we have selected sample data
  | otherwise = case state.selectedSampleData of
      Nothing -> pure unit
      Just sampleId -> do
        -- Clear and render
        clearOutputContainer
        renderForPreset state.selectedAstPreset sampleId

-- | Clear the output SVG container
clearOutputContainer :: Effect Unit
clearOutputContainer = clearContainer "#tree-builder3-output-svg"

-- | Render visualization for a specific preset and sample data
renderForPreset :: AstPreset -> SampleDataId -> Effect Unit
renderForPreset preset sampleId = case preset of
  AstEmpty -> pure unit  -- TryMe mode - no rendering
  AstGrid -> renderGridViz sampleId
  AstScatter -> renderScatterViz sampleId
  AstBubble -> renderBubbleViz sampleId
  AstTree -> renderTreeOutputViz sampleId
  AstUpdateJoin -> renderGupViz sampleId

-- =============================================================================
-- Grid Visualization (Chess, Sudoku, Go)
-- =============================================================================

-- | Render a grid visualization for board games
renderGridViz :: SampleDataId -> Effect Unit
renderGridViz sampleId = do
  let config = gridConfigFor sampleId
  runD3v2M do
    container <- select "#tree-builder3-output-svg" :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      tree :: T.Tree { row :: Int, col :: Int, value :: String, color :: String }
      tree =
        T.named SVG "outputSvg"
          [ F.width (F.num config.svgWidth)
          , F.height (F.num config.svgHeight)
          , F.viewBox 0.0 0.0 config.svgWidth config.svgHeight
          ]
          `T.withChild`
            ( T.named Group "board"
                [ F.transform $ F.text $ "translate(" <> show config.padding <> "," <> show config.padding <> ")" ]
                `T.withChild`
                  ( T.joinData "cells" "g" config.data $ \cell ->
                      T.elem Group
                        [ F.transform $ F.text $
                            "translate(" <> show (toNumber cell.col * config.cellSize) <>
                            "," <> show (toNumber cell.row * config.cellSize) <> ")"
                        ]
                        `T.withChild`
                          ( T.elem Rect
                              [ F.x $ F.num 0.0
                              , F.y $ F.num 0.0
                              , F.width $ F.num (config.cellSize - 1.0)
                              , F.height $ F.num (config.cellSize - 1.0)
                              , F.fill $ F.text cell.color
                              , F.stroke $ F.hex "#333"
                              , F.strokeWidth $ F.num 0.5
                              ]
                          )
                        `T.withChild`
                          ( T.elem Text
                              [ F.x $ F.num (config.cellSize / 2.0)
                              , F.y $ F.num (config.cellSize / 2.0 + 4.0)
                              , F.textAnchor $ F.text "middle"
                              , F.fontSize $ F.text config.fontSize
                              , F.fill $ F.text config.textColor
                              , F.textContent $ F.text cell.value
                              ]
                          )
                  )
            )
    _ <- renderTree container tree
    pure unit

-- | Configuration for grid visualizations
type GridConfig =
  { svgWidth :: Number
  , svgHeight :: Number
  , cellSize :: Number
  , padding :: Number
  , fontSize :: String
  , textColor :: String
  , data :: Array { row :: Int, col :: Int, value :: String, color :: String }
  }

-- | Get grid config for a sample ID
gridConfigFor :: SampleDataId -> GridConfig
gridConfigFor SampleChess =
  { svgWidth: 200.0
  , svgHeight: 200.0
  , cellSize: 22.0
  , padding: 5.0
  , fontSize: "14px"
  , textColor: "#000"
  , data: chessData
  }
gridConfigFor SampleSudoku =
  { svgWidth: 200.0
  , svgHeight: 200.0
  , cellSize: 20.0
  , padding: 5.0
  , fontSize: "11px"
  , textColor: "#333"
  , data: sudokuData
  }
gridConfigFor SampleGo =
  { svgWidth: 200.0
  , svgHeight: 200.0
  , cellSize: 20.0
  , padding: 5.0
  , fontSize: "16px"
  , textColor: "#000"
  , data: goData
  }
gridConfigFor _ =
  { svgWidth: 200.0
  , svgHeight: 200.0
  , cellSize: 20.0
  , padding: 5.0
  , fontSize: "12px"
  , textColor: "#333"
  , data: []
  }

-- | Chess board data (8x8)
chessData :: Array { row :: Int, col :: Int, value :: String, color :: String }
chessData = do
  row <- Array.range 0 7
  col <- Array.range 0 7
  let
    isLight = (row + col) `mod` 2 == 0
    color = if isLight then "#f0d9b5" else "#b58863"
    -- Starting position pieces
    piece = chessPiece row col
  pure { row, col, value: piece, color }

-- | Get chess piece for starting position
chessPiece :: Int -> Int -> String
chessPiece 0 0 = "♜"
chessPiece 0 1 = "♞"
chessPiece 0 2 = "♝"
chessPiece 0 3 = "♛"
chessPiece 0 4 = "♚"
chessPiece 0 5 = "♝"
chessPiece 0 6 = "♞"
chessPiece 0 7 = "♜"
chessPiece 1 _ = "♟"
chessPiece 6 _ = "♙"
chessPiece 7 0 = "♖"
chessPiece 7 1 = "♘"
chessPiece 7 2 = "♗"
chessPiece 7 3 = "♕"
chessPiece 7 4 = "♔"
chessPiece 7 5 = "♗"
chessPiece 7 6 = "♘"
chessPiece 7 7 = "♖"
chessPiece _ _ = ""

-- | Sudoku data (9x9)
sudokuData :: Array { row :: Int, col :: Int, value :: String, color :: String }
sudokuData = do
  row <- Array.range 0 8
  col <- Array.range 0 8
  let
    -- Alternating 3x3 box colors
    boxRow = row / 3
    boxCol = col / 3
    isAltBox = (boxRow + boxCol) `mod` 2 == 0
    color = if isAltBox then "#e8e8e8" else "#ffffff"
    value = sudokuValue row col
  pure { row, col, value, color }

-- | Get sudoku value (sample puzzle)
sudokuValue :: Int -> Int -> String
sudokuValue 0 0 = "5"
sudokuValue 0 1 = "3"
sudokuValue 0 4 = "7"
sudokuValue 1 0 = "6"
sudokuValue 1 3 = "1"
sudokuValue 1 4 = "9"
sudokuValue 1 5 = "5"
sudokuValue 2 1 = "9"
sudokuValue 2 2 = "8"
sudokuValue 2 7 = "6"
sudokuValue 3 0 = "8"
sudokuValue 3 4 = "6"
sudokuValue 3 8 = "3"
sudokuValue 4 0 = "4"
sudokuValue 4 3 = "8"
sudokuValue 4 5 = "3"
sudokuValue 4 8 = "1"
sudokuValue 5 0 = "7"
sudokuValue 5 4 = "2"
sudokuValue 5 8 = "6"
sudokuValue 6 1 = "6"
sudokuValue 6 6 = "2"
sudokuValue 6 7 = "8"
sudokuValue 7 3 = "4"
sudokuValue 7 4 = "1"
sudokuValue 7 5 = "9"
sudokuValue 7 8 = "5"
sudokuValue 8 4 = "8"
sudokuValue 8 7 = "7"
sudokuValue 8 8 = "9"
sudokuValue _ _ = ""

-- | Go board data (9x9 sample)
goData :: Array { row :: Int, col :: Int, value :: String, color :: String }
goData = do
  row <- Array.range 0 8
  col <- Array.range 0 8
  let
    color = "#dcb35c"  -- Wood color
    stone = goStone row col
  pure { row, col, value: stone, color }

-- | Get go stone (sample game position)
goStone :: Int -> Int -> String
goStone 2 2 = "●"  -- Black
goStone 2 6 = "●"
goStone 3 3 = "○"  -- White
goStone 3 5 = "●"
goStone 4 4 = "○"
goStone 5 3 = "●"
goStone 5 5 = "○"
goStone 6 2 = "○"
goStone 6 6 = "●"
goStone _ _ = "·"  -- Grid point

-- =============================================================================
-- Scatter Visualization
-- =============================================================================

-- | Render a scatter plot
renderScatterViz :: SampleDataId -> Effect Unit
renderScatterViz _ = do
  runD3v2M do
    container <- select "#tree-builder3-output-svg" :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      tree :: T.Tree { x :: Number, y :: Number, color :: String }
      tree =
        T.named SVG "outputSvg"
          [ F.width (F.num 200.0)
          , F.height (F.num 200.0)
          , F.viewBox 0.0 0.0 200.0 200.0
          ]
          `T.withChild`
            ( T.named Group "points"
                [ F.transform $ F.text "translate(20, 20)" ]
                `T.withChild`
                  ( T.joinData "circles" "circle" scatterData $ \pt ->
                      T.elem Circle
                        [ F.cx $ F.num pt.x
                        , F.cy $ F.num (160.0 - pt.y)  -- Flip Y axis
                        , F.r $ F.num 5.0
                        , F.fill $ F.text pt.color
                        , F.opacity $ F.num 0.7
                        ]
                  )
            )
          `T.withChild`
            -- X axis
            ( T.elem Line
                [ F.x1 $ F.num 20.0
                , F.y1 $ F.num 180.0
                , F.x2 $ F.num 180.0
                , F.y2 $ F.num 180.0
                , F.stroke $ F.hex "#333"
                , F.strokeWidth $ F.num 1.0
                ]
            )
          `T.withChild`
            -- Y axis
            ( T.elem Line
                [ F.x1 $ F.num 20.0
                , F.y1 $ F.num 20.0
                , F.x2 $ F.num 20.0
                , F.y2 $ F.num 180.0
                , F.stroke $ F.hex "#333"
                , F.strokeWidth $ F.num 1.0
                ]
            )
    _ <- renderTree container tree
    pure unit

-- | Sample scatter plot data
scatterData :: Array { x :: Number, y :: Number, color :: String }
scatterData =
  [ { x: 20.0, y: 30.0, color: "#4A90E2" }
  , { x: 40.0, y: 80.0, color: "#4A90E2" }
  , { x: 60.0, y: 45.0, color: "#4A90E2" }
  , { x: 80.0, y: 120.0, color: "#4A90E2" }
  , { x: 100.0, y: 90.0, color: "#4A90E2" }
  , { x: 120.0, y: 140.0, color: "#4A90E2" }
  , { x: 140.0, y: 70.0, color: "#E24A4A" }
  , { x: 50.0, y: 50.0, color: "#E24A4A" }
  , { x: 90.0, y: 110.0, color: "#E24A4A" }
  , { x: 130.0, y: 100.0, color: "#E24A4A" }
  ]

-- =============================================================================
-- Bubble Visualization (Gapminder style)
-- =============================================================================

-- | Render a bubble chart
renderBubbleViz :: SampleDataId -> Effect Unit
renderBubbleViz _ = do
  runD3v2M do
    container <- select "#tree-builder3-output-svg" :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      tree :: T.Tree { x :: Number, y :: Number, r :: Number, color :: String, label :: String }
      tree =
        T.named SVG "outputSvg"
          [ F.width (F.num 200.0)
          , F.height (F.num 200.0)
          , F.viewBox 0.0 0.0 200.0 200.0
          ]
          `T.withChild`
            ( T.named Group "bubbles"
                [ F.transform $ F.text "translate(20, 20)" ]
                `T.withChild`
                  ( T.joinData "circles" "circle" bubbleData $ \pt ->
                      T.elem Circle
                        [ F.cx $ F.num pt.x
                        , F.cy $ F.num (160.0 - pt.y)
                        , F.r $ F.num pt.r
                        , F.fill $ F.text pt.color
                        , F.opacity $ F.num 0.6
                        , F.stroke $ F.text pt.color
                        , F.strokeWidth $ F.num 1.0
                        ]
                  )
            )
          `T.withChild`
            -- X axis
            ( T.elem Line
                [ F.x1 $ F.num 20.0
                , F.y1 $ F.num 180.0
                , F.x2 $ F.num 180.0
                , F.y2 $ F.num 180.0
                , F.stroke $ F.hex "#333"
                , F.strokeWidth $ F.num 1.0
                ]
            )
          `T.withChild`
            -- Y axis
            ( T.elem Line
                [ F.x1 $ F.num 20.0
                , F.y1 $ F.num 20.0
                , F.x2 $ F.num 20.0
                , F.y2 $ F.num 180.0
                , F.stroke $ F.hex "#333"
                , F.strokeWidth $ F.num 1.0
                ]
            )
          `T.withChild`
            -- X label
            ( T.elem Text
                [ F.x $ F.num 100.0
                , F.y $ F.num 195.0
                , F.textAnchor $ F.text "middle"
                , F.fontSize $ F.text "10px"
                , F.fill $ F.hex "#666"
                , F.textContent $ F.text "GDP per capita"
                ]
            )
    _ <- renderTree container tree
    pure unit

-- | Sample bubble data (Gapminder-like)
bubbleData :: Array { x :: Number, y :: Number, r :: Number, color :: String, label :: String }
bubbleData =
  [ { x: 30.0, y: 40.0, r: 15.0, color: "#E24A4A", label: "China" }      -- Asia
  , { x: 100.0, y: 120.0, r: 20.0, color: "#4A90E2", label: "USA" }       -- Americas
  , { x: 80.0, y: 100.0, r: 12.0, color: "#4AE24A", label: "Germany" }    -- Europe
  , { x: 50.0, y: 60.0, r: 18.0, color: "#E24A4A", label: "India" }       -- Asia
  , { x: 120.0, y: 130.0, r: 8.0, color: "#4AE24A", label: "UK" }         -- Europe
  , { x: 90.0, y: 90.0, r: 10.0, color: "#4A90E2", label: "Brazil" }      -- Americas
  , { x: 40.0, y: 30.0, r: 6.0, color: "#E2E24A", label: "Nigeria" }      -- Africa
  ]

-- =============================================================================
-- Tree Visualization (Force Graph nodes)
-- =============================================================================

-- | Render a tree/node visualization
-- | Uses a combined data type to render both links and nodes
renderTreeOutputViz :: SampleDataId -> Effect Unit
renderTreeOutputViz _ = do
  runD3v2M do
    container <- select "#tree-builder3-output-svg" :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      tree :: T.Tree ForceGraphNode
      tree =
        T.named SVG "outputSvg"
          [ F.width (F.num 200.0)
          , F.height (F.num 200.0)
          , F.viewBox 0.0 0.0 200.0 200.0
          ]
          `T.withChild`
            -- Links (static, no data binding needed)
            ( T.named Group "links"
                []
                `T.withChildren`
                  (map renderLink nodeLinks)
            )
          `T.withChild`
            -- Nodes (with data binding)
            ( T.named Group "nodes"
                []
                `T.withChild`
                  ( T.joinData "nodeGroups" "g" forceGraphData $ \node ->
                      T.elem Group
                        [ F.transform $ F.text $ "translate(" <> show node.x <> "," <> show node.y <> ")" ]
                        `T.withChild`
                          ( T.elem Circle
                              [ F.cx $ F.num 0.0
                              , F.cy $ F.num 0.0
                              , F.r $ F.num 8.0
                              , F.fill $ F.text (groupColor node.group)
                              , F.stroke $ F.hex "#fff"
                              , F.strokeWidth $ F.num 1.5
                              ]
                          )
                        `T.withChild`
                          ( T.elem Text
                              [ F.x $ F.num 12.0
                              , F.y $ F.num 4.0
                              , F.fontSize $ F.text "10px"
                              , F.fill $ F.hex "#333"
                              , F.textContent $ F.text node.label
                              ]
                          )
                  )
            )
    _ <- renderTree container tree
    pure unit

-- | Render a single link as a static line element
renderLink :: { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number } -> T.Tree ForceGraphNode
renderLink link =
  T.elem Line
    [ F.x1 $ F.num link.x1
    , F.y1 $ F.num link.y1
    , F.x2 $ F.num link.x2
    , F.y2 $ F.num link.y2
    , F.stroke $ F.hex "#999"
    , F.strokeWidth $ F.num 1.0
    , F.opacity $ F.num 0.6
    ]

-- | Force graph node data type
type ForceGraphNode = { x :: Number, y :: Number, label :: String, group :: Int }

-- | Get color for force graph group
groupColor :: Int -> String
groupColor 0 = "#4A90E2"
groupColor 1 = "#E24A4A"
groupColor 2 = "#4AE24A"
groupColor _ = "#999"

-- | Sample force graph node data
forceGraphData :: Array ForceGraphNode
forceGraphData =
  [ { x: 100.0, y: 50.0, label: "A", group: 0 }
  , { x: 60.0, y: 100.0, label: "B", group: 0 }
  , { x: 140.0, y: 100.0, label: "C", group: 1 }
  , { x: 40.0, y: 150.0, label: "D", group: 0 }
  , { x: 100.0, y: 150.0, label: "E", group: 1 }
  , { x: 160.0, y: 150.0, label: "F", group: 2 }
  ]

-- | Links between nodes
nodeLinks :: Array { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }
nodeLinks =
  [ { x1: 100.0, y1: 50.0, x2: 60.0, y2: 100.0 }   -- A-B
  , { x1: 100.0, y1: 50.0, x2: 140.0, y2: 100.0 }  -- A-C
  , { x1: 60.0, y1: 100.0, x2: 40.0, y2: 150.0 }   -- B-D
  , { x1: 60.0, y1: 100.0, x2: 100.0, y2: 150.0 }  -- B-E
  , { x1: 140.0, y1: 100.0, x2: 100.0, y2: 150.0 } -- C-E
  , { x1: 140.0, y1: 100.0, x2: 160.0, y2: 150.0 } -- C-F
  ]

-- =============================================================================
-- GUP Visualization (Enter/Update/Exit)
-- =============================================================================

-- | Render GUP demo (enter/update/exit letters)
renderGupViz :: SampleDataId -> Effect Unit
renderGupViz _ = do
  runD3v2M do
    container <- select "#tree-builder3-output-svg" :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      tree :: T.Tree { letter :: String, x :: Number, phase :: String, color :: String }
      tree =
        T.named SVG "outputSvg"
          [ F.width (F.num 200.0)
          , F.height (F.num 200.0)
          , F.viewBox 0.0 0.0 200.0 200.0
          ]
          `T.withChild`
            -- Phase labels
            ( T.elem Text
                [ F.x $ F.num 100.0
                , F.y $ F.num 25.0
                , F.textAnchor $ F.text "middle"
                , F.fontSize $ F.text "10px"
                , F.fill $ F.hex "#4AE24A"
                , F.textContent $ F.text "← Enter"
                ]
            )
          `T.withChild`
            ( T.elem Text
                [ F.x $ F.num 100.0
                , F.y $ F.num 95.0
                , F.textAnchor $ F.text "middle"
                , F.fontSize $ F.text "10px"
                , F.fill $ F.hex "#4A90E2"
                , F.textContent $ F.text "← Update"
                ]
            )
          `T.withChild`
            ( T.elem Text
                [ F.x $ F.num 100.0
                , F.y $ F.num 165.0
                , F.textAnchor $ F.text "middle"
                , F.fontSize $ F.text "10px"
                , F.fill $ F.hex "#E24A4A"
                , F.textContent $ F.text "← Exit"
                ]
            )
          `T.withChild`
            ( T.named Group "letters"
                []
                `T.withChild`
                  ( T.joinData "letterGroups" "g" gupLetters $ \d ->
                      T.elem Group
                        [ F.transform $ F.text $ "translate(" <> show d.x <> "," <> show (phaseY d.phase) <> ")" ]
                        `T.withChild`
                          ( T.elem Text
                              [ F.x $ F.num 0.0
                              , F.y $ F.num 0.0
                              , F.textAnchor $ F.text "middle"
                              , F.fontSize $ F.text "24px"
                              , F.fontFamily $ F.text "monospace"
                              , F.fill $ F.text d.color
                              , F.textContent $ F.text d.letter
                              ]
                          )
                  )
            )
    _ <- renderTree container tree
    pure unit

-- | Get Y position for GUP phase
phaseY :: String -> Number
phaseY "enter" = 50.0
phaseY "update" = 120.0
phaseY "exit" = 190.0
phaseY _ = 120.0

-- | Sample GUP letter data
gupLetters :: Array { letter :: String, x :: Number, phase :: String, color :: String }
gupLetters =
  -- New letters (enter)
  [ { letter: "G", x: 30.0, phase: "enter", color: "#4AE24A" }
  , { letter: "H", x: 60.0, phase: "enter", color: "#4AE24A" }
  , { letter: "I", x: 90.0, phase: "enter", color: "#4AE24A" }
  -- Existing letters (update)
  , { letter: "D", x: 40.0, phase: "update", color: "#4A90E2" }
  , { letter: "E", x: 70.0, phase: "update", color: "#4A90E2" }
  , { letter: "F", x: 100.0, phase: "update", color: "#4A90E2" }
  -- Removed letters (exit)
  , { letter: "A", x: 50.0, phase: "exit", color: "#E24A4A" }
  , { letter: "B", x: 80.0, phase: "exit", color: "#E24A4A" }
  , { letter: "C", x: 110.0, phase: "exit", color: "#E24A4A" }
  ]


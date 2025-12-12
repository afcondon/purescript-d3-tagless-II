-- | Tree Builder App
-- |
-- | CodePen-style interactive tree builder with four columns:
-- | 1. Tree structure (AST)
-- | 2. Attributes of selected node
-- | 3. Editable data
-- | 4. Generated PureScript code
-- |
-- | Full-width visualization preview below.
module TreeBuilder.App
  ( component
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import TreeBuilder.Types (BuilderTree(..), BuilderNode, NodeId, AttributeChoice(..), AttributeBinding, SampleDatum, ElementOption, availableElements, attributeOptionsFor, emptyNode, sudokuSampleData, chessSampleData, goSampleData)
import TreeBuilder.Interpreter (renderPreview)
import PSD3v2.Interpreter.SemiQuine (treeToCode)

-- =============================================================================
-- Component Types
-- =============================================================================

type State =
  { tree :: Maybe BuilderTree
  , sampleData :: Array SampleDatum
  , selectedNodeId :: Maybe NodeId
  , nextNodeId :: NodeId
  , previewError :: Maybe String
  , currentTreeType :: String   -- "grid" | "radial" | "strip"
  , currentDataSet :: String    -- "sudoku" | "chess" | "go"
  }

data Action
  = Initialize
  | AddRootElement String
  | AddChildElement NodeId String
  | SelectNode NodeId
  | RemoveNode NodeId
  | ToggleNodeExpand NodeId
  | UpdateAttribute NodeId String AttributeChoice
  | AddAttribute NodeId String
  | RemoveAttribute NodeId String
  | SelectTreeType String    -- "grid" | "radial" | "strip"
  | SelectDataSet String     -- "sudoku" | "chess" | "go"
  | RefreshPreview

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: State
initialState =
  { tree: Nothing
  , sampleData: sudokuSampleData
  , selectedNodeId: Nothing
  , nextNodeId: 1
  , previewError: Nothing
  , currentTreeType: "grid"
  , currentDataSet: "sudoku"
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "tree-builder-page" ] ]
    [ -- Site Navigation
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Tree Builder"
        }

    -- Main layout: Left (editors + viz) | Right (code panel)
    , HH.div
        [ HP.classes [ HH.ClassName "tree-builder-layout" ] ]
        [ -- Left side: Tree/Data editors + visualization
          HH.div
            [ HP.classes [ HH.ClassName "tree-builder-left" ] ]
            [ -- Top: Tree and Data editors side by side
              HH.div
                [ HP.classes [ HH.ClassName "tree-builder-editors" ] ]
                [ renderTreeColumn state
                , renderDataColumn state
                ]

            -- Bottom: Large visualization with Sankey watermark
            , HH.div
                [ HP.classes [ HH.ClassName "tree-builder-viz" ] ]
                [ renderSankeyWatermark
                , HH.div
                    [ HP.id "tree-builder-preview"
                    , HP.classes [ HH.ClassName "viz-container" ]
                    ]
                    []
                ]
            ]

        -- Right side: Full-height code panel
        , renderCodePanel state
        ]
    ]

-- | Sankey watermark container - SVG rendered via FFI
renderSankeyWatermark :: forall m. H.ComponentHTML Action () m
renderSankeyWatermark =
  HH.div
    [ HP.classes [ HH.ClassName "sankey-watermark" ]
    , HP.id "sankey-watermark-container"
    ]
    []

-- =============================================================================
-- Column 1: Tree Structure
-- =============================================================================

renderTreeColumn :: forall m. State -> H.ComponentHTML Action () m
renderTreeColumn state =
  HH.div
    [ HP.classes [ HH.ClassName "tb-column" ] ]
    [ -- Header with tree type selector only
      HH.div
        [ HP.classes [ HH.ClassName "tb-column-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "tb-column-title" ] ] [ HH.text "Tree" ]
        , HH.div
            [ HP.classes [ HH.ClassName "preset-buttons" ] ]
            [ treeButton state "grid" "Grid"
            , treeButton state "radial" "Radial"
            , treeButton state "strip" "Strip"
            ]
        ]
    -- Content
    , HH.div
        [ HP.classes [ HH.ClassName "tb-column-content" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "tree-view" ] ]
            [ case state.tree of
                Nothing ->
                  HH.div
                    [ HP.classes [ HH.ClassName "empty-tree" ] ]
                    [ HH.text "Select a preset to start" ]
                Just tree ->
                  renderTreeNode state tree
            ]
        -- Element palette
        , HH.div
            [ HP.classes [ HH.ClassName "palette-buttons" ]
            , HP.style "margin-top: 8px; padding-top: 8px; border-top: 1px dashed #999;"
            ]
            (map (renderPaletteBtn state) availableElements)
        ]
    ]

treeButton :: forall m. State -> String -> String -> H.ComponentHTML Action () m
treeButton state treeId label =
  HH.button
    [ HP.classes $ [ HH.ClassName "preset-btn" ] <>
        if state.currentTreeType == treeId then [ HH.ClassName "preset-btn--active" ] else []
    , HE.onClick \_ -> SelectTreeType treeId
    ]
    [ HH.text label ]

dataButton :: forall m. State -> String -> String -> H.ComponentHTML Action () m
dataButton state dataId label =
  HH.button
    [ HP.classes $ [ HH.ClassName "preset-btn" ] <>
        if state.currentDataSet == dataId then [ HH.ClassName "preset-btn--active" ] else []
    , HE.onClick \_ -> SelectDataSet dataId
    ]
    [ HH.text label ]

renderPaletteBtn :: forall m. State -> ElementOption -> H.ComponentHTML Action () m
renderPaletteBtn state elem =
  let
    action = case state.selectedNodeId of
      Just nodeId -> AddChildElement nodeId elem.id
      Nothing -> AddRootElement elem.id
  in
    HH.button
      [ HP.classes [ HH.ClassName "palette-btn", HH.ClassName $ "palette-btn--" <> elem.category ]
      , HP.title elem.description
      , HE.onClick \_ -> action
      ]
      [ HH.text elem.label ]

-- | Render a tree node recursively with inline attribute editing
renderTreeNode :: forall m. State -> BuilderTree -> H.ComponentHTML Action () m
renderTreeNode state tree = case tree of
  BNode node children ->
    let isSelected = state.selectedNodeId == Just node.id
    in HH.div
      [ HP.classes [ HH.ClassName "tree-node" ] ]
      [ -- Node header
        HH.div
          [ HP.classes $
              [ HH.ClassName "node-header" ] <>
              if isSelected then [ HH.ClassName "node-header--selected" ] else []
          , HE.onClick \_ -> SelectNode node.id
          ]
          [ -- Expand/collapse
            if Array.length children > 0
              then HH.span
                [ HP.classes [ HH.ClassName "expand-toggle" ]
                , HE.onClick \_ -> ToggleNodeExpand node.id
                ]
                [ HH.text $ if node.expanded then "▼" else "▶" ]
              else HH.span [ HP.classes [ HH.ClassName "expand-placeholder" ] ] [ HH.text "•" ]
          -- Element type
          , HH.span
              [ HP.classes [ HH.ClassName "element-badge", HH.ClassName $ "element-badge--" <> node.elementType ] ]
              [ HH.text node.elementType ]
          -- Name if set
          , case node.name of
              Just n -> HH.span [ HP.classes [ HH.ClassName "node-name" ] ] [ HH.text n ]
              Nothing -> HH.text ""
          -- Remove
          , HH.button
              [ HP.classes [ HH.ClassName "remove-btn" ]
              , HE.onClick \_ -> RemoveNode node.id
              ]
              [ HH.text "×" ]
          ]
      -- Inline attributes (always shown when expanded)
      , if node.expanded
          then renderInlineAttrs node.id node.elementType node.attributes
          else HH.text ""
      -- Children
      , if node.expanded && Array.length children > 0
          then HH.div
            [ HP.classes [ HH.ClassName "node-children" ] ]
            (map (renderTreeNode state) children)
          else HH.text ""
      ]

  BDataJoin join ->
    let isSelected = state.selectedNodeId == Just join.id
    in HH.div
      [ HP.classes [ HH.ClassName "tree-node" ] ]
      [ HH.div
          [ HP.classes $
              [ HH.ClassName "node-header" ] <>
              if isSelected then [ HH.ClassName "node-header--selected" ] else []
          , HE.onClick \_ -> SelectNode join.id
          ]
          [ HH.span [ HP.classes [ HH.ClassName "join-badge" ] ] [ HH.text "join" ]
          , HH.span
              [ HP.classes [ HH.ClassName "element-badge", HH.ClassName $ "element-badge--" <> join.elementType ] ]
              [ HH.text join.elementType ]
          , HH.span [ HP.classes [ HH.ClassName "node-name" ] ] [ HH.text join.name ]
          ]
      -- Inline attributes for join template (always shown when expanded)
      , if join.expanded
          then renderInlineAttrs join.id join.template.elementType join.template.attributes
          else HH.text ""
      ]

-- | Render inline attributes with compact editing
renderInlineAttrs :: forall m. NodeId -> String -> Array AttributeBinding -> H.ComponentHTML Action () m
renderInlineAttrs nodeId elemType attrs =
  HH.div
    [ HP.classes [ HH.ClassName "inline-attrs" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "inline-attrs-list" ] ]
        (map (renderInlineAttr nodeId) attrs)
    , HH.div
        [ HP.classes [ HH.ClassName "inline-add-attr" ] ]
        (map (renderAddAttrBtn nodeId) (missingAttrs elemType attrs))
    ]

-- | Get attributes not yet added
missingAttrs :: String -> Array AttributeBinding -> Array { name :: String, label :: String }
missingAttrs elemType current =
  let currentNames = map _.attrName current
      available = attributeOptionsFor elemType
  in Array.filter (\opt -> not (Array.elem opt.name currentNames))
       (map (\o -> { name: o.name, label: o.label }) available)

-- | Button to add a missing attribute
renderAddAttrBtn :: forall m. NodeId -> { name :: String, label :: String } -> H.ComponentHTML Action () m
renderAddAttrBtn nodeId attr =
  HH.button
    [ HP.classes [ HH.ClassName "add-attr-btn" ]
    , HP.title $ "Add " <> attr.label
    , HE.onClick \_ -> AddAttribute nodeId attr.name
    ]
    [ HH.text $ "+" <> attr.label ]

-- | Render a single inline attribute with value selector
renderInlineAttr :: forall m. NodeId -> AttributeBinding -> H.ComponentHTML Action () m
renderInlineAttr nodeId binding =
  HH.div
    [ HP.classes [ HH.ClassName "inline-attr" ] ]
    [ HH.span [ HP.classes [ HH.ClassName "inline-attr-name" ] ] [ HH.text binding.attrName ]
    , HH.span [ HP.classes [ HH.ClassName "inline-attr-arrow" ] ] [ HH.text "←" ]
    , renderChoiceButtons nodeId binding
    , HH.button
        [ HP.classes [ HH.ClassName "inline-attr-remove" ]
        , HE.onClick \_ -> RemoveAttribute nodeId binding.attrName
        ]
        [ HH.text "×" ]
    ]

-- | Render choice as clickable buttons instead of dropdown
renderChoiceButtons :: forall m. NodeId -> AttributeBinding -> H.ComponentHTML Action () m
renderChoiceButtons nodeId binding =
  HH.span [ HP.classes [ HH.ClassName "choice-buttons" ] ]
    [ choiceBtn "x" (FromField "x")
    , choiceBtn "y" (FromField "y")
    , choiceBtn "w" (FromField "width")
    , choiceBtn "h" (FromField "height")
    , choiceBtn "r" (FromField "radius")
    , choiceBtn "color" (FromField "color")
    , choiceBtn "label" (FromField "label")
    , choiceBtn "val" (FromField "value")
    ]
  where
  choiceBtn label choice =
    HH.button
      [ HP.classes $
          [ HH.ClassName "choice-btn" ] <>
          if binding.choice == choice then [ HH.ClassName "choice-btn--active" ] else []
      , HE.onClick \_ -> UpdateAttribute nodeId binding.attrName choice
      ]
      [ HH.text label ]

-- Note: Attributes are now edited inline in tree nodes (renderInlineAttrs)

-- =============================================================================
-- Column 2: Data (with data selector)
-- =============================================================================

renderDataColumn :: forall m. State -> H.ComponentHTML Action () m
renderDataColumn state =
  HH.div
    [ HP.classes [ HH.ClassName "tb-column" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "tb-column-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "tb-column-title" ] ] [ HH.text "Data" ]
        , HH.div
            [ HP.classes [ HH.ClassName "preset-buttons" ] ]
            [ dataButton state "sudoku" "9×9"
            , dataButton state "chess" "8×8"
            , dataButton state "go" "19×19"
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "tb-column-content" ] ]
        [ HH.pre
            [ HP.classes [ HH.ClassName "data-json" ] ]
            [ HH.text $ formatSampleData state.sampleData ]
        ]
    ]

formatSampleData :: Array SampleDatum -> String
formatSampleData data_ =
  "[\n" <> Array.intercalate ",\n" (map formatDatum data_) <> "\n]"
  where
  formatDatum d =
    "  { x: " <> show d.x <>
    ", y: " <> show d.y <>
    ", w: " <> show d.width <>
    ", h: " <> show d.height <>
    "\n    color: \"" <> d.color <>
    "\", label: \"" <> d.label <> "\" }"

-- =============================================================================
-- Right Panel: Full-height Code
-- =============================================================================

renderCodePanel :: forall m. State -> H.ComponentHTML Action () m
renderCodePanel state =
  HH.div
    [ HP.classes [ HH.ClassName "code-panel" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "tb-column-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "tb-column-title" ] ] [ HH.text "PureScript" ] ]
    , HH.div
        [ HP.classes [ HH.ClassName "tb-column-content" ] ]
        [ HH.pre
            [ HP.classes [ HH.ClassName "code-view" ] ]
            [ HH.text $ case state.tree of
                Nothing -> "-- No tree defined"
                Just tree -> treeToCode tree
            ]
        ]
    ]

-- =============================================================================
-- Action Handler
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Render the Sankey watermark showing data flow architecture
    liftEffect $ renderSankeyWatermarkImpl "#sankey-watermark-container"
    -- Load initial tree and data
    handleAction (SelectTreeType "grid")
    handleAction (SelectDataSet "sudoku")

  AddRootElement elemType -> do
    state <- H.get
    let newNode = emptyNode state.nextNodeId elemType
    let newTree = BNode newNode []
    H.modify_ _ { tree = Just newTree, nextNodeId = state.nextNodeId + 1, selectedNodeId = Just newNode.id }
    handleAction RefreshPreview

  AddChildElement parentId elemType -> do
    state <- H.get
    let newNode = emptyNode state.nextNodeId elemType
    let updatedTree = addChildToNode parentId (BNode newNode []) <$> state.tree
    H.modify_ _ { tree = updatedTree, nextNodeId = state.nextNodeId + 1, selectedNodeId = Just newNode.id }
    handleAction RefreshPreview

  SelectNode nodeId -> do
    H.modify_ _ { selectedNodeId = Just nodeId }

  RemoveNode nodeId -> do
    state <- H.get
    let updatedTree = join (removeNodeById nodeId <$> state.tree)
    let newSelected = if state.selectedNodeId == Just nodeId then Nothing else state.selectedNodeId
    H.modify_ _ { tree = updatedTree, selectedNodeId = newSelected }
    handleAction RefreshPreview

  ToggleNodeExpand nodeId -> do
    state <- H.get
    let updatedTree = toggleExpand nodeId <$> state.tree
    H.modify_ _ { tree = updatedTree }

  UpdateAttribute nodeId attrName choice -> do
    state <- H.get
    let updatedTree = updateNodeAttribute nodeId attrName choice <$> state.tree
    H.modify_ _ { tree = updatedTree }
    handleAction RefreshPreview

  AddAttribute nodeId attrName -> do
    when (attrName /= "") do
      state <- H.get
      let newBinding = { attrName, choice: FromField "x" }
      let updatedTree = addNodeAttribute nodeId newBinding <$> state.tree
      H.modify_ _ { tree = updatedTree }
      handleAction RefreshPreview

  RemoveAttribute nodeId attrName -> do
    state <- H.get
    let updatedTree = removeNodeAttribute nodeId attrName <$> state.tree
    H.modify_ _ { tree = updatedTree }
    handleAction RefreshPreview

  SelectTreeType treeType -> do
    state <- H.get
    let tree = getTreeForType treeType
    H.modify_ _ { tree = Just tree, currentTreeType = treeType, selectedNodeId = Nothing }
    handleAction RefreshPreview

  SelectDataSet dataSet -> do
    let data_ = getDataSet dataSet
    H.modify_ _ { sampleData = data_, currentDataSet = dataSet }
    handleAction RefreshPreview

  RefreshPreview -> do
    state <- H.get
    case state.tree of
      Nothing -> pure unit
      Just tree -> do
        liftEffect $ clearPreviewContainer "#tree-builder-preview"
        liftEffect $ renderPreview "#tree-builder-preview" tree state.sampleData

-- | Clear preview container (FFI)
foreign import clearPreviewContainer :: String -> Effect Unit

-- | Render Sankey watermark SVG (FFI)
foreign import renderSankeyWatermarkImpl :: String -> Effect Unit

-- =============================================================================
-- Tree Manipulation Helpers
-- =============================================================================

findNode :: NodeId -> BuilderTree -> Maybe BuilderNode
findNode targetId tree = case tree of
  BNode node children ->
    if node.id == targetId
      then Just node
      else Array.findMap (findNode targetId) children
  BDataJoin join ->
    if join.id == targetId
      then Just join.template
      else Nothing

addChildToNode :: NodeId -> BuilderTree -> BuilderTree -> BuilderTree
addChildToNode parentId newChild tree = case tree of
  BNode node children ->
    if node.id == parentId
      then BNode node (Array.snoc children newChild)
      else BNode node (map (addChildToNode parentId newChild) children)
  other -> other

removeNodeById :: NodeId -> BuilderTree -> Maybe BuilderTree
removeNodeById targetId tree = case tree of
  BNode node children ->
    if node.id == targetId
      then Nothing
      else Just $ BNode node (Array.mapMaybe (removeNodeById targetId) children)
  other -> Just other

toggleExpand :: NodeId -> BuilderTree -> BuilderTree
toggleExpand targetId tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { expanded = not node.expanded }) children
      else BNode node (map (toggleExpand targetId) children)
  other -> other

updateNodeAttribute :: NodeId -> String -> AttributeChoice -> BuilderTree -> BuilderTree
updateNodeAttribute targetId attrName choice tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { attributes = updateAttr node.attributes }) children
      else BNode node (map (updateNodeAttribute targetId attrName choice) children)
  BDataJoin join ->
    if join.id == targetId
      then BDataJoin (join { template = join.template { attributes = updateAttr join.template.attributes } })
      else tree
  where
  updateAttr attrs = map (\a -> if a.attrName == attrName then a { choice = choice } else a) attrs

addNodeAttribute :: NodeId -> AttributeBinding -> BuilderTree -> BuilderTree
addNodeAttribute targetId binding tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { attributes = Array.snoc node.attributes binding }) children
      else BNode node (map (addNodeAttribute targetId binding) children)
  BDataJoin join ->
    if join.id == targetId
      then BDataJoin (join { template = join.template { attributes = Array.snoc join.template.attributes binding } })
      else tree

removeNodeAttribute :: NodeId -> String -> BuilderTree -> BuilderTree
removeNodeAttribute targetId attrName tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { attributes = Array.filter (\a -> a.attrName /= attrName) node.attributes }) children
      else BNode node (map (removeNodeAttribute targetId attrName) children)
  BDataJoin join ->
    if join.id == targetId
      then BDataJoin (join { template = join.template { attributes = Array.filter (\a -> a.attrName /= attrName) join.template.attributes } })
      else tree

-- =============================================================================
-- Tree Types and Data Sets
-- =============================================================================

-- | Get tree structure for a tree type
getTreeForType :: String -> BuilderTree
getTreeForType = case _ of
  "grid" -> gridTree
  "radial" -> radialTree
  "strip" -> stripTree
  _ -> gridTree

-- | Get data set by name
getDataSet :: String -> Array SampleDatum
getDataSet = case _ of
  "sudoku" -> sudokuSampleData
  "chess" -> chessSampleData
  "go" -> goSampleData
  _ -> sudokuSampleData

-- | Grid tree: renders data as square grid using x, y coordinates
-- | Rects for cells + text for labels
gridTree :: BuilderTree
gridTree = BNode
  { id: 1
  , elementType: "svg"
  , name: Just "grid"
  , attributes:
      [ { attrName: "width", choice: ConstantNumber 300.0 }
      , { attrName: "height", choice: ConstantNumber 300.0 }
      , { attrName: "viewBox", choice: ConstantString "0 0 300 300" }
      ]
  , expanded: true
  }
  [ -- Cells (join) - rects positioned by x, y
    BDataJoin
      { id: 2
      , name: "cells"
      , elementType: "rect"
      , template:
          { id: 3
          , elementType: "rect"
          , name: Nothing
          , attributes:
              [ { attrName: "x", choice: FromField "x" }
              , { attrName: "y", choice: FromField "y" }
              , { attrName: "width", choice: FromField "width" }
              , { attrName: "height", choice: FromField "height" }
              , { attrName: "fill", choice: FromField "color" }
              ]
          , expanded: true
          }
      , expanded: true
      }
  , -- Labels (join) - text centered in cells
    BDataJoin
      { id: 4
      , name: "labels"
      , elementType: "text"
      , template:
          { id: 5
          , elementType: "text"
          , name: Nothing
          , attributes:
              [ { attrName: "x", choice: FromField "cx" }
              , { attrName: "y", choice: FromField "cy" }
              , { attrName: "text", choice: FromField "label" }
              , { attrName: "fill", choice: ConstantString "#333" }
              , { attrName: "font-size", choice: ConstantNumber 16.0 }
              , { attrName: "text-anchor", choice: ConstantString "middle" }
              ]
          , expanded: true
          }
      , expanded: true
      }
  ]

-- | Radial tree: renders data as polar arrangement using rx, ry coordinates
-- | Circles positioned in concentric rings
radialTree :: BuilderTree
radialTree = BNode
  { id: 1
  , elementType: "svg"
  , name: Just "radial"
  , attributes:
      [ { attrName: "width", choice: ConstantNumber 300.0 }
      , { attrName: "height", choice: ConstantNumber 300.0 }
      , { attrName: "viewBox", choice: ConstantString "0 0 300 300" }
      ]
  , expanded: true
  }
  [ -- Dots (join) - circles positioned by rx, ry (polar coordinates)
    BDataJoin
      { id: 2
      , name: "dots"
      , elementType: "circle"
      , template:
          { id: 3
          , elementType: "circle"
          , name: Nothing
          , attributes:
              [ { attrName: "cx", choice: FromField "rx" }
              , { attrName: "cy", choice: FromField "ry" }
              , { attrName: "r", choice: FromField "radius" }
              , { attrName: "fill", choice: FromField "color" }
              , { attrName: "stroke", choice: ConstantString "#333" }
              , { attrName: "stroke-width", choice: ConstantNumber 0.5 }
              ]
          , expanded: true
          }
      , expanded: true
      }
  , -- Labels (join) - text at radial positions
    BDataJoin
      { id: 4
      , name: "labels"
      , elementType: "text"
      , template:
          { id: 5
          , elementType: "text"
          , name: Nothing
          , attributes:
              [ { attrName: "x", choice: FromField "rx" }
              , { attrName: "y", choice: FromField "ry" }
              , { attrName: "text", choice: FromField "label" }
              , { attrName: "fill", choice: ConstantString "#333" }
              , { attrName: "font-size", choice: ConstantNumber 8.0 }
              , { attrName: "text-anchor", choice: ConstantString "middle" }
              ]
          , expanded: true
          }
      , expanded: true
      }
  ]

-- | Strip tree: renders data as horizontal strip using sx, sy coordinates
-- | All cells in one long row - like a barcode or DNA sequence
stripTree :: BuilderTree
stripTree = BNode
  { id: 1
  , elementType: "svg"
  , name: Just "strip"
  , attributes:
      [ { attrName: "width", choice: ConstantNumber 300.0 }
      , { attrName: "height", choice: ConstantNumber 60.0 }
      , { attrName: "viewBox", choice: ConstantString "0 0 300 60" }
      ]
  , expanded: true
  }
  [ -- Bars (join) - thin vertical bars
    BDataJoin
      { id: 2
      , name: "bars"
      , elementType: "rect"
      , template:
          { id: 3
          , elementType: "rect"
          , name: Nothing
          , attributes:
              [ { attrName: "x", choice: FromField "sx" }
              , { attrName: "y", choice: ConstantNumber 5.0 }
              , { attrName: "width", choice: ConstantNumber 3.0 }
              , { attrName: "height", choice: ConstantNumber 50.0 }
              , { attrName: "fill", choice: FromField "color" }
              ]
          , expanded: true
          }
      , expanded: true
      }
  ]

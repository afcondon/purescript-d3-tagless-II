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
import TreeBuilder.Types (BuilderTree(..), BuilderNode, NodeId, AttributeChoice(..), AttributeBinding, SampleDatum, ElementOption, availableElements, attributeOptionsFor, emptyNode, sudokuSampleData)
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
  , currentPreset :: String
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
  | LoadPreset String
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
  , currentPreset: "sudoku"
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

    -- Main layout
    , HH.div
        [ HP.classes [ HH.ClassName "tree-builder-layout" ] ]
        [ -- Top: 4 columns
          HH.div
            [ HP.classes [ HH.ClassName "tree-builder-columns" ] ]
            [ renderTreeColumn state
            , renderAttributesColumn state
            , renderDataColumn state
            , renderCodeColumn state
            ]

        -- Bottom: Visualization
        , HH.div
            [ HP.classes [ HH.ClassName "tree-builder-viz" ] ]
            [ HH.div
                [ HP.id "tree-builder-preview"
                , HP.classes [ HH.ClassName "viz-container" ]
                ]
                []
            ]
        ]
    ]

-- =============================================================================
-- Column 1: Tree Structure
-- =============================================================================

renderTreeColumn :: forall m. State -> H.ComponentHTML Action () m
renderTreeColumn state =
  HH.div
    [ HP.classes [ HH.ClassName "tb-column" ] ]
    [ -- Header with presets
      HH.div
        [ HP.classes [ HH.ClassName "tb-column-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "tb-column-title" ] ] [ HH.text "Tree" ]
        , HH.div
            [ HP.classes [ HH.ClassName "preset-buttons" ] ]
            [ presetButton state "sudoku" "Sudoku"
            , presetButton state "scatter" "Scatter"
            , presetButton state "bars" "Bars"
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

presetButton :: forall m. State -> String -> String -> H.ComponentHTML Action () m
presetButton state presetId label =
  HH.button
    [ HP.classes $ [ HH.ClassName "preset-btn" ] <>
        if state.currentPreset == presetId then [ HH.ClassName "preset-btn--active" ] else []
    , HE.onClick \_ -> LoadPreset presetId
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

-- | Render a tree node recursively
renderTreeNode :: forall m. State -> BuilderTree -> H.ComponentHTML Action () m
renderTreeNode state tree = case tree of
  BNode node children ->
    HH.div
      [ HP.classes [ HH.ClassName "tree-node" ] ]
      [ -- Node header
        HH.div
          [ HP.classes $
              [ HH.ClassName "node-header" ] <>
              if state.selectedNodeId == Just node.id
                then [ HH.ClassName "node-header--selected" ]
                else []
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
          -- Attr count
          , HH.span
              [ HP.classes [ HH.ClassName "attr-count" ] ]
              [ HH.text $ show (Array.length node.attributes) ]
          -- Remove
          , HH.button
              [ HP.classes [ HH.ClassName "remove-btn" ]
              , HE.onClick \_ -> RemoveNode node.id
              ]
              [ HH.text "×" ]
          ]
      -- Children
      , if node.expanded && Array.length children > 0
          then HH.div
            [ HP.classes [ HH.ClassName "node-children" ] ]
            (map (renderTreeNode state) children)
          else HH.text ""
      ]

  BDataJoin join ->
    HH.div
      [ HP.classes [ HH.ClassName "tree-node" ] ]
      [ HH.div
          [ HP.classes $
              [ HH.ClassName "node-header" ] <>
              if state.selectedNodeId == Just join.id
                then [ HH.ClassName "node-header--selected" ]
                else []
          , HE.onClick \_ -> SelectNode join.id
          ]
          [ HH.span [ HP.classes [ HH.ClassName "join-badge" ] ] [ HH.text "join" ]
          , HH.span
              [ HP.classes [ HH.ClassName "element-badge", HH.ClassName $ "element-badge--" <> join.elementType ] ]
              [ HH.text join.elementType ]
          , HH.span [ HP.classes [ HH.ClassName "node-name" ] ] [ HH.text join.name ]
          , HH.span
              [ HP.classes [ HH.ClassName "attr-count" ] ]
              [ HH.text $ show (Array.length join.template.attributes) ]
          ]
      ]

-- =============================================================================
-- Column 2: Attributes
-- =============================================================================

renderAttributesColumn :: forall m. State -> H.ComponentHTML Action () m
renderAttributesColumn state =
  HH.div
    [ HP.classes [ HH.ClassName "tb-column" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "tb-column-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "tb-column-title" ] ] [ HH.text "Attributes" ] ]
    , HH.div
        [ HP.classes [ HH.ClassName "tb-column-content" ] ]
        [ case state.selectedNodeId of
            Nothing ->
              HH.div
                [ HP.classes [ HH.ClassName "attr-editor-empty" ] ]
                [ HH.text "Select a node to edit attributes" ]
            Just nodeId ->
              renderAttributeEditor state nodeId
        ]
    ]

renderAttributeEditor :: forall m. State -> NodeId -> H.ComponentHTML Action () m
renderAttributeEditor state nodeId =
  let
    mNode = findNode nodeId =<< state.tree
  in case mNode of
    Nothing -> HH.text ""
    Just node ->
      HH.div_
        [ HH.div
            [ HP.classes [ HH.ClassName "attr-editor-header" ] ]
            [ HH.text $ node.elementType <> " attributes" ]
        , HH.div
            [ HP.classes [ HH.ClassName "attr-list" ] ]
            (map (renderAttributeRow nodeId) node.attributes)
        , HH.div
            [ HP.classes [ HH.ClassName "add-attr-row" ] ]
            [ HH.select
                [ HP.classes [ HH.ClassName "add-attr-select" ]
                , HE.onValueChange \v -> AddAttribute nodeId v
                ]
                ([ HH.option [ HP.value "" ] [ HH.text "+ add attribute" ] ] <>
                  map (\opt -> HH.option [ HP.value opt.name ] [ HH.text opt.label ])
                    (attributeOptionsFor node.elementType))
            ]
        ]

renderAttributeRow :: forall m. NodeId -> AttributeBinding -> H.ComponentHTML Action () m
renderAttributeRow nodeId binding =
  HH.div
    [ HP.classes [ HH.ClassName "attr-row" ] ]
    [ HH.span [ HP.classes [ HH.ClassName "attr-name" ] ] [ HH.text binding.attrName ]
    , HH.span [ HP.classes [ HH.ClassName "attr-arrow" ] ] [ HH.text "←" ]
    , HH.select
        [ HP.classes [ HH.ClassName "attr-choice" ]
        , HE.onValueChange \v -> UpdateAttribute nodeId binding.attrName (parseChoice v)
        ]
        [ HH.option [ HP.value "x", HP.selected (binding.choice == FromField "x") ] [ HH.text "_.x" ]
        , HH.option [ HP.value "y", HP.selected (binding.choice == FromField "y") ] [ HH.text "_.y" ]
        , HH.option [ HP.value "radius", HP.selected (binding.choice == FromField "radius") ] [ HH.text "_.radius" ]
        , HH.option [ HP.value "width", HP.selected (binding.choice == FromField "width") ] [ HH.text "_.width" ]
        , HH.option [ HP.value "height", HP.selected (binding.choice == FromField "height") ] [ HH.text "_.height" ]
        , HH.option [ HP.value "color", HP.selected (binding.choice == FromField "color") ] [ HH.text "_.color" ]
        , HH.option [ HP.value "label", HP.selected (binding.choice == FromField "label") ] [ HH.text "_.label" ]
        , HH.option [ HP.value "value", HP.selected (binding.choice == FromField "value") ] [ HH.text "_.value" ]
        , HH.option [ HP.value "index", HP.selected (binding.choice == IndexBased) ] [ HH.text "_.index" ]
        ]
    , HH.button
        [ HP.classes [ HH.ClassName "remove-attr-btn" ]
        , HE.onClick \_ -> RemoveAttribute nodeId binding.attrName
        ]
        [ HH.text "×" ]
    ]

parseChoice :: String -> AttributeChoice
parseChoice = case _ of
  "index" -> IndexBased
  field -> FromField field

-- =============================================================================
-- Column 3: Data
-- =============================================================================

renderDataColumn :: forall m. State -> H.ComponentHTML Action () m
renderDataColumn state =
  HH.div
    [ HP.classes [ HH.ClassName "tb-column" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "tb-column-header" ] ]
        [ HH.h3 [ HP.classes [ HH.ClassName "tb-column-title" ] ] [ HH.text "Data" ]
        , HH.span
            [ HP.style "font-size: 9px; color: #666;" ]
            [ HH.text $ show (Array.length state.sampleData) <> " items" ]
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
-- Column 4: Code
-- =============================================================================

renderCodeColumn :: forall m. State -> H.ComponentHTML Action () m
renderCodeColumn state =
  HH.div
    [ HP.classes [ HH.ClassName "tb-column" ] ]
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
    handleAction (LoadPreset "sudoku")

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

  LoadPreset presetName -> do
    let preset = getPreset presetName
    H.modify_ _ { tree = Just preset.tree, sampleData = preset.data_, nextNodeId = preset.nextId, selectedNodeId = Nothing, currentPreset = presetName }
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
-- Presets
-- =============================================================================

type Preset = { tree :: BuilderTree, data_ :: Array SampleDatum, nextId :: NodeId }

getPreset :: String -> Preset
getPreset = case _ of
  "sudoku" -> sudokuPreset
  "scatter" -> scatterPreset
  "bars" -> barsPreset
  _ -> sudokuPreset

-- | Sudoku preset: SVG with grid cells (rects) and number labels (text)
sudokuPreset :: Preset
sudokuPreset =
  { tree: BNode
      { id: 1
      , elementType: "svg"
      , name: Just "sudoku"
      , attributes:
          [ { attrName: "width", choice: ConstantNumber 280.0 }
          , { attrName: "height", choice: ConstantNumber 280.0 }
          ]
      , expanded: true
      }
      [ -- Background
        BNode
          { id: 2
          , elementType: "rect"
          , name: Just "bg"
          , attributes:
              [ { attrName: "x", choice: ConstantNumber 0.0 }
              , { attrName: "y", choice: ConstantNumber 0.0 }
              , { attrName: "width", choice: ConstantNumber 280.0 }
              , { attrName: "height", choice: ConstantNumber 280.0 }
              , { attrName: "fill", choice: ConstantString "#333" }
              ]
          , expanded: true
          }
          []
      , -- Cells (join)
        BDataJoin
          { id: 3
          , name: "cells"
          , elementType: "rect"
          , template:
              { id: 4
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
      , -- Labels (join)
        BDataJoin
          { id: 5
          , name: "labels"
          , elementType: "text"
          , template:
              { id: 6
              , elementType: "text"
              , name: Nothing
              , attributes:
                  [ { attrName: "x", choice: Computed "d.x + 40.0" }
                  , { attrName: "y", choice: Computed "d.y + 52.0" }
                  , { attrName: "text", choice: FromField "label" }
                  , { attrName: "fill", choice: ConstantString "#333" }
                  , { attrName: "font-size", choice: ConstantNumber 32.0 }
                  , { attrName: "text-anchor", choice: ConstantString "middle" }
                  ]
              , expanded: true
              }
          , expanded: true
          }
      ]
  , data_: sudokuSampleData
  , nextId: 10
  }

scatterPreset :: Preset
scatterPreset =
  { tree: BNode
      { id: 1
      , elementType: "svg"
      , name: Just "scatter"
      , attributes:
          [ { attrName: "width", choice: ConstantNumber 200.0 }
          , { attrName: "height", choice: ConstantNumber 150.0 }
          ]
      , expanded: true
      }
      [ BDataJoin
          { id: 2
          , name: "points"
          , elementType: "circle"
          , template:
              { id: 3
              , elementType: "circle"
              , name: Nothing
              , attributes:
                  [ { attrName: "cx", choice: FromField "x" }
                  , { attrName: "cy", choice: FromField "y" }
                  , { attrName: "r", choice: FromField "radius" }
                  , { attrName: "fill", choice: FromField "color" }
                  ]
              , expanded: true
              }
          , expanded: true
          }
      ]
  , data_:
      [ { x: 30.0, y: 40.0, radius: 12.0, width: 0.0, height: 0.0, color: "#E63946", label: "A", name: "A", value: 10.0, index: 0 }
      , { x: 80.0, y: 100.0, radius: 18.0, width: 0.0, height: 0.0, color: "#5A8A8A", label: "B", name: "B", value: 25.0, index: 1 }
      , { x: 140.0, y: 60.0, radius: 14.0, width: 0.0, height: 0.0, color: "#D4C9A8", label: "C", name: "C", value: 15.0, index: 2 }
      , { x: 170.0, y: 120.0, radius: 10.0, width: 0.0, height: 0.0, color: "#E63946", label: "D", name: "D", value: 30.0, index: 3 }
      ]
  , nextId: 10
  }

barsPreset :: Preset
barsPreset =
  { tree: BNode
      { id: 1
      , elementType: "svg"
      , name: Just "bars"
      , attributes:
          [ { attrName: "width", choice: ConstantNumber 200.0 }
          , { attrName: "height", choice: ConstantNumber 150.0 }
          ]
      , expanded: true
      }
      [ BDataJoin
          { id: 2
          , name: "bars"
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
      ]
  , data_:
      [ { x: 10.0, y: 80.0, radius: 0.0, width: 35.0, height: 60.0, color: "#E63946", label: "A", name: "A", value: 60.0, index: 0 }
      , { x: 55.0, y: 40.0, radius: 0.0, width: 35.0, height: 100.0, color: "#5A8A8A", label: "B", name: "B", value: 100.0, index: 1 }
      , { x: 100.0, y: 100.0, radius: 0.0, width: 35.0, height: 40.0, color: "#D4C9A8", label: "C", name: "C", value: 40.0, index: 2 }
      , { x: 145.0, y: 60.0, radius: 0.0, width: 35.0, height: 80.0, color: "#E63946", label: "D", name: "D", value: 80.0, index: 3 }
      ]
  , nextId: 10
  }

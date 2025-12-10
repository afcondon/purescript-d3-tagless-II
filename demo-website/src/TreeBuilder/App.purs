-- | Tree Builder App
-- |
-- | Main Halogen component for the Interactive Tree Builder.
-- | Provides a visual, interactive tool to build Tree API structures.
module TreeBuilder.App
  ( component
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import TreeBuilder.Types
import TreeBuilder.Interpreter (renderPreview)

-- =============================================================================
-- Component Types
-- =============================================================================

type State =
  { tree :: Maybe BuilderTree
  , sampleData :: Array SampleDatum
  , selectedNodeId :: Maybe NodeId
  , nextNodeId :: NodeId
  , previewError :: Maybe String
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
  | UpdateSampleData (Array SampleDatum)
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
  , sampleData: defaultSampleData
  , selectedNodeId: Nothing
  , nextNodeId: 1
  , previewError: Nothing
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

    -- Main layout: Tree Editor + Preview
    , HH.div
        [ HP.classes [ HH.ClassName "tree-builder-layout" ] ]
        [ -- Left panel: Tree editor
          HH.div
            [ HP.classes [ HH.ClassName "tree-builder-editor" ] ]
            [ renderEditorPanel state ]

        -- Right panel: Preview
        , HH.div
            [ HP.classes [ HH.ClassName "tree-builder-preview" ] ]
            [ renderPreviewPanel state ]
        ]

    -- Floating element palette (bottom)
    , renderElementPalette state
    ]

-- | Editor panel with tree structure
renderEditorPanel :: forall m. State -> H.ComponentHTML Action () m
renderEditorPanel state =
  HH.div
    [ HP.classes [ HH.ClassName "editor-panel" ] ]
    [ HH.h2
        [ HP.classes [ HH.ClassName "panel-title" ] ]
        [ HH.text "Tree Structure" ]

    -- Presets
    , HH.div
        [ HP.classes [ HH.ClassName "presets-row" ] ]
        [ HH.span [ HP.classes [ HH.ClassName "presets-label" ] ] [ HH.text "Presets:" ]
        , HH.button
            [ HP.classes [ HH.ClassName "preset-button" ]
            , HE.onClick \_ -> LoadPreset "circles"
            ]
            [ HH.text "Circles" ]
        , HH.button
            [ HP.classes [ HH.ClassName "preset-button" ]
            , HE.onClick \_ -> LoadPreset "bars"
            ]
            [ HH.text "Bar Chart" ]
        , HH.button
            [ HP.classes [ HH.ClassName "preset-button" ]
            , HE.onClick \_ -> LoadPreset "scatter"
            ]
            [ HH.text "Scatter" ]
        ]

    -- Tree view
    , HH.div
        [ HP.classes [ HH.ClassName "tree-view" ] ]
        [ case state.tree of
            Nothing ->
              HH.div
                [ HP.classes [ HH.ClassName "empty-tree" ] ]
                [ HH.p_ [ HH.text "No tree yet. Add an element from the palette below." ]
                , HH.p_ [ HH.text "Or load a preset to get started." ]
                ]
            Just tree ->
              renderTreeNode state tree 0
        ]

    -- Selected node attributes
    , case state.selectedNodeId of
        Nothing -> HH.text ""
        Just nodeId ->
          renderAttributeEditor state nodeId
    ]

-- | Render a tree node recursively
renderTreeNode :: forall m. State -> BuilderTree -> Int -> H.ComponentHTML Action () m
renderTreeNode state tree depth = case tree of
  BNode node children ->
    HH.div
      [ HP.classes [ HH.ClassName "tree-node" ]
      , HP.style $ "margin-left: " <> show (depth * 20) <> "px"
      ]
      [ -- Node header
        HH.div
          [ HP.classes $
              [ HH.ClassName "node-header" ] <>
              if state.selectedNodeId == Just node.id
                then [ HH.ClassName "node-header--selected" ]
                else []
          , HE.onClick \_ -> SelectNode node.id
          ]
          [ -- Expand/collapse toggle
            if Array.length children > 0
              then HH.span
                [ HP.classes [ HH.ClassName "expand-toggle" ]
                , HE.onClick \_ -> ToggleNodeExpand node.id
                ]
                [ HH.text $ if node.expanded then "▼" else "▶" ]
              else HH.span [ HP.classes [ HH.ClassName "expand-placeholder" ] ] [ HH.text "•" ]
          -- Element type badge
          , HH.span
              [ HP.classes [ HH.ClassName "element-badge", HH.ClassName $ "element-badge--" <> node.elementType ] ]
              [ HH.text node.elementType ]
          -- Node name (if set)
          , case node.name of
              Just n -> HH.span [ HP.classes [ HH.ClassName "node-name" ] ] [ HH.text $ "\"" <> n <> "\"" ]
              Nothing -> HH.text ""
          -- Attribute count
          , HH.span
              [ HP.classes [ HH.ClassName "attr-count" ] ]
              [ HH.text $ "(" <> show (Array.length node.attributes) <> " attrs)" ]
          -- Remove button
          , HH.button
              [ HP.classes [ HH.ClassName "remove-btn" ]
              , HE.onClick \_ -> RemoveNode node.id
              ]
              [ HH.text "×" ]
          ]
      -- Children (if expanded)
      , if node.expanded
          then HH.div
            [ HP.classes [ HH.ClassName "node-children" ] ]
            (map (\child -> renderTreeNode state child (depth + 1)) children)
          else HH.text ""
      ]

  BDataJoin join ->
    HH.div
      [ HP.classes [ HH.ClassName "tree-node", HH.ClassName "tree-node--join" ]
      , HP.style $ "margin-left: " <> show (depth * 20) <> "px"
      ]
      [ HH.div
          [ HP.classes [ HH.ClassName "node-header" ]
          , HE.onClick \_ -> SelectNode join.id
          ]
          [ HH.span [ HP.classes [ HH.ClassName "join-badge" ] ] [ HH.text "⟳ join" ]
          , HH.span [ HP.classes [ HH.ClassName "element-badge" ] ] [ HH.text join.elementType ]
          , HH.span [ HP.classes [ HH.ClassName "node-name" ] ] [ HH.text $ "\"" <> join.name <> "\"" ]
          ]
      ]

-- | Attribute editor for selected node
renderAttributeEditor :: forall m. State -> NodeId -> H.ComponentHTML Action () m
renderAttributeEditor state nodeId =
  let
    mNode = findNode nodeId =<< state.tree
  in case mNode of
    Nothing -> HH.text ""
    Just node ->
      HH.div
        [ HP.classes [ HH.ClassName "attribute-editor" ] ]
        [ HH.h3_ [ HH.text $ "Attributes: " <> node.elementType ]
        , HH.div
            [ HP.classes [ HH.ClassName "attr-list" ] ]
            (map (renderAttributeRow nodeId) node.attributes)
        -- Add attribute button
        , HH.div
            [ HP.classes [ HH.ClassName "add-attr-row" ] ]
            [ HH.select
                [ HP.classes [ HH.ClassName "add-attr-select" ]
                , HE.onValueChange \v -> AddAttribute nodeId v
                ]
                ([ HH.option [ HP.value "" ] [ HH.text "Add attribute..." ] ] <>
                  map (\opt -> HH.option [ HP.value opt.name ] [ HH.text opt.label ])
                    (attributeOptionsFor node.elementType))
            ]
        ]

-- | Render a single attribute row
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

-- | Parse a choice value from dropdown
parseChoice :: String -> AttributeChoice
parseChoice = case _ of
  "index" -> IndexBased
  field -> FromField field

-- | Preview panel
renderPreviewPanel :: forall m. State -> H.ComponentHTML Action () m
renderPreviewPanel state =
  HH.div
    [ HP.classes [ HH.ClassName "preview-panel" ] ]
    [ HH.h2
        [ HP.classes [ HH.ClassName "panel-title" ] ]
        [ HH.text "Live Preview" ]
    , HH.div
        [ HP.id "tree-builder-preview"
        , HP.classes [ HH.ClassName "preview-container" ] ]
        []
    -- Sample data display
    , HH.div
        [ HP.classes [ HH.ClassName "sample-data-panel" ] ]
        [ HH.h3_ [ HH.text "Sample Data" ]
        , HH.pre
            [ HP.classes [ HH.ClassName "sample-data-json" ] ]
            [ HH.text $ formatSampleData state.sampleData ]
        ]
    -- Error display
    , case state.previewError of
        Just err ->
          HH.div
            [ HP.classes [ HH.ClassName "preview-error" ] ]
            [ HH.text err ]
        Nothing -> HH.text ""
    ]

-- | Element palette at bottom
renderElementPalette :: forall m. State -> H.ComponentHTML Action () m
renderElementPalette state =
  HH.div
    [ HP.classes [ HH.ClassName "element-palette" ] ]
    [ HH.span [ HP.classes [ HH.ClassName "palette-label" ] ] [ HH.text "Add Element:" ]
    , HH.div
        [ HP.classes [ HH.ClassName "palette-buttons" ] ]
        (map (renderPaletteButton state) availableElements)
    ]

-- | Render a palette button
renderPaletteButton :: forall m. State -> ElementOption -> H.ComponentHTML Action () m
renderPaletteButton state elem =
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

-- | Format sample data as JSON-like string
formatSampleData :: Array SampleDatum -> String
formatSampleData data_ =
  "[\n" <> Array.intercalate ",\n" (map formatDatum data_) <> "\n]"
  where
  formatDatum d = "  { x: " <> show d.x <> ", y: " <> show d.y <>
    ", radius: " <> show d.radius <> ", color: \"" <> d.color <>
    "\", label: \"" <> d.label <> "\" }"

-- =============================================================================
-- Action Handler
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load a default preset
    handleAction (LoadPreset "circles")

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
    -- removeNodeById returns Maybe, and state.tree is Maybe, so we need join
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

  UpdateSampleData newData -> do
    H.modify_ _ { sampleData = newData }
    handleAction RefreshPreview

  LoadPreset presetName -> do
    let preset = getPreset presetName
    H.modify_ _ { tree = Just preset.tree, sampleData = preset.data_, nextNodeId = preset.nextId, selectedNodeId = Nothing }
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

-- | Find a node by ID
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

-- | Add a child to a specific node
addChildToNode :: NodeId -> BuilderTree -> BuilderTree -> BuilderTree
addChildToNode parentId newChild tree = case tree of
  BNode node children ->
    if node.id == parentId
      then BNode node (Array.snoc children newChild)
      else BNode node (map (addChildToNode parentId newChild) children)
  other -> other

-- | Remove a node by ID
removeNodeById :: NodeId -> BuilderTree -> Maybe BuilderTree
removeNodeById targetId tree = case tree of
  BNode node children ->
    if node.id == targetId
      then Nothing
      else Just $ BNode node (Array.mapMaybe (removeNodeById targetId) children)
  other -> Just other

-- | Toggle expanded state for a node
toggleExpand :: NodeId -> BuilderTree -> BuilderTree
toggleExpand targetId tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { expanded = not node.expanded }) children
      else BNode node (map (toggleExpand targetId) children)
  other -> other

-- | Update an attribute on a node
updateNodeAttribute :: NodeId -> String -> AttributeChoice -> BuilderTree -> BuilderTree
updateNodeAttribute targetId attrName choice tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { attributes = updateAttr node.attributes }) children
      else BNode node (map (updateNodeAttribute targetId attrName choice) children)
  other -> other
  where
  updateAttr attrs = map (\a -> if a.attrName == attrName then a { choice = choice } else a) attrs

-- | Add an attribute to a node
addNodeAttribute :: NodeId -> AttributeBinding -> BuilderTree -> BuilderTree
addNodeAttribute targetId binding tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { attributes = Array.snoc node.attributes binding }) children
      else BNode node (map (addNodeAttribute targetId binding) children)
  other -> other

-- | Remove an attribute from a node
removeNodeAttribute :: NodeId -> String -> BuilderTree -> BuilderTree
removeNodeAttribute targetId attrName tree = case tree of
  BNode node children ->
    if node.id == targetId
      then BNode (node { attributes = Array.filter (\a -> a.attrName /= attrName) node.attributes }) children
      else BNode node (map (removeNodeAttribute targetId attrName) children)
  other -> other

-- =============================================================================
-- Presets
-- =============================================================================

type Preset = { tree :: BuilderTree, data_ :: Array SampleDatum, nextId :: NodeId }

getPreset :: String -> Preset
getPreset = case _ of
  "circles" -> circlesPreset
  "bars" -> barsPreset
  "scatter" -> scatterPreset
  _ -> circlesPreset

circlesPreset :: Preset
circlesPreset =
  { tree: BNode
      { id: 1
      , elementType: "svg"
      , name: Just "root"
      , attributes:
          [ { attrName: "width", choice: ConstantNumber 400.0 }
          , { attrName: "height", choice: ConstantNumber 300.0 }
          ]
      , expanded: true
      }
      [ BDataJoin
          { id: 2
          , name: "circles"
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
  , data_: defaultSampleData
  , nextId: 10
  }

barsPreset :: Preset
barsPreset =
  { tree: BNode
      { id: 1
      , elementType: "svg"
      , name: Just "root"
      , attributes:
          [ { attrName: "width", choice: ConstantNumber 400.0 }
          , { attrName: "height", choice: ConstantNumber 300.0 }
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
      [ { x: 20.0, y: 50.0, radius: 0.0, width: 60.0, height: 80.0, color: "#1f77b4", label: "A", name: "A", value: 80.0, index: 0 }
      , { x: 100.0, y: 90.0, radius: 0.0, width: 60.0, height: 120.0, color: "#ff7f0e", label: "B", name: "B", value: 120.0, index: 1 }
      , { x: 180.0, y: 30.0, radius: 0.0, width: 60.0, height: 60.0, color: "#2ca02c", label: "C", name: "C", value: 60.0, index: 2 }
      , { x: 260.0, y: 10.0, radius: 0.0, width: 60.0, height: 150.0, color: "#d62728", label: "D", name: "D", value: 150.0, index: 3 }
      ]
  , nextId: 10
  }

scatterPreset :: Preset
scatterPreset =
  { tree: BNode
      { id: 1
      , elementType: "svg"
      , name: Just "root"
      , attributes:
          [ { attrName: "width", choice: ConstantNumber 400.0 }
          , { attrName: "height", choice: ConstantNumber 300.0 }
          ]
      , expanded: true
      }
      [ BDataJoin
          { id: 2
          , name: "points"
          , elementType: "group"
          , template:
              { id: 3
              , elementType: "group"
              , name: Nothing
              , attributes: []
              , expanded: true
              }
          , expanded: true
          }
      ]
  , data_: defaultSampleData
  , nextId: 10
  }


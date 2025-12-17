-- | Tree Builder App - 4-Quadrant Interpreter Demo
-- |
-- | Educational demonstration of tagless final / multiple interpreters:
-- | ONE Tree definition → THREE different interpretations
-- |
-- | Layout:
-- |   Q1 (top-left):     TreeBuilder UI - Edit the tree structure
-- |   Q2 (top-right):    MetaAST - Tree structure visualization
-- |   Q3 (bottom-left):  SemiQuine - Generated PureScript code
-- |   Q4 (bottom-right): D3 Preview - Rendered SVG output
-- |
-- | The key insight: The same BuilderTree is converted to a real Tree,
-- | then THREE different interpreters produce THREE different outputs.
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

-- Core types
import TreeBuilder.Types (BuilderTree(..), BuilderNode, NodeId, AttributeChoice(..), AttributeBinding, SampleDatum, ElementOption, availableElements, attributeOptionsFor, emptyNode, sudokuSampleData, chessSampleData, goSampleData)

-- THE KEY CONVERTER: BuilderTree → Tree SampleDatum
import TreeBuilder.ToTree (builderToTreeWithData)

-- INTERPRETERS - all work on Tree SampleDatum
import PSD3.Interpreter.MetaAST (TreeAST(..), toAST)

-- Code generation from actual Tree AST (uses AttrSource metadata)
import PSD3.Interpreter.SemiQuine.TreeToCode (treeToCodeWithSample)

-- Note: BuilderToAST not used - we interpret the actual Tree AST to be honest
-- about what MetaAST can extract from opaque attribute functions

-- D3 rendering for AST visualization
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (SEmpty, ElementType(..))
import PSD3.Expr.Integration (evalAttr, evalAttrStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.AST as T
import Web.DOM.Element (Element)
import Data.Int (toNumber)

-- Preview (FFI-based for now)
import TreeBuilder.Interpreter (renderPreview)

-- Prism syntax highlighting
import PSD3.PrismJS as Prism

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
  , activeTab :: String         -- "tree" | "data" (for Q1)
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
  | SwitchTab String         -- "tree" | "data"
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
  , activeTab: "tree"
  }

-- =============================================================================
-- Render - 4 Quadrant Layout
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "interpreter-demo-page" ] ]
    [ -- Site Navigation
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Multiple Interpreters Demo"
        }

    -- 4-Quadrant Grid
    , HH.div
        [ HP.classes [ HH.ClassName "quadrant-grid" ] ]
        [ -- Q1: TreeBuilder UI (top-left)
          renderQ1TreeBuilder state
        -- Q2: MetaAST Output (top-right)
        , renderQ2MetaAST state
        -- Q3: SemiQuine/Code Output (bottom-left)
        , renderQ3SemiQuine state
        -- Q4: D3 Preview (bottom-right)
        , renderQ4D3Preview state
        ]
    ]

-- =============================================================================
-- Q1: TreeBuilder UI (with Tree/Data tabs)
-- =============================================================================

renderQ1TreeBuilder :: forall m. State -> H.ComponentHTML Action () m
renderQ1TreeBuilder state =
  HH.div
    [ HP.classes [ HH.ClassName "quadrant", HH.ClassName "q1-builder" ] ]
    [ -- Header with tabs
      HH.div
        [ HP.classes [ HH.ClassName "quadrant-header" ] ]
        [ HH.h3_ [ HH.text "TreeBuilder" ]
        , HH.div
            [ HP.classes [ HH.ClassName "quadrant-tabs" ] ]
            [ tabButton state.activeTab "tree" "Tree"
            , tabButton state.activeTab "data" "Data"
            ]
        ]
    -- Tab content
    , HH.div
        [ HP.classes [ HH.ClassName "quadrant-content" ] ]
        [ if state.activeTab == "tree"
            then renderTreeEditor state
            else renderDataEditor state
        ]
    ]

tabButton :: forall m. String -> String -> String -> H.ComponentHTML Action () m
tabButton active tabId label =
  HH.button
    [ HP.classes $ [ HH.ClassName "tab-btn" ] <>
        if active == tabId then [ HH.ClassName "tab-btn--active" ] else []
    , HE.onClick \_ -> SwitchTab tabId
    ]
    [ HH.text label ]

-- Tree editor panel
renderTreeEditor :: forall m. State -> H.ComponentHTML Action () m
renderTreeEditor state =
  HH.div
    [ HP.classes [ HH.ClassName "tree-editor" ] ]
    [ -- Preset buttons
      HH.div
        [ HP.classes [ HH.ClassName "preset-buttons" ] ]
        [ treeButton state "grid" "Grid"
        , treeButton state "radial" "Radial"
        , treeButton state "strip" "Strip"
        ]
    -- Tree view
    , HH.div
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
        [ HP.classes [ HH.ClassName "palette-buttons" ] ]
        (map (renderPaletteBtn state) availableElements)
    ]

-- Data editor panel
renderDataEditor :: forall m. State -> H.ComponentHTML Action () m
renderDataEditor state =
  HH.div
    [ HP.classes [ HH.ClassName "data-editor" ] ]
    [ -- Dataset selector
      HH.div
        [ HP.classes [ HH.ClassName "preset-buttons" ] ]
        [ dataButton state "sudoku" "9x9"
        , dataButton state "chess" "8x8"
        , dataButton state "go" "19x19"
        ]
    -- Data preview (first few items)
    , HH.pre
        [ HP.classes [ HH.ClassName "data-preview" ] ]
        [ HH.text $ formatSampleDataPreview state.sampleData ]
    ]

formatSampleDataPreview :: Array SampleDatum -> String
formatSampleDataPreview data_ =
  let preview = Array.take 5 data_
      count = Array.length data_
  in "// " <> show count <> " items\n" <>
     "[\n" <> Array.intercalate ",\n" (map formatDatumShort preview) <>
     (if count > 5 then "\n  // ... " <> show (count - 5) <> " more" else "") <>
     "\n]"
  where
  formatDatumShort d =
    "  { x: " <> show d.x <> ", y: " <> show d.y <>
    ", color: \"" <> d.color <> "\" }"

-- =============================================================================
-- Q2: MetaAST Output (Visual Tree Diagram)
-- =============================================================================

renderQ2MetaAST :: forall m. State -> H.ComponentHTML Action () m
renderQ2MetaAST _ =
  HH.div
    [ HP.classes [ HH.ClassName "quadrant", HH.ClassName "q2-metaast" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "quadrant-header" ] ]
        [ HH.h3_ [ HH.text "MetaAST Interpreter" ]
        , HH.span
            [ HP.classes [ HH.ClassName "interpreter-tag" ] ]
            [ HH.text "Tree → Structure" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "quadrant-content" ] ]
        [ HH.div
            [ HP.id "ast-viz-output"
            , HP.classes [ HH.ClassName "ast-viz-container" ]
            ]
            []
        ]
    ]

-- | Convert TreeAST to a visual tree diagram using D3
-- | This visualizes the actual AST structure from the MetaAST interpreter
astToTreeVisualization :: TreeAST -> T.Tree Unit
astToTreeVisualization ast =
  T.named SVG "ast-svg"
    [ evalAttr "width" (lit 400.0)
    , evalAttr "height" (lit 350.0)
    , evalAttrStr "viewBox" (str "0 0 400 350")
    ]
    `T.withChild`
      (T.named Group "ast-tree"
        [ evalAttrStr "transform" (str "translate(200, 40)") ]
        `T.withChild` renderASTNode ast 0.0 0)
  where
    renderASTNode :: TreeAST -> Number -> Int -> T.Tree Unit
    renderASTNode node xPos level = case node of
      NodeAST {name, elemType, attrCount, children} ->
        let
          label = case name of
            Just n -> elemType <> ": " <> n
            Nothing -> elemType
          childSpacing = 100.0
          childCount = Array.length children
          startX = xPos - (childSpacing * (toNumber childCount - 1.0) / 2.0)
        in T.named Group ("node-" <> show level)
            []
            `T.withChildren`
              ([ -- Node circle (blue for regular nodes)
                 T.elem Circle
                   [ evalAttr "cx" (lit xPos)
                   , evalAttr "cy" (lit (toNumber level * 70.0))
                   , evalAttr "r" (lit 25.0)
                   , evalAttrStr "fill" (str "#4A90E2")
                   , evalAttrStr "stroke" (str "#2E5C8A")
                   , evalAttr "stroke-width" (lit 2.0)
                   ]
               -- Node label
               , T.elem Text
                   [ evalAttr "x" (lit xPos)
                   , evalAttr "y" (lit (toNumber level * 70.0 + 4.0))
                   , evalAttrStr "textContent" (str label)
                   , evalAttrStr "text-anchor" (str "middle")
                   , evalAttrStr "fill" (str "white")
                   , evalAttrStr "font-size" (str "10px")
                   , evalAttrStr "font-family" (str "monospace")
                   ]
               -- Attribute count badge below
               , T.elem Text
                   [ evalAttr "x" (lit xPos)
                   , evalAttr "y" (lit (toNumber level * 70.0 + 38.0))
                   , evalAttrStr "textContent" (str ("attrs: " <> show attrCount))
                   , evalAttrStr "text-anchor" (str "middle")
                   , evalAttrStr "fill" (str "#666")
                   , evalAttrStr "font-size" (str "9px")
                   ]
               ] <>
               -- Lines to children
               (Array.mapWithIndex (\i _ ->
                 let childX = startX + (toNumber i * childSpacing)
                     childY = (toNumber (level + 1)) * 70.0
                 in T.elem Line
                      [ evalAttr "x1" (lit xPos)
                      , evalAttr "y1" (lit (toNumber level * 70.0 + 25.0))
                      , evalAttr "x2" (lit childX)
                      , evalAttr "y2" (lit (childY - 25.0))
                      , evalAttrStr "stroke" (str "#999")
                      , evalAttr "stroke-width" (lit 1.5)
                      ]
               ) children) <>
               -- Child nodes (recursive)
               (Array.mapWithIndex (\i child ->
                 let childX = startX + (toNumber i * childSpacing)
                 in renderASTNode child childX (level + 1)
               ) children))

      JoinAST {name, dataCount} ->
        T.named Group ("join-" <> show level)
          []
          `T.withChildren`
            [ -- Join node (orange)
              T.elem Circle
                [ evalAttr "cx" (lit xPos)
                , evalAttr "cy" (lit (toNumber level * 70.0))
                , evalAttr "r" (lit 25.0)
                , evalAttrStr "fill" (str "#E27A4A")
                , evalAttrStr "stroke" (str "#8A472E")
                , evalAttr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 4.0))
                , evalAttrStr "textContent" (str ("Join: " <> name))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "white")
                , evalAttrStr "font-size" (str "9px")
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 38.0))
                , evalAttrStr "textContent" (str ("data: " <> show dataCount))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "#666")
                , evalAttrStr "font-size" (str "9px")
                ]
            ]

      NestedJoinAST {name, dataCount} ->
        T.named Group ("nested-join-" <> show level)
          []
          `T.withChildren`
            [ -- Nested join (purple)
              T.elem Circle
                [ evalAttr "cx" (lit xPos)
                , evalAttr "cy" (lit (toNumber level * 70.0))
                , evalAttr "r" (lit 25.0)
                , evalAttrStr "fill" (str "#9B4AE2")
                , evalAttrStr "stroke" (str "#5C2E8A")
                , evalAttr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 4.0))
                , evalAttrStr "textContent" (str ("Nested: " <> name))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "white")
                , evalAttrStr "font-size" (str "9px")
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 38.0))
                , evalAttrStr "textContent" (str ("data: " <> show dataCount))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "#666")
                , evalAttrStr "font-size" (str "9px")
                ]
            ]

      SceneJoinAST {name, dataCount, hasEnter, hasUpdate, hasExit} ->
        T.named Group ("scene-join-" <> show level)
          []
          `T.withChildren`
            [ -- Scene join (green)
              T.elem Circle
                [ evalAttr "cx" (lit xPos)
                , evalAttr "cy" (lit (toNumber level * 70.0))
                , evalAttr "r" (lit 25.0)
                , evalAttrStr "fill" (str "#4AE2A4")
                , evalAttrStr "stroke" (str "#2E8A5C")
                , evalAttr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 4.0))
                , evalAttrStr "textContent" (str ("Scene: " <> name))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "white")
                , evalAttrStr "font-size" (str "9px")
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 38.0))
                , evalAttrStr "textContent" (str ("data: " <> show dataCount <> " {" <>
                              (if hasEnter then "E" else "") <>
                              (if hasUpdate then "U" else "") <>
                              (if hasExit then "X" else "") <> "}"))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "#666")
                , evalAttrStr "font-size" (str "9px")
                ]
            ]

      SceneNestedJoinAST {name, dataCount, hasEnter, hasUpdate, hasExit} ->
        T.named Group ("scene-nested-" <> show level)
          []
          `T.withChildren`
            [ -- Scene nested join (gold)
              T.elem Circle
                [ evalAttr "cx" (lit xPos)
                , evalAttr "cy" (lit (toNumber level * 70.0))
                , evalAttr "r" (lit 25.0)
                , evalAttrStr "fill" (str "#E2A44A")
                , evalAttrStr "stroke" (str "#8A5C2E")
                , evalAttr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 4.0))
                , evalAttrStr "textContent" (str ("SceneNested: " <> name))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "white")
                , evalAttrStr "font-size" (str "8px")
                ]
            , T.elem Text
                [ evalAttr "x" (lit xPos)
                , evalAttr "y" (lit (toNumber level * 70.0 + 38.0))
                , evalAttrStr "textContent" (str ("data: " <> show dataCount <> " {" <>
                              (if hasEnter then "E" else "") <>
                              (if hasUpdate then "U" else "") <>
                              (if hasExit then "X" else "") <> "}"))
                , evalAttrStr "text-anchor" (str "middle")
                , evalAttrStr "fill" (str "#666")
                , evalAttrStr "font-size" (str "9px")
                ]
            ]

-- =============================================================================
-- Q3: SemiQuine/TreeToCode Output
-- =============================================================================

renderQ3SemiQuine :: forall m. State -> H.ComponentHTML Action () m
renderQ3SemiQuine state =
  HH.div
    [ HP.classes [ HH.ClassName "quadrant", HH.ClassName "q3-semiquine" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "quadrant-header" ] ]
        [ HH.h3_ [ HH.text "SemiQuine Interpreter" ]
        , HH.span
            [ HP.classes [ HH.ClassName "interpreter-tag" ] ]
            [ HH.text "Tree → PureScript" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "quadrant-content" ] ]
        [ HH.pre
            [ HP.classes [ HH.ClassName "code-output", HH.ClassName "language-psd3" ] ]
            [ HH.code
                [ HP.classes [ HH.ClassName "language-psd3" ] ]
                [ HH.text $ getSemiQuineOutput state ]
            ]
        ]
    ]

getSemiQuineOutput :: State -> String
getSemiQuineOutput state = case state.tree of
  Nothing -> "-- No tree defined\n-- Select a preset to start"
  Just builderTree ->
    -- Convert BuilderTree → actual Tree SampleDatum (same AST used by MetaAST and D3)
    let actualTree = builderToTreeWithData builderTree state.sampleData
        -- Get first sample datum for evaluating data-driven attrs
        sampleDatum = case Array.head state.sampleData of
          Just d -> d
          Nothing -> { x: 0.0, y: 0.0, cx: 0.0, cy: 0.0, rx: 0.0, ry: 0.0
                     , sx: 0.0, sy: 0.0, radius: 0.0, width: 0.0, height: 0.0
                     , value: 0.0, color: "", label: "", name: "", index: 0 }
    -- Run TreeToCode interpreter on the REAL Tree AST (same source as MetaAST!)
    in treeToCodeWithSample sampleDatum actualTree

-- =============================================================================
-- Q4: D3 Preview
-- =============================================================================

renderQ4D3Preview :: forall m. State -> H.ComponentHTML Action () m
renderQ4D3Preview _ =
  HH.div
    [ HP.classes [ HH.ClassName "quadrant", HH.ClassName "q4-d3preview" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "quadrant-header" ] ]
        [ HH.h3_ [ HH.text "D3 Interpreter" ]
        , HH.span
            [ HP.classes [ HH.ClassName "interpreter-tag" ] ]
            [ HH.text "Tree → SVG" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "quadrant-content" ] ]
        [ HH.div
            [ HP.id "tree-builder-preview"
            , HP.classes [ HH.ClassName "d3-preview-container" ]
            ]
            []
        ]
    ]

-- =============================================================================
-- Tree Node Rendering (simplified from original)
-- =============================================================================

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

renderTreeNode :: forall m. State -> BuilderTree -> H.ComponentHTML Action () m
renderTreeNode state tree = case tree of
  BNode node children ->
    let isSelected = state.selectedNodeId == Just node.id
    in HH.div
      [ HP.classes [ HH.ClassName "tree-node" ] ]
      [ HH.div
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
                [ HH.text $ if node.expanded then "v" else ">" ]
              else HH.span [ HP.classes [ HH.ClassName "expand-placeholder" ] ] [ HH.text "o" ]
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
              [ HH.text "x" ]
          ]
      -- Inline attributes
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
      , if join.expanded
          then renderInlineAttrs join.id join.template.elementType join.template.attributes
          else HH.text ""
      ]

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

missingAttrs :: String -> Array AttributeBinding -> Array { name :: String, label :: String }
missingAttrs elemType current =
  let currentNames = map _.attrName current
      available = attributeOptionsFor elemType
  in Array.filter (\opt -> not (Array.elem opt.name currentNames))
       (map (\o -> { name: o.name, label: o.label }) available)

renderAddAttrBtn :: forall m. NodeId -> { name :: String, label :: String } -> H.ComponentHTML Action () m
renderAddAttrBtn nodeId attr =
  HH.button
    [ HP.classes [ HH.ClassName "add-attr-btn" ]
    , HP.title $ "Add " <> attr.label
    , HE.onClick \_ -> AddAttribute nodeId attr.name
    ]
    [ HH.text $ "+" <> attr.label ]

renderInlineAttr :: forall m. NodeId -> AttributeBinding -> H.ComponentHTML Action () m
renderInlineAttr nodeId binding =
  HH.div
    [ HP.classes [ HH.ClassName "inline-attr" ] ]
    [ HH.span [ HP.classes [ HH.ClassName "inline-attr-name" ] ] [ HH.text binding.attrName ]
    , HH.span [ HP.classes [ HH.ClassName "inline-attr-arrow" ] ] [ HH.text "<-" ]
    , renderChoiceButtons nodeId binding
    , HH.button
        [ HP.classes [ HH.ClassName "inline-attr-remove" ]
        , HE.onClick \_ -> RemoveAttribute nodeId binding.attrName
        ]
        [ HH.text "x" ]
    ]

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

-- =============================================================================
-- Action Handler
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
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
    let tree = getTreeForType treeType
    H.modify_ _ { tree = Just tree, currentTreeType = treeType, selectedNodeId = Nothing }
    handleAction RefreshPreview

  SelectDataSet dataSet -> do
    let data_ = getDataSet dataSet
    H.modify_ _ { sampleData = data_, currentDataSet = dataSet }
    handleAction RefreshPreview

  SwitchTab tab -> do
    H.modify_ _ { activeTab = tab }

  RefreshPreview -> do
    state <- H.get
    case state.tree of
      Nothing -> pure unit
      Just builderTree -> do
        -- Clear both preview containers
        liftEffect $ clearPreviewContainer "#tree-builder-preview"
        liftEffect $ clearPreviewContainer "#ast-viz-output"

        -- Render D3 preview (Q4)
        liftEffect $ renderPreview "#tree-builder-preview" builderTree state.sampleData

        -- Render AST visualization (Q2)
        -- THE KEY: Convert BuilderTree → Tree SampleDatum (the ONE AST)
        let actualTree = builderToTreeWithData builderTree state.sampleData
        -- Run MetaAST interpreter on the REAL Tree to get TreeAST
        let treeAST = toAST actualTree
        -- Convert TreeAST → visual Tree for D3 rendering
        let astVizTree = astToTreeVisualization treeAST
        -- Render with D3
        liftEffect $ runD3v2M do
          astContainer <- select "#ast-viz-output" :: _ (D3v2Selection_ SEmpty Element Unit)
          _ <- renderTree astContainer astVizTree
          pure unit

        -- Highlight code with Prism
        liftEffect Prism.highlightAll

-- FFI
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
-- Tree Types and Data Sets
-- =============================================================================

getTreeForType :: String -> BuilderTree
getTreeForType = case _ of
  "grid" -> gridTree
  "radial" -> radialTree
  "strip" -> stripTree
  _ -> gridTree

getDataSet :: String -> Array SampleDatum
getDataSet = case _ of
  "sudoku" -> sudokuSampleData
  "chess" -> chessSampleData
  "go" -> goSampleData
  _ -> sudokuSampleData

-- Grid tree: renders data as square grid
gridTree :: BuilderTree
gridTree = BNode
  { id: 1
  , elementType: "svg"
  , name: Just "grid"
  , attributes:
      [ { attrName: "width", choice: ConstantNumber 300.0 }
      , { attrName: "height", choice: ConstantNumber 300.0 }
      ]
  , expanded: true
  }
  [ BDataJoin
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
  , BDataJoin
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
              ]
          , expanded: true
          }
      , expanded: true
      }
  ]

-- Radial tree: renders data as polar arrangement
radialTree :: BuilderTree
radialTree = BNode
  { id: 1
  , elementType: "svg"
  , name: Just "radial"
  , attributes:
      [ { attrName: "width", choice: ConstantNumber 300.0 }
      , { attrName: "height", choice: ConstantNumber 300.0 }
      ]
  , expanded: true
  }
  [ BDataJoin
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
              ]
          , expanded: true
          }
      , expanded: true
      }
  ]

-- Strip tree: renders data as horizontal strip
stripTree :: BuilderTree
stripTree = BNode
  { id: 1
  , elementType: "svg"
  , name: Just "strip"
  , attributes:
      [ { attrName: "width", choice: ConstantNumber 300.0 }
      , { attrName: "height", choice: ConstantNumber 60.0 }
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

module PSD3.SimpleTreeBuilder
  ( component
  ) where

import Prelude

import Control.Comonad.Cofree (head)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import DataViz.Layout.Hierarchy.Link (linkBezierVertical)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import EmmetParser.Parser (parseEmmet)
import EmmetParser.Validator (validate)
import EmmetParser.Converter (convertToTree)
import EmmetParser.Types (ParseError, ValidationError)
import TreeBuilder3.TypePropagation (pointType, nodeType, countryType, letterType, boardType, rowType, cellType, propagateTypes)
import TreeBuilder3.Types (DatumType(..), datumTypeLabel, datumTypeFields, PrimType(..), TreeNode, DslNodeType(..), nodeLabel)
import TreeBuilder3.TreeOps (applyLayout, flattenTree, makeLinks, LinkData, filterStructuralTree, getBadgeChildren, positionBadges, isBadgeNodeType)
import TreeBuilder3.Theme as Theme
import Data.Tree (Tree)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (SEmpty, ElementType(..))
import PSD3.Expr.Friendly as F
import PSD3.AST as T
import PSD3.Transform (clearContainer)
import Web.DOM.Element (Element)
import Color (toHexString)

-- =============================================================================
-- State
-- =============================================================================

type State =
  { emmetInput :: String
  , parseResult :: Maybe (Either ParseError (Either ValidationError (Tree TreeNode)))
  , selectedType :: Maybe DatumType
  , exampleIndex :: Maybe Int
  }

-- | Available datum types
availableTypes :: Array DatumType
availableTypes = [ pointType, nodeType, countryType, letterType, boardType ]

-- | Example Emmet expressions
type Example =
  { name :: String
  , expr :: String
  , description :: String
  }

examples :: Array Example
examples =
  [ { name: "Simple Circle"
    , expr: "g>c"
    , description: "Group with a circle child"
    }
  , { name: "Scatter Plot"
    , expr: "j(Point)>c[cx:x,cy:y,r=5,fill=steelblue]"
    , description: "Join points with circle elements"
    }
  , { name: "Bubble Chart"
    , expr: "j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population,fill=coral,opacity=0.6]"
    , description: "Countries with population-sized bubbles"
    }
  , { name: "Nested Grid"
    , expr: "n(Board)>g>n(Row)>g>j(Cell)>r[x@index,y@index,width=20,height=20]"
    , description: "2D board with nested rows and cells"
    }
  , { name: "Bar Chart"
    , expr: "j(Point)>r[x:x,y=0,width=40,height:y,fill=green]"
    , description: "Rectangles for bar chart"
    }
  , { name: "Circle with Multiple Attributes"
    , expr: "c[cx=100,cy=50,r=25,fill=red,stroke=black,k=2]"
    , description: "Circle with many static attributes"
    }
  ]

-- =============================================================================
-- Actions
-- =============================================================================

data Action
  = Initialize
  | UpdateInput String
  | SelectExample Int
  | SelectType DatumType
  | ClearInput

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: forall i. i -> State
initialState _ =
  { emmetInput: ""
  , parseResult: Nothing
  , selectedType: Nothing
  , exampleIndex: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "simple-tree-builder" ] ]
    [ -- Navigation
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Simple Tree Builder"
        }

    , HH.div
        [ HP.classes [ HH.ClassName "builder-container" ] ]
        [ -- Left panel: Input and examples
          HH.div
            [ HP.classes [ HH.ClassName "builder-left-panel" ] ]
            [ renderInputSection state
            , renderExamplesSection state
            ]

        -- Right panel: Type selector and preview
        , HH.div
            [ HP.classes [ HH.ClassName "builder-right-panel" ] ]
            [ renderTypeSelector state
            , renderPreview state
            ]
        ]
    ]

renderInputSection :: forall m. State -> H.ComponentHTML Action () m
renderInputSection state =
  HH.div
    [ HP.classes [ HH.ClassName "input-section" ] ]
    [ HH.h2
        [ HP.classes [ HH.ClassName "section-title" ] ]
        [ HH.text "Emmet Expression" ]

    , HH.div
        [ HP.classes [ HH.ClassName "input-controls" ] ]
        [ HH.textarea
            [ HP.classes [ HH.ClassName "emmet-input" ]
            , HP.placeholder "Enter Emmet expression (e.g., g>c or j(Point)>c[cx:x,cy:y])"
            , HP.value state.emmetInput
            , HE.onValueInput UpdateInput
            , HP.rows 4
            ]
        , HH.button
            [ HP.classes [ HH.ClassName "clear-button" ]
            , HE.onClick \_ -> ClearInput
            , HP.disabled (String.null state.emmetInput)
            ]
            [ HH.text "Clear" ]
        ]

    , renderParseStatus state
    ]

renderParseStatus :: forall m. State -> H.ComponentHTML Action () m
renderParseStatus state =
  case state.parseResult of
    Nothing ->
      if String.null state.emmetInput
        then HH.div
          [ HP.classes [ HH.ClassName "parse-status info" ] ]
          [ HH.text "Enter an Emmet expression or select an example below" ]
        else HH.div_ []

    Just (Left parseError) ->
      HH.div
        [ HP.classes [ HH.ClassName "parse-status error" ] ]
        [ HH.strong_ [ HH.text "Parse Error: " ]
        , HH.text (show parseError)
        ]

    Just (Right (Left validationError)) ->
      HH.div
        [ HP.classes [ HH.ClassName "parse-status error" ] ]
        [ HH.strong_ [ HH.text "Validation Error: " ]
        , HH.text (show validationError)
        ]

    Just (Right (Right _tree)) ->
      HH.div
        [ HP.classes [ HH.ClassName "parse-status success" ] ]
        [ HH.text "✓ Valid expression" ]

renderExamplesSection :: forall m. State -> H.ComponentHTML Action () m
renderExamplesSection state =
  HH.div
    [ HP.classes [ HH.ClassName "examples-section" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "section-subtitle" ] ]
        [ HH.text "Examples" ]

    , HH.div
        [ HP.classes [ HH.ClassName "examples-grid" ] ]
        (Array.mapWithIndex renderExampleCard examples)
    ]
  where
    renderExampleCard :: Int -> Example -> H.ComponentHTML Action () m
    renderExampleCard idx example =
      HH.div
        [ HP.classes
            [ HH.ClassName "example-card"
            , HH.ClassName if state.exampleIndex == Just idx then "selected" else ""
            ]
        , HE.onClick \_ -> SelectExample idx
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "example-name" ] ]
            [ HH.text example.name ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-description" ] ]
            [ HH.text example.description ]
        , HH.code
            [ HP.classes [ HH.ClassName "example-code" ] ]
            [ HH.text example.expr ]
        ]

renderTypeSelector :: forall m. State -> H.ComponentHTML Action () m
renderTypeSelector state =
  HH.div
    [ HP.classes [ HH.ClassName "type-selector-section" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "section-subtitle" ] ]
        [ HH.text "Available Types" ]

    , HH.div
        [ HP.classes [ HH.ClassName "types-grid" ] ]
        (map renderTypeCard availableTypes)
    ]
  where
    renderTypeCard :: DatumType -> H.ComponentHTML Action () m
    renderTypeCard dtype =
      let
        isSelected = state.selectedType == Just dtype
        typeName = datumTypeLabel dtype
        fields = datumTypeFields dtype
      in
        HH.div
          [ HP.classes
              [ HH.ClassName "type-card"
              , HH.ClassName if isSelected then "selected" else ""
              ]
          , HE.onClick \_ -> SelectType dtype
          ]
          [ HH.div
              [ HP.classes [ HH.ClassName "type-name" ] ]
              [ HH.text typeName ]
          , HH.div
              [ HP.classes [ HH.ClassName "type-fields" ] ]
              (map renderField fields)
          ]

    renderField :: { name :: String, typ :: PrimType } -> H.ComponentHTML Action () m
    renderField field =
      HH.div
        [ HP.classes [ HH.ClassName "type-field" ] ]
        [ HH.span
            [ HP.classes [ HH.ClassName "field-name" ] ]
            [ HH.text field.name ]
        , HH.span
            [ HP.classes [ HH.ClassName "field-type" ] ]
            [ HH.text (" : " <> primTypeLabel field.typ) ]
        ]

    primTypeLabel :: PrimType -> String
    primTypeLabel = case _ of
      TNumber -> "Number"
      TString -> "String"
      TInt -> "Int"
      TBoolean -> "Boolean"

renderPreview :: forall m. State -> H.ComponentHTML Action () m
renderPreview _state =
  HH.div
    [ HP.classes [ HH.ClassName "preview-section" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "section-subtitle" ] ]
        [ HH.text "Visualization" ]

    , HH.div
        [ HP.classes [ HH.ClassName "tree-visualization" ]
        , HP.id "simple-tree-viz"
        ]
        []
    ]

-- =============================================================================
-- Visualization
-- =============================================================================

-- | Render node data for visualization
type RenderNode =
  { id :: Int
  , x :: Number
  , y :: Number
  , color :: String
  , label :: String
  , typeLabel :: String
  , isBadge :: Boolean
  , showAsStack :: Boolean
  , strokeWidth :: Number
  }

-- | Check if node should be shown with stack effect
shouldShowAsStack :: DslNodeType -> Boolean
shouldShowAsStack = case _ of
  NodeJoin -> true
  NodeNestedJoin -> true
  NodeUpdateJoin -> true
  NodeUpdateNestedJoin -> true
  _ -> false

-- | Convert TreeNode to RenderNode for visualization
toRenderNode :: TreeNode -> Int -> RenderNode
toRenderNode node _badgeIndex =
  { id: node.id
  , x: node.x
  , y: node.y
  , color: toHexString $ Theme.nodeTypeColor node.nodeType
  , label: nodeLabel node.nodeType
  , typeLabel: datumTypeLabel node.datumType
  , isBadge: isBadgeNodeType node.nodeType
  , showAsStack: shouldShowAsStack node.nodeType
  , strokeWidth: 1.5
  }

-- | Render tree visualization with D3
renderTreeVisualization :: forall o m. MonadAff m => Tree TreeNode -> H.HalogenM State Action () o m Unit
renderTreeVisualization userTree = do
  liftEffect $ clearContainer "#simple-tree-viz"

  -- Propagate types through the tree
  let typedTree = propagateTypes userTree

  -- Apply layout to structural-only tree (excludes badges)
  let structuralTree = filterStructuralTree typedTree
  let positioned = applyLayout structuralTree
  let structuralNodes = flattenTree positioned
  let links = makeLinks positioned

  -- Collect badges for each structural node and position them
  let
    badgeNodes = structuralNodes >>= \parent ->
      let
        badges = getBadgeChildren parent.id typedTree
        positioned' = positionBadges parent badges
      in
        map (\b -> { node: b.node, index: b.index }) positioned'

  -- Convert to render nodes
  let structuralRenderNodes = map (\n -> toRenderNode n 0) structuralNodes
  let badgeRenderNodes = map (\b -> toRenderNode b.node b.index) badgeNodes
  let renderNodes = structuralRenderNodes <> badgeRenderNodes

  -- SVG dimensions
  let svgWidth = 800.0
  let svgHeight = 600.0

  -- Center the tree
  let
    firstX = case Array.head structuralNodes of
      Just n -> n.x
      Nothing -> 0.0
  let minX = Array.foldl (\acc n -> min acc n.x) firstX structuralNodes
  let maxX = Array.foldl (\acc n -> max acc n.x) firstX structuralNodes
  let centerX = (minX + maxX) / 2.0
  let offsetX = (svgWidth / 2.0) - centerX
  let offsetY = 50.0

  liftEffect $ runD3v2M do
    container <- select "#simple-tree-viz" :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Create SVG with links
    let
      linksTree :: T.Tree LinkData
      linksTree =
        T.named SVG "svg"
          [ F.staticStr "width" "100%"
          , F.staticStr "height" "100%"
          , F.viewBox 0.0 0.0 svgWidth svgHeight
          , F.staticStr "preserveAspectRatio" "xMidYMid meet"
          ]
          `T.withChild`
            ( T.named Group "mainGroup"
                []
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
                              , F.strokeWidth (F.num 1.0)
                              ]
                        )
                  )
            )

    linksSelections <- renderTree container linksTree
    mainGroupSel <- liftEffect $ reselectD3v2 "mainGroup" linksSelections

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

    -- Create nodes
    let
      nodesTree :: T.Tree RenderNode
      nodesTree =
        T.named Group "nodesGroup"
          [ F.staticStr "class" "nodes" ]
          `T.withChild`
            ( T.joinData "treeNodes" "g" renderNodes $ \node ->
                T.elem Group
                  [ F.transform $ F.text $ "translate(" <> show (node.x + offsetX) <> "," <> show (node.y + offsetY) <> ")" ]
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
                        ]
                      else
                        -- Regular node rendering
                        ( -- Stacked "punch card" rects for join templates (drawn back-to-front)
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

    _ <- renderTree mainGroupSel nodesTree

    pure unit

-- =============================================================================
-- Event Handling
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    pure unit

  UpdateInput input -> do
    H.modify_ _ { emmetInput = input, exampleIndex = Nothing }
    -- Parse the input
    if String.null input
      then do
        H.modify_ _ { parseResult = Nothing }
        liftEffect $ clearContainer "#simple-tree-viz"
      else do
        let parseResult = parseEmmet input
        case parseResult of
          Left err -> do
            H.modify_ _ { parseResult = Just (Left err) }
            liftEffect $ clearContainer "#simple-tree-viz"
          Right expr -> do
            let validationResult = validate expr
            case validationResult of
              Left err -> do
                H.modify_ _ { parseResult = Just (Right (Left err)) }
                liftEffect $ clearContainer "#simple-tree-viz"
              Right validExpr -> do
                let tree = convertToTree validExpr
                H.modify_ _ { parseResult = Just (Right (Right tree)) }
                renderTreeVisualization tree

  SelectExample idx -> do
    case Array.index examples idx of
      Nothing -> pure unit
      Just example -> do
        H.modify_ _ { emmetInput = example.expr, exampleIndex = Just idx }
        handleAction (UpdateInput example.expr)

  SelectType dtype -> do
    H.modify_ _ { selectedType = Just dtype }

  ClearInput -> do
    H.modify_ _
      { emmetInput = ""
      , parseResult = Nothing
      , exampleIndex = Nothing
      }
    liftEffect $ clearContainer "#simple-tree-viz"

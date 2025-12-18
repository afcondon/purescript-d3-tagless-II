-- | VizMatrix App
-- |
-- | Main Halogen component for the VizMatrix demo.
-- | Shows Type × AST × Data combinations with greyed-out incompatible cards.
-- | Center panel shows both AST tree structure AND rendered visualization.
module VizMatrix.App
  ( component
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

import VizMatrix.Types
  ( DataTypeId(..)
  , AstId(..)
  , DatasetId(..)
  , allDataTypes
  , allAsts
  , allDatasets
  , dataTypeLabel
  , dataTypeDescription
  , astLabel
  , astDescription
  , datasetLabel
  , datasetDescription
  , isCompatible
  , compatibleAsts
  , compatibleDatasets
  , BoardSquare
  )
import VizMatrix.Data (getDataset)

-- =============================================================================
-- Types
-- =============================================================================

type State =
  { selectedType :: Maybe DataTypeId
  , selectedAst :: Maybe AstId
  , selectedDataset :: Maybe DatasetId
  }

data Action
  = SelectType DataTypeId
  | SelectAst AstId
  | SelectDataset DatasetId

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState =
  { selectedType: Nothing
  , selectedAst: Nothing
  , selectedDataset: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.style "display: flex; flex-direction: column; height: 100vh; font-family: system-ui, sans-serif; background: #1a1a2e;" ]
    [ -- TOP: AST cards
      renderTopPanel state
      -- MIDDLE: Type cards (LHS) + Viz (CENTER) + Dataset cards (RHS)
    , HH.div
        [ HP.style "display: flex; flex: 1; overflow: hidden;" ]
        [ renderLeftPanel state
        , renderCenterPanel state
        , renderRightPanel state
        ]
    ]

-- | TOP panel: AST cards
renderTopPanel :: forall m. State -> H.ComponentHTML Action () m
renderTopPanel state =
  HH.div
    [ HP.style "display: flex; justify-content: center; gap: 16px; padding: 16px; background: #16213e; border-bottom: 1px solid #0f3460;" ]
    ( map (renderAstCard state) allAsts )

-- | LEFT panel: Type cards
renderLeftPanel :: forall m. State -> H.ComponentHTML Action () m
renderLeftPanel state =
  HH.div
    [ HP.style "display: flex; flex-direction: column; gap: 12px; padding: 16px; width: 180px; background: #16213e; border-right: 1px solid #0f3460;" ]
    [ HH.div
        [ HP.style "color: #888; font-size: 12px; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px;" ]
        [ HH.text "Data Type" ]
    , HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 8px;" ]
        ( map (renderTypeCard state) allDataTypes )
    ]

-- | RIGHT panel: Dataset cards
renderRightPanel :: forall m. State -> H.ComponentHTML Action () m
renderRightPanel state =
  HH.div
    [ HP.style "display: flex; flex-direction: column; gap: 12px; padding: 16px; width: 180px; background: #16213e; border-left: 1px solid #0f3460;" ]
    [ HH.div
        [ HP.style "color: #888; font-size: 12px; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px;" ]
        [ HH.text "Dataset" ]
    , HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 8px;" ]
        ( map (renderDatasetCard state) allDatasets )
    ]

-- | CENTER panel: AST Tree + Visualization side by side
renderCenterPanel :: forall m. State -> H.ComponentHTML Action () m
renderCenterPanel state =
  HH.div
    [ HP.style "flex: 1; display: flex; flex-direction: row; gap: 24px; padding: 24px; background: #1a1a2e; overflow: auto;" ]
    [ case state.selectedType, state.selectedAst, state.selectedDataset of
        Just typeId, Just astId, Just datasetId ->
          if isCompatible typeId astId datasetId
            then renderDualPanel typeId astId datasetId
            else renderPlaceholder "Incompatible combination"
        _, _, _ ->
          renderPlaceholder "Select a Type, AST, and Dataset"
    ]

-- | Render both AST tree and visualization
renderDualPanel :: forall m. DataTypeId -> AstId -> DatasetId -> H.ComponentHTML Action () m
renderDualPanel typeId astId datasetId =
  HH.div
    [ HP.style "display: flex; flex: 1; gap: 32px; align-items: flex-start; justify-content: center;" ]
    [ -- AST Tree (left side)
      HH.div
        [ HP.style "display: flex; flex-direction: column; align-items: center;" ]
        [ HH.div
            [ HP.style "color: #888; font-size: 12px; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 12px;" ]
            [ HH.text "AST Structure" ]
        , renderAstTree typeId astId
        ]
    , -- Visualization (right side)
      HH.div
        [ HP.style "display: flex; flex-direction: column; align-items: center;" ]
        [ HH.div
            [ HP.style "color: #888; font-size: 12px; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 12px;" ]
            [ HH.text "Rendered Output" ]
        , renderVisualization typeId astId datasetId
        ]
    ]

-- | Placeholder when no viz is shown
renderPlaceholder :: forall m. String -> H.ComponentHTML Action () m
renderPlaceholder msg =
  HH.div
    [ HP.style "flex: 1; display: flex; align-items: center; justify-content: center; color: #666; font-size: 18px; text-align: center;" ]
    [ HH.text msg ]

-- =============================================================================
-- Card Rendering
-- =============================================================================

-- | Render a Type card
renderTypeCard :: forall m. State -> DataTypeId -> H.ComponentHTML Action () m
renderTypeCard state typeId =
  let
    isSelected = state.selectedType == Just typeId
    -- Types are always selectable (they drive compatibility)
    isEnabled = true
    opacity = if isEnabled then "1" else "0.3"
    borderColor = if isSelected then "#e94560" else "#0f3460"
    cursor = if isEnabled then "pointer" else "default"
  in
    HH.div
      [ HP.style $ "padding: 12px; border-radius: 8px; border: 2px solid " <> borderColor
          <> "; background: #0f3460; opacity: " <> opacity
          <> "; cursor: " <> cursor <> "; transition: all 0.2s;"
      , HE.onClick \_ -> SelectType typeId
      ]
      [ HH.div
          [ HP.style "color: #fff; font-weight: 600; font-size: 14px;" ]
          [ HH.text $ dataTypeLabel typeId ]
      , HH.div
          [ HP.style "color: #888; font-size: 11px; margin-top: 4px; font-family: monospace;" ]
          [ HH.text $ dataTypeDescription typeId ]
      ]

-- | Render an AST card
renderAstCard :: forall m. State -> AstId -> H.ComponentHTML Action () m
renderAstCard state astId =
  let
    isSelected = state.selectedAst == Just astId
    -- AST is enabled if no type selected, or if compatible with selected type
    isEnabled = case state.selectedType of
      Nothing -> true
      Just typeId -> Array.elem astId (compatibleAsts typeId)
    opacity = if isEnabled then "1" else "0.3"
    borderColor = if isSelected then "#e94560" else "#0f3460"
    cursor = if isEnabled then "pointer" else "default"
  in
    HH.div
      [ HP.style $ "padding: 12px 20px; border-radius: 8px; border: 2px solid " <> borderColor
          <> "; background: #0f3460; opacity: " <> opacity
          <> "; cursor: " <> cursor <> "; transition: all 0.2s;"
      , if isEnabled
          then HE.onClick \_ -> SelectAst astId
          else HP.style ""  -- No click handler when disabled
      ]
      [ HH.div
          [ HP.style "color: #fff; font-weight: 600; font-size: 14px;" ]
          [ HH.text $ astLabel astId ]
      , HH.div
          [ HP.style "color: #888; font-size: 11px; margin-top: 4px;" ]
          [ HH.text $ astDescription astId ]
      ]

-- | Render a Dataset card (with database icon)
renderDatasetCard :: forall m. State -> DatasetId -> H.ComponentHTML Action () m
renderDatasetCard state datasetId =
  let
    isSelected = state.selectedDataset == Just datasetId
    -- Dataset is enabled if no type selected, or if compatible with selected type
    isEnabled = case state.selectedType of
      Nothing -> true
      Just typeId -> Array.elem datasetId (compatibleDatasets typeId)
    opacity = if isEnabled then "1" else "0.3"
    borderColor = if isSelected then "#e94560" else "#0f3460"
    cursor = if isEnabled then "pointer" else "default"
  in
    HH.div
      [ HP.style $ "padding: 12px; border-radius: 8px; border: 2px solid " <> borderColor
          <> "; background: #0f3460; opacity: " <> opacity
          <> "; cursor: " <> cursor <> "; transition: all 0.2s; display: flex; align-items: center; gap: 10px;"
      , if isEnabled
          then HE.onClick \_ -> SelectDataset datasetId
          else HP.style ""
      ]
      [ -- Database icon
        HH.div
          [ HP.style "font-size: 20px;" ]
          [ HH.text "🗄️" ]
      , HH.div
          []
          [ HH.div
              [ HP.style "color: #fff; font-weight: 600; font-size: 14px;" ]
              [ HH.text $ datasetLabel datasetId ]
          , HH.div
              [ HP.style "color: #888; font-size: 11px; margin-top: 2px;" ]
              [ HH.text $ datasetDescription datasetId ]
          ]
      ]

-- =============================================================================
-- AST Tree Rendering
-- =============================================================================

-- | Render the AST tree based on selected type and AST
renderAstTree :: forall m. DataTypeId -> AstId -> H.ComponentHTML Action () m
renderAstTree typeId astId =
  case typeId, astId of
    TypeBoardSquare, AstGrid -> renderGridAstTree
    _, _ -> HH.text "AST not implemented"

-- | AST tree for Grid visualization of BoardSquare
-- | Structure:
-- |   Join (BoardSquare[])
-- |     └─ Group (key: row,col)
-- |          ├─ Rect (x, y, width, height, fill)
-- |          └─ Text (value)
renderGridAstTree :: forall m. H.ComponentHTML Action () m
renderGridAstTree =
  let
    svgWidth = 320.0
    svgHeight = 280.0

    -- Node dimensions
    nodeWidth = 100.0
    nodeHeight = 28.0
    nodeRx = 6.0

    -- Vertical spacing between levels
    levelHeight = 60.0

    -- Colors matching TreeBuilder3 theme
    joinColor = "#3498db"     -- Blue for Join
    groupColor = "#9b59b6"    -- Purple for Group/template
    elementColor = "#e67e22"  -- Orange for elements
    attrColor = "#27ae60"     -- Green for attributes

    -- Node positions
    joinX = svgWidth / 2.0
    joinY = 30.0

    groupX = svgWidth / 2.0
    groupY = joinY + levelHeight

    rectX = svgWidth / 2.0 - 60.0
    rectY = groupY + levelHeight

    textX = svgWidth / 2.0 + 60.0
    textY = groupY + levelHeight

    -- Attribute badges (small nodes attached to elements)
    attrY = rectY + 35.0
  in
    SE.svg
      [ SA.viewBox 0.0 0.0 svgWidth svgHeight
      , SA.width svgWidth
      , SA.height svgHeight
      ]
      [ -- Links (draw first so they're behind nodes)
        -- Join -> Group
        SE.line
          [ SA.x1 joinX
          , SA.y1 (joinY + nodeHeight/2.0)
          , SA.x2 groupX
          , SA.y2 (groupY - nodeHeight/2.0)
          , SA.stroke (SA.RGB 100 100 120)
          , SA.strokeWidth 2.0
          ]
        -- Group -> Rect
      , SE.line
          [ SA.x1 groupX
          , SA.y1 (groupY + nodeHeight/2.0)
          , SA.x2 rectX
          , SA.y2 (rectY - nodeHeight/2.0)
          , SA.stroke (SA.RGB 100 100 120)
          , SA.strokeWidth 2.0
          ]
        -- Group -> Text
      , SE.line
          [ SA.x1 groupX
          , SA.y1 (groupY + nodeHeight/2.0)
          , SA.x2 textX
          , SA.y2 (textY - nodeHeight/2.0)
          , SA.stroke (SA.RGB 100 100 120)
          , SA.strokeWidth 2.0
          ]

        -- Join node (the operation)
      , renderNode joinX joinY nodeWidth nodeHeight nodeRx joinColor "Join" (Just "BoardSquare[]")

        -- Group node (template - stacked to show it's instantiated for each datum)
      , renderStackedNode groupX groupY nodeWidth nodeHeight nodeRx groupColor "Group" (Just "key: row,col")

        -- Arrow from Type (LHS) to Join - bright cyan
      , SE.line
          [ SA.x1 0.0
          , SA.y1 joinY
          , SA.x2 (joinX - nodeWidth/2.0 - 2.0)
          , SA.y2 joinY
          , SA.stroke (SA.RGB 0 220 255)
          , SA.strokeWidth 3.0
          ]
        -- Cyan arrowhead (triangle)
      , SE.g []
          [ SE.line [ SA.x1 (joinX - nodeWidth/2.0 - 10.0), SA.y1 (joinY - 6.0)
                    , SA.x2 (joinX - nodeWidth/2.0 - 2.0), SA.y2 joinY
                    , SA.stroke (SA.RGB 0 220 255), SA.strokeWidth 3.0 ]
          , SE.line [ SA.x1 (joinX - nodeWidth/2.0 - 10.0), SA.y1 (joinY + 6.0)
                    , SA.x2 (joinX - nodeWidth/2.0 - 2.0), SA.y2 joinY
                    , SA.stroke (SA.RGB 0 220 255), SA.strokeWidth 3.0 ]
          ]

        -- Arrow from Dataset (RHS) to Group - bright magenta
      , SE.line
          [ SA.x1 svgWidth
          , SA.y1 groupY
          , SA.x2 (groupX + nodeWidth/2.0 + 2.0)
          , SA.y2 groupY
          , SA.stroke (SA.RGB 255 100 220)
          , SA.strokeWidth 3.0
          ]
        -- Magenta arrowhead (triangle, pointing left)
      , SE.g []
          [ SE.line [ SA.x1 (groupX + nodeWidth/2.0 + 10.0), SA.y1 (groupY - 6.0)
                    , SA.x2 (groupX + nodeWidth/2.0 + 2.0), SA.y2 groupY
                    , SA.stroke (SA.RGB 255 100 220), SA.strokeWidth 3.0 ]
          , SE.line [ SA.x1 (groupX + nodeWidth/2.0 + 10.0), SA.y1 (groupY + 6.0)
                    , SA.x2 (groupX + nodeWidth/2.0 + 2.0), SA.y2 groupY
                    , SA.stroke (SA.RGB 255 100 220), SA.strokeWidth 3.0 ]
          ]

        -- Rect element
      , renderNode rectX rectY (nodeWidth - 10.0) nodeHeight nodeRx elementColor "Rect" Nothing

        -- Text element
      , renderNode textX textY (nodeWidth - 10.0) nodeHeight nodeRx elementColor "Text" Nothing

        -- Attribute badges for Rect
      , renderBadge (rectX + 55.0) (attrY - 10.0) attrColor "x"
      , renderBadge (rectX + 55.0) (attrY + 12.0) attrColor "y"
      , renderBadge (rectX + 55.0) (attrY + 34.0) attrColor "fill"

        -- Attribute badge for Text
      , renderBadge (textX + 50.0) (attrY) attrColor "text"
      ]

-- | Render a tree node (rectangle with label)
renderNode :: forall m. Number -> Number -> Number -> Number -> Number -> String -> String -> Maybe String -> H.ComponentHTML Action () m
renderNode x y w h rx color label subtitle =
  SE.g []
    [ SE.rect
        [ SA.x (x - w/2.0)
        , SA.y (y - h/2.0)
        , SA.width w
        , SA.height h
        , SA.rx rx
        , SA.ry rx
        , SA.fill (SA.Named color)
        , SA.stroke (SA.RGB 255 255 255)
        , SA.strokeWidth 1.5
        ]
    , SE.text
        [ SA.x x
        , SA.y (y + 1.0)
        , SA.textAnchor SA.AnchorMiddle
        , SA.dominantBaseline SA.BaselineMiddle
        , SA.fill (SA.RGB 255 255 255)
                , SA.fontSize (SA.FontSizeLength (SA.Px 12.0))
        ]
        [ HH.text label ]
    , case subtitle of
        Just sub ->
          SE.text
            [ SA.x x
            , SA.y (y + 16.0)
            , SA.textAnchor SA.AnchorMiddle
            , SA.dominantBaseline SA.BaselineMiddle
            , SA.fill (SA.RGB 200 200 200)
            , SA.fontSize (SA.FontSizeLength (SA.Px 9.0))
            ]
            [ HH.text sub ]
        Nothing -> SE.g [] []
    ]

-- | Render a stacked node (for Join with array data)
renderStackedNode :: forall m. Number -> Number -> Number -> Number -> Number -> String -> String -> Maybe String -> H.ComponentHTML Action () m
renderStackedNode x y w h rx color label subtitle =
  SE.g []
    [ -- Back cards (stacked effect)
      SE.rect
        [ SA.x (x - w/2.0 + 4.0)
        , SA.y (y - h/2.0 - 4.0)
        , SA.width w
        , SA.height h
        , SA.rx rx
        , SA.ry rx
        , SA.fill (SA.Named "#1a1a3e")
        , SA.stroke (SA.Named color)
        , SA.strokeWidth 1.0
        ]
    , SE.rect
        [ SA.x (x - w/2.0 + 2.0)
        , SA.y (y - h/2.0 - 2.0)
        , SA.width w
        , SA.height h
        , SA.rx rx
        , SA.ry rx
        , SA.fill (SA.Named "#1a1a3e")
        , SA.stroke (SA.Named color)
        , SA.strokeWidth 1.0
        ]
      -- Front card
    , SE.rect
        [ SA.x (x - w/2.0)
        , SA.y (y - h/2.0)
        , SA.width w
        , SA.height h
        , SA.rx rx
        , SA.ry rx
        , SA.fill (SA.Named color)
        , SA.stroke (SA.RGB 255 255 255)
        , SA.strokeWidth 1.5
        ]
    , SE.text
        [ SA.x x
        , SA.y (y + 1.0)
        , SA.textAnchor SA.AnchorMiddle
        , SA.dominantBaseline SA.BaselineMiddle
        , SA.fill (SA.RGB 255 255 255)
                , SA.fontSize (SA.FontSizeLength (SA.Px 12.0))
        ]
        [ HH.text label ]
    , case subtitle of
        Just sub ->
          SE.text
            [ SA.x x
            , SA.y (y + 16.0)
            , SA.textAnchor SA.AnchorMiddle
            , SA.dominantBaseline SA.BaselineMiddle
            , SA.fill (SA.RGB 200 200 200)
            , SA.fontSize (SA.FontSizeLength (SA.Px 9.0))
            ]
            [ HH.text sub ]
        Nothing -> SE.g [] []
    ]

-- | Render a small badge (for attributes)
renderBadge :: forall m. Number -> Number -> String -> String -> H.ComponentHTML Action () m
renderBadge x y color label =
  let
    w = 32.0
    h = 18.0
  in
    SE.g []
      [ SE.rect
          [ SA.x x
          , SA.y y
          , SA.width w
          , SA.height h
          , SA.rx 4.0
          , SA.ry 4.0
          , SA.fill (SA.Named color)
          ]
      , SE.text
          [ SA.x (x + w/2.0)
          , SA.y (y + h/2.0 + 1.0)
          , SA.textAnchor SA.AnchorMiddle
          , SA.dominantBaseline SA.BaselineMiddle
          , SA.fill (SA.RGB 255 255 255)
          , SA.fontSize (SA.FontSizeLength (SA.Px 10.0))
          ]
          [ HH.text label ]
      ]

-- =============================================================================
-- Visualization Rendering
-- =============================================================================

-- | Render the actual visualization based on selections
renderVisualization :: forall m. DataTypeId -> AstId -> DatasetId -> H.ComponentHTML Action () m
renderVisualization typeId astId datasetId =
  case typeId, astId of
    TypeBoardSquare, AstGrid ->
      renderGrid (getDataset datasetId) datasetId
    _, _ ->
      renderPlaceholder "Visualization not implemented"

-- | Render a grid visualization for BoardSquare data
renderGrid :: forall m. Array BoardSquare -> DatasetId -> H.ComponentHTML Action () m
renderGrid squares datasetId =
  let
    -- Determine grid size from data
    maxRow = Array.foldl (\acc sq -> max acc sq.row) 0 squares
    maxCol = Array.foldl (\acc sq -> max acc sq.col) 0 squares
    gridSize = max (maxRow + 1) (maxCol + 1)

    -- Cell size based on grid
    cellSize = if gridSize <= 8 then 40 else 32
  in
    HH.div
      [ HP.style "display: flex; flex-direction: column; align-items: center;" ]
      [ -- Title
        HH.div
          [ HP.style "color: #fff; font-size: 16px; font-weight: 600; margin-bottom: 12px;" ]
          [ HH.text $ datasetLabel datasetId ]
        -- Grid
      , HH.div
          [ HP.style $ "display: grid; grid-template-columns: repeat(" <> show gridSize <> ", " <> show cellSize <> "px); "
              <> "grid-template-rows: repeat(" <> show gridSize <> ", " <> show cellSize <> "px); "
              <> "border: 2px solid #333; border-radius: 4px; overflow: hidden;"
          ]
          ( map (renderCell cellSize) squares )
      ]

-- | Render a single cell
renderCell :: forall m. Int -> BoardSquare -> H.ComponentHTML Action () m
renderCell cellSize sq =
  let
    bgColor = case sq.color of
      Just c -> c
      Nothing -> "#fff"
    fontSize = if cellSize >= 40 then "22px" else "18px"
  in
    HH.div
      [ HP.style $ "width: " <> show cellSize <> "px; height: " <> show cellSize <> "px; "
          <> "background: " <> bgColor <> "; "
          <> "display: flex; align-items: center; justify-content: center; "
          <> "font-size: " <> fontSize <> "; "
          <> "border: 1px solid rgba(0,0,0,0.1);"
      ]
      [ HH.text sq.value ]

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SelectType typeId -> do
    state <- H.get
    -- When selecting a type, clear incompatible AST/Dataset selections
    let
      newAst = case state.selectedAst of
        Just astId | Array.elem astId (compatibleAsts typeId) -> Just astId
        _ -> Nothing
      newDataset = case state.selectedDataset of
        Just dsId | Array.elem dsId (compatibleDatasets typeId) -> Just dsId
        _ -> Nothing
    H.modify_ _
      { selectedType = Just typeId
      , selectedAst = newAst
      , selectedDataset = newDataset
      }

  SelectAst astId ->
    H.modify_ _ { selectedAst = Just astId }

  SelectDataset datasetId ->
    H.modify_ _ { selectedDataset = Just datasetId }

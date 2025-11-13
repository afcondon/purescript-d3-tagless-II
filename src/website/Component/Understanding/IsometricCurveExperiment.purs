module PSD3.Understanding.IsometricCurveExperiment where

import Prelude

import D3.Viz.FlareData (HierData, getName)
import D3.Viz.FlareData (getChildren) as FlareData
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (minimum, maximum, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Attributes.Sugar (classed, cx, cy, d, fill, fillOpacity, fontSize, radius, strokeColor, strokeWidth, text, viewBox, x, y)
import PSD3.Internal.Hierarchical (getTreeViaAJAX)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Internal.Utility (removeExistingSVG)
import PSD3.Interpreter.D3 (eval_D3M)
import PSD3.Layout.Hierarchy.Core (hierarchy)
import PSD3.Layout.Hierarchy.Projection (isometricScale, isometricX, isometricY)
import PSD3.Layout.Hierarchy.Tree (TreeNode(..), defaultTreeConfig, tree)

-- | Control point offset direction
data OffsetDir = Positive | Zero | Negative

derive instance eqOffsetDir :: Eq OffsetDir

instance showOffsetDir :: Show OffsetDir where
  show Positive = "+"
  show Zero = "0"
  show Negative = "-"

-- | Page state
type State =
  { treeData :: Maybe TreeJson_
  , cx1Dir :: OffsetDir
  , cy1Dir :: OffsetDir
  , cx2Dir :: OffsetDir
  , cy2Dir :: OffsetDir
  , parentMultiplier :: Number  -- Multiplier for parent control point offset
  , childMultiplier :: Number   -- Multiplier for child control point offset
  }

-- | Actions
data Action
  = Initialize
  | SetCx1 OffsetDir
  | SetCy1 OffsetDir
  | SetCx2 OffsetDir
  | SetCy2 OffsetDir
  | SetParentMultiplier Number
  | SetChildMultiplier Number

-- | Component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { treeData: Nothing
      , cx1Dir: Positive  -- Current working values
      , cy1Dir: Positive
      , cx2Dir: Negative
      , cy2Dir: Negative
      , parentMultiplier: 0.2  -- Base offset
      , childMultiplier: 0.2   -- Base offset
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "experiment-page" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "experiment-header" ] ]
        [ HH.h1_ [ HH.text "Isometric Curve Experiment" ]
        , HH.p_ [ HH.text "Adjust control point offsets to find the best curve shape" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "experiment-controls" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "control-group" ] ]
            [ HH.h3_ [ HH.text "Control Point 1 (Parent)" ]
            , renderOffsetControl "cx1 (X offset)" state.cx1Dir SetCx1
            , renderOffsetControl "cy1 (Y offset)" state.cy1Dir SetCy1
            , renderMultiplierControl "Parent offset size" state.parentMultiplier SetParentMultiplier
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "control-group" ] ]
            [ HH.h3_ [ HH.text "Control Point 2 (Child)" ]
            , renderOffsetControl "cx2 (X offset)" state.cx2Dir SetCx2
            , renderOffsetControl "cy2 (Y offset)" state.cy2Dir SetCy2
            , renderMultiplierControl "Child offset size" state.childMultiplier SetChildMultiplier
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "current-config" ] ]
            [ HH.h4_ [ HH.text "Current Configuration:" ]
            , HH.pre_
                [ HH.text $ "parentOffset = distance * " <> show state.parentMultiplier <> "\n"
                         <> "childOffset = distance * " <> show state.childMultiplier <> "\n"
                         <> "cx1 = x1 " <> offsetFormula state.cx1Dir <> " parentOffset\n"
                         <> "cy1 = y1 " <> offsetFormula state.cy1Dir <> " parentOffset\n"
                         <> "cx2 = x2 " <> offsetFormula state.cx2Dir <> " childOffset\n"
                         <> "cy2 = y2 " <> offsetFormula state.cy2Dir <> " childOffset"
                ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "experiment-viz" ] ]
        []
    ]
  where
    offsetFormula :: OffsetDir -> String
    offsetFormula Positive = "+ "
    offsetFormula Zero = "+ 0 * "
    offsetFormula Negative = "- "

renderOffsetControl :: forall w. String -> OffsetDir -> (OffsetDir -> Action) -> HH.HTML w Action
renderOffsetControl label currentDir mkAction =
  HH.div
    [ HP.classes [ HH.ClassName "offset-control" ] ]
    [ HH.label_ [ HH.text label ]
    , HH.div
        [ HP.classes [ HH.ClassName "radio-group" ] ]
        [ renderRadio Positive currentDir mkAction
        , renderRadio Zero currentDir mkAction
        , renderRadio Negative currentDir mkAction
        ]
    ]

renderRadio :: forall w. OffsetDir -> OffsetDir -> (OffsetDir -> Action) -> HH.HTML w Action
renderRadio dir currentDir mkAction =
  HH.label
    [ HP.classes [ HH.ClassName "radio-label" ] ]
    [ HH.input
        [ HP.type_ HP.InputRadio
        , HP.checked (dir == currentDir)
        , HE.onClick \_ -> mkAction dir
        ]
    , HH.text $ " " <> show dir
    ]

-- | Render a multiplier control with preset options
renderMultiplierControl :: forall w. String -> Number -> (Number -> Action) -> HH.HTML w Action
renderMultiplierControl label currentValue mkAction =
  HH.div
    [ HP.classes [ HH.ClassName "multiplier-control" ] ]
    [ HH.label_ [ HH.text label ]
    , HH.div
        [ HP.classes [ HH.ClassName "radio-group" ] ]
        [ renderMultiplierRadio 0.0 currentValue mkAction "0.0 (none)"
        , renderMultiplierRadio 0.01 currentValue mkAction "0.01 (minimal)"
        , renderMultiplierRadio 0.1 currentValue mkAction "0.1 (tiny)"
        , renderMultiplierRadio 0.15 currentValue mkAction "0.15 (small)"
        , renderMultiplierRadio 0.2 currentValue mkAction "0.2 (base)"
        , renderMultiplierRadio 0.25 currentValue mkAction "0.25 (medium)"
        , renderMultiplierRadio 0.3 currentValue mkAction "0.3 (large)"
        , renderMultiplierRadio 0.4 currentValue mkAction "0.4 (huge)"
        ]
    ]

renderMultiplierRadio :: forall w. Number -> Number -> (Number -> Action) -> String -> HH.HTML w Action
renderMultiplierRadio value currentValue mkAction labelText =
  HH.label
    [ HP.classes [ HH.ClassName "radio-label" ] ]
    [ HH.input
        [ HP.type_ HP.InputRadio
        , HP.checked (value == currentValue)
        , HE.onClick \_ -> mkAction value
        ]
    , HH.text $ " " <> labelText
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Load tree data
    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
    case treeJSON of
      Left _ -> pure unit
      Right json -> do
        H.modify_ _ { treeData = Just json }
        state <- H.get
        _ <- H.liftAff $ drawIsometricTree state json
        pure unit

  SetCx1 dir -> do
    H.modify_ _ { cx1Dir = dir }
    redrawViz

  SetCy1 dir -> do
    H.modify_ _ { cy1Dir = dir }
    redrawViz

  SetCx2 dir -> do
    H.modify_ _ { cx2Dir = dir }
    redrawViz

  SetCy2 dir -> do
    H.modify_ _ { cy2Dir = dir }
    redrawViz

  SetParentMultiplier mult -> do
    H.modify_ _ { parentMultiplier = mult }
    redrawViz

  SetChildMultiplier mult -> do
    H.modify_ _ { childMultiplier = mult }
    redrawViz

redrawViz :: forall o. H.HalogenM State Action () o Aff Unit
redrawViz = do
  state <- H.get
  case state.treeData of
    Nothing -> pure unit
    Just json -> do
      _ <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.experiment-viz"
      _ <- H.liftAff $ drawIsometricTree state json
      pure unit

-- | Helper to convert OffsetDir to multiplier
dirToMultiplier :: OffsetDir -> Number
dirToMultiplier Positive = 1.0
dirToMultiplier Zero = 0.0
dirToMultiplier Negative = -1.0

-- | Generate link path with configurable control points
makeIsometricLinkPath :: State -> TreeNode HierData -> TreeNode HierData -> String
makeIsometricLinkPath state parent child =
  let
    x1 = isometricX parent
    y1 = isometricY parent
    x2 = isometricX child
    y2 = isometricY child

    distance = abs (x2 - x1) + abs (y2 - y1)
    parentOffset = distance * state.parentMultiplier
    childOffset = distance * state.childMultiplier

    cx1 = x1 + (dirToMultiplier state.cx1Dir * parentOffset)
    cy1 = y1 + (dirToMultiplier state.cy1Dir * parentOffset)
    cx2 = x2 + (dirToMultiplier state.cx2Dir * childOffset)
    cy2 = y2 + (dirToMultiplier state.cy2Dir * childOffset)
  in
    "M" <> show x1 <> "," <> show y1
    <> " C" <> show cx1 <> "," <> show cy1
    <> " " <> show cx2 <> "," <> show cy2
    <> " " <> show x2 <> "," <> show y2

-- | Get all nodes (recursive traversal)
getAllNodes :: forall a. TreeNode a -> Array (TreeNode a)
getAllNodes node@(TreeNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- | Calculate bounds
calculateBounds :: Array (TreeNode HierData) -> { minX :: Number, maxX :: Number, minY :: Number, maxY :: Number }
calculateBounds nodes =
  let
    xCoords = map isometricX nodes
    yCoords = map isometricY nodes
    minX = fromMaybe 0.0 $ minimum xCoords
    maxX = fromMaybe 0.0 $ maximum xCoords
    minY = fromMaybe 0.0 $ minimum yCoords
    maxY = fromMaybe 0.0 $ maximum yCoords
  in
    { minX, maxX, minY, maxY }

-- | Draw the isometric tree with current control point settings
drawIsometricTree :: State -> TreeJson_ -> Aff Unit
drawIsometricTree state flareData = liftEffect $ eval_D3M do
  let chartWidth = 1600.0
  let chartHeight = 1200.0

  -- Create hierarchy and apply tree layout
  let root = hierarchy flareData FlareData.getChildren
  let config = defaultTreeConfig
        { size = { width: chartWidth * isometricScale, height: chartHeight * isometricScale }
        }
  let layout = tree config root

  -- Get all nodes
  let nodes = getAllNodes layout
  let bounds = calculateBounds nodes

  -- Calculate viewBox
  let padding = 50.0
  let width = bounds.maxX - bounds.minX + (2.0 * padding)
  let height = bounds.maxY - bounds.minY + (2.0 * padding)
  let viewBoxSpec = { x: bounds.minX - padding, y: bounds.minY - padding, width, height }

  let selector = "div.experiment-viz" :: Selector (D3Selection_ Unit)

  root' <- attach selector :: _ (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox viewBoxSpec.x viewBoxSpec.y viewBoxSpec.width viewBoxSpec.height
    , classed "isometric-tree-experiment"
    ]

  -- Add title showing current config
  _ <- appendTo svg Text
    [ x $ viewBoxSpec.x + 10.0
    , y $ viewBoxSpec.y + 20.0
    , fill "#333"
    , fontSize 14.0
    , text $ "Config: cx1=" <> show state.cx1Dir <> " cy1=" <> show state.cy1Dir
           <> " cx2=" <> show state.cx2Dir <> " cy2=" <> show state.cy2Dir
    , classed "title"
    ]

  -- Create groups
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Render links
  let renderLinks :: TreeNode HierData -> _ Unit
      renderLinks parent@(TreeNode node) = do
        traverse_ (\child -> do
          let pathData = makeIsometricLinkPath state parent child
          _ <- appendTo linksGroup Path
            [ d pathData
            , fill "none"
            , strokeColor "#555"
            , fillOpacity 0.4
            , strokeWidth 1.5
            , classed "link"
            ]
          pure unit
        ) node.children
        traverse_ renderLinks node.children

  -- Render nodes
  let renderNode :: TreeNode HierData -> _ Unit
      renderNode treeNode@(TreeNode node) = do
        let isLeaf = length node.children == 0
        let nodeRadius = if isLeaf then 3.0 else 4.0
        _ <- appendTo nodesGroup Circle
          [ cx $ isometricX treeNode
          , cy $ isometricY treeNode
          , radius nodeRadius
          , fill "#999"
          , strokeColor "#555"
          , strokeWidth 1.5
          , classed "node"
          ]
        traverse_ renderNode node.children

  -- Render everything
  _ <- renderLinks layout
  _ <- renderNode layout

  pure unit

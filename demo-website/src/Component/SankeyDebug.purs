module Component.SankeyDebug where

-- | Sankey Debug Testbed
-- | Shows two columns of actual SVG diagrams at each phase of the algorithm:
-- | 1. D3-sankey (actual JavaScript library)
-- | 2. Our PureScript implementation
-- |
-- | Each column shows the progression through relaxation iterations.
-- |
-- | Note: Markov Chain ordering was removed (archived in kept-for-historical-context/MarkovChain_Archive)
-- | as it didn't produce better results than the D3-style relaxation heuristic.

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (filter, foldl, length, nub, (..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (AttrName(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Elements as SE

-- FFI for d3-sankey
import Component.SankeyDebug.FFI (D3Node, D3Link, D3NodeInput, D3LinkInput, D3SankeyStep, computeD3SankeyWithSteps, sankeyLinkPath_)

-- Our layout modules
import DataViz.Layout.Sankey.CSV (parseSankeyCSV)
import DataViz.Layout.Sankey.Types (SankeyNode, SankeyLink, SankeyStep)
import DataViz.Layout.Sankey.ComputeWithSteps (computeLayoutWithSteps)

-- | State for the debug component
type State =
  { csvData :: Maybe String
  , selectedDataset :: String
  , selectedIteration :: Int
  , maxIterations :: Int
  , d3Steps :: Array D3SankeyStep
  , psSteps :: Array SankeyStep -- Our PureScript steps (from Types)
  , svgWidth :: Number
  , svgHeight :: Number
  }

data Action
  = Initialize
  | LoadDataset String
  | SelectIteration Int
  | RunComparison

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { csvData: Nothing
      , selectedDataset: "/data/d3-library-deps.csv"
      , selectedIteration: 0
      , maxIterations: 6
      , d3Steps: []
      , psSteps: []
      , svgWidth: 350.0
      , svgHeight: 400.0
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall w. State -> HH.HTML w Action
render state =
  HH.div
    [ HP.classes [ HH.ClassName "sankey-debug-page" ]
    , HP.style pageStyle
    ]
    [ -- Header
      HH.div
        [ HP.style "margin-bottom: 20px; border-bottom: 1px solid #333; padding-bottom: 15px;" ]
        [ HH.h1
            [ HP.style "color: #00ff88; margin: 0 0 10px 0; font-size: 24px;" ]
            [ HH.text "Sankey Layout Algorithm Comparison" ]
        , HH.p
            [ HP.style "color: #aaa; margin: 0; font-size: 14px;" ]
            [ HH.text "Visual comparison of D3-sankey (JS) and our PureScript port" ]
        ]

    -- Controls
    , HH.div
        [ HP.style "margin-bottom: 20px; display: flex; gap: 20px; align-items: center; flex-wrap: wrap;" ]
        [ -- Dataset selector
          HH.div_
            [ HH.label [ HP.style "color: #888; margin-right: 8px;" ] [ HH.text "Dataset:" ]
            , HH.select
                [ HP.style selectStyle
                , HE.onValueChange LoadDataset
                ]
                [ HH.option [ HP.value "/data/d3-library-deps.csv" ] [ HH.text "D3 Library Dependencies" ]
                , HH.option [ HP.value "/data/d3-website-deps.csv" ] [ HH.text "Website Dependencies" ]
                , HH.option [ HP.value "/data/energy.csv" ] [ HH.text "Energy flow" ]
                ]
            ]

        -- Iteration slider
        , HH.div
            [ HP.style "flex: 1; min-width: 200px;" ]
            [ HH.label [ HP.style "color: #888; margin-right: 8px;" ]
                [ HH.text $ "Iteration: " <> show state.selectedIteration <> " / " <> show state.maxIterations ]
            , HH.input
                [ HP.type_ HP.InputRange
                , HP.min 0.0
                , HP.max (toNumber state.maxIterations)
                , HP.value (show state.selectedIteration)
                , HP.style "width: 200px; vertical-align: middle;"
                , HE.onValueInput (\v -> SelectIteration (fromMaybe 0 (parseInt v)))
                ]
            ]
        ]

    -- Two-column comparison
    , HH.div
        [ HP.style "display: flex; gap: 15px; flex-wrap: wrap;" ]
        [ -- Column 1: D3-sankey (JavaScript)
          renderColumn "D3-sankey (JS)" "#4a9eff" state.d3Steps state.selectedIteration state.svgWidth state.svgHeight

        -- Column 2: PureScript implementation
        , renderColumnPS "PureScript" "#00ff88" state.psSteps state.selectedIteration state.svgWidth state.svgHeight
        ]

    -- Summary stats
    , renderSummary state
    ]

pageStyle :: String
pageStyle = "padding: 20px; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #1a1a2e; color: #eee; min-height: 100vh;"

selectStyle :: String
selectStyle = "padding: 8px 12px; font-size: 14px; background: #2a2a4e; color: #fff; border: 1px solid #444; border-radius: 4px;"

-- | Render a column for D3 steps
renderColumn :: forall w i. String -> String -> Array D3SankeyStep -> Int -> Number -> Number -> HH.HTML w i
renderColumn title color steps iteration w h =
  HH.div
    [ HP.style "flex: 1; min-width: 360px; background: #2a2a4e; border-radius: 8px; padding: 15px;" ]
    [ HH.h3
        [ HP.style $ "color: " <> color <> "; margin: 0 0 10px 0; font-size: 16px;" ]
        [ HH.text title ]
    , case steps Array.!! iteration of
        Just step ->
          HH.div_
            [ HH.p
                [ HP.style "color: #888; font-size: 12px; margin: 0 0 10px 0;" ]
                [ HH.text step.label ]
            , renderD3SankeySVG step.nodes step.links w h color
            , renderNodeStats step.nodes
            ]
        Nothing ->
          HH.p [ HP.style "color: #666;" ] [ HH.text "No data" ]
    ]

-- | Render a column for PureScript steps
renderColumnPS :: forall w i. String -> String -> Array SankeyStep -> Int -> Number -> Number -> HH.HTML w i
renderColumnPS title color steps iteration w h =
  HH.div
    [ HP.style "flex: 1; min-width: 360px; background: #2a2a4e; border-radius: 8px; padding: 15px;" ]
    [ HH.h3
        [ HP.style $ "color: " <> color <> "; margin: 0 0 10px 0; font-size: 16px;" ]
        [ HH.text title ]
    , case steps Array.!! iteration of
        Just step ->
          HH.div_
            [ HH.p
                [ HP.style "color: #888; font-size: 12px; margin: 0 0 10px 0;" ]
                [ HH.text step.label ]
            , renderPSSankeySVG step.nodes step.links w h color
            , renderPSNodeStats step.nodes
            ]
        Nothing ->
          HH.p [ HP.style "color: #666;" ] [ HH.text "No data" ]
    ]

-- | Render D3 Sankey as SVG
renderD3SankeySVG :: forall w i. Array D3Node -> Array D3Link -> Number -> Number -> String -> HH.HTML w i
renderD3SankeySVG nodes links w h accentColor =
  SE.svg
    [ SA.viewBox 0.0 0.0 w h
    , HP.style $ "width: 100%; height: " <> show h <> "px; background: #1e1e3a; border-radius: 4px;"
    ]
    [ -- Links
      SE.g []
        (map (renderD3Link nodes) links)
    -- Nodes
    , SE.g []
        (map (renderD3Node accentColor) nodes)
    -- Labels
    , SE.g []
        (map (renderD3Label w) nodes)
    ]

renderD3Link :: forall w i. Array D3Node -> D3Link -> HH.HTML w i
renderD3Link nodes link =
  let
    pathD = sankeyLinkPath_ link nodes
  in
    SE.path
      [ HP.attr (AttrName "d") pathD
      , HP.attr (AttrName "fill") "none"
      , HP.attr (AttrName "stroke") "#666"
      , HP.attr (AttrName "stroke-width") (show link.width)
      , HP.attr (AttrName "stroke-opacity") "0.4"
      ]

renderD3Node :: forall w i. String -> D3Node -> HH.HTML w i
renderD3Node color node =
  SE.rect
    [ SA.x node.x0
    , SA.y node.y0
    , SA.width (node.x1 - node.x0)
    , SA.height (max 1.0 (node.y1 - node.y0))
    , SA.fill (Named color)
    , SA.fillOpacity 0.8
    ]

renderD3Label :: forall w i. Number -> D3Node -> HH.HTML w i
renderD3Label svgWidth node =
  let
    isLeftSide = node.x0 < svgWidth / 2.0
    labelX = if isLeftSide then node.x1 + 4.0 else node.x0 - 4.0
    anchor = if isLeftSide then "start" else "end"
  in
    SE.text
      [ SA.x labelX
      , SA.y ((node.y0 + node.y1) / 2.0 + 3.0)
      , HP.attr (AttrName "fill") "#ccc"
      , HP.style $ "font-size: 9px; text-anchor: " <> anchor <> ";"
      ]
      [ HH.text node.name ]

-- | Render PureScript Sankey as SVG
renderPSSankeySVG :: forall w i. Array SankeyNode -> Array SankeyLink -> Number -> Number -> String -> HH.HTML w i
renderPSSankeySVG nodes links w h accentColor =
  SE.svg
    [ SA.viewBox 0.0 0.0 w h
    , HP.style $ "width: 100%; height: " <> show h <> "px; background: #1e1e3a; border-radius: 4px;"
    ]
    [ -- Links
      SE.g []
        (map (renderPSLink nodes) links)
    -- Nodes
    , SE.g []
        (map (renderPSNode accentColor) nodes)
    -- Labels
    , SE.g []
        (map (renderPSLabel w) nodes)
    ]

renderPSLink :: forall w i. Array SankeyNode -> SankeyLink -> HH.HTML w i
renderPSLink nodes link =
  let
    maybeSource = Array.find (\n -> n.index == link.sourceIndex) nodes
    maybeTarget = Array.find (\n -> n.index == link.targetIndex) nodes
  in
    case maybeSource, maybeTarget of
      Just source, Just target ->
        let
          x0 = source.x1
          x1 = target.x0
          pathD = "M" <> show x0 <> "," <> show link.y0
            <> "C"
            <> show ((x0 + x1) / 2.0)
            <> ","
            <> show link.y0
            <> " "
            <> show ((x0 + x1) / 2.0)
            <> ","
            <> show link.y1
            <> " "
            <> show x1
            <> ","
            <> show link.y1
        in
          SE.path
            [ HP.attr (AttrName "d") pathD
            , HP.attr (AttrName "fill") "none"
            , HP.attr (AttrName "stroke") "#666"
            , HP.attr (AttrName "stroke-width") (show link.width)
            , HP.attr (AttrName "stroke-opacity") "0.4"
            ]
      _, _ -> SE.g [] []

renderPSNode :: forall w i. String -> SankeyNode -> HH.HTML w i
renderPSNode color node =
  SE.rect
    [ SA.x node.x0
    , SA.y node.y0
    , SA.width (node.x1 - node.x0)
    , SA.height (max 1.0 (node.y1 - node.y0))
    , SA.fill (Named color)
    , SA.fillOpacity 0.8
    ]

renderPSLabel :: forall w i. Number -> SankeyNode -> HH.HTML w i
renderPSLabel svgWidth node =
  let
    isLeftSide = node.x0 < svgWidth / 2.0
    labelX = if isLeftSide then node.x1 + 4.0 else node.x0 - 4.0
    anchor = if isLeftSide then "start" else "end"
  in
    SE.text
      [ SA.x labelX
      , SA.y ((node.y0 + node.y1) / 2.0 + 3.0)
      , HP.attr (AttrName "fill") "#ccc"
      , HP.style $ "font-size: 9px; text-anchor: " <> anchor <> ";"
      ]
      [ HH.text node.name ]

-- | Node statistics
renderNodeStats :: forall w i. Array D3Node -> HH.HTML w i
renderNodeStats nodes =
  let
    maxLayer = foldl (\acc n -> max acc n.layer) 0 nodes
    layerCounts = map (\l -> length (filter (\n -> n.layer == l) nodes)) (0 .. maxLayer)
  in
    HH.div
      [ HP.style "margin-top: 10px; font-size: 11px; color: #888;" ]
      [ HH.text $ show (length nodes) <> " nodes, layers: " <> show layerCounts ]

renderPSNodeStats :: forall w i. Array SankeyNode -> HH.HTML w i
renderPSNodeStats nodes =
  let
    maxLayer = foldl (\acc n -> max acc n.layer) 0 nodes
    layerCounts = map (\l -> length (filter (\n -> n.layer == l) nodes)) (0 .. maxLayer)
  in
    HH.div
      [ HP.style "margin-top: 10px; font-size: 11px; color: #888;" ]
      [ HH.text $ show (length nodes) <> " nodes, layers: " <> show layerCounts ]

-- | Summary comparison
renderSummary :: forall w i. State -> HH.HTML w i
renderSummary state =
  HH.div
    [ HP.style "margin-top: 20px; background: #2a2a4e; border-radius: 8px; padding: 15px;" ]
    [ HH.h3
        [ HP.style "color: #fff; margin: 0 0 10px 0; font-size: 16px;" ]
        [ HH.text $ "Comparison at iteration " <> show state.selectedIteration ]
    , HH.div
        [ HP.style "display: grid; grid-template-columns: repeat(2, 1fr); gap: 15px; font-size: 12px;" ]
        [ renderSummaryColumn "D3-sankey" "#4a9eff" (state.d3Steps Array.!! state.selectedIteration)
        , renderSummaryColumnPS "PureScript" "#00ff88" (state.psSteps Array.!! state.selectedIteration)
        ]
    ]

renderSummaryColumn :: forall w i. String -> String -> Maybe D3SankeyStep -> HH.HTML w i
renderSummaryColumn title color maybeStep =
  HH.div_
    [ HH.strong [ HP.style $ "color: " <> color <> ";" ] [ HH.text title ]
    , case maybeStep of
        Just step ->
          HH.ul [ HP.style "margin: 5px 0; padding-left: 15px; color: #aaa;" ]
            [ HH.li_ [ HH.text $ "Nodes: " <> show (length step.nodes) ]
            , HH.li_ [ HH.text $ "Links: " <> show (length step.links) ]
            ]
        Nothing ->
          HH.p [ HP.style "color: #666;" ] [ HH.text "No data" ]
    ]

renderSummaryColumnPS :: forall w i. String -> String -> Maybe SankeyStep -> HH.HTML w i
renderSummaryColumnPS title color maybeStep =
  HH.div_
    [ HH.strong [ HP.style $ "color: " <> color <> ";" ] [ HH.text title ]
    , case maybeStep of
        Just step ->
          HH.ul [ HP.style "margin: 5px 0; padding-left: 15px; color: #aaa;" ]
            [ HH.li_ [ HH.text $ "Nodes: " <> show (length step.nodes) ]
            , HH.li_ [ HH.text $ "Links: " <> show (length step.links) ]
            ]
        Nothing ->
          HH.p [ HP.style "color: #666;" ] [ HH.text "No data" ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction (LoadDataset "/data/energy.csv")

  LoadDataset path -> do
    H.modify_ _ { selectedDataset = path }
    result <- liftAff $ AX.get ResponseFormat.string path
    case result of
      Left _ -> liftEffect $ Console.log $ "Failed to load: " <> path
      Right response -> do
        H.modify_ _ { csvData = Just response.body }
        handleAction RunComparison

  SelectIteration iter -> do
    H.modify_ _ { selectedIteration = iter }

  RunComparison -> do
    state <- H.get
    case state.csvData of
      Nothing -> pure unit
      Just csv -> do
        let { d3Steps, psSteps } = computeAllSteps csv state.svgWidth state.svgHeight state.maxIterations
        H.modify_ _ { d3Steps = d3Steps, psSteps = psSteps }

-- | Compute all algorithm steps
computeAllSteps
  :: String
  -> Number
  -> Number
  -> Int
  -> { d3Steps :: Array D3SankeyStep, psSteps :: Array SankeyStep }
computeAllSteps csvData width height maxIterations =
  let
    -- Parse CSV to get unique nodes and links
    linkInputs = parseSankeyCSV csvData
    nodeNames = nub $ Array.concat $ map (\l -> [ l.s, l.t ]) linkInputs

    -- Convert to D3 format
    d3Nodes :: Array D3NodeInput
    d3Nodes = map (\name -> { name }) nodeNames

    d3Links :: Array D3LinkInput
    d3Links = map (\l -> { source: l.s, target: l.t, value: l.v }) linkInputs

    -- Compute D3-sankey steps (JavaScript)
    d3Steps = computeD3SankeyWithSteps d3Nodes d3Links
      { width, height, nodeWidth: 15.0, nodePadding: 10.0, maxIterations }

    -- Compute PureScript steps using our implementation
    psSteps = computeLayoutWithSteps linkInputs width height maxIterations

  in
    { d3Steps, psSteps }

-- Helper
parseInt :: String -> Maybe Int
parseInt s = case Array.head (split (Pattern ".") s) of
  Just intPart -> Just (unsafeParseInt intPart)
  Nothing -> Nothing

foreign import unsafeParseInt :: String -> Int

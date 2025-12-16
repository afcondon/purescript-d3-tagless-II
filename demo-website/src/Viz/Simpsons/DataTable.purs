-- | Data Table with Bar Charts and Mini Donuts
-- |
-- | Displays department-level statistics with:
-- | - Small bar charts showing applied/admitted counts
-- | - Mini donut charts showing acceptance rates
-- | - Highlight ring on donut when that gender has higher rate
module D3.Viz.Simpsons.DataTable
  ( initDataTable
  , updateDataTable
  , TableRowData
  , buildRowData
  ) where

import Prelude

import D3.Viz.Simpsons.Pie (PieSlice, pie)
import D3.Viz.Simpsons.Types (DerivedData, Gender(..), blue, gray, green, population, purple, rates, red)
import Data.Array (mapWithIndex)
import Data.Int (round, toNumber) as Int
import Data.Number (cos, sin)
import Data.Traversable (for_)
import Effect (Effect)
import PSD3.Internal.FFI (ArcGenerator_, arcGenerator_, arcPath_, setArcInnerRadius_, setArcOuterRadius_)
import Web.DOM.Document (toParentNode) as Document
import Web.DOM.Element (toNode) as Element
import Web.DOM.Node (setTextContent) as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import PSD3.Internal.Types (Datum_)
import PSD3.Expr.Friendly (num, text, attr, x, y, fill, stroke, strokeWidth, transform, path, textAnchor, fontSize, textContent, width, height, viewBox, cx, cy, r)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Interpreter.D3 (D3v2M, D3v2Selection_, runD3v2M)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree)
import PSD3.AST as T
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- =============================================================================
-- DOM Helpers
-- =============================================================================

-- | Clear all children from a container selected by CSS selector
clearContainer :: String -> Effect Unit
clearContainer selector = do
  doc <- window >>= document <#> toDocument
  let parentNode = Document.toParentNode doc
  maybeEl <- querySelector (QuerySelector selector) parentNode
  for_ maybeEl \el ->
    Node.setTextContent "" (Element.toNode el)

-- =============================================================================
-- Configuration
-- =============================================================================

type BarConfig =
  { width :: Number
  , height :: Number
  , maxValue :: Number
  }

defaultBarConfig :: BarConfig
defaultBarConfig =
  { width: 50.0
  , height: 20.0
  , maxValue: Int.toNumber population.male
  }

type MiniDonutConfig =
  { size :: Number
  , outerRadius :: Number
  , innerRadius :: Number
  , highlightRadius :: Number
  , labelSize :: Number
  }

defaultMiniDonutConfig :: MiniDonutConfig
defaultMiniDonutConfig =
  { size: 85.0
  , outerRadius: 85.0 / 2.3
  , innerRadius: 85.0 / 2.3 - 10.0
  , highlightRadius: 85.0 / 2.1
  , labelSize: 12.0
  }

-- =============================================================================
-- Bar Chart Tree
-- =============================================================================

barChart :: BarConfig -> Number -> Gender -> Tree Unit
barChart config value gender =
  let
    barWidth = (value / config.maxValue) * config.width
    fillColor = case gender of
      Male -> purple
      Female -> green
  in
    T.elem SVG
      [ width $ num config.width
      , height $ num config.height
      , viewBox 0.0 0.0 config.width config.height
      , attr "class" $ text "bar-chart-svg"
      ]
      `T.withChildren`
        [ T.elem Rect
            [ width $ num config.width
            , height $ num config.height
            , fill $ text "none"
            , stroke $ text gray
            , strokeWidth $ num 2.0
            ]
        , T.elem Rect
            [ width $ num barWidth
            , height $ num config.height
            , fill $ text fillColor
            ]
        ]

-- =============================================================================
-- Mini Donut Chart Tree
-- =============================================================================

type SliceData =
  { label :: String
  , percent :: Number
  }

configureArc :: Number -> Number -> ArcGenerator_
configureArc innerR outerR =
  setArcOuterRadius_
    (setArcInnerRadius_ (arcGenerator_ unit) innerR)
    outerR

sliceToDatum :: PieSlice SliceData -> Datum_
sliceToDatum slice = unsafeCoerce
  { startAngle: slice.startAngle
  , endAngle: slice.endAngle
  }

arcCentroid :: Number -> Number -> PieSlice SliceData -> { x :: Number, y :: Number }
arcCentroid innerR outerR slice =
  let
    midAngle = (slice.startAngle + slice.endAngle) / 2.0 - 1.5707963267948966
    midRadius = (innerR + outerR) / 2.0
  in
    { x: midRadius * cos midAngle
    , y: midRadius * sin midAngle
    }

sliceColor :: String -> String
sliceColor "accepted" = blue
sliceColor "rejected" = red
sliceColor _ = "#999"

miniDonutChart :: MiniDonutConfig -> Number -> Boolean -> Tree Unit
miniDonutChart config acceptedPercent isWinner =
  let
    sliceData :: Array SliceData
    sliceData =
      [ { label: "rejected", percent: 100.0 - acceptedPercent }
      , { label: "accepted", percent: acceptedPercent }
      ]
    slices = pie _.percent sliceData
    indexedSlices = mapWithIndex (\i s -> { index: i, slice: s }) slices
    arcGen = configureArc config.innerRadius config.outerRadius
    centerX = config.size / 2.0
    centerY = config.size / 2.0
    highlightOpacity = if isWinner then 0.6 else 0.0

    mkArcElem :: { index :: Int, slice :: PieSlice SliceData } -> Tree Unit
    mkArcElem { slice } =
      let
        centroid = arcCentroid config.innerRadius config.outerRadius slice
      in
        T.elem Group [ attr "class" $ text "arc" ]
          `T.withChildren`
            [ T.elem Path
                [ path $ text (arcPath_ arcGen (sliceToDatum slice))
                , fill $ text (sliceColor slice.datum.label)
                , stroke $ text "#2C3E50"
                , strokeWidth $ num 1.0
                ]
            , T.elem Text
                [ x $ num centroid.x
                , y $ num centroid.y
                , textAnchor $ text "middle"
                , fontSize $ num config.labelSize
                , fill $ text "#34495e"
                , textContent $ text (show (Int.round slice.datum.percent) <> "%")
                ]
            ]
  in
    T.elem SVG
      [ width $ num config.size
      , height $ num config.size
      , viewBox 0.0 0.0 config.size config.size
      , attr "class" $ text "mini-donut-svg"
      ]
      `T.withChildren`
        [ T.elem Group
            [ transform $ text ("translate(" <> show centerX <> "," <> show centerY <> ")") ]
            `T.withChildren`
              ( [ T.elem Circle
                    [ cx $ num 0.0
                    , cy $ num 0.0
                    , r $ num config.highlightRadius
                    , fill $ text "none"
                    , stroke $ text "#333"
                    , strokeWidth $ num 4.0
                    , attr "stroke-opacity" $ num highlightOpacity
                    ]
                ]
                  <> map mkArcElem indexedSlices
              )
        ]

-- =============================================================================
-- Table Row Data
-- =============================================================================

type TableRowData =
  { department :: String
  , maleApplied :: Number
  , femaleApplied :: Number
  , maleAdmitted :: Number
  , femaleAdmitted :: Number
  , maleRate :: Number
  , femaleRate :: Number
  , maleWins :: Boolean
  , femaleWins :: Boolean
  }

buildRowData :: DerivedData -> Array TableRowData
buildRowData derived =
  let
    easyMaleRate = rates.male.easy * 100.0
    easyFemaleRate = rates.female.easy * 100.0
    hardMaleRate = rates.male.hard * 100.0
    hardFemaleRate = rates.female.hard * 100.0
    combinedMaleRate = derived.combined.male * 100.0
    combinedFemaleRate = derived.combined.female * 100.0

    combinedMaleApplied = derived.departments.easy.male.applied + derived.departments.hard.male.applied
    combinedFemaleApplied = derived.departments.easy.female.applied + derived.departments.hard.female.applied
    combinedMaleAdmitted = derived.departments.easy.male.admitted + derived.departments.hard.male.admitted
    combinedFemaleAdmitted = derived.departments.easy.female.admitted + derived.departments.hard.female.admitted
  in
    [ { department: "\"Easy\""
      , maleApplied: derived.departments.easy.male.applied
      , femaleApplied: derived.departments.easy.female.applied
      , maleAdmitted: derived.departments.easy.male.admitted
      , femaleAdmitted: derived.departments.easy.female.admitted
      , maleRate: easyMaleRate
      , femaleRate: easyFemaleRate
      , maleWins: easyMaleRate > easyFemaleRate
      , femaleWins: easyFemaleRate > easyMaleRate
      }
    , { department: "\"Hard\""
      , maleApplied: derived.departments.hard.male.applied
      , femaleApplied: derived.departments.hard.female.applied
      , maleAdmitted: derived.departments.hard.male.admitted
      , femaleAdmitted: derived.departments.hard.female.admitted
      , maleRate: hardMaleRate
      , femaleRate: hardFemaleRate
      , maleWins: hardMaleRate > hardFemaleRate
      , femaleWins: hardFemaleRate > hardMaleRate
      }
    , { department: "Combined"
      , maleApplied: combinedMaleApplied
      , femaleApplied: combinedFemaleApplied
      , maleAdmitted: combinedMaleAdmitted
      , femaleAdmitted: combinedFemaleAdmitted
      , maleRate: combinedMaleRate
      , femaleRate: combinedFemaleRate
      , maleWins: combinedMaleRate > combinedFemaleRate
      , femaleWins: combinedFemaleRate > combinedMaleRate
      }
    ]

-- =============================================================================
-- Rendering Functions
-- =============================================================================

-- | Render a bar chart into a container
renderBar :: String -> Number -> Gender -> D3v2M Unit
renderBar containerId value gender = do
  container <- select containerId :: _ (D3v2Selection_ SEmpty Element Unit)
  _ <- renderTree container (barChart defaultBarConfig value gender)
  pure unit

-- | Render a mini donut into a container
renderDonut :: String -> Number -> Boolean -> D3v2M Unit
renderDonut containerId rate isWinner = do
  container <- select containerId :: _ (D3v2Selection_ SEmpty Element Unit)
  _ <- renderTree container (miniDonutChart defaultMiniDonutConfig rate isWinner)
  pure unit

-- | Initialize the data table (render all SVG elements)
initDataTable :: DerivedData -> Effect Unit
initDataTable derived = do
  let rows = buildRowData derived
  -- Clear all containers first
  for_ (mapWithIndex (\i _ -> i) rows) \idx -> do
    let prefix = "#row-" <> show idx
    clearContainer (prefix <> "-male-applied")
    clearContainer (prefix <> "-female-applied")
    clearContainer (prefix <> "-male-admitted")
    clearContainer (prefix <> "-female-admitted")
    clearContainer (prefix <> "-male-rate")
    clearContainer (prefix <> "-female-rate")
  -- Then render
  runD3v2M do
    for_ (mapWithIndex (\i row -> { idx: i, row }) rows) \{ idx, row } -> do
      let prefix = "row-" <> show idx
      -- Bar charts
      renderBar ("#" <> prefix <> "-male-applied") row.maleApplied Male
      renderBar ("#" <> prefix <> "-female-applied") row.femaleApplied Female
      renderBar ("#" <> prefix <> "-male-admitted") row.maleAdmitted Male
      renderBar ("#" <> prefix <> "-female-admitted") row.femaleAdmitted Female
      -- Donut charts
      renderDonut ("#" <> prefix <> "-male-rate") row.maleRate row.maleWins
      renderDonut ("#" <> prefix <> "-female-rate") row.femaleRate row.femaleWins

-- | Update the data table (re-render SVG elements with new data)
updateDataTable :: DerivedData -> Effect Unit
updateDataTable = initDataTable -- For now, just re-render everything

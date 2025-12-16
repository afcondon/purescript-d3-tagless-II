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
import PSD3.Expr.Integration (v3Attr, v3AttrStr)
import PSD3.Expr.Expr (lit, str)
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
      [ v3Attr "width" (lit config.width)
      , v3Attr "height" (lit config.height)
      , v3AttrStr "viewBox" (str ("0 0 " <> show config.width <> " " <> show config.height))
      , v3AttrStr "class" (str "bar-chart-svg")
      ]
      `T.withChildren`
        [ T.elem Rect
            [ v3Attr "width" (lit config.width)
            , v3Attr "height" (lit config.height)
            , v3AttrStr "fill" (str "none")
            , v3AttrStr "stroke" (str gray)
            , v3Attr "stroke-width" (lit 2.0)
            ]
        , T.elem Rect
            [ v3Attr "width" (lit barWidth)
            , v3Attr "height" (lit config.height)
            , v3AttrStr "fill" (str fillColor)
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
        T.elem Group [ v3AttrStr "class" (str "arc") ]
          `T.withChildren`
            [ T.elem Path
                [ v3AttrStr "d" (str (arcPath_ arcGen (sliceToDatum slice)))
                , v3AttrStr "fill" (str (sliceColor slice.datum.label))
                , v3AttrStr "stroke" (str "#2C3E50")
                , v3Attr "stroke-width" (lit 1.0)
                ]
            , T.elem Text
                [ v3Attr "x" (lit centroid.x)
                , v3Attr "y" (lit centroid.y)
                , v3AttrStr "text-anchor" (str "middle")
                , v3Attr "font-size" (lit config.labelSize)
                , v3AttrStr "fill" (str "#34495e")
                , v3AttrStr "textContent" (str (show (Int.round slice.datum.percent) <> "%"))
                ]
            ]
  in
    T.elem SVG
      [ v3Attr "width" (lit config.size)
      , v3Attr "height" (lit config.size)
      , v3AttrStr "viewBox" (str ("0 0 " <> show config.size <> " " <> show config.size))
      , v3AttrStr "class" (str "mini-donut-svg")
      ]
      `T.withChildren`
        [ T.elem Group
            [ v3AttrStr "transform" (str ("translate(" <> show centerX <> "," <> show centerY <> ")")) ]
            `T.withChildren`
              ( [ T.elem Circle
                    [ v3Attr "cx" (lit 0.0)
                    , v3Attr "cy" (lit 0.0)
                    , v3Attr "r" (lit config.highlightRadius)
                    , v3AttrStr "fill" (str "none")
                    , v3AttrStr "stroke" (str "#333")
                    , v3Attr "stroke-width" (lit 4.0)
                    , v3Attr "stroke-opacity" (lit highlightOpacity)
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

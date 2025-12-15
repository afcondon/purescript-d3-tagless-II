-- | Donut Chart - Acceptance Rate Visualization
-- |
-- | Shows the famous 44% male vs 35% female acceptance rates
-- | that triggered the Berkeley discrimination investigation.
module D3.Viz.Simpsons.DonutChart
  ( donutChart
  , donutChartArcs
  , donutChartLabel
  , prepareDonutData
  , DonutConfig
  , DonutData
  , defaultConfig
  , IndexedSlice(..)
  , SliceData
  ) where

import Prelude

import D3.Viz.Simpsons.Pie (PieSlice, pie)
import D3.Viz.Simpsons.Types (black, blue, red)
import Data.Array (mapWithIndex)
import Data.Int (round) as Int
import Data.Number (cos, sin)
import PSD3.Internal.FFI (ArcGenerator_, arcGenerator_, arcPath_, setArcInnerRadius_, setArcOuterRadius_)
import PSD3.Internal.Types (Datum_)
-- v3 Integration: all attributes via v3Attr/v3AttrStr (no ToAttr typeclass)
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3.AST (Tree)
import PSD3.AST as T
import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Configuration
-- =============================================================================

type DonutConfig =
  { outerRadius :: Number
  , innerRadius :: Number
  , centerX :: Number
  , centerY :: Number
  , showLabels :: Boolean
  }

defaultConfig :: DonutConfig
defaultConfig =
  { outerRadius: 60.0
  , innerRadius: 40.0
  , centerX: 70.0
  , centerY: 70.0
  , showLabels: true
  }

-- =============================================================================
-- Data Types
-- =============================================================================

type SliceData =
  { label :: String    -- "accepted" or "rejected"
  , percent :: Number  -- 0-100
  }

-- | Indexed slice for unique keys in data join
newtype IndexedSlice = IndexedSlice { index :: Int, slice :: PieSlice SliceData }

instance Eq IndexedSlice where
  eq (IndexedSlice a) (IndexedSlice b) = a.index == b.index

instance Ord IndexedSlice where
  compare (IndexedSlice a) (IndexedSlice b) = compare a.index b.index

-- =============================================================================
-- Arc Generator Helpers
-- =============================================================================

-- | Configure arc generator with inner/outer radius
configureArc :: Number -> Number -> ArcGenerator_
configureArc innerR outerR =
  setArcOuterRadius_
    (setArcInnerRadius_ (arcGenerator_ unit) innerR)
    outerR

-- | Convert PieSlice to Datum_ for arc generator
-- | D3's arc expects { startAngle, endAngle, innerRadius?, outerRadius? }
sliceToDatum :: PieSlice SliceData -> Datum_
sliceToDatum slice = unsafeCoerce
  { startAngle: slice.startAngle
  , endAngle: slice.endAngle
  }

-- | Calculate centroid of arc for label positioning
-- | Returns (x, y) at the middle of the arc, halfway between inner and outer radius
arcCentroid :: Number -> Number -> PieSlice SliceData -> { x :: Number, y :: Number }
arcCentroid innerR outerR slice =
  let
    midAngle = (slice.startAngle + slice.endAngle) / 2.0 - 1.5707963267948966 -- subtract pi/2 to align with SVG coords
    midRadius = (innerR + outerR) / 2.0
  in
    { x: midRadius * cos midAngle
    , y: midRadius * sin midAngle
    }

-- | Color for slice based on accepted/rejected
sliceColor :: String -> String
sliceColor "accepted" = blue
sliceColor "rejected" = red
sliceColor _ = "#999"

-- =============================================================================
-- Donut Chart Tree
-- =============================================================================

-- | Data for a complete donut chart (slices + label)
type DonutData =
  { slices :: Array IndexedSlice
  , label :: String
  , config :: DonutConfig
  }

-- | Prepare donut data from acceptance percentage
prepareDonutData :: DonutConfig -> Number -> String -> DonutData
prepareDonutData config acceptedPercent label =
  let
    sliceData :: Array SliceData
    sliceData =
      [ { label: "rejected", percent: 100.0 - acceptedPercent }
      , { label: "accepted", percent: acceptedPercent }
      ]
    slices = pie _.percent sliceData
    indexedSlices = mapWithIndex (\i s -> IndexedSlice { index: i, slice: s }) slices
  in
    { slices: indexedSlices, label, config }

-- | Create a donut chart showing acceptance rate
-- |
-- | Returns a tree that renders arcs for the slices.
-- | The label is rendered separately to avoid datum type conflicts.
donutChartArcs :: DonutData -> Tree IndexedSlice
donutChartArcs donutData =
  let
    config = donutData.config
    arcGen = configureArc config.innerRadius config.outerRadius
  in
    T.joinData "arcs" "g" donutData.slices \(IndexedSlice { slice }) ->
      T.elem Group
        [ v3AttrStr "class" (str "arc") ]
        `T.withChildren`
          [ T.elem Path
              [ v3AttrStr "d" (str (arcPath_ arcGen (sliceToDatum slice)))
              , v3AttrStr "fill" (str (sliceColor slice.datum.label))
              , v3AttrStr "stroke" (str black)
              , v3Attr "stroke-width" (lit 1.0)
              ]
          , T.elem Text
              [ v3AttrStr "transform" (str ("translate(" <> show (arcCentroid config.innerRadius config.outerRadius slice).x <> "," <> show (arcCentroid config.innerRadius config.outerRadius slice).y <> ")"))
              , v3AttrStr "dy" (str "0.35em")
              , v3AttrStr "text-anchor" (str "middle")
              , v3AttrStr "fill" (str black)
              , v3Attr "font-size" (lit 12.0)
              , v3AttrStr "textContent" (str (show (Int.round slice.datum.percent) <> "%"))
              ]
          ]

-- | Create the static label element for a donut chart
donutChartLabel :: DonutData -> Tree Unit
donutChartLabel donutData =
  T.elem Text
    [ v3AttrStr "dy" (str "0.35em")
    , v3AttrStr "text-anchor" (str "middle")
    , v3AttrStr "fill" (str black)
    , v3Attr "font-size" (lit 14.0)
    , v3AttrStr "transform" (str ("translate(0," <> show (donutData.config.outerRadius + 20.0) <> ")"))
    , v3AttrStr "textContent" (str donutData.label)
    ]

-- | Convenience: Full donut chart (requires Effect to render in two phases)
-- | For pure Tree usage, use donutChartArcs and donutChartLabel separately
donutChart :: DonutConfig -> Number -> String -> { arcs :: Tree IndexedSlice, label :: Tree Unit, centerTransform :: String }
donutChart config acceptedPercent label =
  let
    donutData = prepareDonutData config acceptedPercent label
    centerTransform = "translate(" <> show config.centerX <> "," <> show config.centerY <> ")"
  in
    { arcs: donutChartArcs donutData
    , label: donutChartLabel donutData
    , centerTransform
    }

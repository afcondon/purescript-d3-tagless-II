-- | PSD3v3 Update Pattern Example
-- |
-- | Demonstrates integration of v3 attribute DSL with the v2 selection
-- | system's enter/update/exit pattern. Shows how polymorphic expressions
-- | compile to runtime attributes for DOM manipulation.
module Test.PSD3v3.UpdatePatternExample where

import Prelude hiding (add)

import Data.Array ((..), length)
import Data.Foldable (foldl, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- v3 DSL
import PSD3v3.Expr (class NumExpr, class BoolExpr, class CompareExpr, class StringExpr, ifThenElse)
import PSD3v3.Expr as E
import PSD3v3.Datum (class DatumExpr, field, index)
import PSD3v3.Sugar ((*:), (<.), n, s)

-- Interpreters
import PSD3v3.Interpreter.Eval (EvalD, runEvalD)
import PSD3v3.Interpreter.CodeGen (CodeGen, runCodeGen)

-- =============================================================================
-- Data Types
-- =============================================================================

-- | Bar chart data point
type BarData =
  { label :: String
  , value :: Number
  , category :: String
  }

type BarDataRow = (label :: String, value :: Number, category :: String)

-- Chart dimensions
chartWidth :: Number
chartWidth = 400.0

chartHeight :: Number
chartHeight = 200.0

barPadding :: Number
barPadding = 4.0

-- =============================================================================
-- Sample Data Sets (simulating update pattern)
-- =============================================================================

-- Initial dataset
dataset1 :: Array BarData
dataset1 =
  [ { label: "A", value: 30.0, category: "primary" }
  , { label: "B", value: 80.0, category: "secondary" }
  , { label: "C", value: 45.0, category: "primary" }
  , { label: "D", value: 60.0, category: "secondary" }
  ]

-- Updated dataset (B updated, E enters, C exits)
dataset2 :: Array BarData
dataset2 =
  [ { label: "A", value: 30.0, category: "primary" }
  , { label: "B", value: 95.0, category: "primary" }  -- Updated value and category
  , { label: "D", value: 60.0, category: "secondary" }
  , { label: "E", value: 70.0, category: "primary" }   -- New entry
  ]

-- =============================================================================
-- Field Accessors
-- =============================================================================

barLabel :: forall repr. DatumExpr repr BarDataRow => repr String
barLabel = field (Proxy :: Proxy "label")

barValue :: forall repr. DatumExpr repr BarDataRow => repr Number
barValue = field (Proxy :: Proxy "value")

barCategory :: forall repr. DatumExpr repr BarDataRow => repr String
barCategory = field (Proxy :: Proxy "category")

-- =============================================================================
-- v3 Attribute Expressions (polymorphic!)
-- =============================================================================

-- | Bar X position based on index
-- | barX = index * (barWidth + padding)
barX :: forall repr. NumExpr repr => DatumExpr repr BarDataRow => repr Number
barX = indexNum *: (barWidthVal + barPadding)
  where
    barWidthVal = chartWidth / 5.0 - barPadding
    indexNum :: forall r. NumExpr r => DatumExpr r BarDataRow => r Number
    indexNum = E.mul (n 1.0) (unsafeCoerceField index)

-- | Bar width (fixed for this example)
barWidth :: forall repr. NumExpr repr => repr Number
barWidth = n (chartWidth / 5.0 - barPadding)

-- | Bar height from value (scaled to chart height)
barHeight :: forall repr. NumExpr repr => DatumExpr repr BarDataRow => repr Number
barHeight = barValue *: (chartHeight / 100.0)

-- | Bar Y position (from top, so chart flips)
-- | barY = chartHeight - barHeight
barY :: forall repr. NumExpr repr => DatumExpr repr BarDataRow => repr Number
barY = E.sub (n chartHeight) barHeight

-- | Bar fill color based on value threshold
-- | (In real code, you'd have a numeric category field or use the v2 attribute system)
barFill :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => StringExpr repr => DatumExpr repr BarDataRow => repr String
barFill = ifThenElse
  (barValue <. n 50.0)          -- Low values
  (s "#3498db")                  -- Blue
  (ifThenElse
    (barValue <. n 75.0)         -- Medium values
    (s "#2ecc71")                -- Green
    (s "#e74c3c"))               -- Red for high values

-- | Enter state fill (green for new bars)
enterFill :: forall repr. StringExpr repr => repr String
enterFill = s "#2ecc71"

-- | Exit state fill (gray for departing bars)
exitFill :: forall repr. StringExpr repr => repr String
exitFill = s "#95a5a6"

-- =============================================================================
-- Demo
-- =============================================================================

runExample :: Effect Unit
runExample = do
  log "\n=== PSD3v3 Update Pattern Example ==="

  log "\n--- v3 Attribute Expressions ---"
  log "barX = index * barWidth"
  log $ "  CodeGen: " <> runCodeGen (barX :: CodeGen Number)

  log "\nbarHeight = value * (chartHeight / 100)"
  log $ "  CodeGen: " <> runCodeGen (barHeight :: CodeGen Number)

  log "\nbarFill = if value < 50 then blue else (if value < 75 then green else red)"
  log $ "  CodeGen: " <> runCodeGen (barFill :: CodeGen String)

  log "\n--- Dataset 1 (Initial) ---"
  log "Label    x       y   width  height  fill"
  log "──────────────────────────────────────────────"
  for_ (indexedArray dataset1) \(Tuple i d) -> do
    let x = runEvalD (barX :: EvalD BarData Number) d i
    let y = runEvalD (barY :: EvalD BarData Number) d i
    let w = runEvalD (barWidth :: EvalD BarData Number) d i
    let h = runEvalD (barHeight :: EvalD BarData Number) d i
    let f = runEvalD (barFill :: EvalD BarData String) d i
    log $ padRight 8 d.label <>
          padLeft 6 (show x) <> " " <>
          padLeft 6 (show y) <> " " <>
          padLeft 6 (show w) <> " " <>
          padLeft 6 (show h) <> "  " <>
          f

  log "\n--- Dataset 2 (Updated) ---"
  log "Label    x       y   width  height  fill      Status"
  log "────────────────────────────────────────────────────────"
  for_ (indexedArray dataset2) \(Tuple i d) -> do
    let x = runEvalD (barX :: EvalD BarData Number) d i
    let y = runEvalD (barY :: EvalD BarData Number) d i
    let w = runEvalD (barWidth :: EvalD BarData Number) d i
    let h = runEvalD (barHeight :: EvalD BarData Number) d i
    let f = runEvalD (barFill :: EvalD BarData String) d i
    let status = case d.label of
          "E" -> "ENTER (new)"
          "B" -> "UPDATE (value: 80→95, category: secondary→primary)"
          _ -> "unchanged"
    log $ padRight 8 d.label <>
          padLeft 6 (show x) <> " " <>
          padLeft 6 (show y) <> " " <>
          padLeft 6 (show w) <> " " <>
          padLeft 6 (show h) <> "  " <>
          padRight 9 f <> " " <>
          status

  log "\n--- Key Insight: v3 → v2 Integration ---"
  log "v3 expressions compile to v2 attributes:"
  log ""
  log "  -- v3 expression (polymorphic)"
  log "  barFill :: forall repr. ... => repr String"
  log "  barFill = ifThenElse (value < 50) blue (ifThenElse (value < 75) green red)"
  log ""
  log "  -- v2 attribute (for selection operations)"
  log "  fillAttr :: Attribute BarData"
  log "  fillAttr = v3Attr \"fill\" barFill"
  log ""
  log "  -- Usage in enter/update/exit"
  log "  append Rect [v3Attr \"x\" barX, v3Attr \"fill\" barFill] enterSelection"
  log "  setAttrs [v3Attr \"fill\" barFill] updateSelection"

  log "\n--- Generated SVG (both datasets) ---"
  log $ generateBarChartSVG dataset1 dataset2

-- =============================================================================
-- SVG Generation
-- =============================================================================

generateBarChartSVG :: Array BarData -> Array BarData -> String
generateBarChartSVG data1 data2 =
  """<svg width="850" height="250" xmlns="http://www.w3.org/2000/svg">
  <style>
    text { font: 11px sans-serif; }
    .title { font-weight: bold; }
  </style>

  <!-- Dataset 1 -->
  <g transform="translate(20, 30)">
    <text class="title" x="0" y="-10">Dataset 1 (Initial)</text>""" <>
  foldl (\acc (Tuple i d) ->
    let x = runEvalD (barX :: EvalD BarData Number) d i
        y = runEvalD (barY :: EvalD BarData Number) d i
        w = runEvalD (barWidth :: EvalD BarData Number) d i
        h = runEvalD (barHeight :: EvalD BarData Number) d i
        f = runEvalD (barFill :: EvalD BarData String) d i
    in acc <>
       "\n    <rect x=\"" <> show x <> "\" y=\"" <> show y <>
       "\" width=\"" <> show w <> "\" height=\"" <> show h <>
       "\" fill=\"" <> f <> "\" />" <>
       "\n    <text x=\"" <> show (x + w/2.0) <> "\" y=\"" <> show (chartHeight + 15.0) <>
       "\" text-anchor=\"middle\">" <> d.label <> "</text>"
  ) "" (indexedArray data1) <>
  """
  </g>

  <!-- Dataset 2 -->
  <g transform="translate(450, 30)">
    <text class="title" x="0" y="-10">Dataset 2 (After Update)</text>""" <>
  foldl (\acc (Tuple i d) ->
    let x = runEvalD (barX :: EvalD BarData Number) d i
        y = runEvalD (barY :: EvalD BarData Number) d i
        w = runEvalD (barWidth :: EvalD BarData Number) d i
        h = runEvalD (barHeight :: EvalD BarData Number) d i
        f = runEvalD (barFill :: EvalD BarData String) d i
        -- Highlight enter/update
        strokeWidth = if d.label == "E" then "3" else "1"
        stroke = case d.label of
          "E" -> "#2ecc71"  -- Green border for enter
          "B" -> "#f39c12"  -- Orange border for updated
          _ -> "none"
    in acc <>
       "\n    <rect x=\"" <> show x <> "\" y=\"" <> show y <>
       "\" width=\"" <> show w <> "\" height=\"" <> show h <>
       "\" fill=\"" <> f <> "\" stroke=\"" <> stroke <>
       "\" stroke-width=\"" <> strokeWidth <> "\" />" <>
       "\n    <text x=\"" <> show (x + w/2.0) <> "\" y=\"" <> show (chartHeight + 15.0) <>
       "\" text-anchor=\"middle\">" <> d.label <> "</text>"
  ) "" (indexedArray data2) <>
  """
  </g>

  <!-- Legend -->
  <g transform="translate(20, 240)">
    <rect x="0" y="-8" width="12" height="12" fill="none" stroke="#2ecc71" stroke-width="2" />
    <text x="18" y="0">Enter (new)</text>
    <rect x="100" y="-8" width="12" height="12" fill="none" stroke="#f39c12" stroke-width="2" />
    <text x="118" y="0">Update (changed)</text>
    <text x="230" y="0" fill="#95a5a6">C was removed (exit)</text>
  </g>
</svg>"""

-- =============================================================================
-- Helpers
-- =============================================================================

indexedArray :: forall a. Array a -> Array (Tuple Int a)
indexedArray arr = foldl (\acc x -> acc <> [Tuple (length acc) x]) [] arr

padRight :: Int -> String -> String
padRight n str =
  let len = stringLength str
  in str <> replicate (max 0 (n - len)) " "

padLeft :: Int -> String -> String
padLeft n str =
  let len = stringLength str
  in replicate (max 0 (n - len)) " " <> str

replicate :: Int -> String -> String
replicate n str = foldl (\acc _ -> acc <> str) "" (1..n)

stringLength :: String -> Int
stringLength = go 0
  where
    go acc "" = acc
    go acc s = go (acc + 1) (dropFirst s)

foreign import dropFirst :: String -> String
foreign import unsafeCoerceField :: forall repr a b. repr a -> repr b

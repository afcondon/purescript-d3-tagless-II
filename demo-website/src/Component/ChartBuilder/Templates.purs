module Component.ChartBuilder.Templates where

import Prelude

import Component.ChartBuilder.Types (ChartType(..))

-- =============================================================================
-- Emmet Recipes
-- =============================================================================

-- | Get Emmet recipe for a chart type
getRecipe :: ChartType -> String
getRecipe = case _ of
  BarChart -> barChartRecipe
  LineChart -> lineChartRecipe
  ScatterChart -> scatterChartRecipe

-- | Bar chart recipe
barChartRecipe :: String
barChartRecipe = """j(Point)>r[x@index,y:y,width=40,height:y,fill=steelblue]"""

-- | Line chart recipe
lineChartRecipe :: String
lineChartRecipe = """p[d:pathData,stroke=steelblue,stroke-width=2,fill=none]"""

-- | Scatter chart recipe
scatterChartRecipe :: String
scatterChartRecipe = """j(Point)>c[cx:x,cy:y,r=5,fill=coral,opacity=0.7]"""

-- =============================================================================
-- Explanations
-- =============================================================================

-- | Get explanation for a chart type
getExplanation :: ChartType -> String
getExplanation = case _ of
  BarChart -> barChartExplanation
  LineChart -> lineChartExplanation
  ScatterChart -> scatterChartExplanation

barChartExplanation :: String
barChartExplanation = """Bar chart with rectangles positioned by index.
• j(Point): Join data to rectangles (r)
• x@index: Position bars by their array index
• y:y: Use y field for position and height
• width=40: Fixed bar width in pixels"""

lineChartExplanation :: String
lineChartExplanation = """Line chart using a single path element.
• p: Single path for all data points
• d:pathData: SVG path data (computed from points)
• No data join (j) needed - path connects all points
• stroke-width=2: Line thickness"""

scatterChartExplanation :: String
scatterChartExplanation = """Scatter plot with circles for each data point.
• j(Point): Join creates one circle (c) per point
• cx:x, cy:y: Position from x and y fields
• r=5: Fixed circle radius
• opacity=0.7: Semi-transparent for overlaps"""

-- =============================================================================
-- Tips & Hints
-- =============================================================================

-- | General tips for using the chart builder
generalTips :: Array String
generalTips =
  [ "Click chart type tabs to see different recipes"
  , "Copy recipe and modify attributes to experiment"
  , "Element types: c (circle), r (rect), p (path), g (group), t (text)"
  , "Use @index for evenly-spaced positioning"
  , "Use :fieldName to bind attributes to data fields"
  , "Static values use = like fill=steelblue or r=5"
  ]

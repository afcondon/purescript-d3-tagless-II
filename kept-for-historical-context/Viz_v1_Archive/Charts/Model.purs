module D3.Viz.Charts.Model where

import Prelude
import Data.Int as Int

-- Simple data point for 2D charts
type DataPoint = {
    x :: Number
  , y :: Number
}

-- Sample data for line chart - simulated wave pattern
sineWaveData :: Array DataPoint
sineWaveData = [
    { x: 0.0, y: 100.0 }
  , { x: 5.0, y: 125.0 }
  , { x: 10.0, y: 145.0 }
  , { x: 15.0, y: 150.0 }
  , { x: 20.0, y: 145.0 }
  , { x: 25.0, y: 125.0 }
  , { x: 30.0, y: 100.0 }
  , { x: 35.0, y: 75.0 }
  , { x: 40.0, y: 55.0 }
  , { x: 45.0, y: 50.0 }
  , { x: 50.0, y: 55.0 }
  , { x: 55.0, y: 75.0 }
  , { x: 60.0, y: 100.0 }
]

-- Sample data for bar chart - monthly sales
monthlySales :: Array DataPoint
monthlySales = [
    { x: 0.0, y: 30.0 }
  , { x: 1.0, y: 45.0 }
  , { x: 2.0, y: 60.0 }
  , { x: 3.0, y: 55.0 }
  , { x: 4.0, y: 70.0 }
  , { x: 5.0, y: 65.0 }
  , { x: 6.0, y: 80.0 }
  , { x: 7.0, y: 75.0 }
  , { x: 8.0, y: 90.0 }
  , { x: 9.0, y: 85.0 }
  , { x: 10.0, y: 95.0 }
  , { x: 11.0, y: 100.0 }
]

-- Sample data for scatterplot - random points
scatterData :: Array DataPoint
scatterData = [
    { x: 10.0, y: 20.0 }
  , { x: 25.0, y: 45.0 }
  , { x: 40.0, y: 30.0 }
  , { x: 55.0, y: 60.0 }
  , { x: 70.0, y: 50.0 }
  , { x: 85.0, y: 75.0 }
  , { x: 100.0, y: 65.0 }
  , { x: 115.0, y: 90.0 }
  , { x: 130.0, y: 80.0 }
  , { x: 145.0, y: 95.0 }
]

-- Anscombe's Quartet - four datasets with identical statistical properties
-- but very different distributions when visualized
type QuartetData = {
    dataset1 :: Array DataPoint
  , dataset2 :: Array DataPoint
  , dataset3 :: Array DataPoint
  , dataset4 :: Array DataPoint
}

anscombesQuartet :: QuartetData
anscombesQuartet = {
    dataset1: [
        { x: 10.0, y: 8.04 }
      , { x: 8.0, y: 6.95 }
      , { x: 13.0, y: 7.58 }
      , { x: 9.0, y: 8.81 }
      , { x: 11.0, y: 8.33 }
      , { x: 14.0, y: 9.96 }
      , { x: 6.0, y: 7.24 }
      , { x: 4.0, y: 4.26 }
      , { x: 12.0, y: 10.84 }
      , { x: 7.0, y: 4.82 }
      , { x: 5.0, y: 5.68 }
    ]
  , dataset2: [
        { x: 10.0, y: 9.14 }
      , { x: 8.0, y: 8.14 }
      , { x: 13.0, y: 8.74 }
      , { x: 9.0, y: 8.77 }
      , { x: 11.0, y: 9.26 }
      , { x: 14.0, y: 8.10 }
      , { x: 6.0, y: 6.13 }
      , { x: 4.0, y: 3.10 }
      , { x: 12.0, y: 9.13 }
      , { x: 7.0, y: 7.26 }
      , { x: 5.0, y: 4.74 }
    ]
  , dataset3: [
        { x: 10.0, y: 7.46 }
      , { x: 8.0, y: 6.77 }
      , { x: 13.0, y: 12.74 }
      , { x: 9.0, y: 7.11 }
      , { x: 11.0, y: 7.81 }
      , { x: 14.0, y: 8.84 }
      , { x: 6.0, y: 6.08 }
      , { x: 4.0, y: 5.39 }
      , { x: 12.0, y: 8.15 }
      , { x: 7.0, y: 6.42 }
      , { x: 5.0, y: 5.73 }
    ]
  , dataset4: [
        { x: 8.0, y: 6.58 }
      , { x: 8.0, y: 5.76 }
      , { x: 8.0, y: 7.71 }
      , { x: 8.0, y: 8.84 }
      , { x: 8.0, y: 8.47 }
      , { x: 8.0, y: 7.04 }
      , { x: 8.0, y: 5.25 }
      , { x: 19.0, y: 12.50 }
      , { x: 8.0, y: 5.56 }
      , { x: 8.0, y: 7.91 }
      , { x: 8.0, y: 6.89 }
    ]
}

toNumber :: Int -> Number
toNumber = Int.toNumber

-- Grouped Bar Chart Data (State population by age group)
type GroupedBarData = {
    state :: String
  , age :: String
  , population :: Number
}

-- Sample data for grouped bar chart - Top 6 states by population with age breakdowns
groupedBarData :: Array GroupedBarData
groupedBarData = [
    -- California
    { state: "CA", age: "<10", population: 5038433.0 }
  , { state: "CA", age: "10-19", population: 5055706.0 }
  , { state: "CA", age: "20-29", population: 5656118.0 }
  , { state: "CA", age: "30-39", population: 5485729.0 }
  , { state: "CA", age: "40-49", population: 4897920.0 }
  , { state: "CA", age: "50-59", population: 4826838.0 }
  , { state: "CA", age: "60-69", population: 3561336.0 }
  , { state: "CA", age: "70-79", population: 2087132.0 }
  , { state: "CA", age: "≥80", population: 1322344.0 }
    -- Texas
  , { state: "TX", age: "<10", population: 4131018.0 }
  , { state: "TX", age: "10-19", population: 3870247.0 }
  , { state: "TX", age: "20-29", population: 4057645.0 }
  , { state: "TX", age: "30-39", population: 4038722.0 }
  , { state: "TX", age: "40-49", population: 3605075.0 }
  , { state: "TX", age: "50-59", population: 3439143.0 }
  , { state: "TX", age: "60-69", population: 2454512.0 }
  , { state: "TX", age: "70-79", population: 1390737.0 }
  , { state: "TX", age: "≥80", population: 883990.0 }
    -- Florida
  , { state: "FL", age: "<10", population: 2223676.0 }
  , { state: "FL", age: "10-19", population: 2264020.0 }
  , { state: "FL", age: "20-29", population: 2655856.0 }
  , { state: "FL", age: "30-39", population: 2673043.0 }
  , { state: "FL", age: "40-49", population: 2677768.0 }
  , { state: "FL", age: "50-59", population: 2891117.0 }
  , { state: "FL", age: "60-69", population: 2721540.0 }
  , { state: "FL", age: "70-79", population: 1787456.0 }
  , { state: "FL", age: "≥80", population: 1215746.0 }
    -- New York
  , { state: "NY", age: "<10", population: 2302097.0 }
  , { state: "NY", age: "10-19", population: 2427916.0 }
  , { state: "NY", age: "20-29", population: 2886270.0 }
  , { state: "NY", age: "30-39", population: 2820345.0 }
  , { state: "NY", age: "40-49", population: 2502932.0 }
  , { state: "NY", age: "50-59", population: 2671684.0 }
  , { state: "NY", age: "60-69", population: 2015910.0 }
  , { state: "NY", age: "70-79", population: 1163431.0 }
  , { state: "NY", age: "≥80", population: 805237.0 }
    -- Pennsylvania
  , { state: "PA", age: "<10", population: 1431845.0 }
  , { state: "PA", age: "10-19", population: 1538087.0 }
  , { state: "PA", age: "20-29", population: 1729085.0 }
  , { state: "PA", age: "30-39", population: 1582018.0 }
  , { state: "PA", age: "40-49", population: 1609509.0 }
  , { state: "PA", age: "50-59", population: 1808373.0 }
  , { state: "PA", age: "60-69", population: 1565529.0 }
  , { state: "PA", age: "70-79", population: 993225.0 }
  , { state: "PA", age: "≥80", population: 702076.0 }
    -- Illinois
  , { state: "IL", age: "<10", population: 1575308.0 }
  , { state: "IL", age: "10-19", population: 1724580.0 }
  , { state: "IL", age: "20-29", population: 1880143.0 }
  , { state: "IL", age: "30-39", population: 1750121.0 }
  , { state: "IL", age: "40-49", population: 1587652.0 }
  , { state: "IL", age: "50-59", population: 1724730.0 }
  , { state: "IL", age: "60-69", population: 1308208.0 }
  , { state: "IL", age: "70-79", population: 760186.0 }
  , { state: "IL", age: "≥80", population: 505635.0 }
]

-- Multi-Line Chart Data (Unemployment rate over time)
type MultiLineData = {
    series :: String
  , date :: String
  , value :: Number
}

-- Sample data for multi-line chart - Unemployment rates for 4 metro areas
multiLineData :: Array MultiLineData
multiLineData = [
    -- San Francisco
    { series: "San Francisco", date: "2000-01", value: 3.5 }
  , { series: "San Francisco", date: "2002-01", value: 5.2 }
  , { series: "San Francisco", date: "2004-01", value: 4.8 }
  , { series: "San Francisco", date: "2006-01", value: 3.9 }
  , { series: "San Francisco", date: "2008-01", value: 4.2 }
  , { series: "San Francisco", date: "2010-01", value: 9.8 }
  , { series: "San Francisco", date: "2012-01", value: 7.5 }
  , { series: "San Francisco", date: "2013-01", value: 6.2 }
    -- New York
  , { series: "New York", date: "2000-01", value: 5.3 }
  , { series: "New York", date: "2002-01", value: 7.1 }
  , { series: "New York", date: "2004-01", value: 6.2 }
  , { series: "New York", date: "2006-01", value: 4.9 }
  , { series: "New York", date: "2008-01", value: 5.4 }
  , { series: "New York", date: "2010-01", value: 9.4 }
  , { series: "New York", date: "2012-01", value: 8.8 }
  , { series: "New York", date: "2013-01", value: 8.2 }
    -- Detroit
  , { series: "Detroit", date: "2000-01", value: 4.2 }
  , { series: "Detroit", date: "2002-01", value: 6.8 }
  , { series: "Detroit", date: "2004-01", value: 7.5 }
  , { series: "Detroit", date: "2006-01", value: 7.2 }
  , { series: "Detroit", date: "2008-01", value: 7.9 }
  , { series: "Detroit", date: "2010-01", value: 14.3 }
  , { series: "Detroit", date: "2012-01", value: 11.1 }
  , { series: "Detroit", date: "2013-01", value: 9.9 }
    -- Miami
  , { series: "Miami", date: "2000-01", value: 5.7 }
  , { series: "Miami", date: "2002-01", value: 7.3 }
  , { series: "Miami", date: "2004-01", value: 5.9 }
  , { series: "Miami", date: "2006-01", value: 4.1 }
  , { series: "Miami", date: "2008-01", value: 6.2 }
  , { series: "Miami", date: "2010-01", value: 11.8 }
  , { series: "Miami", date: "2012-01", value: 9.6 }
  , { series: "Miami", date: "2013-01", value: 8.1 }
]

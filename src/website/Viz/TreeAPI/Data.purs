module D3.Viz.TreeAPI.Data where

import Prelude

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

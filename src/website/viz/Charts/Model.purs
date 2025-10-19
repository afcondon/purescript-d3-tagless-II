module D3.Examples.Charts.Model where

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

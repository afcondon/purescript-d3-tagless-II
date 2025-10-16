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

toNumber :: Int -> Number
toNumber = Int.toNumber

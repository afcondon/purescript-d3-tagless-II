module Test.Main where

import Prelude

import DataViz.Layout.Sankey.CompareOrderings (compareOrderings)
import Effect (Effect)
import Effect.Console (log)

-- | UK energy data (from D3 Sankey example)
-- | This is the same data used in the demo-website
energyCSV :: String
energyCSV = """source,target,value
Agricultural 'waste',Bio-conversion,124.729
Bio-conversion,Liquid,0.597
Bio-conversion,Losses,26.862
Bio-conversion,Solid,280.322
Bio-conversion,Gas,81.144
Biofuel imports,Liquid,35
Biomass imports,Solid,35
Coal imports,Coal,11.606
Coal reserves,Coal,63.965
Coal,Solid,75.571
District heating,Industry,10.639
District heating,Heating and cooling - commercial,22.505
District heating,Heating and cooling - homes,46.184
Electricity grid,Over generation / exports,104.453
Electricity grid,Heating and cooling - homes,113.726
Electricity grid,H2 conversion,27.14
Electricity grid,Industry,342.165
Electricity grid,Road transport,37.797
Electricity grid,Agriculture,4.412
Electricity grid,Heating and cooling - commercial,40.858
Electricity grid,Losses,56.691
Electricity grid,Rail transport,7.863
Electricity grid,Lighting & appliances - commercial,90.008
Electricity grid,Lighting & appliances - homes,93.494
Gas imports,Ngas,40.719
Gas reserves,Ngas,82.233
Gas,Heating and cooling - commercial,0.129
Gas,Losses,1.401
Gas,Thermal generation,151.891
Gas,Agriculture,2.096
Gas,Industry,48.58
Geothermal,Electricity grid,7.013
H2 conversion,H2,20.897
H2 conversion,Losses,6.242
H2,Road transport,20.897
Hydro,Electricity grid,6.995
Liquid,Industry,121.066
Liquid,International shipping,128.69
Liquid,Road transport,135.835
Liquid,Domestic aviation,14.458
Liquid,International aviation,206.267
Liquid,Agriculture,3.64
Liquid,National navigation,33.218
Liquid,Rail transport,4.413
Marine algae,Bio-conversion,4.375
Ngas,Gas,122.952
Nuclear,Thermal generation,839.978
Oil imports,Oil,504.287
Oil reserves,Oil,107.703
Oil,Liquid,611.99
Other waste,Solid,56.587
Other waste,Bio-conversion,77.81
Pumped heat,Heating and cooling - homes,193.026
Pumped heat,Heating and cooling - commercial,70.672
Solar PV,Electricity grid,59.901
Solar Thermal,Heating and cooling - homes,19.263
Solar,Solar Thermal,19.263
Solar,Solar PV,59.901
Solid,Agriculture,0.882
Solid,Thermal generation,400.12
Solid,Industry,46.477
Thermal generation,Electricity grid,525.531
Thermal generation,Losses,787.129
Thermal generation,District heating,79.329
Tidal,Electricity grid,9.452
UK land based bioenergy,Bio-conversion,182.01
Wave,Electricity grid,19.013
Wind,Electricity grid,289.366"""

main :: Effect Unit
main = do
  log "======================================"
  log "Sankey Ordering Algorithm Comparison"
  log "======================================"
  log ""

  let result = compareOrderings energyCSV 900.0 500.0

  log $ "Graph size: " <> show result.numNodes <> " nodes, " <> show result.numLinks <> " links, " <> show result.numLayers <> " layers"
  log ""
  log "Weighted Crossing Results:"
  log "--------------------------------------"
  log $ "D3-style Heuristic:     " <> show result.heuristicCrossing
  log $ "Markov Chain (Stage 1): " <> show result.markovChainCrossing
  log $ "After Refinement:       " <> show result.refinedCrossing
  log $ "Best during Refinement: " <> show result.bestRefinedCrossing
  log ""
  log $ "Refinement iterations: " <> show result.refinementIterations
  log ""

  -- Calculate improvements
  let
    improvement1 = if result.heuristicCrossing > 0.0
      then (result.heuristicCrossing - result.markovChainCrossing) / result.heuristicCrossing * 100.0
      else 0.0
    improvement2 = if result.heuristicCrossing > 0.0
      then (result.heuristicCrossing - result.refinedCrossing) / result.heuristicCrossing * 100.0
      else 0.0

  log "Improvement over Heuristic:"
  log $ "  Markov Chain:     " <> show improvement1 <> "%"
  log $ "  After Refinement: " <> show improvement2 <> "%"
  log ""
  log "======================================"

module D3.Viz.ModuleGraph.Model where

import Prelude

import Data.Nullable (Nullable)
import Data.String.CodeUnits as String
import Foreign.Object (Object)
import PSD3.Data.Node (D3Link_Unswizzled)

-- | Raw module graph data from spago
type SpagoModuleGraph = Object ModuleNode

-- | A module and its dependencies
type ModuleNode =
  { depends :: Array String
  }

-- | Node for force simulation
type ModuleSimNode =
  { id :: String
  , name :: String
  , group :: Int      -- Group by top-level module (PSD3, Component, D3, etc.)
  , index :: Int
  , x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , fx :: Nullable Number
  , fy :: Nullable Number
  }

-- | Processed graph ready for visualization
type ModuleGraph =
  { nodes :: Array ModuleSimNode
  , links :: Array D3Link_Unswizzled
  }

-- | Determine group number based on module prefix
moduleGroup :: String -> Int
moduleGroup name
  | startsWith "PSD3.Internal" name = 0
  | startsWith "PSD3v2" name = 1
  | startsWith "PSD3.Layout" name = 2
  | startsWith "PSD3.Shared" name = 3
  | startsWith "PSD3." name = 4
  | startsWith "Component." name = 5
  | startsWith "D3.Viz" name = 6
  | startsWith "D3." name = 7
  | startsWith "Utility" name = 8
  | otherwise = 9

startsWith :: String -> String -> Boolean
startsWith prefix str =
  let prefixLen = String.length prefix
  in String.take prefixLen str == prefix

module D3.Viz.Spago.GitMetrics where

import Prelude

import Effect (Effect)

-- | Load git metrics from JSON file
-- | Should be called during initialization
-- | Returns a promise that resolves when loaded
foreign import loadGitMetrics_ :: Effect Unit

-- | Get a normalized metric (0-1) for a module
-- | metricType can be: "commits", "recency", "age", "authors", "churn", "size", "recentActivity"
foreign import getModuleMetric_ :: String -> String -> Number

-- | Check if metrics have been loaded
foreign import metricsLoaded_ :: Boolean

-- | Convenience functions for specific metrics
getCommitMetric :: String -> Number
getCommitMetric moduleName = getModuleMetric_ moduleName "commits"

getRecencyMetric :: String -> Number
getRecencyMetric moduleName = getModuleMetric_ moduleName "recency"

getAgeMetric :: String -> Number
getAgeMetric moduleName = getModuleMetric_ moduleName "age"

getAuthorMetric :: String -> Number
getAuthorMetric moduleName = getModuleMetric_ moduleName "authors"

getChurnMetric :: String -> Number
getChurnMetric moduleName = getModuleMetric_ moduleName "churn"

getSizeMetric :: String -> Number
getSizeMetric moduleName = getModuleMetric_ moduleName "size"

getRecentActivityMetric :: String -> Number
getRecentActivityMetric moduleName = getModuleMetric_ moduleName "recentActivity"

-- | Color by options for the UI
data ColorByOption
  = ColorByGroup      -- Default: color by package/cluster
  | ColorByDepth      -- Color by tree depth
  | ColorByCommits    -- Color by number of commits (churn indicator)
  | ColorByRecency    -- Color by how recently modified
  | ColorByAge        -- Color by file age
  | ColorByAuthors    -- Color by number of authors
  | ColorByChurn      -- Color by total lines changed
  | ColorBySize       -- Color by file size
  | ColorByReplay     -- Color by git replay state (animated)

derive instance Eq ColorByOption

instance Show ColorByOption where
  show ColorByGroup = "Package"
  show ColorByDepth = "Tree Depth"
  show ColorByCommits = "Commits"
  show ColorByRecency = "Recency"
  show ColorByAge = "Age"
  show ColorByAuthors = "Authors"
  show ColorByChurn = "Churn"
  show ColorBySize = "Size"
  show ColorByReplay = "Replay"

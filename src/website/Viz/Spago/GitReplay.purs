module D3.Viz.Spago.GitReplay where

import Prelude

import Effect (Effect)
import Data.Nullable (Nullable)

-- | Load commit timeline data from JSON
foreign import loadTimeline_ :: Effect Unit

-- | Check if timeline is loaded
foreign import timelineLoaded_ :: Effect Boolean

-- | Get total number of commits
foreign import getCommitCount_ :: Effect Int

-- | Get current commit index
foreign import getCurrentIndex_ :: Effect Int

-- | Start replay animation
foreign import startReplay_ :: Effect Unit

-- | Stop/pause replay animation
foreign import stopReplay_ :: Effect Unit

-- | Reset replay to beginning
foreign import resetReplay_ :: Effect Unit

-- | Set replay speed (ms per commit)
foreign import setReplaySpeed_ :: Int -> Effect Unit

-- | Set fade steps (commits to fade from hot to cool)
foreign import setFadeSteps_ :: Int -> Effect Unit

-- | Check if replay is playing
foreign import isPlaying_ :: Effect Boolean

-- | Jump to specific commit index
foreign import jumpToCommit_ :: Int -> Effect Unit

-- | Get replay color for a module name
foreign import getReplayColor_ :: String -> String

-- | Set callback for replay state updates
foreign import setUpdateCallback_ :: Effect Unit -> Effect Unit

-- | Current commit info (nullable if at end or not loaded)
type CommitInfo =
  { hash :: String
  , date :: String
  , author :: String
  , subject :: String
  }

foreign import getCurrentCommit_ :: Effect (Nullable CommitInfo)

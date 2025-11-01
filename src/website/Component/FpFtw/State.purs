module PSD3.FpFtw.State where

import Prelude

-- | Component state for FP FTW examples
type State =
  { currentExample :: String  -- Which example is currently selected
  }

-- | Initial state
initialState :: forall i. i -> State
initialState _ =
  { currentExample: "intro"
  }

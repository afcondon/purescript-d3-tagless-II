module Stories.Prism where

import Prelude

highlightBlockSynchronous :: String -> Unit
highlightBlockSynchronous selector = highlightBlock_ selector false

foreign import highlightBlock_ :: String -> Boolean -> Unit
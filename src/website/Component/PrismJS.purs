module PSD3.PrismJS where

import Prelude

import Effect (Effect)

-- | Call Prism.highlightAll() to syntax highlight code blocks
foreign import highlightAll :: Effect Unit

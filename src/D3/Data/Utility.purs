module Utility where

import Prelude

import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

getWindowWidthHeight :: Effect (Tuple Number Number)
getWindowWidthHeight = do
  win <- window
  w <- innerWidth win
  h <- innerHeight win
  pure $ Tuple (toNumber w) (toNumber h)

module D3.Data.Zoom where

import D3.Data.Foreign (D3Selection_)

-- Zoom types
type ZoomConfig_ = {
    extent      :: Array (Array Number)
  , scaleExtent :: Array Number
  , qualifier   :: String
  , target      :: D3Selection_
}
type ZoomConfigDefault_ = {
    scaleExtent :: Array Number
  , qualifier   :: String
  , target      :: D3Selection_
}

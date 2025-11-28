module PSD3.Shared.ZoomSticker where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | A small visual indicator that a visualization supports zooming
-- | Usage: Add `ZoomSticker.render` to any zoomable visualization container
render :: forall w i. HH.HTML w i
render =
  HH.div
    [ HP.classes [ HH.ClassName "zoom-sticker" ] ]
    [ HH.span
        [ HP.classes [ HH.ClassName "zoom-sticker__icon" ] ]
        [ HH.text "🔍" ]
    , HH.span
        [ HP.classes [ HH.ClassName "zoom-sticker__text" ] ]
        [ HH.text "Try zooming!" ]
    ]

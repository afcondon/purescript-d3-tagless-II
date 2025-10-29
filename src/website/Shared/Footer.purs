module PSD3.Shared.Footer where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

-- | Simple footer with horizontal rule and acknowledgements link
-- | Usage: Add `Footer.render` at the bottom of any page's render function
render :: forall w i. HH.HTML w i
render =
  HH.footer
    [ HP.classes [ HH.ClassName "site-footer" ] ]
    [ HH.hr
        [ HP.classes [ HH.ClassName "site-footer__rule" ] ]
    , HH.div
        [ HP.classes [ HH.ClassName "site-footer__content" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Acknowledgements
            , HP.classes [ HH.ClassName "site-footer__link" ]
            ]
            [ HH.text "Acknowledgements" ]
        ]
    ]

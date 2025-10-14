-- | This module contains UI components originally from the Ocelot library
-- | (https://github.com/citizennet/purescript-ocelot). The code has been
-- | vendored into this project as Ocelot is no longer actively maintained.
module DemoApp.UI.Builder where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import DemoApp.UI.Properties (IProp, (<&>))

blockBuilder
  :: ∀ r p i
   . ( Array (IProp r i)
       -> Array (HH.HTML p i)
       -> HH.HTML p i
     )
  -> Array HH.ClassName
  -> Array (IProp r i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
blockBuilder elem classes iprops =
  elem $ [ HP.classes classes ] <&> iprops

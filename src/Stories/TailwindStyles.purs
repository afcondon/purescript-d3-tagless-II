module Stories.Tailwind.Styles where

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import Prelude


apply :: forall r i. String ->  HP.IProp (class :: String | r) i
apply = HP.class_ <<< HH.ClassName
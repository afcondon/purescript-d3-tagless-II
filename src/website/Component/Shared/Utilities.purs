module PSD3.Shared.Utilities where -- shared

import Prelude

import Data.Array (singleton)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as RH

classed :: forall r i. String ->  HP.IProp (class :: String | r) i
classed = HP.class_ <<< HH.ClassName

tailwindClass :: forall r i. String ->  HP.IProp (class :: String | r) i
tailwindClass = classed

blurbParagraphs :: forall t16 t19 t20. Functor t16 => t16 String -> t16 (HTML t19 t20)
blurbParagraphs texts = 
  (HH.p [ HP.classes [ HH.ClassName "m-2" ] ]) <$> ((singleton <<< HH.text) <$> texts)

syntaxHighlightedCode :: forall t2 t3. String -> Array (HTML t2 t3)
syntaxHighlightedCode codetext =
  [ HH.pre
    [ HP.class_ $ HH.ClassName "language-haskell" ]  -- Using Haskell syntax for PureScript
    [ HH.code_ [ RH.render_ $ highlightString_ codetext ] ]
  ]

highlightBlockSynchronous :: String -> Unit
highlightBlockSynchronous selector = highlightBlock_ selector false

foreign import highlightBlock_ :: String -> Boolean -> Unit
foreign import highlightString_ :: String -> String

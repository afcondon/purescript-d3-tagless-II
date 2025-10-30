module PSD3.Shared.Mermaid
  ( mermaidDiagram
  , triggerMermaidRendering
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Helper function to create a simple mermaid diagram div
-- | This will be processed by calling triggerMermaidRendering
mermaidDiagram :: forall w i. String -> Maybe String -> HH.HTML w i
mermaidDiagram code className =
  HH.div
    [ HP.classes $
        [ HH.ClassName "mermaid-container" ] <>
        case className of
          Nothing -> []
          Just cls -> [ HH.ClassName cls ]
    ]
    [ HH.div
        [ HP.classes [ HH.ClassName "mermaid" ] ]
        [ HH.text code ]  -- Mermaid will read and replace this
    ]

-- | Trigger Mermaid rendering after diagrams are in the DOM
-- | Call this in your component's Initialize action or after navigation
triggerMermaidRendering :: forall m. MonadEffect m => m Unit
triggerMermaidRendering = do
  H.liftEffect $ logMessage "PureScript: triggerMermaidRendering called"
  H.liftEffect renderMermaidDiagrams_

-- | Foreign function to log to console
foreign import logMessage :: String -> Effect Unit

-- | Foreign function to render Mermaid diagrams
foreign import renderMermaidDiagrams_ :: Effect Unit
module PSD3.Understanding.UnderstandingCapabilities where

import Prelude

import CodeSnippet (codeSnippet, triggerPrismHighlighting)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Website.Types (Section(..))
import Type.Proxy (Proxy(..))

type State = Unit

data Action = Initialize

type Slots =
  ( docsHeader :: forall q. H.Slot q Void Unit
  )

_docsHeader = Proxy :: Proxy "docsHeader"

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just UnderstandingSection }

    -- Main content
    , HH.div
        [ HP.classes [ HH.ClassName "explanation-content" ] ]
        [ -- Page title
          HH.h1
            [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "Capabilities & Interpreters" ]

        , HH.p_ [ HH.text "The SimulationM2 monad looks like:" ]
        -- SNIPPET: simulationM2Class src/lib/PSD3/Capabilities/Simulation.purs 254-320
        , codeSnippet "simulationM2Class" "haskell"
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> triggerPrismHighlighting

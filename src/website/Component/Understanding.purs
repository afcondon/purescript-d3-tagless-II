module PSD3.Understanding where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Understanding page state
type State = Unit

-- | Understanding page actions
data Action = Initialize

-- | Understanding page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "understanding-page" ] ]
    [ HH.header
        [ HP.classes [ HH.ClassName "docs-header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "docs-header__title" ] ]
            [ HH.text "Understanding PSD3" ]
        , HH.p
            [ HP.classes [ HH.ClassName "docs-header__subtitle" ] ]
            [ HH.text "Conceptual overview of the project" ]
        ]

    , HH.main
        [ HP.classes [ HH.ClassName "understanding-content" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "understanding-grid" ] ]
            [ renderConceptCard
                "Interpreters"
                "How the Finally Tagless pattern enables multiple interpretations of visualization specifications"
                "TODO: Content needed - explain finally tagless, D3v2 interpreter, Mermaid interpreter, etc."
            , renderConceptCard
                "Composability"
                "Building complex visualizations from simple, reusable components"
                "TODO: Content needed - explain composition, higher-order functions, etc."
            , renderConceptCard
                "Grammar"
                "The declarative grammar of graphics and how it maps to PSD3's API"
                "TODO: Content needed - explain selection model, data binding, attributes, etc."
            , renderConceptCard
                "Attributes"
                "The contravariant functor pattern for type-safe, composable styling"
                "TODO: Content needed - explain contravariant, datum functions, static values, etc."
            ]
        ]
    ]

-- | Render a concept card
renderConceptCard :: forall w i. String -> String -> String -> HH.HTML w i
renderConceptCard title description placeholder =
  HH.div
    [ HP.classes [ HH.ClassName "understanding-card" ] ]
    [ HH.h2
        [ HP.classes [ HH.ClassName "understanding-card__title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "understanding-card__description" ] ]
        [ HH.text description ]
    , HH.div
        [ HP.classes [ HH.ClassName "understanding-card__placeholder" ] ]
        [ HH.text placeholder ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

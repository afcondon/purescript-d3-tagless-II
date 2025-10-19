module PSD3.Components.ExampleCard where

import Prelude

import PSD3.Types (ExampleMetadata, difficultyEmoji, categoryToString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Foldable (intercalate)

type Input = ExampleMetadata

data Action = Click

type Slots :: forall k. Row k
type Slots = ()

-- | The ExampleCard component
component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
  }

render :: forall m. ExampleMetadata -> H.ComponentHTML Action Slots m
render example =
  HH.div
    [ HP.classes [ HH.ClassName "example-card" ]
    , HE.onClick \_ -> Click
    ]
    [ -- Thumbnail
      HH.div
        [ HP.classes [ HH.ClassName "example-card__thumbnail" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "example-card__thumbnail-placeholder" ] ]
            [ HH.text example.title ]
        ]
    , -- Content
      HH.div
        [ HP.classes [ HH.ClassName "example-card__content" ] ]
        [ -- Header
          HH.div
            [ HP.classes [ HH.ClassName "example-card__header" ] ]
            [ HH.h3
                [ HP.classes [ HH.ClassName "example-card__title" ] ]
                [ HH.text example.title ]
            , HH.span
                [ HP.classes [ HH.ClassName "example-card__difficulty" ] ]
                [ HH.text $ difficultyEmoji example.difficulty ]
            ]
        , -- Description
          HH.p
            [ HP.classes [ HH.ClassName "example-card__description" ] ]
            [ HH.text example.description ]
        , -- Footer
          HH.div
            [ HP.classes [ HH.ClassName "example-card__footer" ] ]
            [ -- Category badge
              HH.span
                [ HP.classes [ HH.ClassName "example-card__category" ] ]
                [ HH.text $ categoryToString example.category ]
            , -- Tags
              HH.div
                [ HP.classes [ HH.ClassName "example-card__tags" ] ]
                (example.tags <#> \tag ->
                  HH.span
                    [ HP.classes [ HH.ClassName "example-card__tag" ] ]
                    [ HH.text tag ]
                )
            , -- Badges
              HH.div
                [ HP.classes [ HH.ClassName "example-card__badges" ] ]
                [ if example.hasInteractivity
                    then HH.span
                      [ HP.classes [ HH.ClassName "example-card__badge" ]
                      , HP.title "Interactive example"
                      ]
                      [ HH.text "⚡" ]
                    else HH.text ""
                , if example.hasComparison
                    then HH.span
                      [ HP.classes [ HH.ClassName "example-card__badge" ]
                      , HP.title "Has D3 JavaScript comparison"
                      ]
                      [ HH.text "📊" ]
                    else HH.text ""
                ]
            ]
        ]
    ]

handleAction :: forall o m. Action -> H.HalogenM ExampleMetadata Action Slots o m Unit
handleAction Click = do
  -- Click is handled by parent (Gallery) component
  pure unit

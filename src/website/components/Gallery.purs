module PSD3.Components.Gallery where

import Prelude

import PSD3.Types (Category(..), ExampleId, ExampleMetadata)
import PSD3.Data.Examples (allExamples, getExamplesByCategory)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

type State = {
  filterCategory :: Maybe Category
, searchTerm :: String
}

data Action
  = SetFilter (Maybe Category)
  | SetSearch String
  | SelectExample ExampleId

type Slots = ()

type Output = ExampleId  -- Notify parent when example is selected

-- | The Gallery component
component :: forall q i m. H.Component q i Output m
component = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
  }

initialState :: State
initialState =
  { filterCategory: Nothing
  , searchTerm: ""
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "gallery" ] ]
    [ -- Header
      HH.div
        [ HP.classes [ HH.ClassName "gallery__header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "gallery__title" ] ]
            [ HH.text "Examples Gallery" ]
        , HH.p
            [ HP.classes [ HH.ClassName "gallery__subtitle" ] ]
            [ HH.text "Explore type-safe, composable data visualizations" ]
        ]

    , -- Filters
      HH.div
        [ HP.classes [ HH.ClassName "gallery__filters" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "gallery__filter-group" ] ]
            [ HH.label
                [ HP.classes [ HH.ClassName "gallery__filter-label" ] ]
                [ HH.text "Category:" ]
            , HH.button
                [ HP.classes $ categoryButtonClasses Nothing state.filterCategory
                , HE.onClick \_ -> SetFilter Nothing
                ]
                [ HH.text "All" ]
            , HH.button
                [ HP.classes $ categoryButtonClasses (Just BasicChart) state.filterCategory
                , HE.onClick \_ -> SetFilter (Just BasicChart)
                ]
                [ HH.text "Basic Charts" ]
            , HH.button
                [ HP.classes $ categoryButtonClasses (Just AdvancedLayout) state.filterCategory
                , HE.onClick \_ -> SetFilter (Just AdvancedLayout)
                ]
                [ HH.text "Advanced Layouts" ]
            , HH.button
                [ HP.classes $ categoryButtonClasses (Just Interactive) state.filterCategory
                , HE.onClick \_ -> SetFilter (Just Interactive)
                ]
                [ HH.text "Interactive" ]
            , HH.button
                [ HP.classes $ categoryButtonClasses (Just Interpreter) state.filterCategory
                , HE.onClick \_ -> SetFilter (Just Interpreter)
                ]
                [ HH.text "Interpreters" ]
            , HH.button
                [ HP.classes $ categoryButtonClasses (Just Application) state.filterCategory
                , HE.onClick \_ -> SetFilter (Just Application)
                ]
                [ HH.text "Applications" ]
            ]
        ]

    , -- Grid
      HH.div
        [ HP.classes [ HH.ClassName "gallery__grid" ] ]
        (filteredExamples state <#> renderExampleCard)
    ]

-- | Helper to get class list for category buttons
categoryButtonClasses :: Maybe Category -> Maybe Category -> Array HH.ClassName
categoryButtonClasses buttonCat selectedCat =
  [ HH.ClassName "gallery__filter-button"
  , if buttonCat == selectedCat
      then HH.ClassName "gallery__filter-button--active"
      else HH.ClassName ""
  ]

-- | Filter examples based on state
filteredExamples :: State -> Array ExampleMetadata
filteredExamples state =
  case state.filterCategory of
    Nothing -> allExamples
    Just cat -> getExamplesByCategory cat

-- | Render a single example card
renderExampleCard :: forall m. ExampleMetadata -> H.ComponentHTML Action Slots m
renderExampleCard example =
  HH.div
    [ HP.classes [ HH.ClassName "gallery__card-wrapper" ]
    , HE.onClick \_ -> SelectExample example.id
    ]
    [ -- Thumbnail
      HH.div
        [ HP.classes [ HH.ClassName "gallery__card-thumbnail" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "gallery__card-thumbnail-placeholder" ] ]
            [ HH.text example.title ]
        ]
    , -- Content
      HH.div
        [ HP.classes [ HH.ClassName "gallery__card-content" ] ]
        [ -- Title with difficulty
          HH.div
            [ HP.classes [ HH.ClassName "gallery__card-header" ] ]
            [ HH.h3
                [ HP.classes [ HH.ClassName "gallery__card-title" ] ]
                [ HH.text example.title ]
            ]
        , -- Description
          HH.p
            [ HP.classes [ HH.ClassName "gallery__card-description" ] ]
            [ HH.text example.description ]
        , -- Tags
          HH.div
            [ HP.classes [ HH.ClassName "gallery__card-tags" ] ]
            (example.tags <#> \tag ->
              HH.span
                [ HP.classes [ HH.ClassName "gallery__card-tag" ] ]
                [ HH.text tag ]
            )
        ]
    ]

handleAction :: forall m. Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  SetFilter category -> do
    H.modify_ _ { filterCategory = category }

  SetSearch term -> do
    H.modify_ _ { searchTerm = term }

  SelectExample exampleId -> do
    -- Notify parent component
    H.raise exampleId

module Component.Tour.TourWealthHealth where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import Effect.Aff (Milliseconds(..), delay)
import Component.WealthHealth.WealthHealth as WealthHealth
import Type.Proxy (Proxy(..))

-- | Tour page state
type State = Unit

-- | Tour page actions
data Action = Initialize

-- | Slots for child components
type Slots = ( wealthHealth :: forall q. H.Slot q Void Unit )

_wealthHealth :: Proxy "wealthHealth"
_wealthHealth = Proxy

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)
    pure unit

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourWealthHealth
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "5b. The Wealth & Health of Nations" ]
            , HH.p_
                [ HH.text "This animated bubble chart shows the relationship between income and life expectancy across 180 nations from 1800 to 2009. Inspired by Hans Rosling's famous TED talk and Mike Bostock's D3 adaptation." ]
            ]

        -- Section 1: The Visualization
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-1"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Interactive Animation" ]
            , HH.p_
                [ HH.text "Each circle represents a nation. The x-axis shows income per person (logarithmic scale), the y-axis shows life expectancy, and circle size represents population. Colors indicate geographic regions." ]
            , HH.p_
                [ HH.text "Use the controls to scrub through time manually, or press Play to animate through history. Hover over circles to see details about each nation." ]
            , HH.div
                [ HP.classes [ HH.ClassName "wealth-health-embed" ] ]
                [ HH.slot_ _wealthHealth unit WealthHealth.component unit ]
            ]

        -- Section 2: Key Insights
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-2"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "What the Data Shows" ]
            , HH.p_
                [ HH.text "Several patterns emerge from this visualization:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "In 1800, most nations clustered in the lower-left: short lives and low incomes" ]
                , HH.li_ [ HH.text "The 20th century saw dramatic improvements in both wealth and health" ]
                , HH.li_ [ HH.text "Today's developing nations follow paths similar to earlier industrialized nations" ]
                , HH.li_ [ HH.text "Population growth (circle size) exploded in the 20th century" ]
                ]
            ]

        -- Section 3: Technical Notes
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-3"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Implementation Notes" ]
            , HH.p_
                [ HH.text "This visualization demonstrates several PS<$>D3 features:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "TreeAPI for declarative SVG structure (axes, grid, labels)" ]
                , HH.li_ [ HH.text "Data joins for binding nation data to circles" ]
                , HH.li_ [ HH.text "Library tooltips for rich hover information" ]
                , HH.li_ [ HH.text "Halogen integration for UI controls and animation" ]
                ]
            , HH.p_
                [ HH.text "The data comes from Gapminder and is interpolated for years with missing values, following Mike Bostock's original implementation." ]
            ]
        ]
    ]

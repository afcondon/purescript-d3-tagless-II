module PSD3.Understanding.BubbleChart where -- Understanding

import Prelude

import D3.Viz.BubbleChart as Bubble
import PSD3.Interpreter.D3 (eval_D3M)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- | BubbleChart page state
type State = Unit

-- | BubbleChart page actions
data Action = Initialize

-- | Child component slots
type Slots = ( sectionNav :: forall q. H.Slot q Void Unit )

_sectionNav = Proxy :: Proxy "sectionNav"

-- | BubbleChart page component
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
    [ -- TOC Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel" ] ]
        [ HH.img
            [ HP.src "bookmark.jpeg"
            , HP.alt ""
            , HP.classes [ HH.ClassName "toc-panel__bookmark-pin" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Contents" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "toc-panel__content" ] ]
                [ HH.nav
                    [ HP.classes [ HH.ClassName "toc-nav" ] ]
                    [ HH.a [ HP.href "#section-1", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "1. Hierarchical Data" ]
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: UnderstandingSection
        , currentRoute: BubbleChart
        , sectionPages:
            [ { route: About, label: "About" }
            , { route: Tutorial, label: "Tutorial" }
            , { route: SimpleCharts, label: "Simple Charts" }
            , { route: ChordDiagram, label: "Chord Diagram" }
            , { route: BubbleChart, label: "Bubble Chart" }
            , { route: SankeyDiagram, label: "Sankey Diagram" }
            , { route: Hierarchies, label: "Hierarchies" }
            , { route: Interpreters, label: "Interpreters" }
            , { route: CodeExplorer, label: "Code Explorer" }
            ]
        , moduleCategories: Nothing
        }

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Bubble Chart: Circle Packing Layout" ]
        , HH.p_
            [ HH.text "Circle packing displays hierarchical data as nested circles, where the size of each circle represents a quantitative value. It's an efficient way to visualize part-to-whole relationships in hierarchical structures." ]
        , HH.p_
            [ HH.text "This layout is particularly effective for showing relative sizes at multiple levels of hierarchy, making it easy to spot dominant elements and compare proportions across branches." ]
        ]

    -- Section 1: Flare Visualization Library
    , HH.section
        [ HP.id "section-1"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Flare Visualization Library Structure" ]
        , HH.p_
            [ HH.text "This bubble chart shows the structure of the Flare visualization library. Each circle represents a class or package, with the size proportional to the lines of code. Nested circles show the package hierarchy." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "bubble-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "The circle packing algorithm automatically arranges circles to minimize wasted space while maintaining the hierarchical relationships. Colors indicate the depth level in the hierarchy." ]
        ]
    ]

-- Handle actions
handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    jsonData <- H.liftAff Bubble.loadFlareData
    _ <- H.liftEffect $ eval_D3M $ Bubble.draw jsonData "div.bubble-viz"
    pure unit

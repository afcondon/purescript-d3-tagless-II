module PSD3.Understanding.ChordDiagram where -- Understanding

import Prelude

import D3.Viz.ChordDiagram as Chord
import PSD3.Interpreter.D3 (eval_D3M)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.RHSNavigation as RHSNav
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | ChordDiagram page state
type State = Unit

-- | ChordDiagram page actions
data Action = Initialize

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | ChordDiagram page component
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
                    [ HH.a [ HP.href "#section-1", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "1. Programming Concepts" ]
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component ChordDiagram

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Chord Diagram: Visualizing Relationships" ]
        , HH.p_
            [ HH.text "Chord diagrams show relationships and flows between entities in a circular layout. They're particularly effective for displaying interconnected systems, dependencies, or flows between groups." ]
        , HH.p_
            [ HH.text "The thickness of each chord represents the strength of the relationship, while colors help distinguish between different entities or groups." ]
        ]

    -- Section 1: Programming Concepts Chord Diagram
    , HH.section
        [ HP.id "section-1"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Programming Concepts Dependencies" ]
        , HH.p_
            [ HH.text "This chord diagram visualizes the dependencies between fundamental programming concepts. Each arc represents a concept, and the ribbons show how strongly they depend on each other." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "chord-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "The circular layout makes it easy to see both direct dependencies (following a single chord) and the overall pattern of interconnections in the system. Thicker chords indicate stronger dependencies." ]
        ]
    ]

-- Handle actions
handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.liftEffect $ eval_D3M $ Chord.draw Chord.exampleMatrix Chord.exampleLabels "div.chord-viz"
    pure unit

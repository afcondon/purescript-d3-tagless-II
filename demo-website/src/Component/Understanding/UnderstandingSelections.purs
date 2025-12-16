module Component.Understanding.UnderstandingSelections where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

-- | State for the Selections page
type State = Unit

-- | Actions for the Selections page
data Action = Initialize

-- | Selections page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> pure unit

-- | Helper to render an SVG diagram from assets
svgDiagram :: forall w i. String -> String -> HH.HTML w i
svgDiagram src alt =
  HH.img
    [ HP.src $ "assets/diagrams/" <> src
    , HP.alt alt
    , HP.style "max-width: 100%; height: auto;"
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- Header with navigation
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadUnderstanding
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    , HH.main_
        [ -- Introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Selection Phantom Types" ]
            , HH.p_
                [ HH.text "PSD3 uses phantom types to encode the state machine of D3 selections. This is similar to the Indexed Monad pattern - operations are only allowed when the selection is in the correct state, and this is enforced at compile time." ]

            -- Section navigation cards
            , HH.div
                [ HP.classes [ HH.ClassName "section-nav-cards" ] ]
                [ renderNavCard "Selection States" "#selection-states" "Five phantom type states"
                , renderNavCard "State Machine" "#state-machine" "Valid state transitions"
                , renderNavCard "Indexed Monad" "#indexed-monad" "Why this pattern works"
                , renderNavCard "Type Safety" "#type-safety" "Errors at compile time"
                ]
            ]

        -- Section 1: Selection States
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "selection-states"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "The Five Selection States" ]
            , HH.p_
                [ HH.text "A selection in PSD3 carries a phantom type parameter indicating its current state:" ]

            , HH.table
                [ HP.classes [ HH.ClassName "tutorial-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "State" ]
                        , HH.th_ [ HH.text "Meaning" ]
                        , HH.th_ [ HH.text "Valid Operations" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SEmpty" ] ]
                        , HH.td_ [ HH.text "Has parent elements, no data bound" ]
                        , HH.td_ [ HH.text "appendChild, appendData, joinData" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SPending" ] ]
                        , HH.td_ [ HH.text "Has data, needs elements (enter)" ]
                        , HH.td_ [ HH.text "append" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SBoundOwns" ] ]
                        , HH.td_ [ HH.text "Elements with bound data" ]
                        , HH.td_ [ HH.text "setAttrs, appendChildInheriting, merge" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SBoundInherits" ] ]
                        , HH.td_ [ HH.text "Child elements inheriting parent data" ]
                        , HH.td_ [ HH.text "setAttrs" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SExiting" ] ]
                        , HH.td_ [ HH.text "Elements to be removed (exit)" ]
                        , HH.td_ [ HH.text "setAttrsExit, remove" ]
                        ]
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Note: "
                , HH.text "These types are 'phantom' - they exist only at compile time and carry no runtime data."
                ]
            ]

        -- Section 2: State Machine
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "state-machine"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "The Selection State Machine" ]
            , HH.p_
                [ HH.text "Operations transition selections between states. The type system ensures only valid transitions are allowed:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ svgDiagram "selection-state-machine.svg" "Selection state machine diagram" ]

            , HH.p_
                [ HH.text "Key transitions:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "select" ]
                    , HH.text " → SEmpty (entry point)"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "joinData" ]
                    , HH.text " → JoinResult with enter/update/exit"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "append" ]
                    , HH.text " → SPending to SBoundOwns (creates elements)"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "remove" ]
                    , HH.text " → SExiting to done (deletes elements)"
                    ]
                ]
            ]

        -- Section 3: Indexed Monad
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "indexed-monad"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "The Indexed Monad Pattern" ]
            , HH.p_
                [ HH.text "This design is inspired by Indexed Monads (also called Parameterized Monads). Unlike regular monads where bind has type:"
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.code_ [ HH.text "bind :: m a -> (a -> m b) -> m b" ]
                ]

            , HH.p_
                [ HH.text "Indexed monads carry additional type parameters for pre/post state:"
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.code_ [ HH.text "ibind :: m i j a -> (a -> m j k b) -> m i k b" ]
                ]

            -- Note: Indexed monad concept is explained textually, no diagram needed

            , HH.p_
                [ HH.text "In PSD3, each selection operation is typed with its input and output states:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "select :: m (sel SEmpty ...)" ]
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "append :: sel SPending ... -> m (sel SBoundOwns ...)" ]
                    ]
                ]

            , HH.p_
                [ HH.text "Sequencing operations chains their states - the output of one becomes input to the next."
                ]
            ]

        -- Section 4: Type Safety
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "type-safety"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Type Safety in Practice" ]
            , HH.p_
                [ HH.text "Invalid operations are compile-time errors, not runtime bugs:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ svgDiagram "type-safety-errors.svg" "Type safety errors caught at compile time" ]

            , HH.p_
                [ HH.text "Examples of errors caught at compile time:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "setAttrs attrs emptySel" ]
                    , HH.text " - Can't set attrs on SEmpty"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "append Circle boundSel" ]
                    , HH.text " - Can't append to SBoundOwns"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "remove pendingSel" ]
                    , HH.text " - Can't remove SPending (has no elements)"
                    ]
                ]

            , HH.p_
                [ HH.text "Compare to D3.js where these errors would fail silently or cause confusing runtime behavior."
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Benefit: "
                , HH.text "The compiler guides you through the correct sequence of operations."
                ]
            ]

        -- Summary
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Summary" ]
            , HH.p_
                [ HH.text "Selection phantom types provide:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Five states: SEmpty, SPending, SBoundOwns, SBoundInherits, SExiting" ]
                , HH.li_ [ HH.text "State machine enforced by types" ]
                , HH.li_ [ HH.text "Indexed Monad pattern for state transitions" ]
                , HH.li_ [ HH.text "Invalid operation sequences caught at compile time" ]
                , HH.li_ [ HH.text "No more silent failures or undefined behavior" ]
                ]
            ]
        ]
    ]

-- | Render a navigation card
renderNavCard :: forall w i. String -> String -> String -> HH.HTML w i
renderNavCard title href description =
  HH.a
    [ HP.href href
    , HP.classes [ HH.ClassName "section-nav-card" ]
    ]
    [ HH.h3_ [ HH.text title ]
    , HH.p_ [ HH.text description ]
    ]

-- | Render header with navigation
renderHeader :: forall w i. HH.HTML w i
renderHeader =
  HH.header
    [ HP.classes [ HH.ClassName "example-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-header-left" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "example-logo-link" ]
            ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "example-logo" ]
                ]
            ]
        , HH.a
            [ HP.href "#/understanding"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "Understanding" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text "Selection Phantom Types" ]
            ]
        ]
    ]

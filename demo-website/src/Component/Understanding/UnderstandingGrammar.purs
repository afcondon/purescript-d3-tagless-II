module Component.Understanding.UnderstandingGrammar where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

-- | State for the Grammar page
type State = Unit

-- | Actions for the Grammar page
data Action = Initialize

-- | Grammar page component
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
                [ HH.text "The Grammar of D3" ]
            , HH.p_
                [ HH.text "D3 can be understood as an embedded Domain Specific Language for data visualization. While the full API has a large surface area, the essential grammar is remarkably small. PSD3 captures this grammar in a type-safe, composable way." ]

            -- Section navigation cards
            , HH.div
                [ HP.classes [ HH.ClassName "section-nav-cards" ] ]
                [ renderNavCard "Core Primitives" "#core-primitives" "The four essential operations"
                , renderNavCard "Selection States" "#selection-states" "Phantom types for safety"
                , renderNavCard "Static vs GUP" "#static-vs-gup" "One-time vs updating"
                , renderNavCard "TreeAPI" "#tree-api" "Declarative alternative"
                ]
            ]

        -- Section 1: Core Primitives
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "core-primitives"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Core Primitives" ]
            , HH.p_
                [ HH.text "The grammar of D3 visualizations consists of four essential primitives. Everything else is built from these:" ]

            , HH.table
                [ HP.classes [ HH.ClassName "tutorial-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Primitive" ]
                        , HH.th_ [ HH.text "Function" ]
                        , HH.th_ [ HH.text "PSD3 API" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "select" ] ]
                        , HH.td_ [ HH.text "Find an entry point in the DOM" ]
                        , HH.td_
                            [ HH.code_ [ HH.text "select" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "selectAll" ]
                            ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "append" ] ]
                        , HH.td_ [ HH.text "Add a DOM element to a selection" ]
                        , HH.td_
                            [ HH.code_ [ HH.text "appendChild" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "append" ]
                            ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "join" ] ]
                        , HH.td_
                            [ HH.text "For each datum "
                            , HH.strong_ [ HH.text "d" ]
                            , HH.text ", create an element "
                            , HH.strong_ [ HH.text "e" ]
                            ]
                        , HH.td_
                            [ HH.code_ [ HH.text "appendData" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "joinData" ]
                            , HH.text ", "
                            , HH.code_ [ HH.text "renderData" ]
                            ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.em_ [ HH.text "attr" ] ]
                        , HH.td_ [ HH.text "Apply attributes to current elements" ]
                        , HH.td_
                            [ HH.code_ [ HH.text "setAttrs" ]
                            , HH.text " (array of Attribute)"
                            ]
                        ]
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key insight: "
                , HH.text "Attributes can be static values or functions of the datum, enabling Data Driven Documents."
                ]
            ]

        -- Section 2: Selection States
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "selection-states"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Selection States (Phantom Types)" ]
            , HH.p_
                [ HH.text "PSD3 uses phantom types to track what operations are legal on a selection. This catches errors at compile time that D3 would only catch at runtime:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ svgDiagram "selection-state-machine.svg" "Selection state machine diagram" ]

            , HH.table
                [ HP.classes [ HH.ClassName "tutorial-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "State" ]
                        , HH.th_ [ HH.text "Meaning" ]
                        , HH.th_ [ HH.text "Legal Operations" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SEmpty" ] ]
                        , HH.td_ [ HH.text "Selection has elements but no data" ]
                        , HH.td_ [ HH.text "appendChild, appendData, joinData" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SPending" ] ]
                        , HH.td_ [ HH.text "Data waiting for elements (enter)" ]
                        , HH.td_ [ HH.text "append" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "SBoundOwns" ] ]
                        , HH.td_ [ HH.text "Elements with bound data" ]
                        , HH.td_ [ HH.text "setAttrs, appendChildInheriting, merge" ]
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
                [ HH.text "Type safety: "
                , HH.text "Trying to call "
                , HH.code_ [ HH.text "setAttrs" ]
                , HH.text " on an "
                , HH.code_ [ HH.text "SEmpty" ]
                , HH.text " selection is a compile error, not a runtime bug."
                ]
            ]

        -- Section 3: Static vs GUP
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "static-vs-gup"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Static Visualizations vs General Update Pattern" ]
            , HH.p_
                [ HH.text "There are two fundamental patterns for D3 visualizations:" ]

            , HH.h3_ [ HH.text "Static: One-time render" ]
            -- Static pattern is simple enough to explain textually

            , HH.p_
                [ HH.text "Use "
                , HH.code_ [ HH.text "appendData" ]
                , HH.text " for simple visualizations that render once with fixed data. This is perfect for charts, static hierarchies, and basic data displays."
                ]

            , HH.h3_ [ HH.text "GUP: Dynamic updates" ]
            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ svgDiagram "gup-flow.svg" "General Update Pattern flow diagram" ]

            , HH.p_
                [ HH.text "Use "
                , HH.code_ [ HH.text "joinData" ]
                , HH.text " when data changes over time. The join splits data into three disjoint sets:"
                ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Enter" ]
                    , HH.text " - new data items that need elements created"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Update" ]
                    , HH.text " - existing elements that should be updated"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Exit" ]
                    , HH.text " - old elements that should be removed"
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "GUP enables: "
                , HH.text "Animated transitions, live data feeds, user interactions, and smooth state changes."
                ]
            ]

        -- Section 4: TreeAPI
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "tree-api"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "TreeAPI: Declarative Alternative" ]
            , HH.p_
                [ HH.text "For static visualizations, the TreeAPI provides a more declarative, succinct syntax. Instead of imperative method chaining, you declare the desired tree structure." ]

            , HH.p_
                [ HH.text "TreeAPI variants handle different use cases:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "Node" ]
                    , HH.text " - static elements with children"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "Join" ]
                    , HH.text " - data-driven element creation (N elements from N data)"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "NestedJoin" ]
                    , HH.text " - nested data with type decomposition"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "UpdateJoin" ]
                    , HH.text " - declarative GUP with enter/update/exit behaviors"
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Note: "
                , HH.text "TreeAPI excels at static visualizations. For complex interactive visualizations with simulations, the underlying Selection API provides more control."
                ]
            ]

        -- Summary
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Summary" ]
            , HH.p_
                [ HH.text "The D3 grammar in PSD3:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Four primitives: select, append, join, attr" ]
                , HH.li_ [ HH.text "Phantom types enforce correct operation ordering" ]
                , HH.li_ [ HH.text "Static visualizations use appendData or TreeAPI" ]
                , HH.li_ [ HH.text "Dynamic visualizations use joinData with GUP" ]
                , HH.li_ [ HH.text "TreeAPI provides declarative syntax for tree structures" ]
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
                [ HH.text "The Grammar of D3" ]
            ]
        ]
    ]

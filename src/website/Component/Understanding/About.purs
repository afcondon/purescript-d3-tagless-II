module PSD3.Understanding.About where -- Understanding

import Prelude

import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.Charts.Model (anscombesQuartet)
import PSD3.Interpreter.D3 (eval_D3M)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- | About page state
type State = Unit

-- | About page actions
data Action = Initialize

-- | Child component slots
type Slots = ( sectionNav :: forall q. H.Slot q Void Unit )

_sectionNav = Proxy :: Proxy "sectionNav"

-- | About page component
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
      renderTOC
        { title: "Page Contents"
        , items:
            [ { anchor: "heading-0", label: "PS<$>D3", level: 0 }
            , { anchor: "heading-2", label: "What is This Project?", level: 1 }
            , { anchor: "heading-3", label: "Motivation", level: 1 }
            , { anchor: "heading-4", label: "The Data Visualization Process", level: 1 }
            , { anchor: "anscombe", label: "Why Visualize? Anscombe's Quartet", level: 2 }
            , { anchor: "heading-5", label: "Design philosophy of D3*", level: 2 }
            , { anchor: "heading-6", label: "Design philosophy of PS<$>D3", level: 2 }
            , { anchor: "heading-7", label: "Project Goals (and non-Goals)", level: 1 }
            , { anchor: "heading-9", label: "Goals: details", level: 2 }
            , { anchor: "heading-13", label: "Non-goals: details", level: 2 }
            , { anchor: "heading-17", label: "What's a DSL? and what's an eDSL?", level: 1 }
            , { anchor: "heading-20", label: "The grammar of D3...", level: 1 }
            ]
        , image: Just "images/understanding-bookmark-trees.jpeg"
        }

    -- Section Navigation (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: UnderstandingSection
        , currentRoute: About
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

    -- Page content
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ]
            , HP.id "heading-0"
            ]
            [ HH.text "PS<$>D3" ]
        , HH.h2
            [ HP.id "heading-1" ]
            [ HH.text "Interactive Data Visualization in PureScript" ]
        , HH.p_
            [ HH.text "A PureScript embedded "
            , HH.em_ [ HH.text "DSL" ]
            , HH.text " for building "
            , HH.em_ [ HH.text "interactive data visualizations" ]
            , HH.text ", using "
            , HH.em_ [ HH.text "D3.js" ]
            , HH.text " both as inspiration and as an implementation layer under the Foreign Function Interface (FFI)."
            ]
        ]

    -- What is This Project?
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ]
            , HP.id "heading-2"
            ]
            [ HH.text "What is This Project?" ]
        , HH.p_
            [ HH.text "This project demonstrates an embedded DSL for building interactive data visualizations with PureScript. The DSL uses a Finally Tagless approach that allows multiple interpreters for the same \"language\" which allows us to generate working code using an FFI to D3 or documentation and other more complex uses which will be discussed below." ]
        ]

    -- Motivation
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ]
            , HP.id "heading-3"
            ]
            [ HH.text "Motivation" ]
        , HH.p_
            [ HH.text "I have built moderately complex, custom interactive data visualisations in the past both in JavaScript and PureScript, using D3.js. I found that JavaScript generally, and D3 in particular, seemed to work best for visualisations that were less \"app-like\" and more \"chart-like\". What i mean by this is that when the complexity started to rise to the level of a small application and when multiple programmers were involved, or if one had to return to some code after time had elapsed, the whole thing was very brittle and refactoring of it prohibitively difficult." ]
        , HH.p_
            [ HH.text "This could certainly be a \"feature, not a bug\" for some domains of application such as building a big beautiful rich visualisation for a one-off publication such as a New York Times feature. However, when the visualisation is used to "
            , HH.em_ [ HH.text "control" ]
            , HH.text " application behaviour or the visualisation begins to approach the complexity and multi-layered-ness of an app...this all in one single script language is a real problem, at least in my experience."
            ]
        , HH.p_
            [ HH.text "In PureScript it is common, and easy, to use JavaScript libraries via the FFI initially as it is a very quick way to get access to the enormous world of functionality that exists in open source JavaScript libraries. Sometimes this can be sufficient, you wrap a component or a function and its abstractions never leak and all is well. Other times, you wrap something but there's a kind of impedance mismatch with the way the JavaScript abstraction work and the way you'd like to handle, and particularly to compose, things in the purely functional world. D3.js was definitely the latter, for me." ]
        , HH.p_
            [ HH.text "D3.js is a big library with thousands of API end-points but, crucially, not all of those end-points are problematic for composing larger scale applications or weaving visualisations into PureScript web applications. Instead, its is primarily two core areas of the API, Selection and Simulation (more details on these later) which tend to actually "
            , HH.em_ [ HH.text "structure" ]
            , HH.text " programs in a characteristic D3 / JavaScript vernacular. It is these APIs that are first wrapped (by FFI) and then made available in purely functional idiomatic way by this library."
            ]
        , HH.p_
            [ HH.text "A secondary, but also very important, consideration is the ability to design and work with Algebraic Data Types (ADTs) and the rich container libraries that are available in PureScript while building and implementing visualisations and especially the code that surrounds the visualization. While D3 ultimately is a kind of array programming DSL "
            , HH.em_ [ HH.text "within" ]
            , HH.text " JavaScript and our PureScript eDSL is going to bottom out to some sort of \"arrays mapped over the DOM\" too, we want to be able to create data models that are more sophisticated and have better invariants as these are keys to both composability and maintainable, long-lived programs."
            ]
        ]

    -- The Data Visualization Process
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ]
            , HP.id "heading-4"
            ]
            [ HH.text "The Data Visualization Process" ]

        -- Anscombe's Quartet subsection
        , HH.h3
            [ HP.id "anscombe" ]
            [ HH.text "Why Visualize? Anscombe's Quartet" ]
        , HH.p_
            [ HH.text "The famous Anscombe's Quartet demonstrates why visualization is essential. These four datasets have nearly identical statistical properties - same mean, variance, correlation, and linear regression line - yet reveal completely different patterns when visualized:" ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "quartet-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Summary statistics alone would suggest these datasets are interchangeable, but the visualizations tell a very different story: linear relationship, curved relationship, linear with outlier, and vertical with outlier. This perfectly illustrates why we need visualization - numbers hide patterns that become immediately obvious when seen." ]

        , HH.p_
            [ HH.text "Data visualization transforms \"boring\" - but more importantly "
            , HH.em_ [ HH.text "less informative" ]
            , HH.text " tables of data into perceivable patterns. We can think of this as a pipeline in which steps 2 and 3 are the purpose of this library."
            ]
        , HH.ol_
            [ HH.li_ [ HH.strong_ [ HH.text "Data" ], HH.text " - Raw data structures (tables, JSON, etc.)" ]
            , HH.li_ [ HH.strong_ [ HH.text "Data structure" ], HH.text " - Relationships between data elements" ]
            , HH.li_ [ HH.strong_ [ HH.text "Data presentation" ], HH.text " - Visual encoding (HTML/SVG/Canvas)" ]
            , HH.li_ [ HH.strong_ [ HH.text "Perception" ], HH.text " - Human insight and understanding" ]
            ]
        , HH.p_
            [ HH.text "The goal is to make relationships in data visible through appropriate visual encoding, enabling the human visual system to process patterns that would be difficult or impossible to perceive in tabular form." ]

        , HH.h3
            [ HP.id "heading-5" ]
            [ HH.text "Design philosophy of D3*" ]
        , HH.ul_
            [ HH.li_ [ HH.text "(as I understand it)" ] ]
        , HH.p_
            [ HH.text "D3's great innovation was to enable an enormous range of visualizations using some simple fundamental concepts and a small core API. It supports dataviz from simple bar charts to complex hierarchical layouts, force-directed graphs, geographic projections, and interactive dashboards. Despite their visual diversity, these visualizations share common structural patterns: data is bound to DOM elements, attributes are computed from data, and layouts determine spatial positioning." ]
        , HH.p_
            [ HH.text "Something that people with limited prior knowledge / experience of data visualisation often seem to find surprising is the degree to which D3 is "
            , HH.em_ [ HH.text "fundamentally different" ]
            , HH.text " from \"a charting library\". While the library has some affordances that make it very easy to do common visualisations it is not in any way about \"canned visualisations\". Rather, it is a language for describing a relationship between arrays of data and arbitrary constructions of DOM (HTML or SVG) element or marks on Canvas, and it could in principle be used to do auditory \"visualisation\" or, who knows, maybe olfactory \"visualisation\" or drone displays or whatever."
            ]
        , HH.p_
            [ HH.text "Another revolutionary aspect of D3 was that it was "
            , HH.em_ [ HH.text "screen/web native" ]
            , HH.text " - right from the beginning it supported responsive design, transitions, animations, force-layouts all of which are completely distinct from data visualisation on paper."
            ]
        , HH.p_
            [ HH.text "You can get a greater sense of the potential of D3 and the range of things that have thus far been produced using it at "
            , HH.a [ HP.href "https://observablehq.com/" ] [ HH.text "ObservableHQ" ]
            , HH.text "."
            ]

        , HH.h3
            [ HP.id "heading-6" ]
            [ HH.text "Design philosophy of PS<$>D3" ]
        , HH.p_
            [ HH.text "This library presents a slighly more formal grammar embedded in a language that is a lot better suited to larger and more long-lived projects. Whereas D3 has patterns of use that you learn, in PS<$>D3 the pattern is formalised into a grammar with an interpreter. Moreover, the same grammar can be interpreted by different interpreters directly "
            , HH.em_ [ HH.text "in" ]
            , HH.text " PureScript, which, as we will see creates some powerful new uses."
            ]
        , HH.p_
            [ HH.text "Where D3 enabled data visualisers to create movement and responsiveness, PS<$>D3 seeks to enable something that's theoretically possible in D3 but not much seen in practice and that is using the data visualisation as the user interface. This is not the same thing as \"interactive visualisation\" in the sense of \"explorable\" visualisation, ie one in which you can interact with the elements "
            , HH.em_ [ HH.text "but only to manipulate the visualisation itself" ]
            , HH.text ". That is a different, and more limited, sense of interaction. What we are talking about when building UI with data visualisation is leveraging the information density of data visualisation to provide direct manipulation of complex or large systems."
            ]
        , HH.p_
            [ HH.text "This is actually not a revolutionary idea - think how much of an improvement over the initial \"folders of icons\" on the Mac the NeXT column browser was. But it is a direction of travel that has been effectively lost during the web UI revolution of the past 20 years." ]
        , HH.p_
            [ HH.text "This repo and website contains one example of how this might work, the Code Explorer." ]
        ]

    -- Project Goals
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ]
            , HP.id "heading-7"
            ]
            [ HH.text "Project Goals (and non-Goals)" ]

        , HH.h3
            [ HP.id "heading-8" ]
            [ HH.text "Priorities and Trade-offs" ]
        , HH.p_
            [ HH.text "We have two principal priorities in writing this library and since you can't have two number one priorities, some trade-offs are inevitable which we will discuss here." ]
        , HH.table
            [ HP.classes [ HH.ClassName "tutorial-table" ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.text "Primary" ]
                    , HH.th_ [ HH.text "Non-goals" ]
                    , HH.th_ [ HH.text "Sub-goals" ]
                    ]
                ]
            , HH.tbody_
                [ HH.tr_
                    [ HH.td_ [ HH.strong_ [ HH.text "Expressivity:" ], HH.text " fully equivalent to D3.js" ]
                    , HH.td_ [ HH.strong_ [ HH.text "Completeness:" ], HH.text " not all D3.js API surface is needed" ]
                    , HH.td_ [ HH.text "Prefer idiomatic PureScript to exposing D3 APIs directly" ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.strong_ [ HH.text "Readability:" ], HH.text " as readable as D3.js equivalent" ]
                    , HH.td_ [ HH.strong_ [ HH.text "Modelling" ], HH.text " of D3 state complexity thru type-system" ]
                    , HH.td_ [ HH.text "Design for progressive enhancement" ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.strong_ [ HH.text "Composable:" ], HH.text " suitable for \"programming in the large\"" ]
                    , HH.td_ [ HH.strong_ [ HH.text "Performance" ], HH.text " equality to D3" ]
                    , HH.td_ [ HH.text "Showcase PureScript ecosystem and libraries" ]
                    ]
                ]
            ]

        , HH.h3
            [ HP.id "heading-9" ]
            [ HH.text "Goals: details" ]

        , HH.h4
            [ HP.id "heading-10" ]
            [ HH.text "Expressivity" ]
        , HH.p_
            [ HH.text "As alluded to above, expressivity is a key to D3's success - it's not limited to some library of canned visualisations and it's far, far more than even the most parameterized control of colours and symbols. It's important not to lose that in making D3 available directly in PureScript." ]

        , HH.h4
            [ HP.id "heading-11" ]
            [ HH.text "Readability" ]
        , HH.p_
            [ HH.text "If we look at D3.js as a kind of embedded DSL in JavaScript it is certainly clear and readable in its core feature: declaratively associating some array(s) of data with some elements in the DOM and attributes of those elements. While it is definitely not a goal to reproduce the structures of D3js own language in PureScript per se, the goal is to have the same readability, leverage the good work that Mike Bostock did in developing it. At the same time the library should be as idiomatically PureScript / Haskell style as possible." ]

        , HH.h4
            [ HP.id "heading-12" ]
            [ HH.text "Composability" ]
        , HH.p_
            [ HH.text "A related concern to readability is composability, the separation of concerns that makes componentisation possible and which makes possible programming in the large." ]
        , HH.p_
            [ HH.text "YMMV but for me this is where the D3 / JavaScript approach breaks down." ]
        , HH.p_
            [ HH.text "If you are a PureScript or Haskell programmer you probably know what i mean by this and if you are, say, a JavaScript D3 programmer perhaps that will seem odd or even contentious." ]
        , HH.p_
            [ HH.text "The benefits we seek from composability include:" ]
        , HH.ul_
            [ HH.li_ [ HH.text "better composability of sub-programs" ]
            , HH.li_ [ HH.text "better expressiveness in describing the problem domain" ]
            , HH.li_ [ HH.text "better correctness in implementations (ie fewer bugs)" ]
            , HH.li_ [ HH.text "better ability to evolve programs due to shifting requirements (re-factoring)" ]
            ]
        , HH.p_
            [ HH.text "Ideally, i would like the person coding the data layer and data model to be somewhat insulated from the concerns of the person using that data model to create a visualisation. And likewise, i would like the person developing the data visualisation to be somewhat insulated from the concerns of a web app developer. Now, these might very well all be the "
            , HH.em_ [ HH.text "same person" ]
            , HH.text " but separating the concerns like this makes it easier to evolve the code and, crucially, makes it all a little less brittle."
            ]

        , HH.h3
            [ HP.id "heading-13" ]
            [ HH.text "Non-goals: details" ]

        , HH.h4
            [ HP.id "heading-14" ]
            [ HH.text "Complete API coverage" ]
        , HH.p_
            [ HH.text "As alluded to above, there's lots of API in D3 that needs nothing more than an FFI wrapper to be accessible from a PureScript eDSL. D3 is both modular and somewhat functional in style (in the JavaScript sense of functional programming, to be clear). So it was from the start a non-goal to completely expose all of D3 as "
            , HH.em_ [ HH.text "idiomatic" ]
            , HH.text " PureScript where a simple wrapper was sufficient."
            ]
        , HH.p_
            [ HH.text "Furthermore, i have only written those wrappers "
            , HH.em_ [ HH.text "as I needed them" ]
            , HH.text " to there are still "
            , HH.em_ [ HH.text "many" ]
            , HH.text " parts of D3 that are not covered by this eDSL."
            ]
        , HH.p_
            [ HH.text "Addendum 2025 - the advent of LLMs and coding assistance agents means that perhaps complete API coverage might arrive sooner than expected." ]

        , HH.h4
            [ HP.id "heading-15" ]
            [ HH.text "Modelling of D3 State" ]
        , HH.p_
            [ HH.text "This might seem like a surprising choice - D3 is inherently "
            , HH.em_ [ HH.text "very" ]
            , HH.text " stateful, there's state in D3, there's state in the DOM, there's statefulness in your (pure) data after you give it to D3. State everywhere. In many cases in functional programming you might try to ameliorate the dangers of this by explicitly modelling the state, using a State Monad or marking everything that changes or depends upon state as \"Effect\"-full."
            ]
        , HH.p_
            [ HH.text "Indeed i have tried this approach in the past. In this library i have instead striven to isolate the statefulness to only the code that uses eDSL represented by the "
            , HH.code_ [ HH.text "Selection" ]
            , HH.text " and "
            , HH.code_ [ HH.text "Simulation" ]
            , HH.text " monads. This "
            , HH.em_ [ HH.text "significantly" ]
            , HH.text " removes but cannot fully eliminate the issues associated with state."
            ]

        , HH.h4
            [ HP.id "heading-16" ]
            [ HH.text "Performance equality with D3" ]
        , HH.p_
            [ HH.text "While this was a non-goal in practice it doesn't seem to have been a problem. Essentially, this is because PS<$>D3 simply leverages D3.js for all the performance critical aspects and because the FFI has been kept simple by the decision described in the previous section." ]
        , HH.p_
            [ HH.text "The performance bottlenecks in a web data visualisation are, by their nature, going to be the assignment of potentially millions of attributes to potentially millions of DOM elements. D3.js does this extremely well and the PureScript layer doesn't compromise it much, if at all." ]
        ]

    -- DSL and eDSL
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ]
            , HP.id "heading-17"
            ]
            [ HH.text "What's a DSL? and what's an eDSL?" ]

        , HH.h3
            [ HP.id "heading-18" ]
            [ HH.text "Domain Specific Language" ]
        , HH.p_
            [ HH.text "Programming language tailored to a particular domain." ]
        , HH.p_
            [ HH.text "Might be limited compared to general purpose programming language." ]
        , HH.p_
            [ HH.text "But, may also be easier for novices, domain experts, non-programmers to use." ]

        , HH.h3
            [ HP.id "heading-19" ]
            [ HH.text "Embedded Domain Specific Language" ]
        , HH.p_
            [ HH.text "Embedded Domain Specific Language makes domain specific tasks easier "
            , HH.em_ [ HH.text "without" ]
            , HH.text " limiting the programmer in any way - they still have access to all parts of the host language."
            ]
        , HH.p_
            [ HH.text "This has Pros & Cons" ]
        ]

    -- The grammar of D3
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ]
            , HP.id "heading-20"
            ]
            [ HH.text "The grammar of D3..." ]
        , HH.p_
            [ HH.text "Analyse the "
            , HH.em_ [ HH.text "implicit" ]
            , HH.text " grammar of D3..."
            ]
        , HH.p_
            [ HH.text "...in order to define a "
            , HH.em_ [ HH.text "minimal" ]
            , HH.text " grammar for our PureScript eDSL"
            ]
        , HH.p_
            [ HH.text "(consistent with priority goals of readability and expressiveness)" ]

        , HH.h3
            [ HP.id "heading-21" ]
            [ HH.text "A Visual D3 Grammar" ]
        , HH.p_
            [ HH.strong_ [ HH.text "This is much simpler than D3's actual AST would be (if it had one) but it is sufficient to express a LOT of D3 scripts." ] ]
        , HH.table
            [ HP.classes [ HH.ClassName "tutorial-table" ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.text "Name" ]
                    , HH.th_ [ HH.text "Function" ]
                    , HH.th_ [ HH.text "Notes" ]
                    ]
                ]
            , HH.tbody_
                [ HH.tr_
                    [ HH.td_ [ HH.em_ [ HH.text "attach" ] ]
                    , HH.td_ [ HH.text "Select an entry point (or points) in the DOM" ]
                    , HH.td_ [ HH.text "Simply uses a CSS type selector to identify the target. Resulting selection can then be used for append / join in order to build up a visualisation from HTML, SVG or Canvas elements." ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.em_ [ HH.text "appendTo" ] ]
                    , HH.td_ [ HH.text "Add some DOM element "
                             , HH.strong_ [ HH.text "e" ]
                             , HH.text " to a selection"
                             ]
                    , HH.td_ [ HH.text "Each element can have attributes. If data has been bound higher up in the AST then that data is available in this element's attributes" ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.em_ [ HH.text "join" ] ]
                    , HH.td_ [ HH.text "For every "
                             , HH.em_ [ HH.text "datum" ]
                             , HH.text " "
                             , HH.strong_ [ HH.text "d" ]
                             , HH.text " in some array, insert an element "
                             , HH.strong_ [ HH.text "e" ]
                             ]
                    , HH.td_ [ HH.text "We'll run the visualisation with some data model which can be arbitrary in structure, but at every point where we want to append "
                             , HH.em_ [ HH.text "multiple" ]
                             , HH.text " elements we need to have a function that yields a simple array."
                             , HH.br_
                             , HH.br_
                             , HH.text "Each element can have attributes that are derived from the datum with which it is associated."
                             , HH.br_
                             , HH.br_
                             , HH.text "The datum at each element is inherited by its children, so any subsequent join starts with the structure of this datum."
                             ]
                    ]
                ]
            ]

        , HH.h3
            [ HP.id "heading-22" ]
            [ HH.text "Grammar diagrams" ]

        , HH.h4
            [ HP.id "heading-23" ]
            [ HH.text "3 Little Circles: the canonical simplest example" ]
        , HH.p_
            [ HH.text "Insert Mermaid diagram here" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """    a        "div#hook"
    ↓
    +        <svg>
    ↓
    +        <g>
    ↓
   <+>       [1,2,3] → <circle>"""
                ]
            ]
        , HH.p_
            [ HH.a [ HP.href "https://bost.ocks.org/mike/circles/" ] [ HH.text "https://bost.ocks.org/mike/circles/" ] ]

        , HH.h4
            [ HP.id "heading-24" ]
            [ HH.text "Radial Tree: A more complex example" ]
        , HH.p_
            [ HH.text "The Radial Tree and Cluster Dendrogram visualizations share the exact same structure:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """    a          "div#hook"
    ↓
    +          <svg>
    ↓
    +          <g.links>    +          <g.nodes>    +          <g.labels>
    ↓                       ↓                       ↓
   <+>         <path>      <+>         <circle>    <+>         <text>
               ↑                       ↑                       ↑
            m->m.links              m->m.nodes              m->m.nodes"""
                ]
            ]
        , HH.p_
            [ HH.text "All the differences are in details of how the node attributes are calculated from the data." ]

        , HH.h4
            [ HP.id "heading-25" ]
            [ HH.text "Even complex examples structurally simple" ]
        , HH.p_
            [ HH.text "Even highly complex visualizations like "
            , HH.a [ HP.href "https://top2000.visualcinnamon.com" ] [ HH.text "Nadieh Bremer's Top 2000 visualization" ]
            , HH.text " can be expressed with this simple grammar - the complexity lies in the data transformations and attribute calculations, not in the fundamental structure."
            ]
        ]
    ]

-- Handle actions
handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet anscombesQuartet "div.quartet-viz"
    pure unit

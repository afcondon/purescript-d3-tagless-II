module PSD3.HowTo.HowtoIndex where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Website.Types (Section(..))
import Type.Proxy (Proxy(..))

-- | Howto Index page state
type State = Unit

-- | Howto Index page actions
data Action = Initialize

-- | Howto Index page slots
type Slots =
  ( docsHeader :: forall q. H.Slot q Void Unit
  )

_docsHeader = Proxy :: Proxy "docsHeader"

-- | Howto Index page component
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
    [ HP.classes [ HH.ClassName "docs-page", HH.ClassName "howto-wiki" ] ]
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just HowToSection }

    -- Hero section
    , HH.section
        [ HP.classes [ HH.ClassName "docs-hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "docs-hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "docs-hero-title" ] ]
                [ HH.text "How-to Guides" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "Practical guides for accomplishing specific tasks with PureScript D3. Each topic includes bullet points of what will be covered." ]
            ]
        ]

    -- Core Techniques
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Core Techniques" ]

        , renderHowtoCard "Creating Animated Transitions"
            "How to create smooth, animated transitions between visualization states."
            [ "Setting up TransitionConfig (duration, easing, delay)"
            , "Enter transitions for new elements"
            , "Update transitions for changing data"
            , "Exit transitions for removed elements"
            , "Staggering animations with index-based delays"
            , "Chaining transitions for multi-step animations"
            ]

        , renderHowtoCard "Responding to User Events"
            "How to make visualizations interactive with click, hover, and other events."
            [ "Click handlers on elements"
            , "Hover effects and highlights"
            , "Mouse position for tooltips"
            , "Keyboard events"
            , "Event delegation with selections"
            ]

        , renderHowtoCard "Adding Tooltips"
            "How to add informative tooltips that appear on hover."
            [ "Basic tooltip div approach"
            , "Positioning relative to mouse/element"
            , "Showing/hiding on hover"
            , "Formatting data in tooltips"
            , "Styling and theming"
            ]
        ]

    -- Data & Scales
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Data & Scales" ]

        , renderHowtoCard "Loading External Data"
            "How to fetch and parse external data sources for visualization."
            [ "Fetching JSON data with Aff"
            , "Parsing CSV data"
            , "Handling loading states"
            , "Error handling and fallbacks"
            , "Caching and refreshing data"
            ]

        , renderHowtoCard "Creating Axes and Scales"
            "How to set up scales and generate axes for charts."
            [ "Setting up linear, ordinal, and time scales"
            , "Generating axes with d3-axis"
            , "Formatting tick labels"
            , "Responsive axes that resize"
            , "Custom tick values"
            ]
        ]

    -- Layouts
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Layouts" ]

        , renderHowtoCard "Building Force-Directed Graphs"
            "How to create interactive force-directed network visualizations."
            [ "Setting up the D3 force simulation"
            , "Configuring forces (charge, link, center, collision)"
            , "Connecting simulation tick to DOM updates"
            , "Node dragging with force interaction"
            , "Restarting simulation on data changes"
            , "Alpha decay and cooling"
            ]

        , renderHowtoCard "Working with Hierarchical Data"
            "How to visualize tree structures, treemaps, and other hierarchical layouts."
            [ "Converting flat data to tree structure"
            , "Using d3-hierarchy layouts (tree, cluster, treemap)"
            , "Link generators for tree connections"
            , "Radial layouts"
            , "Collapsible trees with enter/exit"
            ]

        , renderHowtoCard "Using the TreeAPI"
            "How to use the declarative TreeAPI for building visualization structures."
            [ "When to use TreeAPI vs SelectionM directly"
            , "Building declarative tree structures"
            , "Join nodes for data-driven elements"
            , "NestedJoin for hierarchical DOM"
            , "Choosing the right interpreter"
            ]
        ]

    -- Development
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Development" ]

        , renderHowtoCard "Debugging Visualizations"
            "How to debug and troubleshoot visualization issues."
            [ "Using the Mermaid interpreter to inspect tree structure"
            , "Console logging in attribute functions"
            , "Inspecting enter/update/exit selections"
            , "Common errors and their causes"
            , "Browser DevTools for SVG inspection"
            ]

        , renderHowtoCardWithBadge "Performance Optimization" "In Progress"
            "How to optimize visualizations for large datasets and smooth interactions."
            [ "Strategies for large datasets (sampling, aggregation)"
            , "Canvas rendering for many elements"
            , "Throttling/debouncing updates"
            , "Minimizing DOM mutations"
            , "When to use vanilla D3 escape hatches"
            ]
        ]
    ]

-- | Render a how-to card with title, description, and bullet points
renderHowtoCard :: forall w i. String -> String -> Array String -> HH.HTML w i
renderHowtoCard title description bullets =
  HH.div
    [ HP.classes [ HH.ClassName "howto-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "howto-card__title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "howto-card__description" ] ]
        [ HH.text description ]
    , HH.ul
        [ HP.classes [ HH.ClassName "howto-card__bullets" ] ]
        (map (\bullet -> HH.li_ [ HH.text bullet ]) bullets)
    ]

-- | Render a how-to card with a badge (e.g., "In Progress")
renderHowtoCardWithBadge :: forall w i. String -> String -> String -> Array String -> HH.HTML w i
renderHowtoCardWithBadge title badge description bullets =
  HH.div
    [ HP.classes [ HH.ClassName "howto-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "howto-card__title" ] ]
        [ HH.text title
        , HH.span
            [ HP.classes [ HH.ClassName "howto-card__badge" ] ]
            [ HH.text badge ]
        ]
    , HH.p
        [ HP.classes [ HH.ClassName "howto-card__description" ] ]
        [ HH.text description ]
    , HH.ul
        [ HP.classes [ HH.ClassName "howto-card__bullets" ] ]
        (map (\bullet -> HH.li_ [ HH.text bullet ]) bullets)
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

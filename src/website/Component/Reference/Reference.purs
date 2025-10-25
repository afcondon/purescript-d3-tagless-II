module PSD3.Reference.Reference where -- Reference

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.RHSNavigation as RHSNav
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | Reference page state
type State = Unit

-- | Reference page actions
data Action = Initialize

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | Reference page component
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
    [ HP.classes [ HH.ClassName "reference-page" ] ]
    [ -- TOC Panel (LHS)
      renderTOC
        { title: "API Reference"
        , items:
            [ { anchor: "core", label: "Core Concepts", level: 0 }
            , { anchor: "selection", label: "Selection", level: 0 }
            , { anchor: "attach", label: "attach", level: 1 }
            , { anchor: "appendTo", label: "appendTo", level: 1 }
            , { anchor: "selectAll", label: "selectAll", level: 1 }
            , { anchor: "data", label: "Data Binding", level: 0 }
            , { anchor: "scales", label: "Scales", level: 0 }
            , { anchor: "axes", label: "Axes", level: 0 }
            ]
        , image: Just "images/reference-bookmark-deepseavent.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component Reference

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "API Reference" ]
        , HH.p_
            [ HH.text "Complete technical documentation for the PS<$>D3 library. This reference provides detailed information about every function, type, and type class in the library." ]
        ]

    -- Core Concepts section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "core"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Core Concepts" ]
        , HH.p_
            [ HH.text "PS<$>D3 is built on the Finally Tagless pattern, which allows you to write visualization code once and interpret it in multiple ways. The core abstraction is the "
            , HH.code_ [ HH.text "SelectionM" ]
            , HH.text " type class."
            ]
        , HH.h3_ [ HH.text "The SelectionM Type Class" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """class Monad m <= SelectionM s m | m -> s where
  attach :: Selector s -> m s
  appendTo :: s -> Element -> Array (Attr s) -> m s
  selectAll :: s -> Selector s -> m s
  -- ... and many more operations""" ]
            ]
        , HH.p_
            [ HH.text "This type class defines the operations you can perform on D3 selections. The "
            , HH.code_ [ HH.text "s" ]
            , HH.text " type parameter represents a selection, and "
            , HH.code_ [ HH.text "m" ]
            , HH.text " is the monad in which these operations run."
            ]
        ]

    -- Selection section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "selection"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Selection Operations" ]
        , HH.p_
            [ HH.text "These functions form the core of PS<$>D3's selection API, allowing you to create, modify, and manipulate DOM elements." ]

        -- attach function
        , HH.h3
            [ HP.id "attach" ]
            [ HH.text "attach" ]
        , HH.pre_
            [ HH.code_
                [ HH.text "attach :: forall s m. SelectionM s m => Selector s -> m s" ]
            ]
        , HH.p_
            [ HH.text "Attaches to an existing DOM element using a CSS selector. This is typically the first operation in any D3 visualization." ]
        , HH.p_
            [ HH.strong_ [ HH.text "Parameters:" ] ]
        , HH.ul_
            [ HH.li_
                [ HH.code_ [ HH.text "Selector s" ]
                , HH.text " - A CSS selector string (e.g., \"div.chart\", \"#visualization\")"
                ]
            ]
        , HH.p_
            [ HH.strong_ [ HH.text "Returns:" ] ]
        , HH.ul_
            [ HH.li_ [ HH.text "A selection containing the matched element" ] ]
        , HH.p_
            [ HH.strong_ [ HH.text "Example:" ] ]
        , HH.pre_
            [ HH.code_
                [ HH.text "root <- attach \"div.my-chart\"" ]
            ]

        -- appendTo function
        , HH.h3
            [ HP.id "appendTo" ]
            [ HH.text "appendTo" ]
        , HH.pre_
            [ HH.code_
                [ HH.text "appendTo :: forall s m. SelectionM s m => s -> Element -> Array (Attr s) -> m s" ]
            ]
        , HH.p_
            [ HH.text "Appends a new element to an existing selection with specified attributes." ]
        , HH.p_
            [ HH.strong_ [ HH.text "Parameters:" ] ]
        , HH.ul_
            [ HH.li_
                [ HH.code_ [ HH.text "s" ]
                , HH.text " - The parent selection to append to"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "Element" ]
                , HH.text " - The type of element to create (Svg, Rect, Circle, etc.)"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "Array (Attr s)" ]
                , HH.text " - Array of attributes to apply to the new element"
                ]
            ]
        , HH.p_
            [ HH.strong_ [ HH.text "Returns:" ] ]
        , HH.ul_
            [ HH.li_ [ HH.text "A selection containing the newly created element" ] ]
        , HH.p_
            [ HH.strong_ [ HH.text "Example:" ] ]
        , HH.pre_
            [ HH.code_
                [ HH.text """svg <- appendTo root Svg
  [ Width 400.0
  , Height 300.0
  , ViewBox 0.0 0.0 400.0 300.0
  ]""" ]
            ]

        -- selectAll function
        , HH.h3
            [ HP.id "selectAll" ]
            [ HH.text "selectAll" ]
        , HH.pre_
            [ HH.code_
                [ HH.text "selectAll :: forall s m. SelectionM s m => s -> Selector s -> m s" ]
            ]
        , HH.p_
            [ HH.text "Selects all descendant elements matching the given selector within a selection." ]
        , HH.p_
            [ HH.strong_ [ HH.text "Parameters:" ] ]
        , HH.ul_
            [ HH.li_
                [ HH.code_ [ HH.text "s" ]
                , HH.text " - The parent selection to search within"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "Selector s" ]
                , HH.text " - CSS selector for the elements to find"
                ]
            ]
        , HH.p_
            [ HH.strong_ [ HH.text "Returns:" ] ]
        , HH.ul_
            [ HH.li_ [ HH.text "A selection containing all matched elements" ] ]
        , HH.p_
            [ HH.strong_ [ HH.text "Example:" ] ]
        , HH.pre_
            [ HH.code_
                [ HH.text "circles <- selectAll svg \"circle\"" ]
            ]
        ]

    -- Data Binding section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "data"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Data Binding" ]
        , HH.p_
            [ HH.text "Documentation for data binding functions coming soon..." ]
        ]

    -- Scales section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "scales"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Scales" ]
        , HH.p_
            [ HH.text "Documentation for scale functions coming soon..." ]
        ]

    -- Axes section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "axes"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Axes" ]
        , HH.p_
            [ HH.text "Documentation for axis functions coming soon..." ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

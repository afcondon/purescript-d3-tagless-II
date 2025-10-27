module PSD3.Understanding.Concepts where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Understanding.UnderstandingTabs as UnderstandingTabs
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

type State = Unit

data Action = Initialize

type Slots =
  ( sectionNav :: forall q. H.Slot q Void Unit
  , tabs :: forall q. H.Slot q Void Unit
  )

_sectionNav = Proxy :: Proxy "sectionNav"
_tabs = Proxy :: Proxy "tabs"

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
            [ { anchor: "heading-finally-tagless", label: "Finally Tagless", level: 0 }
            , { anchor: "heading-selectionm", label: "The SelectionM Monad", level: 0 }
            , { anchor: "heading-capabilities", label: "Capabilities & Interpreters", level: 0 }
            , { anchor: "heading-type-safe", label: "Type-Safe Attribute System", level: 0 }
            ]
        , image: Just "images/understanding-bookmark-trees.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: UnderstandingSection
        , currentRoute: UnderstandingConcepts
        , sectionPages:
            [ { route: UnderstandingConcepts, label: "Concepts" }
            , { route: UnderstandingPatterns, label: "Patterns" }
            , { route: UnderstandingPhilosophy, label: "Philosophy" }
            ]
        , moduleCategories: Nothing
        }

    -- Main content
    , HH.div
        [ HP.classes [ HH.ClassName "explanation-content" ] ]
        [ -- Tab navigation
          HH.slot_ _tabs unit UnderstandingTabs.component UnderstandingConcepts

        -- Page title
        , HH.h1
            [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "Core Concepts" ]

        -- Finally Tagless
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-finally-tagless" ]
                [ HH.text "Finally Tagless" ]
            , HH.p_
                [ HH.text "We want the library to be extensible and, in fact, D3 while it's the inspiration and a core target, is not the only possible implementation of the SelectionM monad. For this reason we choose the design pattern called Finally Tagless encoding and implement "
                , HH.em_ [ HH.text "interpreters" ]
                , HH.text " for the SelectionM."
                ]
            , HH.p_
                [ HH.text "Furthermore, using this pattern, we can now "
                , HH.em_ [ HH.text "extend" ]
                , HH.text " the SelectionM monad as can be seen in the library where we provide a SimulationM2 monad which extends the our static DOM element visualizations by allowing them to move under the control of a Force Layout algorithm."
                ]
            ]

        -- SelectionM Monad
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-selectionm" ]
                [ HH.text "The SelectionM Monad" ]
            , HH.h3_ [ HH.text "Grammar" ]
            , HH.p_
                [ HH.text "D3.js is, naturally, a JavaScript idiomatic library, making use of function chaining as a readable and easily-learnt API." ]
            , HH.p_
                [ HH.text "PS<$>D3 formalizes this slightly while retaining the general shape of the pattern and core concepts such as Selection, Attribute." ]
            , HH.p_ [ HH.text "The basic grammar of PS<$>D3 is just this:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "we can ", HH.em_ [ HH.text "attach" ], HH.text " our visualisation into the DOM, typically hooking into something like <div id=\"viz\">" ]
                , HH.li_ [ HH.text "we can build up whatever additional structure we want inside that, using ", HH.em_ [ HH.text "appendTo" ], HH.text " to for example put an <svg> with some <g>'s inside the <div id=\"viz\">" ]
                , HH.li_ [ HH.text "we can apply ", HH.em_ [ HH.text "attributes" ], HH.text " to the elements we've created: styles, ids, css classes and such" ]
                , HH.li_ [ HH.text "we can apply ", HH.em_ [ HH.text "behaviors" ], HH.text " to the elements we've created: things like zoom, drag and click event-handlers" ]
                , HH.li_ [ HH.text "we can ", HH.em_ [ HH.text "join" ], HH.text " some array of data at this point, such that when we next ", HH.em_ [ HH.text "add" ], HH.text " something, say a <circle>, we will put ", HH.em_ [ HH.text "n" ], HH.text " <circle>s in, not just one." ]
                ]
            , HH.p_ [ HH.text "In D3 the thing that is being acted on in this chain of functions is The Selection..." ]
            , HH.h3_ [ HH.text "The SelectionM Monad" ]
            , HH.p_
                [ HH.text "Where functions would be chained in JavaScript, the natural counterpart in PureScript is a Monad, specifically the SelectionM Monad which embodies the core functions above and a few others." ]
            , HH.p_ [ HH.text "It looks like this:" ]
            , HH.pre_
                [ HH.code_
                    [ HH.text """class (Monad m) <= SelectionM selection m where
  attach          :: Selector selection -> m selection
  appendTo        :: selection -> Element -> Array (SelectionAttribute) -> m selection
  setAttributes   :: selection -> Array (SelectionAttribute) -> m Unit
  on              :: selection -> Behavior selection -> m Unit
  simpleJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_) -> m selection
  updateJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_) -> m { enter :: selection, exit :: selection, update :: selection }

  selectUnder     :: selection -> Selector selection -> m selection
  filterSelection :: selection -> Selector selection -> m selection
  mergeSelections :: selection -> selection -> m selection
  openSelection   :: selection -> Selector selection -> m selection"""
                    ]
                ]
            ]

        -- Capabilities/Interpreters
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-capabilities" ]
                [ HH.text "Capabilities & Interpreters" ]
            , HH.p_ [ HH.text "The SimulationM2 monad looks like:" ]
            , HH.pre_
                [ HH.code_
                    [ HH.text """class (Monad m, SelectionM selection m) <= SimulationM2 selection m | m -> selection where
  start :: m Unit
  stop  :: m Unit
  setConfigVariable    :: SimVariable -> m Unit
  actualizeForces:: Set Label -> m Unit
  setNodes :: forall d.   Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  setLinks :: forall d r id. (Eq id) => Array (D3Link id r) -> Array (D3_SimulationNode d) -> (Datum_ -> Index_ ) -> m (Array (D3LinkSwizzled (D3_SimulationNode d) r))
  setNodesFromSelection :: selection -> m Unit
  setLinksFromSelection :: selection -> (Datum_ -> Boolean) -> m Unit
  mergeNewDataWithSim :: forall d r id. (Eq id) =>
    selection -> -- nodes selection
    (Datum_ -> Index_) -> -- nodes keyFn
    selection -> -- links selection
    (Datum_ -> Index_) -> -- links KeyFn
    RawData d r id -> -- links and nodes raw data
    m { links :: (Array (D3LinkSwizzled (D3_SimulationNode d) r)), nodes :: (Array (D3_SimulationNode d))}
  simulationHandle :: m D3Simulation_
  addTickFunction    :: Label -> Step selection -> m Unit
  removeTickFunction :: Label                   -> m Unit"""
                    ]
                ]
            ]

        -- Type-safe Attributes
        , HH.section
            [ HP.classes [ HH.ClassName "concept-section" ] ]
            [ HH.h2
                [ HP.id "heading-type-safe" ]
                [ HH.text "Type-Safe Attribute System" ]
            , HH.p_
                [ HH.text "A key innovation of D3 is the easy, natural way one can go from static to data-driven attributes, for example going from putting "
                , HH.em_ [ HH.text "n" ]
                , HH.text " identical circles into the DOM vs putting "
                , HH.em_ [ HH.text "n" ]
                , HH.text " circles of differing colours and sizes into the DOM. This is the essence (with join/simulation) of Data Drive Documents, ie D3."
                ]
            , HH.p_
                [ HH.text "In PureScript we need a little bit of typeclass trickery to get the same clean syntax, and we use it to enable interchangeable, type-safe attribute setters. For example, we can use an attribute "
                , HH.em_ [ HH.text "cx" ]
                , HH.text " (the center x position of a circle) which can take:"
                ]
            , HH.ul_
                [ HH.li_ [ HH.text "a numeric value OR," ]
                , HH.li_ [ HH.text "a lambda function which takes the datum we are using to a numeric value OR," ]
                , HH.li_ [ HH.text "a lambda function which takes the datum AND the index of that datum in the selection" ]
                ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

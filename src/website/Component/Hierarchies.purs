module PSD3.Hierarchies where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.RHSNavigation as RHSNav
import PSD3.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | Hierarchies page state
type State = {
  currentLayout :: HierarchyLayout
}

-- | Available hierarchy layouts
data HierarchyLayout
  = HorizontalTree
  | VerticalTree
  | RadialTree
  | CirclePacking
  | Treemap

derive instance eqHierarchyLayout :: Eq HierarchyLayout

instance showHierarchyLayout :: Show HierarchyLayout where
  show HorizontalTree = "Horizontal Tree"
  show VerticalTree = "Vertical Tree"
  show RadialTree = "Radial Tree"
  show CirclePacking = "Circle Packing"
  show Treemap = "Treemap"

layoutDescription :: HierarchyLayout -> String
layoutDescription = case _ of
  HorizontalTree -> "Classic left-to-right tree layout showing parent-child relationships. Root on the left, leaves on the right."
  VerticalTree -> "Top-down tree layout with root at the top. Common in organizational charts and file system visualizations."
  RadialTree -> "Circular tree layout emanating from center. Space-efficient and visually striking for large hierarchies."
  CirclePacking -> "Nested circles where size represents values. Each circle contains its children, creating an intuitive part-whole relationship."
  Treemap -> "Space-filling rectangular layout. Area of each rectangle is proportional to its value, excellent for showing proportions."

-- | Hierarchies page actions
data Action
  = Initialize
  | SelectLayout HierarchyLayout

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | Hierarchies page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { currentLayout: HorizontalTree }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- Control Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel", HH.ClassName "control-panel" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main", HH.ClassName "control-panel__main" ] ]
            [ HH.img
                [ HP.src "controller.jpeg"
                , HP.alt ""
                , HP.classes [ HH.ClassName "control-panel__icon" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Layout Controls" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "control-panel__content" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "control-panel__section" ] ]
                    [ HH.h4
                        [ HP.classes [ HH.ClassName "control-panel__section-title" ] ]
                        [ HH.text "Tree Layouts" ]
                    , HH.div
                        [ HP.classes [ HH.ClassName "control-panel__options" ] ]
                        [ renderLayoutOption HorizontalTree "Horizontal" state.currentLayout
                        , renderLayoutOption VerticalTree "Vertical" state.currentLayout
                        , renderLayoutOption RadialTree "Radial" state.currentLayout
                        ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "control-panel__section" ] ]
                    [ HH.h4
                        [ HP.classes [ HH.ClassName "control-panel__section-title" ] ]
                        [ HH.text "Space-Filling" ]
                    , HH.div
                        [ HP.classes [ HH.ClassName "control-panel__options" ] ]
                        [ renderLayoutOption CirclePacking "Circle Pack" state.currentLayout
                        , renderLayoutOption Treemap "Treemap" state.currentLayout
                        ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "control-panel__current" ] ]
                    [ HH.strong_ [ HH.text "Current:" ]
                    , HH.text " "
                    , HH.text $ show state.currentLayout
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component Hierarchies

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Hierarchical Layouts" ]
        , HH.p_
            [ HH.text "Hierarchical data structures are everywhere in computing: file systems, organizational charts, taxonomies, JSON documents, and abstract syntax trees. Different visualization layouts reveal different aspects of the same hierarchical data." ]
        , HH.p_
            [ HH.text "This page demonstrates five different ways to visualize the same hierarchical dataset. Use the controls on the left to switch between layouts and explore how each representation emphasizes different relationships in the data." ]
        ]

    -- Visualization section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text $ show state.currentLayout ]
        , HH.p_
            [ HH.text $ layoutDescription state.currentLayout ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "hierarchies-viz" ] ]
                [ renderLayoutPlaceholder state.currentLayout ]
            ]
        ]

    -- Code section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Implementation" ]
        , HH.p_
            [ HH.text "The visualization code for "
            , HH.strong_ [ HH.text $ show state.currentLayout ]
            , HH.text " demonstrates how D3's hierarchical layout algorithms transform tree data into visual coordinates."
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            [ HH.pre_
                [ HH.code_
                    [ HH.text "-- Code for "
                    , HH.text $ show state.currentLayout
                    , HH.text " layout will go here\n"
                    , HH.text "-- Demonstrates D3 hierarchy layout with PureScript DSL"
                    ]
                ]
            ]
        ]
    ]

-- | Render a layout option button
renderLayoutOption :: forall w. HierarchyLayout -> String -> HierarchyLayout -> HH.HTML w Action
renderLayoutOption layout label currentLayout =
  HH.button
    [ HP.classes $ [ HH.ClassName "control-panel__option" ] <>
        if layout == currentLayout
        then [ HH.ClassName "control-panel__option--active" ]
        else []
    , HE.onClick $ const (SelectLayout layout)
    , HP.type_ HP.ButtonButton
    ]
    [ HH.text label ]

-- | Placeholder for each layout (to be implemented)
renderLayoutPlaceholder :: forall w i. HierarchyLayout -> HH.HTML w i
renderLayoutPlaceholder layout =
  HH.div
    [ HP.classes [ HH.ClassName "layout-placeholder" ] ]
    [ HH.text $ "Placeholder for " <> show layout <> " layout"
    , HH.p_ [ HH.text "Visualization will be implemented here" ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

  SelectLayout layout ->
    H.modify_ _ { currentLayout = layout }

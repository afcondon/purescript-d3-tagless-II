module PSD3.Hierarchies where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Button as Button
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

-- | Hierarchies page actions
data Action
  = Initialize
  | SwitchLayout HierarchyLayout

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
    [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "hierarchies-page" ] ]
    [ -- Navigation Panel (RHS)
      HH.slot_ _rhsNav unit RHSNav.component Hierarchies

    , -- Floating control panel
      HH.div
        [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "hierarchies-controls" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
            [ HH.text "Hierarchy Layouts" ]
        , HH.p
            [ HP.classes [ HH.ClassName "hierarchies-description" ] ]
            [ HH.text "Explore different ways to visualize the same hierarchical data from our codebase." ]
        , HH.div
            [ HP.classes [ HH.ClassName "layout-controls" ] ]
            [ HH.h3_ [ HH.text "Tree Layouts" ]
            , Button.buttonGroup_
                [ Button.buttonLeft
                    [ HE.onClick $ const (SwitchLayout HorizontalTree) ]
                    [ HH.text "Horizontal" ]
                , Button.buttonCenter
                    [ HE.onClick $ const (SwitchLayout VerticalTree) ]
                    [ HH.text "Vertical" ]
                , Button.buttonRight
                    [ HE.onClick $ const (SwitchLayout RadialTree) ]
                    [ HH.text "Radial" ]
                ]
            , HH.h3_ [ HH.text "Other Layouts" ]
            , Button.buttonGroup_
                [ Button.buttonLeft
                    [ HE.onClick $ const (SwitchLayout CirclePacking) ]
                    [ HH.text "Circle Packing" ]
                , Button.buttonRight
                    [ HE.onClick $ const (SwitchLayout Treemap) ]
                    [ HH.text "Treemap" ]
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "current-layout-info" ] ]
            [ HH.p_ [ HH.text $ "Current: " <> show state.currentLayout ] ]
        ]

    -- Main visualization area
    , HH.div
        [ HP.classes [ HH.ClassName "svg-container", HH.ClassName "fullscreen-viz", HH.ClassName "hierarchies-viz" ] ]
        [ renderLayoutPlaceholder state.currentLayout ]
    ]

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

  SwitchLayout layout ->
    H.modify_ _ { currentLayout = layout }

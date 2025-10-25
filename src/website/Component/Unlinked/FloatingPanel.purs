module PSD3.Unlinked.FloatingPanel where -- unlinked

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)

-- | Input configuration for FloatingPanel
type Input = {
  title :: String,
  initialX :: Number,
  initialY :: Number,
  initialWidth :: Maybe Number,
  initialHeight :: Maybe Number,
  collapsible :: Boolean,
  draggable :: Boolean
}

-- | State for FloatingPanel
type State = {
  title :: String,
  x :: Number,
  y :: Number,
  width :: Maybe Number,
  height :: Maybe Number,
  collapsed :: Boolean,
  draggable :: Boolean,
  dragging :: Boolean
}

-- | Slots for child components
type Slots :: forall k. Row k
type Slots = ()

-- | Actions
data Action
  = Initialize
  | ToggleCollapse
  | StartDrag
  | StopDrag

-- | Component definition
component :: forall q o m. MonadAff m => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: Input -> State
initialState input =
  { title: input.title
  , x: input.initialX
  , y: input.initialY
  , width: input.initialWidth
  , height: input.initialHeight
  , collapsed: false
  , draggable: input.draggable
  , dragging: false
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.classes
        [ HH.ClassName "floating-panel"
        , HH.ClassName if state.collapsed then "floating-panel--collapsed" else ""
        , HH.ClassName if state.dragging then "floating-panel--dragging" else ""
        ]
    , HP.attr (H.AttrName "style") $ panelStyle state
    ]
    [ -- Header with title and controls
      HH.div
        [ HP.classes [ HH.ClassName "floating-panel__header" ]
        , HE.onMouseDown \_ -> StartDrag
        ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
            [ HH.text state.title ]
        , HH.button
            [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
            , HE.onClick \_ -> ToggleCollapse
            , HP.type_ HP.ButtonButton
            ]
            [ HH.text if state.collapsed then "+" else "−" ]
        ]

    , -- Content area (hidden when collapsed)
      if state.collapsed then
        HH.text ""
      else
        HH.div
          [ HP.classes [ HH.ClassName "floating-panel__content" ] ]
          [ HH.text "Panel content goes here" ]
    ]

panelStyle :: State -> String
panelStyle state =
  "position: fixed; " <>
  "left: " <> show state.x <> "px; " <>
  "top: " <> show state.y <> "px; " <>
  widthStyle <>
  heightStyle
  where
    widthStyle = case state.width of
      Just w -> "width: " <> show w <> "px; "
      Nothing -> ""
    heightStyle = case state.height of
      Just h -> "height: " <> show h <> "px; "
      Nothing -> ""

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> pure unit

  ToggleCollapse -> do
    H.modify_ \s -> s { collapsed = not s.collapsed }

  StartDrag -> do
    state <- H.get
    when state.draggable do
      H.modify_ _ { dragging = true }
      -- TODO: Implement actual dragging with mouse move/up events

  StopDrag -> do
    H.modify_ _ { dragging = false }

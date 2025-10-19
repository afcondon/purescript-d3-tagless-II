module PSD3.Components.SplitPane where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Effect.Class (liftEffect)
import Effect (Effect)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Data.Foldable (traverse_)
import PSD3.Components.Visualization as Visualization
import Type.Proxy (Proxy(..))

-- | Input for the SplitPane component
type Input =
  { code :: String
  , language :: String
  , visualizationUrl :: Maybe String
  , exampleId :: String
  }

-- | Slots for child components
type Slots =
  ( visualization :: forall q. H.Slot q Void Unit
  )

-- | State tracks which tab is active (mobile), fullscreen mode, and code reference
type State =
  { code :: String
  , language :: String
  , visualizationUrl :: Maybe String
  , exampleId :: String
  , activeTab :: Tab
  , isFullscreen :: Boolean
  , showCode :: Boolean  -- In fullscreen mode, whether code panel is visible
  }

-- | Tabs for mobile view
data Tab = CodeTab | VisualizationTab

derive instance eqTab :: Eq Tab

-- | Actions
_visualization = Proxy :: Proxy "visualization"

data Action
  = Initialize
  | SetTab Tab
  | HighlightCode
  | ToggleFullscreen
  | ToggleCodePanel

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
  { code: input.code
  , language: input.language
  , visualizationUrl: input.visualizationUrl
  , exampleId: input.exampleId
  , activeTab: CodeTab
  , isFullscreen: false
  , showCode: false
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  if state.isFullscreen
    then renderFullscreen state
    else renderSplitView state

-- | Render full-screen mode
renderFullscreen :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderFullscreen state =
  HH.div
    [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "split-pane-fullscreen" ] ]
    [ -- Full-screen visualization
      HH.div
        [ HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "split-pane-fullscreen__viz" ] ]
        [ HH.slot_ _visualization unit Visualization.component
            { exampleId: state.exampleId }
        ]

    , -- Floating toolbar (top-right)
      HH.div
        [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "floating-panel--small", HH.ClassName "split-pane-fullscreen__toolbar" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn--secondary" ]
            , HE.onClick \_ -> ToggleCodePanel
            , HP.title if state.showCode then "Hide Code" else "Show Code"
            ]
            [ HH.text if state.showCode then "Hide Code" else "Show Code" ]
        , HH.button
            [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn--secondary" ]
            , HE.onClick \_ -> ToggleFullscreen
            , HP.title "Exit Fullscreen"
            ]
            [ HH.text "Exit Fullscreen" ]
        ]

    , -- Floating code panel (left side, conditionally visible)
      if state.showCode
        then
          HH.div
            [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "floating-panel--large", HH.ClassName "split-pane-fullscreen__code" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                [ HH.text "PureScript Code" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "line-numbers" ] ]
                [ HH.code
                    [ HP.classes [ HH.ClassName ("language-" <> state.language) ]
                    , HP.id ("code-fullscreen-" <> state.exampleId)
                    ]
                    [ HH.text state.code ]
                ]
            ]
        else HH.text ""
    ]

-- | Render standard split-pane view
renderSplitView :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderSplitView state =
  HH.div
    [ HP.classes [ HH.ClassName "split-pane" ] ]
    [ -- Mobile tabs
      HH.div
        [ HP.classes [ HH.ClassName "split-pane__tabs", HH.ClassName "mobile-only" ] ]
        [ HH.button
            [ HP.classes $
                [ HH.ClassName "split-pane__tab" ] <>
                if state.activeTab == CodeTab
                  then [ HH.ClassName "split-pane__tab--active" ]
                  else []
            , HE.onClick \_ -> SetTab CodeTab
            ]
            [ HH.text "Code" ]
        , HH.button
            [ HP.classes $
                [ HH.ClassName "split-pane__tab" ] <>
                if state.activeTab == VisualizationTab
                  then [ HH.ClassName "split-pane__tab--active" ]
                  else []
            , HE.onClick \_ -> SetTab VisualizationTab
            ]
            [ HH.text "Visualization" ]
        ]

    , -- Split pane content
      HH.div
        [ HP.classes [ HH.ClassName "split-pane__content" ] ]
        [ -- Code panel
          HH.div
            [ HP.classes $
                [ HH.ClassName "split-pane__panel", HH.ClassName "split-pane__panel--code" ] <>
                if state.activeTab == CodeTab
                  then []
                  else [ HH.ClassName "split-pane__panel--hidden" ]
            ]
            [ HH.div
                [ HP.classes [ HH.ClassName "split-pane__code-header" ] ]
                [ HH.h3_ [ HH.text "PureScript Code" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "split-pane__fullscreen-button" ]
                    , HE.onClick \_ -> ToggleFullscreen
                    , HP.title "Enter Fullscreen"
                    ]
                    [ HH.text "⛶ Fullscreen" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "split-pane__copy-button" ]
                    , HP.title "Copy to clipboard"
                    ]
                    [ HH.text "Copy" ]
                ]
            , HH.pre
                [ HP.classes [ HH.ClassName "line-numbers" ] ]
                [ HH.code
                    [ HP.classes [ HH.ClassName ("language-" <> state.language) ]
                    , HP.id ("code-" <> state.exampleId)
                    ]
                    [ HH.text state.code ]
                ]
            ]

        , -- Visualization panel
          HH.div
            [ HP.classes $
                [ HH.ClassName "split-pane__panel", HH.ClassName "split-pane__panel--visualization" ] <>
                if state.activeTab == VisualizationTab || true  -- always show on desktop
                  then []
                  else [ HH.ClassName "split-pane__panel--hidden" ]
            ]
            [ HH.div
                [ HP.classes [ HH.ClassName "split-pane__viz-header" ] ]
                [ HH.h3_ [ HH.text "Visualization" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "split-pane__fullscreen-button" ]
                    , HE.onClick \_ -> ToggleFullscreen
                    , HP.title "Enter Fullscreen"
                    ]
                    [ HH.text "⛶" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "split-pane__viz-content" ] ]
                [ HH.slot_ _visualization unit Visualization.component
                    { exampleId: state.exampleId }
                ]
            ]
        ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- Trigger syntax highlighting after component mounts
    handleAction HighlightCode

  SetTab tab -> do
    H.modify_ _ { activeTab = tab }

  ToggleFullscreen -> do
    state <- H.get
    H.modify_ _ { isFullscreen = not state.isFullscreen }
    -- Re-highlight code in new mode
    handleAction HighlightCode

  ToggleCodePanel -> do
    state <- H.get
    H.modify_ _ { showCode = not state.showCode }
    -- Highlight code when showing panel
    when state.showCode $ handleAction HighlightCode

  HighlightCode -> do
    state <- H.get
    -- Call Prism.highlightAll() via FFI
    H.liftEffect do
      win <- window
      htmlDoc <- document win
      let doc = toDocument htmlDoc
      let node = toNonElementParentNode doc
      -- Try both code element IDs (normal and fullscreen)
      maybeEl1 <- getElementById ("code-" <> state.exampleId) node
      traverse_ highlightElement maybeEl1
      maybeEl2 <- getElementById ("code-fullscreen-" <> state.exampleId) node
      traverse_ highlightElement maybeEl2

-- | FFI function to trigger Prism highlighting
foreign import highlightElement :: forall a. a -> Effect Unit

module V2.Components.SplitPane where

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

-- | Input for the SplitPane component
type Input =
  { code :: String
  , language :: String
  , visualizationUrl :: Maybe String
  , exampleId :: String
  }

-- | No slots needed
type Slots :: forall k. Row k
type Slots = ()

-- | State tracks which tab is active (mobile) and code reference
type State =
  { code :: String
  , language :: String
  , visualizationUrl :: Maybe String
  , exampleId :: String
  , activeTab :: Tab
  }

-- | Tabs for mobile view
data Tab = CodeTab | VisualizationTab

derive instance eqTab :: Eq Tab

-- | Actions
data Action
  = Initialize
  | SetTab Tab
  | HighlightCode

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
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
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
                [ HH.h3_ [ HH.text "Visualization" ] ]
            , case state.visualizationUrl of
                Just url ->
                  HH.iframe
                    [ HP.classes [ HH.ClassName "split-pane__iframe" ]
                    , HP.src url
                    , HP.attr (HH.AttrName "frameborder") "0"
                    ]
                Nothing ->
                  HH.div
                    [ HP.classes [ HH.ClassName "split-pane__placeholder" ] ]
                    [ HH.p_ [ HH.text "Visualization coming soon..." ] ]
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

  HighlightCode -> do
    state <- H.get
    -- Call Prism.highlightAll() via FFI
    H.liftEffect do
      win <- window
      htmlDoc <- document win
      let doc = toDocument htmlDoc
      let node = toNonElementParentNode doc
      maybeEl <- getElementById ("code-" <> state.exampleId) node
      traverse_ highlightElement maybeEl

-- | FFI function to trigger Prism highlighting
foreign import highlightElement :: forall a. a -> Effect Unit

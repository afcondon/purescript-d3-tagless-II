module PSD3.Pages.ExampleDetail where

import Prelude

import PSD3.Types (ExampleId, Route(..))
import PSD3.Data.Examples (getExample)
import PSD3.Data.CodeFiles (getCodeFileUrl)
import PSD3.Router (routeToHash)
import PSD3.Components.Visualization as Visualization
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Type.Proxy (Proxy(..))
import Data.Either (Either(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Effect (Effect)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Data.Foldable (traverse_)

type Input = ExampleId

type State = {
  exampleId :: ExampleId,
  code :: Maybe String,
  loading :: Boolean,
  error :: Maybe String,
  showCode :: Boolean,
  showInfo :: Boolean,
  codeTranslucent :: Boolean  -- If true, code panel is semi-transparent overlay
}

type Slots =
  ( visualization :: forall q. H.Slot q Void Unit
  )

_visualization = Proxy :: Proxy "visualization"

data Action
  = Initialize
  | LoadCode
  | ToggleCode
  | ToggleInfo
  | ToggleCodeTranslucency
  | HighlightCode

-- | Example detail page
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
initialState exampleId =
  { exampleId
  , code: Nothing
  , loading: true
  , error: Nothing
  , showCode: true  -- Start with code visible
  , showInfo: false  -- Start with info hidden
  , codeTranslucent: true  -- Start translucent
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  case getExample state.exampleId of
    Nothing ->
      HH.div
        [ HP.classes [ HH.ClassName "example-detail" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "example-detail__not-found" ] ]
            [ HH.h1_ [ HH.text "Example Not Found" ]
            , HH.p_ [ HH.text $ "Could not find example with ID: " <> state.exampleId ]
            , HH.a
                [ HP.href $ routeToHash Gallery
                , HP.classes [ HH.ClassName "example-detail__back-link" ]
                ]
                [ HH.text "← Back to Gallery" ]
            ]
        ]

    Just example ->
      HH.div
        [ HP.classes [ HH.ClassName "fullscreen-page-wrapper", HH.ClassName "example-fullscreen" ] ]
        [ -- Full-screen container with visualization
          HH.div
            [ HP.classes [ HH.ClassName "fullscreen-container" ] ]
            [ -- Main visualization fills viewport
              HH.div
                [ HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "example-fullscreen__viz" ] ]
                [ HH.slot_ _visualization unit Visualization.component
                    { exampleId: state.exampleId }
                ]

            , -- Floating toolbar (left side) with integrated info
              HH.div
                [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "example-fullscreen__toolbar" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "example-fullscreen__toolbar-title" ] ]
                    [ HH.text example.title ]
                , HH.a
                    [ HP.href $ routeToHash Gallery
                    , HP.classes [ HH.ClassName "btn", HH.ClassName "btn--secondary" ]
                    , HP.title "Back to Gallery"
                    ]
                    [ HH.text "← Gallery" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn--secondary" ]
                    , HE.onClick \_ -> ToggleInfo
                    , HP.title if state.showInfo then "Hide Info" else "Show Info"
                    ]
                    [ HH.text if state.showInfo then "Hide Info" else "ℹ Info" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn--secondary" ]
                    , HE.onClick \_ -> ToggleCode
                    , HP.title if state.showCode then "Hide Code" else "Show Code"
                    , HP.disabled state.loading
                    ]
                    [ HH.text if state.showCode then "Hide Code" else "{ } Code" ]
                , if state.showCode
                    then HH.button
                      [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn--ghost" ]
                      , HE.onClick \_ -> ToggleCodeTranslucency
                      , HP.title if state.codeTranslucent then "Make Solid" else "Make Translucent"
                      ]
                      [ HH.text if state.codeTranslucent then "▢" else "▣" ]
                    else HH.text ""
                -- Info section (collapsible, inside toolbar)
                , if state.showInfo
                    then HH.div
                      [ HP.classes [ HH.ClassName "example-fullscreen__info-section" ] ]
                      [ HH.div
                          [ HP.classes [ HH.ClassName "example-fullscreen__info-label" ] ]
                          [ HH.text "About" ]
                      , HH.p
                          [ HP.classes [ HH.ClassName "example-fullscreen__description" ] ]
                          [ HH.text example.description ]
                      , HH.p
                          [ HP.classes [ HH.ClassName "example-fullscreen__about" ] ]
                          [ HH.text example.about ]
                      ]
                    else HH.text ""
                ]

            , -- Floating code panel (right side, conditionally visible)
              if state.showCode
                then case state.code of
                  Just code ->
                    HH.div
                      [ HP.classes $
                          [ HH.ClassName "floating-panel"
                          , HH.ClassName "floating-panel--top-right"
                          , HH.ClassName "example-fullscreen__code"
                          ] <> if state.codeTranslucent
                            then [ HH.ClassName "example-fullscreen__code--translucent" ]
                            else []
                      ]
                      [ HH.div
                          [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                          [ HH.text "PureScript Code" ]
                      , HH.pre
                          [ HP.classes [ HH.ClassName "line-numbers" ] ]
                          [ HH.code
                              [ HP.classes [ HH.ClassName "language-haskell" ]
                              , HP.id ("code-" <> state.exampleId)
                              ]
                              [ HH.text code ]
                          ]
                      ]
                  Nothing ->
                    if state.loading
                      then HH.div
                        [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "example-fullscreen__loading" ] ]
                        [ HH.text "Loading code..." ]
                      else HH.text ""
                else HH.text ""
            ]
        ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction LoadCode

  LoadCode -> do
    state <- H.get
    case getCodeFileUrl state.exampleId of
      Nothing -> do
        H.modify_ _ { loading = false, error = Just "Code file not found" }
      Just url -> do
        H.modify_ _ { loading = true, error = Nothing }
        result <- H.liftAff $ AX.get ResponseFormat.string url
        case result of
          Left err -> do
            H.modify_ _
              { loading = false
              , error = Just $ "Failed to load code: " <> AX.printError err
              }
          Right response -> do
            H.modify_ _
              { loading = false
              , code = Just response.body
              , error = Nothing
              }
            -- Trigger syntax highlighting after code loads
            handleAction HighlightCode

  ToggleCode -> do
    state <- H.get
    let newShowCode = not state.showCode
    H.modify_ _ { showCode = newShowCode }
    -- Highlight code when showing (wait for DOM update)
    when newShowCode $ handleAction HighlightCode

  ToggleInfo -> do
    state <- H.get
    H.modify_ _ { showInfo = not state.showInfo }

  ToggleCodeTranslucency -> do
    state <- H.get
    H.modify_ _ { codeTranslucent = not state.codeTranslucent }

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

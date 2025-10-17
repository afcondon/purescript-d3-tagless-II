module V2.Pages.ExampleDetail where

import Prelude

import V2.Types (ExampleId)
import V2.Data.Examples (getExample)
import V2.Data.CodeFiles (getCodeFileUrl, getVisualizationUrl)
import V2.Router (routeToHash)
import V2.Types (Route(..))
import V2.Components.SplitPane as SplitPane
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Type.Proxy (Proxy(..))
import Data.Either (Either(..))

type Input = ExampleId

type State = {
  exampleId :: ExampleId,
  code :: Maybe String,
  loading :: Boolean,
  error :: Maybe String
}

type Slots =
  ( splitPane :: forall q. H.Slot q Void Unit
  )

_splitPane = Proxy :: Proxy "splitPane"

data Action
  = Initialize
  | LoadCode

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
        [ HP.classes [ HH.ClassName "example-detail" ] ]
        [ -- Header
          HH.div
            [ HP.classes [ HH.ClassName "example-detail__header" ] ]
            [ HH.a
                [ HP.href $ routeToHash Gallery
                , HP.classes [ HH.ClassName "example-detail__back-link" ]
                ]
                [ HH.text "← Back to Gallery" ]
            , HH.h1
                [ HP.classes [ HH.ClassName "example-detail__title" ] ]
                [ HH.text example.title ]
            , HH.p
                [ HP.classes [ HH.ClassName "example-detail__description" ] ]
                [ HH.text example.description ]
            ]

        , -- About section
          HH.div
            [ HP.classes [ HH.ClassName "example-detail__about" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "example-detail__about-title" ] ]
                [ HH.text "About This Example" ]
            , HH.p
                [ HP.classes [ HH.ClassName "example-detail__about-text" ] ]
                [ HH.text example.about ]
            ]

        , -- Main content area
          HH.div
            [ HP.classes [ HH.ClassName "example-detail__content" ] ]
            [ if state.loading then
                HH.div
                  [ HP.classes [ HH.ClassName "example-detail__loading" ] ]
                  [ HH.text "Loading code..." ]
              else case state.error of
                Just err ->
                  HH.div
                    [ HP.classes [ HH.ClassName "example-detail__error" ] ]
                    [ HH.h3_ [ HH.text "Error Loading Code" ]
                    , HH.p_ [ HH.text err ]
                    , HH.p_
                        [ HH.text "View this example in "
                        , HH.a
                            [ HP.href "../v1/"
                            , HP.target "_blank"
                            ]
                            [ HH.text "V1" ]
                        ]
                    ]
                Nothing ->
                  case state.code of
                    Just code ->
                      HH.slot_ _splitPane unit SplitPane.component
                        { code
                        , language: "haskell"  -- PureScript uses Haskell syntax highlighting
                        , visualizationUrl: getVisualizationUrl state.exampleId
                        , exampleId: state.exampleId
                        }
                    Nothing ->
                      HH.div
                        [ HP.classes [ HH.ClassName "example-detail__no-code" ] ]
                        [ HH.text "No code available for this example." ]
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

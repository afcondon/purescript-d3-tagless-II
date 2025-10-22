module PSD3.About where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Types (Route(..))

type State = {
  content :: Maybe String,
  toc :: Maybe String,
  loading :: Boolean,
  error :: Maybe String
}

data Action
  = Initialize
  | LoadContent
  | LoadTOC

type Slots :: forall k. Row k
type Slots = ()

-- | About page component that loads content from about-content.html
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: State
initialState =
  { content: Nothing
  , toc: Nothing
  , loading: true
  , error: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "about-page" ] ]
    [ -- TOC Panel (if loaded)
      case state.toc of
        Just tocHtml ->
          HH.div
            [ HP.classes [ HH.ClassName "about-page__toc-container" ]
            , HP.prop (H.PropName "innerHTML") tocHtml
            ]
            []
        Nothing -> HH.text ""

    , HH.div
        [ HP.classes [ HH.ClassName "about-page__container", HH.ClassName "editorial" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "about-page__header" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Home
                , HP.classes [ HH.ClassName "about-page__back-link" ]
                ]
                [ HH.text "← Home" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "about-page__content" ] ]
            [ case state.content of
                Nothing ->
                  if state.loading
                    then HH.div
                      [ HP.classes [ HH.ClassName "about-page__loading" ] ]
                      [ HH.text "Loading..." ]
                    else case state.error of
                      Just err -> HH.div
                        [ HP.classes [ HH.ClassName "about-page__error" ] ]
                        [ HH.text $ "Error loading content: " <> err ]
                      Nothing -> HH.text ""
                Just html ->
                  HH.div
                    [ HP.classes [ HH.ClassName "about-page__markdown" ]
                    -- Unsafe HTML - but we control the source (our own markdown)
                    , HP.prop (H.PropName "innerHTML") html
                    ]
                    []
            ]
        ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction LoadContent
    handleAction LoadTOC

  LoadContent -> do
    H.modify_ _ { loading = true, error = Nothing }
    result <- H.liftAff $ AX.get ResponseFormat.string "about-content.html"
    case result of
      Left err -> do
        H.modify_ _
          { loading = false
          , error = Just $ AX.printError err
          }
      Right response -> do
        H.modify_ _
          { loading = false
          , content = Just response.body
          , error = Nothing
          }

  LoadTOC -> do
    result <- H.liftAff $ AX.get ResponseFormat.string "about-toc.html"
    case result of
      Left _ -> pure unit -- Silently fail if TOC doesn't load
      Right response -> do
        H.modify_ _ { toc = Just response.body }

module PSD3.Reference.Modules.Types where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.PrismJS (highlightAll)
import PSD3.Reference.SourceLoader (loadSourceFile)

-- | State
type State =
  { sourceCode :: Maybe String
  , loading :: Boolean
  , error :: Maybe String
  }

-- | Actions
data Action
  = Initialize
  | SourceLoaded (Either String String)

-- | PSD3.Types module viewer
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { sourceCode: Nothing
      , loading: true
      , error: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "module-page" ] ]
    [ -- Module header
      HH.section
        [ HP.classes [ HH.ClassName "module-page__header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "module-page__title" ] ]
            [ HH.text "PSD3.Types" ]
        , HH.p
            [ HP.classes [ HH.ClassName "module-page__description" ] ]
            [ HH.text "Common type definitions used throughout PSD3" ]
        ]

    -- Custom content section (add explanations, diagrams, etc. here)
    , HH.section
        [ HP.classes [ HH.ClassName "module-page__content" ] ]
        [
          -- TODO: Add custom explanatory content here
          -- Examples: diagrams, usage examples, conceptual explanations
        ]

    -- Source code
    , HH.section
        [ HP.classes [ HH.ClassName "module-page__source" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "module-page__source-title" ] ]
            [ HH.text "Source Code" ]
        , case state.sourceCode of
            Nothing ->
              if state.loading
                then HH.div
                  [ HP.classes [ HH.ClassName "module-page__loading" ] ]
                  [ HH.text "Loading source code..." ]
                else case state.error of
                  Just err ->
                    HH.div
                      [ HP.classes [ HH.ClassName "module-page__error" ] ]
                      [ HH.text err ]
                  Nothing ->
                    HH.div
                      [ HP.classes [ HH.ClassName "module-page__error" ] ]
                      [ HH.text "Failed to load source code" ]
            Just code ->
              HH.div
                [ HP.classes [ HH.ClassName "module-page__code" ] ]
                [ HH.pre
                    [ HP.classes [ HH.ClassName "line-numbers" ] ]
                    [ HH.code
                        [ HP.classes [ HH.ClassName "language-haskell" ] ]
                        [ HH.text code ]
                    ]
                ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Load source file
    result <- H.liftAff $ loadSourceFile "PSD3/Types.purs"
    handleAction $ SourceLoaded result

  SourceLoaded result -> do
    case result of
      Left err -> do
        H.modify_ _ { loading = false, error = Just err }
      Right code -> do
        H.modify_ _ { sourceCode = Just code, loading = false, error = Nothing }
        liftEffect highlightAll

module PSD3.Reference.ModuleViewer where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Reference.ModuleRegistry (ModuleInfo)

-- | Input: module to display
type Input = ModuleInfo

-- | State: module info and source code
type State =
  { moduleInfo :: ModuleInfo
  , sourceCode :: Maybe String
  , loading :: Boolean
  }

-- | Actions
data Action
  = Initialize
  | LoadSource
  | SourceLoaded String

-- | Module viewer component
component :: forall q o. H.Component q Input o Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: Input -> State
initialState moduleInfo =
  { moduleInfo
  , sourceCode: Nothing
  , loading: true
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "module-viewer" ] ]
    [ -- Module header
      HH.div
        [ HP.classes [ HH.ClassName "module-viewer__header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "module-viewer__title" ] ]
            [ HH.text state.moduleInfo.name ]
        , HH.p
            [ HP.classes [ HH.ClassName "module-viewer__description" ] ]
            [ HH.text state.moduleInfo.description ]
        ]

    -- Source code
    , case state.sourceCode of
        Nothing ->
          if state.loading
            then HH.div
              [ HP.classes [ HH.ClassName "module-viewer__loading" ] ]
              [ HH.text "Loading source code..." ]
            else HH.div
              [ HP.classes [ HH.ClassName "module-viewer__error" ] ]
              [ HH.text "Failed to load source code" ]
        Just code ->
          HH.div
            [ HP.classes [ HH.ClassName "module-viewer__code" ] ]
            [ HH.pre
                [ HP.classes [ HH.ClassName "line-numbers" ] ]
                [ HH.code
                    [ HP.classes
                        [ HH.ClassName "language-haskell"  -- Prism uses Haskell for PureScript
                        ]
                    ]
                    [ HH.text code ]
                ]
            ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    handleAction LoadSource

  LoadSource -> do
    state <- H.get
    let path = "sources/" <> state.moduleInfo.path
    let placeholderCode =
          "module " <> state.moduleInfo.name <> " where\n\n" <>
          "import Prelude\n\n" <>
          "-- " <> state.moduleInfo.description <> "\n" <>
          "--\n" <>
          "-- This is a placeholder. Source code will be loaded from:\n" <>
          "-- " <> path <> "\n" <>
          "--\n" <>
          "-- The actual implementation will fetch the source file and display it here\n" <>
          "-- with full syntax highlighting via Prism.js.\n\n" <>
          "-- Example type signature:\n" <>
          "exampleFunction :: forall m. Monad m => String -> m Unit\n" <>
          "exampleFunction str = pure unit\n"

    H.modify_ _ { sourceCode = Just placeholderCode, loading = false }

    -- Trigger Prism highlighting
    liftEffect $ highlightAll

  SourceLoaded code -> do
    H.modify_ _ { sourceCode = Just code, loading = false }
    liftEffect $ highlightAll

-- | FFI to Prism.highlightAll()
foreign import highlightAll :: Effect Unit

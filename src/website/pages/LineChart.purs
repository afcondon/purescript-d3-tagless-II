module PSD3.Pages.LineChart where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Examples.Charts.Model as Charts
import D3.Examples.LineChart as LineChart
import PSD3.Components.Blocks.Expandable as Expandable
import PSD3.Components.Blocks.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import PSD3.Components.UI.FormField as FormField
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Snippets (readSnippetFiles)
import PSD3.Pages.Utilities (blurbParagraphs, syntaxHighlightedCode)
import PSD3.Pages.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)

type State = {
    panels :: { blurb :: Expandable.Status, code :: Expandable.Status }
  , snippets :: { draw :: String }
}

_panels = prop (Proxy :: Proxy "panels")
_snippets = prop (Proxy :: Proxy "snippets")

_drawCode :: Lens' State String
_drawCode = _snippets <<< prop (Proxy :: Proxy "draw")

_blurb :: Lens' State Expandable.Status
_blurb = _panels <<< prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = _panels <<< prop (Proxy :: Proxy "code")

component :: forall query output m.
  MonadAff m =>
  H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize = Just Finalize
    }
  }
  where
  initialState :: State
  initialState = {
      panels: { blurb: Expandable.Collapsed, code: Expandable.Collapsed }
    , snippets: { draw: "" }
  }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div
          [ Utils.tailwindClass "story-panel-about" ]
          [ FormField.field_
              { label: HH.text "About"
              , helpText: []
              , error: []
              , inputId: "show-blurb"
              }
            [ Toggle.toggle
                [ HP.id "show-blurb"
                , HP.checked $ Expandable.toBoolean (view _blurb state)
                , HE.onChange \_ -> ToggleCard _blurb
                ]
            ]
          , Expandable.content_ (view _blurb state) blurbtext
          ]
      , HH.div
          [ Utils.tailwindClass "story-panel-code" ]
          [ FormField.field_
              { label: HH.text "(hide this panel if screen too small)"
              , helpText: []
              , error: []
              , inputId: "show-code"
              }
            [ Toggle.toggle
                [ HP.id "show-code"
                , HP.checked $ Expandable.toBoolean (view _code state)
                , HE.onChange \_ -> ToggleCard _code
                ]
            ]
          , Expandable.content_ (view _code state) $ syntaxHighlightedCode (view _drawCode state)
          ]
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]

handleAction :: forall m.
  Bind m =>
  MonadAff m =>
  MonadState State m =>
  Action -> m Unit
handleAction = case _ of
  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    text1 <- H.liftAff $ readSnippetFiles "LineChartDraw"
    _drawCode .= text1

    _ <- H.liftEffect $ eval_D3M $ LineChart.draw Charts.sineWaveData "div.svg-container"
    pure unit

  Finalize -> pure unit

blurbtext :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext = blurbParagraphs [
    """Line charts are one of the most fundamental visualizations for showing trends
    over time or continuous data. They excel at displaying patterns, trends, and
    changes in data series."""

  , """This example demonstrates a simple line chart showing a sine wave pattern.
    The implementation uses D3's scale functions to map data values to pixel
    coordinates, and a line generator to create the SVG path."""

  , """Line charts are commonly used for: time series data, stock prices, temperature
    readings, website analytics, and any scenario where you need to show how a
    value changes continuously."""
]

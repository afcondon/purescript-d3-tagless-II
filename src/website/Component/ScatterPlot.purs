module PSD3.ScatterPlot where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Viz.Charts.Model as Charts
import D3.Viz.ScatterPlot as ScatterPlot
import PSD3.Expandable as Expandable
import PSD3.Toggle as Toggle
import PSD3.Interpreter.D3 (eval_D3M)
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import PSD3.FormField as FormField
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Snippets (readSnippetFiles)
import PSD3.Utilities (blurbParagraphs, syntaxHighlightedCode)
import PSD3.Utilities as Utils
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
    text1 <- H.liftAff $ readSnippetFiles "ScatterPlotQuartet"
    _drawCode .= text1

    _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet Charts.anscombesQuartet "div.svg-container"
    pure unit

  Finalize -> pure unit

blurbtext :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext = blurbParagraphs [
    """This example demonstrates Anscombe's Quartet, a famous dataset created by
    statistician Francis Anscombe in 1973. All four datasets have nearly identical
    statistical properties (same mean, variance, correlation, and linear regression line),
    yet when visualized they reveal completely different patterns."""

  , """The quartet powerfully illustrates why data visualization is essential.
    Summary statistics alone can be misleading - you need to look at the data to
    understand its true structure. Dataset I shows a linear relationship, Dataset II
    is clearly non-linear, Dataset III has an outlier affecting the regression, and
    Dataset IV shows how a single extreme value can dominate the statistics."""

  , """This implementation uses a 'small multiples' layout (term coined by Edward Tufte),
    displaying the four related charts side-by-side for easy comparison. All plots share
    the same scale domains, making visual comparison straightforward."""
]

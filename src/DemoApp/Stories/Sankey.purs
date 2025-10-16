module Stories.Sankey where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Examples.Sankey.Model as Sankey
import D3.Examples.SankeyDiagram as SankeyDiagram
import D3.Layouts.Sankey.Types (SankeyLayoutState_, initialSankeyLayoutState)
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Sankey (runWithD3_Sankey)
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.Record (prop)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DemoApp.UI.FormField as FormField
import Snippets (readSnippetFiles)
import Stories.Utilities (blurbParagraphs, syntaxHighlightedCode)
import Stories.Utilities as Utils
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)

type State = {
    sankeyLayout :: SankeyLayoutState_
  , panels :: { blurb :: Expandable.Status, code :: Expandable.Status }
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
      sankeyLayout: initialSankeyLayoutState
    , panels: { blurb: Expandable.Collapsed, code: Expandable.Collapsed }
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
    text1 <- H.liftAff $ readSnippetFiles "SankeyDraw"
    _drawCode .= text1

    runWithD3_Sankey do
      SankeyDiagram.draw Sankey.energyData "div.svg-container"

  Finalize -> pure unit

blurbtext :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext = blurbParagraphs [
    """Sankey diagrams visualize the flow of resources, energy, costs, or other
    quantities through a system. The width of each connection (link) is
    proportional to the flow quantity, making it easy to see dominant flows at
    a glance."""

  , """This example shows energy flows in the UK energy system. The diagram uses
    D3's Sankey layout algorithm to automatically position nodes and compute
    smooth flow paths between them. Nodes are arranged in columns, and the
    algorithm minimizes crossing connections where possible."""

  , """The implementation follows the Finally Tagless pattern used throughout this
    library. The SankeyM capability extends SelectionM with layout-specific
    operations for processing nodes and links through the Sankey algorithm."""

  , """Common uses for Sankey diagrams include: energy and material flow analysis,
    cost breakdowns, website traffic visualization, and any scenario where you
    need to show how quantities are distributed and transformed through a
    multi-stage process."""
]

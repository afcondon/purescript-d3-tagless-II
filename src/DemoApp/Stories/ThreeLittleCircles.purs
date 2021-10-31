module Stories.ThreeLittleCircles where

import Prelude

import Control.Monad.State (class MonadState, modify_)
import D3.Examples.ThreeLittleCircles as Circles
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.FormField as FormField
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Snippets (Cell(..), Notebook, renderNotebook_, substituteSnippetCells)
import Stories.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)
  | ToggleExample
  
type State = {
    toggle   :: Boolean -- Toggle between ultra simple and merely super-simple examples
  , code     :: Expandable.Status
  , notebooks :: forall w. { simple :: Notebook Unit w Action, parabola :: Notebook Unit w Action}
}

-- some lenses definitions to make setting and using the snippet easier
_code = prop (Proxy :: Proxy "code")
_notebooks = prop (Proxy :: Proxy "notebooks")
_simple = _notebooks <<< prop (Proxy :: Proxy "simple")
_parabola = _notebooks <<< prop (Proxy :: Proxy "parabola")

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize  }
  }
  where

  initialState :: State
  initialState = { 
      toggle: true
    , code:   Expandable.Expanded
    , notebooks: { simple, parabola }
  }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div
            [ Utils.tailwindClass "story-panel-code"]
            [ FormField.field_
                { label: HH.text "(hide this panel if screen too small)"
                , helpText: []
                , error: []
                , inputId: "show-code"
                }
              [ Toggle.toggle
                [ HP.id "show-code"
                , HP.checked
                  $ Expandable.toBoolean state.code
                , HE.onChange \_ -> ToggleCard _code
                ]
              ]
            , Expandable.content_ state.code $ 
                if state.toggle 
                then renderNotebook_ (view _simple state)
                else renderNotebook_ (view _parabola state)
            ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]
        
handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    simple' <- traverse substituteSnippetCells simple
    _simple .= simple'
    parabola' <- traverse substituteSnippetCells parabola
    _parabola .= parabola'
    _ <- H.liftEffect $ eval_D3M $ Circles.drawThreeCircles "div.svg-container"
    pure unit

  ToggleExample -> do
    toggle <- H.gets _.toggle
    let toggle' = not toggle
        container = "div.svg-container"
    void $ H.liftEffect $ eval_D3M $ removeExistingSVG container
    void $ H.liftEffect $ eval_D3M $ 
      if toggle'
      then Circles.drawThreeCircles container
      else Circles.drawWithData [310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310] container
    modify_ (\s -> s { toggle = toggle' })

  Finalize -> pure unit

-- simple :: forall w i. Notebook w i
simple :: forall w. Notebook Unit w Action
simple = [
    Blurb "Simplest possible example, just to show syntax."
  , SnippetFile "TLCSimple"
  , Blurb "Click the button to see a slightly more realistic example."
  , PreRendered $ 
      Button.buttonVertical
        [ HE.onClick $ const ToggleExample ]
        [ HH.text "Simple" ]
]

parabola :: forall w. Notebook Unit w Action
parabola = [
    Blurb "This extends the super-simple model in the direction one would go for a more real-world example."
  , SnippetFile "TLCParabola"

  , Blurb """In this example, the data is passed in and must match the type
  specified in the Model. Because the data loses its type information when
  put into D3 we recover the type of Datum and Index using a couple of
  functions to wrap unsafeCoerce. These functions are then used to write
  any attribute setters that are derived from the data elements themselves,
  or their indices"""

  , SnippetFile "TLCDatum_"

  , Blurb """Again, we're just showing syntax and shape of the DSL here: it's still extremely simple, and the Model,
  datum_ and so on might not be needed for such a simple example."""

  , PreRendered $ 
      Button.buttonVertical
        [ HE.onClick $ const ToggleExample ]
        [ HH.text "Parabola" ]
]

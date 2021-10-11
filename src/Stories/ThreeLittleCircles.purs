module Stories.ThreeLittleCircles where

import Data.Lens
import Prelude

import Control.Monad.State (class MonadState, modify_)
import D3.Examples.ThreeLittleCircles as Circles
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.FormField as FormField
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Array (singleton)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as RH
import Snippets (readSnippetFiles)
import Stories.Utilities (highlightString_)
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
  , snippets :: { ex1 :: String, ex2 :: String }
}

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

_snippets = prop (Proxy :: Proxy "snippets")

_ex1 :: Lens' State String
_ex1 = _snippets <<< prop (Proxy :: Proxy "ex1")

_ex2 :: Lens' State String
_ex2 = _snippets <<< prop (Proxy :: Proxy "ex2")

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
    , snippets: { ex1: "" , ex2: "" }
  }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div
            [ Utils.tailwindClass "story-panel-controls"] 
            [ HH.text $ if state.toggle then "Ex 1" else "Ex 2"
            , Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
              [ Button.buttonVertical
                [ HE.onClick $ const ToggleExample ]
                [ HH.text "Toggle" ]
              ] 
            ]
      , HH.div
            [ Utils.tailwindClass "story-panel-code"]
            [ FormField.field_
                { label: HH.text "Code"
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
            , Expandable.content_ state.code $ if state.toggle then blurbtext1 else blurbtext2
            , Expandable.content_ state.code
                [ HH.pre 
                  [ HP.class_ $ HH.ClassName "language-purescript" ]  
                  [ HH.code_ [ RH.render_ $ highlightString_ $ if state.toggle then view _ex1 state else view _ex2 state ] ]
                ]
            ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]
        
handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> lens %= not

  Initialize -> do 
    text1 <- H.liftAff $ readSnippetFiles "TLCSimple"
    _ex1 .= text1
    text2 <- H.liftAff $ readSnippetFiles "TLCParabola"
    _ex2 .= text2
    _ <- H.liftEffect $ eval_D3M $ Circles.drawThreeCircles "div.svg-container"
    pure unit

  ToggleExample -> do
    toggle <- H.gets _.toggle
    let toggle' = not toggle
        container = "div.svg-container"
    _ <- H.liftEffect $ eval_D3M $ removeExistingSVG container
    _ <- H.liftEffect $ eval_D3M $ 
          if toggle'
          then Circles.drawThreeCircles container
          else Circles.drawWithData [310, 474, 613, 726, 814, 877, 914, 926, 913, 874, 810] container
    modify_ (\s -> s { toggle = toggle' })

  Finalize -> pure unit

blurbtext1 :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext1 = (HH.p [ HP.classes [ HH.ClassName "m-2", HH.ClassName "w-2/3" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where 
    texts = ["Simplest possible example, just to show syntax." ]

blurbtext2 :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext2 = (HH.p [ HP.classes [ HH.ClassName "m-2" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where 
    texts = [
      
      """This extends the super-simple model in the direction one would go for
      a more real-world example."""

      , """In this example, the data is passed in and must match the type
      specified in the Model. Because the data loses its type information when
      put into D3 we recover the type of Datum and Index using a couple of
      functions to wrap unsafeCoerce. These functions are then used to write
      any attribute setters that are derived from the data elements themselves,
      or their indices"""

      , """Again, we're just showing syntax and shape of the DSL here: it's still extremely simple, and the Model,
      datum_ and so on might not be needed for such a simple example."""
    
    ]

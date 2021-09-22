module Stories.ThreeLittleCircles where

import Data.Lens
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
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
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
    toggle  :: Boolean -- Toggle between ultra simple and merely super-simple examples
  , blurb   :: Expandable.Status
  , code    :: Expandable.Status
  , ex1     :: String
  , ex2     :: String
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

_ex1 :: Lens' State String
_ex1 = prop (Proxy :: Proxy "ex1")

_ex2 :: Lens' State String
_ex2 = prop (Proxy :: Proxy "ex2")

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
    , blurb:  Expandable.Expanded
    , code:   Expandable.Expanded
    , ex1: ""
    , ex2: ""
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
            [ Utils.tailwindClass "story-panel-about"]
            [ FormField.field_
              { label: HH.text "About"
              , helpText: []
              , error: []
              , inputId: "show-blurb"
              }
              [ Toggle.toggle
                [ HP.id "show-blurb"
                , HP.checked
                  $ Expandable.toBoolean state.blurb
                , HE.onChange \_ -> ToggleCard _blurb
                ]
              ]
            , Expandable.content_ state.blurb $ if state.toggle then blurbtext1 else blurbtext2
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
            , Expandable.content_ state.code
                [ HH.pre 
                  [ HP.class_ $ HH.ClassName "language-purescript" ]  
                  [ HH.code_ [ RH.render_ $ highlightString_ $ if state.toggle then state.ex1 else state.ex2 ] ]
                ]
            ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]
        
handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> do 
    text1 <- H.liftAff $ readSnippetFiles "ThreeLittleCircles1"
    _ex1 %= const text1
    text2 <- H.liftAff $ readSnippetFiles "ThreeLittleCircles2"
    _ex2 %= const text2
    _ <- H.liftEffect $ eval_D3M $ Circles.threeLittleCircles "div.svg-container"
    pure unit

  ToggleExample -> do
    toggle <- H.gets _.toggle
    let toggle' = not toggle
        container = "div.svg-container"
    _ <- H.liftEffect $ eval_D3M $ removeExistingSVG container
    _ <- H.liftEffect $ eval_D3M $ 
          if toggle'
          then Circles.threeLittleCircles container
          else Circles.threeLittleCircles3 [100, 45, 267] container
    modify_ (\s -> s { toggle = toggle' })

  Finalize -> pure unit

blurbtext1 :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext1 = (HH.p [ HP.classes [ HH.ClassName "m-2" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where 
    texts = ["Simplest possible example, just to show syntax." ]

blurbtext2 :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext2 = (HH.p [ HP.classes [ HH.ClassName "m-2" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where 
    texts = ["This extends the super-simple model in the direction one would go for a more real-world example. It's still extremely simple, and the Model, datum_ and so on would not be necessary for such a simple example. Again, we're just showing syntax and shape of the DSL here." ]

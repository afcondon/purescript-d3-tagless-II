module Stories.ThreeLittleCircles where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState)
import D3.Data.Types (D3Selection_)
import D3.Examples.GUP as GUP
import D3.Examples.ThreeLittleCircles as Circles
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.FormField as FormField
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M, runD3M)
import Data.Array (catMaybes, singleton)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Stories.Tailwind.Styles as Tailwind
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)
  
type State = {
    blurb   :: Expandable.Status
  , code    :: Expandable.Status
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize }
  }
  where

  initialState :: State
  initialState = { 
      blurb:  Expandable.Collapsed
    , code:   Expandable.Collapsed
  }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Tailwind.apply "story-container" ]
      [ HH.div
            [ Tailwind.apply "story-panel-controls"] 
            [ ]
      , HH.div
            [ Tailwind.apply "story-panel-about"]
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
            , Expandable.content_ state.blurb blurbtext
            ]  
      , HH.div
            [ Tailwind.apply "story-panel-code"]
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
            , Expandable.content_ state.code [ HH.pre_ [ HH.code_ [ HH.text codetext] ] ]
            ]  
      , HH.div [ Tailwind.apply "svg-container" ] []
      ]
        

runThreeLittleCircles :: forall m. Bind m => MonadEffect m => m Unit
runThreeLittleCircles = do
  log "Three Little Circles example"
  _ <- H.liftEffect $ eval_D3M $ Circles.threeLittleCircles3 [100, 45, 267] "div.svg-container"
  pure unit

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> runThreeLittleCircles

  Finalize -> pure unit

codetext :: String
codetext = 
  """script :: forall m. SelectionM D3Selection_ m => m ((Array Char) -> m D3Selection_)
  script = do 
    let 
      transition :: ChainableS
      transition = transitionWithDuration $ Milliseconds 2000.0
      -- new entries enter at this position, updating entries need to transition to it on each update
      xFromIndex :: Datum_ -> Index_ -> Number
      xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0)

    root        <- attach "div#gup"
    svg         <- appendElement root $ node Svg [ viewBox 0.0 0.0 650.0 650.0 ]
    letterGroup <- appendElement svg  $ node_ Group

    pure $ \letters -> 
      do 
        letterGroup <+> JoinGeneral {
            element   : Text
          , key       : UseDatumAsKey
          , "data"    : letters
          , behaviour : { 
              enter:  [ classed  "enter"
                      , fill     "green"
                      , x        xFromIndex
                      , y        0.0
                      -- , yu (NWU { i: 0, u: Px })
                      , text     (singleton <<< datumIsChar)
                      , fontSize 48.0
                      ]  
                      `andThen` (transition `to` [ y 200.0 ]) 

            , update: [ classed "update"
                      , fill "gray"
                      , y 200.0
                      ] 
                      `andThen` (transition `to` [ x xFromIndex ] ) 

            , exit:   [ classed "exit"
                      , fill "brown"
                      ] 
                      `andThen` (transition `to` [ y 400.0, remove ])
            }
        }"""

blurbtext :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext = (HH.p [ HP.classes [ HH.ClassName "m-2" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where 
    texts = ["Simplest possible example, just to show syntax." ]

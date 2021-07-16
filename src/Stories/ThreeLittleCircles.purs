module Stories.ThreeLittleCircles where

import Prelude

import Control.Monad.State (class MonadState, modify_)
import D3.Data.Types (D3Selection_)
import D3.Examples.ThreeLittleCircles as Circles
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.FormField as FormField
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Array (singleton)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
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
  | ToggleExample 
  
type State = {
    toggle  :: Boolean -- Toggle between ultra simple and merely super-simple examples
  , blurb   :: Expandable.Status
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
      toggle: true
    , blurb:  Expandable.Expanded
    , code:   Expandable.Expanded
  }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Tailwind.apply "story-container" ]
      [ HH.div
            [ Tailwind.apply "story-panel-controls"] 
            [ HH.text $ if state.toggle then "Ex 1" else "Ex 2"
            , Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
              [ Button.buttonVertical
                [ HE.onClick $ const ToggleExample ]
                [ HH.text "Toggle" ]
              ] 
            ]
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
            , Expandable.content_ state.blurb $ if state.toggle then blurbtext1 else blurbtext2
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
            , Expandable.content_ state.code [ HH.pre_ [ HH.code_ [ HH.text $ if state.toggle then codetext1 else codetext2 ] ] ]
            ]  
      , HH.div [ Tailwind.apply "svg-container" ] []
      ]
        
handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> do 
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

codetext1 :: String
codetext1 = 
  """-- | simple utility function used in all three of these examples
xFromIndex :: Datum_ -> Index_ -> Number
xFromIndex _ i = ((indexIsNumber i) * 100.0)

-- | Pretty much the most basic example imaginable, three ints represented by three circles
threeLittleCircles :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m D3Selection_
threeLittleCircles selector = do 

  let circleAttributes = [ fill "green", cx xFromIndex, cy 50.0, radius 20.0 ]

  root        <- attach selector
  svg         <- root D3.+ (node Svg [ viewBox (-100.0) (-100.0) 650.0 650.0, classed "d3svg gup" ])
  circleGroup <- svg  D3.+ (node Group [])
  circles     <- circleGroup <+> Join Circle [32, 57, 293] circleAttributes

  pure circles"""

codetext2 :: String
codetext2 = 
  """-- | finally, using the data (as opposed to merely the index) in the visualization  
type Model = Array Int  -- not necessary in such a simple example, of course

getDatum :: Datum_ -> Int
getDatum = unsafeCoerce

datum_ :: { color :: Datum_ -> String
, radius :: Datum_ -> Number
}
datum_ = {
    radius: (\d -> Math.sqrt $ toNumber $ getDatum d)
  , color: (\d -> d3SchemeCategory10N_ ((toNumber $ getDatum d) / 100.0))
}

threeLittleCircles3 :: forall m. SelectionM D3Selection_ m => Model -> Selector D3Selection_-> m D3Selection_
threeLittleCircles3 circleData selector = do 

  let circleAttributes = [ fill datum_.color, cx xFromIndex, cy 50.0, radius datum_.radius ]

  root        <- attach selector
  svg         <- root D3.+ (node Svg [ viewBox (-100.0) (-100.0) 650.0 650.0, classed "d3svg gup" ])
  circleGroup <- svg  D3.+ (node Group [])

  circles     <- circleGroup <+> Join Circle circleData circleAttributes

  pure circles
"""

blurbtext1 :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext1 = (HH.p [ HP.classes [ HH.ClassName "m-2" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where 
    texts = ["Simplest possible example, just to show syntax." ]

blurbtext2 :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext2 = (HH.p [ HP.classes [ HH.ClassName "m-2" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where 
    texts = ["This extends the super-simple model in the direction one would go for a more real-world example. It's still extremely simple, and the Model, datum_ and so on would not be necessary for such a simple example. Again, we're just showing syntax and shape of the DSL here." ]

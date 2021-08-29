module Stories.GUP where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState)
import D3.Examples.GUP as GUP
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.FormField as FormField
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M, runD3M)
import Data.Array (catMaybes)
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
import Stories.Utilities (blurbParagraphs, syntaxHighlightedCode)
import Stories.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | SetStatus Status
  | ToggleStatus
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)


data Status = Running | Paused
derive instance eqStatus :: Eq Status

instance showStatus :: Show Status where
  show Running = "Running"
  show Paused  = "Paused"
  
type State = { 
    status  :: Status
  , fiber   :: Maybe (Fiber Unit)
  , update  :: Maybe (Array Char -> Aff Unit)
  , blurb   :: Expandable.Status
  , code    :: Expandable.Status
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

component :: forall query output m. MonadAff m => H.Component query Status output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize
    , receive    = Just <<< SetStatus }
  }
  where

  initialState :: State
  initialState = { 
      status: Paused
    , fiber:  Nothing
    , update: Nothing
    , blurb:  Expandable.Expanded
    , code:   Expandable.Collapsed
  }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div
            [ Utils.tailwindClass "story-panel-controls"] 
            [ Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
              [ Button.buttonVertical
                [ HE.onClick $ const ToggleStatus ]
                [ HH.text $ show state.status ]
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
            , Expandable.content_ state.blurb blurbtext
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
            , Expandable.content_ state.code $ syntaxHighlightedCode codetext
            ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]

runGeneralUpdatePattern :: forall m. Bind m => MonadEffect m => m (Array Char -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  -- detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"
  update   <- H.liftEffect $ eval_D3M $ GUP.script3 "div.svg-container"
  -- the script sets up the SVG and returns a function that the component can run whenever it likes
  -- (but NB if it runs more often than every 2000 milliseconds there will be big problems due to uncompleted transitions)
  pure (\letters -> H.liftEffect $ runD3M (update letters) *> pure unit )

runUpdate :: (Array Char -> Aff Unit) -> Aff Unit
runUpdate update = do
  letters <- H.liftEffect $ getLetters
  update letters
  delay (Milliseconds 2300.0)
  where
    -- | choose a string of random letters (no duplicates), ordered alphabetically
    getLetters :: Effect (Array Char)
    getLetters = do
      let 
        letters = toCharArray "abcdefghijklmnopqrstuvwxyz"
        coinToss :: Char -> Effect (Maybe Char)
        coinToss c = do
          n <- random
          pure $ if n > 0.6 then Just c else Nothing
      
      choices <- sequence $ coinToss <$> letters
      pure $ catMaybes choices


handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> do
    updateFn <- runGeneralUpdatePattern

    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn

    H.modify_ (\state -> state { status = Running, fiber = Just fiber, update = Just updateFn })

  SetStatus status -> H.modify_ (\state -> state { status = status })
  ToggleStatus -> do
    currentStatus <- H.gets _.status
    case currentStatus of
      Running -> pauseUpdating
      _ -> startUpdating

  Finalize -> do
    maybeFiber <- H.gets _.fiber
    _ <- case maybeFiber of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
    -- is it necessary to remove the component from the DOM? don't think it is
    H.modify_ (\state -> state { status = Paused, fiber = Nothing, update = Nothing })


pauseUpdating :: forall m.
  Bind m => 
  MonadState State m =>
  MonadAff m => 
  m Unit
pauseUpdating = do
  maybeFiber <- H.gets _.fiber
  _ <- case maybeFiber of
          Nothing      -> pure unit
          (Just fiber) -> H.liftAff $ killFiber (error "Cancel fiber to suspend computation") fiber
  H.modify_ (\state -> state { status = Paused, fiber = Nothing })

startUpdating :: forall m.
  Bind m => 
  MonadState State m =>
  MonadAff m => 
  m Unit
startUpdating = do
  { status, update } <- H.get
  if status /= Paused
  then pure unit
  else 
    case update of
      Nothing -> pure unit
      (Just updateFn) -> do
        fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
        H.modify_ (\state -> state { status = Running, fiber = Just fiber })


codetext :: String
codetext =
  """type Model = Array Char

script3 :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m ((Array Char) -> m D3Selection_)
script3 selector = do 
  root        <- attach selector
  svg         <- root D3.+ (node Svg [ viewBox 0.0 0.0 650.0 650.0, classed "d3svg gup" ])
  letterGroup <- svg  D3.+ (node Group [])

  pure $ \letters -> letterGroup <+> SplitJoinClose Text letters { enter, update, exit }

  where 
    transition :: ChainableS
    transition = transitionWithDuration $ Milliseconds 2000.0

    -- letters enter at this position, and then must transition to new position on each update
    xFromIndex :: Datum_ -> Index_ -> Number
    xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0) 

    enter = [ classed  "enter"
            , fill     "green"
            , x        xFromIndex
            , y        0.0
            -- , yu (NWU { i: 0, u: Px })
            , text     (singleton <<< datumIsChar)
            , fontSize 96.0 ]  
          `andThen` (transition `to` [ y 200.0 ]) 

    update =  [ classed "update", fill "gray", y 200.0 ] 
              `andThen` (transition `to` [ x xFromIndex ] ) 

    exit =  [ classed "exit", fill "brown"] 
            `andThen` (transition `to` [ y 400.0, remove ])
"""

blurbtext :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext =  blurbParagraphs [
  """This deceptively simple example shows off an aspect of screen-based data
visualization that has no analogue in paper visualizations: the ability to
specify how updates to the data should be represented.""",

"""In this example, some letters of the alphabet are presented and then constantly
updated. When a letter enters at first, it falls in from the top and it is
green. If its still present in the next set of letters it stays on the screen,
but it turns gray and moves to an alphabetically correct new position. And if
its not present in the new data, it turns red and falls out before
disappearing.""",

"""In a more meaningful example, ie with some data that you actually care about,
this helps give continuity, as the eye can track an individual letter thru its
arrival, update and exit phases. Even if this tracking isn't interesting in
itself, it can lessen the fatigue of looking at updated data and it conveys a
sense of how much the data has changed.""",

"""This example is called "General Update Pattern" in D3.js, hence the name of
this example. You can see in the code panel how the "data join" contains three
separate specifications, each with their own *transition*."""

]

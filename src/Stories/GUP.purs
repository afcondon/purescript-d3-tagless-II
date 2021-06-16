module Stories.GUP where

import Prelude hiding (append)

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState)
import D3.Attributes.Sugar (classed, viewBox)
import D3.Data.Types (D3Selection_, Element(..))
import D3.Examples.GUP as GUP
import D3.FFI (d3RemoveSelection_, d3SelectionIsEmpty_, d3SelectionSelect_)
import D3.Interpreter (class D3InterpreterM, append, attach)
import D3.Interpreter.D3 (d3Run, removeExistingSVG, runD3M)
import D3.Selection (node)
import D3Tagless.Block.Card as Card
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Block.Expandable as Expandable
import Data.Array (catMaybes)
import Data.Const (Const)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
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
import Ocelot.Block.Button as Button
import Ocelot.Block.Format as Format
import Ocelot.Block.FormField as FormField
import Ocelot.HTML.Properties (css)
import Stories.Tailwind.Styles as Tailwind
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import Type.Proxy (Proxy(..))

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | RestartGUP
  | PauseGUP
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)

data Status = NotInitialized | Running | Paused
derive instance eqStatus :: Eq Status

instance showStatus :: Show Status where
  show NotInitialized = "Not yet initialized"
  show Running = "GUP is running"
  show Paused  = "GUP is paused"
  
type State = { 
    value  :: Status
  , fiber  :: Maybe (Fiber Unit)
  , update :: Maybe (Array Char -> Aff Unit)
  , blurb :: Expandable.Status
  , code  :: Expandable.Status
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

component :: forall m. MonadAff m => H.Component Query Unit Void m
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
      value: NotInitialized
    , fiber: Nothing
    , update: Nothing
    , blurb: Expandable.Expanded
    , code: Expandable.Collapsed
  }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Tailwind.apply "storygrid" ]
      [ Card.card_
          [ Format.contentHeading_ [ HH.text "General Update Pattern" ]
          , Card.card_ 
              [ HH.text blurbtext 
              , Card.card_ 
                  [ Format.caption_
                    [ HH.text $ show state.value ]
                  , Button.buttonGroup_
                    [ Button.buttonLeft
                      [ HE.onClick $ const PauseGUP ]
                      [ HH.text "Pause" ]
                    , Button.buttonRight
                      [ HE.onClick $ const RestartGUP ]
                      [ HH.text "Restart" ]
                    ]
                  ]
              ]
          ]
      , Card.card_
          [ Format.subHeading_ 
            [ HH.text "Code" ]
          , FormField.field_
            { label: HH.text "Show code panel"
            , helpText: []
            , error: []
            , inputId: "show-code"
            }
            [ Toggle.toggle
              [ HP.id_ "show-code"
              , HP.checked
                $ Expandable.toBoolean state.code
              , HE.onChange \_ -> ToggleCard _code
              ]
            ]
          , Expandable.content_ state.code [ HH.pre_ [ HH.code_ [ HH.text codetext] ] ]
          ]  
      ]

runGeneralUpdatePattern :: forall m. Bind m => MonadEffect m => m (Array Char -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  detached <- H.liftEffect $ d3Run $ removeExistingSVG "div.d3story"
  update   <- H.liftEffect $ d3Run $ GUP.script "div.d3story"
  -- the script sets up the SVG and returns a function that the component can run whenever it likes
  -- (but NB if it runs more often than every 2000 milliseconds there will be big problems)
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

    H.modify_ (\state -> state { value = Running, fiber = Just fiber, update = Just updateFn })

  PauseGUP -> do
    fiber <- H.gets _.fiber
    _ <- case fiber of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Cancel fiber to suspend computation") fiber
    H.modify_ (\state -> state { value = Paused, fiber = Nothing })

  RestartGUP -> do
    { value, update } <- H.get
    if value /= Paused
    then pure unit
    else 
      case update of
        Nothing -> pure unit
        (Just updateFn) -> do
          fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
          H.modify_ (\state -> state { value = Running, fiber = Just fiber })

  Finalize -> do
    fiber <- H.gets _.fiber
    _ <- case fiber of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
    -- is it necessary to remove the component from the DOM? don't think it is
    H.modify_ (\state -> state { value = NotInitialized, fiber = Nothing, update = Nothing })



codetext :: String
codetext = 
  """script :: forall m. D3InterpreterM D3Selection_ m => m ((Array Char) -> m D3Selection_)
  script = do 
    let 
      transition :: ChainableS
      transition = transitionWithDuration $ Milliseconds 2000.0
      -- new entries enter at this position, updating entries need to transition to it on each update
      xFromIndex :: Datum_ -> Index_ -> Number
      xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0)

    root        <- attach "div#gup"
    svg         <- append root $ node Svg [ viewBox 0.0 0.0 650.0 650.0 ]
    letterGroup <- append svg  $ node_ Group

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

blurbtext :: String
blurbtext = 
  """Id sint laboris reprehenderit officia anim nisi consectetur voluptate enim.
  Commodo cillum minim nisi laborum eiusmod veniam ullamco id ex fugiat eu anim.
  Irure est aute laborum duis. Lorem dolore id sunt incididunt ut ea. Nostrud
  enim officia nisi anim consequat cupidatat consectetur consequat ex excepteur.
  Lorem nisi in reprehenderit ex adipisicing magna elit aute sunt. Cillum non
  Lorem minim duis culpa ullamco aute ex minim. Mollit anim in nisi tempor enim
  exercitation dolore. Veniam consequat minim nostrud amet duis dolore tempor
  voluptate quis culpa. Laborum dolor pariatur ut est cupidatat elit deserunt
  occaecat tempor aliquip anim. 
  
  Velit irure ea voluptate ipsum ex exercitation
  dolore voluptate reprehenderit sit anim sunt. Anim fugiat ad ut qui cillum
  tempor occaecat et deserunt nostrud non ipsum. Id non qui mollit culpa elit
  cillum ipsum excepteur adipisicing qui. Incididunt adipisicing sit incididunt
  consequat minim id do exercitation cupidatat est sunt mollit. Anim ut ullamco
  enim culpa. Adipisicing ad non esse laboris anim consequat ut velit esse
  consequat tempor. Commodo magna esse ullamco ipsum et ipsum minim dolore esse
  veniam ea commodo labore. Nulla deserunt id ad anim anim proident labore
  occaecat sint esse nostrud. Duis velit nostrud ullamco cillum cillum Lorem
  cupidatat irure."""

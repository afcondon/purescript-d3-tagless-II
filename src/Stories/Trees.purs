module Stories.Trees where

import Prelude

import Affjax (Error)
import Control.Monad.State (class MonadState, get, put)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Examples.Tree.Configure as Tree
import D3.Interpreter.D3 (d3Run, removeExistingSVG)
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import Data.Array (catMaybes)
import Data.Const (Const)
import Data.Either (Either(..)) as E
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Backdrop as Backdrop
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Radio as Radio
import Ocelot.Documentation as Documentation
import Ocelot.HTML.Properties (css)

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | SetLayout TreeLayout
  | SetType TreeType
  
type State = Maybe TreeModel

component :: forall m. MonadAff m => H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize }
  }
  where

  initialState :: State
  initialState = Nothing

  showState :: Maybe TreeModel -> String
  showState Nothing = "There is no model yet"
  showState (Just model) = show model.treeType <> " " <> show model.treeLayout

  treeClasses :: State -> Array HH.ClassName
  treeClasses model = do
    let classStrings = catMaybes [ Just "trees", (show <<< _.treeLayout) <$> model]
    HH.ClassName <$> classStrings
  
  controlsRadio model =
    Card.card
      [ css "flex-1" ]
      [ HH.div
          [ css "flex-1" ]
          [ HH.h3
            [ HP.classes Format.captionClasses ]
            [ HH.text "Layout controls" ]
          , FormField.fieldset_
            { label: HH.text "Tree orientation"
            , inputId: "radio-horizontal"
            , helpText: []
            , error: []
            }
            [ HH.div
              [ css "flex" ]
              [ Radio.radio
                [ css "pr-6" ]
                [ HP.name "preview"
                , HP.checked true
                , HE.onClick $ const (SetLayout Vertical)
                ]
                [ HH.text "Vertical" ]
              , Radio.radio
                [ css "pr-6" ]
                [ HP.name "preview"
                , HE.onClick $ const (SetLayout Horizontal) ]
                [ HH.text "Horizontal" ]
              , Radio.radio
                [ css "pr-6" ]
                [ HP.name "preview"
                , HE.onClick $ const (SetLayout Radial) ]
                [ HH.text "Radial" ]
              ]
            ]
          , FormField.fieldset_
            { label: HH.text "Tree topology"
            , inputId: "radio-horizontal"
            , helpText: []
            , error: []
            }
            [ HH.div
              [ css "flex" ]
              [ Radio.radio
                [ css "pr-6" ]
                [ HP.name "preview"
                , HP.checked true
                , HE.onClick $ const (SetType TidyTree)
                ]
                [ HH.text "TidyTree" ]
              , Radio.radio
                [ css "pr-6" ]
                [ HP.name "preview"
                , HE.onClick $ const (SetType Dendrogram) ]
                [ HH.text "Dendrogram" ]
              ]
            ]
          ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =  
      HH.div [ HP.id "d3story-overlay", HP.classes $ treeClasses state ]
      [ HH.div [ HP.id "tree" ] [] -- the div where the d3 script will appear

      , HH.div [ HP.id "blurb" ] 
        [ HH.div [ HP.id "inner-blurb" ] [ HH.h1_ [ HH.text $ "Tree layout" ]
                                         , HH.text blurbtext ] 
        , controlsRadio state
        ]

      , HH.div [ HP.id "code" ] [ HH.div [ HP.id "inner-code" ] [ HH.text codetext]]
      ]

selector = "div#d3story" -- TODO redo how all this svg nonsense is handled

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction Initialize = do
  detached <- H.liftEffect $ d3Run $ removeExistingSVG selector

  treeJSON <- H.liftAff $ getTreeViaAJAX "http://localhost:1234/flare-2.json"

  case treeJSON of
    (E.Left err) -> pure unit
    (E.Right (tree :: TreeJson_)) -> do
      model <- H.liftAff $ makeModel TidyTree Vertical tree
      _     <- H.liftAff $ Tree.drawTree model selector
      H.modify_ (\_ -> Just model)
      pure unit
  pure unit

handleAction (SetLayout layout) = do
  detached <- H.liftEffect $ d3Run $ removeExistingSVG selector

  (model :: Maybe TreeModel) <- get
  case model of
    Nothing -> pure unit
    (Just model) -> do
      let updated = model { treeLayout = layout }
      _ <- H.liftAff $ Tree.drawTree updated selector
      put $ Just updated

handleAction (SetType  treetype) = do
  detached <- H.liftEffect $ d3Run $ removeExistingSVG selector

  (model :: Maybe TreeModel) <- get
  case model of
    Nothing -> pure unit
    (Just model) -> do
      let updated = model { treeType = treetype }
      _ <- H.liftAff $ Tree.drawTree updated selector
      put $ Just updated


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

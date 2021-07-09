module Stories.Trees where

import Prelude

import Control.Monad.State (class MonadState, get)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Examples.Tree.Configure as Tree
import D3Tagless.Utility (removeExistingSVG)
import D3Tagless.Instance.Selection (eval_D3M)
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Block.Expandable as Expandable
import Data.Array (catMaybes)
import Data.Const (Const)
import Data.Either (Either(..)) as E
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Radio as Radio
import Ocelot.HTML.Properties (css)
import Stories.Tailwind.Styles as Tailwind
import Type.Proxy (Proxy(..))
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | SetLayout TreeLayout
  | SetType TreeType
  | ToggleCard (Lens' State Expandable.Status)

type State = { 
    tree :: Maybe TreeModel
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
    , initialize = Just Initialize }
  }
  where

  initialState :: State
  initialState = { 
      tree: Nothing
    , blurb: Expandable.Collapsed
    , code: Expandable.Collapsed
  }

  showState :: Maybe TreeModel -> String
  showState Nothing = "There is no model yet"
  showState (Just model) = show model.treeType <> " " <> show model.treeLayout

  treeClasses :: State -> Array HH.ClassName
  treeClasses model = do
    let classStrings = catMaybes [ Just "trees", (show <<< _.treeLayout) <$> model.tree]
    HH.ClassName <$> classStrings
  
  controlsRadio =
    HH.div
      [ css "flex-1" ]
      [ FormField.fieldset_
        { label: HH.text "Tree orientation"
        , inputId: "radio-vertical"
        , helpText: []
        , error: []
        }
        [ HH.div
          [ css "flex-1" ]
          [ Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-layout"
            , HP.checked true
            , HE.onClick $ const (SetLayout Vertical)
            ]
            [ HH.text "Vertical" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-layout"
            , HE.onClick $ const (SetLayout Horizontal) ]
            [ HH.text "Horizontal" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-layout"
            , HE.onClick $ const (SetLayout Radial) ]
            [ HH.text "Radial" ]
          ]
        ]
      , FormField.fieldset_
        { label: HH.text "Tree topology"
        , inputId: "radio-vertical"
        , helpText: []
        , error: []
        }
        [ HH.div
          [ css "flex-1" ]
          [ Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-type"
            , HP.checked true
            , HE.onClick $ const (SetType TidyTree)
            ]
            [ HH.text "TidyTree" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-type"
            , HE.onClick $ const (SetType Dendrogram) ]
            [ HH.text "Dendrogram" ]
          ]
        ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Tailwind.apply "story-container" ]
      [ HH.div [ Tailwind.apply "story-panel-controls"] 
          [ controlsRadio ]
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
            , Expandable.content_ state.blurb [ HH.text blurbtext ]
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

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    treeJSON <- H.liftAff $ getTreeViaAJAX "http://localhost:1234/flare-2.json"

    case treeJSON of
      (E.Left err) -> pure unit
      (E.Right (treeJSON :: TreeJson_)) -> do
        model <- H.liftAff $ makeModel TidyTree Vertical treeJSON
        _     <- H.liftAff $ Tree.drawTree model "div.svg-container"
        H.modify_ (\st -> st { tree = Just model } )
        pure unit
    pure unit

  (SetLayout layout) -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    { tree } <- get
    case tree of
      Nothing -> pure unit
      (Just tree) -> do
        let updated = tree { treeLayout = layout }
        _ <- H.liftAff $ Tree.drawTree updated "div.svg-container"
        H.modify_ (\st -> st { tree = Just updated } )
        pure unit

  (SetType  treetype) -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    { tree } <- get
    case tree of
      Nothing -> pure unit
      (Just tree) -> do
        let updated = tree { treeType = treetype }
        _ <- H.liftAff $ Tree.drawTree updated "div.svg-container"
        H.modify_ (\st -> st { tree = Just updated } )
        pure unit


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

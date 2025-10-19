module PSD3.Pages.PrintTree where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeType(..))
import D3.Examples.Tree.Configure as Tree
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import PSD3.Component.Expandable as Expandable
import PSD3.Component.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Either (Either(..)) as E
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Component.FormField as FormField
import Snippets (readSnippetFiles)
import PSD3.Pages.Utilities (syntaxHighlightedCode)
import PSD3.Pages.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | ToggleCard (Lens' State Expandable.Status)
  
type State = { 
    tree     :: String
  , panels   :: { blurb :: Expandable.Status, code :: Expandable.Status, print :: Expandable.Status }
  , snippets :: { draw :: String, handler :: String }
}

_snippets = prop (Proxy :: Proxy "snippets")
_panels = prop (Proxy :: Proxy "panels")

_drawCode :: Lens' State String
_drawCode = _snippets <<< prop (Proxy :: Proxy "draw")
_handlerCode :: Lens' State String
_handlerCode = _snippets <<< prop (Proxy :: Proxy "handler")
_blurb :: Lens' State Expandable.Status
_blurb = _panels <<< prop (Proxy :: Proxy "blurb")
_code :: Lens' State Expandable.Status
_code = _panels <<< prop (Proxy :: Proxy "code")
_print :: Lens' State Expandable.Status
_print = _panels <<< prop (Proxy :: Proxy "print")
  

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize }
  }
  where

  initialState :: State
  initialState = { tree: "" 
                 , panels: { blurb: Expandable.Collapsed, code: Expandable.Collapsed, print: Expandable.Expanded }
                 , snippets: { draw: "", handler: "" }
                 } 
  
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div 
        [ Utils.tailwindClass "story-panel-controls"] 
        [ HH.text "Les Mis" ]
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
                  $ Expandable.toBoolean (view _blurb state)
                , HE.onChange \_ -> ToggleCard _blurb
                ]
              ]
            , Expandable.content_ (view _blurb state) [ HH.text blurbtext ]
            ]  
      , HH.div
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
                  $ Expandable.toBoolean (view _code state)
                , HE.onChange \_ -> ToggleCard _code
                ]
              ]
            , Expandable.content_ (view _code state) $ syntaxHighlightedCode (view _handlerCode state)
            ]  
      , HH.div
            [ Utils.tailwindClass "story-panel-code"]
            [ FormField.field_
                { label: HH.text "Output"
                , helpText: []
                , error: []
                , inputId: "show-print"
                }
              [ Toggle.toggle
                [ HP.id "show-print"
                , HP.checked
                  $ Expandable.toBoolean (view _print state)
                , HE.onChange \_ -> ToggleCard _print
                ]
              ]
            , Expandable.content_ (view _print state) [ HH.code_ [ HH.text state.tree ] ]
            ]  
      ]

-- Snippet_Start
-- Name: PrintTreeHandleActions
handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    text <- H.liftAff $ readSnippetFiles "PrintTreeHandleActions"
    _handlerCode .= text

    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"

    case treeJSON of
      (E.Left err) -> pure unit
      (E.Right (tree :: TreeJson_)) -> do
        textRep     <- H.liftAff $ Tree.getPrintTree =<< makeModel TidyTree Radial tree
        H.modify_ (\st -> st { tree = textRep } )
        pure unit
    pure unit
-- Snippet_End

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

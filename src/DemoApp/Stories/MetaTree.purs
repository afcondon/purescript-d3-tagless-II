module Stories.MetaTree where

import Data.Lens
import Prelude
import Data.Lens

import Control.Monad.State (class MonadState)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Examples.MetaTree as MetaTree
import D3.Examples.Tree.Configure as Tree
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Either (Either(..)) as E
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DemoApp.UI.FormField as FormField
import Snippets (readSnippetFiles)
import Stories.Utilities (syntaxHighlightedCode)
import Stories.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | ToggleCard (Lens' State Expandable.Status)

type State = { 
    tree :: Maybe TreeModel
  , panels  :: { blurb :: Expandable.Status, code :: Expandable.Status }
  , snippets :: { draw :: String, evaluator :: String, handler :: String }
}

_panels = prop (Proxy :: Proxy "panels")
_snippets = prop (Proxy :: Proxy "snippets")

_drawCode :: Lens' State String
_drawCode = _snippets <<< prop (Proxy :: Proxy "draw")
_handlerCode :: Lens' State String
_handlerCode = _snippets <<< prop (Proxy :: Proxy "handler")
_evaluatorCode :: Lens' State String
_evaluatorCode = _snippets <<< prop (Proxy :: Proxy "evaluator")
_blurb :: Lens' State Expandable.Status
_blurb = _panels <<< prop (Proxy :: Proxy "blurb")
_code :: Lens' State Expandable.Status
_code = _panels <<< prop (Proxy :: Proxy "code")

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
  initialState = { tree: Nothing
                 , panels: { blurb:  Expandable.Expanded, code:   Expandable.Collapsed }
                 , snippets: { draw: "", evaluator: "", handler: "" }
                 }
  
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div
            [ Utils.tailwindClass "story-panel-about"]
            [ HH.h1 [ HP.classes [ HH.ClassName "text-3xl", HH.ClassName "p-2" ] ] 
                    [ HH.text "Meta and Printer Interpreters" ]
            , HH.div_ blurbtext
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
            , Expandable.content_ (view _code state) $ 
                 syntaxHighlightedCode (view _evaluatorCode state)  <>
                 syntaxHighlightedCode (view _drawCode state)  <>
                 syntaxHighlightedCode (view _handlerCode state) 
            ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]

-- Snippet_Start 
-- Name: MetaTreeEvaluator      
-- | evaluate a tree first using the "metatree" interpreter, then draw the RESULTING (syntax) tree using D3 interpreter
drawMetaTree :: TreeJson_ -> Aff Unit
drawMetaTree json =
  MetaTree.drawTree =<< makeModel TidyTree Vertical =<< Tree.getMetaTreeJSON =<< makeModel TidyTree Radial json
-- Snippet_End

-- Snippet_Start
-- Name: MetaTreeHandleActions
handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    text <- H.liftAff $ readSnippetFiles "MetaTreeDraw"
    _drawCode .= text
    text <- H.liftAff $ readSnippetFiles "MetaTreeEvaluator"
    _evaluatorCode .= text
    text <- H.liftAff $ readSnippetFiles "MetaTreeHandleActions"
    _handlerCode .= text
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.d3story"

    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"

    case treeJSON of
      (E.Left err) -> pure unit
      (E.Right (tree :: TreeJson_)) -> do
        _     <- H.liftAff $ drawMetaTree tree
        pure unit
    pure unit
-- Snippet_End

blurbtext = [
    HH.p [ HP.classes [ HH.ClassName "p-2" ]] [ HH.text 
          
          """The way this library works is by creating an embedded DSL in PureScript
          which can be interpreted to cause a visualization to come into
          existence...typically in your browser, as an SVG.
          """

  ]
  , HH.p [ HP.classes [ HH.ClassName "p-2" ]] 
         [ HH.text 
          """
          The primary interpreter that is provided, the one that powers all of the
          other demos here except these two, turns the statements of this eDSL into D3
          actions.
          """
        ]

  , HH.p [ HP.classes [ HH.ClassName "p-2" ]] 
         [ HH.text 
          """
          However, other interpreters are possible. This page shows two of them, both
          quite rudimentary but showing some powerful ideas which could be taken a lot
          further.
          """
  ]

  , HH.h2 [ HP.classes [ HH.ClassName "text-2xl", HH.ClassName "p-2" ]] [ HH.text "MetaTree"]
  , HH.p [ HP.classes [ HH.ClassName "p-2" ]] 
         [ HH.text 
          """
          The first one, called here "MetaTree" turns a "script" written in the DSL
          into a syntax tree and then renders the resulting tree using the other,
          D3-based, interpreter. The result is a kind of x-ray of the script, one which
          visually describes the structure you are producing. Because interaction is easy
          to add to DOM-based visualizations such as D3 this could also be a basis for
          a point-and-click manner for writing visualizations, or perhaps for editing and
          adapting them. 
          """
  ]

  , HH.h2 [ HP.classes [ HH.ClassName "text-2xl", HH.ClassName "p-2" ]] [ HH.text "Printer"]
  , HH.p [ HP.classes [ HH.ClassName "p-2" ]] 
         [ HH.text 
          """
          The second example shows that the "script" can be interpreted into a textual
          form. This could be the basis for documentation or even transpilation. In
          principle, it is possible to emit the JavaScript / D3 version of the script
          via this mechanism, but the current implementation is only a proof-of-concept
          and is not elaborated to that extent.
          
          """
  ]

]

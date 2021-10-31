module Snippets where

import Prelude

import Affjax (printError)
import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Class as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as RH
import Stories.Utilities (highlightString_)

data Cell state w i = 
    Blurb String
  | SnippetFile String
  | Snippet String
  | PreRendered (HH.HTML w i)
  | RenderWithState (state -> HH.HTML w i)

type Notebook state w i = Array (Cell state w i)

renderNotebook :: forall state w i. state -> Notebook state w i -> Array (HH.HTML w i)
renderNotebook state notebook = (renderCell state) <$> notebook

renderCell :: forall state w i. state -> Cell state w i -> HH.HTML w i
renderCell _ (Blurb b) = 
  HH.p [ HP.classes [ HH.ClassName "m-2" ] ]
                    [ HH.text b ]
renderCell _ (Snippet s) = 
  HH.pre [ HP.class_ $ HH.ClassName "language-purescript" ]  
         [ HH.code_ [ RH.render_ $ highlightString_ s ] ]

renderCell _ (SnippetFile filename) =
  HH.p [ HP.classes [ HH.ClassName "m-2" ] ]
                    [ HH.text $ "Snippet file not loaded: " <> filename ]

renderCell _ (PreRendered html) = html

renderCell state (RenderWithState fn) = fn state

substituteSnippetCells :: forall w i m state state'. Bind m => MonadAff m => MonadState state m => 
  Cell state' w i -> m (Cell state' w i)
substituteSnippetCells (SnippetFile snippet) = do
    snippetText <- H.liftAff $ readSnippetFiles snippet
    pure $ Snippet snippetText
substituteSnippetCells cell = pure cell -- no change to other cells

readSnippetFiles :: String -> Aff String
readSnippetFiles name = do
  response <- AJAX.get ResponseFormat.string $ "./code-examples/" <> name
  case response of
    (Left err) -> spy "couldn't read snippet, error: " $ pure (printError err)
    (Right r) -> spy "read snippet: " $ pure r.body

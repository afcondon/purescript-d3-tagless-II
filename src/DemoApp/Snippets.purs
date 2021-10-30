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

data Cell = Blurb String | SnippetFile String | Snippet String | PreRendered (forall w i. HH.HTML w i)
type Notebook = Array Cell

renderNotebook :: forall w i. Notebook -> Array (HH.HTML w i)
renderNotebook notebook = renderCell <$> notebook

renderCell :: forall w i. Cell -> HH.HTML w i
renderCell (Blurb b) = 
  HH.p [ HP.classes [ HH.ClassName "m-2" ] ]
                    [ HH.text b ]
renderCell (Snippet s) = 
  HH.pre [ HP.class_ $ HH.ClassName "language-purescript" ]  
         [ HH.code_ [ RH.render_ $ highlightString_ s ] ]

renderCell (SnippetFile filename) =
  HH.p [ HP.classes [ HH.ClassName "m-2" ] ]
                    [ HH.text $ "Snippet file not loaded: " <> filename ]

renderCell (PreRendered html) = html

substituteSnippetCells :: forall m state. Bind m => MonadAff m => MonadState state m => 
  Cell -> m Cell
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

module PSD3.CodeExplorer.CodeExplorationPage where

-- | Stub module for archived CodeExplorationPage component
-- | This allows Main.purs to compile while CodeExplorer is archived

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))

component :: forall q o m. H.Component q String o m
component =
  H.mkComponent
    { initialState: \snippetId -> snippetId
    , render: \snippetId -> HH.div_ [ HH.text $ "Code Exploration (archived): " <> snippetId ]
    , eval: H.mkEval H.defaultEval
    }

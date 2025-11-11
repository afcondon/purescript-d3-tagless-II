module PSD3.CodeExplorer.CodeExplorerWrapper where

-- | Stub module for archived CodeExplorer component
-- | This allows Main.purs to compile while CodeExplorer is archived

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> unit
    , render: \_ -> HH.div_ [ HH.text "CodeExplorer is archived" ]
    , eval: H.mkEval H.defaultEval
    }

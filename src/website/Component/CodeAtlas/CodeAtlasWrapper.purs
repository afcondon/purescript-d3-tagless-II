module PSD3.CodeAtlas.CodeAtlasWrapper where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import PSD3.CodeAtlas.CodeAtlas as CodeAtlas

-- | Wrapper component for Code Atlas (similar to CodeExplorerWrapper, WealthHealthWrapper)
component :: forall q i o. H.Component q i o Aff
component = CodeAtlas.component

module D3.Examples.Spago.Files where

import Affjax (URL)

-- TODO This is all specific to the Spago output, so break out into file handling modules later
type SpagoModuleJSON  = { key :: String, depends :: Array String, path :: String }
type SpagoPackageJSON = { key :: String, depends :: Array String }
type SpagoLsDepJSON   = { packageName :: String, version :: String, repo :: { tag :: String, contents :: URL } }
type SpagoLOCJSON     = { loc :: Number, path :: String }

-- TODO no error handling at all here RN (OTOH - performant!!)
type Spago_Raw_JSON_ = { 
    packages        :: Array SpagoPackageJSON
  , modules         :: Array SpagoModuleJSON
  , lsDeps          :: Array SpagoLsDepJSON
  , loc             :: Array SpagoLOCJSON } 

-- TODO use a generic, per-file read for this, and lose the FFI file here
foreign import readSpago_Raw_JSON_ :: String -> String -> String -> String -> Spago_Raw_JSON_

module D3.Examples.LesMiserables.File where

import Affjax (Error)
import D3.Examples.LesMiserables.Model (LesMisRawModel)
import Data.Either (Either(..))

-- TODO no error handling at all here RN (OTOH - performant!!)
foreign import readJSONJS_ :: String -> LesMisRawModel 

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> LesMisRawModel
readGraphFromFileContents (Right { body } ) = readJSONJS_ body
-- TODO exceptions dodged using empty Model, fix with Maybe
readGraphFromFileContents (Left err)        = { links: [], nodes: [] } 

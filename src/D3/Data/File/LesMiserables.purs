module D3.Data.File.LesMiserables where

import Affjax (Error)
import D3.Data.Foreign (Datum_)
import D3.Node (D3_Simulation_Link, D3_Simulation_LinkID, D3_Simulation_Node)
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)

type LesMisNodeData = { group :: Number }
type LesMisLinkData = ( value :: Number )
type LesMisModel    = { links :: Array (D3_Simulation_LinkID LesMisLinkData)
                      , nodes :: Array LesMisNodeData }

datumIsLesMisGraphLink_ :: Datum_ -> D3_Simulation_Link LesMisNodeData LesMisLinkData
datumIsLesMisGraphLink_ = unsafeCoerce
datumIsLesMisGraphNode_ :: Datum_ -> D3_Simulation_Node LesMisNodeData
datumIsLesMisGraphNode_ = unsafeCoerce

-- TODO no error handling at all here RN (OTOH - performant!!)
foreign import readJSONJS :: String -> LesMisModel 

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> LesMisModel
readGraphFromFileContents (Right { body } ) = readJSONJS body
-- TODO exceptions dodged using empty Model, fix with Maybe
readGraphFromFileContents (Left err)        = { links: [], nodes: [] } 

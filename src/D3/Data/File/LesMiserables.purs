module D3.Data.File.LesMiserables where

import Affjax (Error)
import D3.Data.Foreign (Datum_)
import D3.FFI (D3ForceLink_, D3ForceNode_)
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)

type NodeExtension = (group :: Number) -- any extra fields beyond what's required of all ForceLayout nodes
type LinkExtension = (value :: Number) -- empty row, this simulation doesn't yet have extra stuff in the links
type LesMisGraphNode_ = D3ForceNode_ Int NodeExtension
type LesMisGraphLink_ = D3ForceLink_ Int NodeExtension LinkExtension

type LesMisModel = { links :: Array LesMisGraphLink_, nodes :: Array LesMisGraphNode_ }

datumIsLesMisGraphLink_ :: Datum_ -> LesMisGraphLink_
datumIsLesMisGraphLink_ = unsafeCoerce
datumIsLesMisGraphNode_ :: Datum_ -> LesMisGraphNode_
datumIsLesMisGraphNode_ = unsafeCoerce

-- TODO no error handling at all here RN (OTOH - performant!!)
foreign import readJSONJS :: String -> LesMisModel 

readGraphFromFileContents :: forall r. Either Error { body ∷ String | r } -> LesMisModel
readGraphFromFileContents (Right { body } ) = readJSONJS body
readGraphFromFileContents (Left err)        = { links: [], nodes: [] } -- TODO exceptions dodged using empty Model

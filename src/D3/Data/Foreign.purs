module D3.Data.Foreign where

foreign import data Datum_ :: Type
foreign import data Index_ :: Type

foreign import data D3Data_       :: Type 
foreign import data D3Selection_  :: Type
foreign import data D3Simulation_ :: Type -- has to be declared here to avoid cycle with Simulation.purs
foreign import data D3Transition_ :: Type -- not clear yet if we need to distinguish from Selection
foreign import data D3This_       :: Type -- not yet used but may be needed, ex. in callbacks
foreign import data D3DomNode_    :: Type

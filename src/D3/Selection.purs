module D3.Selection where

import Prelude hiding (append,join)

import D3.Attributes.Instances (Attribute, Attributes, Datum)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Unsafe.Coerce (unsafeCoerce)


type Selector = String 

data Element = Div | Svg | Circle | Line | Group
instance showElement :: Show Element where
  show Div    = "div"
  show Svg    = "svg"
  show Circle = "circle"
  show Line   = "line"
  show Group  = "group"
  
-- || trying this with Finally Tagless instead of interpreter
foreign import data D3Selection  :: Type
foreign import data D3DomNode    :: Type
foreign import data D3This       :: Type
type D3Group = Array D3DomNode
foreign import data D3Transition :: Type -- not clear yet if we need to distinguish from Selection
-- we'll coerce everything to this type if we can validate attr lambdas against provided data
foreign import data D3Data :: Type 
-- ... and we'll also just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr :: Type 

foreign import d3SelectAllInDOM_     :: Selector -> D3Selection -> D3Selection -- NB passed D3Selection is IGNORED
foreign import d3SelectionSelectAll_ :: Selector -> D3Selection -> D3Selection
foreign import d3EnterAndAppend_     :: String   -> D3Selection -> D3Selection
foreign import d3Append_             :: String   -> D3Selection -> D3Selection
foreign import d3Exit_               :: D3Selection -> D3Selection
foreign import d3Data_               :: D3Data   -> D3Selection -> D3Selection
foreign import d3RemoveSelection_    :: D3Selection -> D3Selection
foreign import d3AddTransition       :: D3Selection -> Transition -> D3Selection -- this is the PS transition record

-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are 
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_ :: String -> D3Selection -> ???? -- solve the ???? as needed later
foreign import d3SetAttr_      :: String -> D3Attr -> D3Selection -> Unit

foreign import emptyD3Selection :: D3Selection -- probably just null

coerceD3Data :: forall a. a -> D3Data
coerceD3Data = unsafeCoerce

data D3_Node = D3_Node Element Attributes (Array D3_Node)

node_ :: Element -> Array Attribute -> D3_Node
node_ = \e a -> D3_Node e a []
node__ :: Element -> D3_Node
node__ = \e -> D3_Node e [] []

appendChildren :: D3_Node -> Array D3_Node -> D3_Node
appendChildren (D3_Node element attrs children) newChildren
  = D3_Node element attrs (children <> newChildren)
infixl 1 appendChildren as ++

type Milliseconds = Int -- TODO smart constructor? possibly not worth it
type EasingTime = Number
type D3EasingFn = EasingTime -> EasingTime -- easing function maps 0-1 to 0-1 in some way with 0 -> 0, 1 -> 1
data EasingFunction = 
    DefaultCubic
  | EasingFunction D3EasingFn
  | EasingFactory (Datum -> Int -> D3Group -> D3This -> D3EasingFn)
type Transition = { 
    name     :: String
  , delay    :: Milliseconds -- can also be a function, ie (\d -> f d)
  , duration :: Milliseconds -- can also be a function, ie (\d -> f d)
  , easing   :: EasingFunction
}
makeTransition :: { 
  delay :: Number
, duration :: Number
, easing :: EasingFunction
, name :: String
}
makeTransition = { name: "", delay: 0.0, duration: 0.0, easing: DefaultCubic }
type TransitionStep = (Tuple Attributes (Maybe Transition))
type AttributeTransitionChain = Array TransitionStep
type EnterUpdateExit = {
    enter  :: AttributeTransitionChain
  , update :: AttributeTransitionChain
  , exit   :: AttributeTransitionChain
}



-- linksGroup :: ∀ m. (D3Tagless m) => m D3Selection
-- linksGroup = do
--   _ <- sequence append [ D3_Node { element: Group, attributes: [] } ]


module Selection where

import Attributes.Instances (Attribute(..), Attributes, Datum, unbox)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, Unit, bind, discard, liftA1, pure, show, ($), (<$>))

import Attributes.Helpers (fill, strokeColor, strokeOpacity)
import Control.Monad.State (class MonadState, StateT, get, put, runStateT)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Type.Proxy (Proxy(..))
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
foreign import data SelectionJS :: Type
foreign import nullSelectionJS :: SelectionJS -- probably just null

data Node = Node Element Attributes (Array Node)

node :: Element -> Array Attribute -> Array Node -> Node
node = Node
node_ :: Element -> Array Attribute -> Node
node_ = \e a -> Node e a []
node__ :: Element -> Node
node__ = \e -> Node e [] []

emptyNode :: Node
emptyNode = Node Group [] []
emptyJoinSelections :: { enter :: Node
, exit :: Node
, update :: Node
}
emptyJoinSelections = { enter: emptyNode, update: emptyNode, exit: emptyNode }

type EnterUpdateExit = {
    enter  :: Node
  , update :: Node
  , exit   :: Node
}

newtype D3M a = D3M (StateT SelectionJS Effect a) -- not using Effect to keep sigs simple for now

derive newtype instance functorD3M     :: Functor           D3M
derive newtype instance applyD3M       :: Apply             D3M
derive newtype instance applicativeD3M :: Applicative       D3M
derive newtype instance bindD3M        :: Bind              D3M
derive newtype instance monadD3M       :: Monad             D3M
derive newtype instance monadStateD3M  :: MonadState SelectionJS D3M
derive newtype instance monadEffD3M    :: MonadEffect       D3M

class (Monad m) <= D3Tagless m where
  hook   :: Selector -> m SelectionJS
  child :: Node -> m SelectionJS
  join   :: Element -> EnterUpdateExit -> m SelectionJS

runD3M :: ∀ a. D3M a -> SelectionJS -> Effect (Tuple a SelectionJS)
runD3M (D3M selection) = runStateT selection

d3State :: ∀ a. D3M a -> Effect SelectionJS
d3State (D3M selection) = liftA1 snd $ runStateT selection nullSelectionJS

d3Run :: ∀ a. D3M a -> Effect a
d3Run (D3M fse) = liftA1 fst $ runStateT fse nullSelectionJS

instance d3TaglessD3M :: D3Tagless D3M where
  hook selector = do
    let selection = d3SelectAll_ selector
    put selection
    pure selection  -- no attributes or children on hook point (at least for now, KISS)

  child (Node element attributes children) = do
    selection <- get
    let appended = d3Append_ (show element) selection
        _ = d3SetAttr_ "x" (unsafeCoerce "foo") appended
        _ = (setAttributeOnSelection appended) <$> attributes
        _ = (addChildToExisting appended) <$> children
    put appended
    pure appended 

  join element enterUpdateExit = do
    selection <- get
    let joined = d3Join_ (show element) selection
    put joined
    pure joined

addChildToExisting :: SelectionJS -> Node -> SelectionJS
addChildToExisting selection (Node element attributes children) = do
    let appended = d3Append_ (show element) selection
        _ = d3SetAttr_ "x" (unsafeCoerce "baar") appended
        _ = (setAttributeOnSelection appended) <$> attributes
    appended


setAttributeOnSelection :: SelectionJS -> Attribute -> Unit
setAttributeOnSelection selection (Attribute label attr) = d3SetAttr_ label (unbox attr) selection

appendChildToSelection :: SelectionJS -> Node -> SelectionJS
appendChildToSelection selection (Node element attributes children)  = d3Append_ (show element) selection

foreign import d3SelectAll_ :: Selector -> SelectionJS
foreign import d3Append_    :: String   -> SelectionJS -> SelectionJS
foreign import d3Join_      :: String   -> SelectionJS -> SelectionJS

-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are 
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_ :: String -> SelectionJS -> ???? -- solve the ???? as needed later
foreign import d3SetAttr_      :: String -> D3Attr -> SelectionJS -> Unit 

-- we'll just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr :: Type 

type SomeDatum = forall r. { fillColorField :: String | r }
_SomeDatum :: Proxy SomeDatum
_SomeDatum = Proxy

coerceFromSomeDatum :: (SomeDatum -> String) -> (Datum -> String)
coerceFromSomeDatum= unsafeCoerce

type SomeOtherDatum = forall r. { snek :: String | r }
_SomeOtherDatum :: Proxy SomeOtherDatum
_SomeOtherDatum = Proxy

-- WIP all the attr functions are written in terms of Datum which is opaque, but we know at compile-time what they 
-- really are so if we coerce the function AFTER we've type checked it against the type we know it will really be
-- we should be able to have polymorphism AND type-checking
someAttributes :: Proxy SomeDatum -> Attributes
someAttributes _ = [
    strokeColor "green"
  , strokeOpacity 0.75
  , fill $ coerceFromSomeDatum (\d -> d.fillColorField)
]

-- linksGroup :: ∀ m. (D3Tagless m) => m SelectionJS
-- linksGroup = do
--   _ <- sequence append [ Node { element: Group, attributes: [] } ]


script :: ∀ m. (D3Tagless m) => m SelectionJS
script = do
    root <- hook "div#root"
    
    svg <- child $ Node Svg (someAttributes _SomeDatum ) [ Node Group [] [ Node Circle [] []]]

    _ <- join Circle emptyJoinSelections 

    pure nullSelectionJS


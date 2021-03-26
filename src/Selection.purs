module Selection where

import Prelude hiding (append, join)

import Attributes.Instances (Attribute(..), Attributes, Datum, unbox)
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

node_ :: Element -> Array Attribute -> Node
node_ = \e a -> Node e a []
node__ :: Element -> Node
node__ = \e -> Node e [] []

appendChildren :: Node -> Array Node -> Node
appendChildren (Node element attrs children) newChildren
  = Node element attrs (children <> newChildren)

testEnterUpdateExit :: { enter :: Node
, exit :: Node
, update :: Node
}
testEnterUpdateExit = { enter: node__ Line, update: node__ Circle, exit: node__ Group }

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
  append :: Node -> m SelectionJS
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

  append node = do
    selection <- get
    let appended = doAppend node selection
    put appended
    pure appended 

  join element enterUpdateExit = do
    selection <- get
    let joined = d3Join_ (show element) selection
        enter  = doAppend enterUpdateExit.enter joined
        update = doAppend enterUpdateExit.update joined -- won't use doAppend with update and exit, i think
        -- plus we might want to handle children differently for all three cases because transitions, attrs, sequencing
        exit   = doAppend enterUpdateExit.exit joined -- then add the remove() call to exit??
        -- call merge now with enter + update? i think so
    put joined
    pure joined

doAppend :: Node -> SelectionJS -> SelectionJS
doAppend (Node element attributes children) selection = do
  let appended = d3Append_ (show element) selection
      _ = d3SetAttr_ "x" (unsafeCoerce "foo") appended
      _ = (setAttributeOnSelection appended) <$> attributes
      _ = (addChildToExisting appended) <$> children
  appended


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

infixl 1 appendChildren as ++

script :: ∀ m. (D3Tagless m) => m SelectionJS
script = do
    root <- hook "div#root"
    
    svg <- append $ Node Svg (someAttributes _SomeDatum ) [ Node Group [] [ node__ Circle ] ]

    _ <- join Circle testEnterUpdateExit 

    pure nullSelectionJS


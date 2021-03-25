module Selection where

import Attributes.Instances
import Prelude

import Control.Monad.State (class MonadState, StateT, get, runStateT)
import Data.Foldable (class Foldable)
import Data.Identity (Identity)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Unsafe.Coerce (unsafeCoerce)


type Selector = String 

data Element = Div | Svg | Circle | Line | Group
instance showElement :: Show Element where
  show Div    = "div"
  show Svg    = "svg"
  show Circle = "circle"
  show Line   = "line"
  show Group  = "group"

data Selection =
    Hook   Selector Selection
  | Append Element Attributes (Array Selection)
  | Join   Element Selection
  | End
  
-- || trying this with Finally Tagless instead of interpreter
foreign import data SelectionJS :: Type
foreign import nullSelectionJS :: SelectionJS -- probably just null

newtype SelectionPS = SelectionPS { attributes :: Attributes
                                  , children   :: Array SelectionPS }

emptySelectionPS = SelectionPS { attributes: [], children: [] }
emptyJoinSelections = { enter: emptySelectionPS, update: emptySelectionPS, exit: emptySelectionPS }

type EnterUpdateExit = {
    enter  :: SelectionPS
  , update :: SelectionPS
  , exit   :: SelectionPS
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
  append :: Element -> Attributes -> SelectionPS -> m SelectionJS
  join   :: Element -> EnterUpdateExit -> m SelectionJS

runD3M :: ∀ a. D3M a -> SelectionJS -> Effect (Tuple a SelectionJS)
runD3M (D3M selection) = runStateT selection

d3State :: ∀ a. D3M a -> Effect SelectionJS
d3State (D3M selection) = liftA1 snd $ runStateT selection nullSelectionJS

d3Run :: ∀ a. D3M a -> Effect a
d3Run (D3M fse) = liftA1 fst $ runStateT fse nullSelectionJS

instance d3TaglessD3M :: D3Tagless D3M where
  hook selector = pure $ d3SelectAll_ selector

  append element attributes selection = do
    (activeSelection :: SelectionJS) <- get
    let appended = d3Append_ (show element) activeSelection 
        _ = d3SetAttr_ "x" (unsafeCoerce "foo") appended
    pure appended 

  join element enterUpdateExit = do
    activeSelection <- get
    let joined = d3Join_ (show element) activeSelection
    pure joined

foreign import d3SelectAll_ :: Selector -> SelectionJS
foreign import d3Append_    :: String -> SelectionJS -> SelectionJS
foreign import d3Join_      :: String -> SelectionJS -> SelectionJS

-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are 
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_      :: String ->           SelectionJS -> ???? -- solve the ???? if needed 
foreign import d3SetAttr_      :: String -> D3Attr -> SelectionJS -> Unit 

foreign import data D3Attr :: Type -- we'll just coerce all our 

script :: ∀ m. (D3Tagless m) => m SelectionJS
script = do
    root  <- hook "div#root"
    svg   <- append Svg [] emptySelectionPS
    joined <- join Circle emptyJoinSelections 
    pure svg


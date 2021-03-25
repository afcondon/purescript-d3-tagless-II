module Selection where

import Attributes.Instances
import Prelude

import Attributes.Helpers (strokeColor, strokeOpacity)
import Control.Monad.State (class MonadState, StateT, get, put, runStateT)
import Data.Foldable (class Foldable)
import Data.Identity (Identity)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
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
  
-- || trying this with Finally Tagless instead of interpreter
foreign import data SelectionJS :: Type
foreign import nullSelectionJS :: SelectionJS -- probably just null

newtype SelectionPS = SelectionPS { element    :: Element
                                  , attributes :: Attributes
                                  , children   :: Array SelectionPS }

emptySelectionPS = SelectionPS { element: Group, attributes: [], children: [] }
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
  append :: SelectionPS -> m SelectionJS
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

  append (SelectionPS { element, attributes, children }) = do
    selection <- get
    let appended = d3Append_ (show element) selection
        _ = d3SetAttr_ "x" (unsafeCoerce "foo") appended
        _ = (setAttributeOnSelection appended) <$> attributes
    put appended
    pure appended 

  join element enterUpdateExit = do
    selection <- get
    let joined = d3Join_ (show element) selection
    put joined
    pure joined

setAttributeOnSelection :: SelectionJS -> Attribute -> Unit
setAttributeOnSelection selection (Attribute label attr) = d3SetAttr_ label (unsafeCoerce attr) selection

foreign import d3SelectAll_ :: Selector -> SelectionJS
foreign import d3Append_    :: String   -> SelectionJS -> SelectionJS
foreign import d3Join_      :: String   -> SelectionJS -> SelectionJS

-- NB D3 returns the selection after setting an Attr but we will only capture Selections that are 
-- meaningfully different _as_ selections, we're not chaining them in the same way
-- foreign import d3GetAttr_ :: String -> SelectionJS -> ???? -- solve the ???? as needed later
foreign import d3SetAttr_      :: String -> D3Attr -> SelectionJS -> Unit 

foreign import data D3Attr :: Type -- we'll just coerce all our Variants to one thing for the FFI since JS don't care

someAttributes :: Attributes
someAttributes = [
    strokeColor "green"
  , strokeOpacity 0.75
]

script :: ∀ m. (D3Tagless m) => m SelectionJS
script = do
    _ <- hook "div#root"
    
    _ <- append $ SelectionPS { element: Svg, attributes: someAttributes, children: [] } 

    _ <- join Circle emptyJoinSelections 

    pure nullSelectionJS


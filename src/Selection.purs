module Selection where

import Prelude hiding (append,join)

import Attributes.Helpers (fill, strokeColor, strokeOpacity)
import Attributes.Instances (Attribute(..), Attributes, Datum, unbox)
import Control.Monad.State (class MonadState, StateT, get, put, runStateT)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
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
foreign import data D3Selection  :: Type
foreign import data D3DomNode    :: Type
foreign import data D3This       :: Type
type D3Group = Array D3DomNode
foreign import data D3Transition :: Type -- not clear yet if we need to distinguish from Selection
-- we'll coerce everything to this type if we can validate attr lambdas against provided data
foreign import data D3Data :: Type 
-- ... and we'll also just coerce all our setters to one thing for the FFI since JS don't care
foreign import data D3Attr :: Type 
foreign import nullD3Selection :: D3Selection -- probably just null

foreign import d3SelectAllInDOM_     :: Selector -> D3Selection
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

coerceD3Data :: forall a. a -> D3Data
coerceD3Data = unsafeCoerce

data Node = Node Element Attributes (Array Node)

node_ :: Element -> Array Attribute -> Node
node_ = \e a -> Node e a []
node__ :: Element -> Node
node__ = \e -> Node e [] []

appendChildren :: Node -> Array Node -> Node
appendChildren (Node element attrs children) newChildren
  = Node element attrs (children <> newChildren)
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
type TransitionStage = (Tuple Attributes (Maybe Transition))
type AttributeTransitionChain = Array TransitionStage
type EnterUpdateExit = {
    enter  :: AttributeTransitionChain
  , update :: AttributeTransitionChain
  , exit   :: AttributeTransitionChain
}

newtype D3M a = D3M (StateT D3Selection Effect a) -- not using Effect to keep sigs simple for now

derive newtype instance functorD3M     :: Functor           D3M
derive newtype instance applyD3M       :: Apply             D3M
derive newtype instance applicativeD3M :: Applicative       D3M
derive newtype instance bindD3M        :: Bind              D3M
derive newtype instance monadD3M       :: Monad             D3M
derive newtype instance monadStateD3M  :: MonadState D3Selection D3M
derive newtype instance monadEffD3M    :: MonadEffect       D3M

class (Monad m) <= D3Tagless m where
  hook   :: Selector -> m D3Selection
  append :: Node -> m D3Selection
  join   :: Element -> EnterUpdateExit -> m D3Selection

runD3M :: ∀ a. D3M a -> D3Selection -> Effect (Tuple a D3Selection)
runD3M (D3M selection) = runStateT selection

d3State :: ∀ a. D3M a -> Effect D3Selection
d3State (D3M selection) = liftA1 snd $ runStateT selection nullD3Selection

d3Run :: ∀ a. D3M a -> Effect a
d3Run (D3M fse) = liftA1 fst $ runStateT fse nullD3Selection

instance d3TaglessD3M :: D3Tagless D3M where
  hook selector = do
    let selection = d3SelectAllInDOM_ selector
    put selection
    pure selection  -- no attributes or children on hook point (at least for now, KISS)

  append node = do
    selection <- get
    let appended = doAppend node selection
    put appended
    pure appended 

  join element enterUpdateExit = do -- TODO add data to the join
    selection <- get
    let -- TOOD d3Data_ with data
        initialS = d3SelectionSelectAll_ (show element) selection
        updateS  = d3Data_ (coerceD3Data [1,2,3]) initialS -- TODO this is where it's really tricky - attribute processing with shape of data open
        _ = foldl setAttributesAndTransition updateS enterUpdateExit.update
        -- TODO process further Tuple Transition Attributes things from enterUpdateExit.update

        enterS  = d3Append_ (show element) updateS -- TODO add Attrs for the inserted element here
        _      = foldl setAttributesAndTransition enterS enterUpdateExit.enter
        -- TODO process further Tuple Transition Attributes things from enterUpdateExit.enter

        exitS   = d3Exit_ updateS
        _       = foldl setAttributesAndTransition exitS enterUpdateExit.exit
        _       = d3RemoveSelection_ exitS -- TODO this is actually optional but by far more common to always remove

    put updateS -- not clear to me what actually has to be returned from join
    pure updateS

setAttributesAndTransition :: D3Selection -> TransitionStage -> D3Selection
setAttributesAndTransition selection (Tuple attributes (Just transition)) = do
  let _ = (setAttributeOnSelection selection) <$> attributes
  -- returning the transition as a "selection"
  d3AddTransition selection transition
setAttributesAndTransition selection (Tuple attributes Nothing) = do -- last stage of chain
  let _ = (setAttributeOnSelection selection) <$> attributes
  selection -- there's no next stage at end of chain

doAppend :: Node -> D3Selection -> D3Selection
doAppend (Node element attributes children) selection = do
  let appended = d3Append_ (show element) selection
      _ = d3SetAttr_ "x" (unsafeCoerce "foo") appended
      _ = (setAttributeOnSelection appended) <$> attributes
      _ = (addChildToExisting appended) <$> children
  appended


addChildToExisting :: D3Selection -> Node -> D3Selection
addChildToExisting selection (Node element attributes children) = do
    let appended = d3Append_ (show element) selection
        _ = d3SetAttr_ "x" (unsafeCoerce "baar") appended
        _ = (setAttributeOnSelection appended) <$> attributes
    appended


setAttributeOnSelection :: D3Selection -> Attribute -> Unit
setAttributeOnSelection selection (Attribute label attr) = d3SetAttr_ label (unbox attr) selection

appendChildToSelection :: D3Selection -> Node -> D3Selection
appendChildToSelection selection (Node element attributes children)  = d3Append_ (show element) selection

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

-- linksGroup :: ∀ m. (D3Tagless m) => m D3Selection
-- linksGroup = do
--   _ <- sequence append [ Node { element: Group, attributes: [] } ]

script :: ∀ m. (D3Tagless m) => m D3Selection
script = do
    root <- hook "div#root"
    
    svg <- append $ Node Svg (someAttributes _SomeDatum ) [ Node Group [] [ node__ Circle ] ]

    _ <- join Circle { enter: [], update: [], exit: [] }

    pure nullD3Selection


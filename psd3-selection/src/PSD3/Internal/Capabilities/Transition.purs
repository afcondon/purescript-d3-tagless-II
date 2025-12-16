module PSD3.Internal.Capabilities.Transition
  ( class TransitionM
  , withTransition
  , withTransitionExit
  , withTransitionStaggered
  , withTransitionExitStaggered
  , staggerByIndex
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import PSD3.Internal.Attribute (Attribute)
import PSD3.Internal.Selection.Types (SBoundOwns, SExiting)
import PSD3.Internal.Transition.Types (TransitionConfig)
import Web.DOM.Element (Element)

-- | Type class for transition operations
-- |
-- | This capability is separate from SelectionM to allow interpreters
-- | to choose whether to support animated transitions.
-- |
-- | Different interpreters can provide different implementations:
-- | - D3v2: Uses D3's transition engine for smooth animations
-- | - String: Could output CSS transitions or ignore transitions
-- | - Meta: Could record transition metadata
-- | - Music: Could create audio transitions
-- |
-- | The phantom type parameter (SBound) ensures that only bound selections
-- | (selections with data) can be transitioned, providing compile-time safety.
-- |
-- | Example user code:
-- | ```purescript
-- | animateCircles :: forall m sel. SelectionM sel m => TransitionM sel m => m Unit
-- | animateCircles = do
-- |   svg <- select "svg"
-- |   circles <- renderData Circle [1, 2, 3] "circle" svg
-- |     (Just \_ -> [fill "green", radius 10.0])
-- |     Nothing
-- |     Nothing
-- |
-- |   -- Animate to new state
-- |   withTransition (transition (Milliseconds 1000.0)) circles
-- |     [ fill "orange"
-- |     , radius 20.0
-- |     ]
-- | ```
class Monad m <= TransitionM sel m | m -> sel where

  -- | Apply a transition to a bound selection
  -- |
  -- | This animates the selection from its current state to the state
  -- | defined by the provided attributes, over the specified duration.
  -- |
  -- | Type Safety:
  -- | - Only works with SBoundOwns selections (selections with data)
  -- | - Attributes must match the selection's datum type
  -- | - Transition config specifies timing (duration, delay, easing)
  -- |
  -- | Example:
  -- | ```purescript
  -- | withTransition (transition (Milliseconds 1500.0)) circles
  -- |   [ fill "red"
  -- |   , cx 100.0
  -- |   , radius 25.0
  -- |   ]
  -- | ```
  -- |
  -- | With easing and delay:
  -- | ```purescript
  -- | let config = transitionWith
  -- |       { duration: Milliseconds 2000.0
  -- |       , delay: Just (Milliseconds 500.0)
  -- |       , easing: Just ElasticOut
  -- |       }
  -- | withTransition config circles [radius 30.0]
  -- | ```
  withTransition
    :: forall datum
     . TransitionConfig
    -> sel SBoundOwns Element datum
    -> Array (Attribute datum)
    -> m Unit

  -- | Apply a transition to an exiting selection
  -- |
  -- | Similar to withTransition but works with selections in the exit phase.
  -- | Useful for animating elements before they are removed (e.g., fade out, slide out).
  withTransitionExit
    :: forall datum
     . TransitionConfig
    -> sel SExiting Element datum
    -> Array (Attribute datum)
    -> m Unit

  -- | Apply a transition with per-element staggered delays
  -- |
  -- | Like withTransition but allows the delay to be computed dynamically
  -- | based on each element's datum and index. This is essential for creating
  -- | staggered animations where elements animate in sequence.
  -- |
  -- | The delay function receives (datum, index) and returns the delay in milliseconds.
  -- | The config.delay field is ignored in favor of the function.
  -- |
  -- | Example:
  -- | ```purescript
  -- | -- Each element delays 100ms more than the previous
  -- | withTransitionStaggered config (\_ i -> Milliseconds (toNumber i * 100.0)) circles
  -- |   [ cy 50.0
  -- |   , fill "red"
  -- |   ]
  -- |
  -- | -- Delay based on data value
  -- | withTransitionStaggered config (\d _ -> Milliseconds d.priority) items
  -- |   [ opacity 1.0 ]
  -- | ```
  withTransitionStaggered
    :: forall datum
     . TransitionConfig
    -> (datum -> Int -> Milliseconds) -- Per-element delay function
    -> sel SBoundOwns Element datum
    -> Array (Attribute datum)
    -> m Unit

  -- | Apply a staggered transition to an exiting selection
  -- |
  -- | Like withTransitionExit but with per-element delays for choreographed exit animations.
  withTransitionExitStaggered
    :: forall datum
     . TransitionConfig
    -> (datum -> Int -> Milliseconds)
    -> sel SExiting Element datum
    -> Array (Attribute datum)
    -> m Unit

-- | Helper to create a simple index-based stagger delay
-- |
-- | Creates a delay function that multiplies the index by a fixed amount.
-- | Useful for simple sequential animations.
-- |
-- | Example:
-- | ```purescript
-- | -- Each element delays 50ms more than the previous
-- | withTransitionStaggered config (staggerByIndex 50.0) circles [cy 100.0]
-- | ```
staggerByIndex :: forall datum. Number -> datum -> Int -> Milliseconds
staggerByIndex msPerElement = \_ i -> Milliseconds (toNumber i * msPerElement)

module PSD3v2.Transition.Types
  ( TransitionConfig
  , Easing(..)
  , transition
  , transitionWith
  , defaultTransition
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

-- | Configuration for a transition
-- |
-- | Specifies timing and easing for animated attribute changes.
-- | All times are in milliseconds.
-- |
-- | Example:
-- | ```purescript
-- | config = transition (Milliseconds 1500.0)
-- | configWithDelay = transitionWith
-- |   { duration: Milliseconds 1500.0
-- |   , delay: Just (Milliseconds 500.0)
-- |   , easing: Just Linear
-- |   }
-- | ```
type TransitionConfig =
  { duration :: Milliseconds
  , delay :: Maybe Milliseconds
  , easing :: Maybe Easing
  }

-- | Easing functions control the rate of change during transitions
-- |
-- | Based on D3's easing functions: https://d3js.org/d3-ease
-- | For more complex easing, use purescript-easings library
-- |
-- | Note: We could integrate with purescript-easings in the future,
-- | but starting simple with D3's built-in easings.
data Easing
  = Linear              -- Constant rate of change
  | Cubic               -- Cubic easing (default in D3)
  | CubicIn             -- Accelerating from zero velocity
  | CubicOut            -- Decelerating to zero velocity
  | CubicInOut          -- Accelerate then decelerate
  | Quad                -- Quadratic easing
  | QuadIn              -- Quadratic acceleration
  | QuadOut             -- Quadratic deceleration
  | QuadInOut           -- Quadratic acceleration/deceleration
  | Sin                 -- Sinusoidal easing
  | SinIn               -- Sinusoidal acceleration
  | SinOut              -- Sinusoidal deceleration
  | SinInOut            -- Sinusoidal acceleration/deceleration
  | Exp                 -- Exponential easing
  | ExpIn               -- Exponential acceleration
  | ExpOut              -- Exponential deceleration
  | ExpInOut            -- Exponential acceleration/deceleration
  | Circle              -- Circular easing
  | CircleIn            -- Circular acceleration
  | CircleOut           -- Circular deceleration
  | CircleInOut         -- Circular acceleration/deceleration
  | Elastic             -- Elastic easing with overshoot
  | ElasticIn           -- Elastic acceleration
  | ElasticOut          -- Elastic deceleration
  | ElasticInOut        -- Elastic acceleration/deceleration
  | Back                -- Anticipatory easing
  | BackIn              -- Anticipatory acceleration
  | BackOut             -- Anticipatory deceleration
  | BackInOut           -- Anticipatory acceleration/deceleration
  | Bounce              -- Bouncing easing
  | BounceIn            -- Bouncing acceleration
  | BounceOut           -- Bouncing deceleration
  | BounceInOut         -- Bouncing acceleration/deceleration

derive instance Eq Easing
derive instance Ord Easing

instance Show Easing where
  show Linear = "linear"
  show Cubic = "cubic"
  show CubicIn = "cubicIn"
  show CubicOut = "cubicOut"
  show CubicInOut = "cubicInOut"
  show Quad = "quad"
  show QuadIn = "quadIn"
  show QuadOut = "quadOut"
  show QuadInOut = "quadInOut"
  show Sin = "sin"
  show SinIn = "sinIn"
  show SinOut = "sinOut"
  show SinInOut = "sinInOut"
  show Exp = "exp"
  show ExpIn = "expIn"
  show ExpOut = "expOut"
  show ExpInOut = "expInOut"
  show Circle = "circle"
  show CircleIn = "circleIn"
  show CircleOut = "circleOut"
  show CircleInOut = "circleInOut"
  show Elastic = "elastic"
  show ElasticIn = "elasticIn"
  show ElasticOut = "elasticOut"
  show ElasticInOut = "elasticInOut"
  show Back = "back"
  show BackIn = "backIn"
  show BackOut = "backOut"
  show BackInOut = "backInOut"
  show Bounce = "bounce"
  show BounceIn = "bounceIn"
  show BounceOut = "bounceOut"
  show BounceInOut = "bounceInOut"

-- | Create a simple transition with just duration
-- |
-- | Uses default easing (Cubic) and no delay.
-- |
-- | Example:
-- | ```purescript
-- | config = transition (Milliseconds 1000.0)
-- | ```
transition :: Milliseconds -> TransitionConfig
transition duration =
  { duration
  , delay: Nothing
  , easing: Nothing  -- Will use D3's default (cubic)
  }

-- | Create a transition with full configuration
-- |
-- | Example:
-- | ```purescript
-- | config = transitionWith
-- |   { duration: Milliseconds 1500.0
-- |   , delay: Just (Milliseconds 500.0)
-- |   , easing: Just ElasticOut
-- |   }
-- | ```
transitionWith :: TransitionConfig -> TransitionConfig
transitionWith = identity

-- | Default transition configuration
-- |
-- | 250ms duration, no delay, cubic easing (D3 default)
defaultTransition :: TransitionConfig
defaultTransition = transition (Milliseconds 250.0)

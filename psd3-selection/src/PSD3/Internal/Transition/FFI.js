// D3 dependencies: d3-selection, d3-transition, d3-ease
import { select } from "d3-selection";
import "d3-transition"; // Extends selection with .transition()
import {
  easeLinear,
  easeQuad, easeQuadIn, easeQuadOut, easeQuadInOut,
  easeCubic, easeCubicIn, easeCubicOut, easeCubicInOut,
  easeSin, easeSinIn, easeSinOut, easeSinInOut,
  easeExp, easeExpIn, easeExpOut, easeExpInOut,
  easeCircle, easeCircleIn, easeCircleOut, easeCircleInOut,
  easeElastic, easeElasticIn, easeElasticOut, easeElasticInOut,
  easeBack, easeBackIn, easeBackOut, easeBackInOut,
  easeBounce, easeBounceIn, easeBounceOut, easeBounceInOut
} from "d3-ease";

// Create a D3 transition from an element
// Configures duration, optional delay, and optional easing
//
// D3 TRANSITION MECHANICS:
// D3 transitions animate FROM the element's current attribute values
// TO the target values set via transition.attr(). This means:
// 1. The element must have initial values set BEFORE the transition is created
// 2. The transition animates to values set on the transition object
// 3. If current and target values are the same, no visible animation occurs
export function createTransition_(duration) {
  return function(delay) {
    return function(easingName) {
      return function(element) {
        return function() {
          // Select the element and start a transition
          let transition = select(element).transition();

          // Set duration (required)
          transition = transition.duration(duration);

          // Set delay if provided
          if (delay != null) {
            transition = transition.delay(delay);
          }

          // Set easing if provided
          if (easingName != null) {
            // Map easing name to D3 easing function
            const easingFn = getD3EasingFunction(easingName);
            if (easingFn) {
              transition = transition.ease(easingFn);
            }
          }

          return transition;
        };
      };
    };
  };
}

// Set an attribute on a D3 transition
// This causes the attribute to animate to the target value
export function transitionSetAttribute_(name) {
  return function(value) {
    return function(transition) {
      return function() {
        transition.attr(name, value);
      };
    };
  };
}

// Remove elements after transition completes
// This is the D3 pattern: transition.remove()
export function transitionRemove_(transition) {
  return function() {
    transition.remove();
  };
}

// Map easing names to D3 easing functions
function getD3EasingFunction(name) {
  // D3 easing functions from d3-ease
  // See: https://d3js.org/d3-ease

  const easingMap = {
    // Linear
    'linear': easeLinear,

    // Polynomial (Quad, Cubic)
    'quad': easeQuad,
    'quadIn': easeQuadIn,
    'quadOut': easeQuadOut,
    'quadInOut': easeQuadInOut,

    'cubic': easeCubic,
    'cubicIn': easeCubicIn,
    'cubicOut': easeCubicOut,
    'cubicInOut': easeCubicInOut,

    // Sinusoidal
    'sin': easeSin,
    'sinIn': easeSinIn,
    'sinOut': easeSinOut,
    'sinInOut': easeSinInOut,

    // Exponential
    'exp': easeExp,
    'expIn': easeExpIn,
    'expOut': easeExpOut,
    'expInOut': easeExpInOut,

    // Circular
    'circle': easeCircle,
    'circleIn': easeCircleIn,
    'circleOut': easeCircleOut,
    'circleInOut': easeCircleInOut,

    // Elastic
    'elastic': easeElastic,
    'elasticIn': easeElasticIn,
    'elasticOut': easeElasticOut,
    'elasticInOut': easeElasticInOut,

    // Back
    'back': easeBack,
    'backIn': easeBackIn,
    'backOut': easeBackOut,
    'backInOut': easeBackInOut,

    // Bounce
    'bounce': easeBounce,
    'bounceIn': easeBounceIn,
    'bounceOut': easeBounceOut,
    'bounceInOut': easeBounceInOut,
  };

  return easingMap[name];
}

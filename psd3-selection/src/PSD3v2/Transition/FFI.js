import * as d3 from 'd3';

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
          let transition = d3.select(element).transition();

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
    'linear': d3.easeLinear,

    // Polynomial (Quad, Cubic)
    'quad': d3.easeQuad,
    'quadIn': d3.easeQuadIn,
    'quadOut': d3.easeQuadOut,
    'quadInOut': d3.easeQuadInOut,

    'cubic': d3.easeCubic,
    'cubicIn': d3.easeCubicIn,
    'cubicOut': d3.easeCubicOut,
    'cubicInOut': d3.easeCubicInOut,

    // Sinusoidal
    'sin': d3.easeSin,
    'sinIn': d3.easeSinIn,
    'sinOut': d3.easeSinOut,
    'sinInOut': d3.easeSinInOut,

    // Exponential
    'exp': d3.easeExp,
    'expIn': d3.easeExpIn,
    'expOut': d3.easeExpOut,
    'expInOut': d3.easeExpInOut,

    // Circular
    'circle': d3.easeCircle,
    'circleIn': d3.easeCircleIn,
    'circleOut': d3.easeCircleOut,
    'circleInOut': d3.easeCircleInOut,

    // Elastic
    'elastic': d3.easeElastic,
    'elasticIn': d3.easeElasticIn,
    'elasticOut': d3.easeElasticOut,
    'elasticInOut': d3.easeElasticInOut,

    // Back
    'back': d3.easeBack,
    'backIn': d3.easeBackIn,
    'backOut': d3.easeBackOut,
    'backInOut': d3.easeBackInOut,

    // Bounce
    'bounce': d3.easeBounce,
    'bounceIn': d3.easeBounceIn,
    'bounceOut': d3.easeBounceOut,
    'bounceInOut': d3.easeBounceInOut,
  };

  return easingMap[name];
}

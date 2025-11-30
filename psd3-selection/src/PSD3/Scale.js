// PSD3.Scale - D3 Scale FFI bindings
// D3 dependencies: d3-scale, d3-scale-chromatic, d3-interpolate
import {
  scaleLinear, scaleLog, scalePow, scaleSqrt, scaleSymlog,
  scaleOrdinal, scaleBand, scalePoint
} from "d3-scale";
import {
  schemeCategory10, schemePaired, schemeSet1, schemeSet2, schemeSet3,
  interpolateViridis, interpolatePlasma, interpolateInferno, interpolateMagma,
  interpolateTurbo, interpolateWarm, interpolateCool, interpolateRainbow,
  interpolateRdYlGn, interpolateRdBu, interpolatePiYG, interpolateBrBG
} from "d3-scale-chromatic";
import { interpolateRgb, interpolateHsl } from "d3-interpolate";

// ============================================================================
// CONTINUOUS SCALE CONSTRUCTORS
// ============================================================================

export const linear = scaleLinear();

export const log = scaleLog();

export const pow = scalePow();

export const sqrt = scaleSqrt();

export const symlog = scaleSymlog();

// ============================================================================
// ORDINAL SCALE CONSTRUCTORS
// ============================================================================

export const ordinal = scaleOrdinal();

export const band = scaleBand();

export const point = scalePoint();

// ============================================================================
// SCALE CONFIGURATION
// All configuration functions return a NEW scale (immutable API)
// ============================================================================

export function domain(d) {
  return function(scale) {
    return scale.copy().domain(d);
  };
}

export function range(r) {
  return function(scale) {
    return scale.copy().range(r);
  };
}

export function clamp(c) {
  return function(scale) {
    return scale.copy().clamp(c);
  };
}

export function nice(scale) {
  return scale.copy().nice();
}

export function niceCount(count) {
  return function(scale) {
    return scale.copy().nice(count);
  };
}

export function padding(p) {
  return function(scale) {
    return scale.copy().padding(p);
  };
}

export function paddingInner(p) {
  return function(scale) {
    return scale.copy().paddingInner(p);
  };
}

export function paddingOuter(p) {
  return function(scale) {
    return scale.copy().paddingOuter(p);
  };
}

export function align(a) {
  return function(scale) {
    return scale.copy().align(a);
  };
}

export function round(r) {
  return function(scale) {
    return scale.copy().round(r);
  };
}

export function base(b) {
  return function(scale) {
    return scale.copy().base(b);
  };
}

export function exponent(e) {
  return function(scale) {
    return scale.copy().exponent(e);
  };
}

export function constant(c) {
  return function(scale) {
    return scale.copy().constant(c);
  };
}

// ============================================================================
// SCALE OPERATIONS
// ============================================================================

export function applyScale(scale) {
  return function(value) {
    return scale(value);
  };
}

export function invert(scale) {
  return function(value) {
    if (typeof scale.invert === 'function') {
      const result = scale.invert(value);
      // Return Maybe (Just/Nothing)
      if (result !== undefined && !isNaN(result)) {
        return { tag: "Just", value: result };
      }
    }
    return { tag: "Nothing" };
  };
}

export function ticks(count) {
  return function(scale) {
    if (typeof scale.ticks === 'function') {
      return scale.ticks(count);
    }
    // For scales without ticks, return domain values
    return scale.domain();
  };
}

export function tickFormat(count) {
  return function(specifier) {
    return function(scale) {
      if (typeof scale.tickFormat === 'function') {
        return scale.tickFormat(count, specifier);
      }
      return String;
    };
  };
}

export function bandwidth(scale) {
  if (typeof scale.bandwidth === 'function') {
    return scale.bandwidth();
  }
  return 0;
}

export function step(scale) {
  if (typeof scale.step === 'function') {
    return scale.step();
  }
  return 0;
}

export function copy(scale) {
  return scale.copy();
}

// ============================================================================
// INTERPOLATORS
// ============================================================================

export function interpolateRgb_(a) {
  return function(b) {
    return interpolateRgb(a, b);
  };
}

export function interpolateHsl_(a) {
  return function(b) {
    return interpolateHsl(a, b);
  };
}

// ============================================================================
// COLOR SCHEMES (Categorical)
// Re-exported directly from d3-scale-chromatic
// ============================================================================

export { schemeCategory10, schemePaired, schemeSet1, schemeSet2, schemeSet3 };

// Indexed access to schemeCategory10 with modular wrapping
export function schemeCategory10At(index) {
  return schemeCategory10[Math.abs(index) % 10];
}

// Indexed access to schemePaired with modular wrapping
export function schemePairedAt(index) {
  return schemePaired[Math.abs(index) % 12];
}

// ============================================================================
// SEQUENTIAL INTERPOLATORS
// Re-exported directly from d3-scale-chromatic
// ============================================================================

export {
  interpolateViridis, interpolatePlasma, interpolateInferno, interpolateMagma,
  interpolateTurbo, interpolateWarm, interpolateCool, interpolateRainbow
};

// ============================================================================
// DIVERGING INTERPOLATORS
// Re-exported directly from d3-scale-chromatic
// ============================================================================

export { interpolateRdYlGn, interpolateRdBu, interpolatePiYG, interpolateBrBG };

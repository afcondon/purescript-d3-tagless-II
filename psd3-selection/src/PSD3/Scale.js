// PSD3.Scale - D3 Scale FFI bindings
import * as d3 from "d3";

// ============================================================================
// CONTINUOUS SCALE CONSTRUCTORS
// ============================================================================

export const linear = d3.scaleLinear();

export const log = d3.scaleLog();

export const pow = d3.scalePow();

export const sqrt = d3.scaleSqrt();

export const symlog = d3.scaleSymlog();

// ============================================================================
// ORDINAL SCALE CONSTRUCTORS
// ============================================================================

export const ordinal = d3.scaleOrdinal();

export const band = d3.scaleBand();

export const point = d3.scalePoint();

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

export function interpolateRgb(a) {
  return function(b) {
    return d3.interpolateRgb(a, b);
  };
}

export function interpolateHsl(a) {
  return function(b) {
    return d3.interpolateHsl(a, b);
  };
}

// ============================================================================
// COLOR SCHEMES (Categorical)
// ============================================================================

export const schemeCategory10 = d3.schemeCategory10;

export const schemePaired = d3.schemePaired;

export const schemeSet1 = d3.schemeSet1;

export const schemeSet2 = d3.schemeSet2;

export const schemeSet3 = d3.schemeSet3;

// ============================================================================
// SEQUENTIAL INTERPOLATORS
// ============================================================================

export const interpolateViridis = d3.interpolateViridis;

export const interpolatePlasma = d3.interpolatePlasma;

export const interpolateInferno = d3.interpolateInferno;

export const interpolateMagma = d3.interpolateMagma;

export const interpolateTurbo = d3.interpolateTurbo;

export const interpolateWarm = d3.interpolateWarm;

export const interpolateCool = d3.interpolateCool;

export const interpolateRainbow = d3.interpolateRainbow;

// ============================================================================
// DIVERGING INTERPOLATORS
// ============================================================================

export const interpolateRdYlGn = d3.interpolateRdYlGn;

export const interpolateRdBu = d3.interpolateRdBu;

export const interpolatePiYG = d3.interpolatePiYG;

export const interpolateBrBG = d3.interpolateBrBG;

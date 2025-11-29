// PSD3.Scale.FP - FFI for functional scale operations
import * as d3 from "d3";

export const createLinear = d3.scaleLinear();

export function unsafeCoerceScale(scale) {
  return scale;
}

export function floorImpl(n) {
  return Math.floor(n);
}

export function unsafeCoerce(x) {
  return x;
}

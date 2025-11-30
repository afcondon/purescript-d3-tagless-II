// PSD3.Scale.FP - FFI for functional scale operations
// D3 dependencies: d3-scale
import { scaleLinear } from "d3-scale";

export const createLinear = scaleLinear();

export function unsafeCoerceScale(scale) {
  return scale;
}

export function floorImpl(n) {
  return Math.floor(n);
}

export function unsafeCoerce(x) {
  return x;
}

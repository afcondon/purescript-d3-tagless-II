// FFI for ForceConfigs.purs - debug logging

let debugCallCount = 0;

export function debugGridPointX_(d) {
  debugCallCount++;

  // Only log first few calls to avoid flood
  if (debugCallCount <= 5) {
    console.log(`debugGridPointX_ call #${debugCallCount}:`, {
      d_type: typeof d,
      d_is_null: d === null,
      d_is_undefined: d === undefined,
      d_keys: d ? Object.keys(d).slice(0, 10) : 'N/A',
      d_x: d ? d.x : 'N/A',
      d_y: d ? d.y : 'N/A',
      d_gridXY: d ? d.gridXY : 'N/A',
      d_name: d ? d.name : 'N/A'
    });
  }

  // Try to return a valid value
  if (d && d.gridXY && d.gridXY.x !== undefined) {
    return d.gridXY.x;
  }
  if (d && d.x !== undefined) {
    return d.x;
  }

  console.error('debugGridPointX_: returning NaN! d=', d);
  return NaN;
}

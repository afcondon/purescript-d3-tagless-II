// FFI for ForceViz - setTimeout for multi-phase animation

export function setTimeout_(effect) {
  return delay => () => {
    return setTimeout(() => effect(), delay);
  };
}

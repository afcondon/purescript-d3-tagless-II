// REVIEW big TODO here is to expose the domain setting of the scales so that this is usable in multiple contexts

const d3SchemeCategory10 = d3.scaleOrdinal(d3.schemeCategory10)
export function d3SchemeCategory10N_(number) { return d3SchemeCategory10(number) }
export function d3SchemeCategory10S_(string) { return d3SchemeCategory10(string) }

const d3SchemePaired = d3.scaleOrdinal(d3.schemePaired)
export function d3SchemePairedN_(number) { return d3SchemePaired(number) }

const d3SchemeDiverging10 = d3.scaleDiverging(d3.interpolateBrBG)
    .domain([0, 250, 500]); // TODO this should be determined by number of nodes in sim
export function d3SchemeDiverging10N_(number) { return d3SchemeDiverging10(number) }

const d3SchemeSequential10 = d3.scaleSequential()
    .interpolator(d3.interpolateYlOrRd)
    .domain([0, 5, 10]); // TODO this should be determined by number of nodes in sim
export function d3SchemeSequential10N_(number) { return d3SchemeSequential10(number) }

// Direct interpolation functions (take 0-1 value, return color string)
export function d3InterpolateViridis_(t) { return d3.interpolateViridis(t) }
export function d3InterpolateRdYlGn_(t) { return d3.interpolateRdYlGn(t) }
export function d3InterpolatePlasma_(t) { return d3.interpolatePlasma(t) }
export function d3InterpolateInferno_(t) { return d3.interpolateInferno(t) }
export function d3InterpolateMagma_(t) { return d3.interpolateMagma(t) }
export function d3InterpolateTurbo_(t) { return d3.interpolateTurbo(t) }

// diverging example for reference
// colorScale = d3.scaleSequential()
//     .interpolator(d3.interpolateBrBG)
//     .domain([0,99]);

// sequential example for reference
// colorScale = d3.scaleSequential()
//     .interpolator(d3.interpolateRgb("purple", "orange"))
//     .domain([0,99]);

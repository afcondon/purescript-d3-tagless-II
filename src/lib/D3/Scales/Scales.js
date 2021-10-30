// REVIEW big TODO here is to expose the domain setting of the scales so that this is usable in multiple contexts

const d3SchemeCategory10 = d3.scaleOrdinal(d3.schemeCategory10)
exports.d3SchemeCategory10N_ = number => d3SchemeCategory10(number)
exports.d3SchemeCategory10S_ = string => d3SchemeCategory10(string)

const d3SchemePaired = d3.scaleOrdinal(d3.schemePaired)
exports.d3SchemePairedN_ = number => d3SchemePaired(number)
exports.d3SchemePairedS_ = string => d3SchemePaired(string)

const d3SchemeDiverging10 = d3.scaleDiverging(d3.interpolateBrBG)
                              .domain([0,250,500]); // TODO this should be determined by number of nodes in sim
exports.d3SchemeDiverging10N_ = number => d3SchemeDiverging10(number)

const d3SchemeSequential10 = d3.scaleSequential()
                               .interpolator(d3.interpolateYlOrRd)
                               .domain([0,5,10]); // TODO this should be determined by number of nodes in sim
exports.d3SchemeSequential10N_ = number => d3SchemeSequential10(number)


// diverging example for reference
// colorScale = d3.scaleSequential()
//     .interpolator(d3.interpolateBrBG)
//     .domain([0,99]);

// sequential example for reference
// colorScale = d3.scaleSequential()
//     .interpolator(d3.interpolateRgb("purple", "orange"))
//     .domain([0,99]);

const d3SchemeCategory10 = d3.scaleOrdinal(d3.schemeCategory10)
exports.d3SchemeCategory10N_ = number => d3SchemeCategory10(number)
exports.d3SchemeCategory10S_ = string => d3SchemeCategory10(string)

const d3SchemeDiverging10 = d3.scaleDiverging(d3.interpolateBrBG)
                              .domain([0,250,500]); // TODO this should be determined by number of nodes in sim
exports.d3SchemeDiverging10N_ = number => d3SchemeDiverging10(number)
# purescript-d3-tagless-II
Tagless final style interpreter / wrapper for D3 in PureScript, latest of many re-writes

Proper readme to come, see readmes for my repos with previous attempts for context

# state of this repo

400-ish lines of interpreter code and a  minimal example showing that this interpreter is capable of implementing 
the "General Update Pattern" (ie for transitions between data sets where entering, exiting and retained data are
all separately animated), a hierarchical example (in this case a radial tree) and a force layout graph. 

The latter two examples require some kind of `httpd` in the dist directory because they're fetching JSON data to show.

The three examples have been chosen to show three very different patterns of usage at the _D3 pattern language level_ rather than for any graphical or data visualization reason.

# next steps

zoom is top priority and then refinements of the DSL based on feedback from others
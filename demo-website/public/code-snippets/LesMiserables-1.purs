link_ = {
    source: _.source <<< unboxD3SimLink
  , target: _.target <<< unboxD3SimLink
  , value:  _.value <<< unboxD3SimLink
  , color:  d3SchemeCategory10N_ <<< toNumber <<< _.target.group <<< unboxD3SimLink
}

datum_ = {
-- direct accessors to fields of the datum (BOILERPLATE)
    id    : _.id <<< unboxD3SimNode -- NB the id in this case is a String
  , x     : _.x <<< unboxD3SimNode
  , y     : _.y <<< unboxD3SimNode
  , group : _.group <<< unboxD3SimNode

  , colorByGroup: d3SchemeCategory10N_ <<< toNumber <<< _.group <<< unboxD3SimNode
}

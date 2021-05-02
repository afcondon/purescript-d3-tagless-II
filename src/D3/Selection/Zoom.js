const debug = false;

// foreign import attachZoom :: D3Selection_ -> ZoomConfigDefault_ -> D3Selection_
exports.d3AttachZoomDefaultExtent_ = selection => config => { 
  if (debug) { showAttachZoomDefaultExtent_(selection)(config); }
  function zoomed({transform}) {
    selection.attr("transform", transform);
  }
  // "If extent is not specified, returns the current extent accessor, which
  // defaults to [[0, 0], [width, height]] where width is the client width of the
  // element and height is its client height; for SVG elements, the nearest
  // ancestor SVG element’s viewBox, or width and height attributes, are used.""
  return selection.call(d3.zoom() 
                  .scaleExtent(config.scaleExtent)
                  .on(`zoom.${config.qualifier}`, zoomed));
                  // .on("zoom", zoomed));
                }
                
// foreign import attachZoom :: D3Selection_ -> ZoomConfig_ -> D3Selection_
exports.d3AttachZoom_ = selection => config => { 
  if (debug) { showAttachZoom_(selection)(config) }
  function zoomed({transform}) { // TODO try arrow function below instead
    selection.attr("transform", transform);
  }
  return selection.call(d3.zoom()
                          .extent(config.extent)
                          .scaleExtent(config.scaleExtent)
                          .on(`zoom.${config.qualifier}`, zoomed));
                          // .on("zoom", zoomed));
}

exports.showAttachZoomDefaultExtent_ = selection => config => {
  return (`\t${selection}.call(zoom ${config})`)
}
exports.showAttachZoom_ = selection => config => {
  return (`\t${selection}.call(zoom ${config})`)
}
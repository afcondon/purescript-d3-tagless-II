// FFI bindings for d3-sankey to enable direct comparison
import { sankey as d3Sankey, sankeyLinkHorizontal, sankeyJustify, sankeyLeft, sankeyRight, sankeyCenter } from "d3-sankey";

// Create and configure a d3-sankey generator, then compute layout
export function computeD3SankeyLayout_(nodes) {
  return links => width => height => nodeWidth => nodePadding => iterations => {
    // Create sankey generator with configuration
    const generator = d3Sankey()
      .nodeId(d => d.name)
      .nodeAlign(sankeyJustify)
      .nodeWidth(nodeWidth)
      .nodePadding(nodePadding)
      .extent([[1, 1], [width - 1, height - 5]])
      .iterations(iterations);

    // Deep clone to avoid mutation issues
    const graphData = {
      nodes: nodes.map(n => ({ ...n })),
      links: links.map(l => ({ ...l }))
    };

    // Compute layout
    const result = generator(graphData);

    // Convert to plain objects for PureScript
    return {
      nodes: result.nodes.map(n => ({
        name: n.name,
        index: n.index,
        depth: n.depth,
        height: n.height,
        layer: n.layer,
        value: n.value,
        x0: n.x0,
        x1: n.x1,
        y0: n.y0,
        y1: n.y1
      })),
      links: result.links.map(l => ({
        sourceIndex: l.source.index,
        targetIndex: l.target.index,
        value: l.value,
        width: l.width,
        y0: l.y0,
        y1: l.y1,
        index: l.index
      }))
    };
  };
}

// Compute layout with intermediate states captured at each iteration
export function computeD3SankeyWithSteps_(nodes) {
  return links => width => height => nodeWidth => nodePadding => maxIterations => {
    const steps = [];

    // Deep clone original data
    const graphData = {
      nodes: nodes.map(n => ({ ...n })),
      links: links.map(l => ({ ...l }))
    };

    // Capture state after each iteration by running with increasing iteration counts
    for (let iter = 0; iter <= maxIterations; iter++) {
      // Re-clone for each iteration count
      const iterData = {
        nodes: nodes.map(n => ({ ...n })),
        links: links.map(l => ({ ...l }))
      };

      const generator = d3Sankey()
        .nodeId(d => d.name)
        .nodeAlign(sankeyJustify)
        .nodeWidth(nodeWidth)
        .nodePadding(nodePadding)
        .extent([[1, 1], [width - 1, height - 5]])
        .iterations(iter);

      const result = generator(iterData);

      steps.push({
        iteration: iter,
        label: iter === 0 ? "Initial (no relaxation)" : `After ${iter} relaxation iteration${iter > 1 ? 's' : ''}`,
        nodes: result.nodes.map(n => ({
          name: n.name,
          index: n.index,
          depth: n.depth,
          height: n.height,
          layer: n.layer,
          value: n.value,
          x0: n.x0,
          x1: n.x1,
          y0: n.y0,
          y1: n.y1
        })),
        links: result.links.map(l => ({
          sourceIndex: l.source.index,
          targetIndex: l.target.index,
          value: l.value,
          width: l.width,
          y0: l.y0,
          y1: l.y1,
          index: l.index
        }))
      });
    }

    return steps;
  };
}

// Generate SVG path for a sankey link (horizontal bezier curve)
export function sankeyLinkPath_(link) {
  return nodes => {
    const source = nodes.find(n => n.index === link.sourceIndex);
    const target = nodes.find(n => n.index === link.targetIndex);
    if (!source || !target) return "";

    // Reconstruct link object for d3's path generator
    const linkObj = {
      source: source,
      target: target,
      width: link.width,
      y0: link.y0,
      y1: link.y1
    };

    return sankeyLinkHorizontal()(linkObj) || "";
  };
}

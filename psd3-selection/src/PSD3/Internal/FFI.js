// D3 dependencies: d3-selection, d3-drag, d3-force, d3-hierarchy, d3-shape, d3-chord, d3-zoom, d3-ease, d3-scale-chromatic, d3-scale
import { select, selectAll } from "d3-selection";
import { drag } from "d3-drag";
import {
  forceSimulation, forceCenter, forceCollide, forceLink,
  forceManyBody, forceRadial, forceX, forceY
} from "d3-force";
import {
  hierarchy, cluster, tree, pack, treemap, partition
} from "d3-hierarchy";
import {
  linkHorizontal, linkVertical, linkRadial, arc
} from "d3-shape";
import { chord, ribbon } from "d3-chord";
import { zoom } from "d3-zoom";
import { easeCubicOut, easeLinear, easeQuadInOut, easeBounceOut } from "d3-ease";
import { schemeCategory10, schemeTableau10, interpolateRdYlGn, interpolateViridis } from "d3-scale-chromatic";
import { scaleLinear, scaleOrdinal } from "d3-scale";

// =============================================================================
// Direct D3 Re-exports (for demo components to use without direct D3 imports)
// =============================================================================
export { select, selectAll, drag, zoom };
export { easeCubicOut, easeLinear, easeQuadInOut, easeBounceOut };
export { schemeCategory10, schemeTableau10, interpolateRdYlGn, interpolateViridis };
export { scaleLinear, scaleOrdinal };

const debug = false
export const emptyD3Data_ = null
export function d3Append_(element) { return selection => { return selection.append(element) } }
export function d3Data_(data) { return selection => { return selection.data(data) } }
export function d3DataWithKeyFunction_(data) { return keyFn => selection => { return selection.data(data, keyFn) } }
export function d3DataWithFunction_(extractFn) { return keyFn => selection => { return selection.data(extractFn, keyFn) } }
export function d3EnterAndAppend_(element) { return selection => { return selection.enter().append(element) } }
export function d3GetExitSelection_(selection) { return selection.exit() }
export function d3GetEnterSelection_(selection) { return selection.enter() }
export function d3GetSelectionData_(selection) { return selection.data() }
export function d3FilterSelection_(selection) { return selector => selection.filter(selector) }
export function d3LowerSelection_(selection) { return selection.lower() }
export function d3MergeSelectionWith_(enter) { return update => { return enter.merge(update); } }
export function d3OrderSelection_(selection) { return selection.order() }
export function d3RaiseSelection_(selection) { return selection.raise() }
export function d3RemoveSelection_(selection) { return selection.remove() }
export function d3SelectAllInDOM_(selector) { return selectAll(selector) }
export function d3SelectFirstInDOM_(selector) { return select(selector) }
export function d3SelectionIsEmpty_(selection) { return selection.empty() }
export function d3SelectionSelect_(selector) { return selection => { return selection.select(selector) } }
export function d3SelectionSelectAll_(selector) { return selection => { return selection.selectAll(selector) } }
export function d3SetAttr_(name) { return value => selection => { return selection.attr(name, value) } }
export function d3SetHTML_(value) { return selection => { return selection.html(value) } }
export function d3SetProperty_(value) { return selection => { return selection.property(value) } }
export function d3SetText_(value) { return selection => { return selection.text(value) } }
export function d3SortSelection_(selection) { return compare => selection.sort(compare) }
export function simulationDrag_(label) { return selection => simulation => dragFn => selection.call(dragFn(label, simulation)) }
export function disableDrag_(selection) { return selection.on('.drag', null) }
export function getIndexFromDatum_(datum) { return (typeof datum.index == `undefined`) ? "?" : datum.index }
export function selectionOn_(selection) { return event => callback => { return selection.on(event, callback) } }
export function d3AddTransition_(selection) {
  return transition => {
    var handle
    if (transition.name == '') {
      handle = selection.transition()
      // if transition is unnamed we configure it...
      if (transition.duration != 0) {
        handle.duration(transition.duration)
      }
      if (transition.delay != 0) {
        handle.delay(transition.delay)
      }
    } else {
      handle = selection.transition(transition.name)
    }
    return handle
  }
}
// *****************************************************************************************************************
// *****  there will either need to be quite a range of these functions or a way of writing them in Purs     *******
// *****  this is really down in the weeds of D3 without supporting abstractions in the PS library           *******
// *****  CONCRETE EXAMPLE: this defaults to updating fx but in Spago example position is on parent, using   *******
// *****  transforms to move both circle and label together (only way to position a <group> in SVG)
// *****************************************************************************************************************
export function simdrag_(label, simulation) {
  function dragstarted(event) {
    if (!event.active) simulation.alphaTarget(0.3).restart();
    event.subject.fx = event.subject.x;
    event.subject.fy = event.subject.y;
  }
  function dragged(event) {
    event.subject.fx = event.x;
    event.subject.fy = event.y;
  }
  function dragended(event) {
    if (!event.active) simulation.alphaTarget(0);
    event.subject.fx = null;
    event.subject.fy = null;
  }
  return drag()
    .on('start.' + label, dragstarted)
    .on('drag.' + label, dragged)
    .on('end.' + label, dragended);
}

// Drag that only allows horizontal movement (preserves fy)
export function simdragHorizontal_(label, simulation) {
  function dragstarted(event) {
    if (!event.active) simulation.alphaTarget(0.3).restart();
    event.subject.fx = event.subject.x;
    // Don't touch fy - keep it pinned
  }
  function dragged(event) {
    event.subject.fx = event.x;
    // Don't touch fy - keep it pinned
  }
  function dragended(event) {
    if (!event.active) simulation.alphaTarget(0);
    event.subject.fx = null;
    // Don't touch fy - keep it pinned
  }
  return drag()
    .on('start.' + label, dragstarted)
    .on('drag.' + label, dragged)
    .on('end.' + label, dragended);
}
// Helper functions for highlighting connected nodes
export function highlightConnectedNodes_(selection) {
  return connectedIds => {
    // Convert PureScript array to Set for fast lookup
    const connectedSet = new Set(connectedIds);
    selection.selectAll('.node-group')
      .classed('highlighted', d => connectedSet.has(d.id))
      .classed('dimmed', d => !connectedSet.has(d.id));
  };
}

export function clearHighlights_(selection) {
  selection.selectAll('.node-group')
    .classed('highlighted', false)
    .classed('dimmed', false);
}

// Unpin all nodes by setting fy to null
export function unpinAllNodes_(simulation) {
  simulation.nodes().forEach(node => {
    node.fx = null;
    node.fy = null;
  });
  // Reheat the simulation to see the effect
  simulation.alpha(0.3).restart();
}

// Update bubble node radii based on expanded state and reheat simulation
export function updateBubbleRadii_(simulation) {
  return nodeRadiusFn => {
    // Select all circles and update their radius
    selectAll('.bubble-graph .node-group circle.node-circle')
      .attr('r', d => nodeRadiusFn(d.expanded)(d.loc))

    // Reheat the simulation so collision forces update
    simulation.alpha(0.3).restart()
  }
}

// Color mapping for declaration kinds
function declarationColor(kind) {
  switch(kind) {
    case 'value':         return '#2196F3'  // Blue - functions/values
    case 'externValue':   return '#00BCD4'  // Cyan - foreign functions
    case 'data':          return '#4CAF50'  // Green - data types
    case 'typeClass':     return '#9C27B0'  // Purple - type classes
    case 'typeSynonym':   return '#FF9800'  // Orange - type synonyms
    case 'alias':         return '#FF9800'  // Orange - aliases
    case 'externData':    return '#F44336'  // Red - foreign data
    case 'typeClassInstance': return '#E91E63'  // Pink - instances
    default:              return '#757575'  // Gray - unknown
  }
}

// Highlight dependencies when hovering over a declaration
function highlightDependencies(qualifiedName, functionCallsData, isHighlighted) {
  if (!functionCallsData || !functionCallsData.functions) {
    console.log('No function calls data available')
    return
  }

  // Find the function in the call graph
  const funcEntry = functionCallsData.functions.find(f => f.key === qualifiedName)

  if (!funcEntry) {
    console.log(`Function ${qualifiedName} not found in call graph`)
    return
  }

  const funcInfo = funcEntry.value
  const relatedNames = new Set()

  // Add all functions this function calls (outbound)
  if (funcInfo.calls) {
    funcInfo.calls.forEach(call => {
      const targetQualifiedName = `${call.targetModule}.${call.target}`
      relatedNames.add(targetQualifiedName)
    })
  }

  // Add all functions that call this function (inbound)
  if (funcInfo.calledBy) {
    funcInfo.calledBy.forEach(callerName => {
      relatedNames.add(callerName)
    })
  }

  console.log(`Highlighting ${relatedNames.size} related functions for ${qualifiedName}`)

  // Highlight all related declaration circles
  relatedNames.forEach(name => {
    const selector = `.decl-circle[data-qualified-name="${name}"]`
    selectAll(selector)
      .classed('dep-highlighted', true)
      .attr('stroke', '#FFD700')  // Gold stroke
      .attr('stroke-width', 3)
      .raise()  // Bring to front
  })

  // Also highlight the hovered circle itself with a different style
  const hoveredSelector = `.decl-circle[data-qualified-name="${qualifiedName}"]`
  selectAll(hoveredSelector)
    .classed('dep-source', true)
    .attr('stroke', '#FF4500')  // Orange-red stroke
    .attr('stroke-width', 4)
    .raise()
}

// Clear dependency highlights
function clearDependencyHighlights() {
  selectAll('.decl-circle.dep-highlighted')
    .classed('dep-highlighted', false)
    .attr('stroke', '#fff')
    .attr('stroke-width', 1)

  selectAll('.decl-circle.dep-source')
    .classed('dep-source', false)
    .attr('stroke', '#fff')
    .attr('stroke-width', 1)
}

// Categorize declarations into groups
function categorizeDeclarations(declarations) {
  const categories = {
    dataTypes: [],
    instances: [],
    functions: []
  }

  declarations.forEach(decl => {
    switch(decl.kind) {
      case 'data':
      case 'typeSynonym':
      case 'alias':
      case 'externData':
        categories.dataTypes.push(decl)
        break
      case 'typeClassInstance':
        categories.instances.push(decl)
        break
      case 'value':
      case 'typeClass':
      default:
        categories.functions.push(decl)
        break
    }
  })

  return categories
}

// Update node expansion state - show/hide declaration circles
export function updateNodeExpansion_(simulation) {
  return nodeRadiusFn => declarationsData => functionCallsData => clickedNodeData => {
    const moduleName = clickedNodeData.name
    const isExpanded = clickedNodeData.expanded

    // Find the node group for this module
    const nodeGroup = selectAll('.bubble-graph .node-group')
      .filter(d => d.id === moduleName)

    if (isExpanded) {
      // Find declarations for this module
      const moduleDecls = declarationsData.modules.find(m => m.name === moduleName)
      if (!moduleDecls || !moduleDecls.declarations || moduleDecls.declarations.length === 0) {
        console.log(`No declarations found for ${moduleName}`)
        return
      }

      const declarations = moduleDecls.declarations
      console.log(`Expanding ${moduleName} with ${declarations.length} declarations`)

      // Categorize declarations into groups
      const categories = categorizeDeclarations(declarations)

      // Create three-layer hierarchy: Module → Categories → Items
      const categoryChildren = []

      if (categories.dataTypes.length > 0) {
        categoryChildren.push({
          name: 'Data Types',
          isCategory: true,
          children: categories.dataTypes.map(d => ({
            name: d.title,
            kind: d.kind,
            isCategory: false,
            value: 1
          }))
        })
      }

      if (categories.instances.length > 0) {
        categoryChildren.push({
          name: 'Instances',
          isCategory: true,
          children: categories.instances.map(d => ({
            name: d.title,
            kind: d.kind,
            isCategory: false,
            value: 1
          }))
        })
      }

      if (categories.functions.length > 0) {
        categoryChildren.push({
          name: 'Functions',
          isCategory: true,
          children: categories.functions.map(d => ({
            name: d.title,
            kind: d.kind,
            isCategory: false,
            value: 1
          }))
        })
      }

      const hierarchyData = {
        name: moduleName,
        children: categoryChildren
      }

      const root = hierarchy(hierarchyData)
        .sum(d => d.value)

      // Compute pack layout (size based on expanded radius)
      const expandedRadius = nodeRadiusFn(true)(clickedNodeData.loc)
      const packLayout = pack()
        .size([expandedRadius * 2, expandedRadius * 2])
        .padding(3)  // More padding for labels

      packLayout(root)

      // Get leaf nodes (the declarations)
      const leaves = root.leaves()

      // Create a map from declaration name to position for link drawing
      const declPositions = new Map()
      leaves.forEach(leaf => {
        declPositions.set(leaf.data.name, {
          x: leaf.x - expandedRadius,
          y: leaf.y - expandedRadius
        })
      })

      // Find intra-module function calls
      const intraModuleLinks = []
      if (functionCallsData && functionCallsData.functions) {
        functionCallsData.functions.forEach(fn => {
          const funcInfo = fn.value
          if (funcInfo.module === moduleName) {
            funcInfo.calls.forEach(call => {
              // Only show links within the same module
              if (call.targetModule === moduleName && !call.isCrossModule) {
                const sourceName = funcInfo.name
                const targetName = call.target

                const sourcePos = declPositions.get(sourceName)
                const targetPos = declPositions.get(targetName)

                if (sourcePos && targetPos && sourceName !== targetName) {
                  intraModuleLinks.push({
                    source: sourceName,
                    target: targetName,
                    sourcePos,
                    targetPos
                  })
                }
              }
            })
          }
        })
      }

      console.log(`Found ${intraModuleLinks.length} intra-module links for ${moduleName}`)

      // Add dependency arrows before circles (so they're behind)
      const linksGroup = nodeGroup.selectAll('g.decl-links')
        .data([0])  // Single element to create the group once
        .enter()
        .append('g')
        .attr('class', 'decl-links')
        .lower()  // Put links behind circles

      const arrows = nodeGroup.select('g.decl-links')
        .selectAll('path.decl-link')
        .data(intraModuleLinks, d => `${d.source}-${d.target}`)

      arrows.enter()
        .append('path')
        .attr('class', 'decl-link')
        .attr('d', d => {
          // Create a curved path from source to target
          const dx = d.targetPos.x - d.sourcePos.x
          const dy = d.targetPos.y - d.sourcePos.y
          const dr = Math.sqrt(dx * dx + dy * dy) * 0.7  // Curve factor
          return `M${d.sourcePos.x},${d.sourcePos.y}A${dr},${dr} 0 0,1 ${d.targetPos.x},${d.targetPos.y}`
        })
        .attr('stroke', '#666')
        .attr('stroke-width', 1)
        .attr('stroke-opacity', 0.4)
        .attr('fill', 'none')

      // Get ALL nodes (not just leaves) - this includes category bubbles
      const allNodes = root.descendants().filter(d => d.depth > 0) // Skip root module node

      // Add groups for both category bubbles and declaration circles
      const nodeGroups = nodeGroup.selectAll('g.decl-group')
        .data(allNodes, d => d.data.name)

      const nodeGroupsEnter = nodeGroups.enter()
        .append('g')
        .attr('class', d => d.data.isCategory ? 'category-group' : 'decl-group')
        .attr('transform', d => `translate(${d.x - expandedRadius}, ${d.y - expandedRadius})`)

      // Add circles (styled differently for categories vs declarations)
      const circles = nodeGroupsEnter.append('circle')
        .attr('class', d => d.data.isCategory ? 'category-circle' : 'decl-circle')
        .attr('r', d => d.r)
        .attr('fill', d => {
          // Category bubbles: neutral gray
          // Declaration circles: colored by kind
          return d.data.isCategory ? '#e0e0e0' : declarationColor(d.data.kind)
        })
        .attr('opacity', d => d.data.isCategory ? 0.3 : 0.8)  // Categories more transparent
        .attr('stroke', '#fff')
        .attr('stroke-width', 1)

      // Add data attribute with qualified name for declarations (module.name)
      circles.filter(d => !d.data.isCategory)
        .attr('data-qualified-name', d => `${moduleName}.${d.data.name}`)

      // Add hover handlers to declaration circles
      circles.filter(d => !d.data.isCategory)
        .on('mouseenter', function(event, d) {
          const qualifiedName = `${moduleName}.${d.data.name}`
          highlightDependencies(qualifiedName, functionCallsData, true)
        })
        .on('mouseleave', function(event, d) {
          clearDependencyHighlights()
        })

      circles.append('title')
        .text(d => d.data.isCategory
          ? d.data.name  // Category name only
          : `${d.data.name} (${d.data.kind})`  // Declaration with kind
        )

      // Add text labels ONLY for declarations (not categories)
      nodeGroupsEnter.filter(d => !d.data.isCategory)
        .append('text')
        .attr('class', 'decl-label')
        .attr('text-anchor', 'middle')
        .attr('dy', '0.35em')
        .attr('font-size', d => Math.min(d.r / 3, 10) + 'px')  // Scale font with circle size
        .attr('fill', '#fff')
        .attr('pointer-events', 'none')
        .text(d => {
          // Truncate long names to fit in circle
          const maxChars = Math.floor(d.r / 3)
          return d.data.name.length > maxChars
            ? d.data.name.substring(0, maxChars) + '...'
            : d.data.name
        })

      // Add arrowhead marker definition (if not already defined)
      if (!select('defs marker#arrowhead').node()) {
        select('.bubble-graph').append('defs')
          .append('marker')
          .attr('id', 'arrowhead')
          .attr('viewBox', '0 -5 10 10')
          .attr('refX', 5)
          .attr('refY', 0)
          .attr('markerWidth', 6)
          .attr('markerHeight', 6)
          .attr('orient', 'auto')
          .append('path')
          .attr('d', 'M0,-5L10,0L0,5')
          .attr('fill', '#666')
      }

    } else {
      // Remove declaration groups and links
      nodeGroup.selectAll('g.decl-group').remove()
      nodeGroup.selectAll('g.decl-links').remove()
    }

    // Update main circle radius
    nodeGroup.select('circle.node-circle')
      .attr('r', nodeRadiusFn(isExpanded)(clickedNodeData.loc))

    // Reheat simulation
    simulation.alpha(0.3).restart()
  }
}

// Unsafe helper to set a field on a JavaScript object
export function unsafeSetField_(field) {
  return value => obj => () => {
    obj[field] = value
  }
}

// Expand a node by its ID (used for expanding connected nodes)
export function expandNodeById_(simulation) {
  return nodeRadiusFn => declarationsData => functionCallsData => nodeId => shouldExpand => () => {
    // Find the node in the simulation
    const nodes = simulation.nodes()
    const node = nodes.find(n => n.id === nodeId)

    if (!node) {
      console.log(`Node ${nodeId} not found in simulation`)
      return
    }

    // Only toggle if the current state is different from desired state
    if (node.expanded !== shouldExpand) {
      node.expanded = shouldExpand
      console.log(`${nodeId} ${shouldExpand ? 'expanded' : 'collapsed'} via expandNodeById`)

      // Call the expansion update
      updateNodeExpansion_(simulation)(nodeRadiusFn)(declarationsData)(functionCallsData)(node)
    }
  }
}

// Add arrow marker definition for module-level links
export function addModuleArrowMarker_(svgSelection) {
  return () => {
    // Check if marker already exists
    // Note: svgSelection is already a D3 selection, don't wrap it again
    if (!svgSelection.select('defs marker#module-arrow').node()) {
      svgSelection.append('defs')
        .append('marker')
        .attr('id', 'module-arrow')
        .attr('viewBox', '0 -5 10 10')
        .attr('refX', 15)
        .attr('refY', 0)
        .attr('markerWidth', 6)
        .attr('markerHeight', 6)
        .attr('orient', 'auto')
        .append('path')
        .attr('d', 'M0,-5L10,0L0,5')
        .attr('fill', '#999')
    }
  }
}

// Draw inter-module declaration links between expanded modules
export function drawInterModuleDeclarationLinks_(zoomGroupSelection) {
  return nodeRadiusFn => declarationsData => functionCallsData => () => {
    // Find all expanded module nodes
    const expandedModules = []
    selectAll('.bubble-graph .node-group').each(function(d) {
      if (d && d.expanded) {
        const moduleDecls = declarationsData.modules.find(m => m.name === d.name)
        if (moduleDecls && moduleDecls.declarations) {
          const expandedRadius = nodeRadiusFn(true)(d.loc)

          // Build declaration position map for this module (using same three-layer structure)
          const categories = categorizeDeclarations(moduleDecls.declarations)
          const categoryChildren = []

          if (categories.dataTypes.length > 0) {
            categoryChildren.push({
              name: 'Data Types',
              isCategory: true,
              children: categories.dataTypes.map(decl => ({
                name: decl.title,
                kind: decl.kind,
                isCategory: false,
                value: 1
              }))
            })
          }

          if (categories.instances.length > 0) {
            categoryChildren.push({
              name: 'Instances',
              isCategory: true,
              children: categories.instances.map(decl => ({
                name: decl.title,
                kind: decl.kind,
                isCategory: false,
                value: 1
              }))
            })
          }

          if (categories.functions.length > 0) {
            categoryChildren.push({
              name: 'Functions',
              isCategory: true,
              children: categories.functions.map(decl => ({
                name: decl.title,
                kind: decl.kind,
                isCategory: false,
                value: 1
              }))
            })
          }

          const hierarchyData = {
            name: d.name,
            children: categoryChildren
          }

          const root = hierarchy(hierarchyData).sum(node => node.value)
          const packLayout = pack()
            .size([expandedRadius * 2, expandedRadius * 2])
            .padding(3)
          packLayout(root)

          const declPositions = new Map()
          root.leaves().forEach(leaf => {
            declPositions.set(leaf.data.name, {
              x: leaf.x - expandedRadius,
              y: leaf.y - expandedRadius
            })
          })

          expandedModules.push({
            moduleName: d.name,
            moduleX: d.x,
            moduleY: d.y,
            declPositions
          })
        }
      }
    })

    console.log(`Found ${expandedModules.length} expanded modules`)

    // Find all cross-module links between expanded modules
    const interModuleLinks = []
    if (functionCallsData && functionCallsData.functions) {
      functionCallsData.functions.forEach(fn => {
        const funcInfo = fn.value
        const sourceModule = expandedModules.find(m => m.moduleName === funcInfo.module)

        if (sourceModule) {
          funcInfo.calls.forEach(call => {
            if (call.isCrossModule) {
              const targetModule = expandedModules.find(m => m.moduleName === call.targetModule)

              if (targetModule) {
                const sourcePos = sourceModule.declPositions.get(funcInfo.name)
                const targetPos = targetModule.declPositions.get(call.target)

                if (sourcePos && targetPos) {
                  interModuleLinks.push({
                    sourceModule: funcInfo.module,
                    targetModule: call.targetModule,
                    source: funcInfo.name,
                    target: call.target,
                    sourceX: sourceModule.moduleX + sourcePos.x,
                    sourceY: sourceModule.moduleY + sourcePos.y,
                    targetX: targetModule.moduleX + targetPos.x,
                    targetY: targetModule.moduleY + targetPos.y
                  })
                }
              }
            }
          })
        }
      })
    }

    console.log(`Found ${interModuleLinks.length} inter-module declaration links`)

    // Create or update inter-module links group
    // Note: zoomGroupSelection is already a D3 selection, don't wrap it again
    let linksGroup = zoomGroupSelection.select('g.inter-module-decl-links')
    if (linksGroup.empty()) {
      linksGroup = zoomGroupSelection
        .insert('g', 'g.link')  // Insert before module-level links
        .attr('class', 'inter-module-decl-links')
    }

    // Bind data and update links
    const paths = linksGroup.selectAll('path.inter-decl-link')
      .data(interModuleLinks, d => `${d.sourceModule}:${d.source}-${d.targetModule}:${d.target}`)

    // Remove old links
    paths.exit().remove()

    // Add new links
    paths.enter()
      .append('path')
      .attr('class', 'inter-decl-link')
      .attr('stroke', '#e91e63')  // Pink color to distinguish from module links
      .attr('stroke-width', 2)
      .attr('stroke-opacity', 0.6)
      .attr('fill', 'none')
      .merge(paths)
      .attr('d', d => {
        const dx = d.targetX - d.sourceX
        const dy = d.targetY - d.sourceY
        const dr = Math.sqrt(dx * dx + dy * dy) * 0.5
        return `M${d.sourceX},${d.sourceY}A${dr},${dr} 0 0,1 ${d.targetX},${d.targetY}`
      })
  }
}

// Filter simulation to only keep nodes with IDs in the provided set
// Also updates the DOM by removing filtered-out nodes and links
export function filterToConnectedNodes_(simulation) {
  return keyFn => nodeIds => {
    // Get current nodes and links
    const allNodes = simulation.nodes();
    const linkForce = simulation.force(linksForceName_);
    const allLinks = linkForce ? linkForce.links() : [];

    // Create a Set for faster lookup
    const idSet = new Set(nodeIds);

    // Filter nodes - keep only those with IDs in the set
    const filteredNodes = allNodes.filter(node => idSet.has(keyFn(node)));

    // Filter links - keep only those where both source and target are in the set
    const filteredLinks = allLinks.filter(link => {
      const sourceId = typeof link.source === 'object' ? keyFn(link.source) : link.source;
      const targetId = typeof link.target === 'object' ? keyFn(link.target) : link.target;
      return idSet.has(sourceId) && idSet.has(targetId);
    });

    // Update simulation with filtered data
    simulation.nodes(filteredNodes);
    if (linkForce) {
      linkForce.links(filteredLinks);
    }

    // Update DOM - remove nodes that aren't in the filtered set
    // Find all node groups and filter them
    select('div.svg-container')
      .selectAll('g.node g.node-group')
      .filter(d => !idSet.has(keyFn(d)))
      .remove();

    // Update DOM - remove links that aren't in the filtered set
    select('div.svg-container')
      .selectAll('g.link path')
      .filter(d => {
        const sourceId = typeof d.source === 'object' ? keyFn(d.source) : d.source;
        const targetId = typeof d.target === 'object' ? keyFn(d.target) : d.target;
        return !idSet.has(sourceId) || !idSet.has(targetId);
      })
      .remove();

    // Reheat and restart
    simulation.alpha(0.5).restart();
  };
}

export const linksForceName_ = "links"
export const dummyForceHandle_ = null
export function disableTick_(simulation) { return name => { return simulation.on('tick.' + name, () => null) } }
export function forceCenter_() { return forceCenter() }
export function forceCollideFn_() { return forceCollide() }
export function forceCustom_(forceFn) { return forceFn() }
export function forceLink_() { return forceLink().id(d => d.id) }
export function forceMany_() { return forceManyBody() }
export function forceRadial_() { return forceRadial() }
export function forceX_() { return forceX() }
export function forceY_() { return forceY() }
export function getLinksFromForce_(linkForce) { return linkForce.links() }
export function getNodes_(simulation) { return simulation.nodes() }
export function keyIsID_(d) {
  // SimulationNode is row-polymorphic - user data fields are at top level
  return d.id;
}
export function keyIsSourceTarget_(d) {
  // console.log(`FFI: looking up the id of node: ${[d.source, d.target]}`);
  return [d.source, d.target];
}

// Key function for swizzled links - extracts id from the node objects
// Uses 'id' field to match link source/target IDs
export function swizzledLinkKey_(d) {
  // After swizzling, source/target are node objects with 'id' field
  const sourceId = typeof d.source === 'object' ? d.source.id : d.source;
  const targetId = typeof d.target === 'object' ? d.target.id : d.target;
  const key = `${sourceId}->${targetId}`;
  // Debug: log first few keys
  if (!window._linkKeyDebugCount) window._linkKeyDebugCount = 0;
  if (window._linkKeyDebugCount < 5) {
    console.log(`swizzledLinkKey_: ${key} (source type: ${typeof d.source}, target type: ${typeof d.target})`);
    window._linkKeyDebugCount++;
  }
  return key;
}
export function setAlpha_(simulation) {
  return alpha => {
    console.log(`FFI: setting simulation.alpha to ${alpha}`);
    simulation.alpha(alpha)
  }
}
export function setAlphaDecay_(simulation) { return alphaDecay => simulation.alphaDecay(alphaDecay) }
export function setAlphaMin_(simulation) { return alphaMin => simulation.alphaMin(alphaMin) }
export function setAlphaTarget_(simulation) { return alphaTarget => simulation.alphaTarget(alphaTarget) }
export function setAsNullForceInSimulation_(simulation) { return label => simulation.force(label, null) }
export function setForceCx_(force) { return attr => force.cx(attr) }
export function setForceCy_(force) { return attr => force.cy(attr) }
export function setForceDistance_(force) { return attr => force.distance(attr) }
export function setForceDistanceMax_(force) { return attr => force.distanceMax(attr) }
export function setForceDistanceMin_(force) { return attr => force.distanceMin(attr) }
export function setForceIterations_(force) { return attr => force.iterations(attr) }
export function setForceRadius_(force) { return attr => force.radius(attr) }
export function setForceStrength_(force) { return attr => force.strength(attr) }
export function setForceTheta_(force) { return attr => force.theta(attr) }
export function setForceX_(force) { return attr => force.x(attr) }
export function setForceY_(force) { return attr => force.y(attr) }
export function setLinksKeyFunction_(force) { return attr => force.id(attr) }
export function setVelocityDecay_(simulation) { return velocityDecay => simulation.velocityDecay(velocityDecay) }
export function startSimulation_(simulation) {
  console.log(`FFI: restarting the simulation, alpha before: ${simulation.alpha()}`);
  // IMPORTANT: restart() alone doesn't reset alpha - we need to set it to 1.0 first
  simulation.alpha(1.0).restart();
  console.log(`FFI: restarted simulation, alpha after: ${simulation.alpha()}`);
}
export function stopSimulation_(simulation) { return simulation.stop() }
export function initSimulation_(config) {
  return keyFn => {
    const simulation = forceSimulation([])
      .force(linksForceName_, forceLink([]).id(keyFn))
      .alpha(config.alpha) // default is 1
      .alphaTarget(config.alphaTarget) // default is 0
      .alphaMin(config.alphaMin) // default is 0.0001
      .alphaDecay(config.alphaDecay) // default is 0.0228
      .velocityDecay(config.velocityDecay) // default is 0.4
    if (true) {
      console.log(`FFI: initSimulation${simulation}`)
    }
    // Expose simulation to window for force control panel
    window._psd3_simulation = simulation;
    return simulation
  }
}
export function configSimulation_(simulation) {
  return config => {
    simulation
      .alpha(config.alpha) // default is 1
      .alphaTarget(config.alphaTarget) // default is 0
      .alphaMin(config.alphaMin) // default is 0.0001
      .alphaDecay(config.alphaDecay) // default is 0.0228
      .velocityDecay(config.velocityDecay) // default is 0.4
    if (debug) {
      console.log(`FFI: configSimulation${simulation}${config}`)
    }
    return simulation
  }
}
export function readSimulationVariables_(simulation) {
  return {
    alpha: simulation.alpha(),
    alphaTarget: simulation.alphaTarget(),
    alphaMin: simulation.alphaMin(),
    alphaDecay: simulation.alphaDecay(),
    velocityDecay: simulation.velocityDecay()
  }
}

export function d3PreserveSimulationPositions_(selection) {
  return nodedata => keyFn => {
    // create a map from our chosen id to the OLD obj reference, got from the data thats attached to selection
    const oldNodeMap = new Map(selection.data().map(d => [keyFn(d), d]));
    // create a map from our chosen id to the NEW / incoming obj reference
    const newNodeMap = new Map(nodedata.map(d => [keyFn(d), d]));
    // we need to copy the fx/fy (at least) from the updating data 
    console.log(`FFI: d3PreserveSimulationPositions_ given ${nodedata.length} nodes, in selection ${selection.data().length}`);

    // REVIEW (also what if we wanted r, say, or x, to change???)
    // we need to be able to specify which fields are to change, ideally, and which are not
    let updatedNodeData = nodedata.map(d => {
      let id = keyFn(d)
      let newNode = newNodeMap.get(id)
      let shell = {}
      if (newNode) {
        console.log(`FFI: copying fx/fy from incoming node to old object (if present)`);
        shell = { fx: newNode.fx, fy: newNode.fy, gridXY: newNode.gridXY, updated: true }
      }
      return Object.assign(oldNodeMap.get(id) || d, shell)
    });
    return updatedNodeData
  }
}
export function d3PreserveLinkReferences_(link) {
  return links => {
    const old = new Map(link.data().map(d => [getLinkID_(d), d]));
    let updatedLinkData = links.map(d => Object.assign(old.get(getLinkID_(d)) || d, {}));
    // now, based on link signature, we should really de-swizzle here? and we may HAVE TO do so
    return updatedLinkData
  }
}
export function getIDsFromNodes_(nodes) {
  return keyFn => {
    const keys = [];
    for (let i = 0; i < nodes.length; i++) {
      keys[i] = keyFn(nodes[i]);
    }
    return keys
  }
}

export function setNodes_(simulation) {
  return nodes => {
    console.log(`FFI: setting nodes in simulation, there are ${nodes.length} nodes`);

    // DEBUG: Check incoming node x/y values
    if (nodes.length > 0) {
      const first5 = nodes.slice(0, 5);
      console.log('FFI setNodes_ INCOMING - first 5 nodes x/y:', first5.map(n => ({ id: n.id, name: n.name, x: n.x, y: n.y, gridXY: n.gridXY })));

      // Check for any NaN or undefined values
      const badNodes = nodes.filter(n => isNaN(n.x) || isNaN(n.y) || n.x === undefined || n.y === undefined);
      if (badNodes.length > 0) {
        console.error(`FFI setNodes_ INCOMING: ${badNodes.length} nodes have NaN/undefined x or y!`);
        console.error('First bad node:', badNodes[0]);
      }
    }

    // Get old nodes from simulation to preserve their positions
    const oldNodes = simulation.nodes();

    // Create map of old nodes by ID for O(1) lookup
    const oldNodeMap = new Map(oldNodes.map(d => [d.id, d]));

    // Merge positions from old nodes into new nodes
    const nodesWithPositions = nodes.map(newNode => {
      const oldNode = oldNodeMap.get(newNode.id);
      if (oldNode) {
        // Preserve simulation state (position, velocity) from old node
        // But keep any explicitly set fx/fy from new node (for pinning)
        return Object.assign({}, newNode, {
          x: oldNode.x,
          y: oldNode.y,
          vx: oldNode.vx,
          vy: oldNode.vy,
          // Only override fx/fy if new node has them explicitly set
          fx: newNode.fx !== undefined ? newNode.fx : oldNode.fx,
          fy: newNode.fy !== undefined ? newNode.fy : oldNode.fy
        });
      }
      return newNode; // New node, no position to preserve
    });

    // DEBUG: Check outgoing node x/y values
    if (nodesWithPositions.length > 0) {
      const first5 = nodesWithPositions.slice(0, 5);
      console.log('FFI setNodes_ OUTGOING - first 5 nodes x/y:', first5.map(n => ({ id: n.id, name: n.name, x: n.x, y: n.y })));

      const badNodes = nodesWithPositions.filter(n => isNaN(n.x) || isNaN(n.y) || n.x === undefined || n.y === undefined);
      if (badNodes.length > 0) {
        console.error(`FFI setNodes_ OUTGOING: ${badNodes.length} nodes have NaN/undefined x or y!`);
        console.error('First bad node:', badNodes[0]);
      }
    }

    simulation.nodes(nodesWithPositions);

    // DEBUG: Check nodes immediately after setting
    const afterNodes = simulation.nodes();
    if (afterNodes.length > 0) {
      const first = afterNodes[0];
      console.log('FFI setNodes_ AFTER simulation.nodes() - first node:', {
        id: first.id,
        name: first.name,
        x: first.x,
        y: first.y,
        vx: first.vx,
        vy: first.vy,
        gridXY: first.gridXY
      });
      if (isNaN(first.x) || isNaN(first.y)) {
        console.error('FFI setNodes_: NaN detected IMMEDIATELY after simulation.nodes()!');
      }
    }

    return afterNodes;
  }
}
// we're going to always use the same name for the links force denominated by the linksForceName string
export function setLinks_(simulation) {
  return links => {
    console.log(`FFI: setting links in simulation, there are ${links.length} links`);
    const linkForce = simulation.force(linksForceName_);
    if (linkForce) {
      linkForce.links(links);
    } else {
      console.log(`FFI: links force not found (may be disabled), skipping setLinks`);
    }
  }
}
// returns array of links with ids replaced by object references, invalid links are discarded
// Creates copies of links to avoid mutating the original array
export function swizzleLinks_(links) {
  return simNodes => keyFn => {
    console.log(`FFI: swizzling links in simulation, there are ${links.length} links`);
    const nodeById = new Map(simNodes.map(d => [keyFn(d), d])); // creates a map from our chosen id to the old obj reference

    // Map to copies first, then filter - this prevents mutation of original links
    // Note: Must explicitly copy source/target as they may be on prototype (PureScript records)
    const swizzledLinks = links.map(link => ({
      source: link.source,
      target: link.target,
      ...link
    })).filter((link, index, arr) => {
      // look up both source and target (which could be id or obj reference)
      // if both source and target are found in nodeMap then we can swizzle and return true
      // else we just return false and this node will go in the bit bucket
      if (typeof link.source !== "object") {
        link.source = nodeById.get(link.source) // try to get object reference if we don't have it
      } else {
        link.source = nodeById.get(keyFn(link.source)) // try to replace object reference with new object reference
      }
      if (typeof link.target !== "object") {
        link.target = nodeById.get(link.target)
      } else {
        link.target = nodeById.get(keyFn(link.target))
      }
      // now let's see what we got from that and if we have a valid link or not
      if (typeof link.source === 'undefined' || link.target === 'undefined') {
        return false; // filter this link
      } else {
        link.id = keyFn(link.source) + "-" + keyFn(link.target)
        return true // we've updated the link
      }
    })
    return swizzledLinks
  }
}
export function unsetLinks_(simulation) {
  // Set link force to null - this is now only called when links shouldn't be displayed at all
  // Scenes that need links displayed should keep the link force in their activeForces
  simulation.force(linksForceName_, null)
  console.log('FFI: disabled link force (set to null)');
  return simulation
}
// this will work on both swizzled and unswizzled links
export function getLinkID_(keyFn) {
  return link => { // version for generating an ID for the link object
    const sourceID = (typeof link.source == `object`) ? keyFn(link.source) : link.source
    const targetID = (typeof link.target == `object`) ? keyFn(link.target) : link.target
    return sourceID + "-" + targetID
  }
}
// For unswizzled links, source/target are just the ID values directly
// (swizzled links would have objects, but this function is for filtering unswizzled links)
export function getLinkIDs_(link) {
  return { sourceID: link.source, targetID: link.target }
}
export function getLinksFromSimulation_(simulation) {
  linksForce = simulation.force(linksForceName_)
  if (typeof linksForce === `undefined`) {
    return [] // either the force wasn't found, or the force wasn't a links force
  }
  const result = linksForce.links()
  if (typeof result === `undefined`) {
    return []
  }
  return result
}
export function onTick_(simulation) {
  return name => tickFn => {
    let tickCount = 0;
    var result = simulation.on('tick.' + name, () => {
      tickCount++;
      // Debug: log simulation node state on first few ticks
      if (tickCount <= 3) {
        const simNodes = simulation.nodes();
        if (simNodes.length > 0) {
          const first = simNodes[0];
          console.log(`TICK ${tickCount} (${name}): first sim node x=${first.x}, y=${first.y}, vx=${first.vx}, vy=${first.vy}`);
          // Check for NaN
          if (isNaN(first.x) || isNaN(first.y)) {
            console.error(`TICK ${tickCount}: NaN detected in simulation nodes!`);
          }
        }
      }
      tickFn()
    })
    return result;
  }
}
export function defaultNodeTick_(label) {
  return simulation => nodeSelection => {
    simulation.on('tick.' + label, () => {
      nodeSelection.attr('cx', d => d.x)
        .attr('cy', d => d.y)
    })
  }
}
export function defaultLinkTick_(label) {
  return simulation => linksShown => {
    simulation.on('tick.' + label, () => {
      linksShown.attr("x1", d => d.source.x)
        .attr("y1", d => d.source.y)
        .attr("x2", d => d.target.x)
        .attr("y2", d => d.target.y);
    })
  }
}
export function lookupForceByName_(simulation) {
  return name => {
    let lookup = simulation.force(name)
    if (typeof lookup === `undefined`) {
      return null;
    }
    return lookup;
  }
}
export function removeFixForceXY_(simulation) {
  return filterFn => {
    let filteredNodes = simulation.nodes().filter(filterFn)
    for (let index = 0; index < filteredNodes.length; index++) {
      // console.log(`removing FixForceXY from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fx = null;
      filteredNodes[index].fy = null;
    }
  }
}
export function removeFixForceX_(simulation) {
  return filterFn => {
    let filteredNodes = simulation.nodes().filter(filterFn)
    for (let index = 0; index < filteredNodes.length; index++) {
      // console.log(`removing FixForceX from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fx = null;
    }
  }
}
export function removeFixForceY_(simulation) {
  return filterFn => {
    let filteredNodes = simulation.nodes().filter(filterFn)
    for (let index = 0; index < filteredNodes.length; index++) {
      // console.log(`removing FixForceY from node: ${filteredNodes[index].id}`);
      filteredNodes[index].fy = null;
    }
  }
}
export function applyFixForceInSimulationXY_(simulation) {
  return label => fn => filterFn => {

    let nodes = simulation.nodes()   // get nodes from simulation
    let filteredNodes = nodes.filter(filterFn)
    for (let index = 0; index < filteredNodes.length; index++) {
      let i = index
      let position = fn(filteredNodes[i])(i)   // set each node's fx,fy using fn function
      filteredNodes[i].fx = position.x
      filteredNodes[i].fy = position.y;
      filteredNodes[i].fixIndex_ = i; // in case _other_ elements need to know the cluster point of this element, because it's index is a filtered index
    }
  }
}
export function applyFixForceInSimulationX_(simulation) {
  return label => fn => filterFn => {
    let nodes = simulation.nodes()
    for (let index = 0; index < nodes.length; index++) {
      if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
        let position = fn(nodes[index])
        nodes[index].fx = position.x
      }
    }
  }
}
export function applyFixForceInSimulationY_(simulation) {
  return label => fn => filterFn => {
    let nodes = simulation.nodes()
    for (let index = 0; index < nodes.length; index++) {
      if (filterFn(nodes[index])) { // only fix nodes that this thing applies to
        let position = fn(nodes[index])
        nodes[index].fy = position.y;
      }
    }
  }
}
export function putForceInSimulation_(simulation) {
  return label => force => {
    // console.log(`FFI: Putting ${label} force in the simulation`);
    simulation.force(label, force)
    // Keep window._psd3_simulation updated for ForceControlPanel
    window._psd3_simulation = simulation;
  }
}

// REVIEW a whole group of side effecting function
export function pinNode_(fx) {
  return fy => node => {
    node.fx = fx
    node.fy = fy
    return node
  }
}
export function pinNamedNode_(name) {
  return fx => fy => node => {
    if (node.name === name) {
      node.fx = fx
      node.fy = fy
    }
    return node
  }
}
export function pinTreeNode_(node) { node.fx = node.treeX; node.fy = node.treeY; return node } // if treeX/Y is null, no harm!
export function setInSimNodeFlag_(node) { node.inSim = true; return node }
export function unsetInSimNodeFlag_(node) { node.inSim = false; return node }
export function unpinNode_(node) { node.fx = null; node.fy = null; return node }
// *****************************************************************************************************************
// ************************** functions from d3js Chord module         *****************************************
// *****************************************************************************************************************
export function chordLayout_(matrix) {
  return chord()(matrix);
}
export function chordLayoutWithPadAngle_(matrix) {
  return padAngle => chord().padAngle(padAngle)(matrix);
}
export function chordGroups_(chordLayout) { return chordLayout.groups }
export function chordArray_(chordLayout) {
  return Array.from(chordLayout);
}
export function ribbonGenerator_() { return ribbon() }
export function arcGenerator_() { return arc() }
export function ribbonPath_(generator) { return chord => generator(chord) }
export function arcPath_(generator) { return group => generator(group) }
export function setRibbonRadius_(generator) { return radius => { generator.radius(radius); return generator } }
export function setArcInnerRadius_(generator) { return radius => { generator.innerRadius(radius); return generator } }
export function setArcOuterRadius_(generator) { return radius => { generator.outerRadius(radius); return generator } }

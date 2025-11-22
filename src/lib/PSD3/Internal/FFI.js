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
export function d3SelectAllInDOM_(selector) { return d3.selectAll(selector) }
export function d3SelectFirstInDOM_(selector) { return d3.select(selector) }
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
  return d3.drag()
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
  return d3.drag()
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
    d3.selectAll('.bubble-graph .node-group circle.node-circle')
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
    d3.selectAll(selector)
      .classed('dep-highlighted', true)
      .attr('stroke', '#FFD700')  // Gold stroke
      .attr('stroke-width', 3)
      .raise()  // Bring to front
  })

  // Also highlight the hovered circle itself with a different style
  const hoveredSelector = `.decl-circle[data-qualified-name="${qualifiedName}"]`
  d3.selectAll(hoveredSelector)
    .classed('dep-source', true)
    .attr('stroke', '#FF4500')  // Orange-red stroke
    .attr('stroke-width', 4)
    .raise()
}

// Clear dependency highlights
function clearDependencyHighlights() {
  d3.selectAll('.decl-circle.dep-highlighted')
    .classed('dep-highlighted', false)
    .attr('stroke', '#fff')
    .attr('stroke-width', 1)

  d3.selectAll('.decl-circle.dep-source')
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
    const nodeGroup = d3.selectAll('.bubble-graph .node-group')
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

      const root = d3.hierarchy(hierarchyData)
        .sum(d => d.value)

      // Compute pack layout (size based on expanded radius)
      const expandedRadius = nodeRadiusFn(true)(clickedNodeData.loc)
      const packLayout = d3.pack()
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
      if (!d3.select('defs marker#arrowhead').node()) {
        d3.select('.bubble-graph').append('defs')
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
    d3.selectAll('.bubble-graph .node-group').each(function(d) {
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

          const root = d3.hierarchy(hierarchyData).sum(node => node.value)
          const packLayout = d3.pack()
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
    d3.select('div.svg-container')
      .selectAll('g.node g.node-group')
      .filter(d => !idSet.has(keyFn(d)))
      .remove();

    // Update DOM - remove links that aren't in the filtered set
    d3.select('div.svg-container')
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
export function forceCenter_() { return d3.forceCenter() }
export function forceCollideFn_() { return d3.forceCollide() }
export function forceCustom_(forceFn) { return forceFn() }
export function forceLink_() { return d3.forceLink().id(d => d.id) }
export function forceMany_() { return d3.forceManyBody() }
export function forceRadial_() { return d3.forceRadial() }
export function forceX_() { return d3.forceX() }
export function forceY_() { return d3.forceY() }
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
    const simulation = d3
      .forceSimulation([])
      .force(linksForceName_, d3.forceLink([]).id(keyFn))
      .alpha(config.alpha) // default is 1
      .alphaTarget(config.alphaTarget) // default is 0
      .alphaMin(config.alphaMin) // default is 0.0001
      .alphaDecay(config.alphaDecay) // default is 0.0228
      .velocityDecay(config.velocityDecay) // default is 0.4
    if (true) {
      console.log(`FFI: initSimulation${simulation}`)
    }
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

    simulation.nodes(nodesWithPositions);
    return simulation.nodes();
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
    var result = simulation.on('tick.' + name, () => {
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
    for (let index = 0; index < filteredNodes.length; index++) { // TODO do this with map
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
  }
}
// exports.restartLinksForceInSimulation_ = simulation => force => links => {
//   console.log(`Re-enabling links force in the simulation`);
//   simulation.force(this.linksForceName, force)
//   simulation.links(links) // NB these links are the SWIZZLED links that are cached in the D3SimulationState_
// }
// exports.putForceInSimulationWithFilter_ = simulation => label => filterFn => force => {
//   console.log(`FFI: Putting ${label} force in the simulation`);
//   console.log("remember to put in the filter here"); // TODO
//   simulation.force(label, force)
// }
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
// ************************** functions from d3js Hierarchy module         *****************************************
// *****************************************************************************************************************
// TODO replace with a configurable hierarchy function in PS and direct calls to hierarchy, sort etc as appropriate
export function ancestors_(tree) { return tree.ancestors() }
export function descendants_(tree) { return tree.descendants() }
export function find_(tree) { return filter => tree.find(filter) }
export function getClusterLayoutFn_() { return d3.cluster() }
export function getTreeLayoutFn_() { return d3.tree() }
export function hasChildren_(d) { return (d.children === 'undefined') ? false : true }
export function getHierarchyValue_(d) { return (d.value === 'undefined') ? null : d.value } // returns a Nullable Number 
export function getHierarchyChildren_(d) { return !d.children ? [] : d.children }
export function getHierarchyParent_(d) { return !d.parent ? [] : d.parent } // don't think this can ever be null in valid hierarchy node but this gives us confidence that PureScript type is right 
export function hierarchyFromJSON_(json) { return d3.hierarchy(json) }
export function cloneTreeJson_(json) { return structuredClone(json) }
export function hNodeDepth_(node) { return node.depth }
export function hNodeHeight_(node) { return node.height }
export function hNodeX_(node) { return node.x }
export function hNodeY_(node) { return node.y }
export function leaves_(tree) { return tree.leaves() }
export function links_(tree) { return tree.links() }
export function path_(from) { return to => tree.path(from, to) }
export function runLayoutFn_(layout) { return root => layout(root) }
export function sharesParent_(a) { return b => a.parent == b.parent }
export function treeSetNodeSize_(tree) { return widthHeight => tree.nodeSize(widthHeight) }
export function treeSetSeparation_(tree) { return separationFn => tree.separation(separationFn) }
export function treeSetSize_(tree) { return widthHeight => tree.size(widthHeight) }
export function treeSortForCirclePack_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.value - a.value
    })
}
export function treeSortForTreeMap_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.height - a.height || b.value - a.value
    })
}
export function treeSortForTree_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.height - a.height || a.id.localeCompare(b.id)
    })
}
export function treeSortForTree_Spago_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      const result =
        b.height - a.height || a.data.name.localeCompare(b.data.name)
      return result
    })
}
export function treeMinMax_(root) {
  let max_x = -(Infinity) // start max with smallest possible number
  let min_x = Infinity    // start min with the largest possible number
  let max_y = -(Infinity)
  let min_y = Infinity
  root.each(d => {
    if (d.x > max_x) max_x = d.x // if we find a value greater than current max, that's our new maximum
    if (d.y > max_y) max_y = d.y

    if (d.x < min_x) min_x = d.x // if we find a value less than current min, that's our new minimum
    if (d.y < min_y) min_y = d.y
    // console.log(`FFI: node ${d} (${min_x}, ${min_y}) (${max_x}, ${max_y})`);
  })
  return { xMin: min_x, xMax: max_x, yMin: min_y, yMax: max_y }
}
export const linkHorizontal_ = d3
  .linkHorizontal()
  .x(d => d.y)
  .y(d => d.x)
export const linkHorizontal2_ = d3
  .linkHorizontal()
  .x(d => d.x)
  .y(d => d.y)
export const linkVertical_ = d3
  .linkVertical()
  .x(d => d.x)
  .y(d => d.y)
export function linkClusterHorizontal_(levelSpacing) {
  return d =>
    `M${d.target.y}, ${d.target.x}
   C${d.source.y + levelSpacing / 2},${d.target.x}
   ${d.source.y + levelSpacing / 2},${d.source.x}
   ${d.source.y},${d.source.x}`
}
export function linkClusterVertical_(levelSpacing) {
  return d =>
    `M${d.target.x}, ${d.target.y}
   C${d.target.x}, ${d.source.y + levelSpacing / 2}
   ${d.source.x},${d.source.y + levelSpacing / 2}
   ${d.source.x},${d.source.y}`
}
export function linkRadial_(angleFn) {
  return radiusFn =>
    d3
      .linkRadial()
      .angle(angleFn)
      .radius(radiusFn)
}
export function autoBox_() {
  document.body.appendChild(this)
  const { x, y, width, height } = this.getBBox()
  document.body.removeChild(this)
  return [x, y, width, height]
}
// *****************************************************************************************************************
// ************************** functions from d3js Chord module         *****************************************
// *****************************************************************************************************************
export function chordLayout_(matrix) {
  return d3.chord()(matrix);
}
export function chordLayoutWithPadAngle_(matrix) {
  return padAngle => d3.chord().padAngle(padAngle)(matrix);
}
export function chordGroups_(chordLayout) { return chordLayout.groups }
export function chordArray_(chordLayout) {
  return Array.from(chordLayout);
}
export function ribbonGenerator_() { return d3.ribbon() }
export function arcGenerator_() { return d3.arc() }
export function ribbonPath_(generator) { return chord => generator(chord) }
export function arcPath_(generator) { return group => generator(group) }
export function setRibbonRadius_(generator) { return radius => { generator.radius(radius); return generator } }
export function setArcInnerRadius_(generator) { return radius => { generator.innerRadius(radius); return generator } }
export function setArcOuterRadius_(generator) { return radius => { generator.outerRadius(radius); return generator } }
// *****************************************************************************************************************
// ************************** functions from d3js Pack (bubble) module         *****************************************
// *****************************************************************************************************************
export function packLayout_() { return d3.pack() }
export function packSetSize_(layout) { return width => height => { layout.size([width, height]); return layout } }
export function packSetPadding_(layout) { return padding => { layout.padding(padding); return layout } }
export function runPackLayout_(layout) { return root => layout(root) }
export function hNodeR_(node) { return node.r }

// *****************************************************************************************************************
// ************************** functions from d3js Treemap module         *****************************************
// *****************************************************************************************************************
export function treemapLayout_() { return d3.treemap() }
export function treemapSetSize_(layout) { return width => height => { layout.size([width, height]); return layout } }
export function treemapSetPadding_(layout) { return padding => { layout.padding(padding); return layout } }
export function runTreemapLayout_(layout) { return root => layout(root) }
// Accessor functions for treemap nodes
export function hNodeX0_(node) { return node.x0 }
export function hNodeY0_(node) { return node.y0 }
export function hNodeX1_(node) { return node.x1 }
export function hNodeY1_(node) { return node.y1 }

// *****************************************************************************************************************
// ************************** functions from d3js Partition (icicle/sunburst) module  ******************************
// *****************************************************************************************************************
export function partitionLayout_() { return d3.partition() }
export function partitionSetSize_(layout) { return width => height => { layout.size([width, height]); return layout } }
export function partitionSetPadding_(layout) { return padding => { layout.padding(padding); return layout } }
export function runPartitionLayout_(layout) { return root => layout(root) }
export function treeSortForPartition_(root) {
  return root
    .sum(function (d) {
      return d.value
    })
    .sort(function (a, b) {
      return b.height - a.height || b.value - a.value
    })
}

// *****************************************************************************************************************
// ************************** functions from d3js zoom module         *****************************************
// *****************************************************************************************************************

export function d3AttachZoomDefaultExtent_(selection) {
  return config => {
    function zoomed({ transform }) {
      config.target.attr('transform', transform)
    }
    // "If extent is not specified, returns the current extent accessor, which
    // defaults to [[0, 0], [width, height]] where width is the client width of the
    // element and height is its client height; for SVG elements, the nearest
    // ancestor SVG element’s viewBox, or width and height attributes, are used.""
    return selection.call(
      d3
        .zoom()
        .scaleExtent(config.scaleExtent)
        .on(`zoom.${config.name}`, zoomed)
    )
  }
}
export function d3AttachZoom_(selection) {
  return config => {
    selection.call(
      d3
        .zoom()
        .extent(config.extent) // extent is [ [], [] ]
        .scaleExtent(config.scaleExtent)
        .on(`zoom.${config.name}`, (event) => { config.target.attr('transform', event.transform) })
    )
    return selection
  }
}
export function showAttachZoomDefaultExtent_(selection) { return config => { return `\t${selection}.call(zoom ${config})` } }
export function showAttachZoom_(selection) {
  return config => {
    return `\t${selection}.call(zoom ${config})`
  }
}

// ==================== Details Panel Helpers ====================

export function showDetailsPanel_(selection) {
  return () => {
    console.log('showDetailsPanel_ called', selection, selection.node())
    selection.classed('hidden', false)
    console.log('Panel should now be visible, class:', selection.attr('class'))
  }
}

export function hideDetailsPanel_(selection) {
  return () => {
    console.log('hideDetailsPanel_ called')
    selection.classed('hidden', true)
  }
}

export function setDetailsModuleName_(selection) {
  return moduleName => () => {
    console.log('setDetailsModuleName_ called', moduleName)
    selection.html(`<h3>${moduleName}</h3>`)
  }
}

export function populateDetailsList_(selection) {
  return items => () => {
    console.log('populateDetailsList_ called', selection.attr('class'), items)
    // Clear existing content
    selection.html('')

    // Add section title based on class
    const classList = selection.attr('class')
    let title = ''
    if (classList.includes('dependencies-list')) {
      title = 'Dependencies:'
    } else if (classList.includes('depended-on-by-list')) {
      title = 'Depended On By:'
    }

    if (title) {
      selection.append('h4').text(title)
    }

    // Add list container
    const ul = selection.append('ul')

    // Add list items
    if (items.length === 0) {
      ul.append('li').text('(none)').classed('empty-item', true)
    } else {
      items.forEach(item => {
        ul.append('li').text(item)
      })
    }
  }
}

export function showModuleLabels_(nodesGroup) {
  return () => {
    console.log('showModuleLabels_ called')
    const labels = nodesGroup.selectAll('.node-label')
    console.log('Found labels:', labels.size())
    labels.attr('fill', '#555')
    // Update y position for each label based on its node's radius
    labels.each(function(d) {
      const expanded = d.expanded || false
      const loc = d.loc || 100
      const baseRadius = (Math.sqrt(loc)) * 0.15 + 2.0  // Match PureScript formula
      const radius = expanded ? baseRadius * 4.0 : baseRadius
      console.log('Setting label y for', d.name, 'to', -radius)
      d3.select(this).attr('y', -radius)
    })
    console.log('showModuleLabels_ done')
  }
}

export function switchToSpotlightForces_(simulation) {
  return () => {
    console.log('Switching to spotlight forces')

    // Get the spotlight forces (they were registered but not active)
    const spotlightCollision = simulation.force('collision-spotlight')
    const spotlightManyBody = simulation.force('manyBody-spotlight')

    console.log('spotlight collision force:', spotlightCollision)
    console.log('spotlight manyBody force:', spotlightManyBody)

    // Remove compact forces
    simulation.force('collision-compact', null)
    simulation.force('manyBody-compact', null)

    // The spotlight forces should already be there, but let's make sure they're active
    // by re-registering them (this is safe - if they're already there, it just updates)
    if (spotlightCollision) {
      simulation.force('collision-spotlight', spotlightCollision)
    }
    if (spotlightManyBody) {
      simulation.force('manyBody-spotlight', spotlightManyBody)
    }

    // Restart the simulation to apply the new forces
    simulation.alpha(0.3).restart()
    console.log('Switched to spotlight forces, restarted simulation')
  }
}

export function switchToCompactForces_(simulation) {
  return () => {
    console.log('Switching back to compact forces')

    // Get the compact forces (they were registered but not active)
    const compactCollision = simulation.force('collision-compact')
    const compactManyBody = simulation.force('manyBody-compact')

    console.log('compact collision force:', compactCollision)
    console.log('compact manyBody force:', compactManyBody)

    // Remove spotlight forces
    simulation.force('collision-spotlight', null)
    simulation.force('manyBody-spotlight', null)

    // The compact forces should already be there, but let's make sure they're active
    if (compactCollision) {
      simulation.force('collision-compact', compactCollision)
    }
    if (compactManyBody) {
      simulation.force('manyBody-compact', compactManyBody)
    }

    // Restart the simulation to apply the new forces
    simulation.alpha(0.3).restart()
    console.log('Switched to compact forces, restarted simulation')
  }
}

export function hideModuleLabels_(nodesGroup) {
  return () => {
    console.log('hideModuleLabels_ called', nodesGroup)
    // nodesGroup is already a D3 selection, don't wrap it again
    const nodeGroups = nodesGroup.selectAll('g.node-group')
    console.log('Found node groups:', nodeGroups.size())
    nodeGroups.selectAll('text.node-label').attr('fill', 'transparent')
    console.log('hideModuleLabels_ done')
  }
}

export function resetNodeFilter_(simulation) {
  return () => {
    console.log('Resetting node filter to show all nodes')

    // Get all nodes from simulation
    const allNodes = simulation.nodes()
    console.log('Total nodes:', allNodes.length)

    // Clear any filtering by restoring all nodes
    simulation.nodes(allNodes)

    // Also need to restore all links
    const linkForce = simulation.force('links')
    if (linkForce && linkForce.links) {
      // Get the original links (they're stored in the force)
      const allLinks = linkForce.links()
      linkForce.links(allLinks)
      console.log('Reset links:', allLinks.length)
    }

    // Restart simulation
    simulation.alpha(0.3).restart()
    console.log('Node filter reset complete')
  }
}

export function restoreAllNodes_(simulation) {
  return nodesGroup => linksGroup => allNodes => allLinks => nodeRadiusFn => keyFn => () => {
    console.log('Restoring all nodes and links to simulation and DOM')
    console.log('Total nodes to restore:', allNodes.length)
    console.log('Total links to restore:', allLinks.length)

    // Restore nodes to simulation
    simulation.nodes(allNodes)

    // Restore links to simulation
    const linkForce = simulation.force('links')
    if (linkForce) {
      linkForce.links(allLinks)
    }

    // Re-render all nodes in the DOM
    const nodeSelection = nodesGroup
      .selectAll('g.node-group')
      .data(allNodes, keyFn)

    // Remove any old nodes
    nodeSelection.exit().remove()

    // Add new nodes (these are the ones that were filtered out)
    const nodeEnter = nodeSelection.enter()
      .append('g')
      .attr('class', 'node-group')

    // Add the main circle for each node
    nodeEnter.append('circle')
      .attr('class', 'node-circle')
      .attr('r', d => nodeRadiusFn(d.expanded)(d.loc))
      .attr('fill', d => d.color || '#999')
      .attr('stroke', '#fff')
      .attr('stroke-width', 1.5)

    // Add label text (initially transparent)
    nodeEnter.append('text')
      .attr('class', 'node-label')
      .attr('text-anchor', 'middle')
      .attr('dy', '.3em')
      .style('font-size', '10px')
      .style('pointer-events', 'none')
      .style('fill', 'transparent')
      .text(d => d.name.split('.').pop())

    // Merge enter and update selections
    const nodeMerge = nodeEnter.merge(nodeSelection)

    // Update positions for all nodes
    nodeMerge
      .attr('transform', d => `translate(${d.x || 0},${d.y || 0})`)

    // Update circle radius in case expansion state changed
    nodeMerge.select('circle.node-circle')
      .attr('r', d => nodeRadiusFn(d.expanded)(d.loc))

    // Re-render all links in the DOM
    const linkSelection = linksGroup
      .selectAll('line')
      .data(allLinks, d => `${keyFn(d.source)}-${keyFn(d.target)}`)

    // Remove old links
    linkSelection.exit().remove()

    // Add new links
    linkSelection.enter()
      .append('line')
      .attr('stroke', '#999')
      .attr('stroke-opacity', 0.6)
      .attr('stroke-width', 1)

    // Restart simulation
    simulation.alpha(0.3).restart()
    console.log('All nodes and links restored')
  }
}

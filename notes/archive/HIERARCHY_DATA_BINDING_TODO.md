# Hierarchy Visualizations Data Binding Issue

## Problem
Current pure PureScript hierarchy visualizations use appendTo in a loop instead of simpleJoin/updateJoin. This means DOM elements don't have __data__ attached, which breaks:
- Reselection (can't select and update existing elements)
- Transitions (D3 transitions need data binding)
- Event handlers that access datum
- General Update Pattern for dynamic data

## Examples
Current pattern in TreeViz, PackViz, etc.:
```purescript
let renderNode :: TreeNode HierData -> m Unit
    renderNode (TreeNode node) = do
      _ <- appendTo rectsGroup Circle [cx node.x, cy node.y, ...]
      traverse_ renderNode node.children
```

This creates DOM elements but doesn't bind data.

## Solution
Use simpleJoin with getAllNodes array:
```purescript
let nodes = getAllNodes treeLayout
circles <- simpleJoin svg Circle nodes keyFn
setAttributes circles [cx (\(TreeNode n) -> n.x), ...]
```

This properly binds each datum to its DOM element via D3's data join.

## Action Required
After phantom type integration, update all hierarchy viz files to use simpleJoin pattern.


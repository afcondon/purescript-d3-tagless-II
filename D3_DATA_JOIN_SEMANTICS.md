# D3 Data Join Semantics - Complete Reference

**Source**: https://bost.ocks.org/mike/join/
**Purpose**: Ensure NativeDOMM interpreter is 100% compatible with D3.js behavior

## Core Concept

The data join binds **data** to **DOM elements** using an optional **key function**.

```javascript
const update = selection.data(data, key);
const enter = update.enter();
const exit = update.exit();
```

## Critical Behaviors

### 1. What `selection.data(data, key)` Returns

**Returns**: The **update selection** (data joined to existing elements)

The enter and exit selections "hang off" the update selection as separate properties.

### 2. Enter/Update/Exit Selection Structure

```
Data: [A, B, C, D, E]
Elements: [X, Y]

Enter:  [A, B, C]  - "Missing elements" (placeholders for new data)
Update: [D, E]      - "Data joined to existing elements" (inner join)
Exit:   [X, Y]      - "Remaining unbound elements" (to be removed)
```

**Key Insight**: Enter selections contain **placeholders**, not real DOM elements yet.

### 3. Parent Group Tracking

**From article**: "When `svg.selectAll("circle")` returns an empty selection, the parent node of this selection is the SVG container."

**Implication**: Even empty selections **preserve their parent context**.

```javascript
// FIRST RUN (no existing circles)
const circles = svg.selectAll("circle");  // Empty, but parent = svg
const update = circles.data([1, 2, 3]);    // Empty update selection
const enter = update.enter();              // 3 placeholders, parent = svg
enter.append("circle");                    // Creates 3 circles under svg!
```

**Critical**: The enter selection remembers the parent from the original `selectAll`, even though that selection was empty!

### 4. How `append()` on Enter Selection Works

**From article**: "Calling `append()` on the enter selection appends a new circle for each data point to the SVG container."

**Behavior**:
1. Enter selection has N placeholders (one per unbound datum)
2. Each placeholder knows its parent group
3. `append("circle")` creates one circle per placeholder
4. Each circle is appended to the corresponding parent
5. Returns a new selection of the created circles

**Example Flow**:
```javascript
// Step 1: Empty selection but parent = svg
const selection = svg.selectAll("circle");  // elements: [], parent: svg

// Step 2: Data join creates enter selection
const update = selection.data([1, 2, 3]);   // update: [], enter: [placeholder, placeholder, placeholder]

// Step 3: Enter selection remembers parent
const enter = update.enter();               // parent: svg, count: 3

// Step 4: Append creates real elements
const circles = enter.append("circle");     // Creates 3 <circle> under svg
```

### 5. Key Matching Algorithm

**Default (no key)**: Match by index
```
Data[0] → Element[0]
Data[1] → Element[1]
...
```

**With key function**: Match by key value
```javascript
data([1, 2, 3], d => d)  // Key is the datum itself

// Example:
Old data: [1, 2, 3] → Elements: [e1, e2, e3]
New data: [2, 3, 4]

// Key matching:
1 (no match) → exit (remove e1)
2 → e2 → update
3 → e3 → update
4 (no match) → enter (create new)
```

**Algorithm** (implicit from D3 behavior):
1. Build map: `oldKey -> element` for existing elements
2. Build map: `newKey -> datum` for new data
3. For each new datum:
   - If key exists in oldMap → **update** (reuse element, rebind data)
   - If key doesn't exist → **enter** (create new element)
4. For each old element:
   - If key doesn't exist in newMap → **exit** (remove element)

### 6. Selection Structure After Join

**Update selection**:
```javascript
{
  _groups: [[elem1, elem2, ...]],  // Matched elements
  _parents: [parentNode],            // Parent group
  _enter: [...],                     // Link to enter selection
  _exit: [...]                       // Link to exit selection
}
```

**Enter selection** (before append):
```javascript
{
  _groups: [[placeholder1, placeholder2, ...]],  // NOT real elements!
  _parents: [parentNode]                          // Same parent as update
}
```

**Enter selection** (after append):
```javascript
{
  _groups: [[newElem1, newElem2, ...]],  // Real DOM elements now
  _parents: [parentNode]
}
```

## Edge Cases

### Empty Initial Selection
```javascript
const sel = svg.selectAll("circle");  // No circles exist
sel.data([1, 2, 3]);
// update: empty, enter: [1,2,3], exit: empty
```

### Empty Data Array
```javascript
const sel = svg.selectAll("circle");  // 5 circles exist
sel.data([]);
// update: empty, enter: empty, exit: [all 5 circles]
```

### No-op Behavior
**From article**: "If a selection is empty, the corresponding code [becomes] a no-op."

```javascript
enter.append("circle")   // If enter is empty, does nothing
exit.remove()            // If exit is empty, does nothing
```

## Implementation Requirements for NativeDOMM

### 1. `openSelection(parent, selector)` Must:
- Query for child elements matching selector
- **If empty**: Return selection with `elements = parent.elements` (preserve parent!)
- **If found**: Return selection with `elements = found children`

**Why**: The parent context must survive even when no children are found.

### 2. `updateJoin(selection, element, data, keyFn)` Must:
- Extract existing elements from selection (might be parents if selection is empty)
- Compute enter/update/exit using key matching
- **Enter selection**: `elements = parent elements`, `boundData = enter data`
- **Update selection**: `elements = matched elements`, `boundData = update data`
- **Exit selection**: `elements = unmatched elements`, `boundData = []`

**Why**: Enter selection needs parent elements so `appendTo` knows where to create new elements.

### 3. `appendTo(selection, element, attrs)` Must:
- For each element in `selection.elements` (the parents)
- Create one new child element
- Bind data from `selection.boundData[i]` to child
- Append child to parent
- Return selection of created children

**Why**: This is how enter selections create real elements from placeholders.

## Current NativeDOMM Implementation Status

✅ **Correct**:
- `computeDataJoin` algorithm matches D3 semantics
- Key serialization to strings works
- Enter/update/exit selections created correctly

✅ **Fixed** (2025-11-12):
- `openSelection` now preserves parent elements when empty
- Enter selection contains parent elements (not empty array)

⚠️ **Potential Issues**:
- `openSelection` always returns empty children (stub implementation)
- Need to actually query DOM and convert NodeList to Array
- Multi-parent selections not tested (D3 supports nested selections)

## Test Cases for Compatibility

### Test 1: First Run (Empty → Data)
```purescript
letterGroup <- appendTo svg Group []
enterSel <- openSelection letterGroup "text"  -- Should have 1 parent element
result <- updateJoin enterSel Text ['A','B','C'] charToKey
-- Expected: enter.boundData = ['A','B','C']
--          enter.elements = [letterGroup element]
--          update/exit both empty

texts <- appendTo result.enter Text []
-- Expected: Creates 3 <text> elements under letterGroup
```

### Test 2: Update Run (Data → Different Data)
```purescript
enterSel <- openSelection letterGroup "text"  -- Should find 3 existing <text>
result <- updateJoin enterSel Text ['B','C','D'] charToKey
-- Expected: enter = ['D'] (new)
--          update = ['B','C'] (matched)
--          exit = ['A'] (removed)
```

### Test 3: Empty Data (Remove All)
```purescript
enterSel <- openSelection letterGroup "text"  -- 3 existing
result <- updateJoin enterSel Text [] charToKey
-- Expected: enter = []
--          update = []
--          exit = [all 3 elements]
```

## References

- **Mike Bostock's Join Article**: https://bost.ocks.org/mike/join/
- **D3 Source**: https://github.com/d3/d3-selection/blob/main/src/selection/data.js
- **Our DataJoin algorithm**: `src/lib/PSD3/Internal/NativeDOM/DataJoin.purs`

## Compatibility Checklist

- [x] Key matching algorithm (Map-based, O(n+m))
- [x] Enter selection preserves parent
- [x] Update selection rebinds data
- [x] Exit selection preserves old elements
- [ ] `openSelection` queries actual DOM (currently stub)
- [ ] Multi-parent selections (nested joins)
- [ ] Null/undefined handling in key function
- [ ] Index-based matching (when no key function provided)

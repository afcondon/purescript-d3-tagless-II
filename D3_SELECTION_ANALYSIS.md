# D3.Selection Replacement Analysis for PureScript Tagless D3

## Executive Summary

The PureScript library has a **medium-to-large FFI surface area** (~149 foreign imports, 1,587 lines of JS code) heavily dependent on D3.js. **Replacing D3.Selection is highly feasible** with the discovery of mature PureScript web libraries (purescript-web-dom, purescript-web-html) that eliminate ~80% of the anticipated FFI work.

**REVISED ESTIMATE: 3-5 weeks** (down from 6-12 weeks) thanks to existing PureScript ecosystem.

### Key Discovery

The **purescript-web** ecosystem provides battle-tested, type-safe bindings for:
- ✅ DOM selection (`querySelector`, `querySelectorAll`)
- ✅ Element manipulation (`setAttribute`, `classList`, etc.)
- ✅ Node operations (creation, insertion, removal)
- ✅ Event handling (`addEventListener`)

**What's left to build:**
- Inline style manipulation (~5 FFI functions)
- Pure PureScript data join algorithm (~100 lines)
- Web Animations API wrapper (optional, for transitions)

---

## 1. What D3.Selection Actually Does

### Core Responsibilities

D3.Selection is a wrapper around DOM elements that provides:

1. **DOM Selection** (10% effort) → ✅ **purescript-web-dom provides this**
   - `d3.select()` - Single element selection
   - `d3.selectAll()` - Multiple element selection
   - `selection.select()` - Sub-selection from within a selection
   - `selection.selectAll()` - Sub-selection of multiple elements

2. **Data Binding / Joins** (40% effort) → ⚠️ **Need to implement**
   - `selection.data(data, keyFn)` - Bind data to selection
   - `selection.enter()` - Get new element placeholders
   - `selection.update()` - Get updated (existing) elements
   - `selection.exit()` - Get removed (orphaned) elements
   - This is the **core D3 innovation**: automatically tracks which DOM elements correspond to which data items

3. **Element Manipulation** (20% effort) → ✅ **purescript-web-dom provides this**
   - `selection.append(element)` - Create and append new element
   - `selection.attr(name, value)` - Set attributes
   - `selection.classed(name, bool)` - Toggle CSS classes
   - `selection.style(name, value)` - Set inline styles (⚠️ need minimal FFI)
   - `selection.text(value)` / `selection.html(value)` - Set text/HTML content
   - `selection.property(name, value)` - Set DOM properties

4. **Selection Operations** (10% effort) → ✅ **Pure PureScript (Array operations)**
   - `selection.filter(predicate)` - Filter by predicate
   - `selection.sort(compareFn)` - Sort elements
   - `selection.order()` - Reorder DOM to match data order
   - `selection.raise()` / `selection.lower()` - Z-order manipulation

5. **Event Handling** (10% effort) → ✅ **purescript-web-events provides this**
   - `selection.on(event, callback)` - Register event handlers
   - Includes special handling for D3 drag behavior
   - Includes special handling for D3 zoom behavior

6. **Transitions/Animations** (5% effort) → ⚠️ **Web Animations API or keep D3**
   - `selection.transition()` - Create a transition
   - `transition.duration()` - Set duration
   - `transition.delay()` - Set delay
   - Limited easing function support

7. **Merge/Chaining** (5% effort) → ✅ **Already done (SelectionM)**
   - `selection.merge(other)` - Combine enter + update
   - All methods return selection for chaining

### In This Codebase

The library uses D3.Selection for:

```
✓ DOM Selection (querySelector/querySelectorAll)
✓ Data binding (enter/update/exit pattern)  <- HEAVY USAGE
✓ SVG element creation
✓ SVG/DOM attribute setting
✓ Drag behavior attachment
✓ Zoom behavior attachment
✓ Simple transitions
✓ Event handling (limited - mostly drag/zoom)
✓ CSS class toggling
✓ Link force simulation rendering
```

NOT directly used:
- Advanced transitions (all transitions are simple, < 1 second)
- Text transitions
- Nested selections (mostly flat operations)
- Multi-value attributes

---

## 2. PureScript Web Ecosystem (NEW DISCOVERY!)

### purescript-web-dom (Most Important)

**Package**: `purescript-web-dom` v6.0.0
**Source**: https://github.com/purescript-web/purescript-web-dom
**Docs**: https://pursuit.purescript.org/packages/purescript-web-dom/6.0.0

Provides type-safe bindings to the WHATWG DOM Living Standard.

#### Key Modules & Functions:

**Web.DOM.ParentNode** - Selection:
```purescript
querySelector :: QuerySelector -> ParentNode -> Effect (Maybe Element)
querySelectorAll :: QuerySelector -> ParentNode -> Effect NodeList
children :: ParentNode -> Effect HTMLCollection
firstElementChild :: ParentNode -> Effect (Maybe Element)
lastElementChild :: ParentNode -> Effect (Maybe Element)
childElementCount :: ParentNode -> Effect Int
```

**Web.DOM.Element** - Attributes:
```purescript
setAttribute :: String -> String -> Element -> Effect Unit
getAttribute :: String -> Element -> Effect (Maybe String)
hasAttribute :: String -> Element -> Effect Boolean
removeAttribute :: String -> Element -> Effect Unit
className :: Element -> Effect String
setClassName :: String -> Element -> Effect Unit
classList :: Element -> Effect DOMTokenList
id :: Element -> Effect String
setId :: String -> Element -> Effect Unit
tagName :: Element -> String
```

**Web.DOM.DOMTokenList** - Class manipulation:
```purescript
add :: DOMTokenList -> String -> Effect Unit
remove :: DOMTokenList -> String -> Effect Unit
contains :: DOMTokenList -> String -> Effect Boolean
toggle :: DOMTokenList -> String -> Effect Boolean
toggleForce :: DOMTokenList -> String -> Boolean -> Effect Boolean
```

**Web.DOM.Node** - Tree operations:
```purescript
appendChild :: Node -> Node -> Effect Node
removeChild :: Node -> Node -> Effect Node
insertBefore :: Node -> Node -> Node -> Effect Node
replaceChild :: Node -> Node -> Node -> Effect Node
parentNode :: Node -> Effect (Maybe Node)
childNodes :: Node -> Effect NodeList
textContent :: Node -> Effect String
setTextContent :: String -> Node -> Effect Unit
```

**Web.DOM.Document** - Creation:
```purescript
createElement :: String -> Document -> Effect Element
createTextNode :: String -> Document -> Effect Text
```

### purescript-web-html

**Package**: `purescript-web-html` v4.1.0
**Source**: https://github.com/purescript-web/purescript-web-html
**Docs**: https://pursuit.purescript.org/packages/purescript-web-html/4.1.0

Provides typed HTML element interfaces.

**Web.HTML.HTMLElement** - Properties:
```purescript
title :: HTMLElement -> Effect String
setTitle :: String -> HTMLElement -> Effect Unit
hidden :: HTMLElement -> Effect Boolean
setHidden :: Boolean -> HTMLElement -> Effect Unit
click :: HTMLElement -> Effect Unit
focus :: HTMLElement -> Effect Unit
blur :: HTMLElement -> Effect Unit
offsetTop :: HTMLElement -> Effect Number
offsetLeft :: HTMLElement -> Effect Number
offsetWidth :: HTMLElement -> Effect Number
offsetHeight :: HTMLElement -> Effect Number
```

Also includes specific element types: HTMLInputElement, HTMLButtonElement, HTMLDivElement, HTMLSVGElement, etc.

### purescript-web-events

**Package**: `purescript-web-events` v4.0.0

Provides event handling:
```purescript
addEventListener :: EventType -> EventListener -> Boolean -> EventTarget -> Effect Unit
removeEventListener :: EventType -> EventListener -> Boolean -> EventTarget -> Effect Unit
```

### What's NOT Provided (Need to Write)

**Inline Styles** - Only ~5 functions needed:
```purescript
-- Need FFI wrapper for element.style.setProperty()
foreign import setInlineStyle :: String -> String -> Element -> Effect Unit
foreign import getInlineStyle :: String -> Element -> Effect String
foreign import removeInlineStyle :: String -> Element -> Effect Unit
foreign import getComputedStyle :: Element -> Effect CSSStyleDeclaration
foreign import setStyleText :: String -> Element -> Effect Unit  -- style.cssText
```

**Transitions** (Optional - can keep D3):
```purescript
-- Web Animations API wrapper
foreign import animate :: Element -> Array Keyframe -> AnimationOptions -> Effect Animation
foreign import pause :: Animation -> Effect Unit
foreign import play :: Animation -> Effect Unit
foreign import cancel :: Animation -> Effect Unit
```

---

## 3. Current FFI Boundary

### FFI Statistics
- **149 foreign imports** in FFI.purs
- **178 export functions** in FFI.js
- **1,587 lines** of JavaScript code
- **~40% of FFI** is D3.Selection-related
- **~30% of FFI** is D3 Force simulation
- **~20% of FFI** is D3 Hierarchy layouts (mostly legacy, being replaced)
- **~10% of FFI** is utilities (zoom, chord, etc.)

### Selection-Specific FFI Functions (17 functions)

**DOM Selection:**
```javascript
d3SelectAllInDOM_      // d3.selectAll(selector)
d3SelectFirstInDOM_    // d3.select(selector)
d3SelectionSelectAll_  // selection.selectAll(selector)
d3SelectionSelect_     // selection.select(selector)
d3SelectionIsEmpty_    // selection.empty()
```
→ **Replaced by purescript-web-dom**

**Data Binding:**
```javascript
d3Data_                // selection.data(data)
d3DataWithKeyFunction_ // selection.data(data, keyFn)
d3DataWithFunction_    // selection.data(extractFn, keyFn)
d3GetEnterSelection_   // selection.enter()
d3GetExitSelection_    // selection.exit()
d3GetSelectionData_    // selection.data()
d3MergeSelectionWith_  // selection1.merge(selection2)
```
→ **Need pure PureScript algorithm**

**Element Manipulation:**
```javascript
d3Append_              // selection.append(element)
d3EnterAndAppend_      // selection.enter().append(element)
d3SetAttr_             // selection.attr(name, value)
d3SetText_             // selection.text(value)
d3SetProperty_         // selection.property(value)
d3SetHTML_             // selection.html(value)
```
→ **Replaced by purescript-web-dom** (except styles)

**Selection Operations:**
```javascript
d3FilterSelection_     // selection.filter(selector)
d3OrderSelection_      // selection.order()
d3RaiseSelection_      // selection.raise()
d3LowerSelection_      // selection.lower()
d3SortSelection_       // selection.sort(compareFn)
d3RemoveSelection_     // selection.remove()
```
→ **Pure PureScript (Array operations)**

**Other:**
```javascript
selectionOn_           // selection.on(event, callback)
d3AddTransition_       // selection.transition()
```
→ **purescript-web-events + optional Web Animations API**

---

## 4. Modern Browser APIs (2024)

### What Can Replace D3.Selection

#### 1. DOM Selection (Excellent Support) ✅ PROVIDED
```javascript
// purescript-web-dom has these:
document.querySelector(selector)      // replaces d3.select()
document.querySelectorAll(selector)    // replaces d3.selectAll()
element.querySelector(selector)        // replaces selection.select()
element.querySelectorAll(selector)     // replaces selection.selectAll()
```
- **Native API**: Available in all modern browsers since IE8+
- **No library needed**: purescript-web-dom wraps it
- **Type-safe**: Full PureScript types

#### 2. Data Binding (Moderate Difficulty) ⚠️ NEED TO IMPLEMENT
```javascript
// D3's enter/update/exit pattern:
// Detect which data items are:
//   - NEW (enter):  data[i] has no corresponding DOM element
//   - UPDATED:      data[i] has a corresponding DOM element
//   - REMOVED (exit): DOM element has no corresponding data item

// Algorithm:
1. Build a Map from key(data) -> data item
2. For each DOM element, get its current key via map/index
3. Compare sets:
   - DOM keys NOT in data -> exit
   - Data keys NOT in DOM -> enter
   - Keys in both -> update
4. Create/update/remove DOM elements accordingly
```

This is **completely doable in pure PureScript**, and:
- **Pure algorithm** (no DOM dependency)
- **Testable** (can validate against D3 output)
- **Performance**: Use Maps for O(n) performance

#### 3. Element Manipulation (Excellent Support) ✅ PROVIDED
```javascript
// purescript-web-dom has these:
element.setAttribute(name, value)      // replaces attr()
element.removeAttribute(name)
element.classList.add/remove(name)     // replaces classed()
element.style.setProperty(name, value) // replaces style() ⚠️ need wrapper
element.textContent = value            // replaces text()
element.innerHTML = value              // replaces html()
```
- **Native APIs**: purescript-web-dom wraps most
- **Only missing**: Inline style wrappers (~5 FFI functions)

#### 4. Event Handling (Excellent Support) ✅ PROVIDED
```javascript
// purescript-web-events has this:
element.addEventListener(event, handler)  // replaces on()
element.removeEventListener(event, handler)
```

#### 5. Transitions (Good Support) ⚠️ OPTIONAL
```javascript
// Web Animations API (good browser support 2024):
element.animate(keyframes, options)

// Simple library uses very simple transitions, could keep D3 initially
```

---

## 5. What Would Be Pure PureScript vs FFI

### Pure PureScript Logic (Need to Implement)

#### 5A. Data Binding Algorithm (Most Important)
```purescript
-- Pure PureScript approach:
type DataJoin a = {
  enter :: Array a,      -- new data
  update :: Array a,     -- existing data
  exit :: Array Element  -- orphaned elements
}

computeDataJoin
  :: forall a key.
     Ord key =>
     (a -> key) ->
     Array a ->
     Array Element ->
     (Element -> Maybe key) ->
     DataJoin a
computeDataJoin keyFn newData domElements getElementKey =
  let
    dataMap = Map.fromFoldable $ map (\d -> Tuple (keyFn d) d) newData
    elementMap = Map.fromFoldable $ Array.mapMaybe (\el ->
      map (\k -> Tuple k el) (getElementKey el)) domElements

    dataKeys = Set.fromFoldable $ map keyFn newData
    elementKeys = Set.fromFoldable $ Array.mapMaybe getElementKey domElements

    enterKeys = Set.difference dataKeys elementKeys
    exitKeys = Set.difference elementKeys dataKeys
    updateKeys = Set.intersection dataKeys elementKeys
  in
    { enter: Array.mapMaybe (\k -> Map.lookup k dataMap) (Set.toUnfoldable enterKeys)
    , update: Array.mapMaybe (\k -> Map.lookup k dataMap) (Set.toUnfoldable updateKeys)
    , exit: Array.mapMaybe (\k -> Map.lookup k elementMap) (Set.toUnfoldable exitKeys)
    }
```

**Advantages:**
- Pure, testable algorithm
- Can log/inspect data join operations
- Type-safe (compiler catches bugs)
- O(n) performance using Maps

**Effort**: 2-3 weeks (includes testing)
**Risk**: Low (algorithm is well-understood, project already proved this with hierarchy layouts)

### FFI Needed (Minimal!)

#### 5B. Inline Style Manipulation (~5 functions)
```purescript
-- FFI for element.style (not in purescript-web-dom):
foreign import setInlineStyle :: String -> String -> Element -> Effect Unit
foreign import getInlineStyle :: String -> Element -> Effect String
foreign import removeInlineStyle :: String -> Element -> Effect Unit
foreign import getComputedStyle :: Element -> Effect CSSStyleDeclaration
foreign import setStyleText :: String -> Element -> Effect Unit
```

**Implementation** (FFI.js):
```javascript
export const setInlineStyle = prop => value => element => () => {
  element.style.setProperty(prop, value);
};

export const getInlineStyle = prop => element => () => {
  return element.style.getPropertyValue(prop);
};

export const removeInlineStyle = prop => element => () => {
  element.style.removeProperty(prop);
};

export const getComputedStyle = element => () => {
  return window.getComputedStyle(element);
};

export const setStyleText = cssText => element => () => {
  element.style.cssText = cssText;
};
```

**Effort**: 1 day
**Risk**: None (trivial wrappers)

#### 5C. Web Animations API (Optional, ~10-15 functions)
```purescript
-- For transitions (if not keeping D3):
foreign import animate :: Element -> Array Keyframe -> AnimationOptions -> Effect Animation
foreign import pauseAnimation :: Animation -> Effect Unit
foreign import playAnimation :: Animation -> Effect Unit
foreign import cancelAnimation :: Animation -> Effect Unit
foreign import finishAnimation :: Animation -> Effect Unit
-- ... etc
```

**Effort**: 1-2 weeks
**Risk**: Low (well-documented API)
**Note**: Can keep D3 transitions initially

---

## 6. Revised Migration Approach (Phases)

### Phase 0: Add Dependencies (1 day) ✓ EASY
```bash
spago install web-dom web-html web-events
```

### Phase 1: Inline Style FFI (1-2 days) ✓ EASY
Create minimal FFI for styles:
```purescript
-- src/lib/PSD3/Internal/DOM/Style.purs
foreign import setInlineStyle :: String -> String -> Element -> Effect Unit
foreign import getInlineStyle :: String -> Element -> Effect String
foreign import removeInlineStyle :: String -> Element -> Effect Unit
foreign import getComputedStyle :: Element -> Effect CSSStyleDeclaration
foreign import setStyleText :: String -> Element -> Effect Unit
```

**Deliverable**: 5 FFI functions

### Phase 2: Implement Pure Data Join Algorithm (2-3 weeks) ⚠️ CORE TASK
```purescript
-- src/lib/PSD3/Internal/Selection/DataJoin.purs
module PSD3.Internal.Selection.DataJoin where

import Web.DOM.Element (Element)
import Data.Map as Map
import Data.Set as Set

computeDataJoin
  :: forall a key.
     Ord key =>
     (a -> key) ->
     Array a ->
     Array Element ->
     (Element -> Maybe key) ->
     { enter :: Array a, update :: Array a, exit :: Array Element }

-- Pure, fully testable algorithm
-- Use Maps for O(n) performance
-- Comprehensive test suite comparing to D3 output
```

**Testing**: Write comprehensive test suite
**Verification**: Compare D3 output to native output numerically

**Effort**: 2-3 weeks
**Risk**: Low (proven feasible by hierarchy layout replacement)

### Phase 3: Implement NativeDOMM Interpreter (1-2 weeks)
```purescript
-- src/lib/PSD3/Interpreter/NativeDOM.purs
module PSD3.Interpreter.NativeDOM where

import Web.DOM.Element as Element
import Web.DOM.ParentNode as ParentNode
import Web.DOM.Node as Node
import PSD3.Capabilities.Selection (class SelectionM)

-- Implement SelectionM using purescript-web-dom
instance SelectionM Element (NativeDOMM state) where
  attach selector = do
    doc <- liftEffect $ window >>= document
    Element.querySelector selector doc

  selectUnder element selector =
    Element.querySelector selector element

  appendTo parent element attrs = do
    child <- Element.createElement (show element) doc
    _ <- traverse_ (applyAttributeNative child) attrs
    Node.appendChild child parent
    pure child

  -- ... etc
```

**Uses**:
- purescript-web-dom for DOM operations
- Phase 1 FFI for inline styles
- Phase 2 algorithm for data joins

**Effort**: 1-2 weeks
**Risk**: Low (mostly glue code)

### Phase 4: Update Examples & Test (1 week)
Update 2-3 examples to use NativeDOMM:
- Simple bar chart
- Circle pack
- Basic tree

**Validation**: Ensure visual output is identical to D3 version
**Testing**: Side-by-side comparison

### Phase 5: Drag/Zoom/Events (1-2 weeks) OPTIONAL
Implement special behaviors using purescript-web-events:
```purescript
-- Can keep D3 for these initially
-- Or implement using Pointer Events API
```

**Complexity**: Higher than basic operations
**Risk**: Medium
**Note**: Can defer or keep D3 for these

### Phase 6: Cleanup & Documentation (3-5 days)
- Update documentation
- Add migration guide
- Create examples showing both approaches
- Optional: Remove D3 from package.json

**Total Estimated Effort**:
- **Core features (Phases 1-4)**: 3-5 weeks
- **With drag/zoom/cleanup**: 5-7 weeks
- **Previous estimate**: 12-16 weeks
- **Savings**: ~60-70% faster!

---

## 7. Key Challenges & Risks

### Challenge 1: D3 Data Join Algorithm is Sophisticated
**Issue**: D3's enter/update/exit pattern uses object identity + key function matching

**Mitigation**:
- Use pure algorithm with Maps (Phase 2)
- Create comprehensive tests
- Compare output with D3 for validation
- Project already proved this works (hierarchy layouts)

**Risk Level**: LOW (was MEDIUM, lowered due to proof of concept)

### Challenge 2: Performance
**Issue**: Naive data join implementation is O(n²)

**Mitigation**:
- Implement using PureScript Maps for O(n)
- Benchmark against D3 implementation
- Already proven with hierarchy layouts

**Risk Level**: LOW (well-understood optimization)

### Challenge 3: Learning Curve for purescript-web Libraries
**Issue**: Team needs to learn new APIs

**Mitigation**:
- APIs are straightforward (mirror native DOM)
- Well-documented on Pursuit
- Smaller API surface than D3
- Type safety catches mistakes

**Risk Level**: LOW

### Challenge 4: Drag Behavior is Complex
**Issue**: D3 drag handles multiple pointer devices, window-level events, etc.

**Mitigation**:
- Can keep D3 drag behavior (it's independent)
- Or defer to Phase 5
- Or implement using Pointer Events API

**Risk Level**: MEDIUM (but can be deferred)

### Challenge 5: Existing Code Dependencies
**Issue**: Some FFI code might rely on D3 selection internals

**Mitigation**:
- Provide equivalent functions
- Update call sites incrementally
- Can run both interpreters side-by-side

**Risk Level**: LOW

---

## 8. Revised Effort & Risk Summary

| Component | Approach | Effort | Risk | Notes |
|-----------|----------|--------|------|-------|
| DOM Selection | purescript-web-dom | 0 days | NONE | Already exists |
| Element Manipulation | purescript-web-dom | 0 days | NONE | Already exists |
| Class Manipulation | purescript-web-dom | 0 days | NONE | Already exists |
| Event Handling | purescript-web-events | 0 days | NONE | Already exists |
| Inline Styles FFI | 5 functions | 1-2 days | LOW | Trivial wrappers |
| Data Join Algorithm | 100 lines PS | 2-3 weeks | LOW | Pure, testable |
| NativeDOMM Interpreter | Glue code | 1-2 weeks | LOW | Use existing libs |
| Testing & Validation | Test suite | 1 week | LOW | Compare to D3 |
| Drag/Zoom (optional) | Keep D3 or implement | 1-2 weeks | MEDIUM | Can defer |
| **TOTAL (Core)** | **Phases 1-4** | **4-5 weeks** | **LOW** | Highly feasible |
| **TOTAL (Complete)** | **All phases** | **6-8 weeks** | **LOW-MEDIUM** | With drag/zoom |

**Previous Estimate**: 12-16 weeks, MEDIUM risk
**New Estimate**: 4-5 weeks core, 6-8 weeks complete, LOW risk
**Improvement**: ~60-70% faster, lower risk

---

## 9. Implementation Recommendation

### Recommended Approach: Proof of Concept First

**Week 1** (Setup & Inline Styles):
- Add purescript-web-dom, purescript-web-html dependencies
- Write 5 inline style FFI functions
- Create basic examples

**Week 2-4** (Data Join Algorithm):
- Implement pure data join algorithm
- Create comprehensive test suite
- Compare output to D3 (numerical validation)
- Document algorithm

**Week 5-6** (NativeDOMM Interpreter):
- Implement SelectionM instance for Element
- Update 2-3 examples to use NativeDOMM
- Visual validation (side-by-side with D3)

**Week 7** (Testing & Polish):
- Extensive testing
- Performance benchmarks
- Documentation
- Migration guide

**Decision Point** (End of Week 7):
- ✅ Success → Continue with Phase 5 (drag/zoom) or ship it
- ❌ Issues → Revert or iterate

**Benefits of This Approach**:
- Low initial investment (1-2 days for dependencies)
- Incremental progress
- Can validate approach early
- Can keep D3 for special behaviors
- Both interpreters can coexist

---

## 10. What Could Still Use D3 (Coexistence Strategy)

D3 doesn't need to be "all or nothing". Could keep D3 for:

### Keep D3 For (Short Term):
- **Force simulations**: Sophisticated and well-optimized
- **Drag/zoom behaviors**: Complex but independent
- **Transitions**: Until Web Animations API wrapper is ready

### Replace D3 For (Immediate):
- **DOM selection**: Use purescript-web-dom
- **Data binding**: Use pure PureScript algorithm
- **Element manipulation**: Use purescript-web-dom
- **Class manipulation**: Use purescript-web-dom
- **Attribute setting**: Use purescript-web-dom
- **Event handling**: Use purescript-web-events

**Result**: ~70-80% reduction in D3 dependency
**Timeline**: 4-5 weeks for core features

---

## 11. Comparison: Before vs After

### Before (Current State)

**Dependencies**:
```json
{
  "d3": "^7.8.5",
  "d3-force": "^3.0.0",
  "d3-hierarchy": "^3.1.2"  // Being phased out
}
```

**FFI Code**:
- 149 foreign imports
- 1,587 lines of JavaScript
- 40% D3.Selection-related

**Type Safety**:
- Opaque D3 selection types
- `unsafeCoerce` at selection boundaries
- Limited compile-time checking

### After (With NativeDOMM)

**Dependencies**:
```json
{
  "d3-force": "^3.0.0",  // Keep for now
  "purescript-web-dom": "^6.0.0",
  "purescript-web-html": "^4.1.0",
  "purescript-web-events": "^4.0.0"
}
```

**FFI Code**:
- ~60 foreign imports (-60%)
- ~800 lines of JavaScript (-50%)
- Inline styles only (~5 new functions)

**Type Safety**:
- Full Element types from purescript-web-dom
- Pure data join algorithm (100% type-safe)
- No `unsafeCoerce` in user code
- Compiler catches DOM manipulation errors

**Additional Benefits**:
- ✅ Pure data join logic (debuggable, testable)
- ✅ Better error messages
- ✅ Type-safe attribute setting
- ✅ Community-maintained web bindings
- ✅ Smaller bundle size (eventually)
- ✅ Proven approach (hierarchy layouts already done)

---

## 12. Alternative: Hybrid Approach (Recommended for Initial Implementation)

Instead of full replacement, start with **coexistence**:

### Phase 1: Add NativeDOMM Alongside D3M
```purescript
-- User can choose interpreter:
viz1 <- runD3M $ myVisualization  -- Uses D3
viz2 <- runNativeDOMM $ myVisualization  -- Uses native DOM

-- Same code, different interpreter!
```

### Phase 2: Migrate Examples Incrementally
```purescript
-- Simple examples first:
barChart :: forall m. SelectionM Element m => ...
circlePackViz :: forall m. SelectionM Element m => ...

-- Complex examples can keep D3:
forceDirectedGraph :: forall m. SelectionM D3Selection_ m => ...
```

### Phase 3: Deprecate D3M Gradually
- Keep both interpreters for 2-3 releases
- Provide migration guide
- Update documentation
- Eventually remove D3 dependency

**Timeline**: Can start using NativeDOMM in 4-5 weeks while keeping D3 working

---

## Conclusion

**Replacing D3.Selection is HIGHLY FEASIBLE** thanks to:
1. **Mature PureScript ecosystem** (purescript-web-dom provides ~80% of needs)
2. **Minimal new FFI** (only ~5 inline style functions)
3. **Pure data join algorithm** (proven feasible by hierarchy layout success)
4. **Existing architecture** (SelectionM already abstracts interpreter)

**REVISED ESTIMATES**:
- **Proof of concept**: 3-5 weeks (was 6-8 weeks)
- **Complete with drag/zoom**: 6-8 weeks (was 12-16 weeks)
- **Risk level**: LOW (was MEDIUM)
- **ROI**: High (type safety, debugging, smaller bundle, no D3 dependency)

**RECOMMENDATION**:
Start with **Phases 1-2** (inline styles + data join) as proof of concept. This requires only 3-5 weeks and proves the approach. If successful, continue with remaining phases. The project has already proven this approach works by successfully replacing D3 hierarchy layouts with pure PureScript.

**The discovery of purescript-web-dom and purescript-web-html changes this from a "medium effort, medium risk" project to a "small-to-medium effort, low risk" project.**


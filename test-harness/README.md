# D3 Force Simulation Test Harness

## Purpose

These test harnesses provide **definitive answers** about D3's force simulation behavior through direct JavaScript testing. They were created to understand the exact behavior needed for PureScript integration, particularly around:

- Force configuration mutability
- General Update Pattern (enter/exit) interactions
- Node object mutation
- Force handle persistence
- State management during scene transitions

## Test Files

### 1. `d3-simulation-test.html`
**Comprehensive D3 Behavior Tests (11 Test Suites)**

Tests fundamental D3 simulation behaviors:

1. **Simulation Creation and State** - Is simulation mutable? What do methods return?
2. **Node Object Mutation** - Are nodes mutated in place or copied?
3. **Force Configuration Mutability** - Are force handles mutable references?
4. **Force Removal and Re-addition** - What happens when forces are removed/replaced?
5. **General Update Pattern** - How does `simulation.nodes()` handle enter/exit?
6. **Force Parameters** - Static values vs. dynamic functions
7. **Simulation Restart and Alpha** - How does restart() affect state?
8. **Tick Callbacks** - When and how are tick handlers called?
9. **Coordinate Systems** - How do fixed positions (fx, fy) work?
10. **Force Initialization Timing** - When are forces initialized?
11. **Memory and Reference Leaks** - Do old references persist?

### 2. `d3-gup-forces-test.html`
**GUP + Forces Integration Tests (5 Scenarios)**

Tests the specific interaction patterns needed for PureScript:

1. **Basic GUP** - Node enter/exit without force changes
2. **GUP with Force Change** - The core PureScript use case
3. **Rapid Changes** - Multiple scene transitions
4. **Old Forces Persisting** - The suspected bug: do removed force handles still affect the simulation?
5. **PureScript Pattern Simulation** - Exact sequence: old system → V2 config → GUP → run

## How to Use

### Opening the Tests

1. **Via File System:**
   ```bash
   open test-harness/d3-simulation-test.html
   open test-harness/d3-gup-forces-test.html
   ```

2. **Via Local Server (if needed):**
   ```bash
   cd test-harness
   python3 -m http.server 8081
   # Then open: http://localhost:8081/d3-simulation-test.html
   # And: http://localhost:8081/d3-gup-forces-test.html
   ```

### Reading the Results

- **Green (PASS)**: Behavior matches expectations
- **Red (FAIL)**: Unexpected behavior found
- **Orange (WARN)**: Notable behavior that may or may not be problematic
- **Red background (CRITICAL)**: Serious issue that explains bugs

### What to Look For

#### Key Questions to Answer:

1. **Are force handles mutable?**
   - If you keep a reference to a force and modify it, does it affect the simulation?
   - See: Test 3 in d3-simulation-test.html

2. **Do removed forces persist?**
   - After `simulation.force('name', null)`, can old handles still affect behavior?
   - See: Scenario 4 in d3-gup-forces-test.html
   - **This is the suspected root cause of the CodeExplorerV2 issues**

3. **How does `simulation.nodes()` work with GUP?**
   - Does it preserve object references for existing nodes?
   - Does it properly initialize new nodes?
   - See: Test 5 and Scenario 1

4. **What's the proper sequence for scene transitions?**
   - Should forces be removed before or after GUP updates?
   - Does order matter?
   - See: Scenarios 2, 3, 5

5. **Do old force configurations interfere with new ones?**
   - This is the core question for the V2 system
   - See: All scenarios in d3-gup-forces-test.html

## Expected Findings

Based on these tests, we expect to discover:

### Hypothesis 1: Force Handle Mutation
**Expectation:** Force handles are mutable, and modifying a handle after removal might still affect the simulation.

**Test:** Scenario 4 - Old Forces Persisting

**Impact:** If true, we need to ensure all force references are fully detached before scene transitions.

### Hypothesis 2: GUP Node Reference Preservation
**Expectation:** `simulation.nodes()` preserves object references for nodes that remain in the data array.

**Test:** Test 5 and Scenario 1

**Impact:** If false, our object identity tracking in PureScript is broken.

### Hypothesis 3: Force Initialization Timing
**Expectation:** Forces need to be configured before `simulation.restart()` or `simulation.alpha()` to take effect.

**Test:** Test 10

**Impact:** Affects when we should call `applySceneConfig`.

### Hypothesis 4: Alpha Management
**Expectation:** Changing forces doesn't automatically reset alpha - we need to manually reheat.

**Test:** Test 7 and Scenario 2

**Impact:** Explains why nodes might not move after force changes.

## Interpreting Results for PureScript

### If Tests Show Force Handles Persist:
- We need to ensure `applySceneConfig` not only removes forces but also nullifies any stored references
- The old `Forces.purs` system with stored handles is fundamentally problematic
- V2 approach (no stored references) is correct

### If Tests Show Proper Cleanup:
- The issue is in our PureScript code, not D3
- Look for: improper initialization order, missing alpha management, incorrect node updates

### If Tests Show GUP Issues:
- We may need to change how we call `simulation.nodes()`
- Object references might not be preserved as expected
- Initialization of new nodes might be incomplete

## Next Steps After Running Tests

1. **Document findings** in this README
2. **Update PureScript code** based on discovered behaviors
3. **Re-test CodeExplorerV2** with fixes applied
4. **Consider instrumenting D3 source** if black-box testing is insufficient

## Adding More Tests

To add tests, edit the HTML files and add new test functions following the existing patterns:

```javascript
function testN_YourTestName() {
  const section = logSection('TEST N: Description');

  // Your test code

  logResult(section, 'Result message', 'pass' | 'fail' | 'info' | 'warn');

  return { /* any data to preserve */ };
}
```

Then call it at the bottom with the other test functions.

## Browser Console

Both test files log additional information to the browser console. Open DevTools (F12) to see:
- Detailed object inspection
- Force handle references
- Node state snapshots
- Timing information

## Questions These Tests Answer

- ✅ What happens when you remove a force?
- ✅ Can old force handles affect the simulation after removal?
- ✅ How does D3 handle node enter/exit?
- ✅ Are node objects mutated in place?
- ✅ When should forces be configured relative to GUP updates?
- ✅ How does alpha management work?
- ✅ What's the proper way to transition between force configurations?
- ✅ Do force parameters (static vs function) behave differently?

---

**Last Updated:** 2025-01-27
**Purpose:** Understanding D3 force simulation for PureScript PSD3 V2 integration
**Status:** Tests complete, awaiting results analysis

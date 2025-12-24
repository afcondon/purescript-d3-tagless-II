# Halogen Emmet Notation - Design Plan

**Status:** Future Work (Not for Current Implementation)
**Created:** 2025-12-24
**Context:** Written immediately after implementing opaque attributes + round-trip for D3/PSD3

## Executive Summary

A concise, declarative syntax for Halogen UI structures that enables:
- Rapid prototyping of component hierarchies
- Visual editing of application structure (move sections, reorganize layouts)
- Round-trip conversion (Halogen AST ↔ Emmet+Metadata ↔ Halogen AST)
- Teaching tool for Halogen patterns
- Export/import of UI templates

**Core Insight:** The opaque attribute system we just built for PSD3 proves this is feasible. Event handlers, slots, state management, and component logic can be preserved as metadata while structure is edited visually.

## Motivation

### Current State: Halogen is Verbose

Building Halogen UIs requires significant boilerplate:

```purescript
-- A simple form with validation
HH.form
  [ HP.classes [ ClassName "user-form" ]
  , HE.onSubmit HandleSubmit
  ]
  [ HH.div
      [ HP.classes [ ClassName "form-group" ] ]
      [ HH.label
          [ HP.for "email" ]
          [ HH.text "Email:" ]
      , HH.input
          [ HP.type_ InputEmail
          , HP.id "email"
          , HP.value state.email
          , HE.onValueInput UpdateEmail
          , HP.classes [ ClassName "form-control" ]
          ]
      ]
  , HH.div
      [ HP.classes [ ClassName "form-group" ] ]
      [ HH.label
          [ HP.for "password" ]
          [ HH.text "Password:" ]
      , HH.input
          [ HP.type_ InputPassword
          , HP.id "password"
          , HP.value state.password
          , HE.onValueInput UpdatePassword
          , HP.classes [ ClassName "form-control" ]
          ]
      ]
  , HH.button
      [ HP.type_ ButtonSubmit
      , HP.classes [ ClassName "btn", ClassName "btn-primary" ]
      ]
      [ HH.text "Submit" ]
  ]
```

**~45 lines** for a 2-field form.

### Proposed: Halogen Emmet

```
form.user-form[onSubmit:HANDLER]
>div.form-group
  >(label[for=email]{Email:}+input:email#email.form-control[value:STATE,onInput:HANDLER])
 +div.form-group
  >(label[for=password]{Password:}+input:password#password.form-control[value:STATE,onInput:HANDLER])
 +button:submit.btn.btn-primary{Submit}
```

**~6 lines** with identical structure. Event handlers and state bindings preserved as opaque metadata.

### Why This Matters

1. **Rapid Prototyping**: Sketch UIs quickly, add logic later
2. **Visual Editing**: Drag-and-drop component reorganization in a visual editor
3. **Template Libraries**: Share UI patterns as concise snippets
4. **Learning Tool**: See structure without drowning in syntax
5. **AI-Friendly**: Easier for LLMs to generate/modify Halogen code

## Design Philosophy (Learned from PSD3)

### 1. Keep Notation Simple

**Cover the 80% case:**
- HTML element types (div, span, button, input, etc.)
- Static attributes (id, type, placeholder)
- CSS classes and IDs (shorthand notation)
- Text content
- Structure (nesting, siblings)

**Omit from notation:**
- Event handlers → `onEvent:HANDLER` (opaque)
- Component slots → `@SlotName` (opaque)
- State bindings → `value:STATE` (opaque)
- Conditional rendering → `?CONDITIONAL` (opaque)
- Dynamic classes → `class:COMPUTED` (opaque)

### 2. Preserve Computation as Metadata

Just like PSD3, complex PureScript code (event handlers, slots, state management) is stored as metadata indexed by node path. Visual editors can restructure the tree without losing behavior.

### 3. Round-Trip Guarantee

```purescript
-- Start with Halogen component
let component = HH.div [...complex handlers and slots...]

-- Convert to Emmet + metadata
let emmetData = toEmmetWithMetadata component

-- Edit structure (e.g., wrap in container)
let edited = "div.container>" <> emmetData.emmetString

-- Convert back with metadata substitution
let newComponent = fromEmmetWithMetadata { emmetData | emmetString = edited }

-- Result: Same handlers/slots, new structure!
```

## Proposed Syntax

### Element Types

Use standard HTML element names:

```
div, span, p, h1, h2, h3, h4, h5, h6
a, button, input, textarea, select, option
form, label, fieldset, legend
ul, ol, li, dl, dt, dd
table, thead, tbody, tfoot, tr, th, td
header, footer, nav, main, section, article, aside
img, svg, canvas
```

### Class and ID Shorthand (Emmet Standard)

```
div.container                    → <div class="container">
div#app                          → <div id="app">
div.row.justify-center           → <div class="row justify-center">
button.btn.btn-primary           → <button class="btn btn-primary">
input#email.form-control         → <input id="email" class="form-control">
```

### Attributes

**Static Attributes:**
```
input[type=email,placeholder=Enter email]
a[href=https://example.com,target=_blank]
img[src=/logo.png,alt=Logo]
```

**State Bindings (Opaque):**
```
input[value:STATE]               → Bind to component state
input[checked:STATE]             → Checkbox/radio state
select[value:STATE]              → Select state
```

**Event Handlers (Opaque):**
```
button[onClick:HANDLER]          → Click handler
input[onInput:HANDLER]           → Input handler
form[onSubmit:HANDLER]           → Form submission
div[onMouseEnter:HANDLER]        → Mouse events
```

**Computed Attributes (Opaque):**
```
div[class:COMPUTED]              → Dynamic classes
button[disabled:COMPUTED]        → Conditional disabled state
a[href:COMPUTED]                 → Computed URL
```

### Text Content

```
p{Hello, world!}                 → <p>Hello, world!</p>
button{Click me}                 → <button>Click me</button>
h1{Welcome to the App}           → <h1>Welcome to the App</h1>
```

### Component Slots (Opaque)

```
div>@Header+main>@Content+@Footer
```

This represents:
```purescript
HH.div_
  [ HH.slot _header unit Header.component unit identity
  , HH.main_
      [ HH.slot _content unit Content.component unit identity ]
  , HH.slot _footer unit Footer.component unit identity
  ]
```

Slot types, inputs, outputs, and handlers are stored in metadata.

### Conditional Rendering (Opaque)

```
div.alert[?CONDITIONAL]          → if state.showAlert then HH.div...
button.delete[?CONDITIONAL]      → when (state.canDelete) (HH.button...)
```

### Operators

Same as D3 Emmet:

```
>    Child         div>p              Parent-child
+    Sibling       div+div            Adjacent siblings
*N   Multiply      li*3               Repeat N times
()   Group         (div+span)*2       Group for multiply
```

### Full Examples

**Navigation Bar:**
```
nav.navbar
>div.container
 >(a.logo[href=/]{MyApp}
  +ul.nav-links
   >(li>a[href=/]{Home}+li>a[href=/about]{About}+li>a[href=/contact]{Contact})
  +div.auth
   >(@UserMenu[?CONDITIONAL]+button.login[onClick:HANDLER]{Login}[?CONDITIONAL]))
```

**Form with Validation:**
```
form.user-form[onSubmit:HANDLER]
>div.form-group
 >(label[for=email]{Email:}+input:email#email[value:STATE,onInput:HANDLER]+span.error[?CONDITIONAL]{VALIDATION_MSG})
+div.form-group
 >(label[for=password]{Password:}+input:password#password[value:STATE,onInput:HANDLER]+span.error[?CONDITIONAL]{VALIDATION_MSG})
+button:submit.btn-primary[disabled:COMPUTED]{Submit}
```

**Dashboard Layout:**
```
div.dashboard
>header.header>@Navigation
+div.content
 >(aside.sidebar>@SidebarMenu
  +main.main-content
   >(section.stats>(@StatCard*4)
    +section.charts
     >(div.chart-container>@LineChart+div.chart-container>@BarChart)
    +section.recent>@ActivityFeed))
+footer.footer>@Footer
```

## Opaque Feature System

Based on PSD3 implementation, we need these opaque types:

### 1. Event Handlers

```purescript
data OpaqueFeature
  = EventHandler
      { name :: String              -- "onClick", "onInput", etc.
      , handler :: Event -> Action  -- The actual handler function
      , description :: String       -- Human-readable description
      }
```

**Emmet Syntax:** `button[onClick:HANDLER]`

**Metadata Storage:**
```purescript
{ opaqueFeatures: Map NodePath
    [ EventHandler
        { name: "onClick"
        , handler: \_ -> NavigateHome
        , description: "Navigate to home page"
        }
    ]
}
```

### 2. State Bindings

```purescript
  | StateBinding
      { name :: String              -- Attribute name ("value", "checked")
      , selector :: State -> String -- Extract value from state
      , description :: String
      }
```

**Emmet Syntax:** `input[value:STATE]`

**Example:**
```purescript
StateBinding
  { name: "value"
  , selector: _.email
  , description: "Bind to state.email"
  }
```

### 3. Component Slots

```purescript
  | ComponentSlot
      { slotName :: String           -- _header, _content, etc.
      , slotType :: SlotType         -- Proxy type info
      , component :: Component       -- The component to render
      , input :: Input               -- Component input
      , output :: Output -> Action   -- Output handler
      , description :: String
      }
```

**Emmet Syntax:** `@Header`, `@UserMenu`, `@StatCard`

### 4. Computed Attributes

```purescript
  | ComputedAttribute
      { name :: String
      , computation :: State -> String  -- Dynamic computation
      , description :: String
      }
```

**Emmet Syntax:** `button[disabled:COMPUTED]`, `div[class:COMPUTED]`

**Example:**
```purescript
ComputedAttribute
  { name: "disabled"
  , computation: \state -> if state.isSubmitting then "true" else "false"
  , description: "Disable button when submitting"
  }
```

### 5. Conditional Rendering

```purescript
  | ConditionalRender
      { predicate :: State -> Boolean
      , description :: String
      }
```

**Emmet Syntax:** `div.alert[?CONDITIONAL]`

**Example:**
```purescript
ConditionalRender
  { predicate: _.showAlert
  , description: "Show when state.showAlert is true"
  }
```

## Round-Trip Implementation

### Architecture

Based on PSD3 implementation:

```
Halogen AST
     ↓
toEmmetWithMetadata
     ↓
{ emmetString :: String
, metadata :: HalogenMetadata
}
     ↓
(Edit structure)
     ↓
fromEmmetWithMetadata
     ↓
Halogen AST (with preserved handlers/slots)
```

### Core Functions

**HalogenEmmet/Metadata.purs:**

```purescript
type HalogenMetadata =
  { opaqueFeatures :: Map NodePath (Array OpaqueFeature)
  , componentSlots :: Map NodePath SlotInfo
  , conditionals :: Map NodePath ConditionalInfo
  }

-- Convert Halogen HTML to Emmet + metadata
toEmmetWithMetadata :: forall w i. HTML w i -> EmmetWithMetadata i

-- Convert Emmet + metadata back to Halogen HTML
fromEmmetWithMetadata :: forall w i. EmmetWithMetadata i -> Maybe (HTML w i)

-- Check if Halogen HTML is Emmet-compatible (no opaque features)
isEmmetCompatible :: forall w i. HTML w i -> Boolean

-- Analyze what can't be represented
analyzeCompatibility :: forall w i. HTML w i -> CompatibilityReport
```

### Tree Traversal

Similar to PSD3:

```purescript
-- Walk Halogen HTML tree
data HTML w i
  = Text String
  | Element (Maybe Namespace) ElemName (Array (Prop i)) (Array (HTML w i))
  | Slot SlotType SlotAddress ComponentHTML Output (Output -> i)
  | ...

-- Extract structure to Emmet, preserve complex parts as metadata
treeToEmmet :: forall w i. NodePath -> HTML w i -> EmmetResult i
```

### Metadata Substitution

```purescript
-- Replace opaque placeholders with actual handlers/slots
substituteOpaqueFeatures ::
  NodePath ->
  HalogenTree ->           -- Intermediate representation
  HalogenMetadata ->
  HalogenTree
```

## Implementation Phases

### Phase 0: Research & Design (2-3 days)

**Goals:**
- Study Halogen internals (HTML, Prop, Slot types)
- Map Halogen constructs to Emmet notation
- Define complete grammar (BNF)
- Validate design with sample components

**Deliverables:**
- `HALOGEN_EMMET_GRAMMAR.md` (complete spec)
- `HALOGEN_EMMET_EXAMPLES.md` (20+ examples)
- Type signatures for core functions

### Phase 1: Parser & Types (1 week)

**Tasks:**
1. Create `HalogenEmmet/Types.purs`
   - `EmmetExpr`, `EmmetNode`, `Attribute` ADTs
   - `OpaqueFeature` ADT (5 constructors)
   - `HalogenMetadata` type

2. Create `HalogenEmmet/Parser.purs`
   - Parse elements, classes, IDs
   - Parse attributes (static, STATE, HANDLER, COMPUTED, CONDITIONAL)
   - Parse text content `{...}`
   - Parse component slots `@SlotName`
   - Parse operators (`>`, `+`, `*`, `()`)

3. Create `HalogenEmmet/Validator.purs`
   - Validate element names
   - Validate attribute names
   - Validate class/ID syntax

**Tests:**
- Parse 50+ example strings
- Validate error messages
- Round-trip parse → print → parse

**Success Criteria:**
- All valid Emmet strings parse successfully
- Invalid strings produce helpful errors
- Parser is fast (<1ms for typical components)

### Phase 2: Converter (Emmet → Halogen) (1 week)

**Tasks:**
1. Create `HalogenEmmet/Converter.purs`
   - Convert `EmmetExpr` to intermediate tree
   - Handle opaque placeholders
   - Generate Halogen HTML

2. Handle edge cases:
   - Void elements (input, img, br)
   - Text nodes
   - Nested structures
   - Multipliers

**Tests:**
- Convert 50+ examples
- Verify generated HTML structure
- Test with real Halogen components

**Success Criteria:**
- Simple components render correctly
- Opaque features error gracefully (without metadata)
- Structure matches expected output

### Phase 3: Round-Trip (Halogen → Emmet → Halogen) (2 weeks)

**Tasks:**
1. Create `HalogenEmmet/Metadata.purs`
   - Implement `toEmmetWithMetadata`
   - Walk Halogen HTML tree
   - Extract opaque features to metadata
   - Generate Emmet string with placeholders

2. Implement `fromEmmetWithMetadata`
   - Parse Emmet string
   - Substitute opaque features from metadata
   - Reconstruct Halogen HTML

3. Implement `substituteOpaqueFeatures`
   - Map NodePath to metadata
   - Replace HANDLER with actual handlers
   - Replace STATE with actual bindings
   - Replace slots with actual components

**Challenges:**
- **Slot types**: Halogen slots are heavily typed. Need to preserve type info in metadata.
- **Event types**: Different events (MouseEvent, KeyboardEvent, FormEvent). Store in metadata.
- **State access**: Handlers close over state. Need to preserve closures.

**Tests:**
- Round-trip 20+ real components
- Verify handlers still work
- Verify slots render correctly
- Test structural edits (wrapping, moving, reordering)

**Success Criteria:**
- Lossless round-trip for all Halogen features
- Structural edits preserve behavior
- Performance acceptable (<10ms for typical components)

### Phase 4: Printer (Halogen → String) (3 days)

**Tasks:**
1. Create `HalogenEmmet/Printer.purs`
   - Convert `EmmetExpr` to formatted string
   - Handle multi-line formatting
   - Indent nested structures
   - Pretty-print long attribute lists

**Success Criteria:**
- Output is readable and idiomatic
- Round-trip parse → print → parse is identity
- Formatting is configurable

### Phase 5: Documentation & Examples (1 week)

**Tasks:**
1. Create `HALOGEN_EMMET_TUTORIAL.md`
   - Introduction for Halogen developers
   - Progressive examples (simple → complex)
   - Round-trip workflow explanation
   - Common patterns library

2. Create `HALOGEN_EMMET_REFERENCE.md`
   - Complete syntax reference
   - All opaque feature types
   - API documentation
   - Migration guide

3. Build demo page
   - Live editor (Emmet → preview)
   - Template library
   - Round-trip demonstration
   - Visual tree editor (future)

**Success Criteria:**
- Developers can learn syntax in <30 minutes
- Complete grammar reference available
- 50+ template examples

### Phase 6: Visual Editor (2-3 weeks) - OPTIONAL

**Vision:**
A graphical UI builder that uses Emmet as the underlying representation:

1. **Tree View**: D3 tree layout showing component hierarchy
2. **Property Panel**: Edit attributes, classes, text
3. **Drag-and-Drop**: Reorganize structure
4. **Live Preview**: See Halogen component render
5. **Code Export**: Generate Emmet or PureScript

**Features:**
- Add/remove elements
- Edit attributes inline
- Mark opaque features (show "HANDLER", "STATE" badges)
- Undo/redo
- Save/load templates

**Implementation:**
- Halogen component for editor UI
- D3 for tree visualization
- `fromEmmetWithMetadata` for live updates
- LocalStorage for persistence

**Success Criteria:**
- Build simple form without typing
- Reorganize dashboard layout via drag-drop
- Export to PureScript code
- Works on mobile (touch)

## Use Cases

### 1. Rapid Prototyping

**Before (20 minutes):**
Write verbose Halogen boilerplate, get lost in syntax.

**After (2 minutes):**
```
div.dashboard
>header>@Nav
+main>(aside>@Sidebar+section.content>@Feed)
+footer>@Footer
```

Sketch structure quickly, add handlers later.

### 2. Component Templates

**Before:** Copy-paste Halogen code, manually edit structure.

**After:** Share Emmet templates:

```purescript
-- Template library
cardTemplate = "div.card>(div.card-header>h3{TITLE}+div.card-body>p{CONTENT}+div.card-footer>@Actions)"

modalTemplate = "div.modal-overlay[onClick:HANDLER]>div.modal-content>@ModalBody"

formGroupTemplate = "div.form-group>(label[for=ID]{LABEL}+input#ID[value:STATE,onInput:HANDLER]+span.error[?CONDITIONAL]{ERROR})"
```

### 3. Visual Reorganization

**Scenario:** Move sidebar from left to right.

**Before:** Manually edit PureScript, track all nesting changes.

**After:**
```purescript
-- Original
let emmetData = toEmmetWithMetadata myLayout
-- "div.container>(aside.left>@Sidebar+main>@Content)"

-- Edit structure
let edited = "div.container>(main>@Content+aside.right>@Sidebar)"

-- Regenerate with same slots
let newLayout = fromEmmetWithMetadata { emmetData | emmetString = edited }
```

Sidebar component unchanged, just moved visually.

### 4. Learning Halogen

**For newcomers:**
- See structure without syntax noise
- Understand component composition
- Learn patterns (forms, lists, conditionals)

**Example:**
Show "This is what a typical Halogen form looks like" in 5 lines of Emmet vs. 50 lines of PureScript.

### 5. LLM Code Generation

**Current Problem:** LLMs generate verbose, repetitive Halogen code with syntax errors.

**With Emmet:** LLMs can generate concise, correct Emmet notation. Convert to Halogen after.

**Example Prompt:**
```
Generate a Halogen Emmet template for a user profile page with:
- Header with avatar and name
- Bio section
- Stats (followers, following, posts)
- Recent posts list
- Edit button
```

**LLM Output:**
```
div.profile
>header.profile-header
 >(img.avatar[src:STATE,alt=Avatar]+h1.name{STATE}+button.edit[onClick:HANDLER]{Edit})
+section.bio>p{STATE}
+section.stats
 >(div.stat>span.count{STATE}+span.label{Followers}
  +div.stat>span.count{STATE}+span.label{Following}
  +div.stat>span.count{STATE}+span.label{Posts})
+section.recent>ul.posts>li.post*5>@PostCard
```

Much easier for LLM to generate correctly!

## Technical Challenges

### 1. Halogen's Complex Type System

**Problem:** Slots are heavily typed with proxies, addresses, component types.

**Solution:** Store full type info in metadata. Round-trip within same module (preserve types).

### 2. Event Handler Types

**Problem:** Different events (MouseEvent, KeyboardEvent, FormEvent, CustomEvent).

**Solution:** Store event type in metadata. Reconstruct with correct type.

### 3. Component Props

**Problem:** Components have custom input/output types.

**Solution:** Metadata stores full input record and output handler.

### 4. Conditional Rendering

**Problem:** Halogen uses `when`, `guard`, pattern matching.

**Solution:** Store predicate function in metadata. Emmet shows `[?CONDITIONAL]`.

### 5. Performance

**Problem:** Round-trip on every keystroke in visual editor?

**Solution:**
- Cache metadata (only recompute on structural change)
- Incremental updates (only re-render changed subtrees)
- Debounce updates
- Use virtual DOM diffing

## Comparison to PSD3 Implementation

### Similarities

| PSD3 Emmet | Halogen Emmet |
|------------|---------------|
| Element types (g, c, r, p, l, t) | Element types (div, span, button, input) |
| Attributes (name=value, name:field) | Attributes (name=value, name:STATE) |
| Joins (j, n, u, x) | Slots (@SlotName) |
| Opaque COMPUTED/SCALE/CONDITIONAL | Opaque HANDLER/STATE/COMPUTED/CONDITIONAL |
| `toEmmetWithMetadata` | `toEmmetWithMetadata` |
| `fromEmmetWithMetadata` | `fromEmmetWithMetadata` |
| NodePath metadata indexing | NodePath metadata indexing |

### Differences

| PSD3 | Halogen |
|------|---------|
| SVG elements | HTML elements |
| Data joins (one element per datum) | Component slots (one component per slot) |
| D3 behaviors (zoom, drag) | Event handlers (onClick, onInput) |
| Attributes bind to data | Attributes bind to state |
| Tree layouts, force simulation | Forms, layouts, navigation |

### Lessons Learned from PSD3

**What worked well:**
1. ✅ Opaque tokens (COMPUTED, SCALE, CONDITIONAL) - Clear and parseable
2. ✅ NodePath indexing - Robust across structural changes
3. ✅ Metadata separation - Clean separation of structure vs. computation
4. ✅ Progressive documentation - Grammar → Examples → Interactive demos

**What to improve:**
1. 🔧 Make opaque features more discoverable in visual editor
2. 🔧 Better error messages when metadata is missing
3. 🔧 Performance optimization for large trees (1000+ nodes)
4. 🔧 Incremental parsing for live editing

## Success Metrics

### Adoption Metrics

- **Developers trying it:** Track downloads/usage
- **Components converted:** How many real components use Emmet?
- **Community templates:** User-contributed templates

### Technical Metrics

- **Parse time:** <1ms for typical components (<100 nodes)
- **Round-trip accuracy:** 100% for supported features
- **Code size reduction:** 5-10x smaller than equivalent PureScript
- **Learning time:** <30 minutes to basic proficiency

### Qualitative Metrics

- **Developer feedback:** "This makes Halogen fun!"
- **Use in tutorials:** Educational adoption
- **Visual editor usage:** Graphical vs. text editing ratio

## Future Extensions

### 1. CSS-in-PureScript

Extend to handle inline styles:

```
div[style=color:red;background:blue]
div[style:COMPUTED]                    -- Dynamic styles
```

### 2. Animations

```
div.fade-in[animation:TRANSITION]
button[onClick:HANDLER,transition:ANIMATION]
```

### 3. Responsive Layouts

```
div.grid[cols=1,md:cols=2,lg:cols=3]    -- Tailwind-style responsive
```

### 4. Component Libraries

Pre-built templates for:
- Forms (login, register, contact, survey)
- Layouts (dashboard, admin, blog, landing)
- Navigation (navbar, sidebar, tabs, breadcrumbs)
- Cards, modals, tooltips, dropdowns

### 5. Integration with CSS Frameworks

```
button.btn.btn-primary.btn-lg          -- Bootstrap
button.bg-blue-500.hover:bg-blue-700   -- Tailwind
```

### 6. TypeScript/JSX Export

Generate React/Vue/Svelte from Emmet:

```purescript
emmetToReact :: EmmetExpr -> JSX
emmetToVue :: EmmetExpr -> VueTemplate
emmetToSvelte :: EmmetExpr -> SvelteTemplate
```

Interoperability with other ecosystems!

## Risk Assessment

### High Risks

1. **Halogen changes**: Breaking changes in Halogen could require rewrites
   - **Mitigation**: Pin to Halogen version, abstract over changes

2. **Adoption resistance**: "Why not just write Halogen?"
   - **Mitigation**: Demonstrate clear value (visual editor, templates, learning)

3. **Type preservation**: Losing type info during round-trip
   - **Mitigation**: Store complete type info in metadata

### Medium Risks

1. **Performance**: Slow parsing/conversion
   - **Mitigation**: Optimize hot paths, use incremental updates

2. **Edge cases**: Obscure Halogen features
   - **Mitigation**: Comprehensive test suite, clear limitations

3. **Maintenance**: Keeping up with Halogen ecosystem
   - **Mitigation**: Community involvement, automated tests

### Low Risks

1. **Parser bugs**: Incorrect parsing
   - **Mitigation**: Property-based testing, fuzzing

2. **Documentation drift**: Docs out of sync with code
   - **Mitigation**: Generate docs from code, version together

## Related Work

### Existing Tools

1. **HTML Emmet** (VS Code, Sublime, etc.)
   - Standard Emmet for HTML
   - Inspiration for syntax
   - No PureScript/Halogen support

2. **Halogen Formless**
   - Declarative forms in Halogen
   - Similar goal (less boilerplate)
   - Different approach (type-level)

3. **Halogen Hooks**
   - Simplified component authoring
   - Complementary (could use Emmet for render)

4. **Elm-UI**
   - Declarative UI in Elm
   - Similar goals, different ecosystem

5. **React JSX**
   - Declarative markup in JavaScript
   - Closer to HTML than Halogen

### Why Halogen Emmet is Different

- **Round-trip capability**: Edit structure, preserve logic
- **Visual editing**: Enable graphical tools
- **PureScript native**: Works with Halogen's types
- **Metadata system**: Opaque features, not limited syntax

## Open Questions

1. **Syntax for props records?**
   - How to handle `HP.attr (AttrName "data-foo") "value"`?
   - Proposal: `div[data-foo=value]` or `div[attr:data-foo=value]`

2. **Syntax for component configuration?**
   - Components often have complex input records
   - Proposal: `@ComponentName[prop1=value,prop2:STATE]`

3. **Syntax for array rendering?**
   - `ul>li*DYNAMIC` for state.items.map?
   - Proposal: `ul>li[*STATE]` with metadata storing map function

4. **Syntax for refs?**
   - Halogen refs for DOM access
   - Proposal: `input[#refName]` with metadata storing ref handling

5. **Integration with existing tools?**
   - Editor plugins (VS Code, Vim, Emacs)?
   - Build tools (Spago, Bundler)?
   - Linters, formatters?

## Conclusion

Halogen Emmet notation is **feasible and valuable** based on our PSD3 implementation. The opaque attribute + metadata system we just built proves that complex framework features can be preserved during visual editing.

**Recommended Timeline:** 6-8 weeks for full implementation (if pursued).

**Next Steps (when ready):**
1. Review this plan with Halogen community
2. Build prototype parser (Phase 1)
3. Validate with real components
4. Iterate on syntax based on feedback
5. Build round-trip converter
6. Create documentation and demos
7. Consider visual editor as extension

**Key Insight:** The patterns from PSD3 Emmet transfer directly to Halogen. We've proven the concept works. Now it's a matter of applying it to a different domain (UI components vs. data visualization).

This would be an excellent contribution to the Halogen ecosystem and could significantly improve the developer experience for building PureScript web applications.

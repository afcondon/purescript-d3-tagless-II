# Understanding Pages Diagram Plan

Replacing Mermaid diagrams with PSD3-rendered SVGs. This document describes each diagram's purpose, visual design, and key elements.

---

## Design Principles

1. **Consistent color palette** - Use PSD3 brand colors
2. **Clear visual hierarchy** - Important concepts larger/bolder
3. **Minimal chrome** - No unnecessary boxes or arrows
4. **Readable at small sizes** - Works in sidebar TOC context too

### Color Palette

```
Primary:    #4A90A4  (teal-blue, PSD3 brand)
Secondary:  #7B68EE  (medium purple, accent)
Success:    #50C878  (emerald green, "good" states)
Warning:    #F4A460  (sandy orange, transitions)
Error:      #CD5C5C  (indian red, errors/exit)
Neutral:    #708090  (slate gray, backgrounds)
Light:      #F5F5F5  (white smoke, light bg)
Dark:       #2F4F4F  (dark slate, text)
```

---

## Diagram Catalog

### 1. Selection State Machine

**Page**: UnderstandingSelections, UnderstandingGrammar

**Current Mermaid**: `stateDiagram-v2` with 5 states and transitions

**Proposed Design**: Horizontal flow with state bubbles

```
┌─────────┐     ┌──────────┐     ┌────────────┐
│ SEmpty  │────▶│ SPending │────▶│ SBoundOwns │
└─────────┘     └──────────┘     └────────────┘
     │                                  │
     │         ┌───────────┐            │
     └────────▶│ JoinResult│────────────┤
               └───────────┘            │
                    │                   ▼
                    │          ┌──────────────┐
                    └─────────▶│ SBoundInherts│
                               └──────────────┘
                    │
                    ▼
               ┌──────────┐
               │ SExiting │
               └──────────┘
```

**Visual Elements**:
- Rounded rectangles for states
- Color-coded: SEmpty (neutral), SPending (warning), SBound* (success), SExiting (error)
- Arrows with operation labels (`select`, `append`, `joinData`, etc.)
- JoinResult as a special "fork" node (splits into enter/update/exit)

**Size**: 600 x 300

---

### 2. GUP Flow

**Page**: UnderstandingGrammar

**Current Mermaid**: `graph TD` showing General Update Pattern

**Proposed Design**: Vertical swimlane showing data flow

```
        ┌─────────────┐
        │  selectAll  │
        └──────┬──────┘
               │
        ┌──────▼──────┐
        │  joinData   │
        └──────┬──────┘
               │
     ┌─────────┼─────────┐
     │         │         │
┌────▼────┐┌───▼───┐┌────▼────┐
│  enter  ││update ││  exit   │
│ (green) ││(blue) ││ (red)   │
└────┬────┘└───┬───┘└────┬────┘
     │         │         │
     │    ┌────▼────┐    │
     └───▶│  merge  │    │
          └────┬────┘    │
               │         │
        ┌──────▼──────┐  │
        │  setAttrs   │  │
        └─────────────┘  │
                         │
                  ┌──────▼──────┐
                  │   remove    │
                  └─────────────┘
```

**Visual Elements**:
- Three-way fork showing enter/update/exit split
- Color coding: enter=green, update=blue, exit=red
- Merge point where enter and update rejoin
- Clear separation of "what happens to each group"

**Size**: 400 x 500

---

### 3. Layer Cake Architecture

**Page**: UnderstandingTreeAPI

**Current Mermaid**: `graph TB` showing API layers

**Proposed Design**: Stacked layers with connections

```
┌─────────────────────────────────────────────┐
│           High-Level APIs                    │
│  ┌─────────────┐  ┌──────────────────┐      │
│  │  TreeAPI    │  │  Simulation API  │      │
│  └──────┬──────┘  └────────┬─────────┘      │
└─────────┼──────────────────┼────────────────┘
          │                  │
          ▼                  ▼
┌─────────────────────────────────────────────┐
│              Core Layer                      │
│         ┌────────────────────┐              │
│         │    SelectionM      │              │
│         └─────────┬──────────┘              │
└───────────────────┼─────────────────────────┘
                    │
          ┌─────────┼─────────┐
          ▼         ▼         ▼
┌─────────────────────────────────────────────┐
│              Interpreters                    │
│  ┌────────┐  ┌────────┐  ┌────────┐        │
│  │  D3v2  │  │ String │  │  ...   │        │
│  └────────┘  └────────┘  └────────┘        │
└─────────────────────────────────────────────┘
```

**Visual Elements**:
- Horizontal bands for each layer
- Gradient or subtle texture to distinguish layers
- Arrows showing "builds on" relationship
- Could animate: start with D3, build up to TreeAPI

**Size**: 600 x 400

**Connection to Sankey**: The Sankey shows D3 modules flowing IN to PSD3. This layer cake shows how PSD3 organizes what it builds FROM those modules.

---

### 4. Tree Variants

**Page**: UnderstandingTreeAPI

**Current Mermaid**: `graph TD` showing Node/Join/NestedJoin/SceneJoin

**Proposed Design**: Four cards in a grid

```
┌──────────────┐  ┌──────────────┐
│    Node      │  │    Join      │
│  ┌───┐       │  │  ┌───┐───┐   │
│  │ A │       │  │  │ A │ B │...│
│  └───┘       │  │  └───┘───┘   │
│  static      │  │  data-driven │
└──────────────┘  └──────────────┘

┌──────────────┐  ┌──────────────┐
│ NestedJoin   │  │  SceneJoin   │
│  ┌───┬───┐   │  │  ┌───┐       │
│  │ A │ a │   │  │  │ + │ enter │
│  │   │ b │   │  │  │ ~ │ update│
│  └───┴───┘   │  │  │ - │ exit  │
│  decompose   │  │  └───┘       │
└──────────────┘  └──────────────┘
```

**Visual Elements**:
- 2x2 grid of cards
- Each card shows the structure visually
- Simple icons/shapes representing the pattern
- One-word description below

**Size**: 500 x 400

---

### 5. Attribute Type Resolution

**Page**: UnderstandingAttributes

**Current Mermaid**: `graph TD` showing Static/Datum/DatumIdx → IsAttribute → Resolved

**Proposed Design**: Converging arrows

```
     Static           Datum           Datum+Index
    cx 100.0      cx (_.x)        cx (\d i -> ...)
        │              │                  │
        └──────────────┼──────────────────┘
                       │
                       ▼
              ┌─────────────────┐
              │  IsAttribute    │
              │   typeclass     │
              └────────┬────────┘
                       │
                       ▼
              ┌─────────────────┐
              │   Compile-time  │
              │   resolution    │
              └─────────────────┘
```

**Visual Elements**:
- Three inputs converging to one process
- Typeclass as a "funnel" that unifies them
- Code examples in monospace
- Emphasis on "compile-time" (no runtime cost)

**Size**: 450 x 350

---

### 6. Contravariant Pattern

**Page**: UnderstandingAttributes

**Current Mermaid**: `graph LR` showing Attr → cmap → AttrNew

**Proposed Design**: Data flow with transform

```
┌─────────────┐         ┌─────────────┐
│  newDatum   │         │   datum     │
│  { user }   │────f───▶│  { name }   │
└─────────────┘         └──────┬──────┘
                               │
                        ┌──────▼──────┐
                        │  Attribute  │
                        │  datum      │
                        └──────┬──────┘
                               │
                        ┌──────▼──────┐
                        │   String    │
                        │   output    │
                        └─────────────┘

        cmap _.user : Attribute {name} -> Attribute {user: {name}}
```

**Visual Elements**:
- Show data flowing "backwards" through cmap
- f transforms data BEFORE attribute sees it
- Type signature at bottom
- Arrows show direction of data vs direction of cmap

**Size**: 400 x 400

---

### 7. Scene Lifecycle (NEW - for formalized system)

**Page**: UnderstandingScenes (updated)

**Proposed Design**: Three-phase timeline

```
    ┌──────────────────────────────────────────────────────┐
    │                    Scene Lifecycle                    │
    ├──────────────┬──────────────────┬───────────────────┤
    │  INIT        │  TRANSITION      │  STABLE           │
    │              │                  │                   │
    │  initRules   │  interpolate     │  finalRules      │
    │  applied     │  positions       │  applied          │
    │              │                  │                   │
    │  ┌────┐      │  ┌────┐──▶┌────┐│  ┌────┐           │
    │  │pin │      │  │here│   │there││ │unpin│           │
    │  └────┘      │  └────┘   └────┘│  └────┘           │
    │              │                  │                   │
    │              │  easeInOutCubic  │  Physics/Static   │
    └──────────────┴──────────────────┴───────────────────┘
         t=0              t=0→1              t=done
```

**Visual Elements**:
- Timeline from left to right
- Three distinct phases with vertical separators
- Small node icons showing what happens to nodes
- Easing curve shown in transition phase

**Size**: 700 x 250

---

### 8. DumbEngine vs PhysicsEngine (NEW)

**Page**: UnderstandingScenes or new Force Simulation page

**Proposed Design**: Two-panel comparison

```
┌────────────────────────┐  ┌────────────────────────┐
│     DumbEngine         │  │    PhysicsEngine       │
│   (Interpolation)      │  │   (Force Simulation)   │
├────────────────────────┤  ├────────────────────────┤
│                        │  │                        │
│    A ───────▶ B        │  │    A ~~~◊~~~▶ B       │
│    (straight line)     │  │    (forces guide)      │
│                        │  │                        │
│  • Predetermined path  │  │  • Emergent path       │
│  • Fixed duration      │  │  • Until settled       │
│  • Eased progress      │  │  • Physics-based       │
│                        │  │                        │
│  Use for: transitions  │  │  Use for: exploration  │
│  between known states  │  │  and stable states     │
└────────────────────────┘  └────────────────────────┘
```

**Visual Elements**:
- Side-by-side comparison
- DumbEngine: straight interpolated path
- PhysicsEngine: wavy/bouncy path with force indicators
- Bullet points summarizing differences

**Size**: 600 x 300

---

### 9. Type Safety Errors

**Page**: UnderstandingSelections

**Current Mermaid**: `graph LR` with red error boxes

**Proposed Design**: "Don't do this" examples with X marks

```
┌─────────────────────────────────────────────┐
│         Compile-Time Errors                  │
├─────────────────────────────────────────────┤
│                                             │
│  ✗  setAttrs attrs emptySel                │
│     └─ Can't set attrs on SEmpty           │
│                                             │
│  ✗  append Circle boundSel                 │
│     └─ Can't append to SBoundOwns          │
│                                             │
│  ✗  remove pendingSel                      │
│     └─ SPending has no elements to remove  │
│                                             │
└─────────────────────────────────────────────┘

        In D3: silent failure or undefined behavior
        In PSD3: compiler catches it ✓
```

**Visual Elements**:
- Red X marks for invalid operations
- Code in monospace
- Explanation indented below each
- Contrast with D3's behavior at bottom

**Size**: 500 x 350

---

### 10. Imperative vs Declarative

**Page**: UnderstandingTreeAPI, UnderstandingScenes

**Current Mermaid**: Side-by-side comparison

**Proposed Design**: Before/After style

```
        IMPERATIVE                    DECLARATIVE
        (method chain)                (tree structure)

        select                        renderTree
           │                              │
        appendChild                   Tree {
           │                            svg [
        appendChild                       group [
           │                                circles data
        joinData                          ]
           │                            ]
        append                        }
           │
        setAttrs
           │
          ...

     6+ operations                   1 declaration
     order matters                   structure matters
     mutations                       values
```

**Visual Elements**:
- Two columns
- Left: vertical chain of operations
- Right: nested structure (like code)
- Summary comparison at bottom

**Size**: 550 x 400

---

## Implementation Approach

### Option A: Static SVGs
- Create as .svg files in `assets/diagrams/`
- Reference via `<img>` in Halogen
- Simplest, but not "eating our own dog food"

### Option B: PSD3 TreeAPI
- Create a diagram DSL on top of TreeAPI
- Each diagram is a `Tree datum` value
- Render with `renderTree`
- Shows off PSD3's declarative nature

### Option C: Hybrid
- Simple diagrams as static SVG (boxes and arrows)
- Complex/animated diagrams with PSD3
- Gradually migrate static → PSD3

### Recommendation: Option C

Start with static SVGs to unblock the Understanding pages update. Then create a simple diagram DSL that makes it easy to build these kinds of diagrams with PSD3. The diagram DSL could become a showcase example itself.

---

## Diagram DSL Sketch

```purescript
-- Simple DSL for documentation diagrams
data DiagramElement
  = Box { label :: String, color :: String, x :: Number, y :: Number }
  | Arrow { from :: String, to :: String, label :: Maybe String }
  | Group { elements :: Array DiagramElement, layout :: Layout }

data Layout = Horizontal | Vertical | Grid Int

-- Usage
selectionStateDiagram :: Tree DiagramDatum
selectionStateDiagram = diagram
  [ box "SEmpty" neutral { x: 0, y: 0 }
  , box "SPending" warning { x: 200, y: 0 }
  , box "SBoundOwns" success { x: 400, y: 0 }
  , arrow "SEmpty" "SPending" (Just "appendData")
  , arrow "SPending" "SBoundOwns" (Just "append")
  ]
```

This would be a nice addition to the showcase examples - "diagrams made with PSD3".

---

## Priority Order

1. **Selection State Machine** - Core concept, referenced multiple times
2. **GUP Flow** - Essential for understanding D3 pattern
3. **Layer Cake** - Architecture overview
4. **Scene Lifecycle** - For the formalized scene system
5. **Tree Variants** - TreeAPI deep dive
6. **Attribute Resolution** - Type system explanation
7. **Type Safety Errors** - Selling point
8. **Imperative vs Declarative** - Comparison
9. **Contravariant** - Advanced concept
10. **DumbEngine vs Physics** - New for scenes page

# PS<$>D3: Interactive Data Visualization in PureScript

A PureScript embedded DSL for building interactive data visualizations, using D3.js both as inspiration and as an implementation layer under the Foreign Function Interface (FFI).

**Demo**: [https://afcondon.github.io/purescript-d3-tagless-II/](https://afcondon.github.io/purescript-d3-tagless-II/)

## What is This Project?

This project demonstrates an embedded DSL for building interactive data visualizations with PureScript. The DSL uses a Finally Tagless approach that allows multiple implementations for the "language" which permits both alternative implementations (only one, D3, is currently shown) but also alternative interpreters which generate code, documentation or even further "meta" visualizations to allow one to manipulate the DSL syntax tree.

## The Data Visualization Process

Data visualization transforms boring tables of data into perceivable patterns:

1. **Data** - Raw data structures (tables, JSON, etc.)
2. **Data structure** - Relationships between data elements
3. **Data presentation** - Visual encoding (HTML/SVG/Canvas)
4. **Perception** - Human insight and understanding

The goal is to make relationships in data visible through appropriate visual encoding, enabling the human visual system to process patterns that would be difficult or impossible to perceive in tabular form.

### Diverse visuals... but structural similarity

D3 enables an enormous range of visualizations - from simple bar charts to complex hierarchical layouts, force-directed graphs, geographic projections, and interactive dashboards. Despite their visual diversity, these visualizations share common structural patterns: data is bound to DOM elements, attributes are computed from data, and layouts determine spatial positioning.

## Project Goals (and non-Goals)

### Two #1 priorities, necessitates trade-offs

| Primary | Non-goals | Sub-goals |
|---------|-----------|-----------|
| **Expressivity:** fully equivalent to D3.js | **Completeness:** not all D3.js API surface is needed | Prefer idiomatic PureScript to exposing D3 APIs directly |
| **Readability:** ideally *more* readable than JS equivalent | **Modelling** of D3 state complexity thru type-system | Design for progressive enhancement |
| **Composable:** suitable for "programming in the large" | **Performance** equality to D3 | Showcase PureScript ecosystem and libraries |

### Expressivity

Something that people with limited prior knowledge / experience of data visualisation often seem to find surprising is the degree to which D3 is *fundamentally different* from "a charting library". While the library has some affordances that make it very easy to do common visualisations it is not in any way about "canned visualisations". Rather, it is a language for describing a relationship between arrays of data and arbitrary constructions of DOM (HTML or SVG) element or marks on Canvas, and it could in principle be used to do auditory "visualisation" or, who knows, maybe olfactory "visualisation" or drone displays or whatever.

You can get a greater sense of the potential of D3 and the range of things that have thus far been produced using it at [ObservableHQ](https://observablehq.com/).

So when we talk about expressability as a goal for this PureScript eDSL, we're not talking about working at the level of "make me a bar graph", "make me a scatterplot", we're talking about retaining the expressability of that translation from array to, for example, SVG.

Furthermore, I am especially interested in, and an advocate for, interactive data visualisations that act as control elements for other aspects of applications. It's worth calling this out here because "interactive visualisation" very often means "explorable" visualisation, ie one in which you can interact with the elements *but only to manipulate the visualisation itself*. That is a different, and more limited, sense of interaction from what i intend.

### Readability

If we look at D3.js as a kind of embedded DSL in JavaScript it is certainly clear and readable in it's core feature: declaratively associating some array(s) of data with some elements in the DOM and attributes of those elements. While it is definitely not a goal to reproduce the structures of D3js own language, it's also unwise to change it arbitrarily because there's a very large amount of real world experience to be leveraged or at least not shunned. So that's one kind of readability concern - can it be somewhat isomorphic to the equivalent JavaScript where that's appropriate?

A second form of readability is concerned with the places where - for me - the D3 / JavaScript approach breaks down, which is in roles and responsabilities of different parts of the code. This is related pretty closely to composability (see next goal for more on this).

Ideally, i would like the person coding the data layer and data model to be somewhat insulated from the concerns of the person using that data model to create a visualisation. And likewise, i would like the person developing the data visualisation to be somewhat insulated from the concerns of a web app developer. Now, these might very well all be the *same person* but separating the concerns like this makes it easier to evolve the code and, crucially, makes it all a little less brittle.

### Composability

There is an additional important goal which is *composability*, if you are a PureScript or Haskell programmer you probably know what i mean by this and if you are, say, a JavaScript D3 programmer perhaps that will seem odd or even contentious.

The benefits we seek from composability include:

* better composability of sub-programs
* better expressiveness in describing the problem domain
* better correctness in implementations (ie fewer bugs)
* better ability to evolve programs due to shifting requirements (re-factoring)

### Non-goal: Complete API coverage

As alluded to above, there's lots of API in D3 that needs nothing more than an FFI wrapper to be accessible from a PureScript eDSL. D3 is both modular and somewhat functional in style (in the JavaScript sense of functional programming, to be clear). So it was from the start a non-goal to completely expose all of D3 as *idiomatic* PureScript where a simple wrapper was sufficient.

Furthermore, i have only written those wrappers *as I needed them* to there are still *many* parts of D3 that are not covered by this eDSL.

### Non-goal: Modelling of D3 State

This might seem like a surprising choice - D3 is inherently _very_ stateful, there's state in D3, there's state in the DOM, there's statefulness in your (pure) data after you give it to D3. State everywhere. In many cases in functional programming you might try to ameliorate the dangers of this by explicitly modelling the state, using a State Monad or marking everything that changes or depends upon state as "Effect"-full.

Indeed i have tried this approach in the past. In this library i have instead striven to isolate the statefulness to only the code that uses eDSL represented by the `Selection` and `Simulation` monads. This *significantly* removes but cannot fully eliminate the issues associated with state. However, i believe it is a good compromise although much more could be said on the matter.

### Non-goal: performance optimisation that compromise readability

The performance bottlenecks in a D3 visualisation are, by their nature, going to be the assignment of attributes to (potentially millions of) DOM elements. It is likely that this code will always have to have a native implementation, ie FFI, *or* it would have to be re-written in a way that was as performant as possible in PureScript. I have chosen to use D3 for this as a low-level, well-tested, optimised layer in order to preserve readability of the visualisation code.

## What's a DSL? and what's an eDSL?

### Domain Specific Language

Programming language tailored to a particular domain.

Might be limited compared to general purpose programming language.

But, may also be easier for novices, domain experts, non-programmers to use.

### Embedded Domain Specific Language

Embedded Domain Specific Language makes domain specific tasks easier *without* limiting the programmer in any way - they still have access to all parts of the host language.

This has Pros & Cons

## The grammar of D3...

Analyse the *implicit* grammar of D3...

...in order to define a *minimal* grammar for our PureScript eDSL

(consistent with priority goals of readability and expressiveness)

### A Visual D3 Grammar

**This is much simpler than D3's actual AST would be (if it had one) but it is sufficient to express a LOT of D3 scripts.**

| Name | Symbol | Function | Notes |
|------|--------|----------|-------|
| attach | a | Select an entry point (or points) in the DOM | Simply uses a CSS type selector to identify the target. Resulting selection can then be used for append / join in order to build up a visualisation from HTML, SVG or Canvas elements. |
| appendTo | + | Add some DOM element **e** to a selection | Each element can have attributes. If data has been bound higher up in the AST then that data is available in this element's attributes |
| join | <+> | For every *datum* **d** in some array, insert an element **e** | We'll run the visualisation with some data model which can be arbitrary in structure, but at every point where we want to append *multiple* elements we need to have a function that yields a simple array.<br><br>Each element can have attributes that are derived from the datum with which it is associated.<br><br>The datum at each element is inherited by its children, so any subsequent join starts with the structure of this datum. |

### 3 Little Circles: A canonical simplest example

```
    a        "div#hook"
    ↓
    +        <svg>
    ↓
    +        <g>
    ↓
   <+>       [1,2,3] → <circle>
```

https://bost.ocks.org/mike/circles/

### Radial Tree: A more complex example

The Radial Tree and Cluster Dendrogram visualizations share the exact same structure:

```
    a          "div#hook"
    ↓
    +          <svg>
    ↓
    +          <g.links>    +          <g.nodes>    +          <g.labels>
    ↓                       ↓                       ↓
   <+>         <path>      <+>         <circle>    <+>         <text>
               ↑                       ↑                       ↑
            m->m.links              m->m.nodes              m->m.nodes
```

All the differences are in details of how the node attributes are calculated from the data.

### Even complex examples structurally simple

Even highly complex visualizations like [Nadieh Bremer's Top 2000 visualization](https://top2000.visualcinnamon.com) can be expressed with this simple grammar - the complexity lies in the data transformations and attribute calculations, not in the fundamental structure.

## Interpreter for Selections

```purescript
class (Monad m) <= SelectionM selection m where
  appendTo    :: selection -> Element -> Array (SelectionAttribute) -> m selection
  attach      :: Selector selection                                  -> m selection
  simpleJoin  :: ∀ datum. selection -> Element -> (Array datum) -> (Datum_ -> Index_)
                                                                     -> m selection
```

**(as you can see, actual grammar a bit richer, but still small)**

The full `SelectionM` type class includes:

```purescript
class (Monad m) <= SelectionM selection m where
  appendTo         :: selection -> Element -> Array (SelectionAttribute) -> m selection
  attach           :: Selector selection                                  -> m selection
  simpleJoin       :: ∀ datum. selection -> Element -> (Array datum) -> (Datum_ -> Index_)
                                                                          -> m selection
  selectUnder      :: selection -> Selector selection                    -> m selection
  filterSelection  :: selection -> Selector selection                    -> m selection
  mergeSelections  :: selection -> selection                             -> m selection
  setAttributes    :: selection -> Array (SelectionAttribute)            -> m Unit
  on               :: selection -> Behavior selection                    -> m Unit
  updateJoin       :: ∀ datum. selection -> Element -> (Array datum) -> (Datum_ -> Index_)
                      -> m { enter :: selection, exit :: selection, update :: selection }
```

## Interpreter for Simulations

**(a lot more complicated, much Effect, many State etc)**

The `SimulationM` type class extends `SelectionM` with physics simulation capabilities:

```purescript
class (Monad m, SelectionM selection m) <= SimulationM selection m | m -> selection where
  -- control
  start :: m Unit
  stop  :: m Unit

  -- config
  setConfigVariable :: SimVariable -> m Unit

  -- management of forces
  actualizeForces :: Map Label ForceStatus -> m Unit

  -- management of data (nodes and links)
  setNodes :: forall d. Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  setLinks :: forall d r id. (Eq id) =>
              Array (D3Link id r) -> Array (D3_SimulationNode d) -> (Datum_ -> Index_)
              -> m (Array (D3LinkSwizzled (D3_SimulationNode d) r))

  -- updating with selections (less type-safe but necessary for dynamic updates)
  setNodesFromSelection :: selection -> m Unit
  setLinksFromSelection :: selection -> (Datum_ -> Boolean) -> m Unit

  -- merge new data with existing simulation state
  mergeNewDataWithSim :: forall d r id. (Eq id) =>
    selection ->                                    -- nodes selection
    (Datum_ -> Index_) ->                          -- nodes keyFn
    selection ->                                    -- links selection
    (Datum_ -> Index_) ->                          -- links KeyFn
    RawData d r id ->                              -- links and nodes raw data
    m { links :: (Array (D3LinkSwizzled (D3_SimulationNode d) r))
      , nodes :: (Array (D3_SimulationNode d))
      }

  -- tick functions
  addTickFunction    :: Label -> Step selection -> m Unit
  removeTickFunction :: Label                    -> m Unit
```

## Multiple Interpreters FTW

The Finally Tagless encoding enables multiple interpretations of the same visualization code:

* **"D3" interpreter** - consumes "script" and produces visualisation in the DOM - just like D3.js script would

* **"Printer" interpreter** - consumes "script" and produces a textual representation of the script actions - in principle, this could be extended to output the equivalent JavaScript.

* **"Meta" interpreter** - consumes "script" and produces...another Tree structure, representing the structure of the visualisation itself. This Tree structure can then be fed back in to, for example, the "D3" interpreter to produce a diagrammatic documentation of the script.

## Motivation

I have built moderately complex, custom interactive data visualisations in the past both in JavaScript and PureScript, using D3.js. I found that JavaScript generally, and D3 in particular, seemed to work best for visualisations that were less "app-like" and more "chart-like". What i mean by this is that when the complexity started to rise to the level of a small application and when multiple programmers were involved, or if one had to return to some code after time had elapsed, the whole thing was very brittle and refactoring of it prohibitively difficult.

This could certainly be a "feature, not a bug" for some domains of application such as building a big beautiful rich visualisation for a one-off publication such as a New York Times feature. However, when the visualisation is used to *control* application behaviour or the visualisation begins to approach the complexity and multi-layered-ness of an app...this all in one single script language is a real problem, at least in my experience.

In PureScript it is common, and easy, to use JavaScript libraries via the FFI initially as it is a very quick way to get access to the enormous world of functionality that exists in open source JavaScript libraries. Sometimes this can be sufficient, you wrap a component or a function and its abstractions never leak and all is well. Other times, you wrap something but there's a kind of impedance mismatch with the way the JavaScript abstraction work and the way you'd like to handle, and particularly to compose, things in the purely functional world. D3.js was definitely the latter, for me.

D3.js is a big library with thousands of API end-points but, crucially, not all of those end-points are problematic for composing larger scale applications or weaving visualisations into PureScript web applications. Instead, its is primarily two core areas of the API, Selection and Simulation (more details on these later) which tend to actually _structure_ programs in a characteristic D3 / JavaScript vernacular. It is these APIs that are first wrapped (by FFI) and then made available in purely functional idiomatic way by this library.

A secondary, but also very important, consideration is the ability to design and work with Algebraic Data Types (ADTs) and the rich container libraries that are available in PureScript while building and implementing visualisations and especially the code that surrounds the visualization. While D3 ultimately is a kind of array programming DSL _within_ JavaScript and our PureScript eDSL is going to bottom out to some sort of "arrays mapped over the DOM" too, we want to be able to create data models that are more sophisticated and have better invariants as these are keys to both composability and maintainable, long-lived programs.

## Installation

After cloning the repo, you should be able to do:

* `yarn install`
* `yarn run build`
* `yarn run bundle`

and then if you serve `http` from the `docs` directory you should be able to see the same demo as at the link above.

## Examples

The project includes several examples demonstrating different aspects of the library:

### Simple Examples

* **Three Little Circles** - The canonical simplest D3 example, demonstrating basic data binding
* **General Update Pattern** - Shows how to handle dynamic data updates with enter/exit/update
* **Anscombe's Quartet** - Four datasets with identical statistics but different distributions

### Layout Examples

* **Tree Layouts** - Multiple variations (horizontal, vertical, radial) showing hierarchical data
* **Bubble Chart** - Circle packing layout
* **Sankey Diagram** - Flow visualization
* **Chord Diagram** - Relationship visualization

### Interactive Examples

* **Les Misérables Network** - Force-directed graph with draggable nodes
* **Spago Explorer** - Full Halogen application demonstrating multiple scenes, force configurations, and interactive controls

### Alternative Interpreters

* **MetaTree** - Visualizes the structure of visualization code itself
* **Printer** - Generates textual representation of visualization scripts

## Further Reading

* [ObservableHQ D3 Gallery](https://observablehq.com/@d3/gallery) - Excellent examples of D3 visualizations
* [D3 Graph Gallery](https://www.d3-graph-gallery.com/) - Comprehensive D3 examples
* [Visual Vocabulary (FT)](https://ft-interactive.github.io/visual-vocabulary/) - Guide to choosing visualizations
* Munzner, Tamara. *Visualization Analysis and Design* - Theoretical foundation
* Edward Tufte's books - Classic data visualization theory

## License

See LICENSE file for details.

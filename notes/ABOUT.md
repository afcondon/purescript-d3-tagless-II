# PS<$>D3
## Interactive Data Visualization in PureScript

A PureScript embedded *DSL* for building *interactive data visualizations*, using *D3.js* both as inspiration and as an implementation layer under the Foreign Function Interface (FFI).

This repo also contains documentation and examples using a website built with PureScript Halogen and the PS<$>D3 library. Here is a link to this website, hosted under GitHub Pages - [https://afcondon.github.io/purescript-d3-tagless-II/](https://afcondon.github.io/purescript-d3-tagless-II/)

## What is This Project?

This project demonstrates an embedded DSL for building interactive data visualizations with PureScript. The DSL uses a Finally Tagless approach that allows multiple interpreters for the same "language" which allows us to generate working code using an FFI to D3 or documentation and other more complex uses which will be discussed below. 

## The Data Visualization Process

Data visualization transforms "boring" - but more importantly _less informative_ tables of data into perceivable patterns. We can think of this as a pipeline in which steps 2 and 3 are the purpose of this library. 

1. **Data** - Raw data structures (tables, JSON, etc.)
2. **Data structure** - Relationships between data elements
3. **Data presentation** - Visual encoding (HTML/SVG/Canvas)
4. **Perception** - Human insight and understanding

The goal is to make relationships in data visible through appropriate visual encoding, enabling the human visual system to process patterns that would be difficult or impossible to perceive in tabular form.

### Design philosophy of D3*
* (as I understand it)

D3's great innovation was to enable an enormous range of visualizations using some simple fundamental concepts and a small core API. It supports dataviz from simple bar charts to complex hierarchical layouts, force-directed graphs, geographic projections, and interactive dashboards. Despite their visual diversity, these visualizations share common structural patterns: data is bound to DOM elements, attributes are computed from data, and layouts determine spatial positioning.

Something that people with limited prior knowledge / experience of data visualisation often seem to find surprising is the degree to which D3 is *fundamentally different* from "a charting library". While the library has some affordances that make it very easy to do common visualisations it is not in any way about "canned visualisations". Rather, it is a language for describing a relationship between arrays of data and arbitrary constructions of DOM (HTML or SVG) element or marks on Canvas, and it could in principle be used to do auditory "visualisation" or, who knows, maybe olfactory "visualisation" or drone displays or whatever.

Another revolutionary aspect of D3 was that it was *screen/web native* - right from the beginning it supported responsive design, transitions, animations, force-layouts all of which are completely distinct from data visualisation on paper.

You can get a greater sense of the potential of D3 and the range of things that have thus far been produced using it at [ObservableHQ](https://observablehq.com/).

### Design philosophy of PS<$>D3

This library presents a slighly more formal grammar embedded in a language that is a lot better suited to larger and more long-lived projects. Whereas D3 has patterns of use that you learn, in PS<$>D3 the pattern is formalised into a grammar with an interpreter. Moreover, the same grammar can be interpreted by different interpreters directly *in* PureScript, which, as we will see creates some powerful new uses.

Where D3 enabled data visualisers to create movement and responsiveness, PS<$>D3 seeks to enable something that's theoretically possible in D3 but not much seen in practice and that is using the data visualisation as the user interface.  This is not the same thing as "interactive visualisation" in the sense of "explorable" visualisation, ie one in which you can interact with the elements *but only to manipulate the visualisation itself*. That is a different, and more limited, sense of interaction. What we are talking about when building UI with data visualisation is leveraging the information density of data visualisation to provide direct manipulation of complex or large systems. 

This is actually not a revolutionary idea - think how much of an improvement over the initial "folders of icons" on the Mac the NeXT column browser was. But it is a direction of travel that has been effectively lost during the web UI revolution of the past 20 years.

This repo and website contains one example of how this might work, the Code Explorer.
 
## Project Goals (and non-Goals)

### Priorities and Trade-offs

We have two principal priorities in writing this library and since you can't have two number one priorities, some trade-offs are inevitable which we will discuss here. 

| Primary                                                 | Non-goals                                             | Sub-goals                                                |
|---------------------------------------------------------|-------------------------------------------------------|----------------------------------------------------------|
| **Expressivity:** fully equivalent to D3.js             | **Completeness:** not all D3.js API surface is needed | Prefer idiomatic PureScript to exposing D3 APIs directly |
| **Readability:** as readable as D3.js equivalent        | **Modelling** of D3 state complexity thru type-system | Design for progressive enhancement                       |
| **Composable:** suitable for "programming in the large" | **Performance** equality to D3                        | Showcase PureScript ecosystem and libraries              |

### Goals: details
#### Expressivity

As alluded to above, expressivity is a key to D3's success - it's not limited to some library of canned visualisations and it's far, far more than even the most parameterized control of colours and symbols. It's important not to lose that in making D3 available directly in PureScript. 

#### Readability

If we look at D3.js as a kind of embedded DSL in JavaScript it is certainly clear and readable in its core feature: declaratively associating some array(s) of data with some elements in the DOM and attributes of those elements. While it is definitely not a goal to reproduce the structures of D3js own language in PureScript per se, the goal is to have the same readability, leverage the good work that Mike Bostock did in developing it. At the same time the library should be as idiomatically PureScript / Haskell style as possible.

#### Composability

A related concern to readability is composability, the separation of concerns that makes componentisation possible and which makes possible programming in the large. 

YMMV but for me this is where the D3 / JavaScript approach breaks down. 

If you are a PureScript or Haskell programmer you probably know what i mean by this and if you are, say, a JavaScript D3 programmer perhaps that will seem odd or even contentious.

The benefits we seek from composability include:

* better composability of sub-programs
* better expressiveness in describing the problem domain
* better correctness in implementations (ie fewer bugs)
* better ability to evolve programs due to shifting requirements (re-factoring)

Ideally, i would like the person coding the data layer and data model to be somewhat insulated from the concerns of the person using that data model to create a visualisation. And likewise, i would like the person developing the data visualisation to be somewhat insulated from the concerns of a web app developer. Now, these might very well all be the *same person* but separating the concerns like this makes it easier to evolve the code and, crucially, makes it all a little less brittle.

### Non-goals: details 
#### Complete API coverage

As alluded to above, there's lots of API in D3 that needs nothing more than an FFI wrapper to be accessible from a PureScript eDSL. D3 is both modular and somewhat functional in style (in the JavaScript sense of functional programming, to be clear). So it was from the start a non-goal to completely expose all of D3 as *idiomatic* PureScript where a simple wrapper was sufficient.

Furthermore, i have only written those wrappers *as I needed them* to there are still *many* parts of D3 that are not covered by this eDSL.

Addendum 2025 - the advent of LLMs and coding assistance agents means that perhaps complete API coverage might arrive sooner than expected.

#### Modelling of D3 State

This might seem like a surprising choice - D3 is inherently _very_ stateful, there's state in D3, there's state in the DOM, there's statefulness in your (pure) data after you give it to D3. State everywhere. In many cases in functional programming you might try to ameliorate the dangers of this by explicitly modelling the state, using a State Monad or marking everything that changes or depends upon state as "Effect"-full.

Indeed i have tried this approach in the past. In this library i have instead striven to isolate the statefulness to only the code that uses eDSL represented by the `Selection` and `Simulation` monads. This *significantly* removes but cannot fully eliminate the issues associated with state. 

#### Performance equality with D3

While this was a non-goal in practice it doesn't seem to have been a problem. Essentially, this is because PS<$>D3 simply leverages D3.js for all the performance critical aspects and because the FFI has been kept simple by the decision described in the previous section. 

The performance bottlenecks in a web data visualisation are, by their nature, going to be the assignment of potentially millions of attributes to potentially millions of DOM elements. D3.js does this extremely well and the PureScript layer doesn't compromise it much, if at all.

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

| Name       | Function                                                       | Notes                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|------------|----------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *attach*   | Select an entry point (or points) in the DOM                   | Simply uses a CSS type selector to identify the target. Resulting selection can then be used for append / join in order to build up a visualisation from HTML, SVG or Canvas elements.                                                                                                                                                                                                                                                          |
| *appendTo* | Add some DOM element **e** to a selection                      | Each element can have attributes. If data has been bound higher up in the AST then that data is available in this element's attributes                                                                                                                                                                                                                                                                                                          |
| *join*     | For every *datum* **d** in some array, insert an element **e** | We'll run the visualisation with some data model which can be arbitrary in structure, but at every point where we want to append *multiple* elements we need to have a function that yields a simple array.<br><br>Each element can have attributes that are derived from the datum with which it is associated.<br><br>The datum at each element is inherited by its children, so any subsequent join starts with the structure of this datum. |

### Grammar diagrams

#### 3 Little Circles: the canonical simplest example

Insert Mermaid diagram here

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

#### Radial Tree: A more complex example

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

#### Even complex examples structurally simple

Even highly complex visualizations like [Nadieh Bremer's Top 2000 visualization](https://top2000.visualcinnamon.com) can be expressed with this simple grammar - the complexity lies in the data transformations and attribute calculations, not in the fundamental structure.

# Move the rest to different documents

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

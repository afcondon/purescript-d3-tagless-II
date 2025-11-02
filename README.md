![Pasted Graphic 8](https://user-images.githubusercontent.com/1260895/138756849-2d9e93c3-2393-48eb-ad86-88b4798e14c0.png)

# Project Overview: Functional Programming and Data Visualization

This project demonstrates an embedded DSL for building interactive data visualizations with PureScript, using D3.js both as inspiration and as an implementation layer under the Foreign Function Interface (FFI).

The DSL uses a Finally Tagless approach that allows multiple implementations for the "language" which permits both alternative implementations (only one, D3, is currently shown) but also alternative interpreters which generate code, documentation or even further "meta" visualizations to allow one to manipulate the DSL syntax tree.

This overview document is an introduction to the ideas in the project and a guide to the other documentation and articles about it. Presuming that many people who might be interested in this work are going to come from only one of two disciplines: data-visualisation OR functional programming, i've tried to provide introductions for each camp to at least be acquainted with the important aspects of the other before reading further.

## Show me something right now

There's a demo, see it working [here](https://afcondon.github.io/purescript-d3-tagless-II/).

## Installation

After cloning the repo, you should be able to do:

* `yarn install`
* `yarn run build`
* `yarn run bundle`

and then if you serve `http` from the `docs` directory you should be able to see the same demo as at the link above.

![image](https://user-images.githubusercontent.com/1260895/138859399-a758dcb2-e05b-407f-9538-035acd99845f.png)

## Motivation and goals

### Motivation

I have built moderately complex, custom interactive data visualisations in the past both in JavaScript and PureScript, using D3.js. I found that JavaScript generally, and D3 in particular, seemed to work best for visualisations that were less "app-like" and more "chart-like". What i mean by this is that when the complexity started to rise to the level of a small application and when multiple programmers were involved, or if one had to return to some code after time had elapsed, the whole thing was very brittle and refactoring of it prohibitively difficult.

This could certainly be a "feature, not a bug" for some domains of application such as building a big beautiful rich visualisation for a one-off publication such as a New York Times feature. However, when the visualisation is used to *control* application behaviour or the visualisation begins to approach the complexity and multi-layered-ness of an app...this all in one single script language is a real problem, at least in my experience.

Other's experiences may vary -- some people have more tolerance and/or ability to handle those complexities. Nonetheless, most people who turn to languages like PureScript (Haskell family languages in general) do so because they share those experience and have found that there are better mechanisms for composing programs together which are more robust.

In PureScript it is common, and easy, to use JavaScript libraries via the FFI initially as it is a very quick way to get access to the enormous world of functionality that exists in open source JavaScript libraries. Sometimes this can be sufficient, you wrap a component or a function and its abstractions never leak and all is well. Other times, you wrap something but there's a kind of impedance mismatch with the way the JavaScript abstraction work and the way you'd like to handle, and particularly to compose, things in the purely functional world. D3.js was definitely the latter, for me.

D3.js is a big library with thousands of API end-points but, crucially, not all of those end-points are problematic for composing larger scale applications or weaving visualisations into PureScript web applications. Instead, its is primarily two core areas of the API, Selection and Simulation (more details on these later) which tend to actually _structure_ programs in a characteristic D3 / JavaScript vernacular. It is these APIs that are first wrapped (by FFI) and then made available in purely functional idiomatic way by this library.

A secondary, but also very important, consideration is the ability to design and work with Algebraic Data Types (ADTs) and the rich container libraries that are available in PureScript while building and implementing visualisations and especially the code that surrounds the visualization. While D3 ultimately is a kind of array programming DSL _within_ JavaScript and our PureScript eDSL is going to bottom out to some sort of "arrays mapped over the DOM" too, we want to be able to create data models that are more sophisticated and have better invariants as these are keys to both composability and maintainable, long-lived programs.

## Goals

There are two "number one priority" goals: *expressability* and *readability*. Since it's impossible to have two number one priorities these are necessarily somewhat in tension and has been necessary at all times to try to balance them.

### Expressability

Something that people with limited prior knowledge / experience of data visualisation often seem to find surprising is the degree to which D3 is *fundamentally different* from "a charting library". While the library has some affordances that make it very easy to do common visualisations it is not in any way about "canned visualisations". Rather, it is a language for describing a relationship between arrays of data and arbitrary constructions of DOM (HTML or SVG) element or marks on Canvas, and it could in principle be used to do auditory "visualisation" or, who knows, maybe olfactory "visualisation" or drone displays or whatever.

You can get a greater sense of the potential of D3 and the range of things that have thus far been produced using it at [ObservableHQ](https://observablehq.com/) and i will discuss this again in the [section "Introduction to Data Visualisation for Functional Programmers](#data-visualization-for-functional-programmers).

So when we talk about expressability as a goal for this PureScript eDSL, we're not talking about working at the level of "make me a bar graph", "make me a scatterplot", we're talking about retaining the expressability of that translation from array to, for example, SVG.

Furthermore, I am especially interested in, and an advocate for, interactive data visualisations that act as control elements for other aspects of applications. It's worth calling this out here because "interactive visualisation" very often means "explorable" visualisation, ie one in which you can interact with the elements *but only to manipulate the visualisation itself*. That is a different, and more limited, sense of interaction from what i intend.

Before starting this project the question of criteria for "expressability" arose. My approach has been to analyse hundreds of existing visualisations that can be found on ObservableHQ and previously on `http://bl.ocks.org` and to select, model and then translate a set of examples that contained at least the most important structurally distinct visualisation types.

Things that the examples were chosen to show:

* simple use of selections and attributes
* use of the D3 design pattern known as the "General Update Pattern" together with transitions
* at least one example of `d3-hierarchy`, with six variations on the basic tree 
* an example of the D3 simulation functionality to render graphs (ie nodes and edges) using a physics engine to do the layout

In addition, some of these examples feature other key ideas that i knew would be necessary to convince _me_ that any new library was more than pre-canned charts. I'm referring to features such as dragging elements, panning and zooming the visualisation etc, here.

Finally, because these examples are all pretty small and don't really hit the limits of complexity that motivated this project i have also written a PureScript Halogen app that is more fully featured and designed to be interactive in that second sense, allowing actions in the visualisation to trigger Halogen level events.

### Readability

If we look at D3.js as a kind of embedded DSL in JavaScript it is certainly clear and readable in it's core feature: declaratively associating some array(s) of data with some elements in the DOM and attributes of those elements. While it is definitely not a goal to reproduce the structures of D3js own language, it's also unwise to change it arbitrarily because there's a very large amount of real world experience to be leveraged or at least not shunned. So that's one kind of readability concern - can it be somewhat isomorphic to the equivalent JavaScript where that's appropriate?

A second form of readability is concerned with the places where - for me - the D3 / JavaScript approach breaks down, which is in roles and responsabilities of different parts of the code. This is related pretty closely to composability (see next goal for more on this).

Ideally, i would like the person coding the data layer and data model to be somewhat insulated from the concerns of the person using that data model to create a visualisation. And likewise, i would like the person developing the data visualisation to be somewhat insulated from the concerns of a web app developer. Now, these might very well all be the *same person* but separating the concerns like this makes it easier to evolve the code and, crucially, makes it all a little less brittle.

For example, PureScript's strong typing helps the compiler catch changes to the data model that would break the data visualisation at runtime and a capability-based API for the visualisation (see [Library Architecture](missing link) enables the web application developer to treat the visualisation as a component and the data model code as normal, idiomatic PureScript.

So "readability" here is multi-faceted and it can be summarized as:

* unless there's a good reason not to, preserve the declarative simplicity of D3's selection chaining and attribute setting
* tease apart the different roles and responsabilities in the code to disentangle data-model from visualisation from application framework code.

### Composability

There is an additional important goal which is *composability*, if you are a PureScript or Haskell programmer you probably know what i mean by this and if you are, say, a JavaScript D3 programmer perhaps that will seem odd or even contentious. I will discuss composability at more length in the section "Functional Programming for Data Visualizers" [below](#functional-programming-for-data-visualizers).

## Non-goals

### Non-goal: Complete API coverage

As alluded to above, there's lots of API in D3 that needs nothing more than an FFI wrapper to be accessible from a PureScript eDSL. D3 is both modular and somewhat functional in style (in the JavaScript sense of functional programming, to be clear). So it was from the start a non-goal to completely expose all of D3 as *idiomatic* PureScript where a simple wrapper was sufficient.

Furthermore, i have only written those wrappers *as I needed them* to there are still *many* parts of D3 that are not covered by this eDSL.

### Non-goal: Modelling of D3 State

This might seem like a surprising choice - D3 is inherently _very_ stateful, there's state in D3, there's state in the DOM, there's statefulness in your (pure) data after you give it to D3. State everywhere. In many cases in functional programming you might try to ameliorate the dangers of this by explicitly modelling the state, using a State Monad or marking everything that changes or depends upon state as "Effect"-full.

Indeed i have tried this approach in the past. In this library i have instead striven to isolate the statefulness to only the code that uses eDSL represented by the `Selection` and `Simulation` monads. This *significantly* removes but cannot fully eliminate the issues associated with state. However, i believe it is a good compromise although much more could be said on the matter.

### Non-goal: performance optimisation that compromise readability

The performance bottlenecks in a D3 visualisation are, by their nature, going to be the assignment of attributes to (potentially millions of) DOM elements. It is likely that this code will always have to have a native implementation, ie FFI, *or* it would have to be re-written in a way that was as performant as possible in PureScript. I have chosen to use D3 for this as a low-level, well-tested, optimised layer in order to preserve readability of the visualisation code.

There definitely are other ways one could make this tradeoff, the approach taken has proved satisfactory thus far but other options would be very interesting to discuss and PRs are welcome, particularly if they are additive rather than replacements to what is here. A 100% PureScript alternative for D3's Selection, Attribute and Transition APIs would be very welcome even at a price of performance.
  
## Data Visualization for Functional Programmers

Data visualisation is a huge topic and i'm only going to make the briefest of introductions here. Let's start with a definition from a really good text book on the subject, followed by one strong example, the visualization that the late, great Hans Rosling made famous in his TED talk about human development: 

> *Computer-based visualization systems provide visual  representations of datasets designed to help people carry  out tasks more effectively*.  Visualization is suitable when there is a need to augment  human capabilities rather than replace people with computational decision-making methods. The design space  of possible vis idioms is huge, and includes the considerations of both how to create and how to interact with  visual representations. Vis design is full of trade-offs, and  most possibilities in the design space are ineffective for a  particular task, so validating the effectiveness of a design  is both necessary and difficult. Vis designers must take  into account three very different kinds of resource limitations: those of computers, of humans, and of displays.  Vis usage can be analyzed in terms of why the user needs  it, what data is shown, and how the idiom is designed.
>
> -- <cite>Munzner, Tamara. Visualization Analysis and Design (AK Peters Visualization Series) (p. 1). A K Peters/CRC Press.</cite>

* [Gapminder version](https://www.gapminder.org/tools/?from=world#$chart-type=bubbles&url=v1)
* [D3 version](https://observablehq.com/@mbostock/the-wealth-health-of-nations)
* [Hans Rosling's TED talk](https://youtu.be/hVimVzgtD6w) which is, somewhat incredibly, now 14 years old

I'm primarily concerned in this section to address common confusions or misperceptions that have arisen when i've demo-ed this stuff to other FP programmers. I'm not going to try to argue for the _utility_ of dataviz, only to get some clarification as to what it is.

### General overview: DataViz

One way of looking at data visualisation if you've never really thought about it or worked with it is that it is a form of parallelisation of the _human_ computer: all people who are lucky enough to have "normal" vision are able to pick out anomalies and/or patterns in data that is presented visually that we would find extraordinarily difficult to perceive in tabular, numeric data.

We often, and increasingly, use computers to pick out these patterns nowadays using ML and statistical tools. However, there are many circumstances where these techniques *don't* work or don't work in a *timely* or *convenient* of *ad hoc* manner. So it is still very valuable to have the possibility to use the human brain for this.

The range of _common_ data visualizations is large and the range of _possible_ data visualisations is infinite and the reason for this is that each data visualization is (or should be) specifically tuned to _make it easy for a human to perceive the important detail_.

#### DataViz for Sanity Checks

Consider this famous example known as [Anscombe's Quartet](https://en.wikipedia.org/wiki/Anscombe's_quartet).

> Anscombe's quartet comprises four data sets that have nearly identical simple descriptive statistics, yet have very different distributions and appear very different when graphed. Each dataset consists of eleven (x,y) points. They were constructed in 1973 by the statistician Francis Anscombe to demonstrate both the importance of graphing data before analyzing it, and the effect of outliers and other influential observations on statistical properties.
> -- <cite>Wikipedia</cite>

![image](https://user-images.githubusercontent.com/1260895/139079259-8ada3640-359d-4ac9-b96e-4fe47c7cf642.png)
<cite>image from [Wikipedia](https://en.wikipedia.org/wiki/File:Anscombe%27s_quartet_3.svg)</cite>

So one reason to visualize things could just be as a sanity check on statistical or geographical anomalies like the outliers in tables 3 and 4 of Anscombe's quartet.

#### Interactive DataViz for Invariants

Another reason to use dataviz techniques is something that has an analogy in one of the foundational ideas of strong static typing in Haskell-family languages and that is "to make illegal states unrepresentable".

Consider a tree structure such as a HTML page. We can represent this as text, edit it as text, give it to a browser as text and have it rendered in the browser DOM as a tree data structure. This is so familiar that it probably seems natural, inevitable, unimprovable.

I'm not going to argue that we should also use visual editors for editing HTML - there are reasons why this is a local maximum for most people, not least because we probably don't want to take our hands of the keyboard and most of the input is likely to be `text` rather than `<tags>` . But consider these two closely related implications of this approach:

* we can easily formulate malformed trees
* we can't make atomic edits to change the tree structure

In both cases we're likely either having a linter pick up the error or we're hitting save and seeing a browser refresh of the page we're working on and immediately noticing our error. But what if we were working on tree-shaped data that _didn't have a linter_ or didn't have a quick auto-reloading validation? This is the circumstance we find ourselves in with many kinds of data structures, notably many Concrete Syntax Trees.

And even if we can get instantaneous feedback that we've botched the structure it's often not that easy to see how to fix it, as anyone who's tried to re-factor a pageful of components has surely found.

So one reason we might use visualisation as an accompaniment to textual or tabular data is just this kind of sanity checking.

If the dataviz is _interactive_ however, we can enable editing that *cannot* break the structure. A tree or graph editor for example. And if we can round-trip from data-structures in our programming language of choice to editable dataviz of those structures this can be very useful.

#### DataViz for discovery and exploration

Another aspect of interactive dataviz is in seeing and exploring structures that are simply too big to examine in a debugger window or a terminal.

You're probably already very familiar with situations where you need to debug a data structure that is many levels deep and which requires clicking little dropdowns repeatedly to get to the thing you want to examine while debugging. This is useful but it won't help much if you are looking for relationships between elements in the data structure such or want to know if two nodes in a tree share a parent or what links are responsable for the cycles in a graph. In cases where a graphical representation can help you identify patterns it makes sense to use dataviz, if the overhead of producing it is advantageous.

Additionally, there are many existing examples of techniques for exploring data of the sorts you can see here:

* [Density Contour Matrix with Brushing](https://observablehq.com/@pstuffa/density-contour-matrix-with-brushing)
* [Dashboard with Brushing and Linking Example in Vega-Lite](https://observablehq.com/@weiglemc/brushing-and-linking-example-with-vega-lite)
* [Zoomable sunburst](https://observablehq.com/@d3/zoomable-sunburst)
* [Collapsable tree](https://observablehq.com/@d3/collapsible-tree)
* [Sequences sunburst](https://observablehq.com/@kerryrodden/sequences-sunburst)

(most of those examples are taken from the gallery [here](https://observablehq.com/@d3/gallery), which contains a very broad range of well-chosen examples of dataviz specifically written with D3)

#### Dataviz and Napoleon

The books of Edward Tufte are where a lot of modern dataviz as a discipline started and he invented or popularized a lot of data visualization techniques that continue to be relevant even as we've moved from paper to screens and from static to dynamic in many cases.

Amongst the early dataviz examples that he championed is a famous graphic describing Napoleon's disastrous invasion of Russia in 1812, developed by Charles Minard in 1869. The map shows six variables (longitude, latitute, number of soldiers, temperature, directions of travel) yet it is displayed coherently on the two dimensions of paper. The map and some ideas about adapting it for modern dataviz can be found [here](http://minardmap.org).

This concept of showing more than two variables on the two dimension paper, or more than three on an animated display is quite a key concept, and one which extended "dataviz" beyond mere bar charts and scatterplots. Though the latter should not be disdained if they are sufficient to the task, the possibilities are much greater.

### D3 particularly

### Further reading and examples and exploration

Tamara Munzner, [FT guide](https://ft-interactive.github.io/visual-vocabulary/), Wattenberger, Manuel Lima

Examples: [d3-graph-gallery](https://www.d3-graph-gallery.com/index.html)

## Functional Programming for Data Visualizers

The TL;DR of this section is just that you should know that there exist communities of programmers who work in these somewhat esoteric languages because we believe they give us:

* better composability of sub-programs
* better expressiveness in describing the problem domain
* better correctness in implementations (ie fewer bugs)
* better ability to evolve programs due to shifting requirements (re-factoring)

One note for the reader: the features i'm going to describe get much of their power from their combinations. It's the hardest thing to show and a significant part of the learning curve.

### General overview: PureScript

PureScript the language is part of a family of languages that are known as "statically-typed, functional programming language with type inference". I'll explain what those terms mean and why they matter in this section. This will necessarily be a very brief introduction with pointers for further reading as it is a very big topic.

PureScript is very closely related to Haskell. Unlike Haskell, PureScript has no run-time of its own and instead compiles ("transpiles") to JavaScript and runs in a JavaScript runtime such as a browser or Node.js. Other target languages and runtimes are possible and are actually in use (Erlang in particular) but this library really only works with JavaScript so i'll assume that version in this section.

So what do those phrases "statically-typed", "functional" and "type-inference" mean in this context? and why would we care if building data visualizations?

The sub-sections which follow will explain some of the terminology of pure functional programming in somewhat abstract terms and then look at concrete language elements and how they can benefit programmers.

#### Functional programming, abstractly

This most fundamental distinction means that programs in the language are exclusively built up out of "pure" functions. Pure functions are ones that will always return the same value for the same input and which have no side-effects. This restriction has enormous implications for how programs are built that are typically not obvious until you try to write a program in the language. The reason that this restriction - which makes *writing* programs harder, arguably - is worth doing is because it makes *reasoning* about programs much, much easier.

As well as being "pure", Haskell-family functions are "higher-order functions": you can write functions which take other functions as arguments and return still other functions as results. In this case, the advantage is in allowing abstractions which lessen the amount of code we have to write and understand.

Statically-typed with type-inference means something more than just checking that a function which takes, say, an `Int` as an argument is being called with an `Int` as an argument. It means that a function may, for example, take any numeric type as an argument and that the compiler will check that this is consistent with the way the function is used in the program. The compiler does a kind of forward and backward deduction process to see if your program is consistent in this way. This can catch a whole category of bugs at compile time.

Additionally, the type-system in Haskell-like languages such as PureScript uses Algebraic Data Types (ADT) which are very expressive and allow one to completely eliminate some common sources of bugs in programs such as `null` (Tony Hoare's famous "billion dollar mistake"). Expressiveness in types is advantageous because it enables us to reach towards a goal of "making illegal states unrepresentable" and have this tracked and managed by the compiler instead of defensively programming at every usage site of a more primitive type.

Each of these concepts is quite deep and has an extensive literature as well as much discussion in books that teach functional programming. These paragraphs are really just signposts to further reading if you are curious, as well as hopefully connecting claims made in the following section to specific theoretical concepts.

#### Functional programming in PureScript, concretely

Leaving programming language theory aside here are some concrete examples of these ideas in PureScript.

##### No nulls

We often have values that are uninitialized at some point in the program. Maybe we need user input to get a value for them, or maybe they can only be initialized if some other value, such as a date, validates correctly.

In languages that have `undefined`, `null`, `NaN` (which are all just different types of null) we will typically check for this before using the value. PureScript is no different except that it won't allow us to forget to check because we'll have to wrap the value in some type, probably a `Maybe` from the `Data.Maybe` library or, if it was something we were getting via, say, AJAX, it might be an `Either` from `Data.Either`

```haskell
data Maybe a = Just a | Nothing
data Either a b = Left a | Right b
```

`a` and `b` are *type variables* so you can have a `Maybe String` or a `Maybe Int` or a `Maybe Foo` or an `Either Error String` and these things are all different from one another and they're all going to force you to pattern-match them explicitly to get out what may be inside them. 

This is surprisingly lovely to use in practice, but it gets better: because `Maybe` and `Either` have *instances* of common *type-classes* we can do a range of common operations to the possible contents without tediously wrapping and unwrapping them. A `Functor` instance allows us to map some function over them. 

```haskell
add2 :: Int -> Int
add2 i = i + 2

foo :: Maybe Int
foo = Just 1

bar :: Maybe Int
bar = Nothing

fooPlusTwo = map add2 foo -- Just 3
barPlusTwo = map add2 bar -- Nothing
```

It's important to note that the `add2` function there knows absolutely nothing about unwrapping `Maybe` or `Either`. This kind of thing is what enables us to bottom-out writing very simple functions (perhaps not as simple as `add2`) and thus having more confidence in the correctness of our programs even before we write any tests.

##### Totality

Following on from this, PureScript enforces *totality* meaning not only do you have to pattern-match but you can't accidentally omit a pattern to match on.

In the case of `Maybe` you're not going to forget a match but in more application or domain-specific types, you very easily could:

```haskell
data Month = January | February | March | April | May | June | July | August | September | October | November | December
data Season = Spring | Summer | Autumn | Winter

getSeason :: Month -> Season
getSeason m = 
    case m of
        January -> Winter
        February -> Winter
        March -> Spring
        April -> Spring
        May -> Spring
        June -> Summer
        July -> Summer
        August -> Summer
        September -> Autumn
        October -> Autumn
        November -> Autumn
        December -> Winter -- omit any of these and compiler will generate error
```

##### Type-classes

I already alluded to one type-class above when I said that both `Maybe` and `Either` had "Functor instances". Let's look at what this means now and then look at some other type-classes and what they buy us.

This is the definition for the type-class Functor, from `Data.Functor`:
```haskell
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

The parameter `f` there is a *type* parameter. We can see this from the instance definition for `Maybe`:

```haskell
instance functorMaybe :: Functor Maybe where
  map fn (Just x) = Just (fn x)
  map _  _        = Nothing
```

So these type-classes are kind of like methods on a class in an Object Oriented language. In OO parlance we might call them mix-ins. However, they're not totally ad hoc, because to be useful for program composition a type class will have laws associated with it (these laws are not enforced by the compiler, that's currently above the pay-grade of the PureScript and Haskell compilers). But, if your instance is lawful, then other people who use your types will immediately be able to do quite sophisticated things with values of your types *without knowing anything about the type beforehand*.

For example, a common sort of validation involves the pattern where you get given a `List (Maybe Int)` and you want to have `Maybe (List Int)`, ie if all values were `Just`. This can be done with the library function `sequence`. But that library function does not deal with either `List` or `Maybe`, instead it is implemented in much more abstract terms.

This means that `sequence` will work just as well going from `Array (Maybe Int)` to `Maybe (Array Int)` as it did with `List` and the benefit of this is that that line of code does not need to be re-written if you later decide that you need `Array` instead of `List`. 

There is a LOT MORE to be said about type-classes, particularly the type-classes `Monoid`, `Semigroup`, `Applicative`,`Monad`, `Foldable` but the most important thing to know about them is that they form a deep structuring mechanism inside PureScript and Haskell programs that is a major source of their power.

![Pasted Graphic 6](https://user-images.githubusercontent.com/1260895/138757124-2edcdb52-ba96-4200-9acb-a3138639c0d3.png)

## Next steps: guide to other docs

This is the list of documents that i intend to write. In keeping with the useful ideas in the Divio [documentation system](https://documentation.divio.com), i'll be categorizing these into four types, as shown below.

### Tutorials

Introduce the concepts of the library using the small examples.

#### Building the Three Little Circles example

![image](https://user-images.githubusercontent.com/1260895/138757643-a5c0596d-129c-47e8-81b0-f4f2547917be.png)

#### Building the General Update Pattern example

https://user-images.githubusercontent.com/1260895/138759374-53fb1cd1-a501-460e-8e77-ed01f951e6dd.mov

#### Building the Trees example

![image](https://user-images.githubusercontent.com/1260895/138757828-b9fd83b8-0fc9-40c0-8980-68a01d82111e.png)

#### Building the Les Miserables example

![image](https://user-images.githubusercontent.com/1260895/138757898-717c38af-9811-4890-91a0-646677bd8992.png)

### How-to Guide

Enable the reader to understand how a more fully complex, app-like example such as the Spago demo is built.

#### Designing and Building the Spago App

![image](https://user-images.githubusercontent.com/1260895/138757015-9d169f46-f0ac-4e5e-8a15-da087349d09d.png)

### Explanations

More general discussions of concepts in the library and its implementation.

#### Finally Tagless Viz

How Finally Tagless encodings work and how they can be useful for eDSL's with multiple interpreters such as this library.

#### Roles and Responsabilities

Describes the particular approach to application development and the separation of concerns that is advocated / facilitated by this libary.

#### Swapping out, re-writing or augmenting D3.js

Possibility of using GraphViz for layout, of writing a PureScript native Selection monad etc etc

#### Layers of DSLs

### Reference

#### Library Architecture and API guide (probably just Pursuit once published)

## API Documentation

Generate Pursuit-style API documentation for the library:

```bash
npm run docs:api
```

This generates HTML documentation for library modules in `docs/api/`. The script:
1. Generates full docs (including dependencies) in `generated-docs/html/` using Spago
2. Filters to only include modules from `src/lib/` (the core library, excluding the website app)
3. Replaces external module links with a helpful explanation page
4. Copies the filtered docs to `docs/api/` for serving on GitHub Pages

To view the full unfiltered docs (all 834 modules including dependencies):

```bash
npm run docs:api:open
```

To generate and view the filtered docs locally:

```bash
npm run docs:api:local
```

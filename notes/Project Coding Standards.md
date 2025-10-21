# Coding Standards for PS<$>D3 project
## Overview
This repo contains a library for building D3.js-style visualizations in PureScript AND a website to explain, document and demonstrate the usage of the library.

## Directory and file structure
All directories shall be lower case EXCEPT those with PureScript modules which should match PureScript naming requirements of capitalized module names, ie module Foo.Bar.Baz is found in <src>/Foo/Bar/Baz.purs 

### /docs
The website lives in the /docs directory from where it is served by Github Pages. It would be better if this could be called 'website' but it's a limitation of Github Pages that the Pages can only be served from the root or a docs folder.

This folder contains static assets as well as compiled assets such as the bundle.js output by Spago. Eventually the CSS and the D3 js assets that are in here will also be bundled.

### /src
All PureScript code lives under the /src directory
- PureScript files for the library live in /src/lib
- PureScript files for the website live in /src/website
- There is also a /src/scripts directory which contains additional scripts for the build and publishing process.

#### /src/website
Contains
- /src/website/Component    -- Halogen components used in the website, larger components have their own sub-directories
- /src/website/HTML         -- non-Halogen reusable chunks
- /src/website/Viz          -- data viz code which uses the project library/DSL to render D3 visualizations
- Main.purs, Router.purs, Types.purs  -- top-level Halogen code for the website 

### /notes
All markdown files and other design notes live in /notes, except README.md and TODO.md
- /notes/archive contains outdated design documents, plans and notes, kept for reference

### /output
Contains compiled outputs from PureScript code

### /node_modules
Contains all npm-installed packages

## Coding standards and conventions

### Lenses have a PREFIX underscore
As with common practice in PureScript we use `_lensName` format in naming lenses. 

In this project we use lenses primarily to manipulate Halogen State and Visualization Model State, because they're a composable way of reaching into potentially complex model state wihout decomposing it and reassembling the results. This dramatically simplifies code readability (albeit at a price of deeper code comprehensibility). It also, but less importantly, helps retain some structural similarity to the antecedent D3 script style.

Example:
`
  Scene PackageGrid -> do
    _chooseNodes     .= allNodes
    _linksShown      .= isM2P_Link
    _linksActive     .= const true
    _cssClass        .= "cluster"
    _sceneAttributes .= clusterSceneAttributes
    _forceStatuses   %= onlyTheseForcesActive [ "clusterx_P", "clustery_P", "clusterx_M", "clustery_M", "collide2" ]
    _nodeInitializerFunctions .= [ unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY ]
    runSimulation
`
### Foreign functions (and Types) have a POSTFIX underscore
This is intended as a mnemonic that everywhere you see this naming convention in the PureScript D3 DSL, there is potentially type unsafety. See the About.md document for an exploration of why this approach is favored, at least as long as the system is implemented under the hood by D3.js.

Example:
`foreign import d3SelectFirstInDOM_   :: Selector D3Selection_    -> D3Selection_
`

### Datum_ and datum_

Fundamentally we are using D3 to mutate data which we largely represent on the PureScript side as immutable. The reasons for this are all about syntactic lightness - this comes after earlier versions which took a more ideologically pure functional programming approach to wrapping the APIs. It is certainly possible to do but it is IMHO massive overkill and unlikely to ever be used by anybody as a library. 

Instead what we do is, is leverage the fact that D3 is generally mutating the data in two quite specific ways (it varies by visualizations). This wouldn't even matter to us except that we want to interact with this data, or use user interactions with the drawn elements to do something else in our program. 

Firstly, it is extending the individual data elements, giving them `x`and `y` (and `x1`/ `y1` / `x2` / `y2`) fields as a result of some layout algorithm.

Secondly, it is changing values, again such as `x` and ¨y`, as it unfolds transitions, animates changes, runs a force layout algorithm etc.

What it is not doing is removing fields from our data or changing the data itself.

So we have a need to access our data elements ("datum", singular of data) after they've been modified but we can make a contract with the PureScript programmer to specify exactly what will be available. And we do this by declaring a record of conversion functions, most of which are simply field accessors plus an `UnsafeCoerce`. D3 is not super-consistent about this itself, some layouts such as hierarchies embed the original data as a field inside the D3 object, others simply extend whatever they've been given.

If at some point, the D3 parts of this library were re-written, which might happen, probably a much cleaner solution with greater consistency could be implemented. For now, the idea is that the person writing the `datum_` accessors in the Visualisation's Model module must understand this to some extent but it would be possible for developers further down stream, writing and editing visualisations, not to be concerned with these issues.

### FFI and JavaScript files
Fundamentally all raw JavaScript is considered technical debt in this project, but pragmatism dictates the amount is probably never going to be zero.

All FFI that is concerned with the underlying D3.js library should be in the /lib/D3/FFI.purs and /lib/D3/FFI.js pair of files. Similarly, other necessary FFI should be amalgamated according to the specific JavaScript library that it's using, such as Prism.js or whatever.

It's acceptable during development to accumulate some technical debt here with short-lived .js FFI files on an ad-hoc basis but these should be periodically reviewed and swept up into cleaner abstractions which are then used and the small local .js FFI files excised.

### Debug and Console
For console debugging it is preferred to use the purescript-debug library as this generates a custom warning on the function which uses it, aiding us not to ship production code with debug statements in it. Generally usage of the form `x = spy "what's going on here?" $ functionWhichGivesUsX` is preferred. 

### The Simulation Monad
It's quite easy to get confused between the State that's being managed by Halogen and the State that's being managed by the D3Simulation monad. See commit hash f570f0844c59113b5585c0d99aa71c811bf3043a for some commentary

The management of events to and from Simulations is one of the hardest and most complicated things in the library - however the Spago example has shown that it's possible to do things with D3 using this library that have rarely if ever been attempted from JavaScript. There's probably work to do here to clarify and abstract this complexity out of such apps.

Of particular interest in the Spago example is the concept of mise-en-scene: a configuration record for a particular view of the model, the forces acting on it and the styling.
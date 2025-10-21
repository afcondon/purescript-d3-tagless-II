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
We use lenses primarily to manipulate Halogen State in the Visualizations, because they're a composable way of reaching into potentially complex model state wihout decomposing it and reassembling the results. This dramatically simplifies code readability (albeit at a price of deeper code comprehensibility). It also, but less importantly, helps retain some structural similarity to the antecedent D3 script style.

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

### FFI and JavaScript files
Fundamentally all raw JavaScript is considered technical debt in this project, but pragmatism dictates the amount is probably never going to be zero.

All FFI that is concerned with the underlying D3.js library should be in the /lib/D3/FFI.purs and /lib/D3/FFI.js pair of files. Similarly, other necessary FFI should be amalgamated according to the specific JavaScript library that it's using, such as Prism.js or whatever.

It's acceptable during development to accumulate some technical debt here with short-lived .js FFI files on an ad-hoc basis but these should be periodically reviewed and swept up into cleaner abstractions which are then used and the small local .js FFI files excised.

### Debug and Console
For console debugging it is preferred to use the purescript-debug library as this generates a custom warning on the function which uses it, aiding us not to ship production code with debug statements in it. Generally usage of the form `x = spy "what's going on here?" $ functionWhichGivesUsX` is preferred. 
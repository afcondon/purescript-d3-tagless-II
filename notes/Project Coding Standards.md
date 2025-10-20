# Coding Standards for PSD3 project
## Overview
This repo contains a library for building D3.js visualizations in PureScript AND a website to explain, document and demonstrate the usage of the library.

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

# Restructure Repo

## Standards
All directories shall be lower case EXCEPT those with PureScript modules which should match PureScript naming requirements of capitalized module names, ie module Foo.Bar.Baz is found in <src>/Foo/Bar/Baz.purs 

## Desired structure
-[] only one markdown file at top level: TODO.md
-[] docs directory - which is the root for Github pages site contains all static and generated website assets
    - code-snippets, generated from the repository Purescript code by a script
    - data, the data files that are needed by some pages
    - d3, the d3 js files that are needed for the website
        - currently this is d3.color.js
        - d3.interpolate.js
        - d3.scale-chromatic.js
        - d3.js (TODO check what minimum set really should be)
    - styles, seems like this should only be 
        - components.css
        - fullscreen.css
        - main.css
        - spago-forces.css
        - spago.css
    - index.html, which needs to have a pure HTML backup for screen-readers and browsers without JavaScript. Probably a text version of the "About" blog post.
-[] node_modules, for all the npm resources needed to build projects
-[] notes, for all markdown and text files, any documentation that is generated during development of the project and not intended for external consumption
-[] output, where all the compiled PureScript lives
-[] src, the source for the library, scripts that are used to create artefacts in the repo and the website that showcases the library on GitHub Pages
    -[] lib, the FFI for the DSL, the interpreters for the DSL, and any other PureScript code used in the library
    -[] scripts, such as the snippet creating script that populates the docs/code-snippets directory for the website to use
    -[] website, all the Halogen-based PureScript code that is necessary to build the website
        -[] apps, Halogen elements that amount to individual SPAs, such as the front page navigation and the Spago example and later perhaps others
        -[] components, Halogen elements used (potentially) in multiple webpages
        -[] pages, Halogen elements that correspond directly to Routes or simple Pages in the Website (realize that the distinction is a bit fuzzy in a SPA)
        -[] Main.purs, module PSD3.Main, the entry point for the website in the bundle.js
        -[] Router.purs, module PSD3.Routes, the routes for the website
        -[] Types.purs, module PSD3.Types, as used by Main.purs
-[] test, for the tests that we might have some day
-[] .gitignore
-[] .gitmodules
-[] package-lock.json and package.json, for installation and development, keep as simple as possible, build scripts in package.json should amount to no more than the following
    "scripts": {
        "build": "spago build",
        "install": "spago install",
        "upgrade-set": "spago upgrade-set",
        "snippets": "node src/scripts/extract-snippets.js",
        "bundle": "spago bundle-app --main PSD3.Main --to docs/bundle.js",
        "dev": "npm run build && npm run bundle"
    },
-[] packages.dhall, needed by spago, the Purescript repository tool
-[] spago.dhall, specifies the PureScript dependencies of the repo
-[] TODO.md, single source of truth for planning and coordinating work on the repo

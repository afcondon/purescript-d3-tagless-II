Bubblepack visualization - leveraging the existing, working complex force-layout visualizations code-explorer and lesmisgup but NOT importing any of their code (copying is fine, no dependencies) we will implement a dynamic navigator for the /docs/data/spago-data.

The initial view will be a force layout diagram with our code (the pseudo-package "my-project") at the center and all the packages that it depends on in orbit around it using a radial force.

Each package will be a bubblepack with four levels of hierarchy - the outer, the package itself. next level, the modules within it. next level the categories of module contents, inner level the contents (type, data and newtype definitions; type classes; instances; functions; foreign data definitions, foreign function definitions). Colour scheme for the packages, pastels. Colour scheme inside the bubblepack hierarchy bright and clear.

Hovering over a package will result in highlighting it's dependencies which we'll do by desaturating the NON-dependencies and/or making their opacity lower.

Clicking on a package will cause it to "disolve" and all other packages to disappear. Now instead there will be a force layout of the modules of that package with the module dependencies instead of the package dependencies.

Hovering over an inner circle such as a function will now cause dependency highlighting to be applied to the circle in question, if it's a function then other modules that use it will be highlighted and those that don't will be faded as for the package highlight. 

There will be a floating panel of controls in the top left, which will have at a minimum a button to return to the package view.

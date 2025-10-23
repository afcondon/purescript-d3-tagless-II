# Vision doc

1. Enhancements and prettification of examples
    1. Simple charts: each one should be nicer
        1. Line chart: multi-line with highlighting
        2. Bar chart: fix first bar offset; interesting data and stacked bar chart or something
        3. Lose the basic scatter plot, Anscombe is a fine demo for this
    2. Chord diagram: add some labels, make sure the data is interesting, respect the height to keep all visible
    3. Bubble chart: make interactive?, respect height to keep visible, fix labelling (drop tiny labels, labels that don't fit in their circle) use our own data, stretch goal live transition to tree, treemap, graph
    4. Treemap: add this viz
    5. Sankey: add margins, add highlighting, would be really nice to add some feature that highlights the "primary energy fallacy" as a piece of climate advocacy, implies finding and using more up to date data
    6. Tree layouts:
        1. First one (the one that incorporates all three layouts) isn't working at the moment, debug
        2. Horizontal: needs centering; needs zoom
        3. Vertical: also needs centering; needs zoom; needs labels rotated 45˚ to be readable
        4. Radial: needs centering and zoom; needs a control to rotate it OR when you click on a label it rotates automatically so that label is at the 3 o'clock position?
    7. Three Little Circles - this example should go into the About Page, along with some progressively more complicated examples. Therefore consider this as part of an enhanced About/Explanation page
    8. General Update Pattern - currently broken, stops after three updates; like Three Little Circles this belongs in an enhanced About page
    9. Les Mis example - currently broken, needs rewrite with new SimulationM API we developed; consider this part of a larger package of work on Force Layout and simulation
    10. Meta-Tree: this is another major package of work; see section below
    11. String Interpreter: show string interpreters PLURAL, one that emits D3, one that gives an english lang version of the visualisation
    12. Spago - example is currently not working but that's because it's now a kind of semi-standalone app (which is working)

2. Examples conclusion of points above is
    1. Remove the indirection thru Gallery - just directly show simplest examples, along with their code embedded in an explanatory page. This would be "three little circles", "small parabola of circles", line, bar, anscombe, chord, simpler bubble chart, simpler/smaller tree (just one would suffice), Sankey, small/simple Treemap, general update pattern, Les Mis
    2. A comprehensive hierarchy display page showing the same data switchable between three types of tree, circle packing, treemap and any others that apply. Code overlay panels and control panel
    3. Meta-tree page: see vision for this below

3. String interpreters page: explanation with diagram showing that PureScript code for a visualisation can be interpreted as an AST because Finally Tagless and instead of drawing the visualisation you can translate it or cause other actions to occur. Would it help to generate two pseudo-code outputs in different languages? can think about what to do here

4. Hierarchical data display page
    1. one example live on the page, control panel allows switching between h tree, v tree, r tree, circle pack, treemap
    2. stretch goal: animate transitions between layouts, probably just retaining the labels in some cases but between trees it would be nice to keep all elements and just move nodes and rotate the labels. 
    3. stretch goal: final boss, animate the tree into a force layout graph

5. Meta tree page
    1. In this repo this will just be a description but longterm vision here is to build an interactive tool that allows creation of a visualization AST. This would be a separate repo once the library is mature and available as a package from npm etc
    2. Key idea here is the formal grammar that we've developed above D3 which is expressed in our Selection Monad 
    3. If we find a way to add code to the nodes, then in principle (modulo setting up backend compilation of PureScript like the try.purescript.org site which proves this is possible) we could create a WYSIWYG visualization editor, at least a toy one as a demonstrator. 
    4. If it's a toy, then we can significantly simplify the task by having some canned pieces of viz code such as "a green circle with radius of log(d.value)", "a label with such-and-such offsets". Just spitballing here but i think it'd be possible to give a sort of simple LEGO pieces approach that would enable different data visualisations to be created graphically. 

6. Spago
    1. We should rename this "Code Explorer"
    2. As with Meta Tree this will eventually be a standalone app once library is mature and published as a package
    3. The backend will be a data base with a Node/PureScript or Haskell server.
    4. The database will be loaded by a COMPREHENSIVE dependency analysis of a Haskell/PureScript codebase: ie every line of code that has a dependency on a type or function will be captured
    5. combined with well-designed filters and drag and drop tools, this will enable dynamic re-factoring of types and functions from module to module, in principle
    6. even if drag and drop isn't implemented it should be possible to directly see code that is living in the "wrong" package, causing unnessary compilations and difficulties in writing code
    

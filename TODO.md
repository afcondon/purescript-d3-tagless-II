# TODO

## Feature Enhancements

### 1. Add simple two dimensional charts (line, bar, scatterplot) example
Add examples demonstrating basic 2D chart types using the Finally Tagless DSL:
- Line chart with axes and transitions
- Bar chart with dynamic data updates
- Scatterplot with interactive tooltips
- Demonstrate the General Update Pattern for each chart type
- enhance the simple line chart to a multi-line chart with highlighting

### 2. Add a Sankey Diagram example
Implement a Sankey diagram visualization to show flow data:
- Wrap D3's Sankey layout in the Finally Tagless API
- Create example data showing energy flows or data pipelines
- Add interactive features (hover states, click handlers)
- Integrate as a new Story in the demo application

### 3. Improve comments and inline documentation
Enhance code documentation throughout the codebase BUT ALSO if documenation is complex prefer refactor of DSL 
- datum_:
    - document the datum_ pattern
    - rationalize it thru ALL examples
    - simplify syntax and pattern if possible
- Add module-level documentation to all core modules
- Document the Finally Tagless pattern and how to extend it
- Add inline comments for complex FFI interactions
- Document the relationship between capabilities and interpreters
- Add usage examples in documentation comments
- can we get the comments to appear on hover over each line of code or something? or attach 'i' icons to sections?
- make the four kinds of documentation: API/reference, overview, tutorial, cookbook

### 4. Replace Perl scripts with PureScript code for dependency extraction
Create PureScript tooling to derive project dependencies for the Spago example:
- Replace existing Perl scripts with idiomatic PureScript code
- Use spago's graph command to get the data for the project
- Generate the data structure consumed by the Spago force-directed graph visualization OR adapt the Spago example to consume the output of 'spago graph'
- Consider using `purescript-dhall` library for parsing
- Add as a build step or standalone tool

### 5. Add database and data-serving app
Create a backend service to serve visualization data:
- Set up a database (PostgreSQL/SQLite) to store example datasets
- Create a simple HTTP API server (using Servant or similar)
- Migrate example data from static files to database
- Update demo stories to fetch data via HTTP requests
- Consider using `purescript-affjax` for client-side requests
- Add development setup instructions for running the data server

### 6. Add Chord Diagram example
Implement a Chord diagram vizualization to show dependency data
- Wrap D3's Chord layout in the Finally Tagless API
- Use example data from flare-2.json for now (later use project's own dependencies)
- Integrate as a new Story in the demo application

### 7. Add Bubble Chart example
Implement a Bubble chart vizualization to show dependency data
- Wrap D3's Bubble chart in the Finally Tagless API
- Use example data from flare-2.json for now (later use project's own dependencies)
- Integrate as a new Story in the demo application

### 8. Replace Ocelot-derived panes with modern CSS single page
Replace left hand sidebar with thumbnails of the examples, then make single page for each example with good CSS choices for readability on different screensizes
- Each example should have title and some or all of the about text at the top then the chart and then the code snippet example
- see if we can delete all of the Ocelot-derived stuff from the project once this is implemented

### 9. Add e-charts support
Alternative to D3 for some uses and there is an existing Purescript wrapper for it at https://github.com/lucasdicioccio/purescript-halogen-echarts-simple?tab=readme-ov-file

### 10. Enhance String Interpreter
The string interpreter could - maybe, in principle - emit D3 Javascript code
- first cut could just be to overhaul the output a LOT for clarity, newlines, etc etc
- secondly, we could try to make it as D3-ish as possible, at least to the point of pseudocode
- stretch goal, multiple string interpreters, do echarts as well? Emit a succinct english paragraph describing how the visualisation is built

### 11. Generating the site using PureScriptD3 
- write a parser to take markdown to tree data and allow it to be displayed as either (1) text (2) tree layout (3) force layout

### 12. Home page overhaul
- use the above to make homepage from a markdown file
- key elements: what, why, how, code, examples, apps, finally tagless
- screen readers and those without javascript - ensure the main text appears and is navigable, explain what a sighted user would see and do on the site. stretch goal: think about sonic "visualisations"

### 13. Further data visualizations beyond the basics
1. https://blocks.roadtolarissa.com/emeeks/894ff63d02551badfc75536d77dcd49c
2. https://setosa.io/simpsons/
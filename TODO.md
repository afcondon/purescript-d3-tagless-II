# TODO

## Feature Enhancements

### 1. Add simple two dimensional charts (line, bar, scatterplot) example
Add examples demonstrating basic 2D chart types using the Finally Tagless DSL:
- Line chart with axes and transitions
- Bar chart with dynamic data updates
- Scatterplot with interactive tooltips
- Demonstrate the General Update Pattern for each chart type

### 2. Add a Sankey Diagram example
Implement a Sankey diagram visualization to show flow data:
- Wrap D3's Sankey layout in the Finally Tagless API
- Create example data showing energy flows or data pipelines
- Add interactive features (hover states, click handlers)
- Integrate as a new Story in the demo application

### 3. Improve comments and inline documentation
Enhance code documentation throughout the codebase:
- Add module-level documentation to all core modules
- Document the Finally Tagless pattern and how to extend it
- Add inline comments for complex FFI interactions
- Document the relationship between capabilities and interpreters
- Add usage examples in documentation comments

### 4. Replace Perl scripts with PureScript code for dependency extraction
Create PureScript tooling to derive project dependencies for the Spago example:
- Replace existing Perl scripts with idiomatic PureScript code
- Parse `spago.dhall` and `packages.dhall` to extract dependency graph
- Generate the data structure consumed by the Spago force-directed graph visualization
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

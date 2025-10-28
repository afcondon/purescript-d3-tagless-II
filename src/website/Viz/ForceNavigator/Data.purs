module D3.Viz.ForceNavigator.Data where

import D3.Viz.ForceNavigator.Model (Category(..), NavigationRawModel, NodeType(..))
import PSD3.Data.Node (D3Link_Unswizzled, D3_SimulationNode(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)

-- | Helper to construct unswizzled links
packLink :: forall r. { source :: String, target :: String | r } -> D3Link_Unswizzled
packLink = unsafeCoerce

-- | All navigation nodes
navigationData :: NavigationRawModel
navigationData = {
  nodes: [
    -- Central root node
    D3SimNode {
      id: "purescript-d3"
    , label: "PureScript D3"
    , nodeType: Center
    , category: Nothing
    , children: Just ["gallery", "about", "spago", "interpreters", "github"]
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: notNull 0.0  -- Fix center node
    , fy: notNull 0.0
    }

    -- Main sections
  , D3SimNode {
      id: "gallery"
    , label: "Gallery"
    , nodeType: Section
    , category: Nothing
    , children: Just [
        "line-chart", "bar-chart", "scatter-plot", "scatter-quartet",
        "chord-diagram", "bubble-chart", "sankey",
        "tree", "tree-horizontal", "tree-vertical", "tree-radial",
        "three-little-circles", "gup", "les-mis"
      ]
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "about"
    , label: "About"
    , nodeType: Section
    , category: Nothing
    , children: Just ["type-safe", "composable", "interpreters-feature", "d3-powered", "interactive-feature", "documented"]
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "spago"
    , label: "Spago Explorer"
    , nodeType: Section
    , category: Nothing
    , children: Nothing
    , url: Just "#/spago"
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "interpreters"
    , label: "Interpreters"
    , nodeType: Section
    , category: Nothing
    , children: Just ["meta-tree", "print-tree"]
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "github"
    , label: "GitHub"
    , nodeType: Section
    , category: Nothing
    , children: Nothing
    , url: Just "https://github.com/afcondon/purescript-d3-tagless"
    , external: Just true
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

    -- Gallery children - Basic Charts
  , D3SimNode {
      id: "line-chart"
    , label: "Line Chart"
    , nodeType: Example
    , category: Just BasicChart
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "bar-chart"
    , label: "Bar Chart"
    , nodeType: Example
    , category: Just BasicChart
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "scatter-plot"
    , label: "Scatter Plot"
    , nodeType: Example
    , category: Just BasicChart
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "scatter-quartet"
    , label: "Anscombe's Quartet"
    , nodeType: Example
    , category: Just BasicChart
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

    -- Gallery children - Advanced Layouts
  , D3SimNode {
      id: "chord-diagram"
    , label: "Chord Diagram"
    , nodeType: Example
    , category: Just AdvancedLayout
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "bubble-chart"
    , label: "Bubble Chart"
    , nodeType: Example
    , category: Just AdvancedLayout
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "sankey"
    , label: "Sankey Diagram"
    , nodeType: Example
    , category: Just AdvancedLayout
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "tree"
    , label: "Tree Layout"
    , nodeType: Example
    , category: Just AdvancedLayout
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "tree-horizontal"
    , label: "Horizontal Tree"
    , nodeType: Example
    , category: Just AdvancedLayout
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "tree-vertical"
    , label: "Vertical Tree"
    , nodeType: Example
    , category: Just AdvancedLayout
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "tree-radial"
    , label: "Radial Tree"
    , nodeType: Example
    , category: Just AdvancedLayout
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

    -- Gallery children - Interactive
  , D3SimNode {
      id: "three-little-circles"
    , label: "Three Little Circles"
    , nodeType: Example
    , category: Just Interactive
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "gup"
    , label: "General Update Pattern"
    , nodeType: Example
    , category: Just Interactive
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "les-mis"
    , label: "Les Misérables Network"
    , nodeType: Example
    , category: Just Interactive
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

    -- Interpreters children
  , D3SimNode {
      id: "meta-tree"
    , label: "MetaTree Visualizer"
    , nodeType: Example
    , category: Just Interpreter
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "print-tree"
    , label: "String Generator"
    , nodeType: Example
    , category: Just Interpreter
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Nothing
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

    -- About children (features)
  , D3SimNode {
      id: "type-safe"
    , label: "Type-Safe"
    , nodeType: Feature
    , category: Nothing
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Just "Strong type safety with PureScript"
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "composable"
    , label: "Composable"
    , nodeType: Feature
    , category: Nothing
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Just "Build complex visualizations from simple components"
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "interpreters-feature"
    , label: "Multiple Interpreters"
    , nodeType: Feature
    , category: Nothing
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Just "Finally Tagless pattern enables different interpretations"
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "d3-powered"
    , label: "D3-Powered"
    , nodeType: Feature
    , category: Nothing
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Just "Leverages D3.js for battle-tested rendering"
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "interactive-feature"
    , label: "Interactive"
    , nodeType: Feature
    , category: Nothing
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Just "Support for drag, zoom, and other behaviors"
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }

  , D3SimNode {
      id: "documented"
    , label: "Well-Documented"
    , nodeType: Feature
    , category: Nothing
    , children: Nothing
    , url: Nothing
    , external: Nothing
    , description: Just "Comprehensive examples with comparisons"
    , x: 0.0
    , y: 0.0
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    }
  ],

  links: [
    -- Center to sections
    packLink { source: "purescript-d3", target: "gallery" }
  , packLink { source: "purescript-d3", target: "about" }
  , packLink { source: "purescript-d3", target: "spago" }
  , packLink { source: "purescript-d3", target: "interpreters" }
  , packLink { source: "purescript-d3", target: "github" }

    -- Gallery to examples
  , packLink { source: "gallery", target: "line-chart" }
  , packLink { source: "gallery", target: "bar-chart" }
  , packLink { source: "gallery", target: "scatter-plot" }
  , packLink { source: "gallery", target: "scatter-quartet" }
  , packLink { source: "gallery", target: "chord-diagram" }
  , packLink { source: "gallery", target: "bubble-chart" }
  , packLink { source: "gallery", target: "sankey" }
  , packLink { source: "gallery", target: "tree" }
  , packLink { source: "gallery", target: "tree-horizontal" }
  , packLink { source: "gallery", target: "tree-vertical" }
  , packLink { source: "gallery", target: "tree-radial" }
  , packLink { source: "gallery", target: "three-little-circles" }
  , packLink { source: "gallery", target: "gup" }
  , packLink { source: "gallery", target: "les-mis" }

    -- About to features
  , packLink { source: "about", target: "type-safe" }
  , packLink { source: "about", target: "composable" }
  , packLink { source: "about", target: "interpreters-feature" }
  , packLink { source: "about", target: "d3-powered" }
  , packLink { source: "about", target: "interactive-feature" }
  , packLink { source: "about", target: "documented" }

    -- Interpreters to examples
  , packLink { source: "interpreters", target: "meta-tree" }
  , packLink { source: "interpreters", target: "print-tree" }
  ]
}

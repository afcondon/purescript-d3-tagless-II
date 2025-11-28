module PSD3.Reference.ModuleRegistry where

import Prelude

import Data.Array (filter)

-- | Module metadata for the reference documentation
type ModuleInfo =
  { name :: String          -- Display name
  , path :: String          -- File path relative to src/lib
  , description :: String   -- Short description
  }

-- | Category of modules
type ModuleCategory =
  { title :: String
  , modules :: Array ModuleInfo
  }

-- | All library modules organized by category
moduleCategories :: Array ModuleCategory
moduleCategories =
  [ { title: "Core API"
    , modules:
        [ { name: "PSD3.Types"
          , path: "PSD3/Types.purs"
          , description: "Common type definitions used throughout PSD3"
          }
        , { name: "PSD3.Attributes"
          , path: "PSD3/Attributes.purs"
          , description: "SVG and HTML attribute combinators"
          }
        ]
    }
  , { title: "Capabilities"
    , modules:
        [ { name: "PSD3.Capabilities.Selection"
          , path: "PSD3/Capabilities/Selection.purs"
          , description: "Core selection and element manipulation operations"
          }
        , { name: "PSD3.Capabilities.Simulation"
          , path: "PSD3/Capabilities/Simulation.purs"
          , description: "Force simulation capabilities"
          }
        , { name: "PSD3.Capabilities.Sankey"
          , path: "PSD3/Capabilities/Sankey.purs"
          , description: "Sankey diagram layout capabilities"
          }
        ]
    }
  , { title: "Interpreters"
    , modules:
        [ { name: "PSD3.Interpreter.D3"
          , path: "PSD3/Interpreter/D3.purs"
          , description: "D3.js interpreter - runs visualizations in the browser"
          }
        , { name: "PSD3.Interpreter.String"
          , path: "PSD3/Interpreter/String.purs"
          , description: "String interpreter - generates code as text"
          }
        , { name: "PSD3.Interpreter.MetaTree"
          , path: "PSD3/Interpreter/MetaTree.purs"
          , description: "MetaTree interpreter - produces AST representation"
          }
        ]
    }
  , { title: "Data Structures"
    , modules:
        [ { name: "PSD3.Data.Tree"
          , path: "PSD3/Data/Tree.purs"
          , description: "Tree data structure for hierarchical layouts"
          }
        , { name: "PSD3.Data.Node"
          , path: "PSD3/Data/Node.purs"
          , description: "Node types for tree structures"
          }
        , { name: "PSD3.Data.Utility"
          , path: "PSD3/Data/Utility.purs"
          , description: "Utility functions for data manipulation"
          }
        ]
    }
  , { title: "Internal - Selection"
    , modules:
        [ { name: "PSD3.Internal.Selection.Types"
          , path: "PSD3/Internal/Selection/Types.purs"
          , description: "Selection type definitions"
          }
        , { name: "PSD3.Internal.Selection.Functions"
          , path: "PSD3/Internal/Selection/Functions.purs"
          , description: "Selection implementation functions"
          }
        ]
    }
  , { title: "Internal - Simulation"
    , modules:
        [ { name: "PSD3.Internal.Simulation.Types"
          , path: "PSD3/Internal/Simulation/Types.purs"
          , description: "Simulation type definitions"
          }
        , { name: "PSD3.Internal.Simulation.Functions"
          , path: "PSD3/Internal/Simulation/Functions.purs"
          , description: "Simulation implementation functions"
          }
        , { name: "PSD3.Internal.Simulation.Forces"
          , path: "PSD3/Internal/Simulation/Forces.purs"
          , description: "Force definitions for simulations"
          }
        , { name: "PSD3.Internal.Simulation.Config"
          , path: "PSD3/Internal/Simulation/Config.purs"
          , description: "Simulation configuration"
          }
        ]
    }
  , { title: "Internal - Other"
    , modules:
        [ { name: "PSD3.Internal.Types"
          , path: "PSD3/Internal/Types.purs"
          , description: "Internal type definitions"
          }
        , { name: "PSD3.Internal.FFI"
          , path: "PSD3/Internal/FFI.purs"
          , description: "Foreign function interface to D3.js"
          }
        , { name: "PSD3.Internal.Attributes.Instances"
          , path: "PSD3/Internal/Attributes/Instances.purs"
          , description: "Type class instances for attributes"
          }
        , { name: "PSD3.Internal.Attributes.Sugar"
          , path: "PSD3/Internal/Attributes/Sugar.purs"
          , description: "Syntactic sugar for attributes"
          }
        , { name: "PSD3.Internal.Axes"
          , path: "PSD3/Internal/Axes.purs"
          , description: "Axis generation"
          }
        , { name: "PSD3.Internal.Hierarchical"
          , path: "PSD3/Internal/Hierarchical.purs"
          , description: "Hierarchical layout algorithms"
          }
        , { name: "PSD3.Internal.Generators.Line"
          , path: "PSD3/Internal/Generators/Line.purs"
          , description: "Line and area generators"
          }
        , { name: "PSD3.Internal.Scales.Scales"
          , path: "PSD3/Internal/Scales/Scales.purs"
          , description: "Scale functions"
          }
        , { name: "PSD3.Internal.Scales.Linear"
          , path: "PSD3/Internal/Scales/Linear.purs"
          , description: "Linear scale implementation"
          }
        , { name: "PSD3.Internal.Sankey.Types"
          , path: "PSD3/Internal/Sankey/Types.purs"
          , description: "Sankey diagram types"
          }
        , { name: "PSD3.Internal.Sankey.Functions"
          , path: "PSD3/Internal/Sankey/Functions.purs"
          , description: "Sankey diagram functions"
          }
        , { name: "PSD3.Internal.Utility"
          , path: "PSD3/Internal/Utility.purs"
          , description: "Internal utility functions"
          }
        , { name: "PSD3.Internal.Zoom"
          , path: "PSD3/Internal/Zoom.purs"
          , description: "Zoom and pan behavior"
          }
        ]
    }
  ]

-- | Get all modules flattened
allModules :: Array ModuleInfo
allModules = moduleCategories >>= _.modules

-- | Find a module by name
findModule :: String -> Array ModuleInfo -> Array ModuleInfo
findModule name = filter (\m -> m.name == name)

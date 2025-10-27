module PSD3.CodeAtlas.Types where

import Prelude

-- | Active tab in Code Atlas
data AtlasTab
  = DeclarationsTab    -- Table browser with search/filters
  | VisualizationTab   -- Static graph visualization (TBD)

derive instance eqAtlasTab :: Eq AtlasTab

instance showAtlasTab :: Show AtlasTab where
  show DeclarationsTab = "Declarations"
  show VisualizationTab = "Visualization"

-- | Declaration metadata from declarations.json
type Declaration =
  { title :: String
  , kind :: String  -- value | data | typeClass | typeSynonym | alias | externData
  , module :: String
  , comments :: String
  , sourceSpan :: { start :: Array Int, end :: Array Int }
  }

-- | Module with its declarations
type ModuleDeclarations =
  { name :: String
  , comments :: String
  , declarations :: Array Declaration
  }

-- | Kind statistics
type KindStat =
  { kind :: String
  , count :: Int
  }

-- | Complete declarations data structure
type DeclarationsData =
  { modules :: Array ModuleDeclarations
  , stats ::
      { totalModules :: Int
      , totalDeclarations :: Int
      , byKind :: Array KindStat
      }
  }

-- | Function call data from function-calls.json
type FunctionCall =
  { target :: String
  , targetModule :: String
  , identifier :: String
  , isCrossModule :: Boolean
  }

type FunctionInfo =
  { name :: String
  , module :: String
  , calls :: Array FunctionCall
  , calledBy :: Array String
  }

type FunctionCallsData =
  { functions :: Array { key :: String, value :: FunctionInfo }
  , stats ::
      { totalFunctions :: Int
      , totalCalls :: Int
      , crossModuleCalls :: Int
      }
  }

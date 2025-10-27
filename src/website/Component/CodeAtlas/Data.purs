module PSD3.CodeAtlas.Data where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import PSD3.CodeAtlas.Types (DeclarationsData, FunctionCallsData)

-- | Load declarations.json (stubbed for now - we'll implement proper loading later)
loadDeclarations :: Aff (Either String DeclarationsData)
loadDeclarations = do
  -- For now, return stub data so the component renders
  -- TODO: Implement actual JSON loading with Simple.JSON
  pure $ Right
    { modules: []
    , stats:
        { totalModules: 0
        , totalDeclarations: 0
        , byKind: []
        }
    }

-- | Load function-calls.json (stubbed for now)
loadFunctionCalls :: Aff (Either String FunctionCallsData)
loadFunctionCalls = do
  -- For now, return stub data
  -- TODO: Implement actual JSON loading
  pure $ Right
    { functions: []
    , stats:
        { totalFunctions: 0
        , totalCalls: 0
        , crossModuleCalls: 0
        }
    }

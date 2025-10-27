module PSD3.CodeAtlas.Data where

import Prelude

import Affjax.Web as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json, toObject, toString, toNumber, toArray, toBoolean)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import PSD3.CodeAtlas.Types (Declaration, DeclarationsData, FunctionCall, FunctionCallsData, FunctionInfo, KindStat, ModuleDeclarations)

-- | Load declarations.json
loadDeclarations :: Aff (Either String DeclarationsData)
loadDeclarations = do
  result <- Affjax.get ResponseFormat.json "data/spago-data/declarations.json"
  case result of
    Left err -> pure $ Left $ "Failed to load declarations.json"
    Right response -> pure $ parseDeclarations response.body

-- | Load function-calls.json
loadFunctionCalls :: Aff (Either String FunctionCallsData)
loadFunctionCalls = do
  result <- Affjax.get ResponseFormat.json "data/spago-data/function-calls.json"
  case result of
    Left err -> pure $ Left $ "Failed to load function-calls.json"
    Right response -> pure $ parseFunctionCalls response.body

-- | Parse declarations JSON
parseDeclarations :: Json -> Either String DeclarationsData
parseDeclarations json = do
  obj <- note "Root not an object" $ toObject json

  -- Parse modules
  modulesObj <- note "Missing modules" $ Object.lookup "modules" obj >>= toObject
  let modulesArray = Object.toUnfoldable modulesObj :: Array (Tuple String Json)
  modules <- traverse parseModule modulesArray

  -- Parse stats
  statsObj <- note "Missing stats" $ Object.lookup "stats" obj >>= toObject
  totalModules <- note "Missing totalModules" $ Object.lookup "totalModules" statsObj >>= toNumber <#> floor
  totalDecls <- note "Missing totalDeclarations" $ Object.lookup "totalDeclarations" statsObj >>= toNumber <#> floor
  byKindObj <- note "Missing byKind" $ Object.lookup "byKind" statsObj >>= toObject

  let byKind = Object.toUnfoldable byKindObj <#> \(Tuple kind countJson) ->
        { kind
        , count: fromMaybe 0 (toNumber countJson <#> floor)
        }

  pure
    { modules
    , stats:
        { totalModules
        , totalDeclarations: totalDecls
        , byKind
        }
    }

-- | Parse a single module
parseModule :: Tuple String Json -> Either String ModuleDeclarations
parseModule (Tuple name json) = do
  obj <- note "Module not an object" $ toObject json
  comments <- note "Missing comments" $ Object.lookup "comments" obj >>= toString
  declsJson <- note "Missing declarations" $ Object.lookup "declarations" obj
  declsArray <- note "Declarations not an array" $ toArray declsJson
  decls <- catMaybes <$> traverse (parseDeclaration name) declsArray

  pure { name, comments, declarations: decls }

-- | Parse a single declaration
parseDeclaration :: String -> Json -> Either String (Maybe Declaration)
parseDeclaration moduleName json = do
  obj <- note "Declaration not an object" $ toObject json

  -- Extract fields
  let title = Object.lookup "title" obj >>= toString
  let kind = Object.lookup "kind" obj >>= toString
  let comments = Object.lookup "comments" obj >>= toString
  let sourceSpan = Object.lookup "sourceSpan" obj >>= toObject

  case title, kind of
    Just t, Just k -> do
      -- Parse sourceSpan
      span <- case sourceSpan of
        Just spanObj -> do
          startArray <- note "Missing start" $ Object.lookup "start" spanObj >>= toArray
          endArray <- note "Missing end" $ Object.lookup "end" spanObj >>= toArray
          start <- note "Invalid start array" $ traverse (\j -> toNumber j <#> floor) startArray
          end <- note "Invalid end array" $ traverse (\j -> toNumber j <#> floor) endArray
          pure { start, end }
        Nothing -> pure { start: [0, 0], end: [0, 0] }

      pure $ Just
        { title: t
        , kind: k
        , module: moduleName
        , comments: fromMaybe "" comments
        , sourceSpan: span
        }
    _, _ -> pure Nothing  -- Skip invalid declarations

-- | Parse function calls JSON
parseFunctionCalls :: Json -> Either String FunctionCallsData
parseFunctionCalls json = do
  obj <- note "Root not an object" $ toObject json

  -- Parse functions
  functionsObj <- note "Missing functions" $ Object.lookup "functions" obj >>= toObject
  let functionsArray = Object.toUnfoldable functionsObj :: Array (Tuple String Json)
  functions <- catMaybes <$> traverse parseFunctionInfo functionsArray

  -- Parse stats
  statsObj <- note "Missing stats" $ Object.lookup "stats" obj >>= toObject
  totalFunctions <- note "Missing totalFunctions" $ Object.lookup "totalFunctions" statsObj >>= toNumber <#> floor
  totalCalls <- note "Missing totalCalls" $ Object.lookup "totalCalls" statsObj >>= toNumber <#> floor
  crossModuleCalls <- note "Missing crossModuleCalls" $ Object.lookup "crossModuleCalls" statsObj >>= toNumber <#> floor

  pure
    { functions
    , stats: { totalFunctions, totalCalls, crossModuleCalls }
    }

-- | Parse a single function info
parseFunctionInfo :: Tuple String Json -> Either String (Maybe { key :: String, value :: FunctionInfo })
parseFunctionInfo (Tuple key json) = do
  obj <- note "Function not an object" $ toObject json

  let name = Object.lookup "name" obj >>= toString
  let module_ = Object.lookup "module" obj >>= toString
  callsJson <- note "Missing calls" $ Object.lookup "calls" obj
  calledByJson <- note "Missing calledBy" $ Object.lookup "calledBy" obj

  callsArray <- note "Calls not an array" $ toArray callsJson
  calledByArray <- note "CalledBy not an array" $ toArray calledByJson

  calls <- catMaybes <$> traverse parseFunctionCall callsArray
  calledBy <- traverse (\j -> toString j # note "CalledBy item not string") calledByArray

  case name, module_ of
    Just n, Just m -> pure $ Just
      { key
      , value:
          { name: n
          , module: m
          , calls
          , calledBy
          }
      }
    _, _ -> pure Nothing

-- | Parse a function call
parseFunctionCall :: Json -> Either String (Maybe FunctionCall)
parseFunctionCall json = do
  obj <- note "Call not an object" $ toObject json

  let target = Object.lookup "target" obj >>= toString
  let targetModule = Object.lookup "targetModule" obj >>= toString
  let identifier = Object.lookup "identifier" obj >>= toString
  let isCrossModule = Object.lookup "isCrossModule" obj >>= toBoolean

  case target, targetModule, identifier, isCrossModule of
    Just t, Just tm, Just i, Just icm -> pure $ Just
      { target: t
      , targetModule: tm
      , identifier: i
      , isCrossModule: icm
      }
    _, _, _, _ -> pure Nothing

-- Helper functions

note :: forall a. String -> Maybe a -> Either String a
note err Nothing = Left err
note _ (Just a) = Right a

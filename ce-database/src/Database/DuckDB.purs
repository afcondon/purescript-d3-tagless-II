-- | DuckDB database bindings for PureScript
-- |
-- | Provides type-safe access to DuckDB via FFI.
-- | All operations are async (Aff) to match DuckDB's callback-based API.
module Database.DuckDB
  ( Database
  , Statement
  , Row
  , Rows
  -- Connection
  , openDB
  , openMemoryDB
  , closeDB
  -- Queries
  , queryAll
  , queryAllParams
  , exec
  , run
  -- Batch
  , execBatch
  , prepare
  , runPrepared
  , finalize
  -- Helpers
  , toJsonString
  , firstRow
  , isEmpty
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)

-- =============================================================================
-- Types
-- =============================================================================

-- | Opaque database connection handle
foreign import data Database :: Type

-- | Opaque prepared statement handle
foreign import data Statement :: Type

-- | A single row result (Foreign object)
type Row = Foreign

-- | Multiple row results
type Rows = Array Foreign

-- =============================================================================
-- FFI Imports - Connection
-- =============================================================================

foreign import openDB_ :: String -> Effect (Promise Database)
foreign import openMemoryDB_ :: Effect (Promise Database)
foreign import closeDB_ :: Database -> Effect (Promise Unit)

-- =============================================================================
-- FFI Imports - Queries
-- =============================================================================

foreign import queryAll_ :: Database -> String -> Effect (Promise Rows)
foreign import queryAllParams_ :: Database -> String -> Array Foreign -> Effect (Promise Rows)
foreign import exec_ :: Database -> String -> Effect (Promise Unit)
foreign import run_ :: Database -> String -> Array Foreign -> Effect (Promise Unit)

-- =============================================================================
-- FFI Imports - Batch
-- =============================================================================

foreign import execBatch_ :: Database -> Array String -> Effect (Promise Unit)
foreign import prepare_ :: Database -> String -> Effect (Promise Statement)
foreign import runPrepared_ :: Statement -> Array Foreign -> Effect (Promise Unit)
foreign import finalize_ :: Statement -> Effect (Promise Unit)

-- =============================================================================
-- FFI Imports - Helpers
-- =============================================================================

foreign import toJsonString_ :: Foreign -> String
foreign import firstRow_ :: Rows -> Nullable Foreign
foreign import isEmpty_ :: Rows -> Boolean

-- =============================================================================
-- Public API - Connection
-- =============================================================================

-- | Open a database file (creates if doesn't exist)
openDB :: String -> Aff Database
openDB path = toAffE (openDB_ path)

-- | Open an in-memory database
openMemoryDB :: Aff Database
openMemoryDB = toAffE openMemoryDB_

-- | Close a database connection
closeDB :: Database -> Aff Unit
closeDB db = toAffE (closeDB_ db)

-- =============================================================================
-- Public API - Queries
-- =============================================================================

-- | Execute a query and return all rows
queryAll :: Database -> String -> Aff Rows
queryAll db sql = toAffE (queryAll_ db sql)

-- | Execute a parameterized query and return all rows
queryAllParams :: Database -> String -> Array Foreign -> Aff Rows
queryAllParams db sql params = toAffE (queryAllParams_ db sql params)

-- | Execute a statement (DDL, INSERT, UPDATE, DELETE)
exec :: Database -> String -> Aff Unit
exec db sql = toAffE (exec_ db sql)

-- | Execute a parameterized statement
run :: Database -> String -> Array Foreign -> Aff Unit
run db sql params = toAffE (run_ db sql params)

-- =============================================================================
-- Public API - Batch
-- =============================================================================

-- | Execute multiple statements in a transaction
execBatch :: Database -> Array String -> Aff Unit
execBatch db statements = toAffE (execBatch_ db statements)

-- | Prepare a statement for repeated execution
prepare :: Database -> String -> Aff Statement
prepare db sql = toAffE (prepare_ db sql)

-- | Run a prepared statement with parameters
runPrepared :: Statement -> Array Foreign -> Aff Unit
runPrepared stmt params = toAffE (runPrepared_ stmt params)

-- | Finalize (close) a prepared statement
finalize :: Statement -> Aff Unit
finalize stmt = toAffE (finalize_ stmt)

-- =============================================================================
-- Public API - Helpers
-- =============================================================================

-- | Convert a value to JSON string for storage
toJsonString :: Foreign -> String
toJsonString = toJsonString_

-- | Get the first row from results
firstRow :: Rows -> Maybe Foreign
firstRow rows = toMaybe (firstRow_ rows)

-- | Check if results are empty
isEmpty :: Rows -> Boolean
isEmpty = isEmpty_

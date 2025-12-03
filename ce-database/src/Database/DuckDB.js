// DuckDB FFI bindings for PureScript
//
// Uses the duckdb Node.js bindings with Promise-based async operations.
// All database operations are wrapped to work with PureScript's Effect/Aff.

import duckdb from 'duckdb';

// =============================================================================
// Database Connection
// =============================================================================

// Open a database (creates if doesn't exist)
// Returns a Promise that resolves to the database instance
export function openDB_(path) {
  return function() {
    return new Promise((resolve, reject) => {
      const db = new duckdb.Database(path, (err) => {
        if (err) reject(err);
        else resolve(db);
      });
    });
  };
}

// Open an in-memory database
export function openMemoryDB_() {
  return function() {
    return new Promise((resolve, reject) => {
      const db = new duckdb.Database(':memory:', (err) => {
        if (err) reject(err);
        else resolve(db);
      });
    });
  };
}

// Close a database connection
export function closeDB_(db) {
  return function() {
    return new Promise((resolve, reject) => {
      db.close((err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  };
}

// =============================================================================
// Query Execution
// =============================================================================

// Execute a query and return all rows
export function queryAll_(db) {
  return function(sql) {
    return function() {
      return new Promise((resolve, reject) => {
        db.all(sql, (err, rows) => {
          if (err) reject(err);
          else resolve(rows || []);
        });
      });
    };
  };
}

// Execute a query with parameters and return all rows
export function queryAllParams_(db) {
  return function(sql) {
    return function(params) {
      return function() {
        return new Promise((resolve, reject) => {
          db.all(sql, ...params, (err, rows) => {
            if (err) reject(err);
            else resolve(rows || []);
          });
        });
      };
    };
  };
}

// Execute a statement (INSERT, UPDATE, DELETE) - returns nothing
export function exec_(db) {
  return function(sql) {
    return function() {
      return new Promise((resolve, reject) => {
        db.exec(sql, (err) => {
          if (err) reject(err);
          else resolve();
        });
      });
    };
  };
}

// Execute a prepared statement with parameters
export function run_(db) {
  return function(sql) {
    return function(params) {
      return function() {
        return new Promise((resolve, reject) => {
          db.run(sql, ...params, (err) => {
            if (err) reject(err);
            else resolve();
          });
        });
      };
    };
  };
}

// =============================================================================
// Batch Operations
// =============================================================================

// Execute multiple statements in a transaction
export function execBatch_(db) {
  return function(statements) {
    return function() {
      return new Promise((resolve, reject) => {
        const allSql = 'BEGIN TRANSACTION;\n' +
          statements.join(';\n') +
          ';\nCOMMIT;';
        db.exec(allSql, (err) => {
          if (err) reject(err);
          else resolve();
        });
      });
    };
  };
}

// Prepare a statement for repeated execution
export function prepare_(db) {
  return function(sql) {
    return function() {
      return new Promise((resolve, reject) => {
        const stmt = db.prepare(sql, (err) => {
          if (err) reject(err);
          else resolve(stmt);
        });
      });
    };
  };
}

// Run a prepared statement with parameters
export function runPrepared_(stmt) {
  return function(params) {
    return function() {
      return new Promise((resolve, reject) => {
        stmt.run(...params, (err) => {
          if (err) reject(err);
          else resolve();
        });
      });
    };
  };
}

// Finalize a prepared statement
export function finalize_(stmt) {
  return function() {
    return new Promise((resolve, reject) => {
      stmt.finalize((err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  };
}

// =============================================================================
// JSON Helpers
// =============================================================================

// Convert a JavaScript value to JSON string for storage
export function toJsonString_(value) {
  return JSON.stringify(value);
}

// Parse JSON string back to JavaScript value
export function fromJsonString_(str) {
  return function() {
    try {
      return JSON.parse(str);
    } catch (e) {
      return null;
    }
  };
}

// =============================================================================
// Utility
// =============================================================================

// Get the first row from results (or null)
export function firstRow_(rows) {
  return rows.length > 0 ? rows[0] : null;
}

// Check if results are empty
export function isEmpty_(rows) {
  return rows.length === 0;
}

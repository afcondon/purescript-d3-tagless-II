#!/usr/bin/env node
/**
 * Initialize the DuckDB database with schema
 *
 * Usage:
 *   node init-schema.js           # Use v2 schema (default)
 *   node init-schema.js --v1      # Use legacy v1 schema
 *   node init-schema.js --fresh   # Delete existing DB first
 */

import duckdb from 'duckdb';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const DB_PATH = path.join(__dirname, '..', 'ce-data.duckdb');

// Parse args
const args = process.argv.slice(2);
const useV1 = args.includes('--v1');
const fresh = args.includes('--fresh');

const SCHEMA_PATH = path.join(__dirname, '..', 'schema', useV1 ? 'init.sql' : 'init-v2.sql');

console.log('Initializing Code Explorer database...\n');
console.log(`Schema: ${useV1 ? 'v1 (legacy)' : 'v2 (multi-project)'}`);

// Remove existing database if --fresh flag
if (fresh && fs.existsSync(DB_PATH)) {
  console.log('Removing existing database (--fresh)...');
  fs.unlinkSync(DB_PATH);
}

// Read schema
const schema = fs.readFileSync(SCHEMA_PATH, 'utf8');

// Create database and run schema
const db = new duckdb.Database(DB_PATH, (err) => {
  if (err) {
    console.error('Failed to create database:', err);
    process.exit(1);
  }

  console.log(`Created database: ${DB_PATH}`);

  db.exec(schema, (err) => {
    if (err) {
      console.error('Failed to initialize schema:', err);
      process.exit(1);
    }

    console.log('Schema initialized successfully!');

    db.close((err) => {
      if (err) {
        console.error('Failed to close database:', err);
        process.exit(1);
      }
      console.log('\nDatabase ready for data loading.');
    });
  });
});

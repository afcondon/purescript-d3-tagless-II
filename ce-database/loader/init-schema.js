#!/usr/bin/env node
/**
 * Initialize the DuckDB database with schema
 */

import duckdb from 'duckdb';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const DB_PATH = path.join(__dirname, '..', 'ce-data.duckdb');
const SCHEMA_PATH = path.join(__dirname, '..', 'schema', 'init.sql');

console.log('Initializing Code Explorer database...\n');

// Remove existing database if it exists
if (fs.existsSync(DB_PATH)) {
  console.log('Removing existing database...');
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

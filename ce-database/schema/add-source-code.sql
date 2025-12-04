-- Migration: Add source_code column to declarations table
-- This stores the actual source code text for each declaration

ALTER TABLE declarations ADD COLUMN source_code TEXT;

-- Index for faster lookups
CREATE INDEX IF NOT EXISTS idx_declarations_lookup
  ON declarations(snapshot_id, module, title);

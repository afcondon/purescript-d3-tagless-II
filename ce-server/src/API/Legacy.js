// Legacy API FFI - JSON builders
//
// These functions transform database query results back into the
// exact JSON format expected by ce-website.

// =============================================================================
// modules.json builder
// =============================================================================

export function buildModulesJson(rows) {
  const result = {};
  for (const row of rows) {
    result[row.name] = {
      depends: JSON.parse(row.depends || '[]'),
      package: row.package,
      path: row.path
    };
  }
  return JSON.stringify(result);
}

// =============================================================================
// packages.json builder
// =============================================================================

export function buildPackagesJson(rows) {
  const result = {};
  for (const row of rows) {
    result[row.name] = {
      depends: JSON.parse(row.depends || '[]')
    };
  }
  return JSON.stringify(result);
}

// =============================================================================
// LOC.json builder
// =============================================================================

export function buildLocJson(rows) {
  const result = {
    loc: rows.map(row => ({
      loc: row.loc,
      path: row.path
    }))
  };
  return JSON.stringify(result);
}

// =============================================================================
// declarations-summary.json builder
// =============================================================================

export function buildDeclarationsSummaryJson(rows) {
  // Group by module: { "ModuleName": [{ kind, title }] }
  const result = {};
  for (const row of rows) {
    if (!result[row.module]) {
      result[row.module] = [];
    }
    result[row.module].push({
      kind: row.kind,
      title: row.title
    });
  }
  return JSON.stringify(result);
}

// =============================================================================
// module-metrics.json builder
// =============================================================================

export function buildModuleMetricsJson(metaRows) {
  return function(maxRows) {
    return function(rows) {
      // Extract metadata
      const generated = metaRows.length > 0
        ? JSON.parse(metaRows[0].value)
        : new Date().toISOString();

      // Extract max values
      const maxVals = maxRows.length > 0 ? maxRows[0] : {};

      // Build modules object
      const modules = {};
      for (const row of rows) {
        modules[row.module] = {
          path: row.path,
          commitCount: row.commit_count,
          daysSinceModified: row.days_since_modified,
          ageInDays: row.age_in_days,
          authorCount: row.author_count,
          linesChanged: row.lines_changed,
          recentCommits: row.recent_commits,
          lineCount: row.line_count,
          authors: JSON.parse(row.authors || '[]'),
          normalized: JSON.parse(row.normalized || '{}')
        };
      }

      const result = {
        generated: generated,
        fileCount: rows.length,
        maxValues: {
          commits: maxVals.max_commits || 0,
          age: maxVals.max_age || 0,
          recency: maxVals.max_recency || 0,
          authors: maxVals.max_authors || 0,
          churn: maxVals.max_churn || 0,
          size: maxVals.max_size || 0
        },
        modules: modules
      };

      return JSON.stringify(result);
    };
  };
}

// =============================================================================
// commit-timeline.json builder
// =============================================================================

export function buildCommitTimelineJson(metaRows) {
  return function(rows) {
    // Extract metadata
    let meta = {};
    if (metaRows.length > 0) {
      try {
        meta = JSON.parse(metaRows[0].value);
      } catch (e) {
        // Use defaults
      }
    }

    // Build timeline array
    const timeline = rows.map(row => ({
      hash: row.hash,
      timestamp: row.timestamp,
      date: row.date,
      author: row.author,
      subject: row.subject,
      created: JSON.parse(row.created || '[]'),
      modified: JSON.parse(row.modified || '[]'),
      deleted: JSON.parse(row.deleted || '[]')
    }));

    const result = {
      generated: meta.generated || new Date().toISOString(),
      commitCount: meta.commitCount || rows.length,
      moduleCount: meta.moduleCount || 0,
      dateRange: meta.dateRange || { start: '', end: '' },
      modules: meta.modules || [],
      timeline: timeline
    };

    return JSON.stringify(result);
  };
}

// =============================================================================
// function-calls.json builder
// =============================================================================

export function buildFunctionCallsJson(rows) {
  // Build functions object: { "Module.func": { module, name, calls, calledBy } }
  const functions = {};

  for (const row of rows) {
    const key = `${row.module}.${row.name}`;
    functions[key] = {
      module: row.module,
      name: row.name,
      calls: JSON.parse(row.calls || '[]'),
      calledBy: JSON.parse(row.called_by || '[]')
    };
  }

  const result = {
    functions: functions
  };

  return JSON.stringify(result);
}

// =============================================================================
// module declarations builder (granular endpoint)
// =============================================================================

export function buildModuleDeclarationsJson(rows) {
  // Simple array of declarations: [{ kind, title, sourceCode }]
  const declarations = rows.map(row => ({
    kind: row.kind,
    title: row.title,
    sourceCode: row.source_code || null
  }));

  return JSON.stringify({ declarations });
}

// =============================================================================
// module function-calls builder (granular endpoint)
// =============================================================================

export function buildModuleFunctionCallsJson(moduleName) {
  return function(rows) {
    // Build functions object for this module: { "funcName": { calls, calledBy } }
    const functions = {};

    for (const row of rows) {
      functions[row.name] = {
        module: moduleName,
        name: row.name,
        calls: JSON.parse(row.calls || '[]'),
        calledBy: JSON.parse(row.called_by || '[]')
      };
    }

    return JSON.stringify({ module: moduleName, functions });
  };
}

// =============================================================================
// batch function-calls builder
// =============================================================================

export function buildBatchFunctionCallsJson(rows) {
  // Build functions object: { "Module.func": { module, name, calls, calledBy } }
  // Same format as full function-calls.json but filtered to requested modules
  const functions = {};

  for (const row of rows) {
    const key = `${row.module}.${row.name}`;
    functions[key] = {
      module: row.module,
      name: row.name,
      calls: JSON.parse(row.calls || '[]'),
      calledBy: JSON.parse(row.called_by || '[]')
    };
  }

  return JSON.stringify({ functions });
}

// =============================================================================
// batch declarations builder
// =============================================================================

export function buildBatchDeclarationsJson(rows) {
  // Group by module: { "ModuleName": [{ kind, title }] }
  const result = {};
  for (const row of rows) {
    if (!result[row.module]) {
      result[row.module] = [];
    }
    result[row.module].push({
      kind: row.kind,
      title: row.title
    });
  }
  return JSON.stringify(result);
}

// =============================================================================
// Helper: split comma-separated modules string
// =============================================================================

export function splitModules(str) {
  return str.split(',').map(s => s.trim()).filter(s => s.length > 0);
}

// =============================================================================
// SQL query builders for batch endpoints (DuckDB compatible)
// =============================================================================

// Build SQL IN clause with proper escaping
function buildInClause(values) {
  return values.map(v => `'${v.replace(/'/g, "''")}'`).join(', ');
}

// Build batch function calls query
export function buildBatchFunctionCallsQuery(snapshotId) {
  return function(modulesStr) {
    const modules = splitModules(modulesStr);
    if (modules.length === 0) {
      return `SELECT module, name, calls, called_by FROM function_calls WHERE 1=0`;
    }
    const inClause = buildInClause(modules);
    return `
      SELECT module, name, calls, called_by
      FROM function_calls
      WHERE snapshot_id = ${parseInt(snapshotId, 10)} AND module IN (${inClause})
      ORDER BY module, name
    `;
  };
}

// Build batch declarations query
export function buildBatchDeclarationsQuery(snapshotId) {
  return function(modulesStr) {
    const modules = splitModules(modulesStr);
    if (modules.length === 0) {
      return `SELECT module, kind, title FROM declarations WHERE 1=0`;
    }
    const inClause = buildInClause(modules);
    return `
      SELECT module, kind, title
      FROM declarations
      WHERE snapshot_id = ${parseInt(snapshotId, 10)} AND module IN (${inClause})
      ORDER BY module, title
    `;
  };
}

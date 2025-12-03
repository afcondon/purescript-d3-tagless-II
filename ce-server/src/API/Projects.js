// FFI for API.Projects module

// Build JSON array of projects
export const buildProjectsListJson = (rows) => {
  const projects = rows.map(row => ({
    id: row.id,
    name: row.name,
    repoPath: row.repo_path,
    description: row.description,
    createdAt: row.created_at,
    snapshotCount: Number(row.snapshot_count),
    latestSnapshotAt: row.latest_snapshot_at
  }));
  return JSON.stringify({ projects });
};

// Build JSON for a single project with its snapshots
export const buildProjectWithSnapshotsJson = (project) => (snapshots) => {
  return JSON.stringify({
    project: {
      id: project.id,
      name: project.name,
      repoPath: project.repo_path,
      description: project.description,
      createdAt: project.created_at
    },
    snapshots: snapshots.map(s => ({
      id: s.id,
      gitHash: s.git_hash,
      gitRef: s.git_ref,
      label: s.label,
      snapshotAt: s.snapshot_at,
      createdAt: s.created_at,
      moduleCount: Number(s.module_count),
      packageCount: Number(s.package_count),
      declarationCount: Number(s.declaration_count)
    }))
  });
};

// Build JSON array of snapshots
export const buildSnapshotsListJson = (rows) => {
  const snapshots = rows.map(s => ({
    id: s.id,
    projectId: s.project_id,
    gitHash: s.git_hash,
    gitRef: s.git_ref,
    label: s.label,
    snapshotAt: s.snapshot_at,
    createdAt: s.created_at,
    moduleCount: Number(s.module_count),
    packageCount: Number(s.package_count),
    declarationCount: Number(s.declaration_count)
  }));
  return JSON.stringify({ snapshots });
};

// Build JSON for a single snapshot
export const buildSnapshotJson = (snapshot) => {
  return JSON.stringify({
    id: snapshot.id,
    projectId: snapshot.project_id,
    projectName: snapshot.project_name,
    gitHash: snapshot.git_hash,
    gitRef: snapshot.git_ref,
    label: snapshot.label,
    snapshotAt: snapshot.snapshot_at,
    createdAt: snapshot.created_at,
    moduleCount: Number(snapshot.module_count),
    packageCount: Number(snapshot.package_count),
    declarationCount: Number(snapshot.declaration_count)
  });
};

// Extract id from a row, returning Nullable Int
export const getIdFromRow_ = (row) => {
  if (row && row.id !== undefined && row.id !== null) {
    return row.id;
  }
  return null;
};

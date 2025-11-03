# API Documentation Workflow

## Overview

The API documentation is generated from library source code and stored in `docs/api/`. To keep git commits clean, these generated HTML files are **not automatically committed** with every change.

## When to Regenerate and Commit API Docs

Regenerate the API docs when:
- Public API changes (new functions, types, or modules added)
- Type signatures change
- Documentation comments are significantly updated
- Preparing for a release or deployment

## How to Update API Docs

### 1. Regenerate the API docs:
```bash
npm run docs:api
```

This will:
- Run `spago docs --format html` to generate docs to `generated-docs/html/`
- Run `scripts/filter-api-docs.js` to filter and copy only library modules to `docs/api/`
- Inject the quadrant navigation header into all API doc pages

### 2. Review the changes:
```bash
git status docs/api/
git diff docs/api/index.html  # Check a sample file
```

### 3. Stage and commit when satisfied:
```bash
npm run docs:api:commit
# Then commit manually with an appropriate message
```

Or manually:
```bash
git add docs/api/
git commit -m "Update API documentation"
```

## Why This Workflow?

- **Clean commits**: Generated HTML files create large diffs that clutter commit history
- **Intentional updates**: API docs should be updated deliberately when the API changes
- **Flexibility**: Developers can work on the library without constantly regenerating docs
- **Deployment ready**: When committed, docs are ready for deployment to GitHub Pages

## Files Involved

- `docs/api/*.html` - Generated API documentation (commit when API changes)
- `generated-docs/html/` - Raw generated docs (in .gitignore, never commit)
- `scripts/filter-api-docs.js` - Filters and injects navigation into docs

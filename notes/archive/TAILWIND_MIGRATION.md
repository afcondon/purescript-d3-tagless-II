# Tailwind CSS Migration: v2 → v3

## Current State
- **Version**: 2.1.4 (May 2021)
- **Custom CSS**: 637 lines
- **@apply usage**: ~80 instances
- **Custom utilities**: ~100 pseudo-selectors
- **Risk level**: HIGH (security, maintenance)

## Target State
- **Version**: 3.4.16 (latest v3, stable)
- **Timeline**: 3-4 days
- **Risk**: LOW
- **Impact**: Visual regressions minimal

---

## Phase 1: Dependency Updates (Day 1)

### 1.1 Update package.json

```bash
npm install -D \
  tailwindcss@^3.4.16 \
  postcss@^8.4.49 \
  postcss-cli@^11.0.1 \
  postcss-import@^16.1.1 \
  postcss-nesting@^13.0.2 \
  autoprefixer@^10.4.21 \
  cssnano@^7.1.1
```

### 1.2 Update build tooling

```bash
npm install -D \
  esbuild@^0.25.10 \
  spago@^0.21.0
```

### 1.3 Update D3 (while we're at it)

```bash
npm install \
  d3@^7.9.0 \
  d3-scale-chromatic@^3.1.0
```

---

## Phase 2: Configuration Migration (Day 1-2)

### 2.1 Update tailwind.config.js

**Old (v2):**
```javascript
module.exports = {
  purge: [],
  darkMode: false,
  theme: { /* ... */ }
}
```

**New (v3):**
```javascript
module.exports = {
  content: [
    "./src/**/*.{purs,html}",
    "./docs/index.html"
  ],
  darkMode: 'media', // or 'class'
  theme: {
    // Keep existing config as-is
    screens: { /* ... */ },
    extend: { /* ... */ }
  },
  plugins: []
}
```

**Key changes:**
- `purge` → `content` (mandatory)
- Add file globs for PureScript files
- `darkMode: false` → `darkMode: 'media'`

### 2.2 Update postcss.config.js

No changes needed - v3 is PostCSS 8 compatible.

---

## Phase 3: CSS Migration (Day 2)

### 3.1 Update imports in index.css

**Current:**
```css
@import "tailwindcss/base";
@import "tailwindcss/components";
@import "tailwindcss/utilities";
```

**Change to:**
```css
@tailwind base;
@tailwind components;
@tailwind utilities;
```

**Or keep as-is** - both syntaxes work in v3.

### 3.2 Review custom utilities

**Good news**: All your custom CSS works in v3!
- `@apply` still supported
- Custom pseudo-selectors work
- No breaking changes for your patterns

**Optional modernization:**
```css
/* Old way (still works): */
.custom-class {
  @apply bg-blue-500 text-white p-4;
}

/* New way (consider later): */
.custom-class {
  @apply bg-blue-500 text-white p-4;
  /* Or refactor to utility classes in PureScript */
}
```

### 3.3 Test custom colors

Your custom color palette (black-10, grey-50, blue-88, etc.) works identically.
No changes needed.

---

## Phase 4: Testing (Day 3)

### 4.1 Build CSS

```bash
npm run build-css
```

**Expected output:** `docs/bundle.css` should be ~same size or smaller (JIT optimization).

### 4.2 Visual regression testing

Test all example pages:
1. Three Little Circles
2. General Update Pattern
3. Trees (all variants)
4. LesMis
5. MetaTree
6. PrintTree
7. Spago (most complex - test thoroughly)

**Check for:**
- Button styles intact
- Checkbox/radio styling correct
- Form field layouts correct
- Table rendering
- Colors match
- Transitions work
- Range sliders function

### 4.3 Browser testing

Test in:
- Chrome/Edge (latest)
- Firefox (latest)
- Safari (latest)

v3 supports: **IE11+ dropped** (good riddance!)

---

## Phase 5: PureScript Integration (Day 3-4)

### 5.1 Build PureScript

```bash
spago build
spago bundle-app --to docs/bundle.js
```

**Expected:** No errors, bundle size similar.

### 5.2 Runtime testing

1. Start local server: `python3 -m http.server -d docs 8000`
2. Open: `http://localhost:8000`
3. Test interactivity:
   - Button clicks
   - Form interactions
   - D3 visualizations
   - Simulation controls
   - Layout changes

### 5.3 Production build

```bash
npm run build        # PureScript
npm run bundle       # Bundle app
npm run build-css    # CSS
```

**Check file sizes:**
```bash
ls -lh docs/bundle.{js,css}
```

---

## Phase 6: Post-Migration Optimization (Day 4)

### 6.1 Enable JIT mode optimizations

JIT is default in v3, but you can optimize:

**Update tailwind.config.js:**
```javascript
module.exports = {
  content: [
    "./src/**/*.purs",
    "./docs/index.html",
    // Add more specific paths for better tree-shaking
  ],
  // ... rest of config
}
```

### 6.2 Consider removing unused @apply

**Low priority**, but for maintainability:
- Identify heavily-used @apply patterns
- Consider extracting to utility classes
- Keep complex ones (range sliders, animations)

### 6.3 Update documentation

Document:
- New Tailwind version
- Build process changes
- Any visual changes
- Content paths for future developers

---

## Rollback Plan

If migration fails:

1. **Revert package.json:**
   ```bash
   git checkout package.json package-lock.json
   npm install
   ```

2. **Revert config changes:**
   ```bash
   git checkout src/DemoApp/css/
   ```

3. **Rebuild:**
   ```bash
   npm run build-css
   spago build
   ```

---

## Breaking Changes Checklist

### ✅ Non-issues for this project

- [x] IE11 support dropped (you're not supporting it)
- [x] Legacy color palette (you use custom colors)
- [x] Extended spacing scale (you use custom spacing)
- [x] Default ring color (you don't use rings)
- [x] Transform utilities (you use custom `@apply`)

### ⚠️ Check these

- [ ] `purge` → `content` in config
- [ ] File globs include PureScript files
- [ ] Test all custom pseudo-selectors
- [ ] Verify custom color usage
- [ ] Check range slider styling

### 🔴 Known deprecations (OK to ignore for now)

- `@apply` - Works in v3, deprecated in v4
- Some color opacity utilities - Still work

---

## Success Criteria

- [ ] All examples render correctly
- [ ] No visual regressions
- [ ] CSS bundle size < 200KB (currently ~180KB)
- [ ] No console errors
- [ ] Interactive elements work
- [ ] Builds complete without warnings
- [ ] Security audit passes: `npm audit`

---

## Timeline

| Phase | Duration | Blocker Risk |
|-------|----------|--------------|
| 1. Dependencies | 2 hours | Low |
| 2. Configuration | 2 hours | Low |
| 3. CSS Migration | 4 hours | Medium |
| 4. Testing | 6 hours | Medium |
| 5. PureScript | 4 hours | Low |
| 6. Optimization | 4 hours | Low |
| **Total** | **3-4 days** | **Low-Medium** |

---

## Future: v3 → v4 Considerations

**Don't do now**, but consider in 6-12 months:

### When to migrate to v4:
- [ ] When you need new features (container queries, etc.)
- [ ] When refactoring CSS anyway
- [ ] When team has capacity (1-2 weeks)
- [ ] After v4 matures (currently v4.1)

### v4 migration challenges:
1. **@apply deprecation** - 80+ instances to refactor
2. **Custom utilities** - Plugin API v2 rewrite
3. **CSS-first config** - Complete paradigm shift
4. **PostCSS changes** - New package structure

**Estimated v3→v4 effort**: 1-2 weeks (vs 3-4 days for v2→v3)

---

## Notes

- Keep this document updated as migration progresses
- Document any unexpected issues
- Take screenshots before/after
- Consider creating a feature branch: `git checkout -b upgrade/tailwind-v3`

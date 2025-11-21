# CSS Consolidation Worklist

## Design System Summary

### Page Types
1. **Home** - Main landing page (keep as is, extract reusable parts)
2. **Section Gallery** - Top-level docs pages (How-To, Understanding, Getting Started overviews)
3. **Tutorial Page** - Second-level docs (individual tutorials/guides)
4. **API Reference** - Generated docs (limited styling control)
5. **Example Page** - Examples with embedded code

### Core Components
- **Header nav** - From main page, used everywhere
- **Four squares indicator** - Docs nav, shows current section
- **Card grid** - "Take the tour" style, for galleries
- **Prev/Next nav** - Pointing hand icons, for sequenced content
- **Control panel** - Code explorer style (preserve for future)

---

## Worklist

### Phase 1: Extract Reusable CSS

- [ ] **1.1** Audit current CSS files in `docs/styles/`
- [ ] **1.2** Create `components.css` for shared components (header, cards, nav)
- [ ] **1.3** Create `layout.css` for page-type layouts
- [ ] **1.4** Keep page-specific CSS minimal (only overrides)

### Phase 2: Header & Navigation

- [ ] **2.1** Extract header nav from home.css to components.css
- [ ] **2.2** Add four-squares docs indicator to header component
- [ ] **2.3** Ensure header works on all pages (Docs, Tour, Showcase, Examples, Logo)
- [ ] **2.4** Add Prev/Next nav component with pointing hand icons
- [ ] **2.5** Update all pages to use new header

### Phase 3: Section Gallery Pages

- [ ] **3.1** Extract card grid component from main page
- [ ] **3.2** Apply to How-To gallery page
- [ ] **3.3** Apply to Understanding overview page
- [ ] **3.4** Apply to Getting Started page
- [ ] **3.5** Ensure consistent spacing/typography

### Phase 4: Tutorial Pages

- [ ] **4.1** Verify How-To tutorials and Understanding pages use same styles
- [ ] **4.2** Create/update `tutorial.css` as canonical tutorial style
- [ ] **4.3** Add Prev/Next nav to sequenced tutorial pages
- [ ] **4.4** Ensure code blocks, headings, prose are consistent

### Phase 5: API Reference

- [ ] **5.1** Check if API docs can include custom header
- [ ] **5.2** Add code-explorer screenshot to API index page (if possible)
- [ ] **5.3** Minimal styling tweaks for consistency

### Phase 6: Example Pages

- [ ] **6.1** Review current example page layout
- [ ] **6.2** Ensure code display and viz container are consistent
- [ ] **6.3** Add standard header/nav

### Phase 7: Cleanup

- [ ] **7.1** Remove deprecated/unused CSS
- [ ] **7.2** Consolidate duplicate rules
- [ ] **7.3** Test all pages for regressions
- [ ] **7.4** Document the design system briefly

---

## Files to Touch

**Create/Major changes:**
- `docs/styles/components.css` (new)
- `docs/styles/layout.css` (new or major update)

**Update:**
- `docs/styles/home.css` - extract shared parts
- `docs/styles/tutorial.css` - canonical tutorial style
- `docs/styles/howto.css` - may merge into tutorial.css
- `docs/styles/examples.css` - update for consistency

**Preserve:**
- `docs/styles/spago.css` - code explorer control panel

**Review/Possibly remove:**
- Any page-specific CSS that can be consolidated

---

## Notes

- Don't break things - test incrementally
- The main page is the gold standard for header/cards
- Code explorer control panel styling is good - keep for showcase
- API docs generation may limit what we can style there

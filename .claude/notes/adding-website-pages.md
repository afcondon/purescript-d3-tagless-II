# How to Add a Page to the PSD3 Website

## Quick Reference: Steps to Add a New Route/Page

### 1. Create the Component
**File:** `src/website/Component/YourPage.purs`
```purescript
module PSD3.Component.YourPage where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Effect.Aff.Class (class MonadAff)

type State = { ... }
data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent { initialState, render, eval: ... }
```

### 2. Add Route to Types
**File:** `src/website/Types.purs`

Add to `data Route`:
```purescript
data Route
  = ...
  | YourPage
  | ...
```

Add to `Show` instance:
```purescript
show YourPage = "Your Page Title"
```

### 3. Add Route Parser
**File:** `src/website/RoutingDSL.purs`

Add parser function:
```purescript
-- | Match: /your-page
yourPage :: Match Route
yourPage = YourPage <$ lit "your-page" <* end
```

Add to route combinator (around line 35):
```purescript
routing :: Match Route
routing =
  <|> yourPage
  <|> ...
```

Add to `routeToPath`:
```purescript
routeToPath YourPage = "/your-page"
```

### 4. Wire Up in Main
**File:** `src/website/Main.purs`

Import component:
```purescript
import PSD3.Component.YourPage as YourPage
```

Add slot proxy (around line 96):
```purescript
_yourPage = Proxy :: Proxy "yourPage"
```

Add to `type Slots` (around line 72):
```purescript
type Slots =
  ( yourPage :: forall q. H.Slot q Void Unit
  , ...
  )
```

Add route handler (around line 180):
```purescript
YourPage ->
  HH.slot_ _yourPage unit YourPage.component unit
```

### 5. Build and Test
```bash
cd /Users/andrew/work/PureScript\ Tagless\ D3
spago build
npm run dev    # or your dev server command
```

Navigate to `http://localhost:1234/#/your-page`

---

## Example: MermaidDiagrams Page

We just added `/mermaid-diagrams`:
1. Component: `src/website/Component/MermaidDiagrams.purs`
2. Types: Added `MermaidDiagrams` route
3. RoutingDSL: Added `mermaidDiagrams` parser and `/mermaid-diagrams` path
4. Main: Imported, added `_mermaidDiagrams` proxy, added to Slots, added route case

---

## Common Patterns

**For pages with parameters** (e.g., `/example/:id`):
- Route: `| Example String`
- Parser: `example = Example <$> (lit "example" *> str) <* end`
- Path: `routeToPath (Example id) = "/example/" <> id`
- Handler: `Example exampleId -> HH.slot ...`

**For nested routes** (e.g., `/reference/Module.Name`):
- Use `str` in parser to capture segments
- Pattern match on constructor with parameter

**Slot outputs:**
- Most pages use `Void` for no output
- Use specific type if component emits events: `H.Slot q OutputType Unit`

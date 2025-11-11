# PureScript Type Error Debugging Techniques

## The "Remove and Infer" Technique

When you have a complex type error that's hard to diagnose, especially with PartiallyAppliedSynonym or complex constraint errors:

### Strategy:
1. **Remove the problematic type signature** entirely
2. **Build the code** - if it type checks without the signature, you have proof that a correct type exists
3. **Check the warning** - The compiler will warn about missing type declaration and show the **inferred type**
4. **Use the inferred type** - This is the most general, fully expanded type that works
5. **Simplify** - Either use it as-is or simplify by:
   - Importing type synonyms that are expanded in the inference
   - Removing redundant constraints
   - Adding newtype wrappers if needed

### Example:

**Original (broken):**
```purescript
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
```

**Error:** `PartiallyAppliedSynonym` - `State` is a type synonym that needs parameters

**Fix process:**
1. Remove the signature
2. Build - it compiles!
3. Check warning - shows fully expanded inferred type
4. Either use expanded type or import/fix the synonym

### Why this works:
- The compiler's type inference is complete and correct
- The inferred type is always maximally general
- Seeing the expanded form reveals what constraints/parameters are actually needed
- You can work backwards from correct inference to clean signature

### When to use:
- PartiallyAppliedSynonym errors
- Complex type class constraint errors
- Mysterious "kinds do not unify" errors
- When refactoring changes affect type signatures

---

## Other Techniques

### Use type holes
Replace unknowns with `?hole` to see what type is expected:
```purescript
myFunc :: ?signature
myFunc x = x + 1
```

### Check kind signatures
Use `:kind` in the REPL to verify type constructors:
```purescript
:kind D3Selection_  -- Type -> Type
:kind D3M           -- Type -> (Type -> Type) -> Type -> Type
```

### Simplify step by step
Comment out parts of complex expressions to isolate the error.

### Use explicit type annotations
Add inline annotations to narrow down where the error occurs:
```purescript
(x :: Int) <- getValue
```

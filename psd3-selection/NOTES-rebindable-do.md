# Rebindable-Do for Attribute Syntax

## Summary

Explored using PureScript's rebindable-do (qualified do) to provide an alternative attribute syntax that avoids array brackets and commas.

## Current Syntax

```purescript
circle
  [ fill "blue"
  , stroke "black"
  , cx 50.0
  , cy 50.0
  , r 20.0
  ]
```

## Proposed Attrs.do Syntax

```purescript
import PSD3.Attrs.Builder as Attrs

circle Attrs.do
  fill "blue"
  stroke "black"
  cx 50.0
  cy 50.0
  r 20.0
```

## Implementation Sketch

```purescript
-- PSD3/Attrs/Builder.purs
module PSD3.Attrs.Builder where

newtype Attrs a = Attrs (Array Attribute)

-- Rebindable-do: discard is used for statement sequencing (no <-)
discard :: forall a b. Attrs a -> Attrs b -> Attrs b
discard (Attrs as) (Attrs bs) = Attrs (as <> bs)

-- Rebindable-do: bind needed but rarely used (we don't bind results)
bind :: forall a b. Attrs a -> (a -> Attrs b) -> Attrs b
bind (Attrs as) _ = Attrs as

toArray :: forall a. Attrs a -> Array Attribute
toArray (Attrs as) = as

single :: Attribute -> Attrs Unit
single a = Attrs [a]
```

Attribute helpers would return `Attrs Unit`:
```purescript
fill :: String -> Attrs Unit
fill = single <<< attr "fill" <<< StringValue
```

## Backwards Compatibility

Use a typeclass to accept either syntax:

```purescript
class ToAttrs a where
  toAttrs :: a -> Array Attribute

instance ToAttrs (Array Attribute) where
  toAttrs = identity

instance ToAttrs (Attrs b) where
  toAttrs (Attrs as) = as
```

Then shapes accept both:
```purescript
circle :: forall attrs. ToAttrs attrs => attrs -> Selection m Unit
```

## Benefits

- No closing bracket to forget
- No commas between attributes
- Cleaner visual alignment
- Works with existing dynamic attribute helpers (`fillF`, `rF`, etc.)

## Considerations

- Requires `import ... as Attrs` and qualified `Attrs.do`
- Slight learning curve for users unfamiliar with rebindable-do
- Both syntaxes can coexist for gradual adoption

## Status

Parking this idea for potential future implementation. The typeclass approach for backwards compatibility looks viable.

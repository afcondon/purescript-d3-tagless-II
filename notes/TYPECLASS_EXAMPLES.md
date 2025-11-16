# Typeclass Instance Examples

This document shows practical examples of the Functor and Contravariant instances.

## Functor for Selection

### Example 1: Transform Numeric Data

```purescript
-- You have a selection of circles with numeric positions
positions :: Selection SBound Element Number
positions = ... -- [10.0, 20.0, 30.0]

-- Scale them up
scaled :: Selection SBound Element Number
scaled = map (_ * 2.0) positions  -- [20.0, 40.0, 60.0]

-- No DOM mutation! Pure data transformation.
```

### Example 2: Extract Fields from Records

```purescript
type Person = { name :: String, age :: Int, salary :: Number }

people :: Selection SBound Element Person
people = ...

-- Extract just the ages
ages :: Selection SBound Element Int
ages = map _.age people

-- Extract salaries for tax calculation
salaries :: Selection SBound Element Number
salaries = map _.salary people
```

### Example 3: Compose Transformations

```purescript
-- Chain multiple transformations
doubled :: Selection SBound Element Int
doubled = people
  # map _.age        -- Extract age field
  # map (_ * 2)      -- Double it

-- Works with other functors
maybeSelection :: Maybe (Selection SBound Element Int)
maybeSelection = Just ages

doubledMaybe :: Maybe (Selection SBound Element Int)
doubledMaybe = map (map (_ * 2)) maybeSelection
--                 ^^^^ outer map for Maybe
--                      ^^^^ inner map for Selection
```

---

## Contravariant for Attribute

### Example 1: Reuse Simple Attributes on Complex Types

```purescript
import Data.Functor.Contravariant (cmap)

-- Define attributes for simple types
radiusAttr :: Attribute Number
radiusAttr = DataAttr (AttributeName "r") NumberValue

fillAttr :: Attribute String
fillAttr = DataAttr (AttributeName "fill") StringValue

-- Adapt them to work with records
type Circle = { radius :: Number, color :: String, x :: Number, y :: Number }

circleAttrs :: Array (Attribute Circle)
circleAttrs =
  [ cmap _.radius radiusAttr    -- Number -> Circle (projection)
  , cmap _.color fillAttr       -- String -> Circle (projection)
  , cmap _.x (cx (\n -> n))     -- Can also use built-in constructors
  , cmap _.y (cy (\n -> n))
  ]
```

### Example 2: Create Attribute Libraries

```purescript
-- Create a library of reusable attributes
module Attributes.Circle where

import Data.Functor.Contravariant (cmap)

-- Generic attributes that work on any type with appropriate fields
radiusFromField :: forall r. Attribute { radius :: Number | r }
radiusFromField = cmap _.radius (radius (\n -> n))

colorFromField :: forall r. Attribute { color :: String | r }
colorFromField = cmap _.color (fill (\s -> s))

positionAttrs :: forall r. Array (Attribute { x :: Number, y :: Number | r })
positionAttrs =
  [ cmap _.x (cx (\n -> n))
  , cmap _.y (cy (\n -> n))
  ]

-- Use them anywhere!
type Circle = { radius :: Number, color :: String, x :: Number, y :: Number }
type Bubble = { radius :: Number, color :: String, x :: Number, y :: Number, value :: Number }

circleAttrs :: Array (Attribute Circle)
circleAttrs = [radiusFromField, colorFromField] <> positionAttrs

bubbleAttrs :: Array (Attribute Bubble)
bubbleAttrs = [radiusFromField, colorFromField] <> positionAttrs
  -- Same attributes work on both types!
```

### Example 3: Adapt Indexed Attributes

```purescript
-- Even indexed attributes can be adapted
stripePattern :: Attribute Int
stripePattern = IndexedAttr (AttributeName "fill") \val idx ->
  StringValue if idx `mod` 2 == 0 then "white" else "black"

-- Adapt to work on records
type Item = { value :: Int, name :: String }

itemStripePattern :: Attribute Item
itemStripePattern = cmap _.value stripePattern
-- Now it stripes based on the value field, not the whole record
```

### Example 4: Combine with ToAttr Polymorphism

```purescript
-- ToAttr gives you polymorphism in INPUT type (static vs function)
-- Contravariant gives you polymorphism in DATUM type (projection)

type Point = { x :: Number, y :: Number }
type Circle = { center :: Point, radius :: Number }

-- Use both together:
centerXAttr :: Attribute Circle
centerXAttr = cmap (_.center >>> _.x) (cx \n -> n)
--            ^^^^^^^^^^^^^^^^^^^^^^^ Contravariant projection
--                                     ^^ ToAttr polymorphism

-- Or more explicitly:
centerYAttr :: Attribute Circle
centerYAttr = cmap _.center (cy (\p -> p.y))
--            ^^^^^^^^^^^^^ Project to Point
--                           ^^ ToAttr: datum -> Number
```

---

## Combining Both: Functor + Contravariant

### Example: Data Pipeline

```purescript
-- Start with complex data
type SalesData = { product :: String, revenue :: Number, quarter :: Int }

sales :: Selection SBound Element SalesData
sales = ...

-- Extract just revenue (Functor)
revenues :: Selection SBound Element Number
revenues = map _.revenue sales

-- Apply attribute designed for numbers (Contravariant not needed here)
revenueCircles :: Effect (Selection SBound Element Number)
revenueCircles = setAttrs
  [ radius (\r -> r / 1000.0)    -- Scale revenue to radius
  , fill "green"
  ]
  revenues

-- OR: Keep rich data and use Contravariant to project
revenueCircles2 :: Effect (Selection SBound Element SalesData)
revenueCircles2 = setAttrs
  [ cmap _.revenue (radius (\r -> r / 1000.0))  -- Project then scale
  , fill "green"
  ]
  sales
```

The choice depends on whether you need to preserve the rich data for later use!

---

## The Data Flow Pattern

```
Selection (produces data) ----Functor-----> Selection (transformed data)
                |
                | bind to DOM elements
                v
           Attributes (consume data) <---Contravariant--- Attributes (simpler types)
```

### Functor (Covariant):
- **Transforms outputs**: `f: a -> b` lifts to `Selection a -> Selection b`
- **Data flows forward**: from simple to complex, from specific to general
- **Use when**: You want to transform the data bound to selections

### Contravariant:
- **Transforms inputs**: `f: b -> a` lifts to `Attribute a -> Attribute b`
- **Data flows backward**: from complex to simple, from general to specific
- **Use when**: You want to reuse attributes on richer types via projection

Together they form a beautiful algebraic symmetry in the library!

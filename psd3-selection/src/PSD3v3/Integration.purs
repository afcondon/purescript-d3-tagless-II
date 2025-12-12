-- | PSD3v3 Integration with v2 Selection System
-- |
-- | This module bridges the v3 "finally tagless" attribute DSL with the
-- | v2 selection/update pattern. v3 expressions compile to v2 attributes,
-- | enabling:
-- |   - Polymorphic attribute definitions (same expr → different interpreters)
-- |   - Type-safe datum field access
-- |   - Full enter/update/exit pattern support
-- |
-- | Usage:
-- | ```purescript
-- | -- Define v3 expression (polymorphic)
-- | nodeRadius :: forall repr. NumExpr repr => BoolExpr repr => DatumExpr repr NodeRow => repr Number
-- | nodeRadius = ifThenElse hasChildren (n 8.0) (n 5.0)
-- |
-- | -- Convert to v2 attribute
-- | radiusAttr :: Attribute Node
-- | radiusAttr = v3Attr "r" nodeRadius
-- |
-- | -- Use in selection
-- | append Circle [radiusAttr, v3Attr "fill" nodeFill] enterSelection
-- | ```
module PSD3v3.Integration
  ( -- * Attribute Constructors
    v3Attr
  , v3AttrIndexed
  , v3Static
    -- * Batch Conversion
  , v3Attrs
    -- * Re-exports for convenience
  , module PSD3v2.Attribute.Types
  ) where

import Prelude

import PSD3v2.Attribute.Types (Attribute(..), AttributeName, AttributeValue(..))
import PSD3v3.Interpreter.Eval (EvalD, runEvalD)

-- | Convert a v3 datum expression to a v2 data-driven attribute.
-- |
-- | The expression is interpreted using EvalD, which produces a function
-- | from (datum, index) to the attribute value.
-- |
-- | ```purescript
-- | -- v3 expression
-- | scaleX :: forall repr. NumExpr repr => DatumExpr repr PointRow => repr Number
-- | scaleX = xField *: 20.0 +: 200.0
-- |
-- | -- v2 attribute
-- | cxAttr :: Attribute Point
-- | cxAttr = v3Attr "cx" scaleX
-- | ```
v3Attr :: forall datum a
        . Show a
       => AttributeName
       -> EvalD datum a
       -> Attribute datum
v3Attr name expr = DataAttr name (\d -> StringValue $ show (runEvalD expr d 0))

-- | Convert a v3 expression to a v2 indexed attribute.
-- |
-- | Use this when your expression uses the datum index (e.g., for staggered
-- | positioning or animation delays).
-- |
-- | ```purescript
-- | -- v3 expression using index
-- | xFromIndex :: forall repr. NumExpr repr => DatumExpr repr PointRow => repr Number
-- | xFromIndex = indexNum *: 50.0 +: 25.0
-- |   where indexNum = E.mul (n 1.0) (unsafeCoerce index)  -- Convert Int to Number
-- |
-- | -- v2 indexed attribute
-- | xAttr :: Attribute Point
-- | xAttr = v3AttrIndexed "x" xFromIndex
-- | ```
v3AttrIndexed :: forall datum a
               . Show a
              => AttributeName
              -> EvalD datum a
              -> Attribute datum
v3AttrIndexed name expr = IndexedAttr name (\d i -> StringValue $ show (runEvalD expr d i))

-- | Create a static v2 attribute from a v3 static expression.
-- |
-- | For expressions that don't depend on datum at all, this produces
-- | a StaticAttr which is more efficient (evaluated once, not per-element).
-- |
-- | Note: The expression must be fully evaluable without datum context.
-- | For truly static values, prefer v2's native static attributes.
v3Static :: forall a
          . Show a
         => AttributeName
         -> a
         -> Attribute Unit
v3Static name value = StaticAttr name (StringValue $ show value)

-- | Convert multiple v3 expressions to v2 attributes at once.
-- |
-- | This is a convenience for defining attribute sets:
-- |
-- | ```purescript
-- | circleAttrs :: Array (Attribute Point)
-- | circleAttrs = v3Attrs
-- |   [ Tuple "cx" scaleX
-- |   , Tuple "cy" scaleY
-- |   , Tuple "r" radius
-- |   ]
-- | ```
v3Attrs :: forall datum a
         . Show a
        => Array { name :: AttributeName, expr :: EvalD datum a }
        -> Array (Attribute datum)
v3Attrs = map (\{ name, expr } -> v3Attr name expr)

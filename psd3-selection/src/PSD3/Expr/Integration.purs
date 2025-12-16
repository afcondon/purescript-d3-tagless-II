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
module PSD3.Expr.Integration
  ( -- * Attribute Constructors
    v3Attr
  , v3AttrStr
  , v3AttrIndexed
  , v3AttrIndexedStr
  , v3Static
  , v3StaticStr
    -- * Lifting PureScript functions
  , liftFn
  , liftFnI
  , v3AttrFn
  , v3AttrFnStr
  , v3AttrFnI
  , v3AttrFnIStr
    -- * Batch Conversion
  , v3Attrs
    -- * Re-exports for convenience
  , module PSD3.Internal.Attribute
  ) where

import Prelude

import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3.Expr.Interpreter.Eval (EvalD(..), runEvalD)

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
       => String
       -> EvalD datum a
       -> Attribute datum
v3Attr name expr = DataAttr (AttributeName name) (\d -> StringValue $ show (runEvalD expr d 0))

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
              => String
              -> EvalD datum a
              -> Attribute datum
v3AttrIndexed name expr = IndexedAttr (AttributeName name) (\d i -> StringValue $ show (runEvalD expr d i))

-- | Create a static v2 attribute from a v3 static expression.
-- |
-- | For expressions that don't depend on datum at all, this produces
-- | a StaticAttr which is more efficient (evaluated once, not per-element).
-- |
-- | Note: The expression must be fully evaluable without datum context.
-- | For truly static values, prefer v2's native static attributes.
v3Static :: forall a
          . Show a
         => String
         -> a
         -> Attribute Unit
v3Static name value = StaticAttr (AttributeName name) (StringValue $ show value)

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
        => Array { name :: String, expr :: EvalD datum a }
        -> Array (Attribute datum)
v3Attrs = map (\{ name, expr } -> v3Attr name expr)

-- =============================================================================
-- String-specific versions (no Show/quotes)
-- =============================================================================

-- | Convert a v3 string expression to a v2 attribute (no quoting)
-- |
-- | Use this for string-valued attributes like fill, stroke, class, d, etc.
v3AttrStr :: forall datum
           . String
          -> EvalD datum String
          -> Attribute datum
v3AttrStr name expr = DataAttr (AttributeName name) (\d -> StringValue $ runEvalD expr d 0)

-- | Convert a v3 indexed string expression to a v2 attribute (no quoting)
v3AttrIndexedStr :: forall datum
                  . String
                 -> EvalD datum String
                 -> Attribute datum
v3AttrIndexedStr name expr = IndexedAttr (AttributeName name) (\d i -> StringValue $ runEvalD expr d i)

-- | Create a static string attribute (no quoting)
v3StaticStr :: String -> String -> Attribute Unit
v3StaticStr name value = StaticAttr (AttributeName name) (StringValue value)

-- =============================================================================
-- Lifting PureScript functions to EvalD
-- =============================================================================

-- | Lift a datum-dependent PureScript function to an EvalD expression
-- |
-- | This is an escape hatch for complex computations that can't easily
-- | be expressed in the v3 DSL.
-- |
-- | ```purescript
-- | linePath :: Series -> String
-- | linePath s = ...complex path computation...
-- |
-- | d (liftFn linePath)
-- | ```
liftFn :: forall datum a. (datum -> a) -> EvalD datum a
liftFn f = EvalD (\d _ -> f d)

-- | Lift an indexed PureScript function to an EvalD expression
liftFnI :: forall datum a. (datum -> Int -> a) -> EvalD datum a
liftFnI = EvalD

-- | Create a data-driven attribute from a plain PureScript function
-- |
-- | Shortcut for v3Attr name (liftFn f)
v3AttrFn :: forall datum a
          . Show a
         => String
         -> (datum -> a)
         -> Attribute datum
v3AttrFn name f = DataAttr (AttributeName name) (\d -> StringValue $ show (f d))

-- | Create a string data-driven attribute from a plain PureScript function
-- |
-- | Shortcut for v3AttrStr name (liftFn f)
v3AttrFnStr :: forall datum
             . String
            -> (datum -> String)
            -> Attribute datum
v3AttrFnStr name f = DataAttr (AttributeName name) (\d -> StringValue (f d))

-- | Create an indexed attribute from a plain PureScript function (datum -> Int -> a)
-- |
-- | Use this when you need both datum and index access
v3AttrFnI :: forall datum a
           . Show a
          => String
          -> (datum -> Int -> a)
          -> Attribute datum
v3AttrFnI name f = IndexedAttr (AttributeName name) (\d i -> StringValue $ show (f d i))

-- | Create an indexed string attribute from a plain PureScript function
v3AttrFnIStr :: forall datum
              . String
             -> (datum -> Int -> String)
             -> Attribute datum
v3AttrFnIStr name f = IndexedAttr (AttributeName name) (\d i -> StringValue (f d i))

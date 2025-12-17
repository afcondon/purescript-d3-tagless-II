-- | Integration with Selection System
-- |
-- | This module bridges the "finally tagless" attribute DSL with the
-- | selection/update pattern. Expressions compile to attributes,
-- | enabling:
-- |   - Polymorphic attribute definitions (same expr → different interpreters)
-- |   - Type-safe datum field access
-- |   - Full enter/update/exit pattern support
-- |
-- | Usage:
-- | ```purescript
-- | -- Define expression (polymorphic)
-- | nodeRadius :: forall repr. NumExpr repr => BoolExpr repr => DatumExpr repr NodeRow => repr Number
-- | nodeRadius = ifThenElse hasChildren (n 8.0) (n 5.0)
-- |
-- | -- Convert to attribute
-- | radiusAttr :: Attribute Node
-- | radiusAttr = evalAttr "r" nodeRadius
-- |
-- | -- Use in selection
-- | append Circle [radiusAttr, evalAttr "fill" nodeFill] enterSelection
-- | ```
module PSD3.Expr.Integration
  ( -- * Attribute Constructors
    evalAttr
  , evalAttrStr
  , evalAttrIndexed
  , evalAttrIndexedStr
  , staticNum
  , staticStr
    -- * Lifting PureScript functions
  , liftFn
  , liftFnI
  , fnAttr
  , fnAttrStr
  , fnAttrI
  , fnAttrIStr
    -- * Batch Conversion
  , evalAttrs
    -- * Re-exports for convenience
  , module PSD3.Internal.Attribute
  ) where

import Prelude

import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Expr.Interpreter.Eval (EvalD(..), runEvalD)

-- | Convert a datum expression to a data-driven attribute.
-- |
-- | The expression is interpreted using EvalD, which produces a function
-- | from (datum, index) to the attribute value.
-- |
-- | ```purescript
-- | -- expression
-- | scaleX :: forall repr. NumExpr repr => DatumExpr repr PointRow => repr Number
-- | scaleX = xField *: 20.0 +: 200.0
-- |
-- | -- attribute
-- | cxAttr :: Attribute Point
-- | cxAttr = evalAttr "cx" scaleX
-- | ```
evalAttr :: forall datum a
          . Show a
         => String
         -> EvalD datum a
         -> Attribute datum
evalAttr name expr = DataAttr (AttributeName name) UnknownSource (\d -> StringValue $ show (runEvalD expr d 0))

-- | Convert a string expression to an attribute (no quoting)
-- |
-- | Use this for string-valued attributes like fill, stroke, class, d, etc.
evalAttrStr :: forall datum
             . String
            -> EvalD datum String
            -> Attribute datum
evalAttrStr name expr = DataAttr (AttributeName name) UnknownSource (\d -> StringValue $ runEvalD expr d 0)

-- | Convert an expression to an indexed attribute.
-- |
-- | Use this when your expression uses the datum index (e.g., for staggered
-- | positioning or animation delays).
-- |
-- | ```purescript
-- | -- expression using index
-- | xFromIndex :: forall repr. NumExpr repr => DatumExpr repr PointRow => repr Number
-- | xFromIndex = indexNum *: 50.0 +: 25.0
-- |   where indexNum = E.mul (n 1.0) (unsafeCoerce index)  -- Convert Int to Number
-- |
-- | -- indexed attribute
-- | xAttr :: Attribute Point
-- | xAttr = evalAttrIndexed "x" xFromIndex
-- | ```
evalAttrIndexed :: forall datum a
                 . Show a
                => String
                -> EvalD datum a
                -> Attribute datum
evalAttrIndexed name expr = IndexedAttr (AttributeName name) UnknownSource (\d i -> StringValue $ show (runEvalD expr d i))

-- | Convert an indexed string expression to an attribute (no quoting)
evalAttrIndexedStr :: forall datum
                    . String
                   -> EvalD datum String
                   -> Attribute datum
evalAttrIndexedStr name expr = IndexedAttr (AttributeName name) UnknownSource (\d i -> StringValue (runEvalD expr d i))

-- | Create a static numeric attribute.
-- |
-- | For expressions that don't depend on datum at all, this produces
-- | a StaticAttr which is more efficient (evaluated once, not per-element).
staticNum :: forall datum a
           . Show a
          => String
          -> a
          -> Attribute datum
staticNum name value = StaticAttr (AttributeName name) (StringValue $ show value)

-- | Create a static string attribute (no quoting)
staticStr :: forall datum. String -> String -> Attribute datum
staticStr name value = StaticAttr (AttributeName name) (StringValue value)

-- | Convert multiple expressions to attributes at once.
-- |
-- | This is a convenience for defining attribute sets:
-- |
-- | ```purescript
-- | circleAttrs :: Array (Attribute Point)
-- | circleAttrs = evalAttrs
-- |   [ { name: "cx", expr: scaleX }
-- |   , { name: "cy", expr: scaleY }
-- |   , { name: "r", expr: radius }
-- |   ]
-- | ```
evalAttrs :: forall datum a
           . Show a
          => Array { name :: String, expr :: EvalD datum a }
          -> Array (Attribute datum)
evalAttrs = map (\{ name, expr } -> evalAttr name expr)

-- =============================================================================
-- Lifting PureScript functions to EvalD
-- =============================================================================

-- | Lift a datum-dependent PureScript function to an EvalD expression
-- |
-- | This is an escape hatch for complex computations that can't easily
-- | be expressed in the DSL.
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
-- | Shortcut for evalAttr name (liftFn f)
fnAttr :: forall datum a
        . Show a
       => String
       -> (datum -> a)
       -> Attribute datum
fnAttr name f = DataAttr (AttributeName name) UnknownSource (\d -> StringValue $ show (f d))

-- | Create a string data-driven attribute from a plain PureScript function
-- |
-- | Shortcut for evalAttrStr name (liftFn f)
fnAttrStr :: forall datum
           . String
          -> (datum -> String)
          -> Attribute datum
fnAttrStr name f = DataAttr (AttributeName name) UnknownSource (\d -> StringValue (f d))

-- | Create an indexed attribute from a plain PureScript function (datum -> Int -> a)
-- |
-- | Use this when you need both datum and index access
fnAttrI :: forall datum a
         . Show a
        => String
        -> (datum -> Int -> a)
        -> Attribute datum
fnAttrI name f = IndexedAttr (AttributeName name) UnknownSource (\d i -> StringValue $ show (f d i))

-- | Create an indexed string attribute from a plain PureScript function
fnAttrIStr :: forall datum
            . String
           -> (datum -> Int -> String)
           -> Attribute datum
fnAttrIStr name f = IndexedAttr (AttributeName name) UnknownSource (\d i -> StringValue (f d i))

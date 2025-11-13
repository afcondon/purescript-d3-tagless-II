module PSD3.Shared.TreeHelpers where

import PSD3 (Datum_)
import PSD3.Data.Tree (TreeLayoutFn_, TreeType)

-- Tree layouts use a special type from D3
foreign import data D3HierarchyNode :: Type

-- | Get D3 layout algorithm based on TreeType (TidyTree uses d3.tree(), Dendrogram uses d3.cluster())
foreign import getLayout :: TreeType -> TreeLayoutFn_

-- | Unsafe FFI helpers for accessing tree node properties
foreign import unsafeFieldImpl :: forall a b. String -> a -> b
foreign import unsafeCoerceImpl :: forall a b. a -> b

-- | Check if a D3 hierarchy node has children
foreign import hasChildren_ :: D3HierarchyNode -> Boolean

-- | Accessor functions for tree node data
-- | These provide type-safe access to D3 hierarchy node properties
treeDatum_ ::
  { hasChildren :: Datum_ -> Boolean
  , name :: Datum_ -> String
  , x :: Datum_ -> Number
  , y :: Datum_ -> Number
  }
treeDatum_ =
  { hasChildren: \d -> hasChildren_ (unsafeCoerceImpl d)
  , name: \d -> (unsafeFieldImpl "data" d).name
  , x: \d -> unsafeFieldImpl "x" d
  , y: \d -> unsafeFieldImpl "y" d
  }

-- | Helper to coerce a Datum_ accessor function to work with any phantom-typed datum
-- | This is safe because phantom types exist only at compile time - at runtime,
-- | all D3 selections work with Datum_ (foreign any type)
-- | Usage: x (datum treeDatum_.x)
datum :: forall d a. (Datum_ -> a) -> (d -> a)
datum = unsafeCoerceImpl

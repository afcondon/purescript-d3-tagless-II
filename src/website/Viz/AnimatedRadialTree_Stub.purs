module D3.Viz.AnimatedRadialTree where

-- | Stub module for archived AnimatedRadialTree
-- | This allows Example.purs and LesMiserables.purs to compile
-- | All functions are no-ops

import Prelude
import Data.Unit (Unit)
import PSD3 (Datum_, D3Selection_)
import Unsafe.Coerce (unsafeCoerce)

-- Stub for TreeType (used in function signatures)
data TreeType = TidyTree | Dendrogram

instance showTreeType :: Show TreeType where
  show TidyTree = "TidyTree"
  show Dendrogram = "Dendrogram"

-- Stub functions
drawAnimatedRadialTree :: forall a b c m. Applicative m =>
  a -> b -> c -> m { svg :: D3Selection_ Unit, rotationGroup :: D3Selection_ Unit, linksGroup :: D3Selection_ Unit, nodesGroup :: D3Selection_ Unit, root :: Datum_ }
drawAnimatedRadialTree _ _ _ = pure $ unsafeCoerce unit

updateToLayout :: forall m a b c d. Applicative m => a -> b -> c -> d -> m Unit
updateToLayout _ _ _ _ = pure unit

rotateTree :: forall m a. Applicative m => Number -> Number -> a -> m Unit
rotateTree _ _ _ = pure unit

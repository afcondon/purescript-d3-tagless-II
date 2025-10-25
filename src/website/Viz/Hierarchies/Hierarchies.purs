module D3.Viz.Hierarchies where

import Prelude

import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Types (D3Selection_, Selector)
import D3.Viz.BubbleChart (draw) as Bubble
import D3.Viz.Icicle (draw) as Icicle
import D3.Viz.Treemap (draw) as Treemap
import Data.Tuple (Tuple)
import PSD3.Interpreter.D3 (runD3M)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Unsafe.Coerce (unsafeCoerce)

-- | FFI: Clear the visualization container
foreign import clearContainer_ :: Selector D3Selection_ -> Effect Unit

-- | Clear the visualization container
clearViz :: Selector D3Selection_ -> Effect Unit
clearViz = clearContainer_

-- | Draw circle packing layout
drawCirclePacking :: TreeJson_ -> Selector D3Selection_ -> Aff Unit
drawCirclePacking treeJson selector = liftEffect $ do
  -- Convert TreeJson_ to Json format for BubbleChart
  let jsonData = unsafeCoerce treeJson
  (_ :: Tuple Unit Unit) <- runD3M (Bubble.draw jsonData selector)
  pure unit

-- | Draw icicle layout
drawIcicle :: TreeJson_ -> Selector D3Selection_ -> Aff Unit
drawIcicle treeJson selector = liftEffect $ do
  (_ :: Tuple Unit Unit) <- runD3M (Icicle.draw treeJson selector)
  pure unit

-- | Draw treemap layout
drawTreemap :: TreeJson_ -> Selector D3Selection_ -> Aff Unit
drawTreemap treeJson selector = liftEffect $ do
  (_ :: Tuple Unit Unit) <- runD3M (Treemap.draw treeJson selector)
  pure unit

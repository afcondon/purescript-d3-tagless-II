module D3.Interpreter where

import D3.Selection (D3_Node, Join, Selector, ZoomConfig)
import Prelude (class Monad)



-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= D3InterpreterM selection m where
  attach :: Selector                  -> m selection
  append :: selection      -> D3_Node -> m selection
  join   :: ∀ a. selection -> Join a  -> m selection

  attachZoom :: selection -> ZoomConfig -> m selection -- this could very well be an extension of the monad

infix 4 join as <+>




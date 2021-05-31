module D3.Interpreter where

import D3.Data.Types (Selector)
import D3.Selection (Behavior, ChainableS, D3_Node, Join)
import Prelude (class Monad)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= D3InterpreterM selection m where
  attach :: Selector                          -> m selection
  append :: selection -> D3_Node              -> m selection
  filter :: selection -> Selector             -> m selection
  modify :: selection -> Array (ChainableS)   -> m selection
  on     :: selection -> Behavior             -> m selection

  join   :: ∀ datum.  selection -> Join datum -> m selection

infix 4 join as <+>

-- TODO things that are not handled by this (deliberately) ultra-simple grammar so far:
-- 1) say you wanted to attach to "div#hook" and then select an _already existing_ <h1> in it and apply Attrs to that h1
-- 2)...

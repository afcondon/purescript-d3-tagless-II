module PSD3.FpFtw.Actions where

import Prelude

-- | Actions for FP FTW component
data Action
  = Initialize
  | SelectExample String

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Initialize -> "Initialize"
    SelectExample ex -> "SelectExample " <> ex

module Halogen.Storybook.Proxy
  ( proxy
  ) where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slots f o = (child :: H.Slot f o Unit)

_child = Proxy :: Proxy "child"

-- | A proxy that hides both the Query and Message of wrapped component.
proxy
  :: forall f i o m
   . H.Component f i o m
  -> H.Component (Const Void) i Void m
proxy innerComponent = H.mkComponent
  { initialState: identity
  , render: render innerComponent
  , eval: H.mkEval H.defaultEval
  }

render
  :: forall f i o m
   . H.Component f i o m
  -> i
  -> H.ComponentHTML Void (Slots f o) m
render innerComponent state =
  HH.slot_ _child unit innerComponent state

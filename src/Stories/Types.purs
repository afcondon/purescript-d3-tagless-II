module D3Tagless.App.Routes.Types where

import Prelude

import D3.Simulation.Types (SimulationState_)
import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H

data Group
  = Examples
  | AltInterpreters
  | Application

derive instance eqGroup :: Eq Group
derive instance ordGroup :: Ord Group
instance showGroup :: Show Group where
  show Examples        = "Examples"
  show Application     = "Application"
  show AltInterpreters = "Alternative interpreters"

data RouteConfig = 
    SimpleRoute { anchor :: String
                , component :: H.Component (Const Void) Unit Void Aff
                , group :: Group
                }
  | SimulationRoute { anchor :: String
                    , component :: H.Component (Const Void) SimulationState_ Void Aff
                    , group :: Group
                    }

getGroup :: RouteConfig -> Group
getGroup = case _ of
    (SimpleRoute r )     -> r.group
    (SimulationRoute r ) -> r.group

getAnchor :: RouteConfig -> String
getAnchor = case _ of
    (SimpleRoute r )     -> r.anchor
    (SimulationRoute r ) -> r.anchor



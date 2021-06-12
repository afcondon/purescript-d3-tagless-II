module UIGuide.App.Routes
  ( routes, groups )
where

import Prelude
import Data.Const (Const)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import UIGuide.App (Group(..), proxy)
import UIGuide.Component.Modals as Modals
import Stories.GUP as D3GUP
import Stories.LesMis as LesMis
import Stories.MetaTree as MetaTree
import Stories.PrintTree as PrintTree
import Stories.Spago as Spago
import Stories.Trees as Trees

----------
-- Routes

groups :: Array Group
groups =
  [ Examples
  , Application
  ]

type RouteConfig =
  { anchor :: String
  , component :: H.Component (Const Void) Unit Void Aff
  , group :: Group
  }

routes :: Map String RouteConfig
routes = fromFoldable
  [ Tuple "gup"
    { anchor: "GUP"
    , component: proxy D3GUP.component
    , group: Examples
    }
    , Tuple "modals"
    { anchor: "Modals"
    , component: proxy Modals.component
    , group: Examples
    }
    , Tuple "lesmis"
    { anchor: "LesMis"
    , component: proxy LesMis.component
    , group: Examples
    }
  , Tuple "trees"
    { anchor: "Trees"
    , component: proxy Trees.component
    , group: Examples
    }
  , Tuple "metatree"
    { anchor: "Meta Trees"
    , component: proxy MetaTree.component
    , group: Examples
    }
  , Tuple "printtree"
    { anchor: "Print Tree"
    , component: proxy PrintTree.component
    , group: Examples
    }
  , Tuple "spago"
    { anchor: "Spago"
    , component: proxy Spago.component
    , group: Application
    }
  ]

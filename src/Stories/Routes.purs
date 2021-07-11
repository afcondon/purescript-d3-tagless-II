module D3Tagless.App.Routes
  ( routes, groups )
where


import D3Tagless.App (Group(..), proxy)
import D3Tagless.App.Routes.Types (RouteConfig(..))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
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
  , AltInterpreters
  , Application
  ]

routes :: Map String RouteConfig
routes = fromFoldable
  [ Tuple "gup" (SimpleRoute
                { anchor: "GUP"
                , component: proxy D3GUP.component
                , group: Examples
                })
  , Tuple "trees" (SimpleRoute
                { anchor: "Trees"
                , component: proxy Trees.component
                , group: Examples
                })
  , Tuple "metatree" (SimpleRoute
                { anchor: "Meta Trees"
                , component: proxy MetaTree.component
                , group: AltInterpreters
                })
  , Tuple "printtree" (SimpleRoute
                { anchor: "Print Tree"
                , component: proxy PrintTree.component
                , group: AltInterpreters
                })
  , Tuple "lesmis" (SimulationRoute     -- NB 
                { anchor: "LesMis"
                , component: proxy LesMis.component
                , group: Examples
                })
  , Tuple "spago" (SimpleRoute
                { anchor: "Spago"
                , component: proxy Spago.component
                , group: Application
                })
  ]

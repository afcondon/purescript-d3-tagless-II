module PSD3.Tutorial where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.GUP (Status(..))
import PSD3.ThreeLittleCircles as ThreeLittleCircles
import PSD3.GUP as GUP
import Type.Proxy (Proxy(..))

-- | Tutorial page state
type State = Unit

-- | Tutorial page actions
data Action = Initialize

-- | Child component slots
type Slots =
  ( threeLittleCircles :: forall q. H.Slot q Void Unit
  , gup :: forall q. H.Slot q Void Unit
  )

_threeLittleCircles = Proxy :: Proxy "threeLittleCircles"
_gup = Proxy :: Proxy "gup"

-- | Tutorial page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- Tutorial introduction
      HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Tutorial: Building Visualizations with PureScript D3" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat." ]
        , HH.p_
            [ HH.text "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ]
        ]

    -- Section 1: Three Little Circles
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Three Little Circles" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Vestibulum tortor quam, feugiat vitae, ultricies eget, tempor sit amet, ante." ]
        , HH.p_
            [ HH.text "Donec eu libero sit amet quam egestas semper. Aenean ultricies mi vitae est. Mauris placerat eleifend leo. Quisque sit amet est et sapien ullamcorper pharetra." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.slot_ _threeLittleCircles unit ThreeLittleCircles.component unit ]
        , HH.p_
            [ HH.text "Vestibulum erat wisi, condimentum sed, commodo vitae, ornare sit amet, wisi. Aenean fermentum, elit eget tincidunt condimentum, eros ipsum rutrum orci, sagittis tempus lacus enim ac dui." ]
        ]

    -- Section 2: General Update Pattern
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. The General Update Pattern" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In sagittis dui vel nisl. Duis ac tellus et risus vulputate vehicula. Donec lobortis risus a elit. Etiam tempor. Ut ullamcorper, ligula eu tempor congue, eros est euismod turpis." ]
        , HH.p_
            [ HH.text "Mauris sollicitudin fermentum libero. Praesent nonummy mi in odio. Nunc interdum lacus sit amet orci. Vestibulum rutrum, mi nec elementum vehicula, eros quam gravida nisl, id fringilla neque ante vel mi." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.slot_ _gup unit GUP.component Running ]
        , HH.p_
            [ HH.text "Sed egestas, ante et vulputate volutpat, eros pede semper est, vitae luctus metus libero eu augue. Morbi purus libero, faucibus adipiscing, commodo quis, gravida id, est." ]
        ]

    -- Section 3: Next Steps
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-conclusion" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Next Steps" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc, quis gravida magna mi a libero. Fusce vulputate eleifend sapien." ]
        , HH.ul_
            [ HH.li_ [ HH.text "Explore hierarchical data visualizations" ]
            , HH.li_ [ HH.text "Learn about the Finally Tagless pattern with interpreters" ]
            , HH.li_ [ HH.text "Dive into the Code Explorer for complex applications" ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

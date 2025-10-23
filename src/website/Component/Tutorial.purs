module PSD3.Tutorial where

import Prelude

import Control.Monad.Rec.Class (forever)
import D3.Viz.ThreeLittleCircles as Circles
import D3.Viz.GUP as GUP
import D3.Viz.BarChart as BarChart
import D3.Viz.LineChart as LineChart
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.Charts.Model (monthlySales, sineWaveData, anscombesQuartet)
import D3Tagless.Instance.Selection (eval_D3M, runD3M)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, killFiber)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Types (Route(..))

-- | Tutorial page state
type State = {
  gupFiber :: Maybe (Fiber Unit)
}

-- | Tutorial page actions
data Action = Initialize | Finalize

-- | Tutorial page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { gupFiber: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- TOC Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel" ] ]
        [ HH.img
            [ HP.src "bookmark.jpeg"
            , HP.alt ""
            , HP.classes [ HH.ClassName "toc-panel__bookmark-pin" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Contents" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "toc-panel__content" ] ]
                [ HH.nav
                    [ HP.classes [ HH.ClassName "toc-nav" ] ]
                    [ HH.a [ HP.href "#section-1", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "1. Three Little Circles" ]
                    , HH.a [ HP.href "#section-2", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "2. General Update Pattern" ]
                    , HH.a [ HP.href "#section-3", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "3. Data-Driven Positioning" ]
                    , HH.a [ HP.href "#section-4", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "4. Bar Charts with Scales" ]
                    , HH.a [ HP.href "#section-5", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "5. Line Charts and Paths" ]
                    , HH.a [ HP.href "#section-6", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "6. Anscombe's Quartet" ]
                    , HH.a [ HP.href "#section-7", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "7. Next Steps" ]
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.div
        [ HP.classes [ HH.ClassName "tutorial-page__nav-panel" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "tutorial-page__nav-title" ] ]
            [ HH.text "Explore" ]
        , HH.nav
            [ HP.classes [ HH.ClassName "tutorial-page__nav-links" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath About
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "About →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath Hierarchies
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "Hierarchies →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath Interpreters
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "Interpreters →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath CodeExplorer
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "Code Explorer →" ]
            , HH.a
                [ HP.href "https://github.com/afcondon/purescript-d3-tagless"
                , HP.target "_blank"
                , HP.rel "noopener noreferrer"
                , HP.classes [ HH.ClassName "tutorial-page__nav-link", HH.ClassName "tutorial-page__nav-link--external" ]
                ]
                [ HH.text "GitHub ↗" ]
            ]
        ]

    -- Tutorial introduction
    , HH.section
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
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-1"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Three Little Circles" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Vestibulum tortor quam, feugiat vitae, ultricies eget, tempor sit amet, ante." ]
        , HH.p_
            [ HH.text "Donec eu libero sit amet quam egestas semper. Aenean ultricies mi vitae est. Mauris placerat eleifend leo. Quisque sit amet est et sapien ullamcorper pharetra." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-circles-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Vestibulum erat wisi, condimentum sed, commodo vitae, ornare sit amet, wisi. Aenean fermentum, elit eget tincidunt condimentum, eros ipsum rutrum orci, sagittis tempus lacus enim ac dui." ]
        ]

    -- Section 2: General Update Pattern
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-2"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. The General Update Pattern" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In sagittis dui vel nisl. Duis ac tellus et risus vulputate vehicula. Donec lobortis risus a elit. Etiam tempor. Ut ullamcorper, ligula eu tempor congue, eros est euismod turpis." ]
        , HH.p_
            [ HH.text "Mauris sollicitudin fermentum libero. Praesent nonummy mi in odio. Nunc interdum lacus sit amet orci. Vestibulum rutrum, mi nec elementum vehicula, eros quam gravida nisl, id fringilla neque ante vel mi." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "gup-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Sed egestas, ante et vulputate volutpat, eros pede semper est, vitae luctus metus libero eu augue. Morbi purus libero, faucibus adipiscing, commodo quis, gravida id, est." ]
        ]

    -- Section 3: Parabola of Circles
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-3"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Data-Driven Positioning" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Using data to drive visual properties like position and color." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "parabola-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Vestibulum erat wisi, condimentum sed, commodo vitae, ornare sit amet, wisi." ]
        ]

    -- Section 4: Bar Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-4"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "4. Bar Charts with Scales" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Bar charts demonstrate the use of scales to map data to screen coordinates." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "barchart-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Praesent nonummy mi in odio. Nunc interdum lacus sit amet orci." ]
        ]

    -- Section 5: Line Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-5"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "5. Line Charts and Paths" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Line charts use SVG paths to connect data points smoothly." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "linechart-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Mauris sollicitudin fermentum libero. Vestibulum rutrum, mi nec elementum vehicula." ]
        ]

    -- Section 6: Anscombe's Quartet
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-6"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "6. Anscombe's Quartet" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Anscombe's quartet demonstrates why visualization matters - four datasets with identical statistics but very different distributions." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "quartet-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Sed egestas, ante et vulputate volutpat, eros pede semper est." ]
        ]

    -- Section 7: Next Steps with margin links
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-conclusion" ]
        , HP.id "section-7"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Next Steps" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc, quis gravida magna mi a libero. Fusce vulputate eleifend sapien." ]

        -- Contextual "learn more" links
        , HH.aside
            [ HP.classes [ HH.ClassName "tutorial-margin-note" ] ]
            [ HH.p
                [ HP.classes [ HH.ClassName "tutorial-margin-note__label" ] ]
                [ HH.text "Learn More" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath Hierarchies
                , HP.classes [ HH.ClassName "tutorial-margin-note__link" ]
                ]
                [ HH.text "Hierarchies →" ]
            ]

        , HH.ul_
            [ HH.li_ [ HH.text "Explore hierarchical data visualizations" ]
            , HH.li_ [ HH.text "Learn about the Finally Tagless pattern with interpreters" ]
            , HH.li_ [ HH.text "Dive into the Code Explorer for complex applications" ]
            ]

        -- More contextual links
        , HH.aside
            [ HP.classes [ HH.ClassName "tutorial-margin-note" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Interpreters
                , HP.classes [ HH.ClassName "tutorial-margin-note__link" ]
                ]
                [ HH.text "Interpreters →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath CodeExplorer
                , HP.classes [ HH.ClassName "tutorial-margin-note__link" ]
                ]
                [ HH.text "Code Explorer →" ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Draw Three Little Circles
    _ <- H.liftEffect $ eval_D3M $ Circles.drawThreeCircles "div.three-circles-viz"

    -- Set up General Update Pattern animation
    updateFn <- runGeneralUpdatePattern
    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
    H.modify_ (\state -> state { gupFiber = Just fiber })

    -- Draw Parabola of Circles
    _ <- H.liftEffect $ eval_D3M $ Circles.drawWithData [310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310] "div.parabola-viz"

    -- Draw Bar Chart
    _ <- H.liftEffect $ eval_D3M $ BarChart.draw monthlySales "div.barchart-viz"

    -- Draw Line Chart
    _ <- H.liftEffect $ eval_D3M $ LineChart.draw sineWaveData "div.linechart-viz"

    -- Draw Anscombe's Quartet
    _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet anscombesQuartet "div.quartet-viz"

    pure unit

  Finalize -> do
    -- Kill the GUP animation fiber
    maybeFiber <- H.gets _.gupFiber
    case maybeFiber of
      Nothing -> pure unit
      Just fiber -> H.liftAff $ killFiber (error "Cancelling GUP animation") fiber

-- Helper functions for GUP animation
runGeneralUpdatePattern :: forall m. MonadEffect m => m (Array Char -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  update <- H.liftEffect $ eval_D3M $ GUP.exGeneralUpdatePattern "div.gup-viz"
  pure (\letters -> H.liftEffect $ runD3M (update letters) *> pure unit)

runUpdate :: (Array Char -> Aff Unit) -> Aff Unit
runUpdate update = do
  letters <- H.liftEffect $ getLetters
  update letters
  delay (Milliseconds 2300.0)
  where
    -- | choose a string of random letters (no duplicates), ordered alphabetically
    getLetters :: Effect (Array Char)
    getLetters = do
      let
        letters = toCharArray "abcdefghijklmnopqrstuvwxyz"
        coinToss :: Char -> Effect (Maybe Char)
        coinToss c = do
          n <- random
          pure $ if n > 0.6 then Just c else Nothing

      choices <- sequence $ coinToss <$> letters
      pure $ catMaybes choices

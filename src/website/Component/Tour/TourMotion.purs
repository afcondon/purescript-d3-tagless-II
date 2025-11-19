module Component.Tour.TourMotion where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import Effect.Class (liftEffect)
import Effect.Aff (Milliseconds(..), delay)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)
import D3.Viz.TreeAPI.ThreeLittleCirclesTransition as ThreeLittleCirclesTransition
import Effect.Ref as Ref
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import PSD3v2.Selection.Types (SBoundOwns)
import Web.DOM.Element (Element)
import D3.Viz.GUP as GUP
import PSD3.Interpreter.D3 (eval_D3M, runD3M)
import D3.Viz.AnimatedTreeClusterLoop as AnimatedTreeLoop
import D3.Viz.TreeAPI.LesMisSimple as LesMisSimple
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import Data.Array (catMaybes)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect.Random (random)
import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff)
import Effect (Effect)

-- | Tour page state
type State =
  { gupFiber :: Maybe (H.ForkId)
  , colorMixingTrigger :: Maybe { stateRef :: Ref.Ref ThreeLittleCirclesTransition.CircleState
                                 , circlesSel :: D3v2Selection_ SBoundOwns Element ThreeLittleCirclesTransition.CircleData }
  }

-- | Tour page actions
data Action = Initialize | Finalize | TriggerColorMixing

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { gupFiber: Nothing, colorMixingTrigger: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Render Section 1: Three Circles Transition
    trigger <- liftEffect $ ThreeLittleCirclesTransition.threeLittleCirclesTransition "#viz"
    H.modify_ _ { colorMixingTrigger = Just trigger }

    -- Render Section 2: General Update Pattern (v1 restored)
    update <- liftEffect $ eval_D3M $ GUP.exGeneralUpdatePattern "#gup-container"
    forkId <- H.fork $ forever do
      letters <- liftEffect getLetters
      liftEffect $ runD3M (update letters) *> pure unit
      H.liftAff $ delay (Milliseconds 2300.0)
    H.modify_ _ { gupFiber = Just forkId }

    -- Render Section 3: Animated Tree (load flare data and start loop)
    flareResult <- H.liftAff $ AJAX.get ResponseFormat.json "./data/flare-2.json"
    case flareResult of
      Left err -> pure unit  -- Silently fail for now
      Right response -> do
        let flareData = unsafeCoerce response.body
        liftEffect $ AnimatedTreeLoop.startAnimatedTreeClusterLoop flareData "#animated-tree-container"

    -- Render Section 4: Les Mis simulation (load data and start simulation)
    lesMisResult <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    let lesMisModel = readGraphFromFileContents lesMisResult
    liftEffect $ LesMisSimple.startLesMisSimple lesMisModel "#lesmis-container"

    pure unit

  Finalize -> do
    state <- H.get
    case state.gupFiber of
      Nothing -> pure unit
      Just forkId -> H.kill forkId
    H.modify_ _ { gupFiber = Nothing }

  TriggerColorMixing -> do
    state <- H.get
    case state.colorMixingTrigger of
      Nothing -> pure unit
      Just trigger -> liftEffect $ ThreeLittleCirclesTransition.createTransitionTrigger trigger

-- | Choose a string of random letters (no duplicates), ordered alphabetically
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

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourMotion
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "5. Movement & Transition" ]
            , HH.p_
                [ HH.text "This page explores transitions and movement in D3 visualizations, from simple color and position changes to complex physics simulations with the General Update Pattern." ]
            ]

        -- Section 1: Three Circles Color Mixing
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-1"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Basic Transitions: Color Mixing" ]
            , HH.p_
                [ HH.text "Transitions allow smooth animated changes to visual properties. This simple example shows three circles transitioning from green to RGB primary colors (red, green, blue) with 50% opacity. The circles reposition to overlap, demonstrating additive color mixing where overlapping colors create secondary colors: cyan (green + blue), magenta (red + blue), and yellow (red + green)." ]
            , HH.div
                [ HP.id "viz"
                , HP.classes [ HH.ClassName "viz-container" ]
                ]
                []
            , HH.button
                [ HP.classes [ HH.ClassName "transition-button" ]
                , HE.onClick \_ -> TriggerColorMixing
                ]
                [ HH.text "Transition to RGB" ]
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
                [ HH.text "This deceptively simple example shows off an aspect of screen-based data visualization that has no analogue in paper visualizations: the ability to specify how updates to the data should be represented." ]
            , HH.p_
                [ HH.text "In this example, some letters of the alphabet are presented and then constantly updated. When a letter enters at first, it falls in from the top and it is green. If it's still present in the next set of letters it stays on the screen, but it turns gray and moves to an alphabetically correct new position. And if it's not present in the new data, it turns red and falls out before disappearing." ]
            , HH.div
                [ HP.id "gup-container"
                , HP.classes [ HH.ClassName "viz-container" ]
                ]
                []
            , HH.p_
                [ HH.text "The example automatically cycles through random letter selections every 2 seconds, demonstrating all three parts of the pattern: green letters entering from above, gray letters sliding to new positions, and brown letters exiting below." ]
            ]

        -- Section 3: Animated Tree Transitions
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-3"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Animated Tree Transitions" ]
            , HH.p_
                [ HH.text "This demonstration shows smooth automatic transitions between Tidy Tree and Dendrogram (Cluster) layouts. The visualization automatically alternates between layouts every 3 seconds. The same 252 nodes from the Flare visualization toolkit smoothly animate to their new positions—no enter/exit, just updates." ]
            , HH.p_
                [ HH.text "The transitions are implemented using D3's data join with identity-based keys, ensuring each node maintains its identity across layout changes. Children are sorted by height to eliminate crossovers during animation, creating smooth and comprehensible transitions." ]
            , HH.div
                [ HP.id "animated-tree-container"
                , HP.classes [ HH.ClassName "viz-container" ]
                ]
                []
            , HH.p_
                [ HH.text "For a version with manual controls, visit the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath AnimatedTreeCluster ]
                    [ HH.text "full Animated Tree ↔ Cluster demo" ]
                , HH.text "."
                ]
            ]

        -- Section 4: Les Misérables Force Layout
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-4"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "4. Force-Directed Graph: Les Misérables" ]
            , HH.p_
                [ HH.text "Force-directed graphs use physics simulation to position nodes and links. Nodes repel each other like charged particles, while links act as springs pulling connected nodes together. The simulation finds an equilibrium that naturally reveals the structure of the network." ]
            , HH.p_
                [ HH.text "The graph shows character co-occurrence in Victor Hugo's Les Misérables. Try dragging nodes to see the force simulation respond - the simulation applies multiple forces: center (pulls toward middle), charge (nodes repel), collision (prevents overlap), and link (pulls connected nodes together). You can also zoom and pan using the mouse wheel and drag gestures."
                ]
            , HH.div
                [ HP.id "lesmis-container"
                , HP.classes [ HH.ClassName "viz-container" ]
                ]
                []
            ]
        ]
    ]

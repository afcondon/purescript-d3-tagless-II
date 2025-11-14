module PSD3.Component.PSD3v2Examples where

import Prelude

import Data.Array (catMaybes)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence, traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import D3.Viz.ThreeLittleCirclesV2 as ThreeLittleCirclesV2
import D3.Viz.ThreeLittleCirclesTransitionV2 as ThreeLittleCirclesTransitionV2
import D3.Viz.GUPV2 as GUPV2
import D3.Viz.TreeVizV2 as TreeVizV2
import PSD3v2.Interpreter.D3v2 as D3v2
import Halogen.HTML.Events as HE

type State = { gupInitialized :: Boolean }

data Action
  = Initialize
  | UpdateGUPRandom

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> { gupInitialized: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "psd3v2-examples-page" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "page-header" ] ]
        [ HH.h1_ [ HH.text "PSD3v2 Examples" ]
        , HH.p
            [ HP.classes [ HH.ClassName "page-description" ] ]
            [ HH.text "Type-safe D3 visualizations using phantom types and tagless final architecture" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "examples-grid" ] ]
        [ renderExample
            { id: "three-little-circles-v2"
            , title: "Three Little Circles"
            , description: "The classic D3 example reimplemented with PSD3v2"
            }
        , renderExample
            { id: "three-circles-transition-v2"
            , title: "Three Circles with Transitions"
            , description: "Animated transitions showing color mixing with elastic easing"
            }
        , renderGUPExample
        , renderExample
            { id: "tree-v2"
            , title: "Tree Layout"
            , description: "Reingold-Tilford tree layout algorithm (non-animating)"
            }
        ]
    ]

renderGUPExample :: forall m. HH.HTML m Action
renderGUPExample =
  HH.div
    [ HP.classes [ HH.ClassName "example-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "example-title" ] ]
        [ HH.text "General Update Pattern (GUP)" ]
    , HH.p
        [ HP.classes [ HH.ClassName "example-description" ] ]
        [ HH.text "Enter-Update-Exit pattern with transitions. Click button to see random letters animate in/out." ]
    , HH.div
        [ HP.classes [ HH.ClassName "gup-controls" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "gup-button" ]
            , HE.onClick \_ -> UpdateGUPRandom
            ]
            [ HH.text "Update with Random Letters" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "example-viz-container" ]
        , HP.id "gup-v2"
        ]
        []
    ]

renderExample :: forall w. { id :: String, title :: String, description :: String } -> HH.HTML w Action
renderExample { id, title, description } =
  HH.div
    [ HP.classes [ HH.ClassName "example-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "example-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "example-description" ] ]
        [ HH.text description ]
    , HH.div
        [ HP.classes [ HH.ClassName "example-viz-container" ]
        , HP.id id
        ]
        []
    ]

-- | Generate a random subset of the alphabet (no duplicates, already sorted)
-- | Matches the classic D3 GUP demo behavior
generateRandomLetters :: forall m. MonadAff m => m String
generateRandomLetters = do
  let
    alphabet = toCharArray "abcdefghijklmnopqrstuvwxyz"

    -- Coin toss for each letter (60% probability of inclusion)
    coinToss :: Char -> m (Maybe Char)
    coinToss c = do
      n <- liftEffect random
      pure $ if n > 0.4 then Just c else Nothing

  -- Filter alphabet through coin toss
  choices <- traverse coinToss alphabet
  pure $ fromCharArray $ catMaybes choices

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "PSD3v2Examples: Initializing"
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Render Three Little Circles V2 (static)
    H.liftEffect $ D3v2.runD3v2M $ ThreeLittleCirclesV2.drawThreeCircles "#three-little-circles-v2"

    -- Render Three Circles with Transitions (animated)
    H.liftEffect $ D3v2.runD3v2M $ ThreeLittleCirclesTransitionV2.drawThreeCirclesTransition "#three-circles-transition-v2"

    -- Initialize GUP example with random letters
    randomText <- generateRandomLetters
    log $ "GUP: Initial text = " <> randomText
    H.liftEffect $ D3v2.runD3v2M do
      _ <- GUPV2.initializeGUP "#gup-v2"
      GUPV2.updateText "#letter-group" randomText

    -- Render Tree V2 (non-animating)
    H.liftEffect $ D3v2.runD3v2M $ TreeVizV2.drawTree "#tree-v2"

    H.modify_ _ { gupInitialized = true }

    pure unit

  UpdateGUPRandom -> do
    state <- H.get
    when state.gupInitialized do
      randomText <- generateRandomLetters
      log $ "GUP: New text = " <> randomText
      H.liftEffect $ D3v2.runD3v2M $ GUPV2.updateText "#letter-group" randomText

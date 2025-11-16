module PSD3.WealthHealth.HTML where

import Prelude

import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.WealthHealth.Actions (Action(..))
import PSD3.WealthHealth.State (State, LabelMode(..))
import PSD3.WealthHealth.Types (Region(..), regionName, regionColor)

-- | Render the control panel with scrubber, play/pause, and other controls
renderControlPanel :: forall w. State -> HH.HTML w Action
renderControlPanel state =
  HH.div
    [ HP.classes [ HH.ClassName "wealth-health-controls" ] ]
    [ -- Year display (large, prominent)
      HH.div
        [ HP.classes [ HH.ClassName "wealth-health-year-display" ] ]
        [ HH.text $ show state.currentYear ]

    -- Scrubber (range slider)
    , HH.div
        [ HP.classes [ HH.ClassName "wealth-health-scrubber" ] ]
        [ HH.label_
            [ HH.text "Year" ]
        , HH.input
            [ HP.type_ HP.InputRange
            , HP.min $ toNumber $ fromMaybe 1800 (map (_.yearRange.min) state.model)
            , HP.max $ toNumber $ fromMaybe 2009 (map (_.yearRange.max) state.model)
            , HP.value $ show state.currentYear
            , HP.step $ HP.Step 1.0
            , HE.onValueInput \val -> SetYear (fromMaybe state.currentYear (toInt val))
            , HP.classes [ HH.ClassName "wealth-health-scrubber__slider" ]
            ]
        ]

    -- Play/Pause button
    , HH.div
        [ HP.classes [ HH.ClassName "wealth-health-playback" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "wealth-health-playback__button" ]
            , HE.onClick \_ -> TogglePlay
            ]
            [ HH.text if state.playing then "⏸ Pause" else "▶ Play" ]
        ]

    -- Speed control
    , HH.div
        [ HP.classes [ HH.ClassName "wealth-health-speed" ] ]
        [ HH.label_
            [ HH.text "Speed" ]
        , HH.input
            [ HP.type_ HP.InputRange
            , HP.min 1.0
            , HP.max 10.0
            , HP.value $ show state.animationSpeed
            , HP.step $ HP.Step 0.5
            , HE.onValueInput \val -> SetAnimationSpeed (fromMaybe 5.0 (toNum val))
            , HP.classes [ HH.ClassName "wealth-health-speed__slider" ]
            ]
        , HH.span
            [ HP.classes [ HH.ClassName "wealth-health-speed__value" ] ]
            [ HH.text $ show state.animationSpeed <> " years/sec" ]
        ]

    -- Label display toggle
    , HH.div
        [ HP.classes [ HH.ClassName "wealth-health-labels" ] ]
        [ HH.label_
            [ HH.text "Show labels:" ]
        , HH.button
            [ HP.classes [ HH.ClassName "wealth-health-labels__toggle" ]
            , HE.onClick \_ -> ToggleLabels
            ]
            [ HH.text $ case state.labelMode of
                AlwaysShow -> "Always"
                OnHoverOnly -> "Only on hover"
            ]
        ]
    ]
  where
    toInt :: String -> Maybe Int
    toInt s = map floor (toNum s)

    toNum :: String -> Maybe Number
    toNum s = Number.fromString s

-- | Render the region legend
renderLegend :: forall w i. HH.HTML w i
renderLegend =
  HH.div
    [ HP.classes [ HH.ClassName "wealth-health-legend" ] ]
    [ HH.h3_ [ HH.text "Regions" ]
    , HH.div
        [ HP.classes [ HH.ClassName "wealth-health-legend__items" ] ]
        (renderLegendItem <$> allRegions)
    ]

-- | Render a single legend item
renderLegendItem :: forall w i. Region -> HH.HTML w i
renderLegendItem region =
  HH.div
    [ HP.classes [ HH.ClassName "wealth-health-legend__item" ] ]
    [ HH.span
        [ HP.classes [ HH.ClassName "wealth-health-legend__color" ]
        , HP.style $ "background-color: " <> regionColor region
        ]
        []
    , HH.span
        [ HP.classes [ HH.ClassName "wealth-health-legend__label" ] ]
        [ HH.text $ regionName region ]
    ]

-- | All regions for the legend
allRegions :: Array Region
allRegions =
  [ EastAsiaAndPacific
  , Europe
  , LatinAmericaAndCaribbean
  , MiddleEastAndNorthAfrica
  , SouthAsia
  , SubSaharanAfrica
  , NorthAmerica
  ]

-- | Render tooltip for a hovered nation
renderTooltip :: forall w i. Maybe String -> Maybe { income :: Number, population :: Number, lifeExpectancy :: Number, region :: Region } -> HH.HTML w i
renderTooltip hoveredNation nationData =
  case hoveredNation, nationData of
    Just name, Just dataRec ->
      HH.div
        [ HP.classes [ HH.ClassName "wealth-health-tooltip" ] ]
        [ HH.h4_ [ HH.text name ]
        , HH.div_ [ HH.text $ "Region: " <> regionName dataRec.region ]
        , HH.div_ [ HH.text $ "Income: $" <> formatNumber dataRec.income ]
        , HH.div_ [ HH.text $ "Population: " <> formatPopulation dataRec.population ]
        , HH.div_ [ HH.text $ "Life Expectancy: " <> formatNumber dataRec.lifeExpectancy <> " years" ]
        ]
    _, _ ->
      HH.text ""

-- | Format a number with commas
formatNumber :: Number -> String
formatNumber n =
  show $ floor n  -- TODO: Add proper formatting with commas

-- | Format population (in millions)
formatPopulation :: Number -> String
formatPopulation n =
  let millions = n / 1000000.0
  in show (floor millions) <> "M"

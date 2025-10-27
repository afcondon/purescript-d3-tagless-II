module PSD3.CodeAtlas.Tabs.Declarations where

import Prelude

import Data.Array (filter, length, sortBy, take)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.CodeAtlas.Actions (Action(..))
import PSD3.CodeAtlas.State (State)
import PSD3.CodeAtlas.Types (Declaration, DeclarationsData)

-- | Render the Declarations tab
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  case state.declarationsData of
    Nothing ->
      HH.div_ [ HH.text "No data loaded" ]

    Just data_ ->
      HH.div
        [ HP.classes [ HH.ClassName "declarations-tab" ] ]
        [ -- Stats summary
          renderStats data_.stats

        -- Search and filters
        , renderFilters state data_

        -- Declarations table
        , renderDeclarationsTable state data_
        ]

-- | Render statistics summary
renderStats :: forall w. { totalModules :: Int, totalDeclarations :: Int, byKind :: Array { kind :: String, count :: Int } } -> HH.HTML w Action
renderStats stats =
  HH.div
    [ HP.classes [ HH.ClassName "atlas-stats" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "stat-card" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "stat-value" ] ] [ HH.text $ show stats.totalModules ]
        , HH.div [ HP.classes [ HH.ClassName "stat-label" ] ] [ HH.text "Modules" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "stat-card" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "stat-value" ] ] [ HH.text $ show stats.totalDeclarations ]
        , HH.div [ HP.classes [ HH.ClassName "stat-label" ] ] [ HH.text "Declarations" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "stat-card" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "stat-value" ] ] [ HH.text $ show (length stats.byKind) ]
        , HH.div [ HP.classes [ HH.ClassName "stat-label" ] ] [ HH.text "Kinds" ]
        ]
    ]

-- | Render search and filter controls
renderFilters :: forall w. State -> DeclarationsData -> HH.HTML w Action
renderFilters state data_ =
  HH.div
    [ HP.classes [ HH.ClassName "atlas-filters" ] ]
    [ -- Search box
      HH.div
        [ HP.classes [ HH.ClassName "filter-group" ] ]
        [ HH.label_ [ HH.text "Search" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "Search declarations..."
            , HP.value state.searchQuery
            , HE.onValueInput SetSearchQuery
            , HP.classes [ HH.ClassName "search-input" ]
            ]
        ]

    -- Kind filter
    , HH.div
        [ HP.classes [ HH.ClassName "filter-group" ] ]
        [ HH.label_ [ HH.text "Kind" ]
        , HH.select
            [ HE.onValueChange \val -> SetKindFilter (if val == "" then Nothing else Just val)
            , HP.classes [ HH.ClassName "filter-select" ]
            ]
            ([ HH.option [ HP.value "" ] [ HH.text "All" ] ] <>
             (data_.stats.byKind <#> \kindStat ->
               HH.option
                 [ HP.value kindStat.kind
                 , HP.selected (state.selectedKindFilter == Just kindStat.kind)
                 ]
                 [ HH.text $ kindStat.kind <> " (" <> show kindStat.count <> ")" ]
             ))
        ]

    -- Clear filters button
    , HH.button
        [ HE.onClick \_ -> ClearFilters
        , HP.classes [ HH.ClassName "clear-filters-btn" ]
        , HP.disabled (state.searchQuery == "" && state.selectedKindFilter == Nothing)
        ]
        [ HH.text "Clear Filters" ]
    ]

-- | Render the declarations table
renderDeclarationsTable :: forall w. State -> DeclarationsData -> HH.HTML w Action
renderDeclarationsTable state data_ =
  let
    -- Flatten all declarations with their module names
    allDeclarations = data_.modules >>= \mod ->
      mod.declarations <#> \decl -> decl { module = mod.name }

    -- Apply filters
    filtered = allDeclarations
      # filterBySearch state.searchQuery
      # filterByKind state.selectedKindFilter
      # sortBy compareDeclarations
      # take 1000  -- Limit to first 1000 for performance

    resultCount = length filtered
    totalCount = length allDeclarations
  in
    HH.div
      [ HP.classes [ HH.ClassName "declarations-table-container" ] ]
      [ -- Result count
        HH.div
          [ HP.classes [ HH.ClassName "result-count" ] ]
          [ HH.text $ "Showing " <> show resultCount <> " of " <> show totalCount <> " declarations"
          , if resultCount >= 1000
              then HH.span [ HP.classes [ HH.ClassName "limit-note" ] ] [ HH.text " (limited to first 1000)" ]
              else HH.text ""
          ]

      -- Table
      , HH.table
          [ HP.classes [ HH.ClassName "declarations-table" ] ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "Name" ]
                  , HH.th_ [ HH.text "Kind" ]
                  , HH.th_ [ HH.text "Module" ]
                  , HH.th_ [ HH.text "Documentation" ]
                  ]
              ]
          , HH.tbody_
              (filtered <#> renderDeclarationRow)
          ]
      ]

-- | Render a single declaration row
renderDeclarationRow :: forall w. Declaration -> HH.HTML w Action
renderDeclarationRow decl =
  HH.tr
    [ HP.classes [ HH.ClassName "declaration-row" ] ]
    [ HH.td
        [ HP.classes [ HH.ClassName "decl-name" ] ]
        [ HH.code_ [ HH.text decl.title ] ]
    , HH.td
        [ HP.classes [ HH.ClassName "decl-kind" ] ]
        [ HH.span
            [ HP.classes [ HH.ClassName "kind-badge", HH.ClassName ("kind-" <> decl.kind) ] ]
            [ HH.text decl.kind ]
        ]
    , HH.td
        [ HP.classes [ HH.ClassName "decl-module" ] ]
        [ HH.text decl.module ]
    , HH.td
        [ HP.classes [ HH.ClassName "decl-comments" ] ]
        [ HH.text $ truncate 100 decl.comments ]
    ]

-- | Filter declarations by search query
filterBySearch :: String -> Array Declaration -> Array Declaration
filterBySearch "" decls = decls
filterBySearch query decls =
  let lowerQuery = toLower query
  in filter (\decl ->
    contains (Pattern lowerQuery) (toLower decl.title) ||
    contains (Pattern lowerQuery) (toLower decl.module) ||
    contains (Pattern lowerQuery) (toLower decl.comments)
  ) decls

-- | Filter by kind
filterByKind :: Maybe String -> Array Declaration -> Array Declaration
filterByKind Nothing decls = decls
filterByKind (Just kind) decls =
  filter (\decl -> decl.kind == kind) decls

-- | Compare declarations for sorting (by module, then name)
compareDeclarations :: Declaration -> Declaration -> Ordering
compareDeclarations a b =
  case compare a.module b.module of
    EQ -> compare a.title b.title
    other -> other

-- | Truncate text to max length
truncate :: Int -> String -> String
truncate maxLen text =
  if String.length text > maxLen
    then String.take (maxLen - 3) text <> "..."
    else text

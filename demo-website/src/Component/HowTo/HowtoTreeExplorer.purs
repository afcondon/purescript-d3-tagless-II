module Component.HowTo.HowtoTreeExplorer where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (pow, sqrt, log)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Data.String.CodePoints (codePointAt)
import Data.Enum (fromEnum)
import Control.Comonad.Cofree (head, tail)
import Data.Tree (Tree, mkTree)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DataViz.Layout.Hierarchy.Tree as Tree
import DataViz.Layout.Hierarchy.Link (LinkStyle(..), linkGenerator)
import PSD3.Shared.Data (loadFlareData)
import PSD3.Expr.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import PSD3.Shared.SiteNav as SiteNav
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

-- | Layer scale function presets
data LayerScalePreset
  = Linear
  | SquareRoot
  | Logarithmic
  | Exponential
  | Custom

derive instance eqLayerScalePreset :: Eq LayerScalePreset

-- | Hierarchy node type (matches loadFlareData output)
type HierNode = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }

-- | Node with computed radius for variable-size visualization
type NodeWithRadius = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int, radius :: Number }

-- | Link data type
type LinkDatum = { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }

-- | Compute radius for a node based on pseudo-random from name
-- | Returns values in tree units (small) - will be scaled with x coordinates
computeRadius :: HierNode -> Number
computeRadius node =
  let
    -- Simple hash from name length and first char code
    charCode = maybe 0 fromEnum (codePointAt 0 node.name)
    hash = toNumber (length node.name) + toNumber charCode
    -- Scale to get variation, use remainder for pseudo-randomness
    base = hash - (floor (hash / 4.0)) * 4.0 + 1.0
  in
    base * 0.5 + 0.5 -- Range roughly 1.0 to 2.5 in tree units

foreign import floor :: Number -> Number

-- | Add radius field to node
addRadius :: HierNode -> NodeWithRadius
addRadius node =
  { name: node.name
  , value: node.value
  , x: node.x
  , y: node.y
  , depth: node.depth
  , height: node.height
  , radius: computeRadius node
  }

-- | Separation function based on node radii
-- | Returns distance between centers, so needs full sum of radii plus gap
radiusSeparation :: NodeWithRadius -> NodeWithRadius -> Number
radiusSeparation a b = a.radius + b.radius + 0.5

-- | Compute the pixel-to-tree-unit scale factor from positioned nodes
-- | Groups nodes by depth, finds adjacent pairs, computes distance/separation ratio
computeScaleFactor :: Number -> Array NodeWithRadius -> Number
computeScaleFactor minSep nodes =
  let
    -- Group nodes by depth
    grouped = foldl (\m n -> Map.insertWith (<>) n.depth [ n ] m) Map.empty nodes

    -- For each depth level, get scale samples from adjacent pairs
    samples = Array.concatMap getScaleSamples (Array.fromFoldable $ Map.values grouped)

    -- Average the samples, or use fallback
    avgScale =
      if Array.length samples == 0 then 5.0 -- fallback
      else (foldl (+) 0.0 samples) / toNumber (Array.length samples)
  in
    avgScale
  where
  getScaleSamples :: Array NodeWithRadius -> Array Number
  getScaleSamples depthNodes =
    let
      -- Sort by x coordinate
      sorted = Array.sortWith _.x depthNodes
      -- Get adjacent pairs
      pairs = Array.zip sorted (Array.drop 1 sorted)
    in
      Array.mapMaybe computePairScale pairs

  computePairScale :: Tuple NodeWithRadius NodeWithRadius -> Maybe Number
  computePairScale (Tuple a b) =
    let
      pixelDist = b.x - a.x
      -- Expected tree-unit separation = radiusSeparation + minSep
      expectedSep = radiusSeparation a b + minSep
    in
      -- Only include if reasonable
      if pixelDist > 1.0 && expectedSep > 0.01 then Just (pixelDist / expectedSep)
      else Nothing

-- | Compute color for a node (HSL with varying hue)
computeColor :: NodeWithRadius -> String
computeColor node =
  let
    -- Use different part of hash for color variation
    charCode = maybe 65 fromEnum (codePointAt 1 node.name)
    hue = toNumber (charCode * 7) - (floor (toNumber (charCode * 7) / 360.0)) * 360.0
  in
    "hsl(" <> show hue <> ", 70%, 50%)"

-- | Component state
type State =
  { flareData :: Maybe (Tree HierNode)
  , minSeparation :: Number
  , layerScalePreset :: LayerScalePreset
  , exponent :: Number -- For exponential preset
  , linkStyle :: LinkStyle
  , error :: Maybe String
  }

-- | Actions
data Action
  = Initialize
  | SetMinSeparation Number
  | SetLayerScalePreset LayerScalePreset
  | SetExponent Number
  | SetLinkStyle LinkStyle
  | Redraw

-- | Component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: forall i. i -> State
initialState _ =
  { flareData: Nothing
  , minSeparation: 1.0
  , layerScalePreset: Linear
  , exponent: 1.5
  , linkStyle: BezierVertical
  , error: Nothing
  }

-- | Load and draw tree
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Flare data
    result <- H.liftAff loadFlareData
    case result of
      Left err -> H.modify_ _ { error = Just err }
      Right tree -> do
        H.modify_ _ { flareData = Just tree, error = Nothing }
        -- Draw initial visualization
        state <- H.get
        liftEffect $ drawTreeExplorer "#tree-explorer-viz" state tree

  SetMinSeparation sep -> do
    H.modify_ _ { minSeparation = sep }
    handleAction Redraw

  SetLayerScalePreset preset -> do
    H.modify_ _ { layerScalePreset = preset }
    handleAction Redraw

  SetExponent exp -> do
    H.modify_ _ { exponent = exp }
    handleAction Redraw

  SetLinkStyle style -> do
    H.modify_ _ { linkStyle = style }
    handleAction Redraw

  Redraw -> do
    state <- H.get
    case state.flareData of
      Nothing -> pure unit
      Just tree -> liftEffect $ drawTreeExplorer "#tree-explorer-viz" state tree

-- | Create links from parent to children
makeLinks
  :: forall r
   . Tree { x :: Number, y :: Number | r }
  -> Array { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinks tree' = Array.fromFoldable $ makeLinksList tree'
  where
  makeLinksList
    :: Tree { x :: Number, y :: Number | r }
    -> List { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
  makeLinksList t =
    let
      val = head t
      children = tail t
      childLinks = children >>= \child ->
        let childVal = head child
        in Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- | Link path generator (step links for dendrogram style)
linkPath :: Number -> Number -> Number -> Number -> String
linkPath x1' y1' x2' y2' =
  "M" <> show x1' <> "," <> show y1'
    <> "V"
    <> show ((y1' + y2') / 2.0)
    <> "H"
    <> show x2'
    <> "V"
    <> show y2'

-- | Clear container contents
clearContainer :: String -> Effect Unit
clearContainer selector = do
  win <- window
  doc <- document win
  maybeEl <- querySelector (QuerySelector selector) (toParentNode doc)
  case maybeEl of
    Nothing -> pure unit
    Just el -> setInnerHTML el ""

foreign import setInnerHTML :: Element -> String -> Effect Unit

-- | Draw tree with current state configuration
drawTreeExplorer :: String -> State -> Tree HierNode -> Effect Unit
drawTreeExplorer selector state flareTree = do
  -- Clear existing SVG first
  clearContainer selector

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    let chartWidth = 800.0
    let chartHeight = 600.0
    let padding = 40.0

    -- Add radius to each node for variable sizing
    let treeWithRadius = map addRadius flareTree

    -- Build TreeConfig from state with separation function
    let layerScaleFn = getLayerScaleFn state.layerScalePreset state.exponent
    let
      config :: Tree.TreeConfig NodeWithRadius
      config = Tree.defaultTreeConfig
        { size =
            { width: chartWidth - (2.0 * padding)
            , height: chartHeight - (2.0 * padding)
            }
        , minSeparation = state.minSeparation
        , layerScale = Just layerScaleFn
        , separation = Just radiusSeparation
        }

    -- Get link generator for selected style
    let linkPathFn = linkGenerator state.linkStyle

    -- Apply Tree layout
    let positioned = Tree.tree config treeWithRadius

    -- Flatten to arrays
    let nodes = Array.fromFoldable positioned
    let links = makeLinks positioned

    -- Compute actual scale factor from positioned nodes
    let radiusScale = computeScaleFactor state.minSeparation nodes

    liftEffect $ Console.log $ "TreeExplorer: " <> show (Array.length nodes) <> " nodes, minSep=" <> show state.minSeparation <> ", radiusScale=" <> show radiusScale

    -- Build SVG tree structure for links
    let
      linksTree :: T.Tree LinkDatum
      linksTree =
        T.named SVG "svg"
          [ v3Attr "width" (lit chartWidth)
          , v3Attr "height" (lit chartHeight)
          , v3AttrStr "viewBox" (str ("0 0 " <> show chartWidth <> " " <> show chartHeight))
          , v3AttrStr "class" (str "tree-explorer-svg")
          ]
          `T.withChild`
            ( T.named Group "chartGroup"
                [ v3AttrStr "class" (str "tree-content") ]
                `T.withChild`
                  ( T.named Group "linksGroup"
                      [ v3AttrStr "class" (str "links") ]
                      `T.withChild`
                        ( T.joinData "links" "path" links $ \link ->
                            T.elem Path
                              [ v3AttrStr "d" (str
                                  ( linkPathFn
                                      (link.source.x + padding)
                                      (link.source.y + padding)
                                      (link.target.x + padding)
                                      (link.target.y + padding)
                                  ))
                              , v3AttrStr "fill" (str "none")
                              , v3AttrStr "stroke" (str "#555")
                              , v3Attr "stroke-width" (lit 1.5)
                              , v3AttrStr "class" (str "link")
                              ]
                        )
                  )
            )

    -- Render links first
    linksSelections <- renderTree container linksTree

    -- Reselect chartGroup for nodes
    chartGroupSel <- liftEffect $ reselectD3v2 "chartGroup" linksSelections

    -- Build nodes tree with variable radius and color
    let
      nodesTree :: T.Tree NodeWithRadius
      nodesTree =
        T.named Group "nodesGroup"
          [ v3AttrStr "class" (str "nodes") ]
          `T.withChild`
            ( T.joinData "nodeGroups" "g" nodes $ \node ->
                T.elem Circle
                  [ v3Attr "cx" (lit (node.x + padding))
                  , v3Attr "cy" (lit (node.y + padding))
                  , v3Attr "r" (lit (node.radius * radiusScale)) -- Scale from tree units to pixels
                  , v3AttrStr "fill" (str (computeColor node))
                  , v3AttrStr "stroke" (str "none")
                  , v3Attr "stroke-opacity" (lit 0.0)
                  , v3AttrStr "class" (str "node")
                  ]
            )

    -- Render nodes on top
    _ <- renderTree chartGroupSel nodesTree
    pure unit

-- | Get the layer scale function for a preset
getLayerScaleFn :: LayerScalePreset -> Number -> Int -> Number
getLayerScaleFn preset exponent depth =
  let
    d = toNumber depth
  in
    case preset of
      Linear -> d
      SquareRoot -> sqrt d
      Logarithmic -> log (d + 1.0)
      Exponential -> pow d exponent
      Custom -> d -- Default to linear for custom

-- | Render the component
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page", HH.ClassName "tree-explorer" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadHowTo
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    , HH.h1_ [ HH.text "Tree Layout Explorer" ]

    , HH.p_ [ HH.text "Experiment with tree layout configuration options. Adjust the controls below to see how they affect the Reingold-Tilford tree layout algorithm." ]

    -- Error display
    , case state.error of
        Just err -> HH.div [ HP.classes [ HH.ClassName "error" ] ] [ HH.text err ]
        Nothing -> HH.text ""

    -- Controls panel
    , HH.div
        [ HP.classes [ HH.ClassName "tree-explorer-controls" ] ]
        [ -- Min separation slider
          HH.div
            [ HP.classes [ HH.ClassName "control-group" ] ]
            [ HH.label_ [ HH.text $ "Min Separation: " <> show state.minSeparation ]
            , HH.input
                [ HP.type_ HP.InputRange
                , HP.min 0.1
                , HP.max 5.0
                , HP.step (HP.Step 0.1)
                , HP.value (show state.minSeparation)
                , HE.onValueInput (SetMinSeparation <<< parseNumber 1.0)
                ]
            ]

        -- Layer scale preset buttons
        , HH.div
            [ HP.classes [ HH.ClassName "control-group" ] ]
            [ HH.label_ [ HH.text "Layer Scale:" ]
            , HH.div
                [ HP.classes [ HH.ClassName "button-group" ] ]
                [ presetButton Linear "Linear" state.layerScalePreset
                , presetButton SquareRoot "√ Sqrt" state.layerScalePreset
                , presetButton Logarithmic "Log" state.layerScalePreset
                , presetButton Exponential "d^n" state.layerScalePreset
                ]
            ]

        -- Exponent slider (only for Exponential)
        , if state.layerScalePreset == Exponential then HH.div
            [ HP.classes [ HH.ClassName "control-group" ] ]
            [ HH.label_ [ HH.text $ "Exponent: " <> show state.exponent ]
            , HH.input
                [ HP.type_ HP.InputRange
                , HP.min 0.5
                , HP.max 3.0
                , HP.step (HP.Step 0.1)
                , HP.value (show state.exponent)
                , HE.onValueInput (SetExponent <<< parseNumber 1.5)
                ]
            ]
          else HH.text ""

        -- Link style buttons
        , HH.div
            [ HP.classes [ HH.ClassName "control-group" ] ]
            [ HH.label_ [ HH.text "Link Style:" ]
            , HH.div
                [ HP.classes [ HH.ClassName "button-group" ] ]
                [ linkStyleButton StepVertical "Step V" state.linkStyle
                , linkStyleButton StepHorizontal "Step H" state.linkStyle
                , linkStyleButton BezierVertical "Bezier V" state.linkStyle
                , linkStyleButton BezierHorizontal "Bezier H" state.linkStyle
                , linkStyleButton Diagonal "Diagonal" state.linkStyle
                ]
            ]
        ]

    -- Tree visualization container (D3 will render into this)
    , HH.div
        [ HP.id "tree-explorer-viz"
        , HP.classes [ HH.ClassName "viz-container" ]
        ]
        []
    ]

-- | Render a preset button
presetButton :: forall m. LayerScalePreset -> String -> LayerScalePreset -> H.ComponentHTML Action () m
presetButton preset label current =
  HH.button
    [ HP.classes $ [ HH.ClassName "preset-btn" ] <>
        if preset == current then [ HH.ClassName "active" ] else []
    , HE.onClick \_ -> SetLayerScalePreset preset
    ]
    [ HH.text label ]

-- | Render a link style button
linkStyleButton :: forall m. LinkStyle -> String -> LinkStyle -> H.ComponentHTML Action () m
linkStyleButton style label current =
  HH.button
    [ HP.classes $ [ HH.ClassName "preset-btn" ] <>
        if style == current then [ HH.ClassName "active" ] else []
    , HE.onClick \_ -> SetLinkStyle style
    ]
    [ HH.text label ]

-- | Parse a number from string with default
parseNumber :: Number -> String -> Number
parseNumber default str =
  fromMaybe default $ parseFloat str

foreign import parseFloat :: String -> Maybe Number
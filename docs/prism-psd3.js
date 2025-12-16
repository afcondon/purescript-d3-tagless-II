/**
 * PSD3 DSL syntax highlighting for Prism.js
 *
 * Extends PureScript/Haskell with semantic highlighting for the PSD3 visualization DSL.
 * Designed for clarity - each token category tells you something meaningful about the code.
 */

// Start with Haskell as base
Prism.languages.psd3 = Prism.languages.extend('haskell', {});

// Insert our custom tokens BEFORE the inherited ones so they take priority
Prism.languages.insertBefore('psd3', 'keyword', {
  // Comments
  'comment': [
    { pattern: /\{-[\s\S]*?-\}/, greedy: true },
    { pattern: /--.*$/m, greedy: true }
  ],

  // Tree Structure Functions - "building the DOM shape" - INVERSE STYLED
  'tree-function': {
    pattern: /\bT\.(elem|named|withChild|withChildren|withBehaviors|sceneJoin|sceneNestedJoin|joinData)\b/
  },

  // Element Types - "SVG vocabulary" - INVERSE STYLED
  'element-type': {
    pattern: /\b(SVG|Group|Circle|Rect|Line|Path|Text|Ellipse|Polygon|Polyline|Image|Use|Defs|ClipPath|Mask|Pattern|LinearGradient|RadialGradient|Stop|ForeignObject)\b/
  },

  // SVG Attribute Functions - "what visual property is being set"
  'attr-function': {
    pattern: /\b(x|y|x1|y1|x2|y2|cx|cy|r|rx|ry|width|height|viewBox|fill|stroke|strokeWidth|opacity|fontSize|fontFamily|textAnchor|dominantBaseline|textContent|transform|d|points|href|class|id|attr|style)\b(?=\s*[\$\`]|\s+[a-z_]|\s+\$)/
  },

  // Value Constructors - "how you create type-safe values"
  'value-constructor': {
    pattern: /\b(num|text|str|lit|hex|rgb|rgba|pt|px|em|rem|pct|field|eval)\b/
  },

  // Expression Combinators - "composition/math"
  'expr-combinator': {
    pattern: /\b(timesN|plusN|minusN|divN|negateN|absN|maxN|minN)\b/
  },

  // GUP Lifecycle Keywords - "animation/update lifecycle"
  'gup-keyword': {
    pattern: /\b(enterBehavior|updateBehavior|exitBehavior|initialAttrs|attrs|transition|transitionWith|keyFn|duration|delay|staggerDelay|easing)\b/
  },

  // Easing Functions - "animation curves"
  'easing': {
    pattern: /\b(Linear|CubicIn|CubicOut|CubicInOut|QuadIn|QuadOut|QuadInOut|BounceIn|BounceOut|BounceInOut|ElasticIn|ElasticOut|ElasticInOut|BackIn|BackOut|BackInOut|SineIn|SineOut|SineInOut|ExpoIn|ExpoOut|ExpoInOut|CircIn|CircOut|CircInOut)\b/
  },

  // Behavior Types - "interactive behaviors"
  'behavior': {
    pattern: /\b(Drag|Zoom|Brush|SimulationDrag|ScaleExtent|defaultZoom|defaultDrag)\b/
  },

  // Force Simulation - "physics configuration"
  'force-spec': {
    pattern: /\b(ManyBody|Collide|Center|Link|PositionX|PositionY|RadialForce|ForceSpec|defaultManyBody|defaultCollide|defaultCenter|defaultLink)\b/
  },

  // Selection/Rendering Functions - "D3 operations"
  'selection-function': {
    pattern: /\b(select|selectAll|renderTree|runD3v2M)\b/
  }
});

// Alias for use as ```psd3 in markdown
Prism.languages.purs = Prism.languages.psd3;

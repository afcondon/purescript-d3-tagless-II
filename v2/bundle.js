(() => {
  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i2 = 0; i2 < l; i2++) {
        var f = fs[i2];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x7) {
          return f(g(x7));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x7) {
      return x7;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var on = function(f) {
    return function(g) {
      return function(x7) {
        return function(y6) {
          return f(g(x7))(g(y6));
        };
      };
    };
  };
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ (function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  })();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map111(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return function(f) {
      return function(x7) {
        return map111($$const(x7))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };
  var flap = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return function(ff2) {
      return function(x7) {
        return map111(function(f) {
          return f(x7);
        })(ff2);
      };
    };
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map29 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map29($$const(identity2))(a2))(b2);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map29 = map(dictApply.Functor0());
    return function(f) {
      return function(a2) {
        return function(b2) {
          return apply1(map29(f)(a2))(b2);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure17 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure17(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure17 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure17(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure17 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply3(pure17(f))(a2);
      };
    };
  };

  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i2 = 0, l = arr.length; i2 < l; i2++) {
        Array.prototype.push.apply(result, f(arr[i2]));
      }
      return result;
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped12 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped12(f)(g(a2));
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq6) {
      return function(gt) {
        return function(x7) {
          return function(y6) {
            return x7 < y6 ? lt : x7 === y6 ? eq6 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Data.Eq/index.js
  var eqUnit = {
    eq: function(v) {
      return function(v1) {
        return true;
      };
    }
  };
  var eqString = {
    eq: eqStringImpl
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ (function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  })();
  var GT = /* @__PURE__ */ (function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  })();
  var EQ = /* @__PURE__ */ (function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  })();
  var eqOrdering = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LT && v1 instanceof LT) {
          return true;
        }
        ;
        if (v instanceof GT && v1 instanceof GT) {
          return true;
        }
        ;
        if (v instanceof EQ && v1 instanceof EQ) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.Ring/foreign.js
  var intSub = function(x7) {
    return function(y6) {
      return x7 - y6 | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x7) {
    return function(y6) {
      return x7 + y6 | 0;
    };
  };
  var intMul = function(x7) {
    return function(y6) {
      return x7 * y6 | 0;
    };
  };

  // output/Data.Semiring/index.js
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordUnit = {
    compare: function(v) {
      return function(v1) {
        return EQ.value;
      };
    },
    Eq0: function() {
      return eqUnit;
    }
  };
  var ordString = /* @__PURE__ */ (function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  })();
  var ordNumber = /* @__PURE__ */ (function() {
    return {
      compare: ordNumberImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqNumber;
      }
    };
  })();
  var ordInt = /* @__PURE__ */ (function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  })();
  var ordChar = /* @__PURE__ */ (function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  })();
  var compare = function(dict) {
    return dict.compare;
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i2) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i2 + 1;
        var empty8 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty8;
      }
    ) + '"';
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
        };
      }
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x7) {
    return Math.min(Math.abs(x7), 2147483647);
  };
  var intDiv = function(x7) {
    return function(y6) {
      if (y6 === 0) return 0;
      return y6 > 0 ? Math.floor(x7 / y6) : -Math.floor(x7 / -y6);
    };
  };
  var intMod = function(x7) {
    return function(y6) {
      if (y6 === 0) return 0;
      var yy = Math.abs(y6);
      return (x7 % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupUnit = {
    append: function(v) {
      return function(v1) {
        return unit;
      };
    }
  };
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Data.Monoid/index.js
  var monoidUnit = {
    mempty: unit,
    Semigroup0: function() {
      return semigroupUnit;
    }
  };
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ (function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  })();
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq6 = eq(dictEq);
    return function(dictEq1) {
      var eq15 = eq(dictEq1);
      return {
        eq: function(x7) {
          return function(y6) {
            return eq6(x7.value0)(y6.value0) && eq15(x7.value1)(y6.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare2 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x7) {
          return function(y6) {
            var v = compare2(x7.value0)(y6.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x7.value1)(y6.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_ = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ (function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  })();
  var Just = /* @__PURE__ */ (function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  })();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var eqMaybe = function(dictEq) {
    var eq6 = eq(dictEq);
    return {
      eq: function(x7) {
        return function(y6) {
          if (x7 instanceof Nothing && y6 instanceof Nothing) {
            return true;
          }
          ;
          if (x7 instanceof Just && y6 instanceof Just) {
            return eq6(x7.value0)(y6.value0);
          }
          ;
          return false;
        };
      }
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ (function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  })();

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind10 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind10(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind10 = bind(dictMonad.Bind1());
    var pure17 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind10(f)(function(f$prime) {
          return bind10(a2)(function(a$prime) {
            return pure17(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name16, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
  var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);
  var lift22 = /* @__PURE__ */ lift2(applyEffect);
  var semigroupEffect = function(dictSemigroup) {
    return {
      append: lift22(append(dictSemigroup))
    };
  };
  var monoidEffect = function(dictMonoid) {
    var semigroupEffect1 = semigroupEffect(dictMonoid.Semigroup0());
    return {
      mempty: pureE(mempty(dictMonoid)),
      Semigroup0: function() {
        return semigroupEffect1;
      }
    };
  };

  // output/Effect.Aff/foreign.js
  var Aff = (function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left2, right2, eff) {
      try {
        return right2(eff());
      } catch (error4) {
        return left2(error4);
      }
    }
    function runAsync(left2, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left2(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = (function() {
      var limit = 1024;
      var size4 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size4 !== 0) {
          size4--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size4 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size4) % limit] = cb;
          size4++;
          if (!draining) {
            drain();
          }
        }
      };
    })();
    function Supervisor(util2) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util2.isLeft(result) && util2.fromLeft(result)) {
                    setTimeout(function() {
                      throw util2.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util2, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step4 = aff;
      var fail3 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step4 = bhead(step4);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail3 = util2.left(e);
                step4 = null;
              }
              break;
            case STEP_RESULT:
              if (util2.isLeft(step4)) {
                status = RETURN;
                fail3 = step4;
                step4 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step4 = util2.fromRight(step4);
              }
              break;
            case CONTINUE:
              switch (step4.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step4._2;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step4 = util2.right(step4._1);
                  } else {
                    status = STEP_BIND;
                    step4 = step4._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step4 = runSync(util2.left, util2.right, step4._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step4 = runAsync(util2.left, step4._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step4 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail3 = util2.left(step4._1);
                  step4 = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util2, supervisor, step4._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step4._1) {
                    tmp.run();
                  }
                  step4 = util2.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step4 = sequential3(util2, supervisor, step4._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step4 = interrupt || fail3 || step4;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail3) {
                      status = CONTINUE;
                      step4 = attempt._2(util2.fromLeft(fail3));
                      fail3 = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step4 = util2.fromRight(step4);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail3 === null) {
                      result = util2.fromRight(step4);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step4 = attempt._3(result);
                      }
                    }
                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail3), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step4 = attempt._1.killed(util2.fromLeft(interrupt))(attempt._2);
                    } else if (fail3) {
                      step4 = attempt._1.failed(util2.fromLeft(fail3))(attempt._2);
                    } else {
                      step4 = attempt._1.completed(util2.fromRight(step4))(attempt._2);
                    }
                    fail3 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail3), attempts, interrupt);
                    status = CONTINUE;
                    step4 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step4 = attempt._1;
                    fail3 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step4));
                }
              }
              joins = null;
              if (interrupt && fail3) {
                setTimeout(function() {
                  throw util2.fromLeft(fail3);
                }, 0);
              } else if (util2.isLeft(step4) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util2.fromLeft(step4);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step4)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util2.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util2.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util2.left(error4);
              status = COMPLETED;
              step4 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util2.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
                }
                status = RETURN;
                step4 = null;
                fail3 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util2.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step4 = null;
                fail3 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util2, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root2 = EMPTY;
      function kill2(error4, par2, cb2) {
        var step4 = par2;
        var head6 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
          tmp = null;
          switch (step4.tag) {
            case FORKED:
              if (step4._3 === EMPTY) {
                tmp = fibers[step4._1];
                kills2[count++] = tmp.kill(error4, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head6 === null) {
                break loop;
              }
              step4 = head6._2;
              if (tail2 === null) {
                head6 = null;
              } else {
                head6 = tail2._1;
                tail2 = tail2._2;
              }
              break;
            case MAP:
              step4 = step4._2;
              break;
            case APPLY:
            case ALT:
              if (head6) {
                tail2 = new Aff2(CONS, head6, tail2);
              }
              head6 = step4;
              step4 = step4._1;
              break;
          }
        }
        if (count === 0) {
          cb2(util2.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head6, tail2) {
        var fail3, step4, lhs, rhs, tmp, kid;
        if (util2.isLeft(result)) {
          fail3 = result;
          step4 = null;
        } else {
          step4 = result;
          fail3 = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head6 === null) {
            cb(fail3 || step4)();
            return;
          }
          if (head6._3 !== EMPTY) {
            return;
          }
          switch (head6.tag) {
            case MAP:
              if (fail3 === null) {
                head6._3 = util2.right(head6._1(util2.fromRight(step4)));
                step4 = head6._3;
              } else {
                head6._3 = fail3;
              }
              break;
            case APPLY:
              lhs = head6._1._3;
              rhs = head6._2._3;
              if (fail3) {
                head6._3 = fail3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, fail3 === lhs ? head6._2 : head6._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(fail3, null, null);
                    } else {
                      join3(fail3, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step4 = util2.right(util2.fromRight(lhs)(util2.fromRight(rhs)));
                head6._3 = step4;
              }
              break;
            case ALT:
              lhs = head6._1._3;
              rhs = head6._2._3;
              if (lhs === EMPTY && util2.isLeft(rhs) || rhs === EMPTY && util2.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util2.isLeft(lhs) && rhs !== EMPTY && util2.isLeft(rhs)) {
                fail3 = step4 === lhs ? rhs : lhs;
                step4 = null;
                head6._3 = fail3;
              } else {
                head6._3 = step4;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, step4 === lhs ? head6._2 : head6._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(step4, null, null);
                    } else {
                      join3(step4, tail2._1, tail2._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail2 === null) {
            head6 = null;
          } else {
            head6 = tail2._1;
            tail2 = tail2._2;
          }
        }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step4 = par;
        var head6 = null;
        var tail2 = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step4.tag) {
                case MAP:
                  if (head6) {
                    tail2 = new Aff2(CONS, head6, tail2);
                  }
                  head6 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                  step4 = step4._2;
                  break;
                case APPLY:
                  if (head6) {
                    tail2 = new Aff2(CONS, head6, tail2);
                  }
                  head6 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                  step4 = step4._1;
                  break;
                case ALT:
                  if (head6) {
                    tail2 = new Aff2(CONS, head6, tail2);
                  }
                  head6 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                  step4 = step4._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step4;
                  step4 = new Aff2(FORKED, fid, new Aff2(CONS, head6, tail2), EMPTY);
                  tmp = Fiber(util2, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step4)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head6 === null) {
                break loop;
              }
              if (head6._1 === EMPTY) {
                head6._1 = step4;
                status = CONTINUE;
                step4 = head6._2;
                head6._2 = EMPTY;
              } else {
                head6._2 = step4;
                step4 = head6;
                if (tail2 === null) {
                  head6 = null;
                } else {
                  head6 = tail2._1;
                  tail2 = tail2._2;
                }
              }
          }
        }
        root2 = step4;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util2.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root2, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential3(util2, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util2, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  })();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value13) {
          return Aff.Pure(f(value13));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util2, aff) {
    return function() {
      return Aff.Fiber(util2, null, aff);
    };
  }
  var _delay = /* @__PURE__ */ (function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right2, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer2 = setDelay(ms, cb(right2()));
          return function() {
            return Aff.Sync(function() {
              return right2(clearDelay(ms, timer2));
            });
          };
        };
      });
    };
  })();
  var _sequential = Aff.Seq;

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ (function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  })();
  var Right = /* @__PURE__ */ (function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  })();
  var note = function(a2) {
    return maybe(new Left(a2))(Right.create);
  };
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map29 = map(Monad0.Bind1().Apply0().Functor0());
    var pure17 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map29(Right.create)(a2))(function($52) {
        return pure17(Left.create($52));
      });
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x7) {
    return x7;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref2) {
      return function() {
        ref2.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_2 = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map3 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ (function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  })();
  var Done = /* @__PURE__ */ (function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  })();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): " + [v.constructor.name]);
        };
        return function __do3() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!(function __do4() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): " + [v.constructor.name]);
            })()) {
            }
            ;
            return {};
          })();
          return map3(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map4 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x7) {
    return x7;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map111(map4(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind10 = bind(dictMonad.Bind1());
    var pure17 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind10(v)(either(function($187) {
            return pure17(Left.create($187));
          })(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: (function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      })(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: (function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      })(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append9 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind10 = bind(Bind1);
      var pure17 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind10(v)(function(rm) {
              if (rm instanceof Right) {
                return pure17(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind10(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure17(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure17(new Left(append9(rm.value0)(rn.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [rn.constructor.name]);
                });
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [rm.constructor.name]);
            });
          };
        },
        Functor0: function() {
          return functorExceptT1;
        }
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x7) {
    return x7;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var rmap = function(dictBifunctor) {
    return bimap(dictBifunctor)(identity4);
  };
  var bifunctorEither = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return new Left(v(v2.value0));
          }
          ;
          if (v2 instanceof Right) {
            return new Right(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x7) {
    return x7;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Data.Foldable/index.js
  var eq12 = /* @__PURE__ */ eq(eqOrdering);
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure17 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure17(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_14 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_14(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate2 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append9 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(sep) {
        return function(xs) {
          var go2 = function(v) {
            return function(v1) {
              if (v.init) {
                return {
                  init: false,
                  acc: v1
                };
              }
              ;
              return {
                init: false,
                acc: append9(v.acc)(append9(sep)(v1))
              };
            };
          };
          return foldl22(go2)({
            init: true,
            acc: mempty3
          })(xs).acc;
        };
      };
    };
  };
  var maximumBy = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(cmp) {
      var max$prime = function(v) {
        return function(v1) {
          if (v instanceof Nothing) {
            return new Just(v1);
          }
          ;
          if (v instanceof Just) {
            return new Just((function() {
              var $303 = eq12(cmp(v.value0)(v1))(GT.value);
              if ($303) {
                return v.value0;
              }
              ;
              return v1;
            })());
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 441, column 3 - line 441, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return foldl22(max$prime)(Nothing.value);
    };
  };
  var maximum = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(dictFoldable) {
      return maximumBy(dictFoldable)(compare2);
    };
  };
  var minimumBy = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(cmp) {
      var min$prime = function(v) {
        return function(v1) {
          if (v instanceof Nothing) {
            return new Just(v1);
          }
          ;
          if (v instanceof Just) {
            return new Just((function() {
              var $307 = eq12(cmp(v.value0)(v1))(LT.value);
              if ($307) {
                return v.value0;
              }
              ;
              return v1;
            })());
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 454, column 3 - line 454, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return foldl22(min$prime)(Nothing.value);
    };
  };
  var minimum = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(dictFoldable) {
      return minimumBy(dictFoldable)(compare2);
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty3;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append9 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x7) {
          return function(acc) {
            return append9(f(x7))(acc);
          };
        })(mempty3);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var any = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap2(monoidDisj(dictHeytingAlgebra)));
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = /* @__PURE__ */ (function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map29) {
        return function(pure17) {
          return function(f) {
            return function(array4) {
              function go2(bot, top4) {
                switch (top4 - bot) {
                  case 0:
                    return pure17([]);
                  case 1:
                    return map29(array1)(f(array4[bot]));
                  case 2:
                    return apply3(map29(array2)(f(array4[bot])))(f(array4[bot + 1]));
                  case 3:
                    return apply3(apply3(map29(array3)(f(array4[bot])))(f(array4[bot + 1])))(f(array4[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top4 - bot) / 4) * 2;
                    return apply3(map29(concat2)(go2(bot, pivot)))(go2(pivot, top4));
                }
              }
              return go2(0, array4.length);
            };
          };
        };
      };
    };
  })();

  // output/Data.Traversable/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity5);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Control.Parallel/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var traverse_9 = traverse_(dictParallel.Applicative1());
    var parallel3 = parallel(dictParallel);
    return function(dictFoldable) {
      var traverse_14 = traverse_9(dictFoldable);
      return function(f) {
        var $48 = traverse_14(function($50) {
          return parallel3(f($50));
        });
        return function($49) {
          return sequential3($48($49));
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictFoldable) {
      return parTraverse_1(dictFoldable)(identity6);
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy2 = function(name16, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map5 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x7) {
    return x7;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map1 = /* @__PURE__ */ map(functorAff);
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ (function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  })();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do3() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var delay = function(v) {
    return _delay(Right.create, v);
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure22(unit))($$const(fin))($$const(a2));
    };
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($74) {
    return Canceler($$const(liftEffect2($74)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map5(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map5(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped3(function($77) {
        return liftEffect2(k($77));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void3(runAff(k)(aff));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Monad0: function() {
      return monadAff;
    },
    Applicative1: function() {
      return $lazy_applicativeParAff(0);
    }
  };
  var $lazy_applicativeParAff = /* @__PURE__ */ $runtime_lazy2("applicativeParAff", "Effect.Aff", function() {
    return {
      pure: (function() {
        var $79 = parallel(parallelAff);
        return function($80) {
          return $79(pure22($80));
        };
      })(),
      Apply0: function() {
        return applyParAff;
      }
    };
  });
  var applicativeParAff = /* @__PURE__ */ $lazy_applicativeParAff(131);
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure22(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 102, column 7 - line 104, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));

  // output/Control.Monad.State.Trans/index.js
  var runStateT = function(v) {
    return v;
  };
  var monadTransStateT = {
    lift: function(dictMonad) {
      var bind10 = bind(dictMonad.Bind1());
      var pure17 = pure(dictMonad.Applicative0());
      return function(m) {
        return function(s) {
          return bind10(m)(function(x7) {
            return pure17(new Tuple(x7, s));
          });
        };
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransStateT);
  var functorStateT = function(dictFunctor) {
    var map29 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map29(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind10 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind10(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure17 = pure(dictMonad.Applicative0());
    return {
      pure: function(a2) {
        return function(s) {
          return pure17(new Tuple(a2, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadEffectState = function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var monadStateT1 = monadStateT(Monad0);
    return {
      liftEffect: (function() {
        var $197 = lift3(Monad0);
        var $198 = liftEffect(dictMonadEffect);
        return function($199) {
          return $197($198($199));
        };
      })(),
      Monad0: function() {
        return monadStateT1;
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure17 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure17(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };
  var liftAff = function(dict) {
    return dict.liftAff;
  };

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x7) {
    return x7;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map6 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map6(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type2) {
    return function(listener) {
      return function(useCapture) {
        return function(target7) {
          return function() {
            return target7.addEventListener(type2, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type2) {
    return function(listener) {
      return function(useCapture) {
        return function(target7) {
          return function() {
            return target7.removeEventListener(type2, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return function() {
      return doc.readyState;
    };
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ (function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  })();
  var Interactive = /* @__PURE__ */ (function() {
    function Interactive3() {
    }
    ;
    Interactive3.value = new Interactive3();
    return Interactive3;
  })();
  var Complete = /* @__PURE__ */ (function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  })();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var map7 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = /* @__PURE__ */ (function() {
    var $2 = map7((function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    })());
    return function($3) {
      return $2(_readyState($3));
    };
  })();

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value13) {
    var tag = Object.prototype.toString.call(value13);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value13);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var fromElement = function(x7) {
    return _read(Nothing.value, Just.create, x7);
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value13 = b2;
              while (true) {
                var maybe2 = f(value13);
                if (isNothing2(maybe2)) return result;
                var tuple = fromJust5(maybe2);
                result.push(fst2(tuple));
                value13 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value13 = b2;
              while (true) {
                var tuple = f(value13);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2)) return result;
                value13 = fromJust5(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Enum/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom1 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x7) {
          var v = toEnum1(x7);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x7 < fromEnum1(bottom1);
            if ($140) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= bottom2 && v <= top2) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ (function() {
    return {
      cardinality: toCharCode(top(boundedChar)) - toCharCode(bottom(boundedChar)) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  })();

  // output/Web.HTML.Location/foreign.js
  function hash(location2) {
    return function() {
      return location2.hash;
    };
  }
  function setHash(hash2) {
    return function(location2) {
      return function() {
        location2.hash = hash2;
      };
    };
  }

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }
  function location(window2) {
    return function() {
      return window2.location;
    };
  }
  function innerWidth(window2) {
    return function() {
      return window2.innerWidth;
    };
  }
  function innerHeight(window2) {
    return function() {
      return window2.innerHeight;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded = "DOMContentLoaded";

  // output/Halogen.Aff.Util/index.js
  var bind2 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure3 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map8 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2((function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    })())(document2))(windowImpl)))(function(mel) {
      return pure3(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do3() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document2)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map8(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
    return bind2(selectElement("body"))(function(body2) {
      return maybe(throwError2(error("Could not find body")))(pure3)(body2);
    });
  });

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ (function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  })();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ (function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  })();
  var singleton2 = function(dictPlus) {
    var empty8 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty8);
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ (function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  })();
  var Cons = /* @__PURE__ */ (function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  })();
  var NonEmptyList = function(x7) {
    return x7;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = (function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        })();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty3);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
      };
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ (function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  })();

  // output/Data.List/index.js
  var reverse = /* @__PURE__ */ (function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  })();
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ (function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  })();
  var Two = /* @__PURE__ */ (function() {
    function Two2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Two2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Two2;
  })();
  var Three = /* @__PURE__ */ (function() {
    function Three2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Three2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  })();
  var TwoLeft = /* @__PURE__ */ (function() {
    function TwoLeft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoLeft2(value0, value1, value22);
        };
      };
    };
    return TwoLeft2;
  })();
  var TwoRight = /* @__PURE__ */ (function() {
    function TwoRight2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoRight2(value0, value1, value22);
        };
      };
    };
    return TwoRight2;
  })();
  var ThreeLeft = /* @__PURE__ */ (function() {
    function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  })();
  var ThreeMiddle = /* @__PURE__ */ (function() {
    function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  })();
  var ThreeRight = /* @__PURE__ */ (function() {
    function ThreeRight2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeRight2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  })();
  var KickUp = /* @__PURE__ */ (function() {
    function KickUp2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new KickUp2(value0, value1, value22, value32);
          };
        };
      };
    };
    return KickUp2;
  })();
  var lookup = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = compare2(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = compare2(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare2(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_v1) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return v1;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare2 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var down = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v2 instanceof Leaf) {
                $tco_done1 = true;
                return up(v1)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v2 instanceof Two) {
                var v3 = compare2(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Two(v2.value0, k, v, v2.value3));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new TwoLeft(v2.value1, v2.value2, v2.value3), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new TwoRight(v2.value0, v2.value1, v2.value2), v1);
                $copy_v2 = v2.value3;
                return;
              }
              ;
              if (v2 instanceof Three) {
                var v3 = compare2(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, k, v, v2.value3, v2.value4, v2.value5, v2.value6));
                }
                ;
                var v4 = compare2(k)(v2.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, k, v, v2.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeLeft(v2.value1, v2.value2, v2.value3, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeMiddle(v2.value0, v2.value1, v2.value2, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value3;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new ThreeRight(v2.value0, v2.value1, v2.value2, v2.value3, v2.value4, v2.value5), v1);
                $copy_v2 = v2.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var pop = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare2 = compare(dictOrd);
    return function(k) {
      var up = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree2) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree2;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree2 instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree2 instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree2, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree2);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(tree2, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree2)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree2 instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree2 instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree2 instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree2, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree2), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree2, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree2)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree2, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree2), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree2, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree2)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var down = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = compare2(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max7 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max7.key, max7.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = (function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              })();
              var v = compare2(k)(m.value4);
              var v3 = compare2(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max7 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max7.key, max7.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max7 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max7.key, max7.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty3;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(append22(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append22(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var empty2 = /* @__PURE__ */ (function() {
    return Leaf.value;
  })();
  var fromFoldable = function(dictOrd) {
    var insert13 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert13(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var $$delete = function(dictOrd) {
    var pop12 = pop(dictOrd);
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop12(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    var lookup13 = lookup(dictOrd);
    var delete1 = $$delete(dictOrd);
    var insert13 = insert(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup13(k)(m));
          if (v instanceof Nothing) {
            return delete1(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert13(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };

  // output/Halogen.Data.OrdBox/index.js
  var OrdBox = /* @__PURE__ */ (function() {
    function OrdBox2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    OrdBox2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new OrdBox2(value0, value1, value22);
        };
      };
    };
    return OrdBox2;
  })();
  var mkOrdBox = function(dictOrd) {
    return OrdBox.create(eq(dictOrd.Eq0()))(compare(dictOrd));
  };
  var eqOrdBox = {
    eq: function(v) {
      return function(v1) {
        return v.value0(v.value2)(v1.value2);
      };
    }
  };
  var ordOrdBox = {
    compare: function(v) {
      return function(v1) {
        return v.value1(v.value2)(v1.value2);
      };
    },
    Eq0: function() {
      return eqOrdBox;
    }
  };

  // output/Halogen.Data.Slot/index.js
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordString)(ordOrdBox);
  var pop1 = /* @__PURE__ */ pop(ordTuple2);
  var lookup1 = /* @__PURE__ */ lookup(ordTuple2);
  var insert1 = /* @__PURE__ */ insert(ordTuple2);
  var pop2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return pop1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var lookup2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return lookup1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var insert2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(val) {
              return function(v) {
                return insert1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(val)(v);
              };
            };
          };
        };
      };
    };
  };
  var foreachSlot = function(dictApplicative) {
    var traverse_9 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_9(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty3 = empty2;

  // output/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ (function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  })();
  var Action = /* @__PURE__ */ (function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  })();

  // output/Data.Array/foreign.js
  var range2 = function(start3) {
    return function(end) {
      var step4 = start3 > end ? -1 : 1;
      var result = new Array(step4 * (end - start3) + 1);
      var i2 = start3, n = 0;
      while (i2 !== end) {
        result[n++] = i2;
        i2 += step4;
      }
      result[n] = i2;
      return result;
    };
  };
  var replicateFill = function(count) {
    return function(value13) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value13);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value13) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value13;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var length4 = function(xs) {
    return xs.length;
  };
  var indexImpl = function(just) {
    return function(nothing) {
      return function(xs) {
        return function(i2) {
          return i2 < 0 || i2 >= xs.length ? nothing : just(xs[i2]);
        };
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i2 = 0, l = xs.length; i2 < l; i2++) {
            if (f(xs[i2])) return just(i2);
          }
          return nothing;
        };
      };
    };
  };
  var _deleteAt = function(just) {
    return function(nothing) {
      return function(i2) {
        return function(l) {
          if (i2 < 0 || i2 >= l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i2, 1);
          return just(l1);
        };
      };
    };
  };
  var filter2 = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Data.Array.ST/foreign.js
  var pushAll = function(as) {
    return function(xs) {
      return function() {
        return xs.push.apply(xs, as);
      };
    };
  };
  var unsafeFreeze = function(xs) {
    return function() {
      return xs;
    };
  };
  function copyImpl(xs) {
    return function() {
      return xs.slice();
    };
  }
  var thaw = copyImpl;

  // output/Data.Array.ST/index.js
  var withArray = function(f) {
    return function(xs) {
      return function __do3() {
        var result = thaw(xs)();
        f(result)();
        return unsafeFreeze(result)();
      };
    };
  };
  var push = function(a2) {
    return pushAll([a2]);
  };

  // output/Data.Array/index.js
  var intercalate1 = /* @__PURE__ */ intercalate2(foldableArray);
  var map12 = /* @__PURE__ */ map(functorMaybe);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var snoc = function(xs) {
    return function(x7) {
      return withArray(push(x7))(xs)();
    };
  };
  var singleton4 = function(a2) {
    return [a2];
  };
  var intercalate3 = function(dictMonoid) {
    return intercalate1(dictMonoid);
  };
  var index2 = /* @__PURE__ */ (function() {
    return indexImpl(Just.create)(Nothing.value);
  })();
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var findIndex = /* @__PURE__ */ (function() {
    return findIndexImpl(Just.create)(Nothing.value);
  })();
  var find2 = function(f) {
    return function(xs) {
      return map12(unsafeIndex1(xs))(findIndex(f)(xs));
    };
  };
  var deleteAt = /* @__PURE__ */ (function() {
    return _deleteAt(Just.create)(Nothing.value);
  })();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust4(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var cons3 = function(x7) {
    return function(xs) {
      return append2([x7])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap((function() {
      var $187 = maybe([])(singleton4);
      return function($188) {
        return $187(f($188));
      };
    })());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ (function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  })();
  var unStep = unsafeCoerce2;
  var step3 = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var map9 = /* @__PURE__ */ map(functorArray);
  var map13 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ (function() {
    function Text3(value0) {
      this.value0 = value0;
    }
    ;
    Text3.create = function(value0) {
      return new Text3(value0);
    };
    return Text3;
  })();
  var Elem = /* @__PURE__ */ (function() {
    function Elem2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem2;
  })();
  var Keyed = /* @__PURE__ */ (function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  })();
  var Widget = /* @__PURE__ */ (function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  })();
  var Grafted = /* @__PURE__ */ (function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  })();
  var Graft = /* @__PURE__ */ (function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  })();
  var unGraft = function(f) {
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map9(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map9(map13(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name16, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name16);
    } else {
      return doc.createElement(name16);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener2(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener2(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name16) {
    return function(doctype) {
      return doctype[name16];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy3 = function(name16, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy3("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step3(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy3("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $63 = v === v1;
    if ($63) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy3("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length4(vdom.value3);
        var v1 = length4(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step3(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step3(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children22 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children22
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy3("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length4(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step3(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step3(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children22 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children22,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children3 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children3,
      length: length4(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(ix, child) {
      var res = build(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children3 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children3
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy3("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign/foreign.js
  function typeOf(value13) {
    return typeof value13;
  }
  function tagOf(value13) {
    return Object.prototype.toString.call(value13).slice(8, -1);
  }
  var isArray = Array.isArray || function(value13) {
    return Object.prototype.toString.call(value13) === "[object Array]";
  };

  // output/Data.Int/foreign.js
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var abs = Math.abs;

  // output/Data.Number/index.js
  var pi = 3.141592653589793;

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ (function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  })();
  var head2 = function(v) {
    return v.value0;
  };
  var cons4 = function(y6) {
    return function(v) {
      return new NonEmpty(y6, new Cons(v.value0, v.value1));
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton6 = function(c) {
    return c;
  };
  var length5 = function(s) {
    return s.length;
  };
  var _indexOf = function(just) {
    return function(nothing) {
      return function(x7) {
        return function(s) {
          var i2 = s.indexOf(x7);
          return i2 === -1 ? nothing : just(i2);
        };
      };
    };
  };
  var take2 = function(n) {
    return function(s) {
      return s.substr(0, n);
    };
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length) return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var indexOf = /* @__PURE__ */ (function() {
    return _indexOf(Just.create)(Nothing.value);
  })();

  // output/Foreign/index.js
  var show2 = /* @__PURE__ */ show(showString);
  var show1 = /* @__PURE__ */ show(showInt);
  var ForeignError = /* @__PURE__ */ (function() {
    function ForeignError2(value0) {
      this.value0 = value0;
    }
    ;
    ForeignError2.create = function(value0) {
      return new ForeignError2(value0);
    };
    return ForeignError2;
  })();
  var TypeMismatch = /* @__PURE__ */ (function() {
    function TypeMismatch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch2.create = function(value0) {
      return function(value1) {
        return new TypeMismatch2(value0, value1);
      };
    };
    return TypeMismatch2;
  })();
  var ErrorAtIndex = /* @__PURE__ */ (function() {
    function ErrorAtIndex2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtIndex2.create = function(value0) {
      return function(value1) {
        return new ErrorAtIndex2(value0, value1);
      };
    };
    return ErrorAtIndex2;
  })();
  var ErrorAtProperty = /* @__PURE__ */ (function() {
    function ErrorAtProperty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtProperty2.create = function(value0) {
      return function(value1) {
        return new ErrorAtProperty2(value0, value1);
      };
    };
    return ErrorAtProperty2;
  })();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var renderForeignError = function(v) {
    if (v instanceof ForeignError) {
      return v.value0;
    }
    ;
    if (v instanceof ErrorAtIndex) {
      return "Error at array index " + (show1(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof ErrorAtProperty) {
      return "Error at property " + (show2(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof TypeMismatch) {
      return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    }
    ;
    throw new Error("Failed pattern match at Foreign (line 78, column 1 - line 78, column 45): " + [v.constructor.name]);
  };
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton5($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure17 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value13) {
        if (tagOf(value13) === tag) {
          return pure17(unsafeFromForeign(value13));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value13)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value13.constructor.name]);
      };
    };
  };

  // output/Foreign.Object/foreign.js
  var empty4 = {};
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Data.Function.Uncurried/foreign.js
  var mkFn2 = function(fn) {
    return function(a2, b2) {
      return fn(a2)(b2);
    };
  };
  var runFn3 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return fn(a2, b2, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d6) {
            return fn(a2, b2, c, d6);
          };
        };
      };
    };
  };

  // output/Foreign.Object/index.js
  var lookup3 = /* @__PURE__ */ (function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  })();

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy4 = function(name16, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ (function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  })();
  var Removed = /* @__PURE__ */ (function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  })();
  var Attribute = /* @__PURE__ */ (function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  })();
  var Property = /* @__PURE__ */ (function() {
    function Property2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property2.create = function(value0) {
      return function(value1) {
        return new Property2(value0, value1);
      };
    };
    return Property2;
  })();
  var Handler = /* @__PURE__ */ (function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  })();
  var Ref = /* @__PURE__ */ (function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  })();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key, el) {
    var v = hasAttribute(nullImpl, key, el);
    if (v) {
      return removeAttribute(nullImpl, key, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key, el));
    if (v1 === "string") {
      return unsafeSetAny(key, "", el);
    }
    ;
    if (key === "rowSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    if (key === "colSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    return unsafeSetAny(key, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener2(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup3("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $66 = v11.value2 === v2.value2;
            if ($66) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref2 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do3() {
                var f$prime = read(ref2)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
            addEventListener2(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy4("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x7) {
    return x7;
  };
  var widget = function($28) {
    return HTML(Widget.create($28));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text5 = function($29) {
    return HTML(Text.create($29));
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var handler = /* @__PURE__ */ (function() {
    return Handler.create;
  })();
  var element = function(ns) {
    return function(name16) {
      return function(props) {
        return function(children3) {
          return new Elem(ns, name16, props, children3);
        };
      };
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ (function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  })();
  var Lift = /* @__PURE__ */ (function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  })();
  var Ap = /* @__PURE__ */ (function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  })();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ (function() {
    return Lift.create;
  })();
  var goLeft = function(dictApplicative) {
    var pure17 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure17(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons4(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply3(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
              return new Right(new Tuple(new Cons({
                func: gRes,
                count: fStack.value0.count - 1 | 0
              }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x7) {
        return mkAp(new Pure(f))(x7);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure17 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure17(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity7);
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ (function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  })();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ (function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  })();
  var uncons2 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc3 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null2 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty5 = /* @__PURE__ */ (function() {
    return new CatQueue(Nil.value, Nil.value);
  })();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ (function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  })();
  var CatCons = /* @__PURE__ */ (function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  })();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc3(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr3 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl4 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons2(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl4(function(x7) {
                  return function(i2) {
                    return i2(x7);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons3 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, (function() {
        var $66 = $$null2(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr3(link)(CatNil.value)(v.value1);
      })()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty6 = /* @__PURE__ */ (function() {
    return CatNil.value;
  })();
  var append3 = link;
  var semigroupCatList = {
    append: append3
  };
  var snoc4 = function(cat) {
    return function(a2) {
      return append3(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy5 = function(name16, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var append4 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ (function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  })();
  var Return = /* @__PURE__ */ (function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  })();
  var Bind = /* @__PURE__ */ (function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  })();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append4(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons3(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty6);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)((function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        })())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc4(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($191) {
      return fromView(Return.create($191));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy5("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure4 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure4($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map111 = map(Monad0.Bind1().Apply0().Functor0());
    var pure17 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map111(Done.create)(pure17(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map111(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM4(go2);
    };
  };

  // output/Halogen.Query.ChildQuery/index.js
  var unChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var coerce3 = /* @__PURE__ */ coerce();
  var bind3 = /* @__PURE__ */ bind(bindEffect);
  var append5 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void4(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var makeEmitter = coerce3;
  var create3 = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do3() {
          modify_2(function(v) {
            return append5(v)([k]);
          })(subscribers)();
          return modify_2(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind3(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var SubscriptionId = function(x7) {
    return x7;
  };
  var ForkId = function(x7) {
    return x7;
  };
  var State = /* @__PURE__ */ (function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  })();
  var Subscribe = /* @__PURE__ */ (function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  })();
  var Unsubscribe = /* @__PURE__ */ (function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  })();
  var Lift2 = /* @__PURE__ */ (function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  })();
  var ChildQuery2 = /* @__PURE__ */ (function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  })();
  var Raise = /* @__PURE__ */ (function() {
    function Raise3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise3.create = function(value0) {
      return function(value1) {
        return new Raise3(value0, value1);
      };
    };
    return Raise3;
  })();
  var Par = /* @__PURE__ */ (function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  })();
  var Fork = /* @__PURE__ */ (function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  })();
  var Join = /* @__PURE__ */ (function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  })();
  var Kill = /* @__PURE__ */ (function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  })();
  var GetRef = /* @__PURE__ */ (function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  })();
  var HalogenM = function(x7) {
    return x7;
  };
  var subscribe2 = function(es) {
    return liftF(new Subscribe(function(v) {
      return es;
    }, identity8));
  };
  var raise = function(o) {
    return liftF(new Raise(o, unit));
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: (function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift2.create($186($187))));
        };
      })(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var monadAffHalogenM = function(dictMonadAff) {
    var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    return {
      liftAff: (function() {
        var $188 = liftAff(dictMonadAff);
        return function($189) {
          return HalogenM(liftF(Lift2.create($188($189))));
        };
      })(),
      MonadEffect0: function() {
        return monadEffectHalogenM1;
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ (function() {
    function Initialize6(value0) {
      this.value0 = value0;
    }
    ;
    Initialize6.create = function(value0) {
      return new Initialize6(value0);
    };
    return Initialize6;
  })();
  var Finalize = /* @__PURE__ */ (function() {
    function Finalize2(value0) {
      this.value0 = value0;
    }
    ;
    Finalize2.create = function(value0) {
      return new Finalize2(value0);
    };
    return Finalize2;
  })();
  var Receive = /* @__PURE__ */ (function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  })();
  var Action2 = /* @__PURE__ */ (function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  })();
  var Query = /* @__PURE__ */ (function() {
    function Query2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query2.create = function(value0) {
      return function(value1) {
        return new Query2(value0, value1);
      };
    };
    return Query2;
  })();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy6 = function(name16, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy6("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step3(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map10 = /* @__PURE__ */ map(functorHalogenM);
  var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
  var lookup4 = /* @__PURE__ */ lookup2();
  var pop3 = /* @__PURE__ */ pop2();
  var insert3 = /* @__PURE__ */ insert2();
  var ComponentSlot = /* @__PURE__ */ (function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  })();
  var ThunkSlot = /* @__PURE__ */ (function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  })();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map10(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponentSlot = unsafeCoerce2;
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ (function() {
    return {
      handleAction: $$const(pure5(unit)),
      handleQuery: $$const(pure5(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  })();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup4(dictIsSymbol);
      var pop12 = pop3(dictIsSymbol);
      var insert13 = insert3(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        var pop22 = pop12(dictOrd);
        var insert22 = insert13(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(comp) {
              return function(input3) {
                return function(output2) {
                  return mkComponentSlot({
                    get: lookup23(label5)(p2),
                    pop: pop22(label5)(p2),
                    set: insert22(label5)(p2),
                    component: comp,
                    input: input3,
                    output: output2
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ (function() {
    return element(Nothing.value);
  })();
  var h1 = /* @__PURE__ */ element2("h1");
  var h1_ = /* @__PURE__ */ h1([]);
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var h3 = /* @__PURE__ */ element2("h3");
  var h3_ = /* @__PURE__ */ h3([]);
  var img = function(props) {
    return element2("img")(props)([]);
  };
  var label4 = /* @__PURE__ */ element2("label");
  var li = /* @__PURE__ */ element2("li");
  var li_ = /* @__PURE__ */ li([]);
  var main = /* @__PURE__ */ element2("main");
  var nav = /* @__PURE__ */ element2("nav");
  var p = /* @__PURE__ */ element2("p");
  var p_ = /* @__PURE__ */ p([]);
  var pre = /* @__PURE__ */ element2("pre");
  var section = /* @__PURE__ */ element2("section");
  var span3 = /* @__PURE__ */ element2("span");
  var ul = /* @__PURE__ */ element2("ul");
  var ul_ = /* @__PURE__ */ ul([]);
  var div2 = /* @__PURE__ */ element2("div");
  var code = /* @__PURE__ */ element2("code");
  var button = /* @__PURE__ */ element2("button");
  var a = /* @__PURE__ */ element2("a");

  // output/Halogen.HTML.Properties/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var rel4 = /* @__PURE__ */ prop22("rel");
  var src9 = /* @__PURE__ */ prop22("src");
  var target5 = /* @__PURE__ */ prop22("target");
  var title3 = /* @__PURE__ */ prop22("title");
  var id2 = /* @__PURE__ */ prop22("id");
  var href4 = /* @__PURE__ */ prop22("href");
  var classes = /* @__PURE__ */ (function() {
    var $32 = prop22("className");
    var $33 = joinWith(" ");
    var $34 = map(functorArray)(unwrap2);
    return function($35) {
      return $32($33($34($35)));
    };
  })();
  var alt5 = /* @__PURE__ */ prop22("alt");

  // output/Halogen.HTML/index.js
  var componentSlot2 = /* @__PURE__ */ componentSlot();
  var slot_ = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component10) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot22(label5)(p2)(component10)(input3)($$const(Nothing.value))));
              };
            };
          };
        };
      };
    };
  };
  var slot = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component10) {
              return function(input3) {
                return function(outputQuery) {
                  return widget(new ComponentSlot(componentSlot22(label5)(p2)(component10)(input3)(function($11) {
                    return Just.create(outputQuery($11));
                  })));
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork = function(dict) {
    return dict.fork;
  };

  // output/Effect.Console/foreign.js
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_9 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_9(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component10) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do3() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty3)();
            var childrenOut = $$new(empty3)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty2))();
            var forks = $$new(empty2)();
            var ds = {
              component: component10,
              state: component10.initialState(input3),
              refs: empty2,
              children: empty3,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup5 = /* @__PURE__ */ lookup(ordSubscriptionId);
  var bind12 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard3(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
  var pure6 = /* @__PURE__ */ pure(applicativeAff);
  var map14 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var map15 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map22 = /* @__PURE__ */ map(functorMaybe);
  var insert4 = /* @__PURE__ */ insert(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert12 = /* @__PURE__ */ insert(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup12 = /* @__PURE__ */ lookup(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do3() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped5(lookup5(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard1(liftEffect4(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind12(liftEffect4(f))(function(result) {
          return bind12(liftEffect4(read(lchs)))(function(v) {
            return discard1(traverse_22(fork3)(v.finalizers))(function() {
              return discard1(parSequence_2(v.initializers))(function() {
                return pure6(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref2) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        return liftEffect4(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render10) {
    return function(ref2) {
      return function(q2) {
        return bind12(liftEffect4(read(ref2)))(function(v) {
          return evalM(render10)(ref2)(v["component"]["eval"](new Query(map14(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render10) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind12(liftEffect4(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel2(bind12(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render10)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map15(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure6(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect4(write({
                    component: v2.component,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers,
                    state: v3.value1
                  })(ref2)))(function() {
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render10(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure6(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind12(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind12(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render10)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return discard1(liftEffect4(modify_2(map22(insert4(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure6(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure6(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift2) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref2)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                  return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp((function() {
                var $118 = evalM(render10)(ref2);
                return function($119) {
                  return parallel2($118($119));
                };
              })())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind12(fresh(ForkId)(ref2))(function(fid) {
                return bind12(liftEffect4(read(ref2)))(function(v2) {
                  return bind12(liftEffect4($$new(false)))(function(doneRef) {
                    return bind12(fork3($$finally(liftEffect4(function __do3() {
                      modify_2($$delete2(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render10)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_2(insert12(fid)(fiber))(v2.forks))))(function() {
                        return pure6(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(joinFiber)(lookup12(v1.value0)(forkMap)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                    return pure6(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return pure6(v1.value1(lookup22(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render10) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect4(flip(modify_2)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers,
              refs: alter2($$const(v.value1))(v.value0)(st.refs)
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind12(liftEffect4(read(ref2)))(function(v1) {
            return evalM(render10)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind4 = /* @__PURE__ */ bind(bindEffect);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork(monadForkAff);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard22 = /* @__PURE__ */ discard4(bindAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure7 = /* @__PURE__ */ pure(applicativeEffect);
  var map16 = /* @__PURE__ */ map(functorEffect);
  var pure12 = /* @__PURE__ */ pure(applicativeAff);
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void5 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ (function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  })();
  var handlePending = function(ref2) {
    return function __do3() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_2(queue)((function() {
        var $58 = traverse_5(fork4);
        return function($59) {
          return handleAff($58(reverse($59)));
        };
      })())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do3() {
      bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped6(traverse_33((function() {
        var $60 = killFiber(error("finalized"));
        return function($61) {
          return handleAff($60($61));
        };
      })()))(read(v.forks))();
      return write(empty2)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component10) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render10)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_2(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect5(function __do3() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do3() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped6(unDriverStateX((function() {
                    var $62 = render10(lchs);
                    return function($63) {
                      return $62((function(v) {
                        return v.selfRef;
                      })($63));
                    };
                  })()))(read($$var2))();
                  bindFlipped6(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot3) {
                  return function __do3() {
                    var childrenIn = map16(slot3.pop)(read(childrenInRef))();
                    var $$var2 = (function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do4() {
                            flip(write)(st.handlerRef)((function() {
                              var $64 = maybe(pure12(unit))(handler3);
                              return function($65) {
                                return $64(slot3.output($65));
                              };
                            })())();
                            return handleAff(evalM(render10)(st.selfRef)(st["component"]["eval"](new Receive(slot3.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)((function() {
                          var $66 = maybe(pure12(unit))(handler3);
                          return function($67) {
                            return $66(slot3.output($67));
                          };
                        })())(slot3.input)(slot3.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    })();
                    var isDuplicate = map16(function($68) {
                      return isJust(slot3.get($68));
                    })(read(childrenOutRef))();
                    when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_2(slot3.set($$var2))(childrenOutRef)();
                    return bind4(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure7(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render10 = function(lchs) {
          return function($$var2) {
            return function __do3() {
              var v = read($$var2)();
              var shouldProcessHandlers = map16(isNothing)(read(v.pendingHandlers))();
              when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty3)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = (function() {
                var $69 = queueOrRun(v.pendingHandlers);
                var $70 = evalF(render10)(v.selfRef);
                return function($71) {
                  return $69($$void5($70($71)));
                };
              })();
              var childHandler = (function() {
                var $72 = queueOrRun(v.pendingQueries);
                return function($73) {
                  return $72(handler3(Action.create($73)));
                };
              })();
              var rendering = renderSpec2.render(function($74) {
                return handleAff(handler3($74));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children3 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do4() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_2)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers,
                  rendering: new Just(rendering),
                  children: children3
                };
              }))();
              return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do4() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23((function() {
                    var $75 = traverse_5(fork4);
                    return function($76) {
                      return handleAff($75(reverse($76)));
                    };
                  })())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $51 = maybe(false)($$null)(mmore);
                  if ($51) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do3() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render10)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_2(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do4() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref2) {
            return function(q2) {
              return bind13(liftEffect5(read(disposed)))(function(v) {
                if (v) {
                  return pure12(Nothing.value);
                }
                ;
                return evalQ(render10)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do3() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do4() {
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_2(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind13(liftEffect5(newLifecycleHandlers))(function(lchs) {
          return bind13(liftEffect5($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do3() {
              var sio = create3();
              var dsx = bindFlipped6(read)(runComponent(lchs)((function() {
                var $77 = notify(sio.listener);
                return function($78) {
                  return liftEffect5($77($78));
                };
              })())(i2)(component10))();
              return unDriverStateX(function(st) {
                return pure7({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map17 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ (function() {
    var $6 = map17(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  })();
  var nextSibling = /* @__PURE__ */ (function() {
    var $15 = map17(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  })();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy7 = function(name16, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var $$void6 = /* @__PURE__ */ $$void(functorEffect);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap3 = /* @__PURE__ */ unwrap();
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map18 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void6(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void6(appendChild(v)(v2.value0));
        }
        ;
        return pure8(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do3() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document3) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap3)(spec);
          var $lazy_patch = $runtime_lazy7("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot3) {
              if (st instanceof Just) {
                if (slot3 instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot3.value0);
                }
                ;
                if (slot3 instanceof ThunkSlot) {
                  var step$prime = step3(st.value0, slot3.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot3.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot3);
            };
          });
          var $lazy_render = $runtime_lazy7("render", "Halogen.VDom.Driver", function() {
            return function(slot3) {
              if (slot3 instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot3.value0);
              }
              ;
              if (slot3 instanceof ThunkSlot) {
                var step4 = buildThunk2(slot3.value0);
                return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot3.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy7("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch2 = $lazy_patch(91);
          var render10 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render10;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document3
        };
      };
    };
  };
  var renderSpec = function(document3) {
    return function(container) {
      var render10 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do3() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document3);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void6(appendChild(node)(toNode(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do3() {
                  write(child)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step3(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when3(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render10,
        renderChild: identity9,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component10) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map18(toDocument)(bindFlipped7(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element3))(component10)(i2);
        });
      };
    };
  };

  // output/Control.Monad.Except/index.js
  var unwrap4 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap4(runExceptT($3));
  };

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click2 = "click";

  // output/Halogen.HTML.Events/index.js
  var mouseHandler = unsafeCoerce2;
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onClick = /* @__PURE__ */ (function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  })();

  // output/V2.Types/index.js
  var Home = /* @__PURE__ */ (function() {
    function Home2() {
    }
    ;
    Home2.value = new Home2();
    return Home2;
  })();
  var Gallery = /* @__PURE__ */ (function() {
    function Gallery2() {
    }
    ;
    Gallery2.value = new Gallery2();
    return Gallery2;
  })();
  var Example = /* @__PURE__ */ (function() {
    function Example2(value0) {
      this.value0 = value0;
    }
    ;
    Example2.create = function(value0) {
      return new Example2(value0);
    };
    return Example2;
  })();
  var Spago = /* @__PURE__ */ (function() {
    function Spago2() {
    }
    ;
    Spago2.value = new Spago2();
    return Spago2;
  })();
  var Interpreters = /* @__PURE__ */ (function() {
    function Interpreters2() {
    }
    ;
    Interpreters2.value = new Interpreters2();
    return Interpreters2;
  })();
  var NotFound = /* @__PURE__ */ (function() {
    function NotFound2() {
    }
    ;
    NotFound2.value = new NotFound2();
    return NotFound2;
  })();
  var Beginner = /* @__PURE__ */ (function() {
    function Beginner2() {
    }
    ;
    Beginner2.value = new Beginner2();
    return Beginner2;
  })();
  var Intermediate = /* @__PURE__ */ (function() {
    function Intermediate2() {
    }
    ;
    Intermediate2.value = new Intermediate2();
    return Intermediate2;
  })();
  var Advanced = /* @__PURE__ */ (function() {
    function Advanced2() {
    }
    ;
    Advanced2.value = new Advanced2();
    return Advanced2;
  })();
  var BasicChart = /* @__PURE__ */ (function() {
    function BasicChart2() {
    }
    ;
    BasicChart2.value = new BasicChart2();
    return BasicChart2;
  })();
  var AdvancedLayout = /* @__PURE__ */ (function() {
    function AdvancedLayout2() {
    }
    ;
    AdvancedLayout2.value = new AdvancedLayout2();
    return AdvancedLayout2;
  })();
  var Interactive2 = /* @__PURE__ */ (function() {
    function Interactive3() {
    }
    ;
    Interactive3.value = new Interactive3();
    return Interactive3;
  })();
  var Interpreter = /* @__PURE__ */ (function() {
    function Interpreter2() {
    }
    ;
    Interpreter2.value = new Interpreter2();
    return Interpreter2;
  })();
  var Application = /* @__PURE__ */ (function() {
    function Application2() {
    }
    ;
    Application2.value = new Application2();
    return Application2;
  })();
  var eqCategory = {
    eq: function(x7) {
      return function(y6) {
        if (x7 instanceof BasicChart && y6 instanceof BasicChart) {
          return true;
        }
        ;
        if (x7 instanceof AdvancedLayout && y6 instanceof AdvancedLayout) {
          return true;
        }
        ;
        if (x7 instanceof Interactive2 && y6 instanceof Interactive2) {
          return true;
        }
        ;
        if (x7 instanceof Interpreter && y6 instanceof Interpreter) {
          return true;
        }
        ;
        if (x7 instanceof Application && y6 instanceof Application) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/V2.Data.Examples/index.js
  var eq13 = /* @__PURE__ */ eq(eqCategory);
  var allExamples = /* @__PURE__ */ (function() {
    return [{
      id: "line-chart",
      title: "Line Chart",
      description: "Basic line chart with linear scales, axes, and path generation",
      about: "A fundamental 2D chart demonstrating how to map data to visual coordinates using linear scales. Shows the use of D3's path generators to create smooth lines through data points. This example covers the core concepts of scales (domain to range mapping), axes (rendering scale ticks and labels), and SVG path generation. Perfect starting point for understanding how D3 transforms data into visual representations.",
      difficulty: Beginner.value,
      category: BasicChart.value,
      tags: ["scales", "axes", "paths", "2d"],
      thumbnail: "assets/thumbnails/line-chart.svg",
      hasInteractivity: false,
      hasComparison: true
    }, {
      id: "bar-chart",
      title: "Bar Chart",
      description: "Vertical bar chart demonstrating rectangles, scales, and data binding",
      about: "Introduces the foundational D3 pattern of binding data to DOM elements. Each data point is mapped to an SVG rectangle, with height and position determined by scales. Demonstrates band scales for categorical data on the x-axis and linear scales for continuous data on the y-axis. Shows how to use data joins to create one rectangle per data point with type-safe attribute assignment.",
      difficulty: Beginner.value,
      category: BasicChart.value,
      tags: ["scales", "axes", "rectangles", "2d"],
      thumbnail: "assets/thumbnails/bar-chart.svg",
      hasInteractivity: false,
      hasComparison: true
    }, {
      id: "scatter-plot",
      title: "Scatter Plot",
      description: "Scatter plot with circles, showing relationship between two variables",
      about: "Visualizes the relationship between two continuous variables by plotting points in 2D space. Each circle's x and y coordinates are determined by linear scales mapping data values to pixel coordinates. This example demonstrates working with circles in SVG and highlights D3's ability to reveal correlations and patterns in bivariate data.",
      difficulty: Beginner.value,
      category: BasicChart.value,
      tags: ["scales", "axes", "circles", "2d"],
      thumbnail: "assets/thumbnails/scatter-plot.svg",
      hasInteractivity: false,
      hasComparison: true
    }, {
      id: "scatter-quartet",
      title: "Anscombe's Quartet",
      description: "Four scatter plots demonstrating Anscombe's Quartet in small multiples",
      about: "Displays Anscombe's famous statistical quartet - four datasets with identical statistical properties but dramatically different distributions. Uses the 'small multiples' technique to show all four plots side by side, demonstrating why visualization is essential. Shows how to create multiple coordinated charts and the importance of always visualizing your data.",
      difficulty: Intermediate.value,
      category: BasicChart.value,
      tags: ["scatter", "small-multiples", "statistics"],
      thumbnail: "assets/thumbnails/scatter-quartet.svg",
      hasInteractivity: false,
      hasComparison: false
    }, {
      id: "chord-diagram",
      title: "Chord Diagram",
      description: "Circular chord diagram showing relationships in a dependency matrix",
      about: "Visualizes relationships between entities arranged in a circle, with ribbons connecting related items. Uses D3's chord layout to transform a dependency matrix into visual arcs and ribbons. Perfect for showing flows, dependencies, or relationships in systems. Demonstrates working with D3's arc and ribbon generators, as well as radial coordinate systems.",
      difficulty: Advanced.value,
      category: AdvancedLayout.value,
      tags: ["chord", "circular", "relationships", "arcs", "ribbons"],
      thumbnail: "assets/thumbnails/chord-diagram.svg",
      hasInteractivity: false,
      hasComparison: false
    }, {
      id: "bubble-chart",
      title: "Bubble Chart (Circle Pack)",
      description: "Hierarchical bubble chart using D3's pack layout",
      about: "Displays hierarchical data as nested circles, where each node is sized according to its value and positioned to minimize overlaps. Uses D3's pack layout algorithm to efficiently arrange circles in a space-filling pattern. Great for showing part-to-whole relationships and hierarchical structure simultaneously. Demonstrates working with D3 hierarchy layouts and the Finally Tagless pattern.",
      difficulty: Intermediate.value,
      category: AdvancedLayout.value,
      tags: ["hierarchy", "circles", "pack-layout"],
      thumbnail: "assets/thumbnails/bubble-chart.svg",
      hasInteractivity: false,
      hasComparison: false
    }, {
      id: "sankey",
      title: "Sankey Diagram",
      description: "Flow diagram with configurable node and link visualization",
      about: "Visualizes flow between nodes, where the width of connections represents flow magnitude. Interactive controls allow configuration of node padding, alignment, and iterations. Excellent for showing energy flows, process steps, or any system where quantity flows between stages. Demonstrates D3's sankey layout and the library's approach to providing user controls that modify visualization parameters.",
      difficulty: Advanced.value,
      category: AdvancedLayout.value,
      tags: ["flow", "sankey", "interactive"],
      thumbnail: "assets/thumbnails/sankey.svg",
      hasInteractivity: true,
      hasComparison: false
    }, {
      id: "tree",
      title: "Tree Layout",
      description: "Hierarchical tree visualization with multiple layout options",
      about: "Displays hierarchical data as a node-link tree diagram. Provides multiple layout algorithms (tidy tree, cluster, radial) that can be toggled to show different aspects of the same data. Demonstrates D3's powerful hierarchy layouts and how the Finally Tagless pattern allows switching between different interpretations of the same visualization code.",
      difficulty: Intermediate.value,
      category: AdvancedLayout.value,
      tags: ["hierarchy", "tree", "dendogram"],
      thumbnail: "assets/thumbnails/tree.svg",
      hasInteractivity: true,
      hasComparison: false
    }, {
      id: "three-little-circles",
      title: "Three Little Circles",
      description: "Introduction to D3 selections and data binding",
      about: "The classic 'Hello World' of D3, based on Mike Bostock's famous Three Little Circles tutorial. Demonstrates the fundamental concept of data binding: associating an array of data with DOM elements. Shows how D3 creates, updates, and removes elements based on data changes. This simple example contains the core idea that makes D3 powerful - declaratively describing what the page should look like for any given data.",
      difficulty: Beginner.value,
      category: Interactive2.value,
      tags: ["basics", "selections", "data-binding"],
      thumbnail: "assets/thumbnails/three-circles.svg",
      hasInteractivity: true,
      hasComparison: true
    }, {
      id: "gup",
      title: "General Update Pattern",
      description: "Interactive demonstration of enter, update, and exit selections",
      about: "Interactive exploration of D3's General Update Pattern - the enter/update/exit selection pattern that handles dynamic data. Click to add or remove data points and watch as D3 smoothly transitions elements in, updates them, or removes them. This pattern is the heart of D3's data-driven approach and this example makes it tangible by letting you trigger updates and see the three selection types in action with animated transitions.",
      difficulty: Intermediate.value,
      category: Interactive2.value,
      tags: ["data-join", "transitions", "update-pattern"],
      thumbnail: "assets/thumbnails/gup.svg",
      hasInteractivity: true,
      hasComparison: false
    }, {
      id: "les-mis",
      title: "Les Mis\xE9rables Network",
      description: "Force-directed graph with drag interaction",
      about: "Network visualization of character relationships in Les Mis\xE9rables, using D3's force simulation to position nodes. Nodes attract and repel each other based on connections, creating an organic layout. Drag nodes to reposition them, and watch the physics simulation respond. Demonstrates D3's force-directed graph capabilities and how to add drag behavior. Shows integration between D3's simulation engine and the type-safe PureScript API.",
      difficulty: Advanced.value,
      category: Interactive2.value,
      tags: ["force", "network", "drag", "simulation"],
      thumbnail: "assets/thumbnails/les-mis.svg",
      hasInteractivity: true,
      hasComparison: false
    }, {
      id: "meta-tree",
      title: "MetaTree Visualizer",
      description: "Visualizes the DSL syntax tree of visualizations",
      about: "Demonstrates the Finally Tagless pattern by running the same visualization code through a different interpreter. Instead of rendering to SVG, this interpreter visualizes the abstract syntax tree of the visualization itself as a tree diagram. This meta-visualization shows how the same code can be interpreted in completely different ways - a key advantage of the Finally Tagless approach. Perfect for understanding the DSL structure and debugging complex visualizations.",
      difficulty: Advanced.value,
      category: Interpreter.value,
      tags: ["meta", "interpreter", "dsl"],
      thumbnail: "assets/thumbnails/meta-tree.svg",
      hasInteractivity: false,
      hasComparison: false
    }, {
      id: "print-tree",
      title: "String Generator",
      description: "Generates code and documentation from visualization definitions",
      about: "Another interpreter demonstrating Finally Tagless flexibility. This interpreter takes visualization code and generates human-readable text descriptions or code snippets. Instead of creating visual output, it produces strings documenting what the visualization does. Shows how the same high-level visualization definition can be used for documentation generation, code analysis, or teaching - all without modifying the original visualization code.",
      difficulty: Advanced.value,
      category: Interpreter.value,
      tags: ["meta", "interpreter", "codegen"],
      thumbnail: "assets/thumbnails/print-tree.svg",
      hasInteractivity: false,
      hasComparison: false
    }, {
      id: "spago",
      title: "Spago Dependency Explorer",
      description: "Interactive application for exploring PureScript package dependencies",
      about: "A real-world application that fetches and visualizes PureScript package dependencies from your spago.dhall file. Uses force-directed layout to show package relationships, with interactive features like drag, zoom, and filtering. Demonstrates how D3 visualizations integrate into larger Halogen applications with bidirectional communication - visualization events trigger app-level actions and app state updates drive visualization changes. This is what the library was built for: serious, interactive applications.",
      difficulty: Advanced.value,
      category: Application.value,
      tags: ["force", "network", "interactive", "real-world"],
      thumbnail: "assets/thumbnails/spago.svg",
      hasInteractivity: true,
      hasComparison: false
    }];
  })();
  var getExample = function(id5) {
    return find2(function(ex) {
      return ex.id === id5;
    })(allExamples);
  };
  var getExamplesByCategory = function(cat) {
    return filter2(function(ex) {
      return eq13(ex.category)(cat);
    })(allExamples);
  };

  // output/V2.Components.Gallery/index.js
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorArray);
  var modify_3 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var eq2 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqCategory));
  var SetFilter = /* @__PURE__ */ (function() {
    function SetFilter2(value0) {
      this.value0 = value0;
    }
    ;
    SetFilter2.create = function(value0) {
      return new SetFilter2(value0);
    };
    return SetFilter2;
  })();
  var SetSearch = /* @__PURE__ */ (function() {
    function SetSearch2(value0) {
      this.value0 = value0;
    }
    ;
    SetSearch2.create = function(value0) {
      return new SetSearch2(value0);
    };
    return SetSearch2;
  })();
  var SelectExample = /* @__PURE__ */ (function() {
    function SelectExample2(value0) {
      this.value0 = value0;
    }
    ;
    SelectExample2.create = function(value0) {
      return new SelectExample2(value0);
    };
    return SelectExample2;
  })();
  var renderExampleCard = function(example) {
    return div2([classes(["gallery__card-wrapper"]), onClick(function(v) {
      return new SelectExample(example.id);
    })])([div2([classes(["gallery__card-thumbnail"])])([div2([classes(["gallery__card-thumbnail-placeholder"])])([text5(example.title)])]), div2([classes(["gallery__card-content"])])([div2([classes(["gallery__card-header"])])([h3([classes(["gallery__card-title"])])([text5(example.title)])]), p([classes(["gallery__card-description"])])([text5(example.description)]), div2([classes(["gallery__card-tags"])])(mapFlipped2(example.tags)(function(tag) {
      return span3([classes(["gallery__card-tag"])])([text5(tag)]);
    }))])]);
  };
  var initialState = /* @__PURE__ */ (function() {
    return {
      filterCategory: Nothing.value,
      searchTerm: ""
    };
  })();
  var handleAction = function(v) {
    if (v instanceof SetFilter) {
      return modify_3(function(v1) {
        var $17 = {};
        for (var $18 in v1) {
          if ({}.hasOwnProperty.call(v1, $18)) {
            $17[$18] = v1[$18];
          }
          ;
        }
        ;
        $17.filterCategory = v.value0;
        return $17;
      });
    }
    ;
    if (v instanceof SetSearch) {
      return modify_3(function(v1) {
        var $21 = {};
        for (var $22 in v1) {
          if ({}.hasOwnProperty.call(v1, $22)) {
            $21[$22] = v1[$22];
          }
          ;
        }
        ;
        $21.searchTerm = v.value0;
        return $21;
      });
    }
    ;
    if (v instanceof SelectExample) {
      return raise(v.value0);
    }
    ;
    throw new Error("Failed pattern match at V2.Components.Gallery (line 161, column 16 - line 170, column 22): " + [v.constructor.name]);
  };
  var filteredExamples = function(state3) {
    if (state3.filterCategory instanceof Nothing) {
      return allExamples;
    }
    ;
    if (state3.filterCategory instanceof Just) {
      return getExamplesByCategory(state3.filterCategory.value0);
    }
    ;
    throw new Error("Failed pattern match at V2.Components.Gallery (line 117, column 3 - line 119, column 42): " + [state3.filterCategory.constructor.name]);
  };
  var categoryButtonClasses = function(buttonCat) {
    return function(selectedCat) {
      return ["gallery__filter-button", (function() {
        var $28 = eq2(buttonCat)(selectedCat);
        if ($28) {
          return "gallery__filter-button--active";
        }
        ;
        return "";
      })()];
    };
  };
  var render = function(state3) {
    return div2([classes(["gallery"])])([div2([classes(["gallery__header"])])([h1([classes(["gallery__title"])])([text5("Examples Gallery")]), p([classes(["gallery__subtitle"])])([text5("Explore type-safe, composable data visualizations")])]), div2([classes(["gallery__filters"])])([div2([classes(["gallery__filter-group"])])([label4([classes(["gallery__filter-label"])])([text5("Category:")]), button([classes(categoryButtonClasses(Nothing.value)(state3.filterCategory)), onClick(function(v) {
      return new SetFilter(Nothing.value);
    })])([text5("All")]), button([classes(categoryButtonClasses(new Just(BasicChart.value))(state3.filterCategory)), onClick(function(v) {
      return new SetFilter(new Just(BasicChart.value));
    })])([text5("Basic Charts")]), button([classes(categoryButtonClasses(new Just(AdvancedLayout.value))(state3.filterCategory)), onClick(function(v) {
      return new SetFilter(new Just(AdvancedLayout.value));
    })])([text5("Advanced Layouts")]), button([classes(categoryButtonClasses(new Just(Interactive2.value))(state3.filterCategory)), onClick(function(v) {
      return new SetFilter(new Just(Interactive2.value));
    })])([text5("Interactive")]), button([classes(categoryButtonClasses(new Just(Interpreter.value))(state3.filterCategory)), onClick(function(v) {
      return new SetFilter(new Just(Interpreter.value));
    })])([text5("Interpreters")]), button([classes(categoryButtonClasses(new Just(Application.value))(state3.filterCategory)), onClick(function(v) {
      return new SetFilter(new Just(Application.value));
    })])([text5("Applications")])])]), div2([classes(["gallery__grid"])])(mapFlipped2(filteredExamples(state3))(renderExampleCard))]);
  };
  var component = /* @__PURE__ */ (function() {
    return mkComponent({
      initialState: function(v) {
        return initialState;
      },
      render,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize,
        handleAction
      })
    });
  })();

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i2 = 0; i2 < n; ++i2) {
            var o = iter.next();
            if (o.done) return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodePoints/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map19 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div3 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons4 = function(s) {
    var v = length5(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map19(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons4(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length5(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length8 = function($74) {
    return length4(toCodePointArray($74));
  };
  var indexOf2 = function(p2) {
    return function(s) {
      return map19(function(i2) {
        return length8(take2(i2)(s));
      })(indexOf(p2)(s));
    };
  };
  var fromCharCode2 = /* @__PURE__ */ (function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  })();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div3(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton7 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons4(v1);
      if (v2 instanceof Just) {
        return singleton7(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take3 = /* @__PURE__ */ _take(takeFallback);
  var drop4 = function(n) {
    return function(s) {
      return drop2(length5(take3(n)(s)))(s);
    };
  };

  // output/V2.Router/index.js
  var routeToHash = function(v) {
    if (v instanceof Home) {
      return "#/";
    }
    ;
    if (v instanceof Gallery) {
      return "#/gallery";
    }
    ;
    if (v instanceof Example) {
      return "#/example/" + v.value0;
    }
    ;
    if (v instanceof Spago) {
      return "#/spago";
    }
    ;
    if (v instanceof Interpreters) {
      return "#/interpreters";
    }
    ;
    if (v instanceof NotFound) {
      return "#/not-found";
    }
    ;
    throw new Error("Failed pattern match at V2.Router (line 35, column 1 - line 35, column 31): " + [v.constructor.name]);
  };
  var parseRoute = function(hash2) {
    var path2 = (function() {
      var $12 = length5(hash2) > 0 && take3(1)(hash2) === "#";
      if ($12) {
        return drop4(1)(hash2);
      }
      ;
      return hash2;
    })();
    var cleanPath = (function() {
      var $13 = length5(path2) > 0 && take3(1)(path2) === "/";
      if ($13) {
        return drop4(1)(path2);
      }
      ;
      return path2;
    })();
    var segments = split("/")(cleanPath);
    var v = index2(segments)(0);
    if (v instanceof Nothing) {
      return Home.value;
    }
    ;
    if (v instanceof Just && v.value0 === "") {
      return Home.value;
    }
    ;
    if (v instanceof Just && v.value0 === "gallery") {
      return Gallery.value;
    }
    ;
    if (v instanceof Just && v.value0 === "example") {
      var v1 = index2(segments)(1);
      if (v1 instanceof Just) {
        return new Example(v1.value0);
      }
      ;
      if (v1 instanceof Nothing) {
        return Gallery.value;
      }
      ;
      throw new Error("Failed pattern match at V2.Router (line 27, column 23 - line 29, column 25): " + [v1.constructor.name]);
    }
    ;
    if (v instanceof Just && v.value0 === "spago") {
      return Spago.value;
    }
    ;
    if (v instanceof Just && v.value0 === "interpreters") {
      return Interpreters.value;
    }
    ;
    return NotFound.value;
  };

  // output/V2.Components.Navigation/index.js
  var append6 = /* @__PURE__ */ append(semigroupArray);
  var isActive = function(v) {
    return function(v1) {
      if (v instanceof Home && v1 instanceof Home) {
        return true;
      }
      ;
      if (v instanceof Gallery && v1 instanceof Gallery) {
        return true;
      }
      ;
      if (v instanceof Example && v1 instanceof Example) {
        return true;
      }
      ;
      if (v instanceof Spago && v1 instanceof Spago) {
        return true;
      }
      ;
      if (v instanceof Interpreters && v1 instanceof Interpreters) {
        return true;
      }
      ;
      return false;
    };
  };
  var navLink = function(route) {
    return function(label5) {
      return function(currentRoute) {
        return li([classes(["navigation__item"])])([a([href4(routeToHash(route)), classes(append6(["navigation__link"])((function() {
          var $8 = isActive(route)(currentRoute);
          if ($8) {
            return ["navigation__link--active"];
          }
          ;
          return [];
        })()))])([text5(label5)])]);
      };
    };
  };
  var render2 = function(currentRoute) {
    return nav([classes(["navigation"])])([div2([classes(["navigation__container"])])([a([href4(routeToHash(Home.value)), classes(["navigation__brand"])])([img([src9("../v1/PSD3-logo.png"), alt5("PureScript Tagless D3"), classes(["navigation__logo"])]), span3([classes(["navigation__brand-text"])])([text5("PureScript Tagless D3")])]), ul([classes(["navigation__links"])])([navLink(Home.value)("Home")(currentRoute), navLink(Gallery.value)("Gallery")(currentRoute), navLink(Spago.value)("Spago Explorer")(currentRoute), navLink(Interpreters.value)("Interpreters")(currentRoute), li([classes(["navigation__item"])])([a([href4("https://github.com/afcondon/purescript-d3-tagless"), target5("_blank"), rel4("noopener noreferrer"), classes(["navigation__link"])])([text5("GitHub")])])])])]);
  };
  var component2 = /* @__PURE__ */ mkComponent({
    initialState: /* @__PURE__ */ identity(categoryFn),
    render: render2,
    "eval": /* @__PURE__ */ mkEval(defaultEval)
  });

  // output/Affjax/foreign.js
  function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options2) {
    return function(errback, callback) {
      var xhr = platformSpecificDriver.newXHR();
      var fixedUrl = platformSpecificDriver.fixupUrl(options2.url, xhr);
      xhr.open(options2.method || "GET", fixedUrl, true, options2.username, options2.password);
      if (options2.headers) {
        try {
          for (var i2 = 0, header2; (header2 = options2.headers[i2]) != null; i2++) {
            xhr.setRequestHeader(header2.field, header2.value);
          }
        } catch (e) {
          errback(e);
        }
      }
      var onerror = function(msgIdent) {
        return function() {
          errback(new Error(msgIdent));
        };
      };
      xhr.onerror = onerror(requestFailedMessageIdent);
      xhr.ontimeout = onerror(timeoutErrorMessageIdent);
      xhr.onload = function() {
        callback({
          status: xhr.status,
          statusText: xhr.statusText,
          headers: xhr.getAllResponseHeaders().split("\r\n").filter(function(header3) {
            return header3.length > 0;
          }).map(function(header3) {
            var i3 = header3.indexOf(":");
            return mkHeader(header3.substring(0, i3))(header3.substring(i3 + 2));
          }),
          body: xhr.response
        });
      };
      xhr.responseType = options2.responseType;
      xhr.withCredentials = options2.withCredentials;
      xhr.timeout = options2.timeout;
      xhr.send(options2.content);
      return function(error4, cancelErrback, cancelCallback) {
        try {
          xhr.abort();
        } catch (e) {
          return cancelErrback(e);
        }
        return cancelCallback();
      };
    };
  }

  // output/Data.MediaType.Common/index.js
  var applicationJSON = "application/json";
  var applicationFormURLEncoded = "application/x-www-form-urlencoded";

  // output/Affjax.RequestBody/index.js
  var ArrayView = /* @__PURE__ */ (function() {
    function ArrayView2(value0) {
      this.value0 = value0;
    }
    ;
    ArrayView2.create = function(value0) {
      return new ArrayView2(value0);
    };
    return ArrayView2;
  })();
  var Blob = /* @__PURE__ */ (function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  })();
  var Document = /* @__PURE__ */ (function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  })();
  var $$String = /* @__PURE__ */ (function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  })();
  var FormData = /* @__PURE__ */ (function() {
    function FormData2(value0) {
      this.value0 = value0;
    }
    ;
    FormData2.create = function(value0) {
      return new FormData2(value0);
    };
    return FormData2;
  })();
  var FormURLEncoded = /* @__PURE__ */ (function() {
    function FormURLEncoded2(value0) {
      this.value0 = value0;
    }
    ;
    FormURLEncoded2.create = function(value0) {
      return new FormURLEncoded2(value0);
    };
    return FormURLEncoded2;
  })();
  var Json = /* @__PURE__ */ (function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  })();
  var toMediaType = function(v) {
    if (v instanceof FormURLEncoded) {
      return new Just(applicationFormURLEncoded);
    }
    ;
    if (v instanceof Json) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
  };

  // output/Affjax.RequestHeader/index.js
  var unwrap5 = /* @__PURE__ */ unwrap();
  var Accept = /* @__PURE__ */ (function() {
    function Accept2(value0) {
      this.value0 = value0;
    }
    ;
    Accept2.create = function(value0) {
      return new Accept2(value0);
    };
    return Accept2;
  })();
  var ContentType = /* @__PURE__ */ (function() {
    function ContentType2(value0) {
      this.value0 = value0;
    }
    ;
    ContentType2.create = function(value0) {
      return new ContentType2(value0);
    };
    return ContentType2;
  })();
  var RequestHeader = /* @__PURE__ */ (function() {
    function RequestHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RequestHeader2.create = function(value0) {
      return function(value1) {
        return new RequestHeader2(value0, value1);
      };
    };
    return RequestHeader2;
  })();
  var value12 = function(v) {
    if (v instanceof Accept) {
      return unwrap5(v.value0);
    }
    ;
    if (v instanceof ContentType) {
      return unwrap5(v.value0);
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value1;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
  };
  var name15 = function(v) {
    if (v instanceof Accept) {
      return "Accept";
    }
    ;
    if (v instanceof ContentType) {
      return "Content-Type";
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [v.constructor.name]);
  };

  // output/Affjax.ResponseFormat/index.js
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var $$ArrayBuffer = /* @__PURE__ */ (function() {
    function $$ArrayBuffer2(value0) {
      this.value0 = value0;
    }
    ;
    $$ArrayBuffer2.create = function(value0) {
      return new $$ArrayBuffer2(value0);
    };
    return $$ArrayBuffer2;
  })();
  var Blob2 = /* @__PURE__ */ (function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  })();
  var Document2 = /* @__PURE__ */ (function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  })();
  var Json2 = /* @__PURE__ */ (function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  })();
  var $$String2 = /* @__PURE__ */ (function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  })();
  var Ignore = /* @__PURE__ */ (function() {
    function Ignore2(value0) {
      this.value0 = value0;
    }
    ;
    Ignore2.create = function(value0) {
      return new Ignore2(value0);
    };
    return Ignore2;
  })();
  var toResponseType = function(v) {
    if (v instanceof $$ArrayBuffer) {
      return "arraybuffer";
    }
    ;
    if (v instanceof Blob2) {
      return "blob";
    }
    ;
    if (v instanceof Document2) {
      return "document";
    }
    ;
    if (v instanceof Json2) {
      return "text";
    }
    ;
    if (v instanceof $$String2) {
      return "text";
    }
    ;
    if (v instanceof Ignore) {
      return "";
    }
    ;
    throw new Error("Failed pattern match at Affjax.ResponseFormat (line 44, column 3 - line 50, column 19): " + [v.constructor.name]);
  };
  var toMediaType2 = function(v) {
    if (v instanceof Json2) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
  };
  var string = /* @__PURE__ */ (function() {
    return new $$String2(identity10);
  })();
  var ignore = /* @__PURE__ */ (function() {
    return new Ignore(identity10);
  })();

  // output/Affjax.ResponseHeader/index.js
  var ResponseHeader = /* @__PURE__ */ (function() {
    function ResponseHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseHeader2.create = function(value0) {
      return function(value1) {
        return new ResponseHeader2(value0, value1);
      };
    };
    return ResponseHeader2;
  })();

  // output/Data.Argonaut.Core/foreign.js
  function id3(x7) {
    return x7;
  }
  function stringify(j) {
    return JSON.stringify(j);
  }

  // output/Data.Argonaut.Core/index.js
  var jsonEmptyObject = /* @__PURE__ */ id3(empty4);

  // output/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail3, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail3(e.message);
    }
  }

  // output/Data.Argonaut.Parser/index.js
  var jsonParser = function(j) {
    return _jsonParser(Left.create, Right.create, j);
  };

  // output/JSURI/foreign.js
  function toRFC3896(input3) {
    return input3.replace(/[!'()*]/g, function(c) {
      return "%" + c.charCodeAt(0).toString(16);
    });
  }
  var _encodeFormURLComponent = function encode(fail3, succeed, input3) {
    try {
      return succeed(toRFC3896(encodeURIComponent(input3)).replace(/%20/g, "+"));
    } catch (err) {
      return fail3(err);
    }
  };

  // output/JSURI/index.js
  var encodeFormURLComponent = /* @__PURE__ */ (function() {
    return runFn3(_encodeFormURLComponent)($$const(Nothing.value))(Just.create);
  })();

  // output/Data.FormURLEncoded/index.js
  var apply2 = /* @__PURE__ */ apply(applyMaybe);
  var map20 = /* @__PURE__ */ map(functorMaybe);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeMaybe);
  var toArray = function(v) {
    return v;
  };
  var encode2 = /* @__PURE__ */ (function() {
    var encodePart = function(v) {
      if (v.value1 instanceof Nothing) {
        return encodeFormURLComponent(v.value0);
      }
      ;
      if (v.value1 instanceof Just) {
        return apply2(map20(function(key) {
          return function(val) {
            return key + ("=" + val);
          };
        })(encodeFormURLComponent(v.value0)))(encodeFormURLComponent(v.value1.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 16 - line 39, column 114): " + [v.constructor.name]);
    };
    var $37 = map20(joinWith("&"));
    var $38 = traverse2(encodePart);
    return function($39) {
      return $37($38(toArray($39)));
    };
  })();

  // output/Data.HTTP.Method/index.js
  var OPTIONS = /* @__PURE__ */ (function() {
    function OPTIONS2() {
    }
    ;
    OPTIONS2.value = new OPTIONS2();
    return OPTIONS2;
  })();
  var GET2 = /* @__PURE__ */ (function() {
    function GET3() {
    }
    ;
    GET3.value = new GET3();
    return GET3;
  })();
  var HEAD = /* @__PURE__ */ (function() {
    function HEAD2() {
    }
    ;
    HEAD2.value = new HEAD2();
    return HEAD2;
  })();
  var POST2 = /* @__PURE__ */ (function() {
    function POST3() {
    }
    ;
    POST3.value = new POST3();
    return POST3;
  })();
  var PUT = /* @__PURE__ */ (function() {
    function PUT2() {
    }
    ;
    PUT2.value = new PUT2();
    return PUT2;
  })();
  var DELETE = /* @__PURE__ */ (function() {
    function DELETE2() {
    }
    ;
    DELETE2.value = new DELETE2();
    return DELETE2;
  })();
  var TRACE = /* @__PURE__ */ (function() {
    function TRACE2() {
    }
    ;
    TRACE2.value = new TRACE2();
    return TRACE2;
  })();
  var CONNECT = /* @__PURE__ */ (function() {
    function CONNECT2() {
    }
    ;
    CONNECT2.value = new CONNECT2();
    return CONNECT2;
  })();
  var PROPFIND = /* @__PURE__ */ (function() {
    function PROPFIND2() {
    }
    ;
    PROPFIND2.value = new PROPFIND2();
    return PROPFIND2;
  })();
  var PROPPATCH = /* @__PURE__ */ (function() {
    function PROPPATCH2() {
    }
    ;
    PROPPATCH2.value = new PROPPATCH2();
    return PROPPATCH2;
  })();
  var MKCOL = /* @__PURE__ */ (function() {
    function MKCOL2() {
    }
    ;
    MKCOL2.value = new MKCOL2();
    return MKCOL2;
  })();
  var COPY = /* @__PURE__ */ (function() {
    function COPY2() {
    }
    ;
    COPY2.value = new COPY2();
    return COPY2;
  })();
  var MOVE = /* @__PURE__ */ (function() {
    function MOVE2() {
    }
    ;
    MOVE2.value = new MOVE2();
    return MOVE2;
  })();
  var LOCK = /* @__PURE__ */ (function() {
    function LOCK2() {
    }
    ;
    LOCK2.value = new LOCK2();
    return LOCK2;
  })();
  var UNLOCK = /* @__PURE__ */ (function() {
    function UNLOCK2() {
    }
    ;
    UNLOCK2.value = new UNLOCK2();
    return UNLOCK2;
  })();
  var PATCH = /* @__PURE__ */ (function() {
    function PATCH2() {
    }
    ;
    PATCH2.value = new PATCH2();
    return PATCH2;
  })();
  var unCustomMethod = function(v) {
    return v;
  };
  var showMethod = {
    show: function(v) {
      if (v instanceof OPTIONS) {
        return "OPTIONS";
      }
      ;
      if (v instanceof GET2) {
        return "GET";
      }
      ;
      if (v instanceof HEAD) {
        return "HEAD";
      }
      ;
      if (v instanceof POST2) {
        return "POST";
      }
      ;
      if (v instanceof PUT) {
        return "PUT";
      }
      ;
      if (v instanceof DELETE) {
        return "DELETE";
      }
      ;
      if (v instanceof TRACE) {
        return "TRACE";
      }
      ;
      if (v instanceof CONNECT) {
        return "CONNECT";
      }
      ;
      if (v instanceof PROPFIND) {
        return "PROPFIND";
      }
      ;
      if (v instanceof PROPPATCH) {
        return "PROPPATCH";
      }
      ;
      if (v instanceof MKCOL) {
        return "MKCOL";
      }
      ;
      if (v instanceof COPY) {
        return "COPY";
      }
      ;
      if (v instanceof MOVE) {
        return "MOVE";
      }
      ;
      if (v instanceof LOCK) {
        return "LOCK";
      }
      ;
      if (v instanceof UNLOCK) {
        return "UNLOCK";
      }
      ;
      if (v instanceof PATCH) {
        return "PATCH";
      }
      ;
      throw new Error("Failed pattern match at Data.HTTP.Method (line 43, column 1 - line 59, column 23): " + [v.constructor.name]);
    }
  };
  var print6 = /* @__PURE__ */ either(/* @__PURE__ */ show(showMethod))(unCustomMethod);

  // output/Effect.Aff.Compat/index.js
  var fromEffectFnAff = function(v) {
    return makeAff(function(k) {
      return function __do3() {
        var v1 = v(function($9) {
          return k(Left.create($9))();
        }, function($10) {
          return k(Right.create($10))();
        });
        return function(e) {
          return makeAff(function(k2) {
            return function __do4() {
              v1(e, function($11) {
                return k2(Left.create($11))();
              }, function($12) {
                return k2(Right.create($12))();
              });
              return nonCanceler;
            };
          });
        };
      };
    });
  };

  // output/Affjax/index.js
  var pure9 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var fail2 = /* @__PURE__ */ fail(monadIdentity);
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var alt6 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var map21 = /* @__PURE__ */ map(functorMaybe);
  var any3 = /* @__PURE__ */ any(foldableArray)(heytingAlgebraBoolean);
  var eq3 = /* @__PURE__ */ eq(eqString);
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var map110 = /* @__PURE__ */ map(functorArray);
  var mapFlipped3 = /* @__PURE__ */ mapFlipped(functorAff);
  var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
  var pure13 = /* @__PURE__ */ pure(applicativeAff);
  var RequestContentError = /* @__PURE__ */ (function() {
    function RequestContentError2(value0) {
      this.value0 = value0;
    }
    ;
    RequestContentError2.create = function(value0) {
      return new RequestContentError2(value0);
    };
    return RequestContentError2;
  })();
  var ResponseBodyError = /* @__PURE__ */ (function() {
    function ResponseBodyError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseBodyError2.create = function(value0) {
      return function(value1) {
        return new ResponseBodyError2(value0, value1);
      };
    };
    return ResponseBodyError2;
  })();
  var TimeoutError = /* @__PURE__ */ (function() {
    function TimeoutError2() {
    }
    ;
    TimeoutError2.value = new TimeoutError2();
    return TimeoutError2;
  })();
  var RequestFailedError = /* @__PURE__ */ (function() {
    function RequestFailedError2() {
    }
    ;
    RequestFailedError2.value = new RequestFailedError2();
    return RequestFailedError2;
  })();
  var XHROtherError = /* @__PURE__ */ (function() {
    function XHROtherError2(value0) {
      this.value0 = value0;
    }
    ;
    XHROtherError2.create = function(value0) {
      return new XHROtherError2(value0);
    };
    return XHROtherError2;
  })();
  var request2 = function(driver2) {
    return function(req2) {
      var parseJSON = function(v2) {
        if (v2 === "") {
          return pure9(jsonEmptyObject);
        }
        ;
        return either(function($74) {
          return fail2(ForeignError.create($74));
        })(pure9)(jsonParser(v2));
      };
      var fromResponse = (function() {
        if (req2.responseFormat instanceof $$ArrayBuffer) {
          return unsafeReadTagged2("ArrayBuffer");
        }
        ;
        if (req2.responseFormat instanceof Blob2) {
          return unsafeReadTagged2("Blob");
        }
        ;
        if (req2.responseFormat instanceof Document2) {
          return function(x7) {
            return alt6(unsafeReadTagged2("Document")(x7))(alt6(unsafeReadTagged2("XMLDocument")(x7))(unsafeReadTagged2("HTMLDocument")(x7)));
          };
        }
        ;
        if (req2.responseFormat instanceof Json2) {
          return composeKleisliFlipped3(function($75) {
            return req2.responseFormat.value0(parseJSON($75));
          })(unsafeReadTagged2("String"));
        }
        ;
        if (req2.responseFormat instanceof $$String2) {
          return unsafeReadTagged2("String");
        }
        ;
        if (req2.responseFormat instanceof Ignore) {
          return $$const(req2.responseFormat.value0(pure9(unit)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 274, column 18 - line 283, column 57): " + [req2.responseFormat.constructor.name]);
      })();
      var extractContent = function(v2) {
        if (v2 instanceof ArrayView) {
          return new Right(v2.value0(unsafeToForeign));
        }
        ;
        if (v2 instanceof Blob) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof Document) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof $$String) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormData) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormURLEncoded) {
          return note("Body contains values that cannot be encoded as application/x-www-form-urlencoded")(map21(unsafeToForeign)(encode2(v2.value0)));
        }
        ;
        if (v2 instanceof Json) {
          return new Right(unsafeToForeign(stringify(v2.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 235, column 20 - line 250, column 69): " + [v2.constructor.name]);
      };
      var addHeader = function(mh) {
        return function(hs) {
          if (mh instanceof Just && !any3(on(eq3)(name15)(mh.value0))(hs)) {
            return snoc(hs)(mh.value0);
          }
          ;
          return hs;
        };
      };
      var headers = function(reqContent) {
        return addHeader(map21(ContentType.create)(bindFlipped8(toMediaType)(reqContent)))(addHeader(map21(Accept.create)(toMediaType2(req2.responseFormat)))(req2.headers));
      };
      var ajaxRequest = function(v2) {
        return {
          method: print6(req2.method),
          url: req2.url,
          headers: map110(function(h) {
            return {
              field: name15(h),
              value: value12(h)
            };
          })(headers(req2.content)),
          content: v2,
          responseType: toResponseType(req2.responseFormat),
          username: toNullable(req2.username),
          password: toNullable(req2.password),
          withCredentials: req2.withCredentials,
          timeout: fromMaybe(0)(map21(function(v1) {
            return v1;
          })(req2.timeout))
        };
      };
      var send = function(content3) {
        return mapFlipped3($$try3(fromEffectFnAff(_ajax(driver2, "AffjaxTimeoutErrorMessageIdent", "AffjaxRequestFailedMessageIdent", ResponseHeader.create, ajaxRequest(content3)))))(function(v2) {
          if (v2 instanceof Right) {
            var v1 = runExcept(fromResponse(v2.value0.body));
            if (v1 instanceof Left) {
              return new Left(new ResponseBodyError(head2(v1.value0), v2.value0));
            }
            ;
            if (v1 instanceof Right) {
              return new Right({
                headers: v2.value0.headers,
                status: v2.value0.status,
                statusText: v2.value0.statusText,
                body: v1.value0
              });
            }
            ;
            throw new Error("Failed pattern match at Affjax (line 209, column 9 - line 211, column 52): " + [v1.constructor.name]);
          }
          ;
          if (v2 instanceof Left) {
            return new Left((function() {
              var message2 = message(v2.value0);
              var $61 = message2 === "AffjaxTimeoutErrorMessageIdent";
              if ($61) {
                return TimeoutError.value;
              }
              ;
              var $62 = message2 === "AffjaxRequestFailedMessageIdent";
              if ($62) {
                return RequestFailedError.value;
              }
              ;
              return new XHROtherError(v2.value0);
            })());
          }
          ;
          throw new Error("Failed pattern match at Affjax (line 207, column 144 - line 219, column 28): " + [v2.constructor.name]);
        });
      };
      if (req2.content instanceof Nothing) {
        return send(toNullable(Nothing.value));
      }
      ;
      if (req2.content instanceof Just) {
        var v = extractContent(req2.content.value0);
        if (v instanceof Right) {
          return send(toNullable(new Just(v.value0)));
        }
        ;
        if (v instanceof Left) {
          return pure13(new Left(new RequestContentError(v.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 199, column 7 - line 203, column 48): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Affjax (line 195, column 3 - line 203, column 48): " + [req2.content.constructor.name]);
    };
  };
  var printError = function(v) {
    if (v instanceof RequestContentError) {
      return "There was a problem with the request content: " + v.value0;
    }
    ;
    if (v instanceof ResponseBodyError) {
      return "There was a problem with the response body: " + renderForeignError(v.value0);
    }
    ;
    if (v instanceof TimeoutError) {
      return "There was a problem making the request: timeout";
    }
    ;
    if (v instanceof RequestFailedError) {
      return "There was a problem making the request: request failed";
    }
    ;
    if (v instanceof XHROtherError) {
      return "There was a problem making the request: " + message(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Affjax (line 113, column 14 - line 123, column 66): " + [v.constructor.name]);
  };
  var defaultRequest = /* @__PURE__ */ (function() {
    return {
      method: new Left(GET2.value),
      url: "/",
      headers: [],
      content: Nothing.value,
      username: Nothing.value,
      password: Nothing.value,
      withCredentials: false,
      responseFormat: ignore,
      timeout: Nothing.value
    };
  })();
  var get2 = function(driver2) {
    return function(rf) {
      return function(u2) {
        return request2(driver2)({
          method: defaultRequest.method,
          headers: defaultRequest.headers,
          content: defaultRequest.content,
          username: defaultRequest.username,
          password: defaultRequest.password,
          withCredentials: defaultRequest.withCredentials,
          timeout: defaultRequest.timeout,
          url: u2,
          responseFormat: rf
        });
      };
    };
  };

  // output/Affjax.Web/foreign.js
  var driver = {
    newXHR: function() {
      return new XMLHttpRequest();
    },
    fixupUrl: function(url2) {
      return url2 || "/";
    }
  };

  // output/Affjax.Web/index.js
  var get3 = /* @__PURE__ */ get2(driver);

  // output/V2.Components.SplitPane/foreign.js
  function highlightElement(element3) {
    return function() {
      if (typeof Prism !== "undefined" && Prism.highlightElement) {
        try {
          Prism.highlightElement(element3);
        } catch (e) {
          console.error("Prism highlighting error:", e);
        }
      }
    };
  }

  // output/V2.Components.Visualization/foreign.js
  function highlightElement2(element3) {
    return function() {
      if (typeof Prism !== "undefined" && Prism.highlightElement) {
        try {
          Prism.highlightElement(element3);
        } catch (e) {
          console.error("Prism highlighting error:", e);
        }
      }
    };
  }

  // output/D3.Data.Tree/index.js
  var TidyTree = /* @__PURE__ */ (function() {
    function TidyTree2() {
    }
    ;
    TidyTree2.value = new TidyTree2();
    return TidyTree2;
  })();
  var Dendrogram = /* @__PURE__ */ (function() {
    function Dendrogram2() {
    }
    ;
    Dendrogram2.value = new Dendrogram2();
    return Dendrogram2;
  })();
  var Radial = /* @__PURE__ */ (function() {
    function Radial2() {
    }
    ;
    Radial2.value = new Radial2();
    return Radial2;
  })();
  var Horizontal = /* @__PURE__ */ (function() {
    function Horizontal2() {
    }
    ;
    Horizontal2.value = new Horizontal2();
    return Horizontal2;
  })();
  var Vertical = /* @__PURE__ */ (function() {
    function Vertical2() {
    }
    ;
    Vertical2.value = new Vertical2();
    return Vertical2;
  })();
  var eqTreeLayout = {
    eq: function(x7) {
      return function(y6) {
        if (x7 instanceof Radial && y6 instanceof Radial) {
          return true;
        }
        ;
        if (x7 instanceof Horizontal && y6 instanceof Horizontal) {
          return true;
        }
        ;
        if (x7 instanceof Vertical && y6 instanceof Vertical) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/D3.Attributes.Instances/index.js
  var Static = /* @__PURE__ */ (function() {
    function Static2(value0) {
      this.value0 = value0;
    }
    ;
    Static2.create = function(value0) {
      return new Static2(value0);
    };
    return Static2;
  })();
  var Fn = /* @__PURE__ */ (function() {
    function Fn2(value0) {
      this.value0 = value0;
    }
    ;
    Fn2.create = function(value0) {
      return new Fn2(value0);
    };
    return Fn2;
  })();
  var FnI = /* @__PURE__ */ (function() {
    function FnI2(value0) {
      this.value0 = value0;
    }
    ;
    FnI2.create = function(value0) {
      return new FnI2(value0);
    };
    return FnI2;
  })();
  var StringAttr = /* @__PURE__ */ (function() {
    function StringAttr2(value0) {
      this.value0 = value0;
    }
    ;
    StringAttr2.create = function(value0) {
      return new StringAttr2(value0);
    };
    return StringAttr2;
  })();
  var NumberAttr = /* @__PURE__ */ (function() {
    function NumberAttr2(value0) {
      this.value0 = value0;
    }
    ;
    NumberAttr2.create = function(value0) {
      return new NumberAttr2(value0);
    };
    return NumberAttr2;
  })();
  var ArrayAttr = /* @__PURE__ */ (function() {
    function ArrayAttr2(value0) {
      this.value0 = value0;
    }
    ;
    ArrayAttr2.create = function(value0) {
      return new ArrayAttr2(value0);
    };
    return ArrayAttr2;
  })();
  var AttributeSetter = /* @__PURE__ */ (function() {
    function AttributeSetter2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AttributeSetter2.create = function(value0) {
      return function(value1) {
        return new AttributeSetter2(value0, value1);
      };
    };
    return AttributeSetter2;
  })();
  var unboxAttr = function(v) {
    if (v instanceof StringAttr && v.value0 instanceof Static) {
      return v.value0.value0;
    }
    ;
    if (v instanceof StringAttr && v.value0 instanceof Fn) {
      return v.value0.value0;
    }
    ;
    if (v instanceof StringAttr && v.value0 instanceof FnI) {
      return v.value0.value0;
    }
    ;
    if (v instanceof NumberAttr && v.value0 instanceof Static) {
      return v.value0.value0;
    }
    ;
    if (v instanceof NumberAttr && v.value0 instanceof Fn) {
      return v.value0.value0;
    }
    ;
    if (v instanceof NumberAttr && v.value0 instanceof FnI) {
      return v.value0.value0;
    }
    ;
    if (v instanceof ArrayAttr && v.value0 instanceof Static) {
      return v.value0.value0;
    }
    ;
    if (v instanceof ArrayAttr && v.value0 instanceof Fn) {
      return v.value0.value0;
    }
    ;
    if (v instanceof ArrayAttr && v.value0 instanceof FnI) {
      return v.value0.value0;
    }
    ;
    throw new Error("Failed pattern match at D3.Attributes.Instances (line 49, column 3 - line 60, column 46): " + [v.constructor.name]);
  };
  var toAttrStringFn = {
    toAttr: function($36) {
      return StringAttr.create(Fn.create($36));
    }
  };
  var toAttrString = {
    toAttr: function($37) {
      return StringAttr.create(Static.create($37));
    }
  };
  var toAttrNumberFnI = {
    toAttr: function($38) {
      return NumberAttr.create(FnI.create(mkFn2($38)));
    }
  };
  var toAttrNumberFn = {
    toAttr: function($39) {
      return NumberAttr.create(Fn.create($39));
    }
  };
  var toAttrNumber = {
    toAttr: function($40) {
      return NumberAttr.create(Static.create($40));
    }
  };
  var toAttr = function(dict) {
    return dict.toAttr;
  };

  // output/D3.Data.Types/index.js
  var MouseEnter = /* @__PURE__ */ (function() {
    function MouseEnter2() {
    }
    ;
    MouseEnter2.value = new MouseEnter2();
    return MouseEnter2;
  })();
  var MouseLeave = /* @__PURE__ */ (function() {
    function MouseLeave2() {
    }
    ;
    MouseLeave2.value = new MouseLeave2();
    return MouseLeave2;
  })();
  var MouseClick = /* @__PURE__ */ (function() {
    function MouseClick2() {
    }
    ;
    MouseClick2.value = new MouseClick2();
    return MouseClick2;
  })();
  var MouseDown = /* @__PURE__ */ (function() {
    function MouseDown2() {
    }
    ;
    MouseDown2.value = new MouseDown2();
    return MouseDown2;
  })();
  var MouseUp = /* @__PURE__ */ (function() {
    function MouseUp2() {
    }
    ;
    MouseUp2.value = new MouseUp2();
    return MouseUp2;
  })();
  var Div = /* @__PURE__ */ (function() {
    function Div2() {
    }
    ;
    Div2.value = new Div2();
    return Div2;
  })();
  var Svg = /* @__PURE__ */ (function() {
    function Svg2() {
    }
    ;
    Svg2.value = new Svg2();
    return Svg2;
  })();
  var Circle = /* @__PURE__ */ (function() {
    function Circle2() {
    }
    ;
    Circle2.value = new Circle2();
    return Circle2;
  })();
  var Line = /* @__PURE__ */ (function() {
    function Line2() {
    }
    ;
    Line2.value = new Line2();
    return Line2;
  })();
  var Group = /* @__PURE__ */ (function() {
    function Group2() {
    }
    ;
    Group2.value = new Group2();
    return Group2;
  })();
  var Text2 = /* @__PURE__ */ (function() {
    function Text3() {
    }
    ;
    Text3.value = new Text3();
    return Text3;
  })();
  var Path = /* @__PURE__ */ (function() {
    function Path3() {
    }
    ;
    Path3.value = new Path3();
    return Path3;
  })();
  var Rect = /* @__PURE__ */ (function() {
    function Rect2() {
    }
    ;
    Rect2.value = new Rect2();
    return Rect2;
  })();
  var DefaultCubic = /* @__PURE__ */ (function() {
    function DefaultCubic2() {
    }
    ;
    DefaultCubic2.value = new DefaultCubic2();
    return DefaultCubic2;
  })();
  var showMouseEvent = {
    show: function(v) {
      if (v instanceof MouseEnter) {
        return "mouseenter";
      }
      ;
      if (v instanceof MouseLeave) {
        return "mouseleave";
      }
      ;
      if (v instanceof MouseClick) {
        return "click";
      }
      ;
      if (v instanceof MouseDown) {
        return "mousedown";
      }
      ;
      if (v instanceof MouseUp) {
        return "mouseup";
      }
      ;
      throw new Error("Failed pattern match at D3.Data.Types (line 46, column 1 - line 51, column 30): " + [v.constructor.name]);
    }
  };
  var showElement = {
    show: function(v) {
      if (v instanceof Div) {
        return "div";
      }
      ;
      if (v instanceof Svg) {
        return "svg";
      }
      ;
      if (v instanceof Circle) {
        return "circle";
      }
      ;
      if (v instanceof Line) {
        return "line";
      }
      ;
      if (v instanceof Group) {
        return "g";
      }
      ;
      if (v instanceof Text2) {
        return "text";
      }
      ;
      if (v instanceof Path) {
        return "path";
      }
      ;
      if (v instanceof Rect) {
        return "rect";
      }
      ;
      throw new Error("Failed pattern match at D3.Data.Types (line 25, column 1 - line 33, column 23): " + [v.constructor.name]);
    }
  };

  // output/D3.FFI/foreign.js
  function d3Append_(element3) {
    return (selection2) => {
      return selection2.append(element3);
    };
  }
  function d3DataWithKeyFunction_(data) {
    return (keyFn) => (selection2) => {
      return selection2.data(data, keyFn);
    };
  }
  function d3EnterAndAppend_(element3) {
    return (selection2) => {
      return selection2.enter().append(element3);
    };
  }
  function d3GetExitSelection_(selection2) {
    return selection2.exit();
  }
  function d3GetEnterSelection_(selection2) {
    return selection2.enter();
  }
  function d3FilterSelection_(selection2) {
    return (selector) => selection2.filter(selector);
  }
  function d3LowerSelection_(selection2) {
    return selection2.lower();
  }
  function d3MergeSelectionWith_(enter) {
    return (update) => {
      return enter.merge(update);
    };
  }
  function d3OrderSelection_(selection2) {
    return selection2.order();
  }
  function d3RaiseSelection_(selection2) {
    return selection2.raise();
  }
  function d3RemoveSelection_(selection2) {
    return selection2.remove();
  }
  function d3SelectAllInDOM_(selector) {
    return d3.selectAll(selector);
  }
  function d3SelectionSelectAll_(selector) {
    return (selection2) => {
      return selection2.selectAll(selector);
    };
  }
  function d3SetAttr_(name16) {
    return (value13) => (selection2) => {
      return selection2.attr(name16, value13);
    };
  }
  function d3SetHTML_(value13) {
    return (selection2) => {
      return selection2.html(value13);
    };
  }
  function d3SetProperty_(value13) {
    return (selection2) => {
      return selection2.property(value13);
    };
  }
  function d3SetText_(value13) {
    return (selection2) => {
      return selection2.text(value13);
    };
  }
  function d3SortSelection_(selection2) {
    return (compare2) => selection2.sort(compare2);
  }
  function selectionOn_(selection2) {
    return (event) => (callback) => {
      return selection2.on(event, callback);
    };
  }
  function d3AddTransition_(selection2) {
    return (transition2) => {
      var handle;
      if (transition2.name == "") {
        handle = selection2.transition();
        if (transition2.duration != 0) {
          handle.duration(transition2.duration);
        }
        if (transition2.delay != 0) {
          handle.delay(transition2.delay);
        }
      } else {
        handle = selection2.transition(transition2.name);
      }
      return handle;
    };
  }
  function keyIsID_(d6) {
    return d6.id;
  }
  unpin = (d6) => {
    d6.fx = null;
    d6.fy = null;
    return d6;
  };
  getBaseForAssign = (newNodeMap, key) => {
    let newnode = newNodeMap.get(key);
    if (newnode) {
      var updatedCount;
      if (typeof newnode.updatedCount === "undefined") {
        updatedCount = 0;
      } else {
        updatedCount = newnode.updatedCount + 1;
      }
      return { fx: newnode.fx, fy: newnode.fy, updatedCount };
    } else {
      return d;
    }
  };
  function descendants_(tree2) {
    return tree2.descendants();
  }
  function getClusterLayoutFn_() {
    return d3.cluster();
  }
  function getTreeLayoutFn_() {
    return d3.tree();
  }
  function hasChildren_(d6) {
    return d6.children === "undefined" ? false : true;
  }
  function getHierarchyValue_(d6) {
    return d6.value === "undefined" ? null : d6.value;
  }
  function hierarchyFromJSON_(json) {
    return d3.hierarchy(json);
  }
  function hNodeDepth_(node) {
    return node.depth;
  }
  function hNodeHeight_(node) {
    return node.height;
  }
  function links_(tree2) {
    return tree2.links();
  }
  function runLayoutFn_(layout) {
    return (root2) => layout(root2);
  }
  function sharesParent_(a2) {
    return (b2) => a2.parent == b2.parent;
  }
  function treeSetNodeSize_(tree2) {
    return (widthHeight) => tree2.nodeSize(widthHeight);
  }
  function treeSetSeparation_(tree2) {
    return (separationFn) => tree2.separation(separationFn);
  }
  function treeSetSize_(tree2) {
    return (widthHeight) => tree2.size(widthHeight);
  }
  function treeMinMax_(root2) {
    let max_x = -Infinity;
    let min_x = Infinity;
    let max_y = -Infinity;
    let min_y = Infinity;
    root2.each((d6) => {
      if (d6.x > max_x) max_x = d6.x;
      if (d6.y > max_y) max_y = d6.y;
      if (d6.x < min_x) min_x = d6.x;
      if (d6.y < min_y) min_y = d6.y;
    });
    return { xMin: min_x, xMax: max_x, yMin: min_y, yMax: max_y };
  }
  var linkHorizontal_ = d3.linkHorizontal().x((d6) => d6.y).y((d6) => d6.x);
  var linkHorizontal2_ = d3.linkHorizontal().x((d6) => d6.x).y((d6) => d6.y);
  var linkVertical_ = d3.linkVertical().x((d6) => d6.x).y((d6) => d6.y);
  function linkClusterHorizontal_(levelSpacing) {
    return (d6) => `M${d6.target.y}, ${d6.target.x}
   C${d6.source.y + levelSpacing / 2},${d6.target.x}
   ${d6.source.y + levelSpacing / 2},${d6.source.x}
   ${d6.source.y},${d6.source.x}`;
  }
  function linkClusterVertical_(levelSpacing) {
    return (d6) => `M${d6.target.x}, ${d6.target.y}
   C${d6.target.x}, ${d6.source.y + levelSpacing / 2}
   ${d6.source.x},${d6.source.y + levelSpacing / 2}
   ${d6.source.x},${d6.source.y}`;
  }
  function linkRadial_(angleFn) {
    return (radiusFn) => d3.linkRadial().angle(angleFn).radius(radiusFn);
  }
  function chordLayout_(matrix) {
    return d3.chord()(matrix);
  }
  function chordGroups_(chordLayout) {
    return chordLayout.groups;
  }
  function chordArray_(chordLayout) {
    return Array.from(chordLayout);
  }
  function ribbonGenerator_() {
    return d3.ribbon();
  }
  function arcGenerator_() {
    return d3.arc();
  }
  function ribbonPath_(generator) {
    return (chord) => generator(chord);
  }
  function arcPath_(generator) {
    return (group3) => generator(group3);
  }
  function setRibbonRadius_(generator) {
    return (radius5) => {
      generator.radius(radius5);
      return generator;
    };
  }
  function setArcInnerRadius_(generator) {
    return (radius5) => {
      generator.innerRadius(radius5);
      return generator;
    };
  }
  function setArcOuterRadius_(generator) {
    return (radius5) => {
      generator.outerRadius(radius5);
      return generator;
    };
  }
  function d3AttachZoomDefaultExtent_(selection2) {
    return (config) => {
      function zoomed({ transform: transform3 }) {
        config.target.attr("transform", transform3);
      }
      return selection2.call(
        d3.zoom().scaleExtent(config.scaleExtent).on(`zoom.${config.name}`, zoomed)
      );
    };
  }
  function d3AttachZoom_(selection2) {
    return (config) => {
      selection2.call(
        d3.zoom().extent(config.extent).scaleExtent(config.scaleExtent).on(`zoom.${config.name}`, (event) => {
          config.target.attr("transform", event.transform);
        })
      );
      return selection2;
    };
  }

  // output/D3.FFI/index.js
  var getLayout = function(layout) {
    if (layout instanceof TidyTree) {
      return getTreeLayoutFn_(unit);
    }
    ;
    if (layout instanceof Dendrogram) {
      return getClusterLayoutFn_(unit);
    }
    ;
    throw new Error("Failed pattern match at D3.FFI (line 260, column 3 - line 262, column 43): " + [layout.constructor.name]);
  };

  // output/D3.Selection/index.js
  var show3 = /* @__PURE__ */ show(showMouseEvent);
  var Order = /* @__PURE__ */ (function() {
    function Order2() {
    }
    ;
    Order2.value = new Order2();
    return Order2;
  })();
  var Sort = /* @__PURE__ */ (function() {
    function Sort2(value0) {
      this.value0 = value0;
    }
    ;
    Sort2.create = function(value0) {
      return new Sort2(value0);
    };
    return Sort2;
  })();
  var Raise2 = /* @__PURE__ */ (function() {
    function Raise3() {
    }
    ;
    Raise3.value = new Raise3();
    return Raise3;
  })();
  var Lower = /* @__PURE__ */ (function() {
    function Lower2() {
    }
    ;
    Lower2.value = new Lower2();
    return Lower2;
  })();
  var AttrT = /* @__PURE__ */ (function() {
    function AttrT2(value0) {
      this.value0 = value0;
    }
    ;
    AttrT2.create = function(value0) {
      return new AttrT2(value0);
    };
    return AttrT2;
  })();
  var TextT = /* @__PURE__ */ (function() {
    function TextT2(value0) {
      this.value0 = value0;
    }
    ;
    TextT2.create = function(value0) {
      return new TextT2(value0);
    };
    return TextT2;
  })();
  var HTMLT = /* @__PURE__ */ (function() {
    function HTMLT2(value0) {
      this.value0 = value0;
    }
    ;
    HTMLT2.create = function(value0) {
      return new HTMLT2(value0);
    };
    return HTMLT2;
  })();
  var PropertyT = /* @__PURE__ */ (function() {
    function PropertyT2(value0) {
      this.value0 = value0;
    }
    ;
    PropertyT2.create = function(value0) {
      return new PropertyT2(value0);
    };
    return PropertyT2;
  })();
  var OrderingT = /* @__PURE__ */ (function() {
    function OrderingT2(value0) {
      this.value0 = value0;
    }
    ;
    OrderingT2.create = function(value0) {
      return new OrderingT2(value0);
    };
    return OrderingT2;
  })();
  var TransitionT = /* @__PURE__ */ (function() {
    function TransitionT2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TransitionT2.create = function(value0) {
      return function(value1) {
        return new TransitionT2(value0, value1);
      };
    };
    return TransitionT2;
  })();
  var RemoveT = /* @__PURE__ */ (function() {
    function RemoveT2() {
    }
    ;
    RemoveT2.value = new RemoveT2();
    return RemoveT2;
  })();
  var OnT = /* @__PURE__ */ (function() {
    function OnT2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    OnT2.create = function(value0) {
      return function(value1) {
        return new OnT2(value0, value1);
      };
    };
    return OnT2;
  })();
  var OnT$prime = /* @__PURE__ */ (function() {
    function OnT$prime2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    OnT$prime2.create = function(value0) {
      return function(value1) {
        return new OnT$prime2(value0, value1);
      };
    };
    return OnT$prime2;
  })();
  var Drag = /* @__PURE__ */ (function() {
    function Drag2(value0) {
      this.value0 = value0;
    }
    ;
    Drag2.create = function(value0) {
      return new Drag2(value0);
    };
    return Drag2;
  })();
  var Zoom = /* @__PURE__ */ (function() {
    function Zoom2(value0) {
      this.value0 = value0;
    }
    ;
    Zoom2.create = function(value0) {
      return new Zoom2(value0);
    };
    return Zoom2;
  })();
  var showOrderingAttribute = {
    show: function(v) {
      if (v instanceof Order) {
        return "Order";
      }
      ;
      if (v instanceof Raise2) {
        return "Raise";
      }
      ;
      if (v instanceof Lower) {
        return "Lower";
      }
      ;
      if (v instanceof Sort) {
        return "Sort";
      }
      ;
      throw new Error("Failed pattern match at D3.Selection (line 71, column 1 - line 75, column 25): " + [v.constructor.name]);
    }
  };
  var applySelectionAttributeD3 = function(v) {
    return function(v1) {
      if (v1 instanceof AttrT) {
        return d3SetAttr_(v1.value0.value0)(unboxAttr(v1.value0.value1))(v);
      }
      ;
      if (v1 instanceof TextT) {
        return d3SetText_(unboxAttr(v1.value0.value1))(v);
      }
      ;
      if (v1 instanceof PropertyT) {
        return d3SetProperty_(unboxAttr(v1.value0.value1))(v);
      }
      ;
      if (v1 instanceof HTMLT) {
        return d3SetHTML_(unboxAttr(v1.value0.value1))(v);
      }
      ;
      if (v1 instanceof RemoveT) {
        var removed_ = d3RemoveSelection_(v);
        return removed_;
      }
      ;
      if (v1 instanceof TransitionT) {
        var tHandler = d3AddTransition_(v)(v1.value1);
        var v2 = foldl2(applySelectionAttributeD3)(tHandler)(v1.value0);
        return v;
      }
      ;
      if (v1 instanceof OnT) {
        return selectionOn_(v)(show3(v1.value0))(v1.value1);
      }
      ;
      if (v1 instanceof OnT$prime) {
        return selectionOn_(v)(show3(v1.value0))(v1.value1);
      }
      ;
      if (v1 instanceof OrderingT) {
        if (v1.value0 instanceof Order) {
          return d3OrderSelection_(v);
        }
        ;
        if (v1.value0 instanceof Sort) {
          return d3SortSelection_(v)(v1.value0.value0);
        }
        ;
        if (v1.value0 instanceof Raise2) {
          return d3RaiseSelection_(v);
        }
        ;
        if (v1.value0 instanceof Lower) {
          return d3LowerSelection_(v);
        }
        ;
        throw new Error("Failed pattern match at D3.Selection (line 108, column 3 - line 112, column 51): " + [v1.value0.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at D3.Selection (line 78, column 1 - line 78, column 80): " + [v.constructor.name, v1.constructor.name]);
    };
  };

  // output/D3.Attributes.Sugar/index.js
  var toAttr2 = /* @__PURE__ */ toAttr(toAttrString);
  var intercalate4 = /* @__PURE__ */ intercalate3(monoidString);
  var map23 = /* @__PURE__ */ map(functorArray);
  var show4 = /* @__PURE__ */ show(showNumber);
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var flap2 = /* @__PURE__ */ flap(functorArray);
  var Meet = /* @__PURE__ */ (function() {
    function Meet2() {
    }
    ;
    Meet2.value = new Meet2();
    return Meet2;
  })();
  var Slice = /* @__PURE__ */ (function() {
    function Slice2() {
    }
    ;
    Slice2.value = new Slice2();
    return Slice2;
  })();
  var None2 = /* @__PURE__ */ (function() {
    function None3() {
    }
    ;
    None3.value = new None3();
    return None3;
  })();
  var YMin = /* @__PURE__ */ (function() {
    function YMin2() {
    }
    ;
    YMin2.value = new YMin2();
    return YMin2;
  })();
  var YMid = /* @__PURE__ */ (function() {
    function YMid2() {
    }
    ;
    YMid2.value = new YMid2();
    return YMid2;
  })();
  var YMax = /* @__PURE__ */ (function() {
    function YMax2() {
    }
    ;
    YMax2.value = new YMax2();
    return YMax2;
  })();
  var XMin = /* @__PURE__ */ (function() {
    function XMin2() {
    }
    ;
    XMin2.value = new XMin2();
    return XMin2;
  })();
  var XMid = /* @__PURE__ */ (function() {
    function XMid2() {
    }
    ;
    XMid2.value = new XMid2();
    return XMid2;
  })();
  var XMax = /* @__PURE__ */ (function() {
    function XMax2() {
    }
    ;
    XMax2.value = new XMax2();
    return XMax2;
  })();
  var AspectRatio = /* @__PURE__ */ (function() {
    function AspectRatio2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    AspectRatio2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new AspectRatio2(value0, value1, value22);
        };
      };
    };
    return AspectRatio2;
  })();
  var showAspectRatioPreserve = {
    show: function(v) {
      if (v instanceof Meet) {
        return "meet";
      }
      ;
      if (v instanceof Slice) {
        return "slice";
      }
      ;
      if (v instanceof None2) {
        return "none";
      }
      ;
      throw new Error("Failed pattern match at D3.Attributes.Sugar (line 63, column 1 - line 66, column 22): " + [v.constructor.name]);
    }
  };
  var show12 = /* @__PURE__ */ show(showAspectRatioPreserve);
  var showAlignAspectRatio_Y = {
    show: function(v) {
      if (v instanceof YMin) {
        return "YMin";
      }
      ;
      if (v instanceof YMid) {
        return "YMid";
      }
      ;
      if (v instanceof YMax) {
        return "YMax";
      }
      ;
      throw new Error("Failed pattern match at D3.Attributes.Sugar (line 58, column 1 - line 61, column 21): " + [v.constructor.name]);
    }
  };
  var show22 = /* @__PURE__ */ show(showAlignAspectRatio_Y);
  var showAlignAspectRatio_X = {
    show: function(v) {
      if (v instanceof XMin) {
        return "xMin";
      }
      ;
      if (v instanceof XMid) {
        return "xMid";
      }
      ;
      if (v instanceof XMax) {
        return "xMax";
      }
      ;
      throw new Error("Failed pattern match at D3.Attributes.Sugar (line 53, column 1 - line 56, column 21): " + [v.constructor.name]);
    }
  };
  var show32 = /* @__PURE__ */ show(showAlignAspectRatio_X);
  var showAspectRatioSpec = {
    show: function(v) {
      if (v.value2 instanceof None2) {
        return "none";
      }
      ;
      return show32(v.value0) + (show22(v.value1) + (" " + show12(v.value2)));
    }
  };
  var y = function(dictToAttr) {
    var $115 = AttributeSetter.create("y");
    var $116 = toAttr(dictToAttr);
    return function($117) {
      return AttrT.create($115($116($117)));
    };
  };
  var x = function(dictToAttr) {
    var $124 = AttributeSetter.create("x");
    var $125 = toAttr(dictToAttr);
    return function($126) {
      return AttrT.create($124($125($126)));
    };
  };
  var width8 = function(dictToAttr) {
    var $127 = AttributeSetter.create("width");
    var $128 = toAttr(dictToAttr);
    return function($129) {
      return AttrT.create($127($128($129)));
    };
  };
  var viewBox = function(xo) {
    return function(yo) {
      return function(w) {
        return function(h) {
          var vb = intercalate4(" ")(map23(show4)([xo, yo, w, h]));
          return AttrT.create(AttributeSetter.create("viewBox")(toAttr2(vb)));
        };
      };
    };
  };
  var transform$prime = /* @__PURE__ */ (function() {
    var $130 = AttributeSetter.create("transform");
    return function($131) {
      return AttrT.create($130(StringAttr.create(Fn.create($131))));
    };
  })();
  var to = function(v) {
    return function(v1) {
      if (v instanceof TransitionT && v.value0.length === 0) {
        return [new TransitionT(v1, v.value1)];
      }
      ;
      if (v instanceof TransitionT) {
        return [new TransitionT(append12(v.value0)(v1), v.value1)];
      }
      ;
      return cons3(v)(v1);
    };
  };
  var textAnchor = function(dictToAttr) {
    var $132 = AttributeSetter.create("text-anchor");
    var $133 = toAttr(dictToAttr);
    return function($134) {
      return AttrT.create($132($133($134)));
    };
  };
  var text6 = function(dictToAttr) {
    var $135 = AttributeSetter.create("text");
    var $136 = toAttr(dictToAttr);
    return function($137) {
      return TextT.create($135($136($137)));
    };
  };
  var strokeWidth = function(dictToAttr) {
    var $138 = AttributeSetter.create("stroke-width");
    var $139 = toAttr(dictToAttr);
    return function($140) {
      return AttrT.create($138($139($140)));
    };
  };
  var strokeOpacity = function(dictToAttr) {
    var $141 = AttributeSetter.create("stroke-opacity");
    var $142 = toAttr(dictToAttr);
    return function($143) {
      return AttrT.create($141($142($143)));
    };
  };
  var strokeColor = function(dictToAttr) {
    var $144 = AttributeSetter.create("stroke");
    var $145 = toAttr(dictToAttr);
    return function($146) {
      return AttrT.create($144($145($146)));
    };
  };
  var remove = /* @__PURE__ */ (function() {
    return RemoveT.value;
  })();
  var radius = function(dictToAttr) {
    var $156 = AttributeSetter.create("r");
    var $157 = toAttr(dictToAttr);
    return function($158) {
      return AttrT.create($156($157($158)));
    };
  };
  var preserveAspectRatio = /* @__PURE__ */ (function() {
    var $159 = AttributeSetter.create("preserveAspectRatio");
    var $160 = show(showAspectRatioSpec);
    return function($161) {
      return AttrT.create($159(toAttr2($160($161))));
    };
  })();
  var height8 = function(dictToAttr) {
    var $171 = AttributeSetter.create("height");
    var $172 = toAttr(dictToAttr);
    return function($173) {
      return AttrT.create($171($172($173)));
    };
  };
  var fontSize = function(dictToAttr) {
    var $174 = AttributeSetter.create("font-size");
    var $175 = toAttr(dictToAttr);
    return function($176) {
      return AttrT.create($174($175($176)));
    };
  };
  var fontFamily = function(dictToAttr) {
    var $177 = AttributeSetter.create("font-family");
    var $178 = toAttr(dictToAttr);
    return function($179) {
      return AttrT.create($177($178($179)));
    };
  };
  var fillOpacity = function(dictToAttr) {
    var $180 = AttributeSetter.create("fill-opacity");
    var $181 = toAttr(dictToAttr);
    return function($182) {
      return AttrT.create($180($181($182)));
    };
  };
  var fill = function(dictToAttr) {
    var $183 = AttributeSetter.create("fill");
    var $184 = toAttr(dictToAttr);
    return function($185) {
      return AttrT.create($183($184($185)));
    };
  };
  var dy = function(dictToAttr) {
    var $186 = AttributeSetter.create("dy");
    var $187 = toAttr(dictToAttr);
    return function($188) {
      return AttrT.create($186($187($188)));
    };
  };
  var defaultTransition = /* @__PURE__ */ (function() {
    return {
      name: "",
      delay: 0,
      duration: 0,
      easing: DefaultCubic.value
    };
  })();
  var transitionWithDuration = function(duration2) {
    return new TransitionT([], {
      name: defaultTransition.name,
      delay: defaultTransition.delay,
      easing: defaultTransition.easing,
      duration: duration2
    });
  };
  var d2 = function(dictToAttr) {
    var $192 = AttributeSetter.create("d");
    var $193 = toAttr(dictToAttr);
    return function($194) {
      return AttrT.create($192($193($194)));
    };
  };
  var cy = function(dictToAttr) {
    var $195 = AttributeSetter.create("cy");
    var $196 = toAttr(dictToAttr);
    return function($197) {
      return AttrT.create($195($196($197)));
    };
  };
  var cx = function(dictToAttr) {
    var $198 = AttributeSetter.create("cx");
    var $199 = toAttr(dictToAttr);
    return function($200) {
      return AttrT.create($198($199($200)));
    };
  };
  var classed = function(dictToAttr) {
    var $204 = AttributeSetter.create("class");
    var $205 = toAttr(dictToAttr);
    return function($206) {
      return AttrT.create($204($205($206)));
    };
  };
  var assembleTransforms = function(fs) {
    return function(d1) {
      return intercalate4(" ")(flap2(fs)(d1));
    };
  };
  var transform = function($210) {
    return transform$prime(assembleTransforms($210));
  };
  var andThen = function(dictSemigroup) {
    return append(dictSemigroup);
  };

  // node_modules/d3-array/src/ascending.js
  function ascending(a2, b2) {
    return a2 == null || b2 == null ? NaN : a2 < b2 ? -1 : a2 > b2 ? 1 : a2 >= b2 ? 0 : NaN;
  }

  // node_modules/d3-array/src/descending.js
  function descending(a2, b2) {
    return a2 == null || b2 == null ? NaN : b2 < a2 ? -1 : b2 > a2 ? 1 : b2 >= a2 ? 0 : NaN;
  }

  // node_modules/d3-array/src/bisector.js
  function bisector(f) {
    let compare12, compare2, delta;
    if (f.length !== 2) {
      compare12 = ascending;
      compare2 = (d6, x7) => ascending(f(d6), x7);
      delta = (d6, x7) => f(d6) - x7;
    } else {
      compare12 = f === ascending || f === descending ? f : zero2;
      compare2 = f;
      delta = f;
    }
    function left2(a2, x7, lo = 0, hi = a2.length) {
      if (lo < hi) {
        if (compare12(x7, x7) !== 0) return hi;
        do {
          const mid = lo + hi >>> 1;
          if (compare2(a2[mid], x7) < 0) lo = mid + 1;
          else hi = mid;
        } while (lo < hi);
      }
      return lo;
    }
    function right2(a2, x7, lo = 0, hi = a2.length) {
      if (lo < hi) {
        if (compare12(x7, x7) !== 0) return hi;
        do {
          const mid = lo + hi >>> 1;
          if (compare2(a2[mid], x7) <= 0) lo = mid + 1;
          else hi = mid;
        } while (lo < hi);
      }
      return lo;
    }
    function center2(a2, x7, lo = 0, hi = a2.length) {
      const i2 = left2(a2, x7, lo, hi - 1);
      return i2 > lo && delta(a2[i2 - 1], x7) > -delta(a2[i2], x7) ? i2 - 1 : i2;
    }
    return { left: left2, center: center2, right: right2 };
  }
  function zero2() {
    return 0;
  }

  // node_modules/d3-array/src/number.js
  function number(x7) {
    return x7 === null ? NaN : +x7;
  }

  // node_modules/d3-array/src/bisect.js
  var ascendingBisect = bisector(ascending);
  var bisectRight = ascendingBisect.right;
  var bisectLeft = ascendingBisect.left;
  var bisectCenter = bisector(number).center;
  var bisect_default = bisectRight;

  // node_modules/d3-array/src/ticks.js
  var e10 = Math.sqrt(50);
  var e5 = Math.sqrt(10);
  var e2 = Math.sqrt(2);
  function tickSpec(start3, stop, count) {
    const step4 = (stop - start3) / Math.max(0, count), power = Math.floor(Math.log10(step4)), error4 = step4 / Math.pow(10, power), factor = error4 >= e10 ? 10 : error4 >= e5 ? 5 : error4 >= e2 ? 2 : 1;
    let i1, i2, inc;
    if (power < 0) {
      inc = Math.pow(10, -power) / factor;
      i1 = Math.round(start3 * inc);
      i2 = Math.round(stop * inc);
      if (i1 / inc < start3) ++i1;
      if (i2 / inc > stop) --i2;
      inc = -inc;
    } else {
      inc = Math.pow(10, power) * factor;
      i1 = Math.round(start3 / inc);
      i2 = Math.round(stop / inc);
      if (i1 * inc < start3) ++i1;
      if (i2 * inc > stop) --i2;
    }
    if (i2 < i1 && 0.5 <= count && count < 2) return tickSpec(start3, stop, count * 2);
    return [i1, i2, inc];
  }
  function ticks(start3, stop, count) {
    stop = +stop, start3 = +start3, count = +count;
    if (!(count > 0)) return [];
    if (start3 === stop) return [start3];
    const reverse3 = stop < start3, [i1, i2, inc] = reverse3 ? tickSpec(stop, start3, count) : tickSpec(start3, stop, count);
    if (!(i2 >= i1)) return [];
    const n = i2 - i1 + 1, ticks2 = new Array(n);
    if (reverse3) {
      if (inc < 0) for (let i3 = 0; i3 < n; ++i3) ticks2[i3] = (i2 - i3) / -inc;
      else for (let i3 = 0; i3 < n; ++i3) ticks2[i3] = (i2 - i3) * inc;
    } else {
      if (inc < 0) for (let i3 = 0; i3 < n; ++i3) ticks2[i3] = (i1 + i3) / -inc;
      else for (let i3 = 0; i3 < n; ++i3) ticks2[i3] = (i1 + i3) * inc;
    }
    return ticks2;
  }
  function tickIncrement(start3, stop, count) {
    stop = +stop, start3 = +start3, count = +count;
    return tickSpec(start3, stop, count)[2];
  }
  function tickStep(start3, stop, count) {
    stop = +stop, start3 = +start3, count = +count;
    const reverse3 = stop < start3, inc = reverse3 ? tickIncrement(stop, start3, count) : tickIncrement(start3, stop, count);
    return (reverse3 ? -1 : 1) * (inc < 0 ? 1 / -inc : inc);
  }

  // node_modules/d3-axis/src/identity.js
  function identity_default(x7) {
    return x7;
  }

  // node_modules/d3-axis/src/axis.js
  var top3 = 1;
  var right = 2;
  var bottom3 = 3;
  var left = 4;
  var epsilon = 1e-6;
  function translateX(x7) {
    return "translate(" + x7 + ",0)";
  }
  function translateY(y6) {
    return "translate(0," + y6 + ")";
  }
  function number2(scale) {
    return (d6) => +scale(d6);
  }
  function center(scale, offset) {
    offset = Math.max(0, scale.bandwidth() - offset * 2) / 2;
    if (scale.round()) offset = Math.round(offset);
    return (d6) => +scale(d6) + offset;
  }
  function entering() {
    return !this.__axis;
  }
  function axis(orient, scale) {
    var tickArguments = [], tickValues = null, tickFormat2 = null, tickSizeInner = 6, tickSizeOuter = 6, tickPadding = 3, offset = typeof window !== "undefined" && window.devicePixelRatio > 1 ? 0 : 0.5, k = orient === top3 || orient === left ? -1 : 1, x7 = orient === left || orient === right ? "x" : "y", transform3 = orient === top3 || orient === bottom3 ? translateX : translateY;
    function axis2(context) {
      var values = tickValues == null ? scale.ticks ? scale.ticks.apply(scale, tickArguments) : scale.domain() : tickValues, format2 = tickFormat2 == null ? scale.tickFormat ? scale.tickFormat.apply(scale, tickArguments) : identity_default : tickFormat2, spacing = Math.max(tickSizeInner, 0) + tickPadding, range3 = scale.range(), range0 = +range3[0] + offset, range1 = +range3[range3.length - 1] + offset, position2 = (scale.bandwidth ? center : number2)(scale.copy(), offset), selection2 = context.selection ? context.selection() : context, path2 = selection2.selectAll(".domain").data([null]), tick = selection2.selectAll(".tick").data(values, scale).order(), tickExit = tick.exit(), tickEnter = tick.enter().append("g").attr("class", "tick"), line = tick.select("line"), text10 = tick.select("text");
      path2 = path2.merge(path2.enter().insert("path", ".tick").attr("class", "domain").attr("stroke", "currentColor"));
      tick = tick.merge(tickEnter);
      line = line.merge(tickEnter.append("line").attr("stroke", "currentColor").attr(x7 + "2", k * tickSizeInner));
      text10 = text10.merge(tickEnter.append("text").attr("fill", "currentColor").attr(x7, k * spacing).attr("dy", orient === top3 ? "0em" : orient === bottom3 ? "0.71em" : "0.32em"));
      if (context !== selection2) {
        path2 = path2.transition(context);
        tick = tick.transition(context);
        line = line.transition(context);
        text10 = text10.transition(context);
        tickExit = tickExit.transition(context).attr("opacity", epsilon).attr("transform", function(d6) {
          return isFinite(d6 = position2(d6)) ? transform3(d6 + offset) : this.getAttribute("transform");
        });
        tickEnter.attr("opacity", epsilon).attr("transform", function(d6) {
          var p2 = this.parentNode.__axis;
          return transform3((p2 && isFinite(p2 = p2(d6)) ? p2 : position2(d6)) + offset);
        });
      }
      tickExit.remove();
      path2.attr("d", orient === left || orient === right ? tickSizeOuter ? "M" + k * tickSizeOuter + "," + range0 + "H" + offset + "V" + range1 + "H" + k * tickSizeOuter : "M" + offset + "," + range0 + "V" + range1 : tickSizeOuter ? "M" + range0 + "," + k * tickSizeOuter + "V" + offset + "H" + range1 + "V" + k * tickSizeOuter : "M" + range0 + "," + offset + "H" + range1);
      tick.attr("opacity", 1).attr("transform", function(d6) {
        return transform3(position2(d6) + offset);
      });
      line.attr(x7 + "2", k * tickSizeInner);
      text10.attr(x7, k * spacing).text(format2);
      selection2.filter(entering).attr("fill", "none").attr("font-size", 10).attr("font-family", "sans-serif").attr("text-anchor", orient === right ? "start" : orient === left ? "end" : "middle");
      selection2.each(function() {
        this.__axis = position2;
      });
    }
    axis2.scale = function(_) {
      return arguments.length ? (scale = _, axis2) : scale;
    };
    axis2.ticks = function() {
      return tickArguments = Array.from(arguments), axis2;
    };
    axis2.tickArguments = function(_) {
      return arguments.length ? (tickArguments = _ == null ? [] : Array.from(_), axis2) : tickArguments.slice();
    };
    axis2.tickValues = function(_) {
      return arguments.length ? (tickValues = _ == null ? null : Array.from(_), axis2) : tickValues && tickValues.slice();
    };
    axis2.tickFormat = function(_) {
      return arguments.length ? (tickFormat2 = _, axis2) : tickFormat2;
    };
    axis2.tickSize = function(_) {
      return arguments.length ? (tickSizeInner = tickSizeOuter = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeInner = function(_) {
      return arguments.length ? (tickSizeInner = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeOuter = function(_) {
      return arguments.length ? (tickSizeOuter = +_, axis2) : tickSizeOuter;
    };
    axis2.tickPadding = function(_) {
      return arguments.length ? (tickPadding = +_, axis2) : tickPadding;
    };
    axis2.offset = function(_) {
      return arguments.length ? (offset = +_, axis2) : offset;
    };
    return axis2;
  }
  function axisBottom(scale) {
    return axis(bottom3, scale);
  }
  function axisLeft(scale) {
    return axis(left, scale);
  }

  // node_modules/d3-dispatch/src/dispatch.js
  var noop = { value: () => {
  } };
  function dispatch() {
    for (var i2 = 0, n = arguments.length, _ = {}, t; i2 < n; ++i2) {
      if (!(t = arguments[i2] + "") || t in _ || /[\s.]/.test(t)) throw new Error("illegal type: " + t);
      _[t] = [];
    }
    return new Dispatch(_);
  }
  function Dispatch(_) {
    this._ = _;
  }
  function parseTypenames(typenames, types) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name16 = "", i2 = t.indexOf(".");
      if (i2 >= 0) name16 = t.slice(i2 + 1), t = t.slice(0, i2);
      if (t && !types.hasOwnProperty(t)) throw new Error("unknown type: " + t);
      return { type: t, name: name16 };
    });
  }
  Dispatch.prototype = dispatch.prototype = {
    constructor: Dispatch,
    on: function(typename, callback) {
      var _ = this._, T = parseTypenames(typename + "", _), t, i2 = -1, n = T.length;
      if (arguments.length < 2) {
        while (++i2 < n) if ((t = (typename = T[i2]).type) && (t = get4(_[t], typename.name))) return t;
        return;
      }
      if (callback != null && typeof callback !== "function") throw new Error("invalid callback: " + callback);
      while (++i2 < n) {
        if (t = (typename = T[i2]).type) _[t] = set(_[t], typename.name, callback);
        else if (callback == null) for (t in _) _[t] = set(_[t], typename.name, null);
      }
      return this;
    },
    copy: function() {
      var copy3 = {}, _ = this._;
      for (var t in _) copy3[t] = _[t].slice();
      return new Dispatch(copy3);
    },
    call: function(type2, that) {
      if ((n = arguments.length - 2) > 0) for (var args = new Array(n), i2 = 0, n, t; i2 < n; ++i2) args[i2] = arguments[i2 + 2];
      if (!this._.hasOwnProperty(type2)) throw new Error("unknown type: " + type2);
      for (t = this._[type2], i2 = 0, n = t.length; i2 < n; ++i2) t[i2].value.apply(that, args);
    },
    apply: function(type2, that, args) {
      if (!this._.hasOwnProperty(type2)) throw new Error("unknown type: " + type2);
      for (var t = this._[type2], i2 = 0, n = t.length; i2 < n; ++i2) t[i2].value.apply(that, args);
    }
  };
  function get4(type2, name16) {
    for (var i2 = 0, n = type2.length, c; i2 < n; ++i2) {
      if ((c = type2[i2]).name === name16) {
        return c.value;
      }
    }
  }
  function set(type2, name16, callback) {
    for (var i2 = 0, n = type2.length; i2 < n; ++i2) {
      if (type2[i2].name === name16) {
        type2[i2] = noop, type2 = type2.slice(0, i2).concat(type2.slice(i2 + 1));
        break;
      }
    }
    if (callback != null) type2.push({ name: name16, value: callback });
    return type2;
  }
  var dispatch_default = dispatch;

  // node_modules/d3-selection/src/namespaces.js
  var xhtml = "http://www.w3.org/1999/xhtml";
  var namespaces_default = {
    svg: "http://www.w3.org/2000/svg",
    xhtml,
    xlink: "http://www.w3.org/1999/xlink",
    xml: "http://www.w3.org/XML/1998/namespace",
    xmlns: "http://www.w3.org/2000/xmlns/"
  };

  // node_modules/d3-selection/src/namespace.js
  function namespace_default(name16) {
    var prefix = name16 += "", i2 = prefix.indexOf(":");
    if (i2 >= 0 && (prefix = name16.slice(0, i2)) !== "xmlns") name16 = name16.slice(i2 + 1);
    return namespaces_default.hasOwnProperty(prefix) ? { space: namespaces_default[prefix], local: name16 } : name16;
  }

  // node_modules/d3-selection/src/creator.js
  function creatorInherit(name16) {
    return function() {
      var document3 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml && document3.documentElement.namespaceURI === xhtml ? document3.createElement(name16) : document3.createElementNS(uri, name16);
    };
  }
  function creatorFixed(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default(name16) {
    var fullname = namespace_default(name16);
    return (fullname.local ? creatorFixed : creatorInherit)(fullname);
  }

  // node_modules/d3-selection/src/selector.js
  function none2() {
  }
  function selector_default(selector) {
    return selector == null ? none2 : function() {
      return this.querySelector(selector);
    };
  }

  // node_modules/d3-selection/src/selection/select.js
  function select_default(select5) {
    if (typeof select5 !== "function") select5 = selector_default(select5);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = new Array(n), node, subnode, i2 = 0; i2 < n; ++i2) {
        if ((node = group3[i2]) && (subnode = select5.call(node, node.__data__, i2, group3))) {
          if ("__data__" in node) subnode.__data__ = node.__data__;
          subgroup[i2] = subnode;
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/array.js
  function array(x7) {
    return x7 == null ? [] : Array.isArray(x7) ? x7 : Array.from(x7);
  }

  // node_modules/d3-selection/src/selectorAll.js
  function empty7() {
    return [];
  }
  function selectorAll_default(selector) {
    return selector == null ? empty7 : function() {
      return this.querySelectorAll(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectAll.js
  function arrayAll(select5) {
    return function() {
      return array(select5.apply(this, arguments));
    };
  }
  function selectAll_default(select5) {
    if (typeof select5 === "function") select5 = arrayAll(select5);
    else select5 = selectorAll_default(select5);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          subgroups.push(select5.call(node, node.__data__, i2, group3));
          parents.push(node);
        }
      }
    }
    return new Selection(subgroups, parents);
  }

  // node_modules/d3-selection/src/matcher.js
  function matcher_default(selector) {
    return function() {
      return this.matches(selector);
    };
  }
  function childMatcher(selector) {
    return function(node) {
      return node.matches(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectChild.js
  var find3 = Array.prototype.find;
  function childFind(match) {
    return function() {
      return find3.call(this.children, match);
    };
  }
  function childFirst() {
    return this.firstElementChild;
  }
  function selectChild_default(match) {
    return this.select(match == null ? childFirst : childFind(typeof match === "function" ? match : childMatcher(match)));
  }

  // node_modules/d3-selection/src/selection/selectChildren.js
  var filter4 = Array.prototype.filter;
  function children2() {
    return Array.from(this.children);
  }
  function childrenFilter(match) {
    return function() {
      return filter4.call(this.children, match);
    };
  }
  function selectChildren_default(match) {
    return this.selectAll(match == null ? children2 : childrenFilter(typeof match === "function" ? match : childMatcher(match)));
  }

  // node_modules/d3-selection/src/selection/filter.js
  function filter_default(match) {
    if (typeof match !== "function") match = matcher_default(match);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = [], node, i2 = 0; i2 < n; ++i2) {
        if ((node = group3[i2]) && match.call(node, node.__data__, i2, group3)) {
          subgroup.push(node);
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/selection/sparse.js
  function sparse_default(update) {
    return new Array(update.length);
  }

  // node_modules/d3-selection/src/selection/enter.js
  function enter_default() {
    return new Selection(this._enter || this._groups.map(sparse_default), this._parents);
  }
  function EnterNode(parent2, datum2) {
    this.ownerDocument = parent2.ownerDocument;
    this.namespaceURI = parent2.namespaceURI;
    this._next = null;
    this._parent = parent2;
    this.__data__ = datum2;
  }
  EnterNode.prototype = {
    constructor: EnterNode,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next) {
      return this._parent.insertBefore(child, next);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-selection/src/constant.js
  function constant_default(x7) {
    return function() {
      return x7;
    };
  }

  // node_modules/d3-selection/src/selection/data.js
  function bindIndex(parent2, group3, enter, update, exit, data) {
    var i2 = 0, node, groupLength = group3.length, dataLength = data.length;
    for (; i2 < dataLength; ++i2) {
      if (node = group3[i2]) {
        node.__data__ = data[i2];
        update[i2] = node;
      } else {
        enter[i2] = new EnterNode(parent2, data[i2]);
      }
    }
    for (; i2 < groupLength; ++i2) {
      if (node = group3[i2]) {
        exit[i2] = node;
      }
    }
  }
  function bindKey(parent2, group3, enter, update, exit, data, key) {
    var i2, node, nodeByKeyValue = /* @__PURE__ */ new Map(), groupLength = group3.length, dataLength = data.length, keyValues = new Array(groupLength), keyValue;
    for (i2 = 0; i2 < groupLength; ++i2) {
      if (node = group3[i2]) {
        keyValues[i2] = keyValue = key.call(node, node.__data__, i2, group3) + "";
        if (nodeByKeyValue.has(keyValue)) {
          exit[i2] = node;
        } else {
          nodeByKeyValue.set(keyValue, node);
        }
      }
    }
    for (i2 = 0; i2 < dataLength; ++i2) {
      keyValue = key.call(parent2, data[i2], i2, data) + "";
      if (node = nodeByKeyValue.get(keyValue)) {
        update[i2] = node;
        node.__data__ = data[i2];
        nodeByKeyValue.delete(keyValue);
      } else {
        enter[i2] = new EnterNode(parent2, data[i2]);
      }
    }
    for (i2 = 0; i2 < groupLength; ++i2) {
      if ((node = group3[i2]) && nodeByKeyValue.get(keyValues[i2]) === node) {
        exit[i2] = node;
      }
    }
  }
  function datum(node) {
    return node.__data__;
  }
  function data_default(value13, key) {
    if (!arguments.length) return Array.from(this, datum);
    var bind10 = key ? bindKey : bindIndex, parents = this._parents, groups = this._groups;
    if (typeof value13 !== "function") value13 = constant_default(value13);
    for (var m = groups.length, update = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
      var parent2 = parents[j], group3 = groups[j], groupLength = group3.length, data = arraylike(value13.call(parent2, parent2 && parent2.__data__, j, parents)), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind10(parent2, group3, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1) i1 = i0 + 1;
          while (!(next = updateGroup[i1]) && ++i1 < dataLength) ;
          previous._next = next || null;
        }
      }
    }
    update = new Selection(update, parents);
    update._enter = enter;
    update._exit = exit;
    return update;
  }
  function arraylike(data) {
    return typeof data === "object" && "length" in data ? data : Array.from(data);
  }

  // node_modules/d3-selection/src/selection/exit.js
  function exit_default() {
    return new Selection(this._exit || this._groups.map(sparse_default), this._parents);
  }

  // node_modules/d3-selection/src/selection/join.js
  function join_default(onenter, onupdate, onexit) {
    var enter = this.enter(), update = this, exit = this.exit();
    if (typeof onenter === "function") {
      enter = onenter(enter);
      if (enter) enter = enter.selection();
    } else {
      enter = enter.append(onenter + "");
    }
    if (onupdate != null) {
      update = onupdate(update);
      if (update) update = update.selection();
    }
    if (onexit == null) exit.remove();
    else onexit(exit);
    return enter && update ? enter.merge(update).order() : update;
  }

  // node_modules/d3-selection/src/selection/merge.js
  function merge_default(context) {
    var selection2 = context.selection ? context.selection() : context;
    for (var groups0 = this._groups, groups1 = selection2._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i2 = 0; i2 < n; ++i2) {
        if (node = group0[i2] || group1[i2]) {
          merge[i2] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Selection(merges, this._parents);
  }

  // node_modules/d3-selection/src/selection/order.js
  function order_default() {
    for (var groups = this._groups, j = -1, m = groups.length; ++j < m; ) {
      for (var group3 = groups[j], i2 = group3.length - 1, next = group3[i2], node; --i2 >= 0; ) {
        if (node = group3[i2]) {
          if (next && node.compareDocumentPosition(next) ^ 4) next.parentNode.insertBefore(node, next);
          next = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/sort.js
  function sort_default(compare2) {
    if (!compare2) compare2 = ascending2;
    function compareNode(a2, b2) {
      return a2 && b2 ? compare2(a2.__data__, b2.__data__) : !a2 - !b2;
    }
    for (var groups = this._groups, m = groups.length, sortgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, sortgroup = sortgroups[j] = new Array(n), node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          sortgroup[i2] = node;
        }
      }
      sortgroup.sort(compareNode);
    }
    return new Selection(sortgroups, this._parents).order();
  }
  function ascending2(a2, b2) {
    return a2 < b2 ? -1 : a2 > b2 ? 1 : a2 >= b2 ? 0 : NaN;
  }

  // node_modules/d3-selection/src/selection/call.js
  function call_default() {
    var callback = arguments[0];
    arguments[0] = this;
    callback.apply(null, arguments);
    return this;
  }

  // node_modules/d3-selection/src/selection/nodes.js
  function nodes_default() {
    return Array.from(this);
  }

  // node_modules/d3-selection/src/selection/node.js
  function node_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i2 = 0, n = group3.length; i2 < n; ++i2) {
        var node = group3[i2];
        if (node) return node;
      }
    }
    return null;
  }

  // node_modules/d3-selection/src/selection/size.js
  function size_default() {
    let size4 = 0;
    for (const node of this) ++size4;
    return size4;
  }

  // node_modules/d3-selection/src/selection/empty.js
  function empty_default() {
    return !this.node();
  }

  // node_modules/d3-selection/src/selection/each.js
  function each_default(callback) {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i2 = 0, n = group3.length, node; i2 < n; ++i2) {
        if (node = group3[i2]) callback.call(node, node.__data__, i2, group3);
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/attr.js
  function attrRemove(name16) {
    return function() {
      this.removeAttribute(name16);
    };
  }
  function attrRemoveNS(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant(name16, value13) {
    return function() {
      this.setAttribute(name16, value13);
    };
  }
  function attrConstantNS(fullname, value13) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value13);
    };
  }
  function attrFunction(name16, value13) {
    return function() {
      var v = value13.apply(this, arguments);
      if (v == null) this.removeAttribute(name16);
      else this.setAttribute(name16, v);
    };
  }
  function attrFunctionNS(fullname, value13) {
    return function() {
      var v = value13.apply(this, arguments);
      if (v == null) this.removeAttributeNS(fullname.space, fullname.local);
      else this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default(name16, value13) {
    var fullname = namespace_default(name16);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value13 == null ? fullname.local ? attrRemoveNS : attrRemove : typeof value13 === "function" ? fullname.local ? attrFunctionNS : attrFunction : fullname.local ? attrConstantNS : attrConstant)(fullname, value13));
  }

  // node_modules/d3-selection/src/window.js
  function window_default(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-selection/src/selection/style.js
  function styleRemove(name16) {
    return function() {
      this.style.removeProperty(name16);
    };
  }
  function styleConstant(name16, value13, priority) {
    return function() {
      this.style.setProperty(name16, value13, priority);
    };
  }
  function styleFunction(name16, value13, priority) {
    return function() {
      var v = value13.apply(this, arguments);
      if (v == null) this.style.removeProperty(name16);
      else this.style.setProperty(name16, v, priority);
    };
  }
  function style_default(name16, value13, priority) {
    return arguments.length > 1 ? this.each((value13 == null ? styleRemove : typeof value13 === "function" ? styleFunction : styleConstant)(name16, value13, priority == null ? "" : priority)) : styleValue(this.node(), name16);
  }
  function styleValue(node, name16) {
    return node.style.getPropertyValue(name16) || window_default(node).getComputedStyle(node, null).getPropertyValue(name16);
  }

  // node_modules/d3-selection/src/selection/property.js
  function propertyRemove(name16) {
    return function() {
      delete this[name16];
    };
  }
  function propertyConstant(name16, value13) {
    return function() {
      this[name16] = value13;
    };
  }
  function propertyFunction(name16, value13) {
    return function() {
      var v = value13.apply(this, arguments);
      if (v == null) delete this[name16];
      else this[name16] = v;
    };
  }
  function property_default(name16, value13) {
    return arguments.length > 1 ? this.each((value13 == null ? propertyRemove : typeof value13 === "function" ? propertyFunction : propertyConstant)(name16, value13)) : this.node()[name16];
  }

  // node_modules/d3-selection/src/selection/classed.js
  function classArray(string2) {
    return string2.trim().split(/^|\s+/);
  }
  function classList2(node) {
    return node.classList || new ClassList(node);
  }
  function ClassList(node) {
    this._node = node;
    this._names = classArray(node.getAttribute("class") || "");
  }
  ClassList.prototype = {
    add: function(name16) {
      var i2 = this._names.indexOf(name16);
      if (i2 < 0) {
        this._names.push(name16);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name16) {
      var i2 = this._names.indexOf(name16);
      if (i2 >= 0) {
        this._names.splice(i2, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name16) {
      return this._names.indexOf(name16) >= 0;
    }
  };
  function classedAdd(node, names) {
    var list = classList2(node), i2 = -1, n = names.length;
    while (++i2 < n) list.add(names[i2]);
  }
  function classedRemove(node, names) {
    var list = classList2(node), i2 = -1, n = names.length;
    while (++i2 < n) list.remove(names[i2]);
  }
  function classedTrue(names) {
    return function() {
      classedAdd(this, names);
    };
  }
  function classedFalse(names) {
    return function() {
      classedRemove(this, names);
    };
  }
  function classedFunction(names, value13) {
    return function() {
      (value13.apply(this, arguments) ? classedAdd : classedRemove)(this, names);
    };
  }
  function classed_default(name16, value13) {
    var names = classArray(name16 + "");
    if (arguments.length < 2) {
      var list = classList2(this.node()), i2 = -1, n = names.length;
      while (++i2 < n) if (!list.contains(names[i2])) return false;
      return true;
    }
    return this.each((typeof value13 === "function" ? classedFunction : value13 ? classedTrue : classedFalse)(names, value13));
  }

  // node_modules/d3-selection/src/selection/text.js
  function textRemove() {
    this.textContent = "";
  }
  function textConstant(value13) {
    return function() {
      this.textContent = value13;
    };
  }
  function textFunction(value13) {
    return function() {
      var v = value13.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default(value13) {
    return arguments.length ? this.each(value13 == null ? textRemove : (typeof value13 === "function" ? textFunction : textConstant)(value13)) : this.node().textContent;
  }

  // node_modules/d3-selection/src/selection/html.js
  function htmlRemove() {
    this.innerHTML = "";
  }
  function htmlConstant(value13) {
    return function() {
      this.innerHTML = value13;
    };
  }
  function htmlFunction(value13) {
    return function() {
      var v = value13.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default(value13) {
    return arguments.length ? this.each(value13 == null ? htmlRemove : (typeof value13 === "function" ? htmlFunction : htmlConstant)(value13)) : this.node().innerHTML;
  }

  // node_modules/d3-selection/src/selection/raise.js
  function raise2() {
    if (this.nextSibling) this.parentNode.appendChild(this);
  }
  function raise_default() {
    return this.each(raise2);
  }

  // node_modules/d3-selection/src/selection/lower.js
  function lower() {
    if (this.previousSibling) this.parentNode.insertBefore(this, this.parentNode.firstChild);
  }
  function lower_default() {
    return this.each(lower);
  }

  // node_modules/d3-selection/src/selection/append.js
  function append_default(name16) {
    var create5 = typeof name16 === "function" ? name16 : creator_default(name16);
    return this.select(function() {
      return this.appendChild(create5.apply(this, arguments));
    });
  }

  // node_modules/d3-selection/src/selection/insert.js
  function constantNull() {
    return null;
  }
  function insert_default(name16, before) {
    var create5 = typeof name16 === "function" ? name16 : creator_default(name16), select5 = before == null ? constantNull : typeof before === "function" ? before : selector_default(before);
    return this.select(function() {
      return this.insertBefore(create5.apply(this, arguments), select5.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-selection/src/selection/remove.js
  function remove2() {
    var parent2 = this.parentNode;
    if (parent2) parent2.removeChild(this);
  }
  function remove_default() {
    return this.each(remove2);
  }

  // node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow() {
    var clone2 = this.cloneNode(false), parent2 = this.parentNode;
    return parent2 ? parent2.insertBefore(clone2, this.nextSibling) : clone2;
  }
  function selection_cloneDeep() {
    var clone2 = this.cloneNode(true), parent2 = this.parentNode;
    return parent2 ? parent2.insertBefore(clone2, this.nextSibling) : clone2;
  }
  function clone_default(deep) {
    return this.select(deep ? selection_cloneDeep : selection_cloneShallow);
  }

  // node_modules/d3-selection/src/selection/datum.js
  function datum_default(value13) {
    return arguments.length ? this.property("__data__", value13) : this.node().__data__;
  }

  // node_modules/d3-selection/src/selection/on.js
  function contextListener(listener) {
    return function(event) {
      listener.call(this, event, this.__data__);
    };
  }
  function parseTypenames2(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name16 = "", i2 = t.indexOf(".");
      if (i2 >= 0) name16 = t.slice(i2 + 1), t = t.slice(0, i2);
      return { type: t, name: name16 };
    });
  }
  function onRemove(typename) {
    return function() {
      var on2 = this.__on;
      if (!on2) return;
      for (var j = 0, i2 = -1, m = on2.length, o; j < m; ++j) {
        if (o = on2[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
        } else {
          on2[++i2] = o;
        }
      }
      if (++i2) on2.length = i2;
      else delete this.__on;
    };
  }
  function onAdd(typename, value13, options2) {
    return function() {
      var on2 = this.__on, o, listener = contextListener(value13);
      if (on2) for (var j = 0, m = on2.length; j < m; ++j) {
        if ((o = on2[j]).type === typename.type && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
          this.addEventListener(o.type, o.listener = listener, o.options = options2);
          o.value = value13;
          return;
        }
      }
      this.addEventListener(typename.type, listener, options2);
      o = { type: typename.type, name: typename.name, value: value13, listener, options: options2 };
      if (!on2) this.__on = [o];
      else on2.push(o);
    };
  }
  function on_default(typename, value13, options2) {
    var typenames = parseTypenames2(typename + ""), i2, n = typenames.length, t;
    if (arguments.length < 2) {
      var on2 = this.node().__on;
      if (on2) for (var j = 0, m = on2.length, o; j < m; ++j) {
        for (i2 = 0, o = on2[j]; i2 < n; ++i2) {
          if ((t = typenames[i2]).type === o.type && t.name === o.name) {
            return o.value;
          }
        }
      }
      return;
    }
    on2 = value13 ? onAdd : onRemove;
    for (i2 = 0; i2 < n; ++i2) this.each(on2(typenames[i2], value13, options2));
    return this;
  }

  // node_modules/d3-selection/src/selection/dispatch.js
  function dispatchEvent2(node, type2, params) {
    var window2 = window_default(node), event = window2.CustomEvent;
    if (typeof event === "function") {
      event = new event(type2, params);
    } else {
      event = window2.document.createEvent("Event");
      if (params) event.initEvent(type2, params.bubbles, params.cancelable), event.detail = params.detail;
      else event.initEvent(type2, false, false);
    }
    node.dispatchEvent(event);
  }
  function dispatchConstant(type2, params) {
    return function() {
      return dispatchEvent2(this, type2, params);
    };
  }
  function dispatchFunction(type2, params) {
    return function() {
      return dispatchEvent2(this, type2, params.apply(this, arguments));
    };
  }
  function dispatch_default2(type2, params) {
    return this.each((typeof params === "function" ? dispatchFunction : dispatchConstant)(type2, params));
  }

  // node_modules/d3-selection/src/selection/iterator.js
  function* iterator_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i2 = 0, n = group3.length, node; i2 < n; ++i2) {
        if (node = group3[i2]) yield node;
      }
    }
  }

  // node_modules/d3-selection/src/selection/index.js
  var root = [null];
  function Selection(groups, parents) {
    this._groups = groups;
    this._parents = parents;
  }
  function selection() {
    return new Selection([[document.documentElement]], root);
  }
  function selection_selection() {
    return this;
  }
  Selection.prototype = selection.prototype = {
    constructor: Selection,
    select: select_default,
    selectAll: selectAll_default,
    selectChild: selectChild_default,
    selectChildren: selectChildren_default,
    filter: filter_default,
    data: data_default,
    enter: enter_default,
    exit: exit_default,
    join: join_default,
    merge: merge_default,
    selection: selection_selection,
    order: order_default,
    sort: sort_default,
    call: call_default,
    nodes: nodes_default,
    node: node_default,
    size: size_default,
    empty: empty_default,
    each: each_default,
    attr: attr_default,
    style: style_default,
    property: property_default,
    classed: classed_default,
    text: text_default,
    html: html_default,
    raise: raise_default,
    lower: lower_default,
    append: append_default,
    insert: insert_default,
    remove: remove_default,
    clone: clone_default,
    datum: datum_default,
    on: on_default,
    dispatch: dispatch_default2,
    [Symbol.iterator]: iterator_default
  };
  var selection_default = selection;

  // node_modules/d3-color/src/define.js
  function define_default(constructor, factory, prototype) {
    constructor.prototype = factory.prototype = prototype;
    prototype.constructor = constructor;
  }
  function extend2(parent2, definition) {
    var prototype = Object.create(parent2.prototype);
    for (var key in definition) prototype[key] = definition[key];
    return prototype;
  }

  // node_modules/d3-color/src/color.js
  function Color() {
  }
  var darker = 0.7;
  var brighter = 1 / darker;
  var reI = "\\s*([+-]?\\d+)\\s*";
  var reN = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)\\s*";
  var reP = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)%\\s*";
  var reHex = /^#([0-9a-f]{3,8})$/;
  var reRgbInteger = new RegExp(`^rgb\\(${reI},${reI},${reI}\\)$`);
  var reRgbPercent = new RegExp(`^rgb\\(${reP},${reP},${reP}\\)$`);
  var reRgbaInteger = new RegExp(`^rgba\\(${reI},${reI},${reI},${reN}\\)$`);
  var reRgbaPercent = new RegExp(`^rgba\\(${reP},${reP},${reP},${reN}\\)$`);
  var reHslPercent = new RegExp(`^hsl\\(${reN},${reP},${reP}\\)$`);
  var reHslaPercent = new RegExp(`^hsla\\(${reN},${reP},${reP},${reN}\\)$`);
  var named = {
    aliceblue: 15792383,
    antiquewhite: 16444375,
    aqua: 65535,
    aquamarine: 8388564,
    azure: 15794175,
    beige: 16119260,
    bisque: 16770244,
    black: 0,
    blanchedalmond: 16772045,
    blue: 255,
    blueviolet: 9055202,
    brown: 10824234,
    burlywood: 14596231,
    cadetblue: 6266528,
    chartreuse: 8388352,
    chocolate: 13789470,
    coral: 16744272,
    cornflowerblue: 6591981,
    cornsilk: 16775388,
    crimson: 14423100,
    cyan: 65535,
    darkblue: 139,
    darkcyan: 35723,
    darkgoldenrod: 12092939,
    darkgray: 11119017,
    darkgreen: 25600,
    darkgrey: 11119017,
    darkkhaki: 12433259,
    darkmagenta: 9109643,
    darkolivegreen: 5597999,
    darkorange: 16747520,
    darkorchid: 10040012,
    darkred: 9109504,
    darksalmon: 15308410,
    darkseagreen: 9419919,
    darkslateblue: 4734347,
    darkslategray: 3100495,
    darkslategrey: 3100495,
    darkturquoise: 52945,
    darkviolet: 9699539,
    deeppink: 16716947,
    deepskyblue: 49151,
    dimgray: 6908265,
    dimgrey: 6908265,
    dodgerblue: 2003199,
    firebrick: 11674146,
    floralwhite: 16775920,
    forestgreen: 2263842,
    fuchsia: 16711935,
    gainsboro: 14474460,
    ghostwhite: 16316671,
    gold: 16766720,
    goldenrod: 14329120,
    gray: 8421504,
    green: 32768,
    greenyellow: 11403055,
    grey: 8421504,
    honeydew: 15794160,
    hotpink: 16738740,
    indianred: 13458524,
    indigo: 4915330,
    ivory: 16777200,
    khaki: 15787660,
    lavender: 15132410,
    lavenderblush: 16773365,
    lawngreen: 8190976,
    lemonchiffon: 16775885,
    lightblue: 11393254,
    lightcoral: 15761536,
    lightcyan: 14745599,
    lightgoldenrodyellow: 16448210,
    lightgray: 13882323,
    lightgreen: 9498256,
    lightgrey: 13882323,
    lightpink: 16758465,
    lightsalmon: 16752762,
    lightseagreen: 2142890,
    lightskyblue: 8900346,
    lightslategray: 7833753,
    lightslategrey: 7833753,
    lightsteelblue: 11584734,
    lightyellow: 16777184,
    lime: 65280,
    limegreen: 3329330,
    linen: 16445670,
    magenta: 16711935,
    maroon: 8388608,
    mediumaquamarine: 6737322,
    mediumblue: 205,
    mediumorchid: 12211667,
    mediumpurple: 9662683,
    mediumseagreen: 3978097,
    mediumslateblue: 8087790,
    mediumspringgreen: 64154,
    mediumturquoise: 4772300,
    mediumvioletred: 13047173,
    midnightblue: 1644912,
    mintcream: 16121850,
    mistyrose: 16770273,
    moccasin: 16770229,
    navajowhite: 16768685,
    navy: 128,
    oldlace: 16643558,
    olive: 8421376,
    olivedrab: 7048739,
    orange: 16753920,
    orangered: 16729344,
    orchid: 14315734,
    palegoldenrod: 15657130,
    palegreen: 10025880,
    paleturquoise: 11529966,
    palevioletred: 14381203,
    papayawhip: 16773077,
    peachpuff: 16767673,
    peru: 13468991,
    pink: 16761035,
    plum: 14524637,
    powderblue: 11591910,
    purple: 8388736,
    rebeccapurple: 6697881,
    red: 16711680,
    rosybrown: 12357519,
    royalblue: 4286945,
    saddlebrown: 9127187,
    salmon: 16416882,
    sandybrown: 16032864,
    seagreen: 3050327,
    seashell: 16774638,
    sienna: 10506797,
    silver: 12632256,
    skyblue: 8900331,
    slateblue: 6970061,
    slategray: 7372944,
    slategrey: 7372944,
    snow: 16775930,
    springgreen: 65407,
    steelblue: 4620980,
    tan: 13808780,
    teal: 32896,
    thistle: 14204888,
    tomato: 16737095,
    turquoise: 4251856,
    violet: 15631086,
    wheat: 16113331,
    white: 16777215,
    whitesmoke: 16119285,
    yellow: 16776960,
    yellowgreen: 10145074
  };
  define_default(Color, color, {
    copy(channels) {
      return Object.assign(new this.constructor(), this, channels);
    },
    displayable() {
      return this.rgb().displayable();
    },
    hex: color_formatHex,
    // Deprecated! Use color.formatHex.
    formatHex: color_formatHex,
    formatHex8: color_formatHex8,
    formatHsl: color_formatHsl,
    formatRgb: color_formatRgb,
    toString: color_formatRgb
  });
  function color_formatHex() {
    return this.rgb().formatHex();
  }
  function color_formatHex8() {
    return this.rgb().formatHex8();
  }
  function color_formatHsl() {
    return hslConvert(this).formatHsl();
  }
  function color_formatRgb() {
    return this.rgb().formatRgb();
  }
  function color(format2) {
    var m, l;
    format2 = (format2 + "").trim().toLowerCase();
    return (m = reHex.exec(format2)) ? (l = m[1].length, m = parseInt(m[1], 16), l === 6 ? rgbn(m) : l === 3 ? new Rgb(m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, (m & 15) << 4 | m & 15, 1) : l === 8 ? rgba(m >> 24 & 255, m >> 16 & 255, m >> 8 & 255, (m & 255) / 255) : l === 4 ? rgba(m >> 12 & 15 | m >> 8 & 240, m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, ((m & 15) << 4 | m & 15) / 255) : null) : (m = reRgbInteger.exec(format2)) ? new Rgb(m[1], m[2], m[3], 1) : (m = reRgbPercent.exec(format2)) ? new Rgb(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, 1) : (m = reRgbaInteger.exec(format2)) ? rgba(m[1], m[2], m[3], m[4]) : (m = reRgbaPercent.exec(format2)) ? rgba(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, m[4]) : (m = reHslPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, 1) : (m = reHslaPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, m[4]) : named.hasOwnProperty(format2) ? rgbn(named[format2]) : format2 === "transparent" ? new Rgb(NaN, NaN, NaN, 0) : null;
  }
  function rgbn(n) {
    return new Rgb(n >> 16 & 255, n >> 8 & 255, n & 255, 1);
  }
  function rgba(r, g, b2, a2) {
    if (a2 <= 0) r = g = b2 = NaN;
    return new Rgb(r, g, b2, a2);
  }
  function rgbConvert(o) {
    if (!(o instanceof Color)) o = color(o);
    if (!o) return new Rgb();
    o = o.rgb();
    return new Rgb(o.r, o.g, o.b, o.opacity);
  }
  function rgb(r, g, b2, opacity) {
    return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b2, opacity == null ? 1 : opacity);
  }
  function Rgb(r, g, b2, opacity) {
    this.r = +r;
    this.g = +g;
    this.b = +b2;
    this.opacity = +opacity;
  }
  define_default(Rgb, rgb, extend2(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    rgb() {
      return this;
    },
    clamp() {
      return new Rgb(clampi(this.r), clampi(this.g), clampi(this.b), clampa(this.opacity));
    },
    displayable() {
      return -0.5 <= this.r && this.r < 255.5 && (-0.5 <= this.g && this.g < 255.5) && (-0.5 <= this.b && this.b < 255.5) && (0 <= this.opacity && this.opacity <= 1);
    },
    hex: rgb_formatHex,
    // Deprecated! Use color.formatHex.
    formatHex: rgb_formatHex,
    formatHex8: rgb_formatHex8,
    formatRgb: rgb_formatRgb,
    toString: rgb_formatRgb
  }));
  function rgb_formatHex() {
    return `#${hex(this.r)}${hex(this.g)}${hex(this.b)}`;
  }
  function rgb_formatHex8() {
    return `#${hex(this.r)}${hex(this.g)}${hex(this.b)}${hex((isNaN(this.opacity) ? 1 : this.opacity) * 255)}`;
  }
  function rgb_formatRgb() {
    const a2 = clampa(this.opacity);
    return `${a2 === 1 ? "rgb(" : "rgba("}${clampi(this.r)}, ${clampi(this.g)}, ${clampi(this.b)}${a2 === 1 ? ")" : `, ${a2})`}`;
  }
  function clampa(opacity) {
    return isNaN(opacity) ? 1 : Math.max(0, Math.min(1, opacity));
  }
  function clampi(value13) {
    return Math.max(0, Math.min(255, Math.round(value13) || 0));
  }
  function hex(value13) {
    value13 = clampi(value13);
    return (value13 < 16 ? "0" : "") + value13.toString(16);
  }
  function hsla(h, s, l, a2) {
    if (a2 <= 0) h = s = l = NaN;
    else if (l <= 0 || l >= 1) h = s = NaN;
    else if (s <= 0) h = NaN;
    return new Hsl(h, s, l, a2);
  }
  function hslConvert(o) {
    if (o instanceof Hsl) return new Hsl(o.h, o.s, o.l, o.opacity);
    if (!(o instanceof Color)) o = color(o);
    if (!o) return new Hsl();
    if (o instanceof Hsl) return o;
    o = o.rgb();
    var r = o.r / 255, g = o.g / 255, b2 = o.b / 255, min6 = Math.min(r, g, b2), max7 = Math.max(r, g, b2), h = NaN, s = max7 - min6, l = (max7 + min6) / 2;
    if (s) {
      if (r === max7) h = (g - b2) / s + (g < b2) * 6;
      else if (g === max7) h = (b2 - r) / s + 2;
      else h = (r - g) / s + 4;
      s /= l < 0.5 ? max7 + min6 : 2 - max7 - min6;
      h *= 60;
    } else {
      s = l > 0 && l < 1 ? 0 : h;
    }
    return new Hsl(h, s, l, o.opacity);
  }
  function hsl(h, s, l, opacity) {
    return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity == null ? 1 : opacity);
  }
  function Hsl(h, s, l, opacity) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity;
  }
  define_default(Hsl, hsl, extend2(Color, {
    brighter(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    darker(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    rgb() {
      var h = this.h % 360 + (this.h < 0) * 360, s = isNaN(h) || isNaN(this.s) ? 0 : this.s, l = this.l, m2 = l + (l < 0.5 ? l : 1 - l) * s, m1 = 2 * l - m2;
      return new Rgb(
        hsl2rgb(h >= 240 ? h - 240 : h + 120, m1, m2),
        hsl2rgb(h, m1, m2),
        hsl2rgb(h < 120 ? h + 240 : h - 120, m1, m2),
        this.opacity
      );
    },
    clamp() {
      return new Hsl(clamph(this.h), clampt(this.s), clampt(this.l), clampa(this.opacity));
    },
    displayable() {
      return (0 <= this.s && this.s <= 1 || isNaN(this.s)) && (0 <= this.l && this.l <= 1) && (0 <= this.opacity && this.opacity <= 1);
    },
    formatHsl() {
      const a2 = clampa(this.opacity);
      return `${a2 === 1 ? "hsl(" : "hsla("}${clamph(this.h)}, ${clampt(this.s) * 100}%, ${clampt(this.l) * 100}%${a2 === 1 ? ")" : `, ${a2})`}`;
    }
  }));
  function clamph(value13) {
    value13 = (value13 || 0) % 360;
    return value13 < 0 ? value13 + 360 : value13;
  }
  function clampt(value13) {
    return Math.max(0, Math.min(1, value13 || 0));
  }
  function hsl2rgb(h, m1, m2) {
    return (h < 60 ? m1 + (m2 - m1) * h / 60 : h < 180 ? m2 : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60 : m1) * 255;
  }

  // node_modules/d3-interpolate/src/basis.js
  function basis(t1, v0, v1, v2, v3) {
    var t2 = t1 * t1, t3 = t2 * t1;
    return ((1 - 3 * t1 + 3 * t2 - t3) * v0 + (4 - 6 * t2 + 3 * t3) * v1 + (1 + 3 * t1 + 3 * t2 - 3 * t3) * v2 + t3 * v3) / 6;
  }
  function basis_default(values) {
    var n = values.length - 1;
    return function(t) {
      var i2 = t <= 0 ? t = 0 : t >= 1 ? (t = 1, n - 1) : Math.floor(t * n), v1 = values[i2], v2 = values[i2 + 1], v0 = i2 > 0 ? values[i2 - 1] : 2 * v1 - v2, v3 = i2 < n - 1 ? values[i2 + 2] : 2 * v2 - v1;
      return basis((t - i2 / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/basisClosed.js
  function basisClosed_default(values) {
    var n = values.length;
    return function(t) {
      var i2 = Math.floor(((t %= 1) < 0 ? ++t : t) * n), v0 = values[(i2 + n - 1) % n], v1 = values[i2 % n], v2 = values[(i2 + 1) % n], v3 = values[(i2 + 2) % n];
      return basis((t - i2 / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/constant.js
  var constant_default2 = (x7) => () => x7;

  // node_modules/d3-interpolate/src/color.js
  function linear(a2, d6) {
    return function(t) {
      return a2 + t * d6;
    };
  }
  function exponential(a2, b2, y6) {
    return a2 = Math.pow(a2, y6), b2 = Math.pow(b2, y6) - a2, y6 = 1 / y6, function(t) {
      return Math.pow(a2 + t * b2, y6);
    };
  }
  function gamma(y6) {
    return (y6 = +y6) === 1 ? nogamma : function(a2, b2) {
      return b2 - a2 ? exponential(a2, b2, y6) : constant_default2(isNaN(a2) ? b2 : a2);
    };
  }
  function nogamma(a2, b2) {
    var d6 = b2 - a2;
    return d6 ? linear(a2, d6) : constant_default2(isNaN(a2) ? b2 : a2);
  }

  // node_modules/d3-interpolate/src/rgb.js
  var rgb_default = (function rgbGamma(y6) {
    var color2 = gamma(y6);
    function rgb2(start3, end) {
      var r = color2((start3 = rgb(start3)).r, (end = rgb(end)).r), g = color2(start3.g, end.g), b2 = color2(start3.b, end.b), opacity = nogamma(start3.opacity, end.opacity);
      return function(t) {
        start3.r = r(t);
        start3.g = g(t);
        start3.b = b2(t);
        start3.opacity = opacity(t);
        return start3 + "";
      };
    }
    rgb2.gamma = rgbGamma;
    return rgb2;
  })(1);
  function rgbSpline(spline) {
    return function(colors2) {
      var n = colors2.length, r = new Array(n), g = new Array(n), b2 = new Array(n), i2, color2;
      for (i2 = 0; i2 < n; ++i2) {
        color2 = rgb(colors2[i2]);
        r[i2] = color2.r || 0;
        g[i2] = color2.g || 0;
        b2[i2] = color2.b || 0;
      }
      r = spline(r);
      g = spline(g);
      b2 = spline(b2);
      color2.opacity = 1;
      return function(t) {
        color2.r = r(t);
        color2.g = g(t);
        color2.b = b2(t);
        return color2 + "";
      };
    };
  }
  var rgbBasis = rgbSpline(basis_default);
  var rgbBasisClosed = rgbSpline(basisClosed_default);

  // node_modules/d3-interpolate/src/numberArray.js
  function numberArray_default(a2, b2) {
    if (!b2) b2 = [];
    var n = a2 ? Math.min(b2.length, a2.length) : 0, c = b2.slice(), i2;
    return function(t) {
      for (i2 = 0; i2 < n; ++i2) c[i2] = a2[i2] * (1 - t) + b2[i2] * t;
      return c;
    };
  }
  function isNumberArray(x7) {
    return ArrayBuffer.isView(x7) && !(x7 instanceof DataView);
  }

  // node_modules/d3-interpolate/src/array.js
  function genericArray(a2, b2) {
    var nb = b2 ? b2.length : 0, na = a2 ? Math.min(nb, a2.length) : 0, x7 = new Array(na), c = new Array(nb), i2;
    for (i2 = 0; i2 < na; ++i2) x7[i2] = value_default(a2[i2], b2[i2]);
    for (; i2 < nb; ++i2) c[i2] = b2[i2];
    return function(t) {
      for (i2 = 0; i2 < na; ++i2) c[i2] = x7[i2](t);
      return c;
    };
  }

  // node_modules/d3-interpolate/src/date.js
  function date_default(a2, b2) {
    var d6 = /* @__PURE__ */ new Date();
    return a2 = +a2, b2 = +b2, function(t) {
      return d6.setTime(a2 * (1 - t) + b2 * t), d6;
    };
  }

  // node_modules/d3-interpolate/src/number.js
  function number_default(a2, b2) {
    return a2 = +a2, b2 = +b2, function(t) {
      return a2 * (1 - t) + b2 * t;
    };
  }

  // node_modules/d3-interpolate/src/object.js
  function object_default(a2, b2) {
    var i2 = {}, c = {}, k;
    if (a2 === null || typeof a2 !== "object") a2 = {};
    if (b2 === null || typeof b2 !== "object") b2 = {};
    for (k in b2) {
      if (k in a2) {
        i2[k] = value_default(a2[k], b2[k]);
      } else {
        c[k] = b2[k];
      }
    }
    return function(t) {
      for (k in i2) c[k] = i2[k](t);
      return c;
    };
  }

  // node_modules/d3-interpolate/src/string.js
  var reA = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g;
  var reB = new RegExp(reA.source, "g");
  function zero3(b2) {
    return function() {
      return b2;
    };
  }
  function one2(b2) {
    return function(t) {
      return b2(t) + "";
    };
  }
  function string_default(a2, b2) {
    var bi = reA.lastIndex = reB.lastIndex = 0, am, bm, bs, i2 = -1, s = [], q2 = [];
    a2 = a2 + "", b2 = b2 + "";
    while ((am = reA.exec(a2)) && (bm = reB.exec(b2))) {
      if ((bs = bm.index) > bi) {
        bs = b2.slice(bi, bs);
        if (s[i2]) s[i2] += bs;
        else s[++i2] = bs;
      }
      if ((am = am[0]) === (bm = bm[0])) {
        if (s[i2]) s[i2] += bm;
        else s[++i2] = bm;
      } else {
        s[++i2] = null;
        q2.push({ i: i2, x: number_default(am, bm) });
      }
      bi = reB.lastIndex;
    }
    if (bi < b2.length) {
      bs = b2.slice(bi);
      if (s[i2]) s[i2] += bs;
      else s[++i2] = bs;
    }
    return s.length < 2 ? q2[0] ? one2(q2[0].x) : zero3(b2) : (b2 = q2.length, function(t) {
      for (var i3 = 0, o; i3 < b2; ++i3) s[(o = q2[i3]).i] = o.x(t);
      return s.join("");
    });
  }

  // node_modules/d3-interpolate/src/value.js
  function value_default(a2, b2) {
    var t = typeof b2, c;
    return b2 == null || t === "boolean" ? constant_default2(b2) : (t === "number" ? number_default : t === "string" ? (c = color(b2)) ? (b2 = c, rgb_default) : string_default : b2 instanceof color ? rgb_default : b2 instanceof Date ? date_default : isNumberArray(b2) ? numberArray_default : Array.isArray(b2) ? genericArray : typeof b2.valueOf !== "function" && typeof b2.toString !== "function" || isNaN(b2) ? object_default : number_default)(a2, b2);
  }

  // node_modules/d3-interpolate/src/round.js
  function round_default(a2, b2) {
    return a2 = +a2, b2 = +b2, function(t) {
      return Math.round(a2 * (1 - t) + b2 * t);
    };
  }

  // node_modules/d3-interpolate/src/transform/decompose.js
  var degrees = 180 / Math.PI;
  var identity11 = {
    translateX: 0,
    translateY: 0,
    rotate: 0,
    skewX: 0,
    scaleX: 1,
    scaleY: 1
  };
  function decompose_default(a2, b2, c, d6, e, f) {
    var scaleX, scaleY, skewX;
    if (scaleX = Math.sqrt(a2 * a2 + b2 * b2)) a2 /= scaleX, b2 /= scaleX;
    if (skewX = a2 * c + b2 * d6) c -= a2 * skewX, d6 -= b2 * skewX;
    if (scaleY = Math.sqrt(c * c + d6 * d6)) c /= scaleY, d6 /= scaleY, skewX /= scaleY;
    if (a2 * d6 < b2 * c) a2 = -a2, b2 = -b2, skewX = -skewX, scaleX = -scaleX;
    return {
      translateX: e,
      translateY: f,
      rotate: Math.atan2(b2, a2) * degrees,
      skewX: Math.atan(skewX) * degrees,
      scaleX,
      scaleY
    };
  }

  // node_modules/d3-interpolate/src/transform/parse.js
  var svgNode;
  function parseCss(value13) {
    const m = new (typeof DOMMatrix === "function" ? DOMMatrix : WebKitCSSMatrix)(value13 + "");
    return m.isIdentity ? identity11 : decompose_default(m.a, m.b, m.c, m.d, m.e, m.f);
  }
  function parseSvg(value13) {
    if (value13 == null) return identity11;
    if (!svgNode) svgNode = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svgNode.setAttribute("transform", value13);
    if (!(value13 = svgNode.transform.baseVal.consolidate())) return identity11;
    value13 = value13.matrix;
    return decompose_default(value13.a, value13.b, value13.c, value13.d, value13.e, value13.f);
  }

  // node_modules/d3-interpolate/src/transform/index.js
  function interpolateTransform(parse6, pxComma, pxParen, degParen) {
    function pop4(s) {
      return s.length ? s.pop() + " " : "";
    }
    function translate(xa, ya, xb, yb, s, q2) {
      if (xa !== xb || ya !== yb) {
        var i2 = s.push("translate(", null, pxComma, null, pxParen);
        q2.push({ i: i2 - 4, x: number_default(xa, xb) }, { i: i2 - 2, x: number_default(ya, yb) });
      } else if (xb || yb) {
        s.push("translate(" + xb + pxComma + yb + pxParen);
      }
    }
    function rotate(a2, b2, s, q2) {
      if (a2 !== b2) {
        if (a2 - b2 > 180) b2 += 360;
        else if (b2 - a2 > 180) a2 += 360;
        q2.push({ i: s.push(pop4(s) + "rotate(", null, degParen) - 2, x: number_default(a2, b2) });
      } else if (b2) {
        s.push(pop4(s) + "rotate(" + b2 + degParen);
      }
    }
    function skewX(a2, b2, s, q2) {
      if (a2 !== b2) {
        q2.push({ i: s.push(pop4(s) + "skewX(", null, degParen) - 2, x: number_default(a2, b2) });
      } else if (b2) {
        s.push(pop4(s) + "skewX(" + b2 + degParen);
      }
    }
    function scale(xa, ya, xb, yb, s, q2) {
      if (xa !== xb || ya !== yb) {
        var i2 = s.push(pop4(s) + "scale(", null, ",", null, ")");
        q2.push({ i: i2 - 4, x: number_default(xa, xb) }, { i: i2 - 2, x: number_default(ya, yb) });
      } else if (xb !== 1 || yb !== 1) {
        s.push(pop4(s) + "scale(" + xb + "," + yb + ")");
      }
    }
    return function(a2, b2) {
      var s = [], q2 = [];
      a2 = parse6(a2), b2 = parse6(b2);
      translate(a2.translateX, a2.translateY, b2.translateX, b2.translateY, s, q2);
      rotate(a2.rotate, b2.rotate, s, q2);
      skewX(a2.skewX, b2.skewX, s, q2);
      scale(a2.scaleX, a2.scaleY, b2.scaleX, b2.scaleY, s, q2);
      a2 = b2 = null;
      return function(t) {
        var i2 = -1, n = q2.length, o;
        while (++i2 < n) s[(o = q2[i2]).i] = o.x(t);
        return s.join("");
      };
    };
  }
  var interpolateTransformCss = interpolateTransform(parseCss, "px, ", "px)", "deg)");
  var interpolateTransformSvg = interpolateTransform(parseSvg, ", ", ")", ")");

  // node_modules/d3-timer/src/timer.js
  var frame = 0;
  var timeout = 0;
  var interval = 0;
  var pokeDelay = 1e3;
  var taskHead;
  var taskTail;
  var clockLast = 0;
  var clockNow = 0;
  var clockSkew = 0;
  var clock = typeof performance === "object" && performance.now ? performance : Date;
  var setFrame = typeof window === "object" && window.requestAnimationFrame ? window.requestAnimationFrame.bind(window) : function(f) {
    setTimeout(f, 17);
  };
  function now() {
    return clockNow || (setFrame(clearNow), clockNow = clock.now() + clockSkew);
  }
  function clearNow() {
    clockNow = 0;
  }
  function Timer() {
    this._call = this._time = this._next = null;
  }
  Timer.prototype = timer.prototype = {
    constructor: Timer,
    restart: function(callback, delay2, time3) {
      if (typeof callback !== "function") throw new TypeError("callback is not a function");
      time3 = (time3 == null ? now() : +time3) + (delay2 == null ? 0 : +delay2);
      if (!this._next && taskTail !== this) {
        if (taskTail) taskTail._next = this;
        else taskHead = this;
        taskTail = this;
      }
      this._call = callback;
      this._time = time3;
      sleep();
    },
    stop: function() {
      if (this._call) {
        this._call = null;
        this._time = Infinity;
        sleep();
      }
    }
  };
  function timer(callback, delay2, time3) {
    var t = new Timer();
    t.restart(callback, delay2, time3);
    return t;
  }
  function timerFlush() {
    now();
    ++frame;
    var t = taskHead, e;
    while (t) {
      if ((e = clockNow - t._time) >= 0) t._call.call(void 0, e);
      t = t._next;
    }
    --frame;
  }
  function wake() {
    clockNow = (clockLast = clock.now()) + clockSkew;
    frame = timeout = 0;
    try {
      timerFlush();
    } finally {
      frame = 0;
      nap();
      clockNow = 0;
    }
  }
  function poke3() {
    var now3 = clock.now(), delay2 = now3 - clockLast;
    if (delay2 > pokeDelay) clockSkew -= delay2, clockLast = now3;
  }
  function nap() {
    var t0, t1 = taskHead, t2, time3 = Infinity;
    while (t1) {
      if (t1._call) {
        if (time3 > t1._time) time3 = t1._time;
        t0 = t1, t1 = t1._next;
      } else {
        t2 = t1._next, t1._next = null;
        t1 = t0 ? t0._next = t2 : taskHead = t2;
      }
    }
    taskTail = t0;
    sleep(time3);
  }
  function sleep(time3) {
    if (frame) return;
    if (timeout) timeout = clearTimeout(timeout);
    var delay2 = time3 - clockNow;
    if (delay2 > 24) {
      if (time3 < Infinity) timeout = setTimeout(wake, time3 - clock.now() - clockSkew);
      if (interval) interval = clearInterval(interval);
    } else {
      if (!interval) clockLast = clock.now(), interval = setInterval(poke3, pokeDelay);
      frame = 1, setFrame(wake);
    }
  }

  // node_modules/d3-timer/src/timeout.js
  function timeout_default(callback, delay2, time3) {
    var t = new Timer();
    delay2 = delay2 == null ? 0 : +delay2;
    t.restart((elapsed) => {
      t.stop();
      callback(elapsed + delay2);
    }, delay2, time3);
    return t;
  }

  // node_modules/d3-transition/src/transition/schedule.js
  var emptyOn = dispatch_default("start", "end", "cancel", "interrupt");
  var emptyTween = [];
  var CREATED = 0;
  var SCHEDULED = 1;
  var STARTING = 2;
  var STARTED = 3;
  var RUNNING = 4;
  var ENDING = 5;
  var ENDED = 6;
  function schedule_default(node, name16, id5, index4, group3, timing) {
    var schedules = node.__transition;
    if (!schedules) node.__transition = {};
    else if (id5 in schedules) return;
    create4(node, id5, {
      name: name16,
      index: index4,
      // For context during callback.
      group: group3,
      // For context during callback.
      on: emptyOn,
      tween: emptyTween,
      time: timing.time,
      delay: timing.delay,
      duration: timing.duration,
      ease: timing.ease,
      timer: null,
      state: CREATED
    });
  }
  function init2(node, id5) {
    var schedule = get5(node, id5);
    if (schedule.state > CREATED) throw new Error("too late; already scheduled");
    return schedule;
  }
  function set2(node, id5) {
    var schedule = get5(node, id5);
    if (schedule.state > STARTED) throw new Error("too late; already running");
    return schedule;
  }
  function get5(node, id5) {
    var schedule = node.__transition;
    if (!schedule || !(schedule = schedule[id5])) throw new Error("transition not found");
    return schedule;
  }
  function create4(node, id5, self) {
    var schedules = node.__transition, tween;
    schedules[id5] = self;
    self.timer = timer(schedule, 0, self.time);
    function schedule(elapsed) {
      self.state = SCHEDULED;
      self.timer.restart(start3, self.delay, self.time);
      if (self.delay <= elapsed) start3(elapsed - self.delay);
    }
    function start3(elapsed) {
      var i2, j, n, o;
      if (self.state !== SCHEDULED) return stop();
      for (i2 in schedules) {
        o = schedules[i2];
        if (o.name !== self.name) continue;
        if (o.state === STARTED) return timeout_default(start3);
        if (o.state === RUNNING) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("interrupt", node, node.__data__, o.index, o.group);
          delete schedules[i2];
        } else if (+i2 < id5) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("cancel", node, node.__data__, o.index, o.group);
          delete schedules[i2];
        }
      }
      timeout_default(function() {
        if (self.state === STARTED) {
          self.state = RUNNING;
          self.timer.restart(tick, self.delay, self.time);
          tick(elapsed);
        }
      });
      self.state = STARTING;
      self.on.call("start", node, node.__data__, self.index, self.group);
      if (self.state !== STARTING) return;
      self.state = STARTED;
      tween = new Array(n = self.tween.length);
      for (i2 = 0, j = -1; i2 < n; ++i2) {
        if (o = self.tween[i2].value.call(node, node.__data__, self.index, self.group)) {
          tween[++j] = o;
        }
      }
      tween.length = j + 1;
    }
    function tick(elapsed) {
      var t = elapsed < self.duration ? self.ease.call(null, elapsed / self.duration) : (self.timer.restart(stop), self.state = ENDING, 1), i2 = -1, n = tween.length;
      while (++i2 < n) {
        tween[i2].call(node, t);
      }
      if (self.state === ENDING) {
        self.on.call("end", node, node.__data__, self.index, self.group);
        stop();
      }
    }
    function stop() {
      self.state = ENDED;
      self.timer.stop();
      delete schedules[id5];
      for (var i2 in schedules) return;
      delete node.__transition;
    }
  }

  // node_modules/d3-transition/src/interrupt.js
  function interrupt_default(node, name16) {
    var schedules = node.__transition, schedule, active, empty8 = true, i2;
    if (!schedules) return;
    name16 = name16 == null ? null : name16 + "";
    for (i2 in schedules) {
      if ((schedule = schedules[i2]).name !== name16) {
        empty8 = false;
        continue;
      }
      active = schedule.state > STARTING && schedule.state < ENDING;
      schedule.state = ENDED;
      schedule.timer.stop();
      schedule.on.call(active ? "interrupt" : "cancel", node, node.__data__, schedule.index, schedule.group);
      delete schedules[i2];
    }
    if (empty8) delete node.__transition;
  }

  // node_modules/d3-transition/src/selection/interrupt.js
  function interrupt_default2(name16) {
    return this.each(function() {
      interrupt_default(this, name16);
    });
  }

  // node_modules/d3-transition/src/transition/tween.js
  function tweenRemove(id5, name16) {
    var tween0, tween1;
    return function() {
      var schedule = set2(this, id5), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = tween0 = tween;
        for (var i2 = 0, n = tween1.length; i2 < n; ++i2) {
          if (tween1[i2].name === name16) {
            tween1 = tween1.slice();
            tween1.splice(i2, 1);
            break;
          }
        }
      }
      schedule.tween = tween1;
    };
  }
  function tweenFunction(id5, name16, value13) {
    var tween0, tween1;
    if (typeof value13 !== "function") throw new Error();
    return function() {
      var schedule = set2(this, id5), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = (tween0 = tween).slice();
        for (var t = { name: name16, value: value13 }, i2 = 0, n = tween1.length; i2 < n; ++i2) {
          if (tween1[i2].name === name16) {
            tween1[i2] = t;
            break;
          }
        }
        if (i2 === n) tween1.push(t);
      }
      schedule.tween = tween1;
    };
  }
  function tween_default(name16, value13) {
    var id5 = this._id;
    name16 += "";
    if (arguments.length < 2) {
      var tween = get5(this.node(), id5).tween;
      for (var i2 = 0, n = tween.length, t; i2 < n; ++i2) {
        if ((t = tween[i2]).name === name16) {
          return t.value;
        }
      }
      return null;
    }
    return this.each((value13 == null ? tweenRemove : tweenFunction)(id5, name16, value13));
  }
  function tweenValue(transition2, name16, value13) {
    var id5 = transition2._id;
    transition2.each(function() {
      var schedule = set2(this, id5);
      (schedule.value || (schedule.value = {}))[name16] = value13.apply(this, arguments);
    });
    return function(node) {
      return get5(node, id5).value[name16];
    };
  }

  // node_modules/d3-transition/src/transition/interpolate.js
  function interpolate_default(a2, b2) {
    var c;
    return (typeof b2 === "number" ? number_default : b2 instanceof color ? rgb_default : (c = color(b2)) ? (b2 = c, rgb_default) : string_default)(a2, b2);
  }

  // node_modules/d3-transition/src/transition/attr.js
  function attrRemove2(name16) {
    return function() {
      this.removeAttribute(name16);
    };
  }
  function attrRemoveNS2(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant2(name16, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttribute(name16);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrConstantNS2(fullname, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttributeNS(fullname.space, fullname.local);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrFunction2(name16, interpolate, value13) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value13(this), string1;
      if (value1 == null) return void this.removeAttribute(name16);
      string0 = this.getAttribute(name16);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attrFunctionNS2(fullname, interpolate, value13) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value13(this), string1;
      if (value1 == null) return void this.removeAttributeNS(fullname.space, fullname.local);
      string0 = this.getAttributeNS(fullname.space, fullname.local);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attr_default2(name16, value13) {
    var fullname = namespace_default(name16), i2 = fullname === "transform" ? interpolateTransformSvg : interpolate_default;
    return this.attrTween(name16, typeof value13 === "function" ? (fullname.local ? attrFunctionNS2 : attrFunction2)(fullname, i2, tweenValue(this, "attr." + name16, value13)) : value13 == null ? (fullname.local ? attrRemoveNS2 : attrRemove2)(fullname) : (fullname.local ? attrConstantNS2 : attrConstant2)(fullname, i2, value13));
  }

  // node_modules/d3-transition/src/transition/attrTween.js
  function attrInterpolate(name16, i2) {
    return function(t) {
      this.setAttribute(name16, i2.call(this, t));
    };
  }
  function attrInterpolateNS(fullname, i2) {
    return function(t) {
      this.setAttributeNS(fullname.space, fullname.local, i2.call(this, t));
    };
  }
  function attrTweenNS(fullname, value13) {
    var t0, i0;
    function tween() {
      var i2 = value13.apply(this, arguments);
      if (i2 !== i0) t0 = (i0 = i2) && attrInterpolateNS(fullname, i2);
      return t0;
    }
    tween._value = value13;
    return tween;
  }
  function attrTween(name16, value13) {
    var t0, i0;
    function tween() {
      var i2 = value13.apply(this, arguments);
      if (i2 !== i0) t0 = (i0 = i2) && attrInterpolate(name16, i2);
      return t0;
    }
    tween._value = value13;
    return tween;
  }
  function attrTween_default(name16, value13) {
    var key = "attr." + name16;
    if (arguments.length < 2) return (key = this.tween(key)) && key._value;
    if (value13 == null) return this.tween(key, null);
    if (typeof value13 !== "function") throw new Error();
    var fullname = namespace_default(name16);
    return this.tween(key, (fullname.local ? attrTweenNS : attrTween)(fullname, value13));
  }

  // node_modules/d3-transition/src/transition/delay.js
  function delayFunction(id5, value13) {
    return function() {
      init2(this, id5).delay = +value13.apply(this, arguments);
    };
  }
  function delayConstant(id5, value13) {
    return value13 = +value13, function() {
      init2(this, id5).delay = value13;
    };
  }
  function delay_default(value13) {
    var id5 = this._id;
    return arguments.length ? this.each((typeof value13 === "function" ? delayFunction : delayConstant)(id5, value13)) : get5(this.node(), id5).delay;
  }

  // node_modules/d3-transition/src/transition/duration.js
  function durationFunction(id5, value13) {
    return function() {
      set2(this, id5).duration = +value13.apply(this, arguments);
    };
  }
  function durationConstant(id5, value13) {
    return value13 = +value13, function() {
      set2(this, id5).duration = value13;
    };
  }
  function duration_default(value13) {
    var id5 = this._id;
    return arguments.length ? this.each((typeof value13 === "function" ? durationFunction : durationConstant)(id5, value13)) : get5(this.node(), id5).duration;
  }

  // node_modules/d3-transition/src/transition/ease.js
  function easeConstant(id5, value13) {
    if (typeof value13 !== "function") throw new Error();
    return function() {
      set2(this, id5).ease = value13;
    };
  }
  function ease_default(value13) {
    var id5 = this._id;
    return arguments.length ? this.each(easeConstant(id5, value13)) : get5(this.node(), id5).ease;
  }

  // node_modules/d3-transition/src/transition/easeVarying.js
  function easeVarying(id5, value13) {
    return function() {
      var v = value13.apply(this, arguments);
      if (typeof v !== "function") throw new Error();
      set2(this, id5).ease = v;
    };
  }
  function easeVarying_default(value13) {
    if (typeof value13 !== "function") throw new Error();
    return this.each(easeVarying(this._id, value13));
  }

  // node_modules/d3-transition/src/transition/filter.js
  function filter_default2(match) {
    if (typeof match !== "function") match = matcher_default(match);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = [], node, i2 = 0; i2 < n; ++i2) {
        if ((node = group3[i2]) && match.call(node, node.__data__, i2, group3)) {
          subgroup.push(node);
        }
      }
    }
    return new Transition(subgroups, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/merge.js
  function merge_default2(transition2) {
    if (transition2._id !== this._id) throw new Error();
    for (var groups0 = this._groups, groups1 = transition2._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i2 = 0; i2 < n; ++i2) {
        if (node = group0[i2] || group1[i2]) {
          merge[i2] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Transition(merges, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/on.js
  function start2(name16) {
    return (name16 + "").trim().split(/^|\s+/).every(function(t) {
      var i2 = t.indexOf(".");
      if (i2 >= 0) t = t.slice(0, i2);
      return !t || t === "start";
    });
  }
  function onFunction(id5, name16, listener) {
    var on0, on1, sit = start2(name16) ? init2 : set2;
    return function() {
      var schedule = sit(this, id5), on2 = schedule.on;
      if (on2 !== on0) (on1 = (on0 = on2).copy()).on(name16, listener);
      schedule.on = on1;
    };
  }
  function on_default2(name16, listener) {
    var id5 = this._id;
    return arguments.length < 2 ? get5(this.node(), id5).on.on(name16) : this.each(onFunction(id5, name16, listener));
  }

  // node_modules/d3-transition/src/transition/remove.js
  function removeFunction(id5) {
    return function() {
      var parent2 = this.parentNode;
      for (var i2 in this.__transition) if (+i2 !== id5) return;
      if (parent2) parent2.removeChild(this);
    };
  }
  function remove_default2() {
    return this.on("end.remove", removeFunction(this._id));
  }

  // node_modules/d3-transition/src/transition/select.js
  function select_default2(select5) {
    var name16 = this._name, id5 = this._id;
    if (typeof select5 !== "function") select5 = selector_default(select5);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = new Array(n), node, subnode, i2 = 0; i2 < n; ++i2) {
        if ((node = group3[i2]) && (subnode = select5.call(node, node.__data__, i2, group3))) {
          if ("__data__" in node) subnode.__data__ = node.__data__;
          subgroup[i2] = subnode;
          schedule_default(subgroup[i2], name16, id5, i2, subgroup, get5(node, id5));
        }
      }
    }
    return new Transition(subgroups, this._parents, name16, id5);
  }

  // node_modules/d3-transition/src/transition/selectAll.js
  function selectAll_default2(select5) {
    var name16 = this._name, id5 = this._id;
    if (typeof select5 !== "function") select5 = selectorAll_default(select5);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          for (var children3 = select5.call(node, node.__data__, i2, group3), child, inherit2 = get5(node, id5), k = 0, l = children3.length; k < l; ++k) {
            if (child = children3[k]) {
              schedule_default(child, name16, id5, k, children3, inherit2);
            }
          }
          subgroups.push(children3);
          parents.push(node);
        }
      }
    }
    return new Transition(subgroups, parents, name16, id5);
  }

  // node_modules/d3-transition/src/transition/selection.js
  var Selection2 = selection_default.prototype.constructor;
  function selection_default2() {
    return new Selection2(this._groups, this._parents);
  }

  // node_modules/d3-transition/src/transition/style.js
  function styleNull(name16, interpolate) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name16), string1 = (this.style.removeProperty(name16), styleValue(this, name16));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : interpolate0 = interpolate(string00 = string0, string10 = string1);
    };
  }
  function styleRemove2(name16) {
    return function() {
      this.style.removeProperty(name16);
    };
  }
  function styleConstant2(name16, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = styleValue(this, name16);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function styleFunction2(name16, interpolate, value13) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name16), value1 = value13(this), string1 = value1 + "";
      if (value1 == null) string1 = value1 = (this.style.removeProperty(name16), styleValue(this, name16));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function styleMaybeRemove(id5, name16) {
    var on0, on1, listener0, key = "style." + name16, event = "end." + key, remove3;
    return function() {
      var schedule = set2(this, id5), on2 = schedule.on, listener = schedule.value[key] == null ? remove3 || (remove3 = styleRemove2(name16)) : void 0;
      if (on2 !== on0 || listener0 !== listener) (on1 = (on0 = on2).copy()).on(event, listener0 = listener);
      schedule.on = on1;
    };
  }
  function style_default2(name16, value13, priority) {
    var i2 = (name16 += "") === "transform" ? interpolateTransformCss : interpolate_default;
    return value13 == null ? this.styleTween(name16, styleNull(name16, i2)).on("end.style." + name16, styleRemove2(name16)) : typeof value13 === "function" ? this.styleTween(name16, styleFunction2(name16, i2, tweenValue(this, "style." + name16, value13))).each(styleMaybeRemove(this._id, name16)) : this.styleTween(name16, styleConstant2(name16, i2, value13), priority).on("end.style." + name16, null);
  }

  // node_modules/d3-transition/src/transition/styleTween.js
  function styleInterpolate(name16, i2, priority) {
    return function(t) {
      this.style.setProperty(name16, i2.call(this, t), priority);
    };
  }
  function styleTween(name16, value13, priority) {
    var t, i0;
    function tween() {
      var i2 = value13.apply(this, arguments);
      if (i2 !== i0) t = (i0 = i2) && styleInterpolate(name16, i2, priority);
      return t;
    }
    tween._value = value13;
    return tween;
  }
  function styleTween_default(name16, value13, priority) {
    var key = "style." + (name16 += "");
    if (arguments.length < 2) return (key = this.tween(key)) && key._value;
    if (value13 == null) return this.tween(key, null);
    if (typeof value13 !== "function") throw new Error();
    return this.tween(key, styleTween(name16, value13, priority == null ? "" : priority));
  }

  // node_modules/d3-transition/src/transition/text.js
  function textConstant2(value13) {
    return function() {
      this.textContent = value13;
    };
  }
  function textFunction2(value13) {
    return function() {
      var value1 = value13(this);
      this.textContent = value1 == null ? "" : value1;
    };
  }
  function text_default2(value13) {
    return this.tween("text", typeof value13 === "function" ? textFunction2(tweenValue(this, "text", value13)) : textConstant2(value13 == null ? "" : value13 + ""));
  }

  // node_modules/d3-transition/src/transition/textTween.js
  function textInterpolate(i2) {
    return function(t) {
      this.textContent = i2.call(this, t);
    };
  }
  function textTween(value13) {
    var t0, i0;
    function tween() {
      var i2 = value13.apply(this, arguments);
      if (i2 !== i0) t0 = (i0 = i2) && textInterpolate(i2);
      return t0;
    }
    tween._value = value13;
    return tween;
  }
  function textTween_default(value13) {
    var key = "text";
    if (arguments.length < 1) return (key = this.tween(key)) && key._value;
    if (value13 == null) return this.tween(key, null);
    if (typeof value13 !== "function") throw new Error();
    return this.tween(key, textTween(value13));
  }

  // node_modules/d3-transition/src/transition/transition.js
  function transition_default() {
    var name16 = this._name, id0 = this._id, id1 = newId();
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          var inherit2 = get5(node, id0);
          schedule_default(node, name16, id1, i2, group3, {
            time: inherit2.time + inherit2.delay + inherit2.duration,
            delay: 0,
            duration: inherit2.duration,
            ease: inherit2.ease
          });
        }
      }
    }
    return new Transition(groups, this._parents, name16, id1);
  }

  // node_modules/d3-transition/src/transition/end.js
  function end_default() {
    var on0, on1, that = this, id5 = that._id, size4 = that.size();
    return new Promise(function(resolve, reject) {
      var cancel = { value: reject }, end = { value: function() {
        if (--size4 === 0) resolve();
      } };
      that.each(function() {
        var schedule = set2(this, id5), on2 = schedule.on;
        if (on2 !== on0) {
          on1 = (on0 = on2).copy();
          on1._.cancel.push(cancel);
          on1._.interrupt.push(cancel);
          on1._.end.push(end);
        }
        schedule.on = on1;
      });
      if (size4 === 0) resolve();
    });
  }

  // node_modules/d3-transition/src/transition/index.js
  var id4 = 0;
  function Transition(groups, parents, name16, id5) {
    this._groups = groups;
    this._parents = parents;
    this._name = name16;
    this._id = id5;
  }
  function transition(name16) {
    return selection_default().transition(name16);
  }
  function newId() {
    return ++id4;
  }
  var selection_prototype = selection_default.prototype;
  Transition.prototype = transition.prototype = {
    constructor: Transition,
    select: select_default2,
    selectAll: selectAll_default2,
    selectChild: selection_prototype.selectChild,
    selectChildren: selection_prototype.selectChildren,
    filter: filter_default2,
    merge: merge_default2,
    selection: selection_default2,
    transition: transition_default,
    call: selection_prototype.call,
    nodes: selection_prototype.nodes,
    node: selection_prototype.node,
    size: selection_prototype.size,
    empty: selection_prototype.empty,
    each: selection_prototype.each,
    on: on_default2,
    attr: attr_default2,
    attrTween: attrTween_default,
    style: style_default2,
    styleTween: styleTween_default,
    text: text_default2,
    textTween: textTween_default,
    remove: remove_default2,
    tween: tween_default,
    delay: delay_default,
    duration: duration_default,
    ease: ease_default,
    easeVarying: easeVarying_default,
    end: end_default,
    [Symbol.iterator]: selection_prototype[Symbol.iterator]
  };

  // node_modules/d3-ease/src/cubic.js
  function cubicInOut(t) {
    return ((t *= 2) <= 1 ? t * t * t : (t -= 2) * t * t + 2) / 2;
  }

  // node_modules/d3-transition/src/selection/transition.js
  var defaultTiming = {
    time: null,
    // Set on use.
    delay: 0,
    duration: 250,
    ease: cubicInOut
  };
  function inherit(node, id5) {
    var timing;
    while (!(timing = node.__transition) || !(timing = timing[id5])) {
      if (!(node = node.parentNode)) {
        throw new Error(`transition ${id5} not found`);
      }
    }
    return timing;
  }
  function transition_default2(name16) {
    var id5, timing;
    if (name16 instanceof Transition) {
      id5 = name16._id, name16 = name16._name;
    } else {
      id5 = newId(), (timing = defaultTiming).time = now(), name16 = name16 == null ? null : name16 + "";
    }
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          schedule_default(node, name16, id5, i2, group3, timing || inherit(node, id5));
        }
      }
    }
    return new Transition(groups, this._parents, name16, id5);
  }

  // node_modules/d3-transition/src/selection/index.js
  selection_default.prototype.interrupt = interrupt_default2;
  selection_default.prototype.transition = transition_default2;

  // node_modules/d3-brush/src/brush.js
  var { abs: abs2, max: max6, min: min5 } = Math;
  function number1(e) {
    return [+e[0], +e[1]];
  }
  function number22(e) {
    return [number1(e[0]), number1(e[1])];
  }
  var X = {
    name: "x",
    handles: ["w", "e"].map(type),
    input: function(x7, e) {
      return x7 == null ? null : [[+x7[0], e[0][1]], [+x7[1], e[1][1]]];
    },
    output: function(xy) {
      return xy && [xy[0][0], xy[1][0]];
    }
  };
  var Y = {
    name: "y",
    handles: ["n", "s"].map(type),
    input: function(y6, e) {
      return y6 == null ? null : [[e[0][0], +y6[0]], [e[1][0], +y6[1]]];
    },
    output: function(xy) {
      return xy && [xy[0][1], xy[1][1]];
    }
  };
  var XY = {
    name: "xy",
    handles: ["n", "w", "e", "s", "nw", "ne", "sw", "se"].map(type),
    input: function(xy) {
      return xy == null ? null : number22(xy);
    },
    output: function(xy) {
      return xy;
    }
  };
  function type(t) {
    return { type: t };
  }

  // node_modules/d3-path/src/path.js
  var pi2 = Math.PI;
  var tau = 2 * pi2;
  var epsilon2 = 1e-6;
  var tauEpsilon = tau - epsilon2;
  function Path2() {
    this._x0 = this._y0 = // start of current subpath
    this._x1 = this._y1 = null;
    this._ = "";
  }
  function path() {
    return new Path2();
  }
  Path2.prototype = path.prototype = {
    constructor: Path2,
    moveTo: function(x7, y6) {
      this._ += "M" + (this._x0 = this._x1 = +x7) + "," + (this._y0 = this._y1 = +y6);
    },
    closePath: function() {
      if (this._x1 !== null) {
        this._x1 = this._x0, this._y1 = this._y0;
        this._ += "Z";
      }
    },
    lineTo: function(x7, y6) {
      this._ += "L" + (this._x1 = +x7) + "," + (this._y1 = +y6);
    },
    quadraticCurveTo: function(x1, y1, x7, y6) {
      this._ += "Q" + +x1 + "," + +y1 + "," + (this._x1 = +x7) + "," + (this._y1 = +y6);
    },
    bezierCurveTo: function(x1, y1, x22, y22, x7, y6) {
      this._ += "C" + +x1 + "," + +y1 + "," + +x22 + "," + +y22 + "," + (this._x1 = +x7) + "," + (this._y1 = +y6);
    },
    arcTo: function(x1, y1, x22, y22, r) {
      x1 = +x1, y1 = +y1, x22 = +x22, y22 = +y22, r = +r;
      var x0 = this._x1, y0 = this._y1, x21 = x22 - x1, y21 = y22 - y1, x01 = x0 - x1, y01 = y0 - y1, l01_2 = x01 * x01 + y01 * y01;
      if (r < 0) throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + (this._x1 = x1) + "," + (this._y1 = y1);
      } else if (!(l01_2 > epsilon2)) ;
      else if (!(Math.abs(y01 * x21 - y21 * x01) > epsilon2) || !r) {
        this._ += "L" + (this._x1 = x1) + "," + (this._y1 = y1);
      } else {
        var x20 = x22 - x0, y20 = y22 - y0, l21_2 = x21 * x21 + y21 * y21, l20_2 = x20 * x20 + y20 * y20, l21 = Math.sqrt(l21_2), l01 = Math.sqrt(l01_2), l = r * Math.tan((pi2 - Math.acos((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2), t01 = l / l01, t21 = l / l21;
        if (Math.abs(t01 - 1) > epsilon2) {
          this._ += "L" + (x1 + t01 * x01) + "," + (y1 + t01 * y01);
        }
        this._ += "A" + r + "," + r + ",0,0," + +(y01 * x20 > x01 * y20) + "," + (this._x1 = x1 + t21 * x21) + "," + (this._y1 = y1 + t21 * y21);
      }
    },
    arc: function(x7, y6, r, a0, a1, ccw) {
      x7 = +x7, y6 = +y6, r = +r, ccw = !!ccw;
      var dx = r * Math.cos(a0), dy3 = r * Math.sin(a0), x0 = x7 + dx, y0 = y6 + dy3, cw = 1 ^ ccw, da = ccw ? a0 - a1 : a1 - a0;
      if (r < 0) throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + x0 + "," + y0;
      } else if (Math.abs(this._x1 - x0) > epsilon2 || Math.abs(this._y1 - y0) > epsilon2) {
        this._ += "L" + x0 + "," + y0;
      }
      if (!r) return;
      if (da < 0) da = da % tau + tau;
      if (da > tauEpsilon) {
        this._ += "A" + r + "," + r + ",0,1," + cw + "," + (x7 - dx) + "," + (y6 - dy3) + "A" + r + "," + r + ",0,1," + cw + "," + (this._x1 = x0) + "," + (this._y1 = y0);
      } else if (da > epsilon2) {
        this._ += "A" + r + "," + r + ",0," + +(da >= pi2) + "," + cw + "," + (this._x1 = x7 + r * Math.cos(a1)) + "," + (this._y1 = y6 + r * Math.sin(a1));
      }
    },
    rect: function(x7, y6, w, h) {
      this._ += "M" + (this._x0 = this._x1 = +x7) + "," + (this._y0 = this._y1 = +y6) + "h" + +w + "v" + +h + "h" + -w + "Z";
    },
    toString: function() {
      return this._;
    }
  };
  var path_default = path;

  // node_modules/d3-format/src/formatDecimal.js
  function formatDecimal_default(x7) {
    return Math.abs(x7 = Math.round(x7)) >= 1e21 ? x7.toLocaleString("en").replace(/,/g, "") : x7.toString(10);
  }
  function formatDecimalParts(x7, p2) {
    if ((i2 = (x7 = p2 ? x7.toExponential(p2 - 1) : x7.toExponential()).indexOf("e")) < 0) return null;
    var i2, coefficient = x7.slice(0, i2);
    return [
      coefficient.length > 1 ? coefficient[0] + coefficient.slice(2) : coefficient,
      +x7.slice(i2 + 1)
    ];
  }

  // node_modules/d3-format/src/exponent.js
  function exponent_default(x7) {
    return x7 = formatDecimalParts(Math.abs(x7)), x7 ? x7[1] : NaN;
  }

  // node_modules/d3-format/src/formatGroup.js
  function formatGroup_default(grouping, thousands) {
    return function(value13, width14) {
      var i2 = value13.length, t = [], j = 0, g = grouping[0], length9 = 0;
      while (i2 > 0 && g > 0) {
        if (length9 + g + 1 > width14) g = Math.max(1, width14 - length9);
        t.push(value13.substring(i2 -= g, i2 + g));
        if ((length9 += g + 1) > width14) break;
        g = grouping[j = (j + 1) % grouping.length];
      }
      return t.reverse().join(thousands);
    };
  }

  // node_modules/d3-format/src/formatNumerals.js
  function formatNumerals_default(numerals) {
    return function(value13) {
      return value13.replace(/[0-9]/g, function(i2) {
        return numerals[+i2];
      });
    };
  }

  // node_modules/d3-format/src/formatSpecifier.js
  var re = /^(?:(.)?([<>=^]))?([+\-( ])?([$#])?(0)?(\d+)?(,)?(\.\d+)?(~)?([a-z%])?$/i;
  function formatSpecifier(specifier) {
    if (!(match = re.exec(specifier))) throw new Error("invalid format: " + specifier);
    var match;
    return new FormatSpecifier({
      fill: match[1],
      align: match[2],
      sign: match[3],
      symbol: match[4],
      zero: match[5],
      width: match[6],
      comma: match[7],
      precision: match[8] && match[8].slice(1),
      trim: match[9],
      type: match[10]
    });
  }
  formatSpecifier.prototype = FormatSpecifier.prototype;
  function FormatSpecifier(specifier) {
    this.fill = specifier.fill === void 0 ? " " : specifier.fill + "";
    this.align = specifier.align === void 0 ? ">" : specifier.align + "";
    this.sign = specifier.sign === void 0 ? "-" : specifier.sign + "";
    this.symbol = specifier.symbol === void 0 ? "" : specifier.symbol + "";
    this.zero = !!specifier.zero;
    this.width = specifier.width === void 0 ? void 0 : +specifier.width;
    this.comma = !!specifier.comma;
    this.precision = specifier.precision === void 0 ? void 0 : +specifier.precision;
    this.trim = !!specifier.trim;
    this.type = specifier.type === void 0 ? "" : specifier.type + "";
  }
  FormatSpecifier.prototype.toString = function() {
    return this.fill + this.align + this.sign + this.symbol + (this.zero ? "0" : "") + (this.width === void 0 ? "" : Math.max(1, this.width | 0)) + (this.comma ? "," : "") + (this.precision === void 0 ? "" : "." + Math.max(0, this.precision | 0)) + (this.trim ? "~" : "") + this.type;
  };

  // node_modules/d3-format/src/formatTrim.js
  function formatTrim_default(s) {
    out: for (var n = s.length, i2 = 1, i0 = -1, i1; i2 < n; ++i2) {
      switch (s[i2]) {
        case ".":
          i0 = i1 = i2;
          break;
        case "0":
          if (i0 === 0) i0 = i2;
          i1 = i2;
          break;
        default:
          if (!+s[i2]) break out;
          if (i0 > 0) i0 = 0;
          break;
      }
    }
    return i0 > 0 ? s.slice(0, i0) + s.slice(i1 + 1) : s;
  }

  // node_modules/d3-format/src/formatPrefixAuto.js
  var prefixExponent;
  function formatPrefixAuto_default(x7, p2) {
    var d6 = formatDecimalParts(x7, p2);
    if (!d6) return x7 + "";
    var coefficient = d6[0], exponent = d6[1], i2 = exponent - (prefixExponent = Math.max(-8, Math.min(8, Math.floor(exponent / 3))) * 3) + 1, n = coefficient.length;
    return i2 === n ? coefficient : i2 > n ? coefficient + new Array(i2 - n + 1).join("0") : i2 > 0 ? coefficient.slice(0, i2) + "." + coefficient.slice(i2) : "0." + new Array(1 - i2).join("0") + formatDecimalParts(x7, Math.max(0, p2 + i2 - 1))[0];
  }

  // node_modules/d3-format/src/formatRounded.js
  function formatRounded_default(x7, p2) {
    var d6 = formatDecimalParts(x7, p2);
    if (!d6) return x7 + "";
    var coefficient = d6[0], exponent = d6[1];
    return exponent < 0 ? "0." + new Array(-exponent).join("0") + coefficient : coefficient.length > exponent + 1 ? coefficient.slice(0, exponent + 1) + "." + coefficient.slice(exponent + 1) : coefficient + new Array(exponent - coefficient.length + 2).join("0");
  }

  // node_modules/d3-format/src/formatTypes.js
  var formatTypes_default = {
    "%": (x7, p2) => (x7 * 100).toFixed(p2),
    "b": (x7) => Math.round(x7).toString(2),
    "c": (x7) => x7 + "",
    "d": formatDecimal_default,
    "e": (x7, p2) => x7.toExponential(p2),
    "f": (x7, p2) => x7.toFixed(p2),
    "g": (x7, p2) => x7.toPrecision(p2),
    "o": (x7) => Math.round(x7).toString(8),
    "p": (x7, p2) => formatRounded_default(x7 * 100, p2),
    "r": formatRounded_default,
    "s": formatPrefixAuto_default,
    "X": (x7) => Math.round(x7).toString(16).toUpperCase(),
    "x": (x7) => Math.round(x7).toString(16)
  };

  // node_modules/d3-format/src/identity.js
  function identity_default2(x7) {
    return x7;
  }

  // node_modules/d3-format/src/locale.js
  var map24 = Array.prototype.map;
  var prefixes = ["y", "z", "a", "f", "p", "n", "\xB5", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y"];
  function locale_default(locale2) {
    var group3 = locale2.grouping === void 0 || locale2.thousands === void 0 ? identity_default2 : formatGroup_default(map24.call(locale2.grouping, Number), locale2.thousands + ""), currencyPrefix = locale2.currency === void 0 ? "" : locale2.currency[0] + "", currencySuffix = locale2.currency === void 0 ? "" : locale2.currency[1] + "", decimal = locale2.decimal === void 0 ? "." : locale2.decimal + "", numerals = locale2.numerals === void 0 ? identity_default2 : formatNumerals_default(map24.call(locale2.numerals, String)), percent = locale2.percent === void 0 ? "%" : locale2.percent + "", minus = locale2.minus === void 0 ? "\u2212" : locale2.minus + "", nan2 = locale2.nan === void 0 ? "NaN" : locale2.nan + "";
    function newFormat(specifier) {
      specifier = formatSpecifier(specifier);
      var fill9 = specifier.fill, align = specifier.align, sign2 = specifier.sign, symbol = specifier.symbol, zero4 = specifier.zero, width14 = specifier.width, comma = specifier.comma, precision = specifier.precision, trim2 = specifier.trim, type2 = specifier.type;
      if (type2 === "n") comma = true, type2 = "g";
      else if (!formatTypes_default[type2]) precision === void 0 && (precision = 12), trim2 = true, type2 = "g";
      if (zero4 || fill9 === "0" && align === "=") zero4 = true, fill9 = "0", align = "=";
      var prefix = symbol === "$" ? currencyPrefix : symbol === "#" && /[boxX]/.test(type2) ? "0" + type2.toLowerCase() : "", suffix = symbol === "$" ? currencySuffix : /[%p]/.test(type2) ? percent : "";
      var formatType = formatTypes_default[type2], maybeSuffix = /[defgprs%]/.test(type2);
      precision = precision === void 0 ? 6 : /[gprs]/.test(type2) ? Math.max(1, Math.min(21, precision)) : Math.max(0, Math.min(20, precision));
      function format2(value13) {
        var valuePrefix = prefix, valueSuffix = suffix, i2, n, c;
        if (type2 === "c") {
          valueSuffix = formatType(value13) + valueSuffix;
          value13 = "";
        } else {
          value13 = +value13;
          var valueNegative = value13 < 0 || 1 / value13 < 0;
          value13 = isNaN(value13) ? nan2 : formatType(Math.abs(value13), precision);
          if (trim2) value13 = formatTrim_default(value13);
          if (valueNegative && +value13 === 0 && sign2 !== "+") valueNegative = false;
          valuePrefix = (valueNegative ? sign2 === "(" ? sign2 : minus : sign2 === "-" || sign2 === "(" ? "" : sign2) + valuePrefix;
          valueSuffix = (type2 === "s" ? prefixes[8 + prefixExponent / 3] : "") + valueSuffix + (valueNegative && sign2 === "(" ? ")" : "");
          if (maybeSuffix) {
            i2 = -1, n = value13.length;
            while (++i2 < n) {
              if (c = value13.charCodeAt(i2), 48 > c || c > 57) {
                valueSuffix = (c === 46 ? decimal + value13.slice(i2 + 1) : value13.slice(i2)) + valueSuffix;
                value13 = value13.slice(0, i2);
                break;
              }
            }
          }
        }
        if (comma && !zero4) value13 = group3(value13, Infinity);
        var length9 = valuePrefix.length + value13.length + valueSuffix.length, padding = length9 < width14 ? new Array(width14 - length9 + 1).join(fill9) : "";
        if (comma && zero4) value13 = group3(padding + value13, padding.length ? width14 - valueSuffix.length : Infinity), padding = "";
        switch (align) {
          case "<":
            value13 = valuePrefix + value13 + valueSuffix + padding;
            break;
          case "=":
            value13 = valuePrefix + padding + value13 + valueSuffix;
            break;
          case "^":
            value13 = padding.slice(0, length9 = padding.length >> 1) + valuePrefix + value13 + valueSuffix + padding.slice(length9);
            break;
          default:
            value13 = padding + valuePrefix + value13 + valueSuffix;
            break;
        }
        return numerals(value13);
      }
      format2.toString = function() {
        return specifier + "";
      };
      return format2;
    }
    function formatPrefix2(specifier, value13) {
      var f = newFormat((specifier = formatSpecifier(specifier), specifier.type = "f", specifier)), e = Math.max(-8, Math.min(8, Math.floor(exponent_default(value13) / 3))) * 3, k = Math.pow(10, -e), prefix = prefixes[8 + e / 3];
      return function(value14) {
        return f(k * value14) + prefix;
      };
    }
    return {
      format: newFormat,
      formatPrefix: formatPrefix2
    };
  }

  // node_modules/d3-format/src/defaultLocale.js
  var locale;
  var format;
  var formatPrefix;
  defaultLocale({
    thousands: ",",
    grouping: [3],
    currency: ["$", ""]
  });
  function defaultLocale(definition) {
    locale = locale_default(definition);
    format = locale.format;
    formatPrefix = locale.formatPrefix;
    return locale;
  }

  // node_modules/d3-format/src/precisionFixed.js
  function precisionFixed_default(step4) {
    return Math.max(0, -exponent_default(Math.abs(step4)));
  }

  // node_modules/d3-format/src/precisionPrefix.js
  function precisionPrefix_default(step4, value13) {
    return Math.max(0, Math.max(-8, Math.min(8, Math.floor(exponent_default(value13) / 3))) * 3 - exponent_default(Math.abs(step4)));
  }

  // node_modules/d3-format/src/precisionRound.js
  function precisionRound_default(step4, max7) {
    step4 = Math.abs(step4), max7 = Math.abs(max7) - step4;
    return Math.max(0, exponent_default(max7) - exponent_default(step4)) + 1;
  }

  // node_modules/d3-scale/src/init.js
  function initRange(domain, range3) {
    switch (arguments.length) {
      case 0:
        break;
      case 1:
        this.range(domain);
        break;
      default:
        this.range(range3).domain(domain);
        break;
    }
    return this;
  }

  // node_modules/d3-scale/src/constant.js
  function constants(x7) {
    return function() {
      return x7;
    };
  }

  // node_modules/d3-scale/src/number.js
  function number3(x7) {
    return +x7;
  }

  // node_modules/d3-scale/src/continuous.js
  var unit2 = [0, 1];
  function identity12(x7) {
    return x7;
  }
  function normalize2(a2, b2) {
    return (b2 -= a2 = +a2) ? function(x7) {
      return (x7 - a2) / b2;
    } : constants(isNaN(b2) ? NaN : 0.5);
  }
  function clamper(a2, b2) {
    var t;
    if (a2 > b2) t = a2, a2 = b2, b2 = t;
    return function(x7) {
      return Math.max(a2, Math.min(b2, x7));
    };
  }
  function bimap3(domain, range3, interpolate) {
    var d0 = domain[0], d1 = domain[1], r0 = range3[0], r1 = range3[1];
    if (d1 < d0) d0 = normalize2(d1, d0), r0 = interpolate(r1, r0);
    else d0 = normalize2(d0, d1), r0 = interpolate(r0, r1);
    return function(x7) {
      return r0(d0(x7));
    };
  }
  function polymap(domain, range3, interpolate) {
    var j = Math.min(domain.length, range3.length) - 1, d6 = new Array(j), r = new Array(j), i2 = -1;
    if (domain[j] < domain[0]) {
      domain = domain.slice().reverse();
      range3 = range3.slice().reverse();
    }
    while (++i2 < j) {
      d6[i2] = normalize2(domain[i2], domain[i2 + 1]);
      r[i2] = interpolate(range3[i2], range3[i2 + 1]);
    }
    return function(x7) {
      var i3 = bisect_default(domain, x7, 1, j) - 1;
      return r[i3](d6[i3](x7));
    };
  }
  function copy2(source2, target7) {
    return target7.domain(source2.domain()).range(source2.range()).interpolate(source2.interpolate()).clamp(source2.clamp()).unknown(source2.unknown());
  }
  function transformer() {
    var domain = unit2, range3 = unit2, interpolate = value_default, transform3, untransform, unknown, clamp = identity12, piecewise, output2, input3;
    function rescale() {
      var n = Math.min(domain.length, range3.length);
      if (clamp !== identity12) clamp = clamper(domain[0], domain[n - 1]);
      piecewise = n > 2 ? polymap : bimap3;
      output2 = input3 = null;
      return scale;
    }
    function scale(x7) {
      return x7 == null || isNaN(x7 = +x7) ? unknown : (output2 || (output2 = piecewise(domain.map(transform3), range3, interpolate)))(transform3(clamp(x7)));
    }
    scale.invert = function(y6) {
      return clamp(untransform((input3 || (input3 = piecewise(range3, domain.map(transform3), number_default)))(y6)));
    };
    scale.domain = function(_) {
      return arguments.length ? (domain = Array.from(_, number3), rescale()) : domain.slice();
    };
    scale.range = function(_) {
      return arguments.length ? (range3 = Array.from(_), rescale()) : range3.slice();
    };
    scale.rangeRound = function(_) {
      return range3 = Array.from(_), interpolate = round_default, rescale();
    };
    scale.clamp = function(_) {
      return arguments.length ? (clamp = _ ? true : identity12, rescale()) : clamp !== identity12;
    };
    scale.interpolate = function(_) {
      return arguments.length ? (interpolate = _, rescale()) : interpolate;
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    return function(t, u2) {
      transform3 = t, untransform = u2;
      return rescale();
    };
  }
  function continuous() {
    return transformer()(identity12, identity12);
  }

  // node_modules/d3-scale/src/tickFormat.js
  function tickFormat(start3, stop, count, specifier) {
    var step4 = tickStep(start3, stop, count), precision;
    specifier = formatSpecifier(specifier == null ? ",f" : specifier);
    switch (specifier.type) {
      case "s": {
        var value13 = Math.max(Math.abs(start3), Math.abs(stop));
        if (specifier.precision == null && !isNaN(precision = precisionPrefix_default(step4, value13))) specifier.precision = precision;
        return formatPrefix(specifier, value13);
      }
      case "":
      case "e":
      case "g":
      case "p":
      case "r": {
        if (specifier.precision == null && !isNaN(precision = precisionRound_default(step4, Math.max(Math.abs(start3), Math.abs(stop))))) specifier.precision = precision - (specifier.type === "e");
        break;
      }
      case "f":
      case "%": {
        if (specifier.precision == null && !isNaN(precision = precisionFixed_default(step4))) specifier.precision = precision - (specifier.type === "%") * 2;
        break;
      }
    }
    return format(specifier);
  }

  // node_modules/d3-scale/src/linear.js
  function linearish(scale) {
    var domain = scale.domain;
    scale.ticks = function(count) {
      var d6 = domain();
      return ticks(d6[0], d6[d6.length - 1], count == null ? 10 : count);
    };
    scale.tickFormat = function(count, specifier) {
      var d6 = domain();
      return tickFormat(d6[0], d6[d6.length - 1], count == null ? 10 : count, specifier);
    };
    scale.nice = function(count) {
      if (count == null) count = 10;
      var d6 = domain();
      var i0 = 0;
      var i1 = d6.length - 1;
      var start3 = d6[i0];
      var stop = d6[i1];
      var prestep;
      var step4;
      var maxIter = 10;
      if (stop < start3) {
        step4 = start3, start3 = stop, stop = step4;
        step4 = i0, i0 = i1, i1 = step4;
      }
      while (maxIter-- > 0) {
        step4 = tickIncrement(start3, stop, count);
        if (step4 === prestep) {
          d6[i0] = start3;
          d6[i1] = stop;
          return domain(d6);
        } else if (step4 > 0) {
          start3 = Math.floor(start3 / step4) * step4;
          stop = Math.ceil(stop / step4) * step4;
        } else if (step4 < 0) {
          start3 = Math.ceil(start3 * step4) / step4;
          stop = Math.floor(stop * step4) / step4;
        } else {
          break;
        }
        prestep = step4;
      }
      return scale;
    };
    return scale;
  }
  function linear2() {
    var scale = continuous();
    scale.copy = function() {
      return copy2(scale, linear2());
    };
    initRange.apply(scale, arguments);
    return linearish(scale);
  }

  // node_modules/d3-shape/src/constant.js
  function constant_default4(x7) {
    return function constant() {
      return x7;
    };
  }

  // node_modules/d3-shape/src/array.js
  var slice3 = Array.prototype.slice;
  function array_default(x7) {
    return typeof x7 === "object" && "length" in x7 ? x7 : Array.from(x7);
  }

  // node_modules/d3-shape/src/curve/linear.js
  function Linear(context) {
    this._context = context;
  }
  Linear.prototype = {
    areaStart: function() {
      this._line = 0;
    },
    areaEnd: function() {
      this._line = NaN;
    },
    lineStart: function() {
      this._point = 0;
    },
    lineEnd: function() {
      if (this._line || this._line !== 0 && this._point === 1) this._context.closePath();
      this._line = 1 - this._line;
    },
    point: function(x7, y6) {
      x7 = +x7, y6 = +y6;
      switch (this._point) {
        case 0:
          this._point = 1;
          this._line ? this._context.lineTo(x7, y6) : this._context.moveTo(x7, y6);
          break;
        case 1:
          this._point = 2;
        // falls through
        default:
          this._context.lineTo(x7, y6);
          break;
      }
    }
  };
  function linear_default(context) {
    return new Linear(context);
  }

  // node_modules/d3-shape/src/point.js
  function x2(p2) {
    return p2[0];
  }
  function y2(p2) {
    return p2[1];
  }

  // node_modules/d3-shape/src/line.js
  function line_default(x7, y6) {
    var defined = constant_default4(true), context = null, curve = linear_default, output2 = null;
    x7 = typeof x7 === "function" ? x7 : x7 === void 0 ? x2 : constant_default4(x7);
    y6 = typeof y6 === "function" ? y6 : y6 === void 0 ? y2 : constant_default4(y6);
    function line(data) {
      var i2, n = (data = array_default(data)).length, d6, defined0 = false, buffer;
      if (context == null) output2 = curve(buffer = path_default());
      for (i2 = 0; i2 <= n; ++i2) {
        if (!(i2 < n && defined(d6 = data[i2], i2, data)) === defined0) {
          if (defined0 = !defined0) output2.lineStart();
          else output2.lineEnd();
        }
        if (defined0) output2.point(+x7(d6, i2, data), +y6(d6, i2, data));
      }
      if (buffer) return output2 = null, buffer + "" || null;
    }
    line.x = function(_) {
      return arguments.length ? (x7 = typeof _ === "function" ? _ : constant_default4(+_), line) : x7;
    };
    line.y = function(_) {
      return arguments.length ? (y6 = typeof _ === "function" ? _ : constant_default4(+_), line) : y6;
    };
    line.defined = function(_) {
      return arguments.length ? (defined = typeof _ === "function" ? _ : constant_default4(!!_), line) : defined;
    };
    line.curve = function(_) {
      return arguments.length ? (curve = _, context != null && (output2 = curve(context)), line) : curve;
    };
    line.context = function(_) {
      return arguments.length ? (_ == null ? context = output2 = null : output2 = curve(context = _), line) : context;
    };
    return line;
  }

  // node_modules/d3-zoom/src/transform.js
  function Transform(k, x7, y6) {
    this.k = k;
    this.x = x7;
    this.y = y6;
  }
  Transform.prototype = {
    constructor: Transform,
    scale: function(k) {
      return k === 1 ? this : new Transform(this.k * k, this.x, this.y);
    },
    translate: function(x7, y6) {
      return x7 === 0 & y6 === 0 ? this : new Transform(this.k, this.x + this.k * x7, this.y + this.k * y6);
    },
    apply: function(point) {
      return [point[0] * this.k + this.x, point[1] * this.k + this.y];
    },
    applyX: function(x7) {
      return x7 * this.k + this.x;
    },
    applyY: function(y6) {
      return y6 * this.k + this.y;
    },
    invert: function(location2) {
      return [(location2[0] - this.x) / this.k, (location2[1] - this.y) / this.k];
    },
    invertX: function(x7) {
      return (x7 - this.x) / this.k;
    },
    invertY: function(y6) {
      return (y6 - this.y) / this.k;
    },
    rescaleX: function(x7) {
      return x7.copy().domain(x7.range().map(this.invertX, this).map(x7.invert, x7));
    },
    rescaleY: function(y6) {
      return y6.copy().domain(y6.range().map(this.invertY, this).map(y6.invert, y6));
    },
    toString: function() {
      return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
    }
  };
  var identity13 = new Transform(1, 0, 0);
  transform2.prototype = Transform.prototype;
  function transform2(node) {
    while (!node.__zoom) if (!(node = node.parentNode)) return identity13;
    return node.__zoom;
  }

  // output/D3.Axes/foreign.js
  function axisBottom2(scale) {
    return axisBottom(scale);
  }
  function axisLeft2(scale) {
    return axisLeft(scale);
  }
  function callAxis(selection2) {
    return (axis2) => () => {
      selection2.call(axis2);
    };
  }

  // output/D3.Scales.Linear/foreign.js
  function createLinearScale(config) {
    return () => {
      return linear2().domain(config.domain).range(config.range);
    };
  }
  function applyScale(scale) {
    return (value13) => scale(value13);
  }

  // output/D3Tagless.Capabilities/index.js
  var updateJoin = function(dict) {
    return dict.updateJoin;
  };
  var simpleJoin = function(dict) {
    return dict.simpleJoin;
  };
  var setAttributes = function(dict) {
    return dict.setAttributes;
  };
  var openSelection = function(dict) {
    return dict.openSelection;
  };
  var attach = function(dict) {
    return dict.attach;
  };
  var appendTo = function(dict) {
    return dict.appendTo;
  };

  // output/D3.Examples.BarChart/index.js
  var map25 = /* @__PURE__ */ map(functorArray);
  var minimum2 = /* @__PURE__ */ minimum(ordNumber)(foldableArray);
  var maximum2 = /* @__PURE__ */ maximum(ordNumber)(foldableArray);
  var classed2 = /* @__PURE__ */ classed(toAttrString);
  var width9 = /* @__PURE__ */ width8(toAttrNumber);
  var height9 = /* @__PURE__ */ height8(toAttrNumber);
  var show5 = /* @__PURE__ */ show(showNumber);
  var x3 = /* @__PURE__ */ x(toAttrNumber);
  var y3 = /* @__PURE__ */ y(toAttrNumber);
  var fill2 = /* @__PURE__ */ fill(toAttrString);
  var strokeColor2 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeWidth2 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var innerWidth2 = function(dims) {
    return dims.width - dims.margin.left - dims.margin.right;
  };
  var innerHeight2 = function(dims) {
    return dims.height - dims.margin.top - dims.margin.bottom;
  };
  var defaultDimensions = {
    width: 800,
    height: 400,
    margin: {
      top: 20,
      right: 30,
      bottom: 30,
      left: 40
    }
  };
  var draw = function(dictBind) {
    var bind10 = bind(dictBind);
    return function(dictMonadEffect) {
      var liftEffect10 = liftEffect(dictMonadEffect);
      var Applicative0 = dictMonadEffect.Monad0().Applicative0();
      var pure17 = pure(Applicative0);
      var traverse_9 = traverse_(Applicative0)(foldableArray);
      return function(dictSelectionM) {
        var attach2 = attach(dictSelectionM);
        var appendTo2 = appendTo(dictSelectionM);
        return function(dataPoints) {
          return function(selector) {
            var iWidth = innerWidth2(defaultDimensions);
            var iHeight = innerHeight2(defaultDimensions);
            var xValues = map25(function(v) {
              return v.x;
            })(dataPoints);
            var yValues = map25(function(v) {
              return v.y;
            })(dataPoints);
            var minX = fromMaybe(0)(minimum2(xValues));
            var maxX = fromMaybe(100)(maximum2(xValues));
            var maxY = fromMaybe(100)(maximum2(yValues));
            return bind10(attach2(selector))(function(v) {
              return bind10(appendTo2(v)(Svg.value)([viewBox(0)(0)(defaultDimensions.width)(defaultDimensions.height), classed2("bar-chart"), width9(defaultDimensions.width), height9(defaultDimensions.height)]))(function(svg) {
                return bind10(appendTo2(svg)(Group.value)([transform([function(v1) {
                  return "translate(" + (show5(defaultDimensions.margin.left) + ("," + (show5(defaultDimensions.margin.top) + ")")));
                }])]))(function(chartGroup) {
                  return bind10(liftEffect10(createLinearScale({
                    domain: [minX, maxX],
                    range: [0, iWidth]
                  })))(function(xScale) {
                    return bind10(liftEffect10(createLinearScale({
                      domain: [0, maxY],
                      range: [iHeight, 0]
                    })))(function(yScale) {
                      return bind10(appendTo2(chartGroup)(Group.value)([classed2("x-axis"), transform([function(v1) {
                        return "translate(0," + (show5(iHeight) + ")");
                      }])]))(function(xAxisGroup) {
                        return bind10(appendTo2(chartGroup)(Group.value)([classed2("y-axis")]))(function(yAxisGroup) {
                          return bind10(liftEffect10(callAxis(xAxisGroup)(axisBottom2(xScale))))(function() {
                            return bind10(liftEffect10(callAxis(yAxisGroup)(axisLeft2(yScale))))(function() {
                              var numBars = length4(dataPoints);
                              var barWidth = (function() {
                                var $37 = numBars > 0;
                                if ($37) {
                                  return iWidth / toNumber(numBars) * 0.8;
                                }
                                ;
                                return 0;
                              })();
                              var addBar = function(point) {
                                var xPos = applyScale(xScale)(point.x) - barWidth / 2;
                                var yPos = applyScale(yScale)(point.y);
                                var barHeight = iHeight - yPos;
                                return bind10(appendTo2(chartGroup)(Rect.value)([x3(xPos), y3(yPos), width9(barWidth), height9(barHeight), fill2("#4a90e2"), strokeColor2("#357abd"), strokeWidth2(1), classed2("bar")]))(function() {
                                  return pure17(unit);
                                });
                              };
                              return bind10(traverse_9(addBar)(dataPoints))(function() {
                                return pure17(unit);
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          };
        };
      };
    };
  };

  // output/D3.Examples.Charts.Model/index.js
  var sineWaveData = [{
    x: 0,
    y: 100
  }, {
    x: 5,
    y: 125
  }, {
    x: 10,
    y: 145
  }, {
    x: 15,
    y: 150
  }, {
    x: 20,
    y: 145
  }, {
    x: 25,
    y: 125
  }, {
    x: 30,
    y: 100
  }, {
    x: 35,
    y: 75
  }, {
    x: 40,
    y: 55
  }, {
    x: 45,
    y: 50
  }, {
    x: 50,
    y: 55
  }, {
    x: 55,
    y: 75
  }, {
    x: 60,
    y: 100
  }];
  var scatterData = [{
    x: 10,
    y: 20
  }, {
    x: 25,
    y: 45
  }, {
    x: 40,
    y: 30
  }, {
    x: 55,
    y: 60
  }, {
    x: 70,
    y: 50
  }, {
    x: 85,
    y: 75
  }, {
    x: 100,
    y: 65
  }, {
    x: 115,
    y: 90
  }, {
    x: 130,
    y: 80
  }, {
    x: 145,
    y: 95
  }];
  var monthlySales = [{
    x: 0,
    y: 30
  }, {
    x: 1,
    y: 45
  }, {
    x: 2,
    y: 60
  }, {
    x: 3,
    y: 55
  }, {
    x: 4,
    y: 70
  }, {
    x: 5,
    y: 65
  }, {
    x: 6,
    y: 80
  }, {
    x: 7,
    y: 75
  }, {
    x: 8,
    y: 90
  }, {
    x: 9,
    y: 85
  }, {
    x: 10,
    y: 95
  }, {
    x: 11,
    y: 100
  }];
  var anscombesQuartet = {
    dataset1: [{
      x: 10,
      y: 8.04
    }, {
      x: 8,
      y: 6.95
    }, {
      x: 13,
      y: 7.58
    }, {
      x: 9,
      y: 8.81
    }, {
      x: 11,
      y: 8.33
    }, {
      x: 14,
      y: 9.96
    }, {
      x: 6,
      y: 7.24
    }, {
      x: 4,
      y: 4.26
    }, {
      x: 12,
      y: 10.84
    }, {
      x: 7,
      y: 4.82
    }, {
      x: 5,
      y: 5.68
    }],
    dataset2: [{
      x: 10,
      y: 9.14
    }, {
      x: 8,
      y: 8.14
    }, {
      x: 13,
      y: 8.74
    }, {
      x: 9,
      y: 8.77
    }, {
      x: 11,
      y: 9.26
    }, {
      x: 14,
      y: 8.1
    }, {
      x: 6,
      y: 6.13
    }, {
      x: 4,
      y: 3.1
    }, {
      x: 12,
      y: 9.13
    }, {
      x: 7,
      y: 7.26
    }, {
      x: 5,
      y: 4.74
    }],
    dataset3: [{
      x: 10,
      y: 7.46
    }, {
      x: 8,
      y: 6.77
    }, {
      x: 13,
      y: 12.74
    }, {
      x: 9,
      y: 7.11
    }, {
      x: 11,
      y: 7.81
    }, {
      x: 14,
      y: 8.84
    }, {
      x: 6,
      y: 6.08
    }, {
      x: 4,
      y: 5.39
    }, {
      x: 12,
      y: 8.15
    }, {
      x: 7,
      y: 6.42
    }, {
      x: 5,
      y: 5.73
    }],
    dataset4: [{
      x: 8,
      y: 6.58
    }, {
      x: 8,
      y: 5.76
    }, {
      x: 8,
      y: 7.71
    }, {
      x: 8,
      y: 8.84
    }, {
      x: 8,
      y: 8.47
    }, {
      x: 8,
      y: 7.04
    }, {
      x: 8,
      y: 5.25
    }, {
      x: 19,
      y: 12.5
    }, {
      x: 8,
      y: 5.56
    }, {
      x: 8,
      y: 7.91
    }, {
      x: 8,
      y: 6.89
    }]
  };

  // output/D3.Examples.ChordDiagram/index.js
  var classed3 = /* @__PURE__ */ classed(toAttrString);
  var width10 = /* @__PURE__ */ width8(toAttrNumber);
  var height10 = /* @__PURE__ */ height8(toAttrNumber);
  var show6 = /* @__PURE__ */ show(showNumber);
  var d4 = /* @__PURE__ */ d2(toAttrString);
  var fill3 = /* @__PURE__ */ fill(toAttrString);
  var fillOpacity2 = /* @__PURE__ */ fillOpacity(toAttrNumber);
  var strokeColor3 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeWidth3 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var getSourceIndex = function(d1) {
    return d1.source.index;
  };
  var exampleMatrix = [[0, 5, 3, 2, 1], [5, 0, 4, 1, 2], [3, 4, 0, 3, 4], [2, 1, 3, 0, 5], [1, 2, 4, 5, 0]];
  var exampleLabels = ["Data Structures", "Algorithms", "Patterns", "Testing", "Architecture"];
  var colors = ["#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6"];
  var draw2 = function(dictBind) {
    var bind10 = bind(dictBind);
    return function(dictMonadEffect) {
      var Applicative0 = dictMonadEffect.Monad0().Applicative0();
      var pure17 = pure(Applicative0);
      var traverse_9 = traverse_(Applicative0)(foldableArray);
      return function(dictSelectionM) {
        var attach2 = attach(dictSelectionM);
        var appendTo2 = appendTo(dictSelectionM);
        var simpleJoin2 = simpleJoin(dictSelectionM);
        return function(matrix) {
          return function(v) {
            return function(selector) {
              var dims = {
                width: 800,
                height: 800
              };
              var centerX = dims.width / 2;
              var centerY = dims.height / 2;
              return bind10(attach2(selector))(function(v1) {
                return bind10(appendTo2(v1)(Svg.value)([viewBox(0)(0)(dims.width)(dims.height), classed3("chord-diagram"), width10(dims.width), height10(dims.height)]))(function(svg) {
                  return bind10(appendTo2(svg)(Group.value)([transform([function(v2) {
                    return "translate(" + (show6(centerX) + ("," + (show6(centerY) + ")")));
                  }]), classed3("chord-group")]))(function(centerGroup) {
                    var chordData = chordLayout_(matrix);
                    var groups = chordGroups_(chordData);
                    var chords = chordArray_(chordData);
                    var ribbonGen0 = ribbonGenerator_(unit);
                    var ribbonGen = setRibbonRadius_(ribbonGen0)(290);
                    var arcGen0 = arcGenerator_(unit);
                    var arcGen1 = setArcInnerRadius_(arcGen0)(290);
                    var arcGen = setArcOuterRadius_(arcGen1)(300);
                    return bind10(appendTo2(centerGroup)(Group.value)([classed3("ribbons")]))(function(ribbonsGroup) {
                      var drawRibbon = function(chord) {
                        var sourceIdx = getSourceIndex(chord);
                        var color2 = (function() {
                          var v2 = index2(colors)(sourceIdx);
                          if (v2 instanceof Just) {
                            return v2.value0;
                          }
                          ;
                          if (v2 instanceof Nothing) {
                            return "#999999";
                          }
                          ;
                          throw new Error("Failed pattern match at D3.Examples.ChordDiagram (line 102, column 21 - line 104, column 43): " + [v2.constructor.name]);
                        })();
                        var pathData = ribbonPath_(ribbonGen)(chord);
                        return bind10(appendTo2(ribbonsGroup)(Path.value)([d4(pathData), fill3(color2), fillOpacity2(0.67), strokeColor3("#000000"), strokeWidth3(0.5), classed3("ribbon")]))(function() {
                          return pure17(unit);
                        });
                      };
                      return bind10(traverse_9(drawRibbon)(chords))(function() {
                        return bind10(appendTo2(centerGroup)(Group.value)([classed3("arcs")]))(function(arcsGroup) {
                          return bind10(simpleJoin2(arcsGroup)(Group.value)(groups)(keyIsID_))(function(groupsJoin) {
                            var drawArc = function(idx) {
                              return function(group3) {
                                var color2 = (function() {
                                  var v2 = index2(colors)(idx);
                                  if (v2 instanceof Just) {
                                    return v2.value0;
                                  }
                                  ;
                                  if (v2 instanceof Nothing) {
                                    return "#999999";
                                  }
                                  ;
                                  throw new Error("Failed pattern match at D3.Examples.ChordDiagram (line 125, column 21 - line 127, column 43): " + [v2.constructor.name]);
                                })();
                                var pathData = arcPath_(arcGen)(group3);
                                return bind10(appendTo2(groupsJoin)(Path.value)([d4(pathData), fill3(color2), strokeColor3("#ffffff"), strokeWidth3(2), classed3("arc")]))(function() {
                                  return pure17(unit);
                                });
                              };
                            };
                            return bind10(traverse_9(function(idx) {
                              var v2 = index2(groups)(idx);
                              if (v2 instanceof Just) {
                                return drawArc(idx)(v2.value0);
                              }
                              ;
                              if (v2 instanceof Nothing) {
                                return pure17(unit);
                              }
                              ;
                              throw new Error("Failed pattern match at D3.Examples.ChordDiagram (line 138, column 27 - line 140, column 48): " + [v2.constructor.name]);
                            })(range2(0)(length4(groups) - 1 | 0)))(function() {
                              return pure17(unit);
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            };
          };
        };
      };
    };
  };

  // output/D3.Examples.GUP/index.js
  var andThen2 = /* @__PURE__ */ andThen(semigroupArray);
  var classed4 = /* @__PURE__ */ classed(toAttrString);
  var fill4 = /* @__PURE__ */ fill(toAttrString);
  var y4 = /* @__PURE__ */ y(toAttrNumber);
  var x4 = /* @__PURE__ */ x(toAttrNumberFnI);
  var text7 = /* @__PURE__ */ text6(toAttrStringFn);
  var fontSize2 = /* @__PURE__ */ fontSize(toAttrNumber);
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var keyFunction = unsafeCoerce2;
  var indexIsNumber = unsafeCoerce2;
  var datumIsChar = unsafeCoerce2;
  var exGeneralUpdatePattern = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind10 = bind(Bind1);
    var attach2 = attach(dictSelectionM);
    var appendTo2 = appendTo(dictSelectionM);
    var pure17 = pure(Monad0.Applicative0());
    var openSelection2 = openSelection(dictSelectionM);
    var updateJoin2 = updateJoin(dictSelectionM);
    var discard15 = discard5(Bind1);
    var setAttributes2 = setAttributes(dictSelectionM);
    return function(selector) {
      var xFromIndex2 = function(v) {
        return function(i2) {
          return 50 + indexIsNumber(i2) * 48;
        };
      };
      var transition2 = transitionWithDuration(2e3);
      var update = andThen2([classed4("update"), fill4("gray"), y4(200)])(to(transition2)([x4(xFromIndex2)]));
      var exit = andThen2([classed4("exit"), fill4("brown")])(to(transition2)([y4(400), remove]));
      var enter = andThen2([classed4("enter"), fill4("green"), x4(xFromIndex2), y4(0), text7(function($24) {
        return singleton6(datumIsChar($24));
      }), fontSize2(96)])(to(transition2)([y4(200)]));
      return bind10(attach2(selector))(function(root2) {
        return bind10(appendTo2(root2)(Svg.value)([viewBox(0)(0)(650)(650), classed4("d3svg gup")]))(function(svg) {
          return bind10(appendTo2(svg)(Group.value)([]))(function(letterGroup) {
            return pure17(function(letters) {
              return bind10(openSelection2(letterGroup)("text"))(function(enterSelection) {
                return bind10(updateJoin2(enterSelection)(Text2.value)(letters)(keyFunction))(function(updateSelections) {
                  return discard15(setAttributes2(updateSelections.exit)(exit))(function() {
                    return discard15(setAttributes2(updateSelections.update)(update))(function() {
                      return bind10(appendTo2(updateSelections.enter)(Text2.value)([]))(function(newlyEntered) {
                        return discard15(setAttributes2(newlyEntered)(enter))(function() {
                          return pure17(newlyEntered);
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    };
  };

  // output/D3.Generators.Line/foreign.js
  function createLineGenerator(config) {
    return () => {
      return line_default().x((d6) => config.xScale(d6.x)).y((d6) => config.yScale(d6.y));
    };
  }
  function generateLinePath(generator) {
    return (data) => generator(data);
  }

  // output/D3.Examples.LineChart/index.js
  var map26 = /* @__PURE__ */ map(functorArray);
  var minimum3 = /* @__PURE__ */ minimum(ordNumber)(foldableArray);
  var maximum3 = /* @__PURE__ */ maximum(ordNumber)(foldableArray);
  var classed5 = /* @__PURE__ */ classed(toAttrString);
  var width11 = /* @__PURE__ */ width8(toAttrNumber);
  var height11 = /* @__PURE__ */ height8(toAttrNumber);
  var show7 = /* @__PURE__ */ show(showNumber);
  var d5 = /* @__PURE__ */ d2(toAttrString);
  var fill5 = /* @__PURE__ */ fill(toAttrString);
  var strokeColor4 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeWidth4 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var innerWidth3 = function(dims) {
    return dims.width - dims.margin.left - dims.margin.right;
  };
  var innerHeight3 = function(dims) {
    return dims.height - dims.margin.top - dims.margin.bottom;
  };
  var defaultDimensions2 = {
    width: 800,
    height: 400,
    margin: {
      top: 20,
      right: 30,
      bottom: 30,
      left: 40
    }
  };
  var draw3 = function(dictBind) {
    var bind10 = bind(dictBind);
    return function(dictMonadEffect) {
      var liftEffect10 = liftEffect(dictMonadEffect);
      var pure17 = pure(dictMonadEffect.Monad0().Applicative0());
      return function(dictSelectionM) {
        var attach2 = attach(dictSelectionM);
        var appendTo2 = appendTo(dictSelectionM);
        return function(dataPoints) {
          return function(selector) {
            var iWidth = innerWidth3(defaultDimensions2);
            var iHeight = innerHeight3(defaultDimensions2);
            var xValues = map26(function(v) {
              return v.x;
            })(dataPoints);
            var yValues = map26(function(v) {
              return v.y;
            })(dataPoints);
            var minX = fromMaybe(0)(minimum3(xValues));
            var maxX = fromMaybe(100)(maximum3(xValues));
            var minY = fromMaybe(0)(minimum3(yValues));
            var maxY = fromMaybe(100)(maximum3(yValues));
            return bind10(attach2(selector))(function(v) {
              return bind10(appendTo2(v)(Svg.value)([viewBox(0)(0)(defaultDimensions2.width)(defaultDimensions2.height), classed5("line-chart"), width11(defaultDimensions2.width), height11(defaultDimensions2.height)]))(function(svg) {
                return bind10(appendTo2(svg)(Group.value)([transform([function(v1) {
                  return "translate(" + (show7(defaultDimensions2.margin.left) + ("," + (show7(defaultDimensions2.margin.top) + ")")));
                }])]))(function(chartGroup) {
                  return bind10(liftEffect10(createLinearScale({
                    domain: [minX, maxX],
                    range: [0, iWidth]
                  })))(function(xScale) {
                    return bind10(liftEffect10(createLinearScale({
                      domain: [minY, maxY],
                      range: [iHeight, 0]
                    })))(function(yScale) {
                      return bind10(appendTo2(chartGroup)(Group.value)([classed5("x-axis"), transform([function(v1) {
                        return "translate(0," + (show7(iHeight) + ")");
                      }])]))(function(xAxisGroup) {
                        return bind10(appendTo2(chartGroup)(Group.value)([classed5("y-axis")]))(function(yAxisGroup) {
                          return bind10(liftEffect10(callAxis(xAxisGroup)(axisBottom2(xScale))))(function() {
                            return bind10(liftEffect10(callAxis(yAxisGroup)(axisLeft2(yScale))))(function() {
                              return bind10(liftEffect10(createLineGenerator({
                                xScale,
                                yScale
                              })))(function(lineGen) {
                                var pathData = generateLinePath(lineGen)(dataPoints);
                                return bind10(appendTo2(chartGroup)(Path.value)([d5(pathData), fill5("none"), strokeColor4("#4a90e2"), strokeWidth4(2), classed5("line")]))(function() {
                                  return pure17(unit);
                                });
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          };
        };
      };
    };
  };

  // output/D3.Examples.ScatterPlot/index.js
  var classed6 = /* @__PURE__ */ classed(toAttrString);
  var width12 = /* @__PURE__ */ width8(toAttrNumber);
  var height12 = /* @__PURE__ */ height8(toAttrNumber);
  var show8 = /* @__PURE__ */ show(showNumber);
  var x5 = /* @__PURE__ */ x(toAttrNumber);
  var y5 = /* @__PURE__ */ y(toAttrNumber);
  var text8 = /* @__PURE__ */ text6(toAttrString);
  var textAnchor2 = /* @__PURE__ */ textAnchor(toAttrString);
  var fontSize3 = /* @__PURE__ */ fontSize(toAttrNumber);
  var cx2 = /* @__PURE__ */ cx(toAttrNumber);
  var cy2 = /* @__PURE__ */ cy(toAttrNumber);
  var radius2 = /* @__PURE__ */ radius(toAttrNumber);
  var fill6 = /* @__PURE__ */ fill(toAttrString);
  var strokeColor5 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeWidth5 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var map27 = /* @__PURE__ */ map(functorArray);
  var minimum4 = /* @__PURE__ */ minimum(ordNumber)(foldableArray);
  var maximum4 = /* @__PURE__ */ maximum(ordNumber)(foldableArray);
  var innerWidth4 = function(dims) {
    return dims.width - dims.margin.left - dims.margin.right;
  };
  var innerHeight4 = function(dims) {
    return dims.height - dims.margin.top - dims.margin.bottom;
  };
  var drawQuartet = function(dictBind) {
    var bind10 = bind(dictBind);
    return function(dictMonadEffect) {
      var liftEffect10 = liftEffect(dictMonadEffect);
      var Applicative0 = dictMonadEffect.Monad0().Applicative0();
      var pure17 = pure(Applicative0);
      var traverse_9 = traverse_(Applicative0)(foldableArray);
      return function(dictSelectionM) {
        var attach2 = attach(dictSelectionM);
        var appendTo2 = appendTo(dictSelectionM);
        return function(quartet) {
          return function(selector) {
            var plotWidth = (900 - 60 * 3) / 2;
            var plotHeight = (700 - 60 * 3) / 2;
            var margin = {
              top: 30,
              right: 20,
              bottom: 40,
              left: 50
            };
            var iWidth = plotWidth - margin.left - margin.right;
            var iHeight = plotHeight - margin.top - margin.bottom;
            var xDomain = [0, 20];
            var yDomain = [0, 14];
            return bind10(attach2(selector))(function(v) {
              return bind10(appendTo2(v)(Svg.value)([viewBox(0)(0)(900)(700), classed6("scatter-quartet"), width12(900), height12(700)]))(function(svg) {
                var drawSubplot = function(title5) {
                  return function(dataPoints) {
                    return function(xOffset) {
                      return function(yOffset) {
                        return bind10(appendTo2(svg)(Group.value)([classed6("subplot"), transform([function(v1) {
                          return "translate(" + (show8(xOffset + margin.left) + ("," + (show8(yOffset + margin.top) + ")")));
                        }])]))(function(subplotGroup) {
                          return bind10(appendTo2(svg)(Text2.value)([x5(xOffset + plotWidth / 2), y5(yOffset + 15), text8(title5), textAnchor2("middle"), fontSize3(16), classed6("subplot-title")]))(function() {
                            return bind10(liftEffect10(createLinearScale({
                              domain: xDomain,
                              range: [0, iWidth]
                            })))(function(xScale) {
                              return bind10(liftEffect10(createLinearScale({
                                domain: yDomain,
                                range: [iHeight, 0]
                              })))(function(yScale) {
                                return bind10(appendTo2(subplotGroup)(Group.value)([classed6("x-axis"), transform([function(v1) {
                                  return "translate(0," + (show8(iHeight) + ")");
                                }])]))(function(xAxisGroup) {
                                  return bind10(appendTo2(subplotGroup)(Group.value)([classed6("y-axis")]))(function(yAxisGroup) {
                                    return bind10(liftEffect10(callAxis(xAxisGroup)(axisBottom2(xScale))))(function() {
                                      return bind10(liftEffect10(callAxis(yAxisGroup)(axisLeft2(yScale))))(function() {
                                        var addPoint = function(point) {
                                          var xPos = applyScale(xScale)(point.x);
                                          var yPos = applyScale(yScale)(point.y);
                                          return bind10(appendTo2(subplotGroup)(Circle.value)([cx2(xPos), cy2(yPos), radius2(4), fill6("#e74c3c"), strokeColor5("#c0392b"), strokeWidth5(1.5), classed6("scatter-point")]))(function() {
                                            return pure17(unit);
                                          });
                                        };
                                        return bind10(traverse_9(addPoint)(dataPoints))(function() {
                                          return pure17(unit);
                                        });
                                      });
                                    });
                                  });
                                });
                              });
                            });
                          });
                        });
                      };
                    };
                  };
                };
                return bind10(drawSubplot("Dataset I")(quartet.dataset1)(60)(60))(function() {
                  return bind10(drawSubplot("Dataset II")(quartet.dataset2)(60 + plotWidth + 60)(60))(function() {
                    return bind10(drawSubplot("Dataset III")(quartet.dataset3)(60)(60 + plotHeight + 60))(function() {
                      return bind10(drawSubplot("Dataset IV")(quartet.dataset4)(60 + plotWidth + 60)(60 + plotHeight + 60))(function() {
                        return pure17(unit);
                      });
                    });
                  });
                });
              });
            });
          };
        };
      };
    };
  };
  var defaultDimensions3 = {
    width: 800,
    height: 400,
    margin: {
      top: 20,
      right: 30,
      bottom: 30,
      left: 40
    }
  };
  var draw4 = function(dictBind) {
    var bind10 = bind(dictBind);
    return function(dictMonadEffect) {
      var liftEffect10 = liftEffect(dictMonadEffect);
      var Applicative0 = dictMonadEffect.Monad0().Applicative0();
      var pure17 = pure(Applicative0);
      var traverse_9 = traverse_(Applicative0)(foldableArray);
      return function(dictSelectionM) {
        var attach2 = attach(dictSelectionM);
        var appendTo2 = appendTo(dictSelectionM);
        return function(dataPoints) {
          return function(selector) {
            var iWidth = innerWidth4(defaultDimensions3);
            var iHeight = innerHeight4(defaultDimensions3);
            var xValues = map27(function(v) {
              return v.x;
            })(dataPoints);
            var yValues = map27(function(v) {
              return v.y;
            })(dataPoints);
            var minX = fromMaybe(0)(minimum4(xValues));
            var maxX = fromMaybe(100)(maximum4(xValues));
            var minY = fromMaybe(0)(minimum4(yValues));
            var maxY = fromMaybe(100)(maximum4(yValues));
            return bind10(attach2(selector))(function(v) {
              return bind10(appendTo2(v)(Svg.value)([viewBox(0)(0)(defaultDimensions3.width)(defaultDimensions3.height), classed6("scatter-plot"), width12(defaultDimensions3.width), height12(defaultDimensions3.height)]))(function(svg) {
                return bind10(appendTo2(svg)(Group.value)([transform([function(v1) {
                  return "translate(" + (show8(defaultDimensions3.margin.left) + ("," + (show8(defaultDimensions3.margin.top) + ")")));
                }])]))(function(chartGroup) {
                  return bind10(liftEffect10(createLinearScale({
                    domain: [minX, maxX],
                    range: [0, iWidth]
                  })))(function(xScale) {
                    return bind10(liftEffect10(createLinearScale({
                      domain: [minY, maxY],
                      range: [iHeight, 0]
                    })))(function(yScale) {
                      return bind10(appendTo2(chartGroup)(Group.value)([classed6("x-axis"), transform([function(v1) {
                        return "translate(0," + (show8(iHeight) + ")");
                      }])]))(function(xAxisGroup) {
                        return bind10(appendTo2(chartGroup)(Group.value)([classed6("y-axis")]))(function(yAxisGroup) {
                          return bind10(liftEffect10(callAxis(xAxisGroup)(axisBottom2(xScale))))(function() {
                            return bind10(liftEffect10(callAxis(yAxisGroup)(axisLeft2(yScale))))(function() {
                              var addPoint = function(point) {
                                var xPos = applyScale(xScale)(point.x);
                                var yPos = applyScale(yScale)(point.y);
                                return bind10(appendTo2(chartGroup)(Circle.value)([cx2(xPos), cy2(yPos), radius2(5), fill6("#4a90e2"), strokeColor5("#357abd"), strokeWidth5(1), classed6("scatter-point")]))(function() {
                                  return pure17(unit);
                                });
                              };
                              return bind10(traverse_9(addPoint)(dataPoints))(function() {
                                return pure17(unit);
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          };
        };
      };
    };
  };

  // output/D3.Scales/foreign.js
  var d3SchemeCategory10 = d3.scaleOrdinal(d3.schemeCategory10);
  function d3SchemeCategory10N_(number4) {
    return d3SchemeCategory10(number4);
  }
  var d3SchemePaired = d3.scaleOrdinal(d3.schemePaired);
  var d3SchemeDiverging10 = d3.scaleDiverging(d3.interpolateBrBG).domain([0, 250, 500]);
  var d3SchemeSequential10 = d3.scaleSequential().interpolator(d3.interpolateYlOrRd).domain([0, 5, 10]);

  // output/D3.Examples.ThreeLittleCircles/index.js
  var fill7 = /* @__PURE__ */ fill(toAttrString);
  var cx3 = /* @__PURE__ */ cx(toAttrNumberFnI);
  var cy3 = /* @__PURE__ */ cy(toAttrNumber);
  var radius3 = /* @__PURE__ */ radius(toAttrNumber);
  var classed7 = /* @__PURE__ */ classed(toAttrString);
  var discard6 = /* @__PURE__ */ discard(discardUnit);
  var xFromIndex = function(v) {
    return function(i2) {
      return i2 * 100;
    };
  };
  var drawThreeCircles = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind10 = bind(Bind1);
    var attach2 = attach(dictSelectionM);
    var appendTo2 = appendTo(dictSelectionM);
    var simpleJoin2 = simpleJoin(dictSelectionM);
    var discard15 = discard6(Bind1);
    var setAttributes2 = setAttributes(dictSelectionM);
    var pure17 = pure(Monad0.Applicative0());
    return function(selector) {
      var circleAttributes = [fill7("green"), cx3(xFromIndex), cy3(50), radius3(20)];
      return bind10(attach2(selector))(function(root2) {
        return bind10(appendTo2(root2)(Svg.value)([viewBox(-100)(-100)(650)(650), classed7("d3svg gup")]))(function(svg) {
          return bind10(appendTo2(svg)(Group.value)([]))(function(circleGroup) {
            return bind10(simpleJoin2(circleGroup)(Circle.value)([32, 57, 293])(keyIsID_))(function(circles) {
              return discard15(setAttributes2(circles)(circleAttributes))(function() {
                return pure17(circles);
              });
            });
          });
        });
      });
    };
  };

  // output/D3.Examples.MetaTree.Unsafe/index.js
  var unboxD3TreeNode = function(v) {
    return v;
  };
  var coerceToTreeNode = unsafeCoerce2;

  // output/D3.Examples.Tree.Draw/index.js
  var eq14 = /* @__PURE__ */ eq(eqTreeLayout);
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var classed8 = /* @__PURE__ */ classed(toAttrString);
  var width13 = /* @__PURE__ */ width8(toAttrNumber);
  var height13 = /* @__PURE__ */ height8(toAttrNumber);
  var fontFamily2 = /* @__PURE__ */ fontFamily(toAttrString);
  var fontSize4 = /* @__PURE__ */ fontSize(toAttrNumber);
  var discard7 = /* @__PURE__ */ discard(discardUnit);
  var strokeWidth6 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var strokeColor6 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeOpacity2 = /* @__PURE__ */ strokeOpacity(toAttrNumber);
  var fill8 = /* @__PURE__ */ fill(toAttrString);
  var fill1 = /* @__PURE__ */ fill(toAttrStringFn);
  var radius4 = /* @__PURE__ */ radius(toAttrNumber);
  var dy2 = /* @__PURE__ */ dy(toAttrNumber);
  var x6 = /* @__PURE__ */ x(toAttrNumberFn);
  var textAnchor3 = /* @__PURE__ */ textAnchor(toAttrStringFn);
  var text9 = /* @__PURE__ */ text6(toAttrStringFn);
  var treeDatum_ = {
    depth: function($50) {
      return (function(v) {
        return v.depth;
      })(unboxD3TreeNode(coerceToTreeNode($50)));
    },
    height: function($51) {
      return (function(v) {
        return v.height;
      })(unboxD3TreeNode(coerceToTreeNode($51)));
    },
    x: function($52) {
      return (function(v) {
        return v.x;
      })(unboxD3TreeNode(coerceToTreeNode($52)));
    },
    y: function($53) {
      return (function(v) {
        return v.y;
      })(unboxD3TreeNode(coerceToTreeNode($53)));
    },
    name: function($54) {
      return (function(v) {
        return v.data.name;
      })(unboxD3TreeNode(coerceToTreeNode($54)));
    },
    value: function($55) {
      return getHierarchyValue_(coerceToTreeNode($55));
    },
    hasChildren: function($56) {
      return hasChildren_(coerceToTreeNode($56));
    },
    textAnchor: function(l) {
      return function(d6) {
        if (l instanceof Radial) {
          var $42 = treeDatum_.hasChildren(d6) === treeDatum_.x(d6) < pi;
          if ($42) {
            return "start";
          }
          ;
          return "end";
        }
        ;
        var $43 = treeDatum_.hasChildren(d6);
        if ($43) {
          return "start";
        }
        ;
        return "end";
      };
    },
    textX: function(l) {
      return function(d6) {
        if (l instanceof Radial) {
          var $45 = treeDatum_.hasChildren(d6) === treeDatum_.x(d6) < pi;
          if ($45) {
            return 6;
          }
          ;
          return -6;
        }
        ;
        var $46 = treeDatum_.hasChildren(d6);
        if ($46) {
          return 6;
        }
        ;
        return -6;
      };
    },
    onRHS: function(l) {
      return function(d6) {
        var $47 = eq14(l)(Radial.value) && treeDatum_.x(d6) >= pi;
        if ($47) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var draw5 = function(dictBind) {
    var bind10 = bind(dictBind);
    var discard15 = discard7(dictBind);
    return function(dictSelectionM) {
      var attach2 = attach(dictSelectionM);
      var appendTo2 = appendTo(dictSelectionM);
      var simpleJoin2 = simpleJoin(dictSelectionM);
      var setAttributes2 = setAttributes(dictSelectionM);
      var pure17 = pure(dictSelectionM.Monad0().Applicative0());
      return function(config) {
        return function(tree2) {
          return bind10(attach2(config.selector))(function(root2) {
            return bind10(appendTo2(root2)(Svg.value)(append7(config.viewbox)([classed8("tree"), width13(config.svg.width), height13(config.svg.height)])))(function(svg) {
              return bind10(appendTo2(svg)(Group.value)([fontFamily2("sans-serif"), fontSize4(10)]))(function(container) {
                return bind10(appendTo2(container)(Group.value)([classed8("links")]))(function(links) {
                  return bind10(appendTo2(container)(Group.value)([classed8("nodes")]))(function(nodes) {
                    return bind10(simpleJoin2(links)(Path.value)(links_(tree2))(keyIsID_))(function(theLinks_) {
                      return discard15(setAttributes2(theLinks_)([strokeWidth6(1.5), strokeColor6(config.color), strokeOpacity2(0.4), fill8("none"), config.linkPath]))(function() {
                        return bind10(simpleJoin2(nodes)(Group.value)(descendants_(tree2))(keyIsID_))(function(nodeJoin_) {
                          return discard15(setAttributes2(nodeJoin_)(config.nodeTransform))(function() {
                            return bind10(appendTo2(nodeJoin_)(Circle.value)([fill1(function(v) {
                              var $49 = treeDatum_.hasChildren(v);
                              if ($49) {
                                return "#999";
                              }
                              ;
                              return "#555";
                            }), radius4(2.5), strokeColor6("white")]))(function(theNodes) {
                              return bind10(appendTo2(nodeJoin_)(Text2.value)([dy2(0.31), x6(treeDatum_.textX(config.layout)), textAnchor3(treeDatum_.textAnchor(config.layout)), text9(treeDatum_.name), fill8(config.color)]))(function(theLabels) {
                                return pure17(svg);
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        };
      };
    };
  };

  // output/D3.Layouts.Hierarchical/foreign.js
  function readJSON_(filecontents) {
    return JSON.parse(filecontents);
  }

  // output/D3.Layouts.Hierarchical/index.js
  var toAttr3 = /* @__PURE__ */ toAttr(toAttrStringFn);
  var pure10 = /* @__PURE__ */ pure(applicativeAff);
  var bind5 = /* @__PURE__ */ bind(bindAff);
  var rmap2 = /* @__PURE__ */ rmap(bifunctorEither);
  var verticalLink = /* @__PURE__ */ (function() {
    return new AttrT(new AttributeSetter("d", toAttr3(linkVertical_)));
  })();
  var verticalClusterLink = function(xOffset) {
    return new AttrT(new AttributeSetter("d", toAttr3(linkClusterVertical_(xOffset))));
  };
  var radialSeparation = function(a2, b2) {
    var $8 = sharesParent_(a2)(b2);
    if ($8) {
      return 1;
    }
    ;
    return 2 / hNodeDepth_(a2);
  };
  var radialLink = function(angleFn) {
    return function(radius_Fn) {
      var radialFn = linkRadial_(angleFn)(radius_Fn);
      return new AttrT(new AttributeSetter("d", toAttr3(radialFn)));
    };
  };
  var makeModel = function(dictBind) {
    return function(dictMonadEffect) {
      return function(treeType) {
        return function(treeLayout) {
          return function(json) {
            var treeLayoutFn = getLayout(treeType);
            var svgConfig = {
              width: 650,
              height: 650
            };
            return pure10({
              json,
              treeType,
              treeLayout,
              treeLayoutFn,
              svgConfig
            });
          };
        };
      };
    };
  };
  var horizontalLink = /* @__PURE__ */ (function() {
    return new AttrT(new AttributeSetter("d", toAttr3(linkHorizontal_)));
  })();
  var horizontalClusterLink = function(yOffset) {
    return new AttrT(new AttributeSetter("d", toAttr3(linkClusterHorizontal_(yOffset))));
  };
  var getTreeViaAJAX = function(url2) {
    return bind5(get3(string)(url2))(function(result) {
      return pure10(rmap2(function(v) {
        return readJSON_(v.body);
      })(result));
    });
  };

  // output/D3Tagless.Capabilities.MetaTree/foreign.js
  prune = function(node) {
    if (node.children.length == 0) {
      delete node.children;
    } else {
      node.children.forEach(
        (child) => prune(child)
      );
    }
    ;
  };

  // output/D3Tagless.Capabilities.String/foreign.js
  function formatElement(element3) {
    const el = element3.toString().replace(/"/g, "");
    return `"${el.toLowerCase()}"`;
  }
  function formatSelector(selector) {
    const s = selector.toString();
    return s.startsWith('"') || s.startsWith("'") ? s : `"${s}"`;
  }
  function formatValue(value13) {
    if (typeof value13 === "function" || value13.toString().includes("=>") || value13.toString().includes("function")) {
      return value13.toString();
    }
    if (!isNaN(value13) && value13 !== "") {
      return value13;
    }
    const str = value13.toString();
    if (str.startsWith('"') || str.startsWith("'")) {
      return str;
    }
    return `"${str}"`;
  }
  function showSelectAllInDOM_(selector) {
    return `d3.select(${formatSelector(selector)})`;
  }
  function showSelectAll_(selector) {
    return (selection2) => {
      return `${selection2}.selectAll(${formatSelector(selector)})`;
    };
  }
  function showAddTransition_(selection2) {
    return (transition2) => {
      let result = `${selection2}.transition()`;
      if (transition2.duration && transition2.duration != 0) {
        result += `
  .duration(${transition2.duration})`;
      }
      if (transition2.delay && transition2.delay != 0) {
        result += `
  .delay(${transition2.delay})`;
      }
      if (transition2.name && transition2.name !== "") {
        result = `${selection2}.transition("${transition2.name}")`;
      }
      return result;
    };
  }
  function showRemoveSelection_(selection2) {
    return `${selection2}.remove()`;
  }
  function showAppend_(element3) {
    return (selection2) => {
      return `${selection2}.append(${formatElement(element3)})`;
    };
  }
  function showData_(data) {
    return (selection2) => {
      const dataStr = Array.isArray(data) ? `[/* ${data.length} items */]` : data.toString();
      return `${selection2}.data(${dataStr})`;
    };
  }
  function showSetAttr_(name16) {
    return (value13) => (selection2) => {
      return `${selection2}.attr("${name16}", ${formatValue(value13)})`;
    };
  }
  function showSetText_(value13) {
    return (selection2) => {
      return `${selection2}.text(${formatValue(value13)})`;
    };
  }
  function showSetHTML_(value13) {
    return (selection2) => {
      return `${selection2}.html(${formatValue(value13)})`;
    };
  }
  function showSetProperty_(value13) {
    return (selection2) => {
      return `${selection2}.property(${formatValue(value13)})`;
    };
  }
  function showSetOrdering_(ordering) {
    return (selection2) => {
      return `${selection2}.${ordering}()`;
    };
  }

  // output/D3Tagless.Capabilities.String/index.js
  var show9 = /* @__PURE__ */ show(showOrderingAttribute);
  var show13 = /* @__PURE__ */ show(showMouseEvent);
  var show23 = /* @__PURE__ */ show(showString);
  var show33 = /* @__PURE__ */ show(showElement);
  var trimSelectionPrefix = function(str) {
    var v = indexOf2(".")(str);
    if (v instanceof Just) {
      return drop4(v.value0 + 1 | 0)(str);
    }
    ;
    if (v instanceof Nothing) {
      return str;
    }
    ;
    throw new Error("Failed pattern match at D3Tagless.Capabilities.String (line 118, column 3 - line 120, column 19): " + [v.constructor.name]);
  };
  var runPrinter = function(v) {
    return function(initialString) {
      return runStateT(v)(initialString);
    };
  };
  var monadStateD3PrinterM = /* @__PURE__ */ monadStateStateT(monadEffect);
  var modify_4 = /* @__PURE__ */ modify_(monadStateD3PrinterM);
  var monadD3PrinterM = /* @__PURE__ */ monadStateT(monadEffect);
  var bindD3PrinterM = /* @__PURE__ */ bindStateT(monadEffect);
  var discard8 = /* @__PURE__ */ discard(discardUnit)(bindD3PrinterM);
  var applySelectionAttributeString = function(selection2) {
    return function(v) {
      if (v instanceof AttrT) {
        return trimSelectionPrefix(showSetAttr_(v.value0.value0)(unboxAttr(v.value0.value1))(selection2));
      }
      ;
      if (v instanceof TextT) {
        return trimSelectionPrefix(showSetText_(unboxAttr(v.value0.value1))(selection2));
      }
      ;
      if (v instanceof PropertyT) {
        return trimSelectionPrefix(showSetProperty_(unboxAttr(v.value0.value1))(selection2));
      }
      ;
      if (v instanceof HTMLT) {
        return trimSelectionPrefix(showSetHTML_(unboxAttr(v.value0.value1))(selection2));
      }
      ;
      if (v instanceof RemoveT) {
        return trimSelectionPrefix(showRemoveSelection_(selection2));
      }
      ;
      if (v instanceof OrderingT) {
        return trimSelectionPrefix(showSetOrdering_(show9(v.value0))(selection2));
      }
      ;
      if (v instanceof TransitionT) {
        var tString = showAddTransition_(selection2)(v.value1);
        var trimmed = trimSelectionPrefix(tString);
        return foldl2(function(acc) {
          return function(attr3) {
            return acc + ("\n  ." + trimSelectionPrefix(applySelectionAttributeString(tString)(attr3)));
          };
        })(trimmed)(v.value0);
      }
      ;
      if (v instanceof OnT) {
        return "on(" + (show13(v.value0) + ", function(d) { /* event handler */ })");
      }
      ;
      if (v instanceof OnT$prime) {
        return "on(" + (show13(v.value0) + ", function(d) { /* event handler */ })");
      }
      ;
      throw new Error("Failed pattern match at D3Tagless.Capabilities.String (line 94, column 3 - line 113, column 70): " + [v.constructor.name]);
    };
  };
  var applicativeD3PrinterM = /* @__PURE__ */ applicativeStateT(monadEffect);
  var pure11 = /* @__PURE__ */ pure(applicativeD3PrinterM);
  var d3Tagless = {
    attach: function(selector) {
      var code2 = showSelectAllInDOM_(selector);
      return discard8(modify_4(function(s) {
        return s + code2;
      }))(function() {
        return pure11(code2);
      });
    },
    selectUnder: function(selection2) {
      return function(selector) {
        var code2 = showSelectAll_(selector)(selection2);
        return discard8(modify_4(function(s) {
          return s + ("\n  ." + code2);
        }))(function() {
          return pure11(code2);
        });
      };
    },
    appendTo: function(selection2) {
      return function(element3) {
        return function(attributes) {
          var appendCode = showAppend_(element3)(selection2);
          var attributeString = foldl2(applySelectionAttributeString)(appendCode)(attributes);
          return discard8(modify_4(function(s) {
            return s + ("\n\nconst /* TODO: varName */ = " + (appendCode + (attributeString + ";")));
          }))(function() {
            return pure11(appendCode);
          });
        };
      };
    },
    filterSelection: function(selection2) {
      return function(selector) {
        var code2 = selection2 + (".filter(" + (show23(selector) + ")"));
        return discard8(modify_4(function(s) {
          return s + ("\n  ." + code2);
        }))(function() {
          return pure11(code2);
        });
      };
    },
    mergeSelections: function(a2) {
      return function(b2) {
        var code2 = a2 + (".merge(" + (b2 + ")"));
        return discard8(modify_4(function(s) {
          return s + ("\n  ." + code2);
        }))(function() {
          return pure11(code2);
        });
      };
    },
    setAttributes: function(selection2) {
      return function(attributes) {
        var attributeString = foldl2(function(acc) {
          return function(attr3) {
            return acc + ("\n  ." + applySelectionAttributeString(selection2)(attr3));
          };
        })("")(attributes);
        return discard8(modify_4(function(s) {
          return s + attributeString;
        }))(function() {
          return pure11(unit);
        });
      };
    },
    on: function(v) {
      return function(v1) {
        if (v1 instanceof Drag) {
          return discard8(modify_4(function(s) {
            return s + "\n  .call(d3.drag())";
          }))(function() {
            return pure11(unit);
          });
        }
        ;
        if (v1 instanceof Zoom) {
          return discard8(modify_4(function(s) {
            return s + "\n  .call(d3.zoom())";
          }))(function() {
            return pure11(unit);
          });
        }
        ;
        throw new Error("Failed pattern match at D3Tagless.Capabilities.String (line 33, column 1 - line 88, column 64): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    openSelection: function(selection2) {
      return function(selector) {
        var code2 = showSelectAll_(selector)(selection2);
        return discard8(modify_4(function(s) {
          return s + ("\n\nconst /* TODO: varName */ = " + (code2 + ";"));
        }))(function() {
          return pure11(code2);
        });
      };
    },
    simpleJoin: function(selection2) {
      return function(e) {
        return function(ds) {
          return function(k) {
            var dataCode = showData_(ds)(selection2);
            var joinCode = dataCode + ("\n  .join(" + (show33(e) + ")"));
            return discard8(modify_4(function(s) {
              return s + ("\n\nconst /* TODO: varName */ = " + (joinCode + ";"));
            }))(function() {
              return pure11(joinCode);
            });
          };
        };
      };
    },
    updateJoin: function(selection2) {
      return function(e) {
        return function(ds) {
          return function(k) {
            var dataCode = showData_(ds)(selection2);
            var enterCode = dataCode + ("\n  .enter().append(" + (show33(e) + ")"));
            var exitCode = dataCode + "\n  .exit().remove()";
            return discard8(modify_4(function(s) {
              return s + ("\n\n// Update pattern\nconst update = " + (dataCode + (";\nconst enter = " + (enterCode + (";\nconst exit = " + (exitCode + ";"))))));
            }))(function() {
              return pure11({
                enter: enterCode,
                exit: exitCode,
                update: dataCode
              });
            });
          };
        };
      };
    },
    Monad0: function() {
      return monadD3PrinterM;
    }
  };

  // output/D3.Zoom/index.js
  var DefaultZoomExtent = /* @__PURE__ */ (function() {
    function DefaultZoomExtent2() {
    }
    ;
    DefaultZoomExtent2.value = new DefaultZoomExtent2();
    return DefaultZoomExtent2;
  })();
  var ZoomExtent = /* @__PURE__ */ (function() {
    function ZoomExtent2(value0) {
      this.value0 = value0;
    }
    ;
    ZoomExtent2.create = function(value0) {
      return new ZoomExtent2(value0);
    };
    return ZoomExtent2;
  })();

  // output/Debug/foreign.js
  var req = typeof module === "undefined" ? void 0 : module.require;
  var util = (function() {
    try {
      return req === void 0 ? void 0 : req("util");
    } catch (e) {
      return void 0;
    }
  })();
  function _spy(tag, x7) {
    if (util !== void 0) {
      console.log(tag + ":", util.inspect(x7, { depth: null, colors: true }));
    } else {
      console.log(tag + ":", x7);
    }
    return x7;
  }
  var now2 = (function() {
    var perf;
    if (typeof performance !== "undefined") {
      perf = performance;
    } else if (req) {
      try {
        perf = req("perf_hooks").performance;
      } catch (e) {
      }
    }
    return (function() {
      return (perf || Date).now();
    });
  })();

  // output/Debug/index.js
  var spy = function() {
    return function(tag) {
      return function(a2) {
        return _spy(tag, a2);
      };
    };
  };

  // output/D3.Selection.Functions/index.js
  var spy2 = /* @__PURE__ */ spy();
  var foldl3 = /* @__PURE__ */ foldl(foldableArray);
  var show10 = /* @__PURE__ */ show(showElement);
  var discard9 = /* @__PURE__ */ discard(discardUnit);
  var selectionUpdateJoin = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(openSelection2) {
      return function(e) {
        return function(theData) {
          return function(keyFn) {
            var updateSelection = d3DataWithKeyFunction_(theData)(keyFn)(openSelection2);
            var exitSelection = d3GetExitSelection_(updateSelection);
            var enterSelection = d3GetEnterSelection_(updateSelection);
            return pure17({
              enter: enterSelection,
              exit: exitSelection,
              update: updateSelection
            });
          };
        };
      };
    };
  };
  var selectionSelectUnder = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection2) {
      return function(selector) {
        return pure17(d3SelectionSelectAll_(selector)(selection2));
      };
    };
  };
  var selectionOpenSelection = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection2) {
      return function(selector) {
        var v = spy2("open selection: ")(selector);
        return pure17(d3SelectionSelectAll_(selector)(selection2));
      };
    };
  };
  var selectionOn = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(v) {
      return function(v1) {
        if (v1 instanceof Drag) {
          return pure17(unit);
        }
        ;
        if (v1 instanceof Zoom) {
          var v2 = (function() {
            if (v1.value0.extent instanceof DefaultZoomExtent) {
              return d3AttachZoomDefaultExtent_(v)({
                scaleExtent: [v1.value0.scale.value0, v1.value0.scale.value1],
                name: v1.value0.name,
                target: v
              });
            }
            ;
            if (v1.value0.extent instanceof ZoomExtent) {
              return d3AttachZoom_(v)({
                extent: [[v1.value0.extent.value0.left, v1.value0.extent.value0.top], [v1.value0.extent.value0.right, v1.value0.extent.value0.bottom]],
                scaleExtent: [v1.value0.scale.value0, v1.value0.scale.value1],
                name: v1.value0.name,
                target: v
              });
            }
            ;
            throw new Error("Failed pattern match at D3.Selection.Functions (line 82, column 9 - line 96, column 14): " + [v1.value0.extent.constructor.name]);
          })();
          return pure17(unit);
        }
        ;
        throw new Error("Failed pattern match at D3.Selection.Functions (line 65, column 1 - line 65, column 104): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var selectionModifySelection = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection_) {
      return function(attributes) {
        var v = foldl3(applySelectionAttributeD3)(selection_)(attributes);
        return pure17(unit);
      };
    };
  };
  var selectionMergeSelections = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selectionA) {
      return function(selectionB) {
        return pure17(d3MergeSelectionWith_(selectionA)(selectionB));
      };
    };
  };
  var selectionJoin = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection2) {
      return function(e) {
        return function(theData) {
          return function(keyFn) {
            var element3 = spy2("Join: ")(show10(e));
            var selectS = d3SelectionSelectAll_(element3)(selection2);
            var dataSelection = d3DataWithKeyFunction_(theData)(keyFn)(selectS);
            var enterSelection = d3EnterAndAppend_(element3)(dataSelection);
            return pure17(enterSelection);
          };
        };
      };
    };
  };
  var selectionFilterSelection = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection_) {
      return function(selector) {
        return pure17(d3FilterSelection_(selection_)(selector));
      };
    };
  };
  var selectionAttach = function(dictSelectionM) {
    var pure17 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selector) {
      return pure17(d3SelectAllInDOM_(selector));
    };
  };
  var selectionAppendElement = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var discard15 = discard9(Monad0.Bind1());
    var selectionModifySelection1 = selectionModifySelection(dictSelectionM);
    var pure17 = pure(Monad0.Applicative0());
    return function(selection_) {
      return function(element3) {
        return function(attributes) {
          var appended_ = d3Append_(show10(element3))(selection_);
          return discard15(selectionModifySelection1(appended_)(attributes))(function() {
            return pure17(appended_);
          });
        };
      };
    };
  };

  // output/D3Tagless.Instance.Selection/index.js
  var liftA12 = /* @__PURE__ */ liftA1(applicativeEffect);
  var runD3M = function(v) {
    return runStateT(v)(unit);
  };
  var monadEffD3M = /* @__PURE__ */ monadEffectState(monadEffectEffect);
  var monadD3M = /* @__PURE__ */ monadStateT(monadEffect);
  var eval_D3M = function(v) {
    return liftA12(fst)(runStateT(v)(unit));
  };
  var d3TaglessD3M = {
    attach: function(selector) {
      return selectionAttach(d3TaglessD3M)(selector);
    },
    selectUnder: function(s_) {
      return selectionSelectUnder(d3TaglessD3M)(s_);
    },
    appendTo: function(s_) {
      return selectionAppendElement(d3TaglessD3M)(s_);
    },
    filterSelection: function(s_) {
      return selectionFilterSelection(d3TaglessD3M)(s_);
    },
    openSelection: function(s_) {
      return selectionOpenSelection(d3TaglessD3M)(s_);
    },
    mergeSelections: function(s_) {
      return selectionMergeSelections(d3TaglessD3M)(s_);
    },
    setAttributes: function(s_) {
      return selectionModifySelection(d3TaglessD3M)(s_);
    },
    simpleJoin: function(s_) {
      return selectionJoin(d3TaglessD3M)(s_);
    },
    updateJoin: function(s_) {
      return selectionUpdateJoin(d3TaglessD3M)(s_);
    },
    on: function(s_) {
      return selectionOn(d3TaglessD3M)(s_);
    },
    Monad0: function() {
      return monadD3M;
    }
  };
  var bindD3M = /* @__PURE__ */ bindStateT(monadEffect);

  // output/Utility/index.js
  var getWindowWidthHeight = function __do2() {
    var win = windowImpl();
    var w = innerWidth(win)();
    var h = innerHeight(win)();
    return new Tuple(toNumber(w), toNumber(h));
  };

  // output/D3.Examples.Tree.Configure/index.js
  var show11 = /* @__PURE__ */ show(showNumber);
  var eq4 = /* @__PURE__ */ eq(eqTreeLayout);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var rotateRadialLabels = function(d6) {
    return "rotate(" + ((function() {
      var $27 = treeDatum_.onRHS(Radial.value)(d6);
      if ($27) {
        return "180";
      }
      ;
      return "0";
    })() + ")");
  };
  var radialTranslate = function(d6) {
    return "translate(" + (show11(treeDatum_.y(d6)) + ",0)");
  };
  var radialRotate = function(x7) {
    return show11(x7 * 180 / pi - 90);
  };
  var radialRotateCommon = function(d6) {
    return "rotate(" + (radialRotate(treeDatum_.x(d6)) + ")");
  };
  var positionXYreflected = function(d6) {
    return "translate(" + (show11(treeDatum_.y(d6)) + ("," + (show11(treeDatum_.x(d6)) + ")")));
  };
  var positionXY = function(d6) {
    return "translate(" + (show11(treeDatum_.x(d6)) + ("," + (show11(treeDatum_.y(d6)) + ")")));
  };
  var configureAndRunScript = function(dictBind) {
    var draw7 = draw5(dictBind);
    return function(dictSelectionM) {
      var draw12 = draw7(dictSelectionM);
      return function(v) {
        return function(model) {
          return function(selector) {
            var svg = {
              width: v.value0,
              height: v.value1
            };
            var root2 = hierarchyFromJSON_(model.json);
            var numberOfLevels = hNodeHeight_(root2) + 1;
            var spacing = (function() {
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Horizontal) {
                return {
                  interChild: 10,
                  interLevel: svg.width / numberOfLevels
                };
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Vertical) {
                return {
                  interChild: 10,
                  interLevel: svg.height / numberOfLevels
                };
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Radial) {
                return {
                  interChild: 0,
                  interLevel: 0
                };
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Horizontal) {
                return {
                  interChild: 10,
                  interLevel: svg.width / numberOfLevels
                };
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Vertical) {
                return {
                  interChild: 10,
                  interLevel: svg.height / numberOfLevels
                };
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Radial) {
                return {
                  interChild: 0,
                  interLevel: 0
                };
              }
              ;
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 67, column 7 - line 74, column 71): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            })();
            var layout = (function() {
              var $33 = eq4(model.treeLayout)(Radial.value);
              if ($33) {
                return treeSetSeparation_(treeSetSize_(getLayout(model.treeType))([2 * pi, svg.width / 2 - 100]))(radialSeparation);
              }
              ;
              return treeSetNodeSize_(getLayout(model.treeType))([spacing.interChild, spacing.interLevel]);
            })();
            var laidOutRoot_ = runLayoutFn_(layout)(root2);
            var v1 = treeMinMax_(laidOutRoot_);
            var yExtent = abs(v1.yMax - v1.yMin);
            var xExtent = abs(v1.xMax - v1.xMin);
            var vtreeYOffset = abs(v.value1 - yExtent) / 2;
            var radialExtent = 2 * v1.yMax;
            var pad = function(n) {
              return n * 1.2;
            };
            var nodeTransform = (function() {
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Horizontal) {
                return [transform([positionXYreflected])];
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Vertical) {
                return [transform([positionXY])];
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Radial) {
                return [transform([radialRotateCommon, radialTranslate, rotateRadialLabels])];
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Horizontal) {
                return [transform([positionXYreflected])];
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Vertical) {
                return [transform([positionXY])];
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Radial) {
                return [transform([radialRotateCommon, radialTranslate, rotateRadialLabels])];
              }
              ;
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 117, column 7 - line 124, column 108): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            })();
            var linkPath = (function() {
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Horizontal) {
                return horizontalClusterLink(spacing.interLevel);
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Vertical) {
                return verticalClusterLink(spacing.interLevel);
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Radial) {
                return radialLink(treeDatum_.x)(treeDatum_.y);
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Horizontal) {
                return horizontalLink;
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Vertical) {
                return verticalLink;
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Radial) {
                return radialLink(treeDatum_.x)(treeDatum_.y);
              }
              ;
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 107, column 7 - line 114, column 71): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            })();
            var viewbox = (function() {
              if (model.treeLayout instanceof Vertical) {
                return [viewBox(v1.xMin)(-vtreeYOffset)(pad(xExtent))(pad(yExtent)), preserveAspectRatio(new AspectRatio(XMid.value, YMid.value, Meet.value))];
              }
              ;
              if (model.treeLayout instanceof Horizontal) {
                return [viewBox(-xExtent * 0.1)(pad(v1.xMin))(pad(yExtent))(pad(xExtent)), preserveAspectRatio(new AspectRatio(XMin.value, YMid.value, Meet.value))];
              }
              ;
              if (model.treeLayout instanceof Radial) {
                return [viewBox(-v1.yMax * 1.2)(-v1.yMax * 1.2)(radialExtent * 1.2)(radialExtent * 1.2), preserveAspectRatio(new AspectRatio(XMin.value, YMin.value, Meet.value))];
              }
              ;
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 98, column 7 - line 104, column 78): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            })();
            var color2 = d3SchemeCategory10N_((function() {
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Horizontal) {
                return 1;
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Vertical) {
                return 2;
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Radial) {
                return 3;
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Horizontal) {
                return 4;
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Vertical) {
                return 5;
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Radial) {
                return 6;
              }
              ;
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 127, column 7 - line 134, column 38): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            })());
            return draw12({
              spacing,
              viewbox,
              selector,
              linkPath,
              nodeTransform,
              color: color2,
              layout: model.treeLayout,
              svg
            })(laidOutRoot_);
          };
        };
      };
    };
  };
  var configureAndRunScript3 = /* @__PURE__ */ configureAndRunScript(bindD3PrinterM)(d3Tagless);
  var getPrintTree = function(treeModel) {
    return liftEffect7(function __do3() {
      var widthHeight = getWindowWidthHeight();
      var printedScript = runPrinter(configureAndRunScript3(widthHeight)(treeModel)("Printer Interpreter Root> "))("Tree Script")();
      return snd(printedScript);
    });
  };

  // output/Effect.Random/foreign.js
  var random = Math.random;

  // output/Web.DOM.Document/foreign.js
  var getEffProp3 = function(name16) {
    return function(doc) {
      return function() {
        return doc[name16];
      };
    };
  };
  var url = getEffProp3("URL");
  var documentURI = getEffProp3("documentURI");
  var origin2 = getEffProp3("origin");
  var compatMode = getEffProp3("compatMode");
  var characterSet = getEffProp3("characterSet");
  var contentType = getEffProp3("contentType");
  var _documentElement2 = getEffProp3("documentElement");

  // output/Web.DOM.Document/index.js
  var toNonElementParentNode = unsafeCoerce2;

  // output/Web.DOM.NonElementParentNode/foreign.js
  function _getElementById(id5) {
    return function(node) {
      return function() {
        return node.getElementById(id5);
      };
    };
  }

  // output/Web.DOM.NonElementParentNode/index.js
  var map28 = /* @__PURE__ */ map(functorEffect);
  var getElementById = function(eid) {
    var $2 = map28(toMaybe);
    var $3 = _getElementById(eid);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/V2.Components.Visualization/index.js
  var bind15 = /* @__PURE__ */ bind(bindHalogenM);
  var get6 = /* @__PURE__ */ get(monadStateHalogenM);
  var draw6 = /* @__PURE__ */ draw3(bindD3M)(monadEffD3M)(d3TaglessD3M);
  var pure14 = /* @__PURE__ */ pure(applicativeHalogenM);
  var draw1 = /* @__PURE__ */ draw(bindD3M)(monadEffD3M)(d3TaglessD3M);
  var draw22 = /* @__PURE__ */ draw4(bindD3M)(monadEffD3M)(d3TaglessD3M);
  var drawQuartet2 = /* @__PURE__ */ drawQuartet(bindD3M)(monadEffD3M)(d3TaglessD3M);
  var draw32 = /* @__PURE__ */ draw2(bindD3M)(monadEffD3M)(d3TaglessD3M);
  var drawThreeCircles2 = /* @__PURE__ */ drawThreeCircles(d3TaglessD3M);
  var exGeneralUpdatePattern2 = /* @__PURE__ */ exGeneralUpdatePattern(d3TaglessD3M);
  var discard10 = /* @__PURE__ */ discard(discardUnit);
  var discard12 = /* @__PURE__ */ discard10(bindHalogenM);
  var bind22 = /* @__PURE__ */ bind(bindAff);
  var liftEffect8 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard23 = /* @__PURE__ */ discard10(bindAff);
  var pure23 = /* @__PURE__ */ pure(applicativeAff);
  var makeModel2 = /* @__PURE__ */ makeModel(bindAff)(monadEffectAff);
  var modify_5 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var traverse_7 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var Initialize2 = /* @__PURE__ */ (function() {
    function Initialize6() {
    }
    ;
    Initialize6.value = new Initialize6();
    return Initialize6;
  })();
  var HighlightCode = /* @__PURE__ */ (function() {
    function HighlightCode3() {
    }
    ;
    HighlightCode3.value = new HighlightCode3();
    return HighlightCode3;
  })();
  var render3 = function(state3) {
    return div2([classes(["visualization"]), id2(state3.containerId)])((function() {
      if (state3.textOutput instanceof Just) {
        return [pre([classes(["visualization__text-output", "line-numbers"])])([code([classes(["language-javascript"]), id2("generated-js-" + state3.exampleId)])([text5(state3.textOutput.value0)])])];
      }
      ;
      if (state3.textOutput instanceof Nothing) {
        return [];
      }
      ;
      throw new Error("Failed pattern match at V2.Components.Visualization (line 83, column 7 - line 94, column 22): " + [state3.textOutput.constructor.name]);
    })());
  };
  var initialState2 = function(input3) {
    return {
      exampleId: input3.exampleId,
      containerId: "viz-container-" + input3.exampleId,
      textOutput: Nothing.value
    };
  };
  var getRandomLetters = /* @__PURE__ */ (function() {
    var letters = toCharArray("abcdefghijklmnopqrstuvwxyz");
    var coinToss = function(c) {
      return function __do3() {
        var n = random();
        var $51 = n > 0.6;
        if ($51) {
          return new Just(c);
        }
        ;
        return Nothing.value;
      };
    };
    return function __do3() {
      var choices = sequence(traversableArray)(applicativeEffect)(map(functorArray)(coinToss)(letters))();
      return catMaybes(choices);
    };
  })();
  var handleAction2 = function(dictMonadAff) {
    var liftEffect12 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
    return function(v) {
      if (v instanceof Initialize2) {
        return bind15(get6)(function(state3) {
          var selector = "#" + state3.containerId;
          if (state3.exampleId === "line-chart") {
            return bind15(liftEffect12(eval_D3M(draw6(sineWaveData)(selector))))(function() {
              return pure14(unit);
            });
          }
          ;
          if (state3.exampleId === "bar-chart") {
            return bind15(liftEffect12(eval_D3M(draw1(monthlySales)(selector))))(function() {
              return pure14(unit);
            });
          }
          ;
          if (state3.exampleId === "scatter-plot") {
            return bind15(liftEffect12(eval_D3M(draw22(scatterData)(selector))))(function() {
              return pure14(unit);
            });
          }
          ;
          if (state3.exampleId === "scatter-quartet") {
            return bind15(liftEffect12(eval_D3M(drawQuartet2(anscombesQuartet)(selector))))(function() {
              return pure14(unit);
            });
          }
          ;
          if (state3.exampleId === "chord-diagram") {
            return bind15(liftEffect12(eval_D3M(draw32(exampleMatrix)(exampleLabels)(selector))))(function() {
              return pure14(unit);
            });
          }
          ;
          if (state3.exampleId === "three-little-circles") {
            return bind15(liftEffect12(eval_D3M(drawThreeCircles2(selector))))(function() {
              return pure14(unit);
            });
          }
          ;
          if (state3.exampleId === "general-update-pattern") {
            return bind15(liftEffect12(eval_D3M(exGeneralUpdatePattern2(selector))))(function(update) {
              return discard12(liftAff2(bind22(liftEffect8(getRandomLetters))(function(letters1) {
                return bind22(liftEffect8(runD3M(update(letters1))))(function() {
                  return discard23(delay(2300))(function() {
                    return bind22(liftEffect8(getRandomLetters))(function(letters2) {
                      return bind22(liftEffect8(runD3M(update(letters2))))(function() {
                        return discard23(delay(2300))(function() {
                          return bind22(liftEffect8(getRandomLetters))(function(letters3) {
                            return bind22(liftEffect8(runD3M(update(letters3))))(function() {
                              return pure23(unit);
                            });
                          });
                        });
                      });
                    });
                  });
                });
              })))(function() {
                return pure14(unit);
              });
            });
          }
          ;
          if (state3.exampleId === "print-tree") {
            return bind15(liftAff2(getTreeViaAJAX("./data/flare-2.json")))(function(treeJSON) {
              return discard12((function() {
                if (treeJSON instanceof Left) {
                  return pure14(unit);
                }
                ;
                if (treeJSON instanceof Right) {
                  return bind15(liftAff2(makeModel2(TidyTree.value)(Radial.value)(treeJSON.value0)))(function(treeModel) {
                    return bind15(liftAff2(getPrintTree(treeModel)))(function(textRep) {
                      return discard12(modify_5(function(v1) {
                        var $56 = {};
                        for (var $57 in v1) {
                          if ({}.hasOwnProperty.call(v1, $57)) {
                            $56[$57] = v1[$57];
                          }
                          ;
                        }
                        ;
                        $56.textOutput = new Just(textRep);
                        return $56;
                      }))(function() {
                        return handleAction2(dictMonadAff)(HighlightCode.value);
                      });
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at V2.Components.Visualization (line 155, column 9 - line 165, column 39): " + [treeJSON.constructor.name]);
              })())(function() {
                return pure14(unit);
              });
            });
          }
          ;
          return pure14(unit);
        });
      }
      ;
      if (v instanceof HighlightCode) {
        return bind15(get6)(function(state3) {
          if (state3.textOutput instanceof Just) {
            return liftEffect12(function __do3() {
              var win = windowImpl();
              var htmlDoc = document2(win)();
              var doc = toDocument(htmlDoc);
              var node = toNonElementParentNode(doc);
              var maybeEl = getElementById("generated-js-" + state3.exampleId)(node)();
              return traverse_7(highlightElement2)(maybeEl)();
            });
          }
          ;
          if (state3.textOutput instanceof Nothing) {
            return pure14(unit);
          }
          ;
          throw new Error("Failed pattern match at V2.Components.Visualization (line 178, column 5 - line 188, column 27): " + [state3.textOutput.constructor.name]);
        });
      }
      ;
      throw new Error("Failed pattern match at V2.Components.Visualization (line 98, column 16 - line 188, column 27): " + [v.constructor.name]);
    };
  };
  var component3 = function(dictMonadAff) {
    return mkComponent({
      initialState: initialState2,
      render: render3,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction2(dictMonadAff),
        initialize: new Just(Initialize2.value)
      })
    });
  };

  // output/V2.Components.SplitPane/index.js
  var modify_6 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var bind6 = /* @__PURE__ */ bind(bindHalogenM);
  var get7 = /* @__PURE__ */ get(monadStateHalogenM);
  var traverse_8 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var slot_2 = /* @__PURE__ */ slot_()({
    reflectSymbol: function() {
      return "visualization";
    }
  })(ordUnit);
  var CodeTab = /* @__PURE__ */ (function() {
    function CodeTab2() {
    }
    ;
    CodeTab2.value = new CodeTab2();
    return CodeTab2;
  })();
  var VisualizationTab = /* @__PURE__ */ (function() {
    function VisualizationTab2() {
    }
    ;
    VisualizationTab2.value = new VisualizationTab2();
    return VisualizationTab2;
  })();
  var Initialize3 = /* @__PURE__ */ (function() {
    function Initialize6() {
    }
    ;
    Initialize6.value = new Initialize6();
    return Initialize6;
  })();
  var SetTab = /* @__PURE__ */ (function() {
    function SetTab2(value0) {
      this.value0 = value0;
    }
    ;
    SetTab2.create = function(value0) {
      return new SetTab2(value0);
    };
    return SetTab2;
  })();
  var HighlightCode2 = /* @__PURE__ */ (function() {
    function HighlightCode3() {
    }
    ;
    HighlightCode3.value = new HighlightCode3();
    return HighlightCode3;
  })();
  var initialState3 = function(input3) {
    return {
      code: input3.code,
      language: input3.language,
      visualizationUrl: input3.visualizationUrl,
      exampleId: input3.exampleId,
      activeTab: CodeTab.value
    };
  };
  var handleAction3 = function(dictMonadAff) {
    var liftEffect10 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    return function(v) {
      if (v instanceof Initialize3) {
        return handleAction3(dictMonadAff)(HighlightCode2.value);
      }
      ;
      if (v instanceof SetTab) {
        return modify_6(function(v1) {
          var $31 = {};
          for (var $32 in v1) {
            if ({}.hasOwnProperty.call(v1, $32)) {
              $31[$32] = v1[$32];
            }
            ;
          }
          ;
          $31.activeTab = v.value0;
          return $31;
        });
      }
      ;
      if (v instanceof HighlightCode2) {
        return bind6(get7)(function(state3) {
          return liftEffect10(function __do3() {
            var win = windowImpl();
            var htmlDoc = document2(win)();
            var doc = toDocument(htmlDoc);
            var node = toNonElementParentNode(doc);
            var maybeEl = getElementById("code-" + state3.exampleId)(node)();
            return traverse_8(highlightElement)(maybeEl)();
          });
        });
      }
      ;
      throw new Error("Failed pattern match at V2.Components.SplitPane (line 155, column 16 - line 172, column 41): " + [v.constructor.name]);
    };
  };
  var eqTab = {
    eq: function(x7) {
      return function(y6) {
        if (x7 instanceof CodeTab && y6 instanceof CodeTab) {
          return true;
        }
        ;
        if (x7 instanceof VisualizationTab && y6 instanceof VisualizationTab) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq5 = /* @__PURE__ */ eq(eqTab);
  var _visualization = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var render4 = function(dictMonadAff) {
    var component12 = component3(dictMonadAff);
    return function(state3) {
      return div2([classes(["split-pane"])])([div2([classes(["split-pane__tabs", "mobile-only"])])([button([classes(append13(["split-pane__tab"])((function() {
        var $37 = eq5(state3.activeTab)(CodeTab.value);
        if ($37) {
          return ["split-pane__tab--active"];
        }
        ;
        return [];
      })())), onClick(function(v) {
        return new SetTab(CodeTab.value);
      })])([text5("Code")]), button([classes(append13(["split-pane__tab"])((function() {
        var $38 = eq5(state3.activeTab)(VisualizationTab.value);
        if ($38) {
          return ["split-pane__tab--active"];
        }
        ;
        return [];
      })())), onClick(function(v) {
        return new SetTab(VisualizationTab.value);
      })])([text5("Visualization")])]), div2([classes(["split-pane__content"])])([div2([classes(append13(["split-pane__panel", "split-pane__panel--code"])((function() {
        var $39 = eq5(state3.activeTab)(CodeTab.value);
        if ($39) {
          return [];
        }
        ;
        return ["split-pane__panel--hidden"];
      })()))])([div2([classes(["split-pane__code-header"])])([h3_([text5("PureScript Code")]), button([classes(["split-pane__copy-button"]), title3("Copy to clipboard")])([text5("Copy")])]), pre([classes(["line-numbers"])])([code([classes(["language-" + state3.language]), id2("code-" + state3.exampleId)])([text5(state3.code)])])]), div2([classes(append13(["split-pane__panel", "split-pane__panel--visualization"])((function() {
        var $40 = eq5(state3.activeTab)(VisualizationTab.value) || true;
        if ($40) {
          return [];
        }
        ;
        return ["split-pane__panel--hidden"];
      })()))])([div2([classes(["split-pane__viz-header"])])([h3_([text5("Visualization")])]), div2([classes(["split-pane__viz-content"])])([slot_2(_visualization)(unit)(component12)({
        exampleId: state3.exampleId
      })])])])]);
    };
  };
  var component4 = function(dictMonadAff) {
    return mkComponent({
      initialState: initialState3,
      render: render4(dictMonadAff),
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction3(dictMonadAff),
        initialize: new Just(Initialize3.value)
      })
    });
  };

  // output/V2.Data.CodeFiles/index.js
  var fromFoldable4 = /* @__PURE__ */ fromFoldable(ordString)(foldableArray);
  var lookup6 = /* @__PURE__ */ lookup(ordString);
  var bind7 = /* @__PURE__ */ bind(bindMaybe);
  var pure15 = /* @__PURE__ */ pure(applicativeMaybe);
  var visualizationUrlMap = /* @__PURE__ */ (function() {
    return fromFoldable4([new Tuple("line-chart", "../v1/index.html#line-chart"), new Tuple("bar-chart", "../v1/index.html#bar-chart"), new Tuple("scatter-plot", "../v1/index.html#scatter-plot"), new Tuple("scatter-quartet", "../v1/index.html#scatter-quartet"), new Tuple("chord-diagram", "../v1/index.html#chord-diagram"), new Tuple("bubble-chart", "../v1/index.html#bubble-chart"), new Tuple("sankey-diagram", "../v1/index.html#sankey-diagram"), new Tuple("tree-layout", "../v1/index.html#tree-layout"), new Tuple("three-little-circles", "../v1/index.html#three-little-circles"), new Tuple("general-update-pattern", "../v1/index.html#general-update-pattern"), new Tuple("les-miserables", "../v1/index.html#les-miserables"), new Tuple("meta-tree", "../v1/index.html#metatree"), new Tuple("print-tree", "../v1/index.html#string-generator"), new Tuple("spago-explorer", "../v1/index.html#spago-explorer")]);
  })();
  var getVisualizationUrl = function(exampleId) {
    return lookup6(exampleId)(visualizationUrlMap);
  };
  var codeFileMap = /* @__PURE__ */ (function() {
    return fromFoldable4([new Tuple("line-chart", "LineChartDraw"), new Tuple("bar-chart", "BarChartDraw"), new Tuple("scatter-plot", "ScatterPlotDraw"), new Tuple("scatter-quartet", "ScatterPlotQuartet"), new Tuple("chord-diagram", "ChordDiagramDraw"), new Tuple("bubble-chart", "BubbleChartDraw"), new Tuple("sankey-diagram", "SankeyDraw"), new Tuple("tree-layout", "TreeDraw"), new Tuple("three-little-circles", "3LC"), new Tuple("general-update-pattern", "GUP"), new Tuple("les-miserables", "LesMisScript"), new Tuple("meta-tree", "MetaTreeDraw"), new Tuple("print-tree", "PrintTreeHandleActions"), new Tuple("spago-explorer", "LesMisScript")]);
  })();
  var getCodeFile = function(exampleId) {
    return lookup6(exampleId)(codeFileMap);
  };
  var getCodeFileUrl = function(exampleId) {
    return bind7(getCodeFile(exampleId))(function(filename) {
      return pure15("./code-examples/" + filename);
    });
  };

  // output/V2.Pages.ExampleDetail/index.js
  var bind8 = /* @__PURE__ */ bind(bindHalogenM);
  var get8 = /* @__PURE__ */ get(monadStateHalogenM);
  var modify_7 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var discard11 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var slot_3 = /* @__PURE__ */ slot_()({
    reflectSymbol: function() {
      return "splitPane";
    }
  })(ordUnit);
  var Initialize4 = /* @__PURE__ */ (function() {
    function Initialize6() {
    }
    ;
    Initialize6.value = new Initialize6();
    return Initialize6;
  })();
  var LoadCode = /* @__PURE__ */ (function() {
    function LoadCode2() {
    }
    ;
    LoadCode2.value = new LoadCode2();
    return LoadCode2;
  })();
  var initialState4 = function(exampleId) {
    return {
      exampleId,
      code: Nothing.value,
      loading: true,
      error: Nothing.value
    };
  };
  var handleAction4 = function(dictMonadAff) {
    var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
    return function(v) {
      if (v instanceof Initialize4) {
        return handleAction4(dictMonadAff)(LoadCode.value);
      }
      ;
      if (v instanceof LoadCode) {
        return bind8(get8)(function(state3) {
          var v1 = getCodeFileUrl(state3.exampleId);
          if (v1 instanceof Nothing) {
            return modify_7(function(v2) {
              var $28 = {};
              for (var $29 in v2) {
                if ({}.hasOwnProperty.call(v2, $29)) {
                  $28[$29] = v2[$29];
                }
                ;
              }
              ;
              $28.loading = false;
              $28.error = new Just("Code file not found");
              return $28;
            });
          }
          ;
          if (v1 instanceof Just) {
            return discard11(modify_7(function(v2) {
              var $31 = {};
              for (var $32 in v2) {
                if ({}.hasOwnProperty.call(v2, $32)) {
                  $31[$32] = v2[$32];
                }
                ;
              }
              ;
              $31.loading = true;
              $31.error = Nothing.value;
              return $31;
            }))(function() {
              return bind8(liftAff2(get3(string)(v1.value0)))(function(result) {
                if (result instanceof Left) {
                  return modify_7(function(v2) {
                    var $35 = {};
                    for (var $36 in v2) {
                      if ({}.hasOwnProperty.call(v2, $36)) {
                        $35[$36] = v2[$36];
                      }
                      ;
                    }
                    ;
                    $35.loading = false;
                    $35.error = new Just("Failed to load code: " + printError(result.value0));
                    return $35;
                  });
                }
                ;
                if (result instanceof Right) {
                  return modify_7(function(v2) {
                    var $39 = {};
                    for (var $40 in v2) {
                      if ({}.hasOwnProperty.call(v2, $40)) {
                        $39[$40] = v2[$40];
                      }
                      ;
                    }
                    ;
                    $39.loading = false;
                    $39.code = new Just(result.value0.body);
                    $39.error = Nothing.value;
                    return $39;
                  });
                }
                ;
                throw new Error("Failed pattern match at V2.Pages.ExampleDetail (line 158, column 9 - line 169, column 16): " + [result.constructor.name]);
              });
            });
          }
          ;
          throw new Error("Failed pattern match at V2.Pages.ExampleDetail (line 152, column 5 - line 169, column 16): " + [v1.constructor.name]);
        });
      }
      ;
      throw new Error("Failed pattern match at V2.Pages.ExampleDetail (line 146, column 16 - line 169, column 16): " + [v.constructor.name]);
    };
  };
  var _splitPane = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var render5 = function(dictMonadAff) {
    var component12 = component4(dictMonadAff);
    return function(state3) {
      var v = getExample(state3.exampleId);
      if (v instanceof Nothing) {
        return div2([classes(["example-detail"])])([div2([classes(["example-detail__not-found"])])([h1_([text5("Example Not Found")]), p_([text5("Could not find example with ID: " + state3.exampleId)]), a([href4(routeToHash(Gallery.value)), classes(["example-detail__back-link"])])([text5("\u2190 Back to Gallery")])])]);
      }
      ;
      if (v instanceof Just) {
        return div2([classes(["example-detail"])])([div2([classes(["example-detail__header"])])([a([href4(routeToHash(Gallery.value)), classes(["example-detail__back-link"])])([text5("\u2190 Back to Gallery")]), h1([classes(["example-detail__title"])])([text5(v.value0.title)]), p([classes(["example-detail__description"])])([text5(v.value0.description)])]), div2([classes(["example-detail__about"])])([h2([classes(["example-detail__about-title"])])([text5("About This Example")]), p([classes(["example-detail__about-text"])])([text5(v.value0.about)])]), div2([classes(["example-detail__content"])])([(function() {
          if (state3.loading) {
            return div2([classes(["example-detail__loading"])])([text5("Loading code...")]);
          }
          ;
          if (state3.error instanceof Just) {
            return div2([classes(["example-detail__error"])])([h3_([text5("Error Loading Code")]), p_([text5(state3.error.value0)]), p_([text5("View this example in "), a([href4("../v1/"), target5("_blank")])([text5("V1")])])]);
          }
          ;
          if (state3.error instanceof Nothing) {
            if (state3.code instanceof Just) {
              return slot_3(_splitPane)(unit)(component12)({
                code: state3.code.value0,
                language: "haskell",
                visualizationUrl: getVisualizationUrl(state3.exampleId),
                exampleId: state3.exampleId
              });
            }
            ;
            if (state3.code instanceof Nothing) {
              return div2([classes(["example-detail__no-code"])])([text5("No code available for this example.")]);
            }
            ;
            throw new Error("Failed pattern match at V2.Pages.ExampleDetail (line 130, column 19 - line 141, column 74): " + [state3.code.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at V2.Pages.ExampleDetail (line 114, column 20 - line 141, column 74): " + [state3.error.constructor.name]);
        })()])]);
      }
      ;
      throw new Error("Failed pattern match at V2.Pages.ExampleDetail (line 61, column 3 - line 143, column 10): " + [v.constructor.name]);
    };
  };
  var component5 = function(dictMonadAff) {
    return mkComponent({
      initialState: initialState4,
      render: render5(dictMonadAff),
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction4(dictMonadAff),
        initialize: new Just(Initialize4.value)
      })
    });
  };

  // output/V2.Pages.Home/index.js
  var append8 = /* @__PURE__ */ append(semigroupArray);
  var navCard = function(href5) {
    return function(emoji) {
      return function(title5) {
        return function(description) {
          return function(isExternal) {
            return a(append8([href4(href5), classes(["home__nav-card"])])((function() {
              if (isExternal) {
                return [target5("_blank"), rel4("noopener noreferrer")];
              }
              ;
              return [];
            })()))([div2([classes(["home__nav-card-icon"])])([text5(emoji)]), h3([classes(["home__nav-card-title"])])([text5(title5)]), p([classes(["home__nav-card-description"])])([text5(description)])]);
          };
        };
      };
    };
  };
  var feature = function(emoji) {
    return function(title5) {
      return function(description) {
        return div2([classes(["home__feature"])])([div2([classes(["home__feature-icon"])])([text5(emoji)]), h3([classes(["home__feature-title"])])([text5(title5)]), p([classes(["home__feature-description"])])([text5(description)])]);
      };
    };
  };
  var render6 = function(v) {
    return div2([classes(["home"])])([section([classes(["home__hero"])])([div2([classes(["home__hero-content"])])([h1([classes(["home__title"])])([text5("PureScript Tagless D3")]), p([classes(["home__subtitle"])])([text5("Type-safe, composable data visualization")]), p([classes(["home__description"])])([text5("A PureScript library implementing a Finally Tagless embedded DSL for building interactive data visualizations with D3.js")]), div2([classes(["home__cta-cards"])])([navCard(routeToHash(Gallery.value))("\u{1F4CA}")("Gallery")("Browse simple chart examples")(false), navCard(routeToHash(Spago.value))("\u{1F50D}")("Spago Explorer")("Real-world application demo")(false), navCard(routeToHash(Interpreters.value))("\u{1F3AD}")("Interpreters")("Alternative interpretations")(false), navCard("https://github.com/afcondon/purescript-d3-tagless")("\u{1F4BB}")("GitHub")("View source code")(true)])])]), section([classes(["home__features"])])([h2([classes(["home__section-title"])])([text5("Key Features")]), div2([classes(["home__features-grid"])])([feature("\u{1F3A8}")("Type-Safe")("Strong type safety with PureScript's type system prevents runtime errors"), feature("\u{1F527}")("Composable")("Build complex visualizations from simple, reusable components"), feature("\u{1F3AD}")("Multiple Interpreters")("Finally Tagless pattern enables different interpretations of the same DSL"), feature("\u{1F4CA}")("D3-Powered")("Leverages D3.js for battle-tested visualization rendering"), feature("\u26A1")("Interactive")("Support for drag, zoom, and other interactive behaviors"), feature("\u{1F4DA}")("Well-Documented")("Comprehensive examples with side-by-side D3 JavaScript comparisons")])]), section([classes(["home__explanation"])])([h2([classes(["home__section-title"])])([text5("What is Finally Tagless?")]), p([classes(["home__text"])])([text5("The Finally Tagless pattern separates the description of a computation from its interpretation. In this library, you describe visualizations using a domain-specific language (DSL), and the same description can be:")]), ul([classes(["home__list"])])([li_([text5("Rendered as an actual D3 visualization")]), li_([text5("Visualized as a tree showing the DSL structure (MetaTree interpreter)")]), li_([text5("Converted to documentation or code (String interpreter)")])]), p([classes(["home__text"])])([text5("This powerful abstraction makes code more testable, reusable, and easier to reason about.")])])]);
  };
  var component6 = /* @__PURE__ */ mkComponent({
    initialState: function(v) {
      return unit;
    },
    render: render6,
    "eval": /* @__PURE__ */ mkEval(defaultEval)
  });

  // output/V2.Pages.Interpreters/index.js
  var render7 = function(v) {
    return div2([classes(["interpreters-page"])])([div2([classes(["interpreters-page__header"])])([h1([classes(["interpreters-page__title"])])([text5("Alternative Interpreters")]), p([classes(["interpreters-page__subtitle"])])([text5("Demonstrating the Finally Tagless pattern")])]), div2([classes(["interpreters-page__content"])])([div2([classes(["interpreters-page__about"])])([h2_([text5("What is Finally Tagless?")]), p_([text5("Finally Tagless is an advanced functional programming pattern that allows the same code to be interpreted in multiple ways. Instead of tying your code to a specific implementation, you write against a set of type class capabilities.")]), p_([text5("This library demonstrates this by providing multiple interpreters for the same visualization code:")])]), div2([classes(["interpreters-page__grid"])])([div2([classes(["interpreter-card"])])([h3_([text5("MetaTree Visualizer")]), p_([text5("Instead of rendering to SVG, this interpreter visualizes the abstract syntax tree of the visualization itself as a tree diagram.")]), p_([text5("This meta-visualization shows how the same code can be interpreted in completely different ways - a key advantage of the Finally Tagless approach.")]), a([href4(routeToHash(new Example("meta-tree"))), classes(["interpreter-card__link"])])([text5("View MetaTree \u2192")])]), div2([classes(["interpreter-card"])])([h3_([text5("String Generator")]), p_([text5("This interpreter takes visualization code and generates human-readable text descriptions or code snippets.")]), p_([text5("Shows how the same high-level visualization definition can be used for documentation generation, code analysis, or teaching - all without modifying the original visualization code.")]), a([href4(routeToHash(new Example("print-tree"))), classes(["interpreter-card__link"])])([text5("View String Generator \u2192")])])]), div2([classes(["interpreters-page__explanation"])])([h3_([text5("Why Multiple Interpreters?")]), p_([text5("The ability to run the same code through different interpreters provides several benefits:")]), ul_([li_([text5("Debugging: Visualize the structure of your visualization")]), li_([text5("Documentation: Generate descriptions automatically")]), li_([text5("Testing: Verify structure without rendering")]), li_([text5("Flexibility: Add new interpretations without changing existing code")])]), p_([a([href4(routeToHash(Gallery.value))])([text5("\u2190 Back to Gallery")])])])])]);
  };
  var component7 = /* @__PURE__ */ mkComponent({
    initialState: /* @__PURE__ */ identity(categoryFn),
    render: render7,
    "eval": /* @__PURE__ */ mkEval(defaultEval)
  });

  // output/V2.Pages.Spago/index.js
  var render8 = function(v) {
    return div2([classes(["spago-page"])])([div2([classes(["spago-page__header"])])([h1([classes(["spago-page__title"])])([text5("Spago Dependency Explorer")]), p([classes(["spago-page__subtitle"])])([text5("Interactive visualization of PureScript package dependencies")])]), div2([classes(["spago-page__content"])])([div2([classes(["spago-page__about"])])([h2_([text5("About This Application")]), p_([text5("The Spago Dependency Explorer is a real-world application demonstrating how D3 visualizations integrate into larger Halogen applications with bidirectional communication.")]), p_([text5("This application fetches and visualizes PureScript package dependencies from your spago.dhall file, using a force-directed layout to show package relationships. Features include:")]), ul_([li_([text5("Force-directed graph layout showing package dependencies")]), li_([text5("Interactive drag to reposition nodes")]), li_([text5("Zoom and pan navigation")]), li_([text5("Package filtering and search")]), li_([text5("Bidirectional Halogen \u2194 D3 event communication")])]), p_([text5("This demonstrates what the library was built for: serious, interactive applications where visualization events trigger app-level actions and app state updates drive visualization changes.")])]), div2([classes(["spago-page__placeholder"])])([h3_([text5("Coming Soon")]), p_([text5("The full Spago Explorer application is being ported to V2.")]), p_([text5("For now, you can view it in "), a([href4("../v1/#/spago"), target5("_blank")])([text5("V1")]), text5(".")]), p_([a([href4(routeToHash(Gallery.value))])([text5("\u2190 Back to Gallery")])])])])]);
  };
  var component8 = /* @__PURE__ */ mkComponent({
    initialState: /* @__PURE__ */ identity(categoryFn),
    render: render8,
    "eval": /* @__PURE__ */ mkEval(defaultEval)
  });

  // output/V2.Main/index.js
  var bind9 = /* @__PURE__ */ bind(bindHalogenM);
  var liftEffect9 = /* @__PURE__ */ liftEffect(/* @__PURE__ */ monadEffectHalogenM(monadEffectAff));
  var bind16 = /* @__PURE__ */ bind(bindEffect);
  var discard13 = /* @__PURE__ */ discard(discardUnit);
  var discard14 = /* @__PURE__ */ discard13(bindHalogenM);
  var modify_8 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var mempty2 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidEffect(monoidUnit));
  var pure16 = /* @__PURE__ */ pure(applicativeHalogenM);
  var slot_4 = /* @__PURE__ */ slot_();
  var slot_1 = /* @__PURE__ */ slot_4({
    reflectSymbol: function() {
      return "home";
    }
  })(ordUnit);
  var slot2 = /* @__PURE__ */ slot()({
    reflectSymbol: function() {
      return "gallery";
    }
  })(ordUnit);
  var slot_22 = /* @__PURE__ */ slot_4({
    reflectSymbol: function() {
      return "exampleDetail";
    }
  })(ordUnit);
  var component1 = /* @__PURE__ */ component5(monadAffAff);
  var slot_32 = /* @__PURE__ */ slot_4({
    reflectSymbol: function() {
      return "spago";
    }
  })(ordUnit);
  var slot_42 = /* @__PURE__ */ slot_4({
    reflectSymbol: function() {
      return "interpreters";
    }
  })(ordUnit);
  var slot_5 = /* @__PURE__ */ slot_4({
    reflectSymbol: function() {
      return "navigation";
    }
  })(ordUnit);
  var Initialize5 = /* @__PURE__ */ (function() {
    function Initialize6() {
    }
    ;
    Initialize6.value = new Initialize6();
    return Initialize6;
  })();
  var Navigate = /* @__PURE__ */ (function() {
    function Navigate2(value0) {
      this.value0 = value0;
    }
    ;
    Navigate2.create = function(value0) {
      return new Navigate2(value0);
    };
    return Navigate2;
  })();
  var HandleGalleryOutput = /* @__PURE__ */ (function() {
    function HandleGalleryOutput2(value0) {
      this.value0 = value0;
    }
    ;
    HandleGalleryOutput2.create = function(value0) {
      return new HandleGalleryOutput2(value0);
    };
    return HandleGalleryOutput2;
  })();
  var HashChanged = /* @__PURE__ */ (function() {
    function HashChanged2(value0) {
      this.value0 = value0;
    }
    ;
    HashChanged2.create = function(value0) {
      return new HashChanged2(value0);
    };
    return HashChanged2;
  })();
  var handleAction5 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof Initialize5) {
        $tco_done = true;
        return bind9(liftEffect9(bind16(bind16(windowImpl)(location))(hash)))(function(currentHash) {
          var route2 = parseRoute(currentHash);
          return discard14(modify_8(function(v1) {
            var $42 = {};
            for (var $43 in v1) {
              if ({}.hasOwnProperty.call(v1, $43)) {
                $42[$43] = v1[$43];
              }
              ;
            }
            ;
            $42.currentRoute = route2;
            return $42;
          }))(function() {
            return bind9(subscribe2(makeEmitter(function(push2) {
              return function __do3() {
                var win = windowImpl();
                var target7 = toEventTarget(win);
                var listener = eventListener(function(v1) {
                  return function __do4() {
                    var newHash = bind16(bind16(windowImpl)(location))(hash)();
                    return push2(new HashChanged(newHash))();
                  };
                })();
                addEventListener("hashchange")(listener)(false)(target7)();
                return mempty2;
              };
            })))(function() {
              return pure16(unit);
            });
          });
        });
      }
      ;
      if (v instanceof Navigate) {
        $tco_done = true;
        return discard14(modify_8(function(v1) {
          var $45 = {};
          for (var $46 in v1) {
            if ({}.hasOwnProperty.call(v1, $46)) {
              $45[$46] = v1[$46];
            }
            ;
          }
          ;
          $45.currentRoute = v.value0;
          return $45;
        }))(function() {
          var newHash = routeToHash(v.value0);
          return liftEffect9(function __do3() {
            var win = windowImpl();
            var loc = location(win)();
            return setHash(newHash)(loc)();
          });
        });
      }
      ;
      if (v instanceof HandleGalleryOutput) {
        $copy_v = new Navigate(new Example(v.value0));
        return;
      }
      ;
      if (v instanceof HashChanged) {
        var route = parseRoute(v.value0);
        $tco_done = true;
        return modify_8(function(v1) {
          var $50 = {};
          for (var $51 in v1) {
            if ({}.hasOwnProperty.call(v1, $51)) {
              $50[$51] = v1[$51];
            }
            ;
          }
          ;
          $50.currentRoute = route;
          return $50;
        });
      }
      ;
      throw new Error("Failed pattern match at V2.Main (line 111, column 16 - line 147, column 41): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var _spago = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _navigation = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _interpreters = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _home = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _gallery = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _exampleDetail = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var renderPage = function(route) {
    if (route instanceof Home) {
      return slot_1(_home)(unit)(component6)(unit);
    }
    ;
    if (route instanceof Gallery) {
      return slot2(_gallery)(unit)(component)(unit)(HandleGalleryOutput.create);
    }
    ;
    if (route instanceof Example) {
      return slot_22(_exampleDetail)(unit)(component1)(route.value0);
    }
    ;
    if (route instanceof Spago) {
      return slot_32(_spago)(unit)(component8)(unit);
    }
    ;
    if (route instanceof Interpreters) {
      return slot_42(_interpreters)(unit)(component7)(unit);
    }
    ;
    if (route instanceof NotFound) {
      return div2([classes(["not-found"])])([h1_([text5("404 - Page Not Found")]), p_([text5("The page you're looking for doesn't exist.")]), a([href4(routeToHash(Home.value))])([text5("Go Home")])]);
    }
    ;
    throw new Error("Failed pattern match at V2.Main (line 84, column 20 - line 108, column 8): " + [route.constructor.name]);
  };
  var render9 = function(state3) {
    return div2([classes(["app"])])([slot_5(_navigation)(unit)(component2)(state3.currentRoute), main([classes(["app__main"])])([renderPage(state3.currentRoute)])]);
  };
  var component9 = /* @__PURE__ */ (function() {
    return mkComponent({
      initialState: function(v) {
        return {
          currentRoute: Home.value
        };
      },
      render: render9,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction5,
        initialize: new Just(Initialize5.value)
      })
    });
  })();
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(component9)(unit)(body2);
  }));

  // <stdin>
  main2();
})();

(() => {
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
        return function(x10) {
          return f(g(x10));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };
  var composeFlipped = function(dictSemigroupoid) {
    var compose1 = compose(dictSemigroupoid);
    return function(f) {
      return function(g) {
        return compose1(g)(f);
      };
    };
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x10) {
      return x10;
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
      return function(x10) {
        return function(y8) {
          return f(g(x10))(g(y8));
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
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map115 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map115(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map115 = map(dictFunctor);
    return function(f) {
      return function(x10) {
        return map115($$const(x10))(f);
      };
    };
  };
  var voidRight = function(dictFunctor) {
    var map115 = map(dictFunctor);
    return function(x10) {
      return map115($$const(x10));
    };
  };
  var functorArray = {
    map: arrayMap
  };
  var flap = function(dictFunctor) {
    var map115 = map(dictFunctor);
    return function(ff2) {
      return function(x10) {
        return map115(function(f) {
          return f(x10);
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
    var map55 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map55($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure111 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure111(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure111 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure111(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply4 = apply(dictApplicative.Apply0());
    var pure111 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply4(pure111(f))(a2);
      };
    };
  };
  var applicativeArray = {
    pure: function(x10) {
      return [x10];
    },
    Apply0: function() {
      return applyArray;
    }
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
  var composeKleisli = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind18(f(a2))(g);
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
    return function(eq8) {
      return function(gt) {
        return function(x10) {
          return function(y8) {
            return x10 < y8 ? lt : x10 === y8 ? eq8 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label6) {
    return function(rec) {
      return rec[label6];
    };
  };
  var unsafeSet = function(label6) {
    return function(value15) {
      return function(rec) {
        var copy2 = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy2[key] = rec[key];
          }
        }
        copy2[label6] = value15;
        return copy2;
      };
    };
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
  var eqInt = {
    eq: eqIntImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var notEq = function(dictEq) {
    var eq32 = eq(dictEq);
    return function(x10) {
      return function(y8) {
        return eq2(eq32(x10)(y8))(false);
      };
    };
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Semiring/foreign.js
  var numAdd = function(n1) {
    return function(n2) {
      return n1 + n2;
    };
  };
  var numMul = function(n1) {
    return function(n2) {
      return n1 * n2;
    };
  };

  // output/Data.Semiring/index.js
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringNumber = {
    add: numAdd,
    zero: 0,
    mul: numMul,
    one: 1
  };
  var add = function(dict) {
    return dict.add;
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
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
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
    return '"' + s.replace(/[\0-\x1F\x7F"\\]/g, function(c, i2) {
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
    }) + '"';
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

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };
  var semigroupFn = function(dictSemigroup) {
    var append111 = append(dictSemigroup);
    return {
      append: function(f) {
        return function(g) {
          return function(x10) {
            return append111(f(x10))(g(x10));
          };
        };
      }
    };
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var monoidArray = {
    mempty: [],
    Semigroup0: function() {
      return semigroupArray;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };
  var monoidFn = function(dictMonoid) {
    var mempty1 = mempty(dictMonoid);
    var semigroupFn2 = semigroupFn(dictMonoid.Semigroup0());
    return {
      mempty: function(v) {
        return mempty1;
      },
      Semigroup0: function() {
        return semigroupFn2;
      }
    };
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
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
  }();
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
    var eq8 = eq(dictEq);
    return function(dictEq1) {
      var eq14 = eq(dictEq1);
      return {
        eq: function(x10) {
          return function(y8) {
            return eq8(x10.value0)(y8.value0) && eq14(x10.value1)(y8.value1);
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
        compare: function(x10) {
          return function(y8) {
            var v = compare2(x10.value0)(y8.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x10.value1)(y8.value1);
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
  var modify = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        var s$prime = f(s);
        return new Tuple(s$prime, s$prime);
      });
    };
  };
  var gets = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(f(s), s);
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
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe$prime = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v(unit);
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 250, column 1 - line 250, column 62): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
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
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();
  var altMaybe = {
    alt: function(v) {
      return function(v1) {
        if (v instanceof Nothing) {
          return v1;
        }
        ;
        return v;
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
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
    function Aff2(tag2, _1, _2, _3) {
      this.tag = tag2;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag2) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag2, _1, _2, _3);
      };
      fn.tag = tag2;
      return fn;
    }
    function nonCanceler2(error6) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error6) {
        setTimeout(function() {
          throw error6;
        }, 0);
      }
    }
    function runSync(left2, right2, eff) {
      try {
        return right2(eff());
      } catch (error6) {
        return left2(error6);
      }
    }
    function runAsync(left2, eff, k) {
      try {
        return eff(k)();
      } catch (error6) {
        k(left2(error6))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size5 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size5 !== 0) {
          size5--;
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
          if (size5 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size5) % limit] = cb;
          size5++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
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
            return function(error6) {
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
      var step5 = aff;
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
                step5 = bhead(step5);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail3 = util2.left(e);
                step5 = null;
              }
              break;
            case STEP_RESULT:
              if (util2.isLeft(step5)) {
                status = RETURN;
                fail3 = step5;
                step5 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step5 = util2.fromRight(step5);
              }
              break;
            case CONTINUE:
              switch (step5.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step5._2;
                  status = CONTINUE;
                  step5 = step5._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step5 = util2.right(step5._1);
                  } else {
                    status = STEP_BIND;
                    step5 = step5._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step5 = runSync(util2.left, util2.right, step5._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step5 = runAsync(util2.left, step5._1, function(result2) {
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
                        step5 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail3 = util2.left(step5._1);
                  step5 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step5, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step5, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step5 = step5._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step5, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step5, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step5 = step5._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util2, supervisor, step5._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step5._1) {
                    tmp.run();
                  }
                  step5 = util2.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step5 = sequential3(util2, supervisor, step5._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step5 = interrupt || fail3 || step5;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail3) {
                      status = CONTINUE;
                      step5 = attempt._2(util2.fromLeft(fail3));
                      fail3 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step5 = util2.fromRight(step5);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail3 === null) {
                      result = util2.fromRight(step5);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step5 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step5, fail3), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step5 = attempt._1.killed(util2.fromLeft(interrupt))(attempt._2);
                    } else if (fail3) {
                      step5 = attempt._1.failed(util2.fromLeft(fail3))(attempt._2);
                    } else {
                      step5 = attempt._1.completed(util2.fromRight(step5))(attempt._2);
                    }
                    fail3 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step5, fail3), attempts, interrupt);
                    status = CONTINUE;
                    step5 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step5 = attempt._1;
                    fail3 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step5));
                }
              }
              joins = null;
              if (interrupt && fail3) {
                setTimeout(function() {
                  throw util2.fromLeft(fail3);
                }, 0);
              } else if (util2.isLeft(step5) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util2.fromLeft(step5);
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
            join4.handler(step5)();
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
      function kill2(error6, cb) {
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
              interrupt = util2.left(error6);
              status = COMPLETED;
              step5 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util2.left(error6);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step5(error6)), attempts, interrupt);
                }
                status = RETURN;
                step5 = null;
                fail3 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util2.left(error6);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step5 = null;
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
      var root = EMPTY;
      function kill2(error6, par2, cb2) {
        var step5 = par2;
        var head7 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step5.tag) {
              case FORKED:
                if (step5._3 === EMPTY) {
                  tmp = fibers[step5._1];
                  kills2[count++] = tmp.kill(error6, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head7 === null) {
                  break loop;
                }
                step5 = head7._2;
                if (tail2 === null) {
                  head7 = null;
                } else {
                  head7 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step5 = step5._2;
                break;
              case APPLY:
              case ALT:
                if (head7) {
                  tail2 = new Aff2(CONS, head7, tail2);
                }
                head7 = step5;
                step5 = step5._1;
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
      function join3(result, head7, tail2) {
        var fail3, step5, lhs, rhs, tmp, kid;
        if (util2.isLeft(result)) {
          fail3 = result;
          step5 = null;
        } else {
          step5 = result;
          fail3 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head7 === null) {
              cb(fail3 || step5)();
              return;
            }
            if (head7._3 !== EMPTY) {
              return;
            }
            switch (head7.tag) {
              case MAP:
                if (fail3 === null) {
                  head7._3 = util2.right(head7._1(util2.fromRight(step5)));
                  step5 = head7._3;
                } else {
                  head7._3 = fail3;
                }
                break;
              case APPLY:
                lhs = head7._1._3;
                rhs = head7._2._3;
                if (fail3) {
                  head7._3 = fail3;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, fail3 === lhs ? head7._2 : head7._1, function() {
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
                  step5 = util2.right(util2.fromRight(lhs)(util2.fromRight(rhs)));
                  head7._3 = step5;
                }
                break;
              case ALT:
                lhs = head7._1._3;
                rhs = head7._2._3;
                if (lhs === EMPTY && util2.isLeft(rhs) || rhs === EMPTY && util2.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util2.isLeft(lhs) && rhs !== EMPTY && util2.isLeft(rhs)) {
                  fail3 = step5 === lhs ? rhs : lhs;
                  step5 = null;
                  head7._3 = fail3;
                } else {
                  head7._3 = step5;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, step5 === lhs ? head7._2 : head7._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(step5, null, null);
                      } else {
                        join3(step5, tail2._1, tail2._2);
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
              head7 = null;
            } else {
              head7 = tail2._1;
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
        var step5 = par;
        var head7 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step5.tag) {
                  case MAP:
                    if (head7) {
                      tail2 = new Aff2(CONS, head7, tail2);
                    }
                    head7 = new Aff2(MAP, step5._1, EMPTY, EMPTY);
                    step5 = step5._2;
                    break;
                  case APPLY:
                    if (head7) {
                      tail2 = new Aff2(CONS, head7, tail2);
                    }
                    head7 = new Aff2(APPLY, EMPTY, step5._2, EMPTY);
                    step5 = step5._1;
                    break;
                  case ALT:
                    if (head7) {
                      tail2 = new Aff2(CONS, head7, tail2);
                    }
                    head7 = new Aff2(ALT, EMPTY, step5._2, EMPTY);
                    step5 = step5._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step5;
                    step5 = new Aff2(FORKED, fid, new Aff2(CONS, head7, tail2), EMPTY);
                    tmp = Fiber(util2, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step5)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head7 === null) {
                  break loop;
                }
                if (head7._1 === EMPTY) {
                  head7._1 = step5;
                  status = CONTINUE;
                  step5 = head7._2;
                  head7._2 = EMPTY;
                } else {
                  head7._2 = step5;
                  step5 = head7;
                  if (tail2 === null) {
                    head7 = null;
                  } else {
                    head7 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step5;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error6, cb2) {
        interrupt = util2.left(error6);
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
        var newKills = kill2(error6, root, cb2);
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
  }();
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
        return Aff.Bind(aff, function(value15) {
          return Aff.Pure(f(value15));
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
  var _delay = function() {
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
          var timer = setDelay(ms, cb(right2()));
          return function() {
            return Aff.Sync(function() {
              return right2(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind18 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind18(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind18 = bind(dictMonad.Bind1());
    var pure21 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind18(f)(function(f$prime) {
          return bind18(a2)(function(a$prime) {
            return pure21(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
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
  var map3 = /* @__PURE__ */ map(functorEither);
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
  var hush = /* @__PURE__ */ function() {
    return either($$const(Nothing.value))(Just.create);
  }();
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };

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

  // output/Effect/index.js
  var $runtime_lazy = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
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
    var map55 = map(Monad0.Bind1().Apply0().Functor0());
    var pure21 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map55(Right.create)(a2))(function($52) {
        return pure21(Left.create($52));
      });
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x10) {
    return x10;
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
  var modify2 = function(f) {
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
      return $$void2(modify2(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
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
            while (!function __do4() {
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
            }()) {
            }
            ;
            return {};
          })();
          return map4(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };
  var forever = function(dictMonadRec) {
    var tailRecM1 = tailRecM(dictMonadRec);
    var voidRight2 = voidRight(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    return function(ma) {
      return tailRecM1(function(u2) {
        return voidRight2(new Loop(u2))(ma);
      })(unit);
    };
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
  var map5 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x10) {
    return x10;
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
    var map115 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map115(map5(f)));
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
    var bind18 = bind(dictMonad.Bind1());
    var pure21 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind18(v)(either(function($187) {
            return pure21(Left.create($187));
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
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append23 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind18 = bind(Bind1);
      var pure21 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind18(v)(function(rm) {
              if (rm instanceof Right) {
                return pure21(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind18(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure21(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure21(new Left(append23(rm.value0)(rn.value0)));
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
  var unsafeCoerce2 = function(x10) {
    return x10;
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
  var under = function() {
    return function() {
      return function(v) {
        return coerce2;
      };
    };
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

  // output/Data.Profunctor/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var profunctorFn = {
    dimap: function(a2b) {
      return function(c2d) {
        return function(b2c) {
          return function($18) {
            return c2d(b2c(a2b($18)));
          };
        };
      };
    }
  };
  var dimap = function(dict) {
    return dict.dimap;
  };
  var rmap = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(b2c) {
      return dimap1(identity4)(b2c);
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
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    var bimap1 = bimap(dictBifunctor);
    return function(f) {
      return bimap1(f)(identity5);
    };
  };
  var rmap2 = function(dictBifunctor) {
    return bimap(dictBifunctor)(identity5);
  };
  var bifunctorTuple = {
    bimap: function(f) {
      return function(g) {
        return function(v) {
          return new Tuple(f(v.value0), g(v.value1));
        };
      };
    }
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

  // output/Data.Maybe.First/index.js
  var First = function(x10) {
    return x10;
  };
  var semigroupFirst = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v;
        }
        ;
        return v1;
      };
    }
  };
  var monoidFirst = /* @__PURE__ */ function() {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupFirst;
      }
    };
  }();

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x10) {
    return x10;
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
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond3 = applySecond(dictApplicative.Apply0());
    var pure21 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($449) {
          return applySecond3(f($449));
        })(pure21(unit));
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
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(sep) {
        return function(xs) {
          var go2 = function(v) {
            return function(x10) {
              if (v.init) {
                return {
                  init: false,
                  acc: x10
                };
              }
              ;
              return {
                init: false,
                acc: append23(v.acc)(append23(sep)(x10))
              };
            };
          };
          return foldl22(go2)({
            init: true,
            acc: mempty2
          })(xs).acc;
        };
      };
    };
  };
  var sum = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictSemiring) {
      return foldl22(add(dictSemiring))(zero(dictSemiring));
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0)(z);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(z)(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty2;
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
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x10) {
          return function(acc) {
            return append23(f(x10))(acc);
          };
        })(mempty2);
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
    var foldMap22 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap22(monoidDisj(dictHeytingAlgebra)));
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
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
    return function(apply4) {
      return function(map55) {
        return function(pure21) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure21([]);
                  case 1:
                    return map55(array1)(f(array[bot]));
                  case 2:
                    return apply4(map55(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply4(apply4(map55(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply4(map55(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity6);
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
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var traverse_7 = traverse_(dictParallel.Applicative1());
    var parallel3 = parallel(dictParallel);
    return function(dictFoldable) {
      var traverse_14 = traverse_7(dictFoldable);
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
      return parTraverse_1(dictFoldable)(identity7);
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
  var $runtime_lazy2 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map6 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x10) {
    return x10;
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
  var ffiUtil = /* @__PURE__ */ function() {
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
  }();
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
      return map6(effectCanceler)(v.join(k));
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
          return map6(effectCanceler)(v.kill(e, k));
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
      pure: function() {
        var $79 = parallel(parallelAff);
        return function($80) {
          return $79(pure22($80));
        };
      }(),
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
      var bind18 = bind(dictMonad.Bind1());
      var pure21 = pure(dictMonad.Applicative0());
      return function(m) {
        return function(s) {
          return bind18(m)(function(x10) {
            return pure21(new Tuple(x10, s));
          });
        };
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransStateT);
  var functorStateT = function(dictFunctor) {
    var map55 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map55(function(v1) {
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
    var bind18 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind18(v(s))(function(v1) {
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
    var pure21 = pure(dictMonad.Applicative0());
    return {
      pure: function(a2) {
        return function(s) {
          return pure21(new Tuple(a2, s));
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
      liftEffect: function() {
        var $197 = lift3(Monad0);
        var $198 = liftEffect(dictMonadEffect);
        return function($199) {
          return $197($198($199));
        };
      }(),
      Monad0: function() {
        return monadStateT1;
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure21 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure21(f($200));
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
  var getEffProp = function(name17) {
    return function(node) {
      return function() {
        return node[name17];
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
  function notNull(x10) {
    return x10;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map7 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map7(toMaybe);
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
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.Common/index.js
  var ClassName = function(x10) {
    return x10;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return function() {
      return doc.readyState;
    };
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
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
  var map8 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = /* @__PURE__ */ function() {
    var $2 = map8(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }());
    return function($3) {
      return $2(_readyState($3));
    };
  }();

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value15) {
    var tag2 = Object.prototype.toString.call(value15);
    if (tag2.indexOf("[object HTML") === 0 && tag2.indexOf("Element]") === tag2.length - 8) {
      return just(value15);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var fromElement = function(x10) {
    return _read(Nothing.value, Just.create, x10);
  };

  // output/Effect.Uncurried/foreign.js
  var mkEffectFn3 = function mkEffectFn32(fn) {
    return function(a2, b2, c) {
      return fn(a2)(b2)(c)();
    };
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value15 = b2;
              while (true) {
                var maybe2 = f(value15);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust6(maybe2);
                result.push(fst2(tuple));
                value15 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value15 = b2;
              while (true) {
                var tuple = f(value15);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value15 = fromJust6(maybe2);
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

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
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
  var input = "input";
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";

  // output/Halogen.Aff.Util/index.js
  var bind2 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure3 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map9 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2(function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document2))(windowImpl)))(function(mel) {
      return pure3(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do3() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document2)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map9(toEventTarget)(windowImpl)();
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
  var CoyonedaF = /* @__PURE__ */ function() {
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
  }();
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

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(i2)(xs[i2]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.FoldableWithIndex/index.js
  var foldr8 = /* @__PURE__ */ foldr(foldableArray);
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var foldl8 = /* @__PURE__ */ foldl(foldableArray);
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldlWithIndex = function(dict) {
    return dict.foldlWithIndex;
  };
  var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
    var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
    return function(dictMonoid) {
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldrWithIndex1(function(i2) {
          return function(x10) {
            return function(acc) {
              return append23(f(i2)(x10))(acc);
            };
          };
        })(mempty2);
      };
    };
  };
  var foldableWithIndexArray = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $289 = foldr8(function(v) {
          return function(y8) {
            return f(v.value0)(v.value1)(y8);
          };
        })(z);
        var $290 = mapWithIndex2(Tuple.create);
        return function($291) {
          return $289($290($291));
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $292 = foldl8(function(y8) {
          return function(v) {
            return f(v.value0)(y8)(v.value1);
          };
        })(z);
        var $293 = mapWithIndex2(Tuple.create);
        return function($294) {
          return $292($293($294));
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
    },
    Foldable0: function() {
      return foldableArray;
    }
  };
  var foldMapWithIndex = function(dict) {
    return dict.foldMapWithIndex;
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
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
  }();
  var singleton2 = function(dictPlus) {
    var empty8 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty8);
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
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
  }();
  var NonEmptyList = function(x10) {
    return x10;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_chunksAcc) {
      return function($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v1) {
            if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }
            ;
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v1, acc) {
                if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }
                ;
                $tco_done1 = true;
                return acc;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
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
        var rev3 = function() {
          var go2 = function($copy_acc) {
            return function($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_acc = new Cons(v.value0, acc);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $281 = foldl(foldableList)(flip(f))(b2);
        return function($282) {
          return $281(rev3($282));
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
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $283 = append23(acc);
          return function($284) {
            return $283(f($284));
          };
        })(mempty2);
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
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Data.List/index.js
  var reverse = /* @__PURE__ */ function() {
    var go2 = function($copy_acc) {
      return function($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(acc, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return acc;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_acc = new Cons(v.value0, acc);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var fromFoldable = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
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
  }();
  var Three = /* @__PURE__ */ function() {
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
  }();
  var TwoLeft = /* @__PURE__ */ function() {
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
  }();
  var TwoRight = /* @__PURE__ */ function() {
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
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
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
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
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
  }();
  var ThreeRight = /* @__PURE__ */ function() {
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
  }();
  var KickUp = /* @__PURE__ */ function() {
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
  }();
  var singleton3 = function(k) {
    return function(v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };
  var toUnfoldable = function(dictUnfoldable) {
    var unfoldr2 = unfoldr(dictUnfoldable);
    return function(m) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof Leaf) {
              $copy_v = v.value1;
              return;
            }
            ;
            if (v.value0 instanceof Two && (v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf)) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), v.value1));
            }
            ;
            if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), new Cons(v.value0.value3, v.value1)));
            }
            ;
            if (v.value0 instanceof Two) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton3(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, v.value1)));
              return;
            }
            ;
            if (v.value0 instanceof Three) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton3(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, new Cons(singleton3(v.value0.value4)(v.value0.value5), new Cons(v.value0.value6, v.value1)))));
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 624, column 18 - line 633, column 71): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 623, column 3 - line 623, column 19): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return unfoldr2(go2)(new Cons(m, Nil.value));
    };
  };
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
  var functorMap = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v1 instanceof Two) {
          return new Two(map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map(functorMap)(v)(v1.value3));
        }
        ;
        if (v1 instanceof Three) {
          return new Three(map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), map(functorMap)(v)(v1.value6));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 116, column 1 - line 119, column 110): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, tree2) {
          if (v instanceof Nil) {
            $tco_done = true;
            return tree2;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree2, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree2);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree2, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree2);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, tree2.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
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
        var down = function($copy_ctx) {
          return function($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done1 = true;
                return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v1 instanceof Two) {
                var v2 = compare2(k)(v1.value1);
                if (v2 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Two(v1.value0, k, v, v1.value3));
                }
                ;
                if (v2 instanceof LT) {
                  $tco_var_ctx = new Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }
              ;
              if (v1 instanceof Three) {
                var v3 = compare2(k)(v1.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }
                ;
                var v4 = compare2(k)(v1.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
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
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
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
      var mempty2 = mempty(dictMonoid);
      var append23 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty2;
          }
          ;
          if (m instanceof Two) {
            return append23(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append23(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append23(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append23(f(m.value2))(append23(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append23(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var foldableWithIndexMap = {
    foldrWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(f(m.value4)(m.value5)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value4)(foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append23 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty2;
          }
          ;
          if (m instanceof Two) {
            return append23(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append23(f(m.value1)(m.value2))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append23(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append23(f(m.value1)(m.value2))(append23(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3))(append23(f(m.value4)(m.value5))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): " + [m.constructor.name]);
        };
      };
    },
    Foldable0: function() {
      return foldableMap;
    }
  };
  var foldrWithIndex2 = /* @__PURE__ */ foldrWithIndex(foldableWithIndexMap);
  var foldlWithIndex2 = /* @__PURE__ */ foldlWithIndex(foldableWithIndexMap);
  var keys = /* @__PURE__ */ function() {
    return foldrWithIndex2(function(k) {
      return function(v) {
        return function(acc) {
          return new Cons(k, acc);
        };
      };
    })(Nil.value);
  }();
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable2 = function(dictOrd) {
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
    var lookup14 = lookup(dictOrd);
    var delete1 = $$delete(dictOrd);
    var insert13 = insert(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup14(k)(m));
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
  var fromFoldableWith = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(dictFoldable) {
      var foldl12 = foldl(dictFoldable);
      return function(f) {
        var combine = function(v) {
          return function(v1) {
            if (v1 instanceof Just) {
              return new Just(f(v)(v1.value0));
            }
            ;
            if (v1 instanceof Nothing) {
              return new Just(v);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 613, column 3 - line 613, column 38): " + [v.constructor.name, v1.constructor.name]);
          };
        };
        return foldl12(function(m) {
          return function(v) {
            return alter1(combine(v.value1))(v.value0)(m);
          };
        })(empty2);
      };
    };
  };
  var unionWith = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(f) {
      return function(m1) {
        return function(m2) {
          var go2 = function(k) {
            return function(m) {
              return function(v) {
                return alter1(function() {
                  var $932 = maybe(v)(f(v));
                  return function($933) {
                    return Just.create($932($933));
                  };
                }())(k)(m);
              };
            };
          };
          return foldlWithIndex2(go2)(m2)(m1);
        };
      };
    };
  };
  var union = function(dictOrd) {
    return unionWith(dictOrd)($$const);
  };
  var update = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          return alter1(maybe(Nothing.value)(f))(k)(m);
        };
      };
    };
  };

  // output/Halogen.Data.OrdBox/index.js
  var OrdBox = /* @__PURE__ */ function() {
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
  }();
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
    var traverse_7 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_7(function($54) {
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

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton = /* @__PURE__ */ function() {
    function InputButton2() {
    }
    ;
    InputButton2.value = new InputButton2();
    return InputButton2;
  }();
  var InputCheckbox = /* @__PURE__ */ function() {
    function InputCheckbox2() {
    }
    ;
    InputCheckbox2.value = new InputCheckbox2();
    return InputCheckbox2;
  }();
  var InputColor = /* @__PURE__ */ function() {
    function InputColor2() {
    }
    ;
    InputColor2.value = new InputColor2();
    return InputColor2;
  }();
  var InputDate = /* @__PURE__ */ function() {
    function InputDate2() {
    }
    ;
    InputDate2.value = new InputDate2();
    return InputDate2;
  }();
  var InputDatetimeLocal = /* @__PURE__ */ function() {
    function InputDatetimeLocal2() {
    }
    ;
    InputDatetimeLocal2.value = new InputDatetimeLocal2();
    return InputDatetimeLocal2;
  }();
  var InputEmail = /* @__PURE__ */ function() {
    function InputEmail2() {
    }
    ;
    InputEmail2.value = new InputEmail2();
    return InputEmail2;
  }();
  var InputFile = /* @__PURE__ */ function() {
    function InputFile2() {
    }
    ;
    InputFile2.value = new InputFile2();
    return InputFile2;
  }();
  var InputHidden = /* @__PURE__ */ function() {
    function InputHidden2() {
    }
    ;
    InputHidden2.value = new InputHidden2();
    return InputHidden2;
  }();
  var InputImage = /* @__PURE__ */ function() {
    function InputImage2() {
    }
    ;
    InputImage2.value = new InputImage2();
    return InputImage2;
  }();
  var InputMonth = /* @__PURE__ */ function() {
    function InputMonth2() {
    }
    ;
    InputMonth2.value = new InputMonth2();
    return InputMonth2;
  }();
  var InputNumber = /* @__PURE__ */ function() {
    function InputNumber2() {
    }
    ;
    InputNumber2.value = new InputNumber2();
    return InputNumber2;
  }();
  var InputPassword = /* @__PURE__ */ function() {
    function InputPassword2() {
    }
    ;
    InputPassword2.value = new InputPassword2();
    return InputPassword2;
  }();
  var InputRadio = /* @__PURE__ */ function() {
    function InputRadio2() {
    }
    ;
    InputRadio2.value = new InputRadio2();
    return InputRadio2;
  }();
  var InputRange = /* @__PURE__ */ function() {
    function InputRange2() {
    }
    ;
    InputRange2.value = new InputRange2();
    return InputRange2;
  }();
  var InputReset = /* @__PURE__ */ function() {
    function InputReset2() {
    }
    ;
    InputReset2.value = new InputReset2();
    return InputReset2;
  }();
  var InputSearch = /* @__PURE__ */ function() {
    function InputSearch2() {
    }
    ;
    InputSearch2.value = new InputSearch2();
    return InputSearch2;
  }();
  var InputSubmit = /* @__PURE__ */ function() {
    function InputSubmit2() {
    }
    ;
    InputSubmit2.value = new InputSubmit2();
    return InputSubmit2;
  }();
  var InputTel = /* @__PURE__ */ function() {
    function InputTel2() {
    }
    ;
    InputTel2.value = new InputTel2();
    return InputTel2;
  }();
  var InputText = /* @__PURE__ */ function() {
    function InputText2() {
    }
    ;
    InputText2.value = new InputText2();
    return InputText2;
  }();
  var InputTime = /* @__PURE__ */ function() {
    function InputTime2() {
    }
    ;
    InputTime2.value = new InputTime2();
    return InputTime2;
  }();
  var InputUrl = /* @__PURE__ */ function() {
    function InputUrl2() {
    }
    ;
    InputUrl2.value = new InputUrl2();
    return InputUrl2;
  }();
  var InputWeek = /* @__PURE__ */ function() {
    function InputWeek2() {
    }
    ;
    InputWeek2.value = new InputWeek2();
    return InputWeek2;
  }();
  var renderInputType = function(v) {
    if (v instanceof InputButton) {
      return "button";
    }
    ;
    if (v instanceof InputCheckbox) {
      return "checkbox";
    }
    ;
    if (v instanceof InputColor) {
      return "color";
    }
    ;
    if (v instanceof InputDate) {
      return "date";
    }
    ;
    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }
    ;
    if (v instanceof InputEmail) {
      return "email";
    }
    ;
    if (v instanceof InputFile) {
      return "file";
    }
    ;
    if (v instanceof InputHidden) {
      return "hidden";
    }
    ;
    if (v instanceof InputImage) {
      return "image";
    }
    ;
    if (v instanceof InputMonth) {
      return "month";
    }
    ;
    if (v instanceof InputNumber) {
      return "number";
    }
    ;
    if (v instanceof InputPassword) {
      return "password";
    }
    ;
    if (v instanceof InputRadio) {
      return "radio";
    }
    ;
    if (v instanceof InputRange) {
      return "range";
    }
    ;
    if (v instanceof InputReset) {
      return "reset";
    }
    ;
    if (v instanceof InputSearch) {
      return "search";
    }
    ;
    if (v instanceof InputSubmit) {
      return "submit";
    }
    ;
    if (v instanceof InputTel) {
      return "tel";
    }
    ;
    if (v instanceof InputText) {
      return "text";
    }
    ;
    if (v instanceof InputTime) {
      return "time";
    }
    ;
    if (v instanceof InputUrl) {
      return "url";
    }
    ;
    if (v instanceof InputWeek) {
      return "week";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 33, column 19 - line 55, column 22): " + [v.constructor.name]);
  };

  // output/DOM.HTML.Indexed.StepValue/index.js
  var show2 = /* @__PURE__ */ show(showNumber);
  var Any = /* @__PURE__ */ function() {
    function Any2() {
    }
    ;
    Any2.value = new Any2();
    return Any2;
  }();
  var Step = /* @__PURE__ */ function() {
    function Step4(value0) {
      this.value0 = value0;
    }
    ;
    Step4.create = function(value0) {
      return new Step4(value0);
    };
    return Step4;
  }();
  var renderStepValue = function(v) {
    if (v instanceof Any) {
      return "any";
    }
    ;
    if (v instanceof Step) {
      return show2(v.value0);
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.StepValue (line 13, column 19 - line 15, column 19): " + [v.constructor.name]);
  };

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
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
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Data.Array/foreign.js
  var range2 = function(start5) {
    return function(end) {
      var step5 = start5 > end ? -1 : 1;
      var result = new Array(step5 * (end - start5) + 1);
      var i2 = start5, n = 0;
      while (i2 !== end) {
        result[n++] = i2;
        i2 += step5;
      }
      result[n] = i2;
      return result;
    };
  };
  var replicateFill = function(count) {
    return function(value15) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value15);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value15) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value15;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head7, tail2) {
      this.head = head7;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head7) {
      return function(tail2) {
        return new Cons3(head7, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr5) {
      return function(xs) {
        return listToArray(foldr5(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length4 = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty8) {
    return function(next2) {
      return function(xs) {
        return xs.length === 0 ? empty8({}) : next2(xs[0])(xs.slice(1));
      };
    };
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
            if (f(xs[i2]))
              return just(i2);
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
          if (i2 < 0 || i2 >= l.length)
            return nothing;
          var l1 = l.slice();
          l1.splice(i2, 1);
          return just(l1);
        };
      };
    };
  };
  var reverse2 = function(l) {
    return l.slice().reverse();
  };
  var filter2 = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var partition = function(f) {
    return function(xs) {
      var yes = [];
      var no = [];
      for (var i2 = 0; i2 < xs.length; i2++) {
        var x10 = xs[i2];
        if (f(x10))
          yes.push(x10);
        else
          no.push(x10);
      }
      return { yes, no };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to2) {
      var mid;
      var i2;
      var j;
      var k;
      var x10;
      var y8;
      var c;
      mid = from2 + (to2 - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to2);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to2) {
        x10 = xs2[i2];
        y8 = xs2[j];
        c = fromOrdering(compare2(x10)(y8));
        if (c > 0) {
          xs1[k++] = y8;
          ++j;
        } else {
          xs1[k++] = x10;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to2) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var zipWith = function(f) {
    return function(xs) {
      return function(ys) {
        var l = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l);
        for (var i2 = 0; i2 < l; i2++) {
          result[i2] = f(xs[i2])(ys[i2]);
        }
        return result;
      };
    };
  };
  var any2 = function(p2) {
    return function(xs) {
      var len = xs.length;
      for (var i2 = 0; i2 < len; i2++) {
        if (p2(xs[i2]))
          return true;
      }
      return false;
    };
  };
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a2) {
      return function() {
        return f(a2());
      };
    };
  };
  var pure_ = function(a2) {
    return function() {
      return a2;
    };
  };
  var bind_ = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i2 = 0, l = as.length; i2 < l; i2++) {
          f(as[i2])();
        }
      };
    };
  };
  function newSTRef(val) {
    return function() {
      return { value: val };
    };
  }
  var read2 = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl2 = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write2 = function(a2) {
    return function(ref2) {
      return function() {
        return ref2.value = a2;
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy3 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var modify$prime2 = modifyImpl2;
  var modify3 = function(f) {
    return modify$prime2(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy3("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

  // output/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
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
  var sortByImpl2 = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to2) {
      var mid;
      var i2;
      var j;
      var k;
      var x10;
      var y8;
      var c;
      mid = from2 + (to2 - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to2);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to2) {
        x10 = xs2[i2];
        y8 = xs2[j];
        c = fromOrdering(compare2(x10)(y8));
        if (c > 0) {
          xs1[k++] = y8;
          ++j;
        } else {
          xs1[k++] = x10;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to2) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

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

  // output/Data.Array.ST.Iterator/index.js
  var map10 = /* @__PURE__ */ map(functorST);
  var not2 = /* @__PURE__ */ not(heytingAlgebraBoolean);
  var $$void4 = /* @__PURE__ */ $$void(functorST);
  var Iterator = /* @__PURE__ */ function() {
    function Iterator2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Iterator2.create = function(value0) {
      return function(value1) {
        return new Iterator2(value0, value1);
      };
    };
    return Iterator2;
  }();
  var peek = function(v) {
    return function __do3() {
      var i2 = read2(v.value1)();
      return v.value0(i2);
    };
  };
  var next = function(v) {
    return function __do3() {
      var i2 = read2(v.value1)();
      modify3(function(v1) {
        return v1 + 1 | 0;
      })(v.value1)();
      return v.value0(i2);
    };
  };
  var pushWhile = function(p2) {
    return function(iter) {
      return function(array) {
        return function __do3() {
          var $$break = newSTRef(false)();
          while (map10(not2)(read2($$break))()) {
            (function __do4() {
              var mx = peek(iter)();
              if (mx instanceof Just && p2(mx.value0)) {
                push(mx.value0)(array)();
                return $$void4(next(iter))();
              }
              ;
              return $$void4(write2(true)($$break))();
            })();
          }
          ;
          return {};
        };
      };
    };
  };
  var iterator = function(f) {
    return map10(Iterator.create(f))(newSTRef(0));
  };
  var iterate = function(iter) {
    return function(f) {
      return function __do3() {
        var $$break = newSTRef(false)();
        while (map10(not2)(read2($$break))()) {
          (function __do4() {
            var mx = next(iter)();
            if (mx instanceof Just) {
              return f(mx.value0)();
            }
            ;
            if (mx instanceof Nothing) {
              return $$void4(write2(true)($$break))();
            }
            ;
            throw new Error("Failed pattern match at Data.Array.ST.Iterator (line 42, column 5 - line 44, column 47): " + [mx.constructor.name]);
          })();
        }
        ;
        return {};
      };
    };
  };

  // output/Data.Array/index.js
  var map11 = /* @__PURE__ */ map(functorST);
  var when2 = /* @__PURE__ */ when(applicativeST);
  var $$void5 = /* @__PURE__ */ $$void(functorST);
  var intercalate1 = /* @__PURE__ */ intercalate2(foldableArray);
  var map12 = /* @__PURE__ */ map(functorMaybe);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var uncons = /* @__PURE__ */ function() {
    return unconsImpl($$const(Nothing.value))(function(x10) {
      return function(xs) {
        return new Just({
          head: x10,
          tail: xs
        });
      };
    });
  }();
  var sortBy = function(comp) {
    return sortByImpl(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 829, column 31 - line 832, column 11): " + [v.constructor.name]);
    });
  };
  var snoc = function(xs) {
    return function(x10) {
      return withArray(push(x10))(xs)();
    };
  };
  var singleton4 = function(a2) {
    return [a2];
  };
  var $$null3 = function(xs) {
    return length4(xs) === 0;
  };
  var nubByEq = function(eq23) {
    return function(xs) {
      return function __do3() {
        var arr = newSTArray();
        foreach(xs)(function(x10) {
          return function __do4() {
            var e = map11(function() {
              var $177 = any2(function(v) {
                return eq23(v)(x10);
              });
              return function($178) {
                return !$177($178);
              };
            }())(unsafeFreeze(arr))();
            return when2(e)($$void5(push(x10)(arr)))();
          };
        })();
        return unsafeFreeze(arr)();
      }();
    };
  };
  var mapWithIndex3 = function(f) {
    return function(xs) {
      return zipWith(f)(range2(0)(length4(xs) - 1 | 0))(xs);
    };
  };
  var intercalate3 = function(dictMonoid) {
    return intercalate1(dictMonoid);
  };
  var index2 = /* @__PURE__ */ function() {
    return indexImpl(Just.create)(Nothing.value);
  }();
  var head2 = function(xs) {
    return index2(xs)(0);
  };
  var groupBy = function(op) {
    return function(xs) {
      return function __do3() {
        var result = newSTArray();
        var iter = iterator(function(v) {
          return index2(xs)(v);
        })();
        iterate(iter)(function(x10) {
          return $$void5(function __do4() {
            var sub1 = newSTArray();
            push(x10)(sub1)();
            pushWhile(op(x10))(iter)(sub1)();
            var grp = unsafeFreeze(sub1)();
            return push(grp)(result)();
          });
        })();
        return unsafeFreeze(result)();
      }();
    };
  };
  var fromFoldable3 = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var find2 = function(f) {
    return function(xs) {
      return map12(unsafeIndex1(xs))(findIndex(f)(xs));
    };
  };
  var elemIndex = function(dictEq) {
    var eq23 = eq(dictEq);
    return function(x10) {
      return findIndex(function(v) {
        return eq23(v)(x10);
      });
    };
  };
  var elem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a2) {
      return function(arr) {
        return isJust(elemIndex1(a2)(arr));
      };
    };
  };
  var deleteAt = /* @__PURE__ */ function() {
    return _deleteAt(Just.create)(Nothing.value);
  }();
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
  var cons3 = function(x10) {
    return function(xs) {
      return append2([x10])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $185 = maybe([])(singleton4);
      return function($186) {
        return $185(f($186));
      };
    }());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));

  // output/Halogen.VDom.Machine/index.js
  var Step2 = /* @__PURE__ */ function() {
    function Step4(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step4.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step4(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step4;
  }();
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
  var map13 = /* @__PURE__ */ map(functorArray);
  var map14 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ function() {
    function Text3(value0) {
      this.value0 = value0;
    }
    ;
    Text3.create = function(value0) {
      return new Text3(value0);
    };
    return Text3;
  }();
  var Elem = /* @__PURE__ */ function() {
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
  }();
  var Keyed = /* @__PURE__ */ function() {
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
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
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
  }();
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
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map13(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map13(map14(go2))(v2.value3));
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
  function createElement(ns, name17, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name17);
    } else {
      return doc.createElement(name17);
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
  var getProp = function(name17) {
    return function(doctype) {
      return doctype[name17];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy4 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy4("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step3(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step2(v.value0, {
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
    var parent3 = parentNode(v.node);
    return removeChild(v.node, parent3);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy4("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step2(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step2(state3.node, nextState, $lazy_patchText(89), haltText));
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
    var parent3 = parentNode(v.node);
    removeChild(v.node, parent3);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent3 = parentNode(v.node);
    removeChild(v.node, parent3);
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
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy4("patchElem", "Halogen.VDom.DOM", function() {
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
          return mkStep(new Step2(state3.node, nextState, $lazy_patchElem(149), haltElem));
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
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step2(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy4("patchKeyed", "Halogen.VDom.DOM", function() {
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
          return mkStep(new Step2(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
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
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step2(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
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
      return mkStep(new Step2(v1.value0, {
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
    return mkStep(new Step2(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length4(ch1)
    };
    return mkStep(new Step2(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(ix, child) {
      var res = build(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step2(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy4("build", "Halogen.VDom.DOM", function() {
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
  function typeOf(value15) {
    return typeof value15;
  }
  function tagOf(value15) {
    return Object.prototype.toString.call(value15).slice(8, -1);
  }
  var isArray = Array.isArray || function(value15) {
    return Object.prototype.toString.call(value15) === "[object Array]";
  };

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };
  var fromStringAsImpl = function(just) {
    return function(nothing) {
      return function(radix) {
        var digits;
        if (radix < 11) {
          digits = "[0-" + (radix - 1).toString() + "]";
        } else if (radix === 11) {
          digits = "[0-9a]";
        } else {
          digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
        }
        var pattern2 = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
        return function(s) {
          if (pattern2.test(s)) {
            var i2 = parseInt(s, radix);
            return (i2 | 0) === i2 ? just(i2) : nothing;
          } else {
            return nothing;
          }
        };
      };
    };
  };

  // output/Data.Number/foreign.js
  var infinity = Infinity;
  var isFiniteImpl = isFinite;
  var abs = Math.abs;
  var ceil = Math.ceil;
  var cos = Math.cos;
  var floor = Math.floor;
  var remainder = function(n) {
    return function(m) {
      return n % m;
    };
  };
  var sin = Math.sin;
  var sqrt = Math.sqrt;

  // output/Data.Number/index.js
  var pi = 3.141592653589793;

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromStringAs = /* @__PURE__ */ function() {
    return fromStringAsImpl(Just.create)(Nothing.value);
  }();
  var fromString = /* @__PURE__ */ fromStringAs(10);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x10) {
    if (!isFiniteImpl(x10)) {
      return 0;
    }
    ;
    if (x10 >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x10 <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x10));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x10.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ function() {
    var $199 = singleton2(plusList);
    return function($200) {
      return NonEmptyList($199($200));
    };
  }();
  var head3 = function(v) {
    return v.value0;
  };
  var cons4 = function(y8) {
    return function(v) {
      return new NonEmpty(y8, new Cons(v.value0, v.value1));
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

  // output/Foreign/index.js
  var show3 = /* @__PURE__ */ show(showString);
  var show1 = /* @__PURE__ */ show(showInt);
  var ForeignError = /* @__PURE__ */ function() {
    function ForeignError2(value0) {
      this.value0 = value0;
    }
    ;
    ForeignError2.create = function(value0) {
      return new ForeignError2(value0);
    };
    return ForeignError2;
  }();
  var TypeMismatch = /* @__PURE__ */ function() {
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
  }();
  var ErrorAtIndex = /* @__PURE__ */ function() {
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
  }();
  var ErrorAtProperty = /* @__PURE__ */ function() {
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
  }();
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
      return "Error at property " + (show3(v.value0) + (": " + renderForeignError(v.value1)));
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
    var pure111 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag2) {
      return function(value15) {
        if (tagOf(value15) === tag2) {
          return pure111(unsafeFromForeign(value15));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag2, tagOf(value15)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag2.constructor.name, value15.constructor.name]);
      };
    };
  };
  var readBoolean = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("Boolean");
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
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
  var keys2 = Object.keys || toArrayWithKey(function(k) {
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
          return function(d2) {
            return fn(a2, b2, c, d2);
          };
        };
      };
    };
  };

  // output/Foreign.Object/index.js
  var lookup3 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy5 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
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
  }();
  var Property = /* @__PURE__ */ function() {
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
  }();
  var Handler = /* @__PURE__ */ function() {
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
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
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
  var propFromNumber = unsafeCoerce2;
  var propFromBoolean = unsafeCoerce2;
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
            var $65 = v11.value2 === v2.value2;
            if ($65) {
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
              var $74 = refEq2(elVal, v2.value1);
              if ($74) {
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
      var $lazy_patchProp = $runtime_lazy5("patchProp", "Halogen.VDom.DOM.Prop", function() {
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
          return mkStep(new Step2(unit, nextState, $lazy_patchProp(100), haltProp));
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
        return mkStep(new Step2(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x10) {
    return x10;
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
  var isPropStepValue = {
    toPropValue: function($36) {
      return propFromString(renderStepValue($36));
    }
  };
  var isPropNumber = {
    toPropValue: propFromNumber
  };
  var isPropInputType = {
    toPropValue: function($45) {
      return propFromString(renderInputType($45));
    }
  };
  var isPropBoolean = {
    toPropValue: propFromBoolean
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name17) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name17, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var Ap = /* @__PURE__ */ function() {
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
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift.create;
  }();
  var goLeft = function(dictApplicative) {
    var pure21 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure21(func.value0),
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
    var apply4 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply4(fStack.value0.func)(gVal);
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
      return function(x10) {
        return mkAp(new Pure(f))(x10);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure21 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure21(v.value1.value0.value0));
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
    return foldFreeAp(dictApplicative)(identity8);
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
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
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
  }();
  var uncons3 = function($copy_v) {
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
  var $$null4 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty5 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
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
  }();
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
        var foldl6 = function($copy_v) {
          return function($copy_c) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_var_c = $copy_c;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, c, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return c;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_c = v(c)(v1.value0);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, c.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_c, $copy_v1);
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
              var v = uncons3(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl6(function(x10) {
                  return function(i2) {
                    return i2(x10);
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
  var uncons4 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $65 = $$null4(v.value1);
        if ($65) {
          return CatNil.value;
        }
        ;
        return foldr3(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty6 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
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
  var $runtime_lazy6 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append4 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ function() {
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
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
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
  }();
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
        var v2 = uncons4(v.value1);
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
        return bindFlipped(freeBind)(function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        }())(f);
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
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy6("freeApply", "Control.Monad.Free", function() {
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
    var map115 = map(Monad0.Bind1().Apply0().Functor0());
    var pure111 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map115(Done.create)(pure111(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map115(function($199) {
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
  var $$void6 = /* @__PURE__ */ $$void(functorEffect);
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
        return $$void6(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
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
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var SubscriptionId = function(x10) {
    return x10;
  };
  var ForkId = function(x10) {
    return x10;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
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
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
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
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
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
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
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
  }();
  var Join = /* @__PURE__ */ function() {
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
  }();
  var Kill = /* @__PURE__ */ function() {
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
  }();
  var GetRef = /* @__PURE__ */ function() {
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
  }();
  var HalogenM = function(x10) {
    return x10;
  };
  var subscribe2 = function(es) {
    return liftF(new Subscribe(function(v) {
      return es;
    }, identity9));
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
      liftEffect: function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift2.create($186($187))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var monadAffHalogenM = function(dictMonadAff) {
    var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    return {
      liftAff: function() {
        var $188 = liftAff(dictMonadAff);
        return function($189) {
          return HalogenM(liftF(Lift2.create($188($189))));
        };
      }(),
      MonadEffect0: function() {
        return monadEffectHalogenM1;
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize10(value0) {
      this.value0 = value0;
    }
    ;
    Initialize10.create = function(value0) {
      return new Initialize10(value0);
    };
    return Initialize10;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize6(value0) {
      this.value0 = value0;
    }
    ;
    Finalize6.create = function(value0) {
      return new Finalize6(value0);
    };
    return Finalize6;
  }();
  var Receive = /* @__PURE__ */ function() {
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
  }();
  var Action2 = /* @__PURE__ */ function() {
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
  }();
  var Query = /* @__PURE__ */ function() {
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
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy7 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
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
    var $lazy_patchThunk = $runtime_lazy7("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step2(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step3(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step2(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step2(extract2(vdom), {
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
  var map15 = /* @__PURE__ */ map(functorHalogenM);
  var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
  var lookup4 = /* @__PURE__ */ lookup2();
  var pop3 = /* @__PURE__ */ pop2();
  var insert3 = /* @__PURE__ */ insert2();
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
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
          var $45 = map15(maybe(v.value1(unit))(g));
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
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure5(unit)),
      handleQuery: $$const(pure5(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      var lookup14 = lookup4(dictIsSymbol);
      var pop12 = pop3(dictIsSymbol);
      var insert13 = insert3(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup14(dictOrd);
        var pop22 = pop12(dictOrd);
        var insert22 = insert13(dictOrd);
        return function(label6) {
          return function(p2) {
            return function(comp) {
              return function(input3) {
                return function(output2) {
                  return mkComponentSlot({
                    get: lookup23(label6)(p2),
                    pop: pop22(label6)(p2),
                    set: insert22(label6)(p2),
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
  var pure6 = /* @__PURE__ */ pure(applicativeMaybe);
  var elementNS = function($15) {
    return element(pure6($15));
  };
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var fieldset = /* @__PURE__ */ element2("fieldset");
  var h1 = /* @__PURE__ */ element2("h1");
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var h3 = /* @__PURE__ */ element2("h3");
  var h4 = /* @__PURE__ */ element2("h4");
  var img = function(props) {
    return element2("img")(props)([]);
  };
  var input2 = function(props) {
    return element2("input")(props)([]);
  };
  var label4 = /* @__PURE__ */ element2("label");
  var legend = /* @__PURE__ */ element2("legend");
  var li = /* @__PURE__ */ element2("li");
  var nav = /* @__PURE__ */ element2("nav");
  var p = /* @__PURE__ */ element2("p");
  var p_ = /* @__PURE__ */ p([]);
  var pre = /* @__PURE__ */ element2("pre");
  var span3 = /* @__PURE__ */ element2("span");
  var table = /* @__PURE__ */ element2("table");
  var td = /* @__PURE__ */ element2("td");
  var th = /* @__PURE__ */ element2("th");
  var tr = /* @__PURE__ */ element2("tr");
  var tr_ = /* @__PURE__ */ tr([]);
  var ul = /* @__PURE__ */ element2("ul");
  var div2 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div2([]);
  var code = /* @__PURE__ */ element2("code");
  var code_ = /* @__PURE__ */ code([]);
  var button = /* @__PURE__ */ element2("button");
  var body = /* @__PURE__ */ element2("body");
  var body_ = /* @__PURE__ */ body([]);
  var a = /* @__PURE__ */ element2("a");

  // output/Halogen.HTML.Properties/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var prop4 = /* @__PURE__ */ prop2(isPropNumber);
  var src9 = /* @__PURE__ */ prop22("src");
  var step4 = /* @__PURE__ */ prop2(isPropStepValue)("step");
  var type_17 = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value12 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var name15 = /* @__PURE__ */ prop22("name");
  var min5 = /* @__PURE__ */ prop4("min");
  var max6 = /* @__PURE__ */ prop4("max");
  var id2 = /* @__PURE__ */ prop22("id");
  var $$for = /* @__PURE__ */ prop22("htmlFor");
  var classes = /* @__PURE__ */ function() {
    var $32 = prop22("className");
    var $33 = joinWith(" ");
    var $34 = map(functorArray)(unwrap2);
    return function($35) {
      return $32($33($34($35)));
    };
  }();
  var class_ = /* @__PURE__ */ function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap2($37));
    };
  }();
  var checked2 = /* @__PURE__ */ prop1("checked");
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();

  // output/Halogen.HTML/index.js
  var componentSlot2 = /* @__PURE__ */ componentSlot();
  var slot_ = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label6) {
          return function(p2) {
            return function(component8) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot22(label6)(p2)(component8)(input3)($$const(Nothing.value))));
              };
            };
          };
        };
      };
    };
  };
  var fromPlainHTML = unsafeCoerce2;

  // output/Control.Monad.Except/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap3(runExceptT($3));
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value15) {
    return value15 == null ? f : s(value15[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail3 = fail(dictMonad);
    var pure21 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value15) {
        return unsafeReadPropImpl(fail3(new TypeMismatch("object", typeOf(value15))), pure21, k, value15);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }

  // output/Web.Event.Event/index.js
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click2 = "click";

  // output/Halogen.HTML.Events/index.js
  var map17 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map17(Action.create)(f(ev));
      });
    };
  };
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onChange = /* @__PURE__ */ handler2(change);
  var onClick = /* @__PURE__ */ function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop27) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped3(reader)(readProp2(prop27))(unsafeToForeign(a2));
          };
          return handler$prime(key)(composeKleisli2(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($85) {
              return Just.create(f($85));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onChecked = /* @__PURE__ */ addForeignPropHandler(change)("checked")(/* @__PURE__ */ readBoolean(monadIdentity));
  var onValueInput = /* @__PURE__ */ addForeignPropHandler(input)("value")(readString2);

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
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_7 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_7(f)(st.rendering);
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
  var initDriverState = function(component8) {
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
              component: component8,
              state: component8.initialState(input3),
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
  var pure7 = /* @__PURE__ */ pure(applicativeAff);
  var map18 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var map19 = /* @__PURE__ */ map(functorAff);
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
                return pure7(result);
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
  var evalQ = function(render3) {
    return function(ref2) {
      return function(q2) {
        return bind12(liftEffect4(read(ref2)))(function(v) {
          return evalM(render3)(ref2)(v["component"]["eval"](new Query(map18(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render3) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind12(liftEffect4(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel2(bind12(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render3)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map19(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
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
                  return pure7(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect4(write({
                    component: v2.component,
                    state: v3.value1,
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
                    lifecycleHandlers: v2.lifecycleHandlers
                  })(ref2)))(function() {
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render3(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure7(v3.value0);
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
                  return handleAff(evalF(render3)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return discard1(liftEffect4(modify_2(map22(insert4(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure7(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure7(v1.value1);
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
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $118 = evalM(render3)(ref2);
                return function($119) {
                  return parallel2($118($119));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind12(fresh(ForkId)(ref2))(function(fid) {
                return bind12(liftEffect4(read(ref2)))(function(v2) {
                  return bind12(liftEffect4($$new(false)))(function(doneRef) {
                    return bind12(fork3($$finally(liftEffect4(function __do3() {
                      modify_2($$delete2(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render3)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_2(insert12(fid)(fiber))(v2.forks))))(function() {
                        return pure7(v1.value1(fid));
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
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return pure7(v1.value1(lookup22(v1.value0)(v2.refs)));
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
  var evalF = function(render3) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect4(flip(modify_2)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              refs: alter2($$const(v.value1))(v.value0)(st.refs),
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
              lifecycleHandlers: st.lifecycleHandlers
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind12(liftEffect4(read(ref2)))(function(v1) {
            return evalM(render3)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
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
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var map20 = /* @__PURE__ */ map(functorEffect);
  var pure12 = /* @__PURE__ */ pure(applicativeAff);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void7 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref2) {
    return function __do3() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_2(queue)(function() {
        var $58 = traverse_5(fork4);
        return function($59) {
          return handleAff($58(reverse($59)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do3() {
      bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped6(traverse_33(function() {
        var $60 = killFiber(error("finalized"));
        return function($61) {
          return handleAff($60($61));
        };
      }()))(read(v.forks))();
      return write(empty2)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component8) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render3)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
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
                  bindFlipped6(unDriverStateX(function() {
                    var $62 = render3(lchs);
                    return function($63) {
                      return $62(function(v) {
                        return v.selfRef;
                      }($63));
                    };
                  }()))(read($$var2))();
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
                return unComponentSlot(function(slot) {
                  return function __do3() {
                    var childrenIn = map20(slot.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do4() {
                            flip(write)(st.handlerRef)(function() {
                              var $64 = maybe(pure12(unit))(handler3);
                              return function($65) {
                                return $64(slot.output($65));
                              };
                            }())();
                            return handleAff(evalM(render3)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $66 = maybe(pure12(unit))(handler3);
                          return function($67) {
                            return $66(slot.output($67));
                          };
                        }())(slot.input)(slot.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map20(function($68) {
                      return isJust(slot.get($68));
                    })(read(childrenOutRef))();
                    when3(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_2(slot.set($$var2))(childrenOutRef)();
                    return bind4(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure8(renderSpec2.renderChild(v.value0));
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
        var render3 = function(lchs) {
          return function($$var2) {
            return function __do3() {
              var v = read($$var2)();
              var shouldProcessHandlers = map20(isNothing)(read(v.pendingHandlers))();
              when3(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty3)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $69 = queueOrRun(v.pendingHandlers);
                var $70 = evalF(render3)(v.selfRef);
                return function($71) {
                  return $69($$void7($70($71)));
                };
              }();
              var childHandler = function() {
                var $72 = queueOrRun(v.pendingQueries);
                return function($73) {
                  return $72(handler3(Action.create($73)));
                };
              }();
              var rendering = renderSpec2.render(function($74) {
                return handleAff(handler3($74));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
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
                  children: children2,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  rendering: new Just(rendering),
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers
                };
              }))();
              return when3(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do4() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $75 = traverse_5(fork4);
                    return function($76) {
                      return handleAff($75(reverse($76)));
                    };
                  }())(handlers)();
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
              var f = evalM(render3)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
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
                return evalQ(render3)(ref2)(q2);
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
              var dsx = bindFlipped6(read)(runComponent(lchs)(function() {
                var $77 = notify(sio.listener);
                return function($78) {
                  return liftEffect5($77($78));
                };
              }())(i2)(component8))();
              return unDriverStateX(function(st) {
                return pure8({
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
  var getEffProp2 = function(name17) {
    return function(node) {
      return function() {
        return node[name17];
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
      return function(parent3) {
        return function() {
          parent3.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent3) {
      return function() {
        parent3.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent3) {
      return function() {
        parent3.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map21 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map21(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map21(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy8 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void8 = /* @__PURE__ */ $$void(functorEffect);
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var not3 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map23 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void8(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void8(appendChild(v)(v2.value0));
        }
        ;
        return pure9(unit);
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
          var buildThunk2 = buildThunk(unwrap4)(spec);
          var $lazy_patch = $runtime_lazy8("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot) {
              if (st instanceof Just) {
                if (slot instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot.value0);
                }
                ;
                if (slot instanceof ThunkSlot) {
                  var step$prime = step3(st.value0, slot.value0);
                  return mkStep(new Step2(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot);
            };
          });
          var $lazy_render = $runtime_lazy8("render", "Halogen.VDom.Driver", function() {
            return function(slot) {
              if (slot instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step5 = buildThunk2(slot.value0);
                return mkStep(new Step2(extract2(step5), new Just(step5), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy8("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step2(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch2 = $lazy_patch(91);
          var render3 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render3;
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
      var render3 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do3() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document3);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void8(appendChild(node)(toNode(container)))();
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
                  var parent3 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step3(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when4(not3(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent3))();
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
        render: render3,
        renderChild: identity10,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component8) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map23(toDocument)(bindFlipped7(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element3))(component8)(i2);
        });
      };
    };
  };

  // output/Ocelot.HTML.Properties/index.js
  var lmap2 = /* @__PURE__ */ lmap(bifunctorTuple);
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var rmap3 = /* @__PURE__ */ rmap2(bifunctorTuple);
  var elem3 = /* @__PURE__ */ elem2(eqString);
  var not4 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean));
  var pure10 = /* @__PURE__ */ pure(applicativeArray);
  var map24 = /* @__PURE__ */ map(functorArray);
  var startsWith = function(str0) {
    return function(str1) {
      return str0 === take2(length5(str0))(str1);
    };
  };
  var extract3 = /* @__PURE__ */ function() {
    var f = function(acc) {
      return function(v) {
        if (v instanceof Property && v.value0 === "className") {
          return lmap2(function(v1) {
            return append12(v1)(split(" ")(v.value1));
          })(acc);
        }
        ;
        return rmap3(function(v1) {
          return append12(v1)([v]);
        })(acc);
      };
    };
    return foldl2(f)(new Tuple([], []));
  }();
  var css = function($39) {
    return class_(ClassName($39));
  };
  var classifySide = function(str) {
    if (startsWith("t-")(str)) {
      return "top";
    }
    ;
    if (startsWith("r-")(str)) {
      return "right";
    }
    ;
    if (startsWith("b-")(str)) {
      return "bottom";
    }
    ;
    if (startsWith("l-")(str)) {
      return "left";
    }
    ;
    if (startsWith("x-")(str)) {
      return "horizontal";
    }
    ;
    if (startsWith("y-")(str)) {
      return "vertical";
    }
    ;
    if (startsWith("-")(str)) {
      return "all";
    }
    ;
    if (otherwise) {
      return "";
    }
    ;
    throw new Error("Failed pattern match at Ocelot.HTML.Properties (line 100, column 1 - line 102, column 12): " + [str.constructor.name]);
  };
  var append$prime = function(x10) {
    return function(v) {
      if (v === "") {
        return x10;
      }
      ;
      return x10 + ("-" + v);
    };
  };
  var classifyOverflow = function(str) {
    if (startsWith("x-")(str)) {
      return append$prime("horizontal")(classifyOverflow(drop2(2)(str)));
    }
    ;
    if (startsWith("y-")(str)) {
      return append$prime("vertical")(classifyOverflow(drop2(2)(str)));
    }
    ;
    if (elem3(str)(["auto", "hidden", "visible", "scroll"])) {
      return "";
    }
    ;
    if (otherwise) {
      return str;
    }
    ;
    throw new Error("Failed pattern match at Ocelot.HTML.Properties (line 113, column 1 - line 115, column 12): " + [str.constructor.name]);
  };
  var classify = function(str) {
    if (startsWith("p")(str) && not4($$null2)(classifySide(drop2(1)(str)))) {
      return append$prime("padding")(classifySide(drop2(1)(str)));
    }
    ;
    if (startsWith("m")(str) && not4($$null2)(classifySide(drop2(1)(str)))) {
      return append$prime("margin")(classifySide(drop2(1)(str)));
    }
    ;
    if (startsWith("-m")(str) && not4($$null2)(classifySide(drop2(2)(str)))) {
      return append$prime("margin")(classifySide(drop2(2)(str)));
    }
    ;
    if (startsWith("min-")(str)) {
      return append$prime("min")(classify(drop2(4)(str)));
    }
    ;
    if (startsWith("max-")(str)) {
      return append$prime("max")(classify(drop2(4)(str)));
    }
    ;
    if (startsWith("w-")(str)) {
      return "width";
    }
    ;
    if (startsWith("h-")(str)) {
      return "height";
    }
    ;
    if (startsWith("overflow-")(str) && classifyOverflow(drop2(9)(str)) !== drop2(9)(str)) {
      return append$prime("overflow")(classifyOverflow(drop2(9)(str)));
    }
    ;
    if (otherwise) {
      return str;
    }
    ;
    throw new Error("Failed pattern match at Ocelot.HTML.Properties (line 82, column 1 - line 84, column 12): " + [str.constructor.name]);
  };
  var appendIProps = function(ip) {
    return function(ip$prime) {
      var v = extract3(ip);
      var v1 = extract3(ip$prime);
      var classNames = pure10(classes(map24(ClassName)(nubByEq(function(c) {
        return function(c$prime) {
          return classify(c) === classify(c$prime);
        };
      })(append12(v1.value0)(v.value0)))));
      return append12(v.value1)(append12(v1.value1)(classNames));
    };
  };

  // output/Ocelot.Block.Format/index.js
  var map25 = /* @__PURE__ */ map(functorArray);
  var subHeadingClasses = /* @__PURE__ */ map25(ClassName)(["text-xl", "font-medium", "leading-loose", "flex", "items-center", "mb-6"]);
  var subHeading = function(iprops) {
    return function(html2) {
      return h2(appendIProps([classes(subHeadingClasses)])(iprops))(html2);
    };
  };
  var subHeading_ = /* @__PURE__ */ subHeading([]);
  var mutedClasses = /* @__PURE__ */ map25(ClassName)(["text-grey-50"]);
  var linkClasses = /* @__PURE__ */ map25(ClassName)(["text-blue-75", "hover:text-blue-65", "no-underline", "font-medium", "cursor-pointer"]);
  var contentHeadingClasses = /* @__PURE__ */ map25(ClassName)(["mb-6", "text-lg", "font-normal", "leading-loose", "flex", "items-center"]);
  var contentHeading = function(iprops) {
    return h3(appendIProps([classes(contentHeadingClasses)])(iprops));
  };
  var contentHeading_ = /* @__PURE__ */ contentHeading([]);
  var captionClasses = /* @__PURE__ */ map25(ClassName)(["block", "font-light", "mb-6", "text-grey-70", "text-sm", "tracking-wide", "uppercase"]);
  var caption2 = function(iprops) {
    return h4(appendIProps([classes(captionClasses)])(iprops));
  };
  var caption_2 = /* @__PURE__ */ caption2([]);

  // output/D3.Attributes.Instances/index.js
  var Static = /* @__PURE__ */ function() {
    function Static2(value0) {
      this.value0 = value0;
    }
    ;
    Static2.create = function(value0) {
      return new Static2(value0);
    };
    return Static2;
  }();
  var Fn = /* @__PURE__ */ function() {
    function Fn2(value0) {
      this.value0 = value0;
    }
    ;
    Fn2.create = function(value0) {
      return new Fn2(value0);
    };
    return Fn2;
  }();
  var FnI = /* @__PURE__ */ function() {
    function FnI2(value0) {
      this.value0 = value0;
    }
    ;
    FnI2.create = function(value0) {
      return new FnI2(value0);
    };
    return FnI2;
  }();
  var StringAttr = /* @__PURE__ */ function() {
    function StringAttr2(value0) {
      this.value0 = value0;
    }
    ;
    StringAttr2.create = function(value0) {
      return new StringAttr2(value0);
    };
    return StringAttr2;
  }();
  var NumberAttr = /* @__PURE__ */ function() {
    function NumberAttr2(value0) {
      this.value0 = value0;
    }
    ;
    NumberAttr2.create = function(value0) {
      return new NumberAttr2(value0);
    };
    return NumberAttr2;
  }();
  var ArrayAttr = /* @__PURE__ */ function() {
    function ArrayAttr2(value0) {
      this.value0 = value0;
    }
    ;
    ArrayAttr2.create = function(value0) {
      return new ArrayAttr2(value0);
    };
    return ArrayAttr2;
  }();
  var AttributeSetter = /* @__PURE__ */ function() {
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
  }();
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
  var attributeLabel = function(v) {
    return v.value0;
  };

  // output/D3.Data.Types/index.js
  var MouseEnter = /* @__PURE__ */ function() {
    function MouseEnter2() {
    }
    ;
    MouseEnter2.value = new MouseEnter2();
    return MouseEnter2;
  }();
  var MouseLeave = /* @__PURE__ */ function() {
    function MouseLeave2() {
    }
    ;
    MouseLeave2.value = new MouseLeave2();
    return MouseLeave2;
  }();
  var MouseClick = /* @__PURE__ */ function() {
    function MouseClick2() {
    }
    ;
    MouseClick2.value = new MouseClick2();
    return MouseClick2;
  }();
  var MouseDown = /* @__PURE__ */ function() {
    function MouseDown2() {
    }
    ;
    MouseDown2.value = new MouseDown2();
    return MouseDown2;
  }();
  var MouseUp = /* @__PURE__ */ function() {
    function MouseUp2() {
    }
    ;
    MouseUp2.value = new MouseUp2();
    return MouseUp2;
  }();
  var Div = /* @__PURE__ */ function() {
    function Div2() {
    }
    ;
    Div2.value = new Div2();
    return Div2;
  }();
  var Svg = /* @__PURE__ */ function() {
    function Svg2() {
    }
    ;
    Svg2.value = new Svg2();
    return Svg2;
  }();
  var Circle = /* @__PURE__ */ function() {
    function Circle2() {
    }
    ;
    Circle2.value = new Circle2();
    return Circle2;
  }();
  var Line = /* @__PURE__ */ function() {
    function Line2() {
    }
    ;
    Line2.value = new Line2();
    return Line2;
  }();
  var Group = /* @__PURE__ */ function() {
    function Group2() {
    }
    ;
    Group2.value = new Group2();
    return Group2;
  }();
  var Text2 = /* @__PURE__ */ function() {
    function Text3() {
    }
    ;
    Text3.value = new Text3();
    return Text3;
  }();
  var Path = /* @__PURE__ */ function() {
    function Path2() {
    }
    ;
    Path2.value = new Path2();
    return Path2;
  }();
  var Rect = /* @__PURE__ */ function() {
    function Rect2() {
    }
    ;
    Rect2.value = new Rect2();
    return Rect2;
  }();
  var DefaultCubic = /* @__PURE__ */ function() {
    function DefaultCubic2() {
    }
    ;
    DefaultCubic2.value = new DefaultCubic2();
    return DefaultCubic2;
  }();
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
    return (selection) => {
      return selection.append(element3);
    };
  }
  function d3DataWithKeyFunction_(data) {
    return (keyFn) => (selection) => {
      return selection.data(data, keyFn);
    };
  }
  function d3EnterAndAppend_(element3) {
    return (selection) => {
      return selection.enter().append(element3);
    };
  }
  function d3GetExitSelection_(selection) {
    return selection.exit();
  }
  function d3GetEnterSelection_(selection) {
    return selection.enter();
  }
  function d3GetSelectionData_(selection) {
    return selection.data();
  }
  function d3FilterSelection_(selection) {
    return (selector) => selection.filter(selector);
  }
  function d3LowerSelection_(selection) {
    return selection.lower();
  }
  function d3MergeSelectionWith_(enter) {
    return (update3) => {
      return enter.merge(update3);
    };
  }
  function d3OrderSelection_(selection) {
    return selection.order();
  }
  function d3RaiseSelection_(selection) {
    return selection.raise();
  }
  function d3RemoveSelection_(selection) {
    return selection.remove();
  }
  function d3SelectAllInDOM_(selector) {
    return d3.selectAll(selector);
  }
  function d3SelectFirstInDOM_(selector) {
    return d3.select(selector);
  }
  function d3SelectionIsEmpty_(selection) {
    return selection.empty();
  }
  function d3SelectionSelect_(selector) {
    return (selection) => {
      return selection.select(selector);
    };
  }
  function d3SelectionSelectAll_(selector) {
    return (selection) => {
      return selection.selectAll(selector);
    };
  }
  function d3SetAttr_(name17) {
    return (value15) => (selection) => {
      return selection.attr(name17, value15);
    };
  }
  function d3SetHTML_(value15) {
    return (selection) => {
      return selection.html(value15);
    };
  }
  function d3SetProperty_(value15) {
    return (selection) => {
      return selection.property(value15);
    };
  }
  function d3SetText_(value15) {
    return (selection) => {
      return selection.text(value15);
    };
  }
  function d3SortSelection_(selection) {
    return (compare2) => selection.sort(compare2);
  }
  function simulationDrag_(label6) {
    return (selection) => (simulation) => (dragFn) => selection.call(dragFn(label6, simulation));
  }
  function disableDrag_(selection) {
    return selection.on(".drag", null);
  }
  function getIndexFromDatum_(datum) {
    return typeof datum.index == `undefined` ? "?" : datum.index;
  }
  function selectionOn_(selection) {
    return (event) => (callback) => {
      return selection.on(event, callback);
    };
  }
  function d3AddTransition_(selection) {
    return (transition) => {
      var handle;
      if (transition.name == "") {
        handle = selection.transition();
        if (transition.duration != 0) {
          handle.duration(transition.duration);
        }
        if (transition.delay != 0) {
          handle.delay(transition.delay);
        }
      } else {
        handle = selection.transition(transition.name);
      }
      return handle;
    };
  }
  function simdrag(label6, simulation) {
    function dragstarted(event) {
      if (!event.active)
        simulation.alphaTarget(0.3).restart();
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }
    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }
    function dragended(event) {
      if (!event.active)
        simulation.alphaTarget(0);
      event.subject.fx = null;
      event.subject.fy = null;
    }
    return d3.drag().on("start." + label6, dragstarted).on("drag." + label6, dragged).on("end." + label6, dragended);
  }
  var linksForceName = "links";
  function disableTick_(simulation) {
    return (name17) => {
      return simulation.on("tick." + name17, () => null);
    };
  }
  function forceCenter_() {
    return d3.forceCenter();
  }
  function forceCollideFn_() {
    return d3.forceCollide();
  }
  function forceLink_() {
    return d3.forceLink().id((d2) => d2.id);
  }
  function forceMany_() {
    return d3.forceManyBody();
  }
  function forceRadial_() {
    return d3.forceRadial();
  }
  function forceX_() {
    return d3.forceX();
  }
  function forceY_() {
    return d3.forceY();
  }
  function getNodes_(simulation) {
    return simulation.nodes();
  }
  function keyIsID_(d2) {
    return d2.id;
  }
  function setAlpha_(simulation) {
    return (alpha) => {
      console.log(`FFI: setting simulation.alpha to ${alpha}`);
      simulation.alpha(alpha);
    };
  }
  function setAlphaDecay_(simulation) {
    return (alphaDecay) => simulation.alphaDecay(alphaDecay);
  }
  function setAlphaMin_(simulation) {
    return (alphaMin) => simulation.alphaMin(alphaMin);
  }
  function setAlphaTarget_(simulation) {
    return (alphaTarget) => simulation.alphaTarget(alphaTarget);
  }
  function setAsNullForceInSimulation_(simulation) {
    return (label6) => simulation.force(label6, null);
  }
  function setForceDistance_(force2) {
    return (attr3) => force2.distance(attr3);
  }
  function setForceDistanceMax_(force2) {
    return (attr3) => force2.distanceMax(attr3);
  }
  function setForceDistanceMin_(force2) {
    return (attr3) => force2.distanceMin(attr3);
  }
  function setForceIterations_(force2) {
    return (attr3) => force2.iterations(attr3);
  }
  function setForceRadius_(force2) {
    return (attr3) => force2.radius(attr3);
  }
  function setForceStrength_(force2) {
    return (attr3) => force2.strength(attr3);
  }
  function setForceTheta_(force2) {
    return (attr3) => force2.theta(attr3);
  }
  function setForceX_(force2) {
    return (attr3) => force2.x(attr3);
  }
  function setForceY_(force2) {
    return (attr3) => force2.y(attr3);
  }
  function setVelocityDecay_(simulation) {
    return (velocityDecay) => simulation.velocityDecay(velocityDecay);
  }
  function startSimulation_(simulation) {
    console.log(`FFI: restarting the simulation, alpha is: ${simulation.alpha()}`);
    simulation.restart();
  }
  function stopSimulation_(simulation) {
    return simulation.stop();
  }
  function initSimulation_(config) {
    return (keyFn) => {
      const simulation = d3.forceSimulation([]).force(linksForceName, d3.forceLink([]).id(keyFn)).alpha(config.alpha).alphaTarget(config.alphaTarget).alphaMin(config.alphaMin).alphaDecay(config.alphaDecay).velocityDecay(config.velocityDecay);
      if (true) {
        console.log(`FFI: initSimulation${simulation}`);
      }
      return simulation;
    };
  }
  function readSimulationVariables(simulation) {
    return {
      alpha: simulation.alpha(),
      alphaTarget: simulation.alphaTarget(),
      alphaMin: simulation.alphaMin(),
      alphaDecay: simulation.alphaDecay(),
      velocityDecay: simulation.velocityDecay()
    };
  }
  unpin = (d2) => {
    d2.fx = null;
    d2.fy = null;
    return d2;
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
  function d3PreserveSimulationPositions_(selection) {
    return (nodedata) => (keyFn) => {
      const oldNodeMap = new Map(selection.data().map((d2) => [keyFn(d2), d2]));
      const newNodeMap = new Map(nodedata.map((d2) => [keyFn(d2), d2]));
      console.log(`FFI: d3PreserveSimulationPositions_ given ${nodedata.length} nodes, in selection ${selection.data().length}`);
      let updatedNodeData = nodedata.map((d2) => {
        let id4 = keyFn(d2);
        let newNode = newNodeMap.get(id4);
        let shell = {};
        if (newNode) {
          console.log(`FFI: copying fx/fy from incoming node to old object (if present)`);
          shell = { fx: newNode.fx, fy: newNode.fy, gridXY: newNode.gridXY, updated: true };
        }
        return Object.assign(oldNodeMap.get(id4) || d2, shell);
      });
      return updatedNodeData;
    };
  }
  function d3PreserveLinkReferences_(link3) {
    return (links) => {
      const old = new Map(link3.data().map((d2) => [getLinkID_(d2), d2]));
      let updatedLinkData = links.map((d2) => Object.assign(old.get(getLinkID_(d2)) || d2, {}));
      return updatedLinkData;
    };
  }
  function getIDsFromNodes_(nodes) {
    return (keyFn) => {
      const keys4 = [];
      for (let i2 = 0; i2 < nodes.length; i2++) {
        keys4[i2] = keyFn(nodes[i2]);
      }
      return keys4;
    };
  }
  function setNodes_(simulation) {
    return (nodes) => {
      console.log(`FFI: setting nodes in simulation, there are ${nodes.length} nodes`);
      simulation.nodes(nodes);
      return simulation.nodes();
    };
  }
  function setLinks_(simulation) {
    return (links) => {
      console.log(`FFI: setting links in simulation, there are ${links.length} links`);
      simulation.force(linksForceName).links(links);
    };
  }
  function swizzleLinks_(links) {
    return (simNodes) => (keyFn) => {
      console.log(`FFI: swizzling links in simulation, there are ${links.length} links`);
      const nodeById = new Map(simNodes.map((d2) => [keyFn(d2), d2]));
      const swizzledLinks = links.filter((link3, index5, arr) => {
        if (typeof link3.source !== "object") {
          link3.source = nodeById.get(link3.source);
        } else {
          link3.source = nodeById.get(keyFn(link3.source));
        }
        if (typeof link3.target !== "object") {
          link3.target = nodeById.get(link3.target);
        } else {
          link3.target = nodeById.get(keyFn(link3.target));
        }
        if (typeof link3.source === "undefined" || link3.target === "undefined") {
          return false;
        } else {
          link3.id = keyFn(link3.source) + "-" + keyFn(link3.target);
          return true;
        }
      });
      return swizzledLinks;
    };
  }
  function unsetLinks_(simulation) {
    const linkForce = d3.forceLink([]);
    console.log("FFI: removing all links from simulation");
    simulation.force(linksForceName, linkForce);
    return simulation;
  }
  function getLinkID_(keyFn) {
    return (link3) => {
      const sourceID = typeof link3.source == `object` ? keyFn(link3.source) : link3.source;
      const targetID = typeof link3.target == `object` ? keyFn(link3.target) : link3.target;
      return sourceID + "-" + targetID;
    };
  }
  function getLinkIDs_(keyFn) {
    return (link3) => {
      const sourceID = typeof link3.source == `object` ? keyFn(link3.source) : link3.source;
      const targetID = typeof link3.target == `object` ? keyFn(link3.target) : link3.target;
      return { sourceID, targetID };
    };
  }
  function getLinksFromSimulation_(simulation) {
    linksForce = simulation.force(linksForceName);
    if (typeof linksForce === `undefined`) {
      return [];
    }
    const result = linksForce.links();
    if (typeof result === `undefined`) {
      return [];
    }
    return result;
  }
  function onTick_(simulation) {
    return (name17) => (tickFn) => {
      var result = simulation.on("tick." + name17, () => {
        tickFn();
      });
      return result;
    };
  }
  function putForceInSimulation_(simulation) {
    return (label6) => (force2) => {
      simulation.force(label6, force2);
    };
  }
  function descendants_(tree2) {
    return tree2.descendants();
  }
  function getClusterLayoutFn_() {
    return d3.cluster();
  }
  function getTreeLayoutFn_() {
    return d3.tree();
  }
  function hasChildren_(d2) {
    return d2.children === "undefined" ? false : true;
  }
  function getHierarchyValue_(d2) {
    return d2.value === "undefined" ? null : d2.value;
  }
  function getHierarchyChildren_(d2) {
    return !d2.children ? [] : d2.children;
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
    return (root) => layout(root);
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
  function treeSortForTree_Spago(root) {
    return root.sum(function(d2) {
      return d2.value;
    }).sort(function(a2, b2) {
      const result = b2.height - a2.height || a2.data.name.localeCompare(b2.data.name);
      return result;
    });
  }
  function treeMinMax_(root) {
    let max_x = -Infinity;
    let min_x = Infinity;
    let max_y = -Infinity;
    let min_y = Infinity;
    root.each((d2) => {
      if (d2.x > max_x)
        max_x = d2.x;
      if (d2.y > max_y)
        max_y = d2.y;
      if (d2.x < min_x)
        min_x = d2.x;
      if (d2.y < min_y)
        min_y = d2.y;
    });
    return { xMin: min_x, xMax: max_x, yMin: min_y, yMax: max_y };
  }
  var linkHorizontal_ = d3.linkHorizontal().x((d2) => d2.y).y((d2) => d2.x);
  var linkHorizontal2_ = d3.linkHorizontal().x((d2) => d2.x).y((d2) => d2.y);
  var linkVertical_ = d3.linkVertical().x((d2) => d2.x).y((d2) => d2.y);
  function linkClusterHorizontal_(levelSpacing) {
    return (d2) => `M${d2.target.y}, ${d2.target.x}
   C${d2.source.y + levelSpacing / 2},${d2.target.x}
   ${d2.source.y + levelSpacing / 2},${d2.source.x}
   ${d2.source.y},${d2.source.x}`;
  }
  function linkClusterVertical_(levelSpacing) {
    return (d2) => `M${d2.target.x}, ${d2.target.y}
   C${d2.target.x}, ${d2.source.y + levelSpacing / 2}
   ${d2.source.x},${d2.source.y + levelSpacing / 2}
   ${d2.source.x},${d2.source.y}`;
  }
  function linkRadial_(angleFn) {
    return (radiusFn) => d3.linkRadial().angle(angleFn).radius(radiusFn);
  }
  function d3AttachZoomDefaultExtent_(selection) {
    return (config) => {
      function zoomed({ transform: transform2 }) {
        config.target.attr("transform", transform2);
      }
      return selection.call(d3.zoom().scaleExtent(config.scaleExtent).on(`zoom.${config.name}`, zoomed));
    };
  }
  function d3AttachZoom_(selection) {
    return (config) => {
      selection.call(d3.zoom().extent(config.extent).scaleExtent(config.scaleExtent).on(`zoom.${config.name}`, (event) => {
        config.target.attr("transform", event.transform);
      }));
      return selection;
    };
  }

  // output/D3.Data.Tree/foreign.js
  function idTreeLeaf_(obj) {
    const treeObj = Object.assign({}, obj);
    treeObj.isTreeLeaf = true;
    return treeObj;
  }
  function idTreeParent_(obj) {
    return (children2) => {
      const treeObj = Object.assign({}, obj);
      treeObj.isTreeLeaf = false;
      treeObj.children = children2;
      return treeObj;
    };
  }
  var emptyTreeJson_ = {};

  // output/D3.Data.Tree/index.js
  var lookup6 = /* @__PURE__ */ lookup(ordInt);
  var map26 = /* @__PURE__ */ map(functorArray);
  var fromFoldable4 = /* @__PURE__ */ fromFoldable3(foldableList);
  var TidyTree = /* @__PURE__ */ function() {
    function TidyTree2() {
    }
    ;
    TidyTree2.value = new TidyTree2();
    return TidyTree2;
  }();
  var Dendrogram = /* @__PURE__ */ function() {
    function Dendrogram2() {
    }
    ;
    Dendrogram2.value = new Dendrogram2();
    return Dendrogram2;
  }();
  var Radial = /* @__PURE__ */ function() {
    function Radial2() {
    }
    ;
    Radial2.value = new Radial2();
    return Radial2;
  }();
  var Horizontal = /* @__PURE__ */ function() {
    function Horizontal2() {
    }
    ;
    Horizontal2.value = new Horizontal2();
    return Horizontal2;
  }();
  var Vertical = /* @__PURE__ */ function() {
    function Vertical2() {
    }
    ;
    Vertical2.value = new Vertical2();
    return Vertical2;
  }();
  var makeD3TreeJSONFromTreeID = function(root) {
    return function(nodesMap) {
      var go2 = function(v) {
        var v1 = lookup6(v.value0)(nodesMap);
        if (v1 instanceof Nothing) {
          return emptyTreeJson_;
        }
        ;
        if (v1 instanceof Just) {
          if (v.value1 instanceof Nil) {
            return idTreeLeaf_(v1.value0);
          }
          ;
          return idTreeParent_(v1.value0)(map26(go2)(fromFoldable4(v.value1)));
        }
        ;
        throw new Error("Failed pattern match at D3.Data.Tree (line 49, column 7 - line 53, column 84): " + [v1.constructor.name]);
      };
      return go2(root);
    };
  };
  var eqTreeLayout = {
    eq: function(x10) {
      return function(y8) {
        if (x10 instanceof Radial && y8 instanceof Radial) {
          return true;
        }
        ;
        if (x10 instanceof Horizontal && y8 instanceof Horizontal) {
          return true;
        }
        ;
        if (x10 instanceof Vertical && y8 instanceof Vertical) {
          return true;
        }
        ;
        return false;
      };
    }
  };

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
  var show4 = /* @__PURE__ */ show(showMouseEvent);
  var Order = /* @__PURE__ */ function() {
    function Order2() {
    }
    ;
    Order2.value = new Order2();
    return Order2;
  }();
  var Sort = /* @__PURE__ */ function() {
    function Sort2(value0) {
      this.value0 = value0;
    }
    ;
    Sort2.create = function(value0) {
      return new Sort2(value0);
    };
    return Sort2;
  }();
  var Raise2 = /* @__PURE__ */ function() {
    function Raise3() {
    }
    ;
    Raise3.value = new Raise3();
    return Raise3;
  }();
  var Lower = /* @__PURE__ */ function() {
    function Lower2() {
    }
    ;
    Lower2.value = new Lower2();
    return Lower2;
  }();
  var AttrT = /* @__PURE__ */ function() {
    function AttrT2(value0) {
      this.value0 = value0;
    }
    ;
    AttrT2.create = function(value0) {
      return new AttrT2(value0);
    };
    return AttrT2;
  }();
  var TextT = /* @__PURE__ */ function() {
    function TextT2(value0) {
      this.value0 = value0;
    }
    ;
    TextT2.create = function(value0) {
      return new TextT2(value0);
    };
    return TextT2;
  }();
  var HTMLT = /* @__PURE__ */ function() {
    function HTMLT2(value0) {
      this.value0 = value0;
    }
    ;
    HTMLT2.create = function(value0) {
      return new HTMLT2(value0);
    };
    return HTMLT2;
  }();
  var PropertyT = /* @__PURE__ */ function() {
    function PropertyT2(value0) {
      this.value0 = value0;
    }
    ;
    PropertyT2.create = function(value0) {
      return new PropertyT2(value0);
    };
    return PropertyT2;
  }();
  var OrderingT = /* @__PURE__ */ function() {
    function OrderingT2(value0) {
      this.value0 = value0;
    }
    ;
    OrderingT2.create = function(value0) {
      return new OrderingT2(value0);
    };
    return OrderingT2;
  }();
  var TransitionT = /* @__PURE__ */ function() {
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
  }();
  var RemoveT = /* @__PURE__ */ function() {
    function RemoveT2() {
    }
    ;
    RemoveT2.value = new RemoveT2();
    return RemoveT2;
  }();
  var OnT = /* @__PURE__ */ function() {
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
  }();
  var OnT$prime = /* @__PURE__ */ function() {
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
  }();
  var DefaultDrag = /* @__PURE__ */ function() {
    function DefaultDrag2() {
    }
    ;
    DefaultDrag2.value = new DefaultDrag2();
    return DefaultDrag2;
  }();
  var NoDrag = /* @__PURE__ */ function() {
    function NoDrag2() {
    }
    ;
    NoDrag2.value = new NoDrag2();
    return NoDrag2;
  }();
  var CustomDrag = /* @__PURE__ */ function() {
    function CustomDrag2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CustomDrag2.create = function(value0) {
      return function(value1) {
        return new CustomDrag2(value0, value1);
      };
    };
    return CustomDrag2;
  }();
  var Drag = /* @__PURE__ */ function() {
    function Drag2(value0) {
      this.value0 = value0;
    }
    ;
    Drag2.create = function(value0) {
      return new Drag2(value0);
    };
    return Drag2;
  }();
  var Zoom = /* @__PURE__ */ function() {
    function Zoom2(value0) {
      this.value0 = value0;
    }
    ;
    Zoom2.create = function(value0) {
      return new Zoom2(value0);
    };
    return Zoom2;
  }();
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
  var show12 = /* @__PURE__ */ show(showOrderingAttribute);
  var showSelectionAttribute = {
    show: function(v) {
      if (v instanceof AttrT) {
        return "chainable: attr " + attributeLabel(v.value0);
      }
      ;
      if (v instanceof TextT) {
        return "chainable: text";
      }
      ;
      if (v instanceof HTMLT) {
        return "chainable: html" + attributeLabel(v.value0);
      }
      ;
      if (v instanceof PropertyT) {
        return "chainable: property" + attributeLabel(v.value0);
      }
      ;
      if (v instanceof TransitionT) {
        return "chainable: transition";
      }
      ;
      if (v instanceof RemoveT) {
        return "chainable: remove";
      }
      ;
      if (v instanceof OnT) {
        return show4(v.value0);
      }
      ;
      if (v instanceof OnT$prime) {
        return show4(v.value0);
      }
      ;
      if (v instanceof OrderingT) {
        return "chainable: ordering" + show12(v.value0);
      }
      ;
      throw new Error("Failed pattern match at D3.Selection (line 55, column 1 - line 68, column 62): " + [v.constructor.name]);
    }
  };
  var applySelectionAttributeD3 = function(selection_) {
    return function(v) {
      if (v instanceof AttrT) {
        return d3SetAttr_(v.value0.value0)(unboxAttr(v.value0.value1))(selection_);
      }
      ;
      if (v instanceof TextT) {
        return d3SetText_(unboxAttr(v.value0.value1))(selection_);
      }
      ;
      if (v instanceof PropertyT) {
        return d3SetProperty_(unboxAttr(v.value0.value1))(selection_);
      }
      ;
      if (v instanceof HTMLT) {
        return d3SetHTML_(unboxAttr(v.value0.value1))(selection_);
      }
      ;
      if (v instanceof RemoveT) {
        var removed_ = d3RemoveSelection_(selection_);
        return removed_;
      }
      ;
      if (v instanceof TransitionT) {
        var tHandler = d3AddTransition_(selection_)(v.value1);
        var v1 = foldl2(applySelectionAttributeD3)(tHandler)(v.value0);
        return selection_;
      }
      ;
      if (v instanceof OnT) {
        return selectionOn_(selection_)(show4(v.value0))(v.value1);
      }
      ;
      if (v instanceof OnT$prime) {
        return selectionOn_(selection_)(show4(v.value0))(v.value1);
      }
      ;
      if (v instanceof OrderingT) {
        if (v.value0 instanceof Order) {
          return d3OrderSelection_(selection_);
        }
        ;
        if (v.value0 instanceof Sort) {
          return d3SortSelection_(selection_)(v.value0.value0);
        }
        ;
        if (v.value0 instanceof Raise2) {
          return d3RaiseSelection_(selection_);
        }
        ;
        if (v.value0 instanceof Lower) {
          return d3LowerSelection_(selection_);
        }
        ;
        throw new Error("Failed pattern match at D3.Selection (line 108, column 3 - line 112, column 51): " + [v.value0.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at D3.Selection (line 78, column 1 - line 78, column 80): " + [selection_.constructor.name, v.constructor.name]);
    };
  };

  // output/D3.Attributes.Sugar/index.js
  var toAttr2 = /* @__PURE__ */ toAttr(toAttrString);
  var intercalate4 = /* @__PURE__ */ intercalate3(monoidString);
  var map27 = /* @__PURE__ */ map(functorArray);
  var show5 = /* @__PURE__ */ show(showNumber);
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var flap2 = /* @__PURE__ */ flap(functorArray);
  var Meet = /* @__PURE__ */ function() {
    function Meet2() {
    }
    ;
    Meet2.value = new Meet2();
    return Meet2;
  }();
  var Slice = /* @__PURE__ */ function() {
    function Slice2() {
    }
    ;
    Slice2.value = new Slice2();
    return Slice2;
  }();
  var None2 = /* @__PURE__ */ function() {
    function None4() {
    }
    ;
    None4.value = new None4();
    return None4;
  }();
  var YMin = /* @__PURE__ */ function() {
    function YMin2() {
    }
    ;
    YMin2.value = new YMin2();
    return YMin2;
  }();
  var YMid = /* @__PURE__ */ function() {
    function YMid2() {
    }
    ;
    YMid2.value = new YMid2();
    return YMid2;
  }();
  var YMax = /* @__PURE__ */ function() {
    function YMax2() {
    }
    ;
    YMax2.value = new YMax2();
    return YMax2;
  }();
  var XMin = /* @__PURE__ */ function() {
    function XMin2() {
    }
    ;
    XMin2.value = new XMin2();
    return XMin2;
  }();
  var XMid = /* @__PURE__ */ function() {
    function XMid2() {
    }
    ;
    XMid2.value = new XMid2();
    return XMid2;
  }();
  var XMax = /* @__PURE__ */ function() {
    function XMax2() {
    }
    ;
    XMax2.value = new XMax2();
    return XMax2;
  }();
  var AspectRatio = /* @__PURE__ */ function() {
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
  }();
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
      throw new Error("Failed pattern match at D3.Attributes.Sugar (line 60, column 1 - line 63, column 22): " + [v.constructor.name]);
    }
  };
  var show13 = /* @__PURE__ */ show(showAspectRatioPreserve);
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
      throw new Error("Failed pattern match at D3.Attributes.Sugar (line 55, column 1 - line 58, column 21): " + [v.constructor.name]);
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
      throw new Error("Failed pattern match at D3.Attributes.Sugar (line 50, column 1 - line 53, column 21): " + [v.constructor.name]);
    }
  };
  var show32 = /* @__PURE__ */ show(showAlignAspectRatio_X);
  var showAspectRatioSpec = {
    show: function(v) {
      if (v.value2 instanceof None2) {
        return "none";
      }
      ;
      return show32(v.value0) + (show22(v.value1) + (" " + show13(v.value2)));
    }
  };
  var y2 = function(dictToAttr) {
    var $105 = AttributeSetter.create("y2");
    var $106 = toAttr(dictToAttr);
    return function($107) {
      return AttrT.create($105($106($107)));
    };
  };
  var y1 = function(dictToAttr) {
    var $108 = AttributeSetter.create("y1");
    var $109 = toAttr(dictToAttr);
    return function($110) {
      return AttrT.create($108($109($110)));
    };
  };
  var y = function(dictToAttr) {
    var $111 = AttributeSetter.create("y");
    var $112 = toAttr(dictToAttr);
    return function($113) {
      return AttrT.create($111($112($113)));
    };
  };
  var x2 = function(dictToAttr) {
    var $114 = AttributeSetter.create("x2");
    var $115 = toAttr(dictToAttr);
    return function($116) {
      return AttrT.create($114($115($116)));
    };
  };
  var x1 = function(dictToAttr) {
    var $117 = AttributeSetter.create("x1");
    var $118 = toAttr(dictToAttr);
    return function($119) {
      return AttrT.create($117($118($119)));
    };
  };
  var x = function(dictToAttr) {
    var $120 = AttributeSetter.create("x");
    var $121 = toAttr(dictToAttr);
    return function($122) {
      return AttrT.create($120($121($122)));
    };
  };
  var width8 = function(dictToAttr) {
    var $123 = AttributeSetter.create("width");
    var $124 = toAttr(dictToAttr);
    return function($125) {
      return AttrT.create($123($124($125)));
    };
  };
  var viewBox = function(xo) {
    return function(yo) {
      return function(w) {
        return function(h) {
          var vb = intercalate4(" ")(map27(show5)([xo, yo, w, h]));
          return AttrT.create(AttributeSetter.create("viewBox")(toAttr2(vb)));
        };
      };
    };
  };
  var transform$prime = /* @__PURE__ */ function() {
    var $126 = AttributeSetter.create("transform");
    return function($127) {
      return AttrT.create($126(StringAttr.create(Fn.create($127))));
    };
  }();
  var to = function(v) {
    return function(v1) {
      if (v instanceof TransitionT && v.value0.length === 0) {
        return [new TransitionT(v1, v.value1)];
      }
      ;
      if (v instanceof TransitionT) {
        return [new TransitionT(append13(v.value0)(v1), v.value1)];
      }
      ;
      return cons3(v)(v1);
    };
  };
  var textAnchor = function(dictToAttr) {
    var $128 = AttributeSetter.create("text-anchor");
    var $129 = toAttr(dictToAttr);
    return function($130) {
      return AttrT.create($128($129($130)));
    };
  };
  var text6 = function(dictToAttr) {
    var $131 = AttributeSetter.create("text");
    var $132 = toAttr(dictToAttr);
    return function($133) {
      return TextT.create($131($132($133)));
    };
  };
  var strokeWidth = function(dictToAttr) {
    var $134 = AttributeSetter.create("stroke-width");
    var $135 = toAttr(dictToAttr);
    return function($136) {
      return AttrT.create($134($135($136)));
    };
  };
  var strokeOpacity = function(dictToAttr) {
    var $137 = AttributeSetter.create("stroke-opacity");
    var $138 = toAttr(dictToAttr);
    return function($139) {
      return AttrT.create($137($138($139)));
    };
  };
  var strokeColor = function(dictToAttr) {
    var $140 = AttributeSetter.create("stroke");
    var $141 = toAttr(dictToAttr);
    return function($142) {
      return AttrT.create($140($141($142)));
    };
  };
  var remove = /* @__PURE__ */ function() {
    return RemoveT.value;
  }();
  var radius = function(dictToAttr) {
    var $152 = AttributeSetter.create("r");
    var $153 = toAttr(dictToAttr);
    return function($154) {
      return AttrT.create($152($153($154)));
    };
  };
  var preserveAspectRatio = /* @__PURE__ */ function() {
    var $155 = AttributeSetter.create("preserveAspectRatio");
    var $156 = show(showAspectRatioSpec);
    return function($157) {
      return AttrT.create($155(toAttr2($156($157))));
    };
  }();
  var opacity = function(dictToAttr) {
    var $164 = AttributeSetter.create("opacity");
    var $165 = toAttr(dictToAttr);
    return function($166) {
      return AttrT.create($164($165($166)));
    };
  };
  var onMouseEventEffectful = function(event) {
    return function(listener) {
      return new OnT$prime(event, mkEffectFn3(listener));
    };
  };
  var height8 = function(dictToAttr) {
    var $167 = AttributeSetter.create("height");
    var $168 = toAttr(dictToAttr);
    return function($169) {
      return AttrT.create($167($168($169)));
    };
  };
  var fontSize = function(dictToAttr) {
    var $170 = AttributeSetter.create("font-size");
    var $171 = toAttr(dictToAttr);
    return function($172) {
      return AttrT.create($170($171($172)));
    };
  };
  var fontFamily = function(dictToAttr) {
    var $173 = AttributeSetter.create("font-family");
    var $174 = toAttr(dictToAttr);
    return function($175) {
      return AttrT.create($173($174($175)));
    };
  };
  var fill = function(dictToAttr) {
    var $176 = AttributeSetter.create("fill");
    var $177 = toAttr(dictToAttr);
    return function($178) {
      return AttrT.create($176($177($178)));
    };
  };
  var dy = function(dictToAttr) {
    var $179 = AttributeSetter.create("dy");
    var $180 = toAttr(dictToAttr);
    return function($181) {
      return AttrT.create($179($180($181)));
    };
  };
  var defaultTransition = /* @__PURE__ */ function() {
    return {
      name: "",
      delay: 0,
      duration: 0,
      easing: DefaultCubic.value
    };
  }();
  var transitionWithDuration = function(duration2) {
    return new TransitionT([], {
      name: defaultTransition.name,
      delay: defaultTransition.delay,
      duration: duration2,
      easing: defaultTransition.easing
    });
  };
  var cy = function(dictToAttr) {
    var $185 = AttributeSetter.create("cy");
    var $186 = toAttr(dictToAttr);
    return function($187) {
      return AttrT.create($185($186($187)));
    };
  };
  var cx = function(dictToAttr) {
    var $188 = AttributeSetter.create("cx");
    var $189 = toAttr(dictToAttr);
    return function($190) {
      return AttrT.create($188($189($190)));
    };
  };
  var cursor = function(dictToAttr) {
    var $191 = AttributeSetter.create("cursor");
    var $192 = toAttr(dictToAttr);
    return function($193) {
      return AttrT.create($191($192($193)));
    };
  };
  var classed = function(dictToAttr) {
    var $194 = AttributeSetter.create("class");
    var $195 = toAttr(dictToAttr);
    return function($196) {
      return AttrT.create($194($195($196)));
    };
  };
  var assembleTransforms = function(fs) {
    return function(d2) {
      return intercalate4(" ")(flap2(fs)(d2));
    };
  };
  var transform = function($200) {
    return transform$prime(assembleTransforms($200));
  };
  var andThen = function(dictSemigroup) {
    return append(dictSemigroup);
  };

  // output/D3Tagless.Capabilities/index.js
  var updateJoin = function(dict) {
    return dict.updateJoin;
  };
  var stop = function(dict) {
    return dict.stop;
  };
  var start2 = function(dict) {
    return dict.start;
  };
  var simulationHandle = function(dict) {
    return dict.simulationHandle;
  };
  var simpleJoin = function(dict) {
    return dict.simpleJoin;
  };
  var setNodesFromSelection = function(dict) {
    return dict.setNodesFromSelection;
  };
  var setNodes = function(dict) {
    return dict.setNodes;
  };
  var setLinksFromSelection = function(dict) {
    return dict.setLinksFromSelection;
  };
  var setLinks = function(dict) {
    return dict.setLinks;
  };
  var setConfigVariable = function(dict) {
    return dict.setConfigVariable;
  };
  var setAttributes = function(dict) {
    return dict.setAttributes;
  };
  var selectUnder = function(dict) {
    return dict.selectUnder;
  };
  var openSelection = function(dict) {
    return dict.openSelection;
  };
  var on2 = function(dict) {
    return dict.on;
  };
  var mergeSelections = function(dict) {
    return dict.mergeSelections;
  };
  var mergeNewDataWithSim = function(dict) {
    return dict.mergeNewDataWithSim;
  };
  var attach = function(dict) {
    return dict.attach;
  };
  var appendTo = function(dict) {
    return dict.appendTo;
  };
  var addTickFunction = function(dict) {
    return dict.addTickFunction;
  };
  var actualizeForces = function(dict) {
    return dict.actualizeForces;
  };

  // output/D3.Examples.GUP/index.js
  var andThen2 = /* @__PURE__ */ andThen(semigroupArray);
  var classed2 = /* @__PURE__ */ classed(toAttrString);
  var fill2 = /* @__PURE__ */ fill(toAttrString);
  var y3 = /* @__PURE__ */ y(toAttrNumber);
  var x3 = /* @__PURE__ */ x(toAttrNumberFnI);
  var text7 = /* @__PURE__ */ text6(toAttrStringFn);
  var fontSize2 = /* @__PURE__ */ fontSize(toAttrNumber);
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var keyFunction = unsafeCoerce2;
  var indexIsNumber = unsafeCoerce2;
  var datumIsChar = unsafeCoerce2;
  var exGeneralUpdatePattern = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind18 = bind(Bind1);
    var attach2 = attach(dictSelectionM);
    var appendTo2 = appendTo(dictSelectionM);
    var pure21 = pure(Monad0.Applicative0());
    var openSelection2 = openSelection(dictSelectionM);
    var updateJoin2 = updateJoin(dictSelectionM);
    var discard111 = discard5(Bind1);
    var setAttributes2 = setAttributes(dictSelectionM);
    return function(selector) {
      var xFromIndex2 = function(v) {
        return function(i2) {
          return 50 + indexIsNumber(i2) * 48;
        };
      };
      var transition = transitionWithDuration(2e3);
      var update3 = andThen2([classed2("update"), fill2("gray"), y3(200)])(to(transition)([x3(xFromIndex2)]));
      var exit = andThen2([classed2("exit"), fill2("brown")])(to(transition)([y3(400), remove]));
      var enter = andThen2([classed2("enter"), fill2("green"), x3(xFromIndex2), y3(0), text7(function($24) {
        return singleton6(datumIsChar($24));
      }), fontSize2(96)])(to(transition)([y3(200)]));
      return bind18(attach2(selector))(function(root) {
        return bind18(appendTo2(root)(Svg.value)([viewBox(0)(0)(650)(650), classed2("d3svg gup")]))(function(svg2) {
          return bind18(appendTo2(svg2)(Group.value)([]))(function(letterGroup) {
            return pure21(function(letters) {
              return bind18(openSelection2(letterGroup)("text"))(function(enterSelection) {
                return bind18(updateJoin2(enterSelection)(Text2.value)(letters)(keyFunction))(function(updateSelections) {
                  return discard111(setAttributes2(updateSelections.exit)(exit))(function() {
                    return discard111(setAttributes2(updateSelections.update)(update3))(function() {
                      return bind18(appendTo2(updateSelections.enter)(Text2.value)([]))(function(newlyEntered) {
                        return discard111(setAttributes2(newlyEntered)(enter))(function() {
                          return pure21(newlyEntered);
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

  // output/D3Tagless.Block.Button/index.js
  var map28 = /* @__PURE__ */ map(functorArray);
  var append6 = /* @__PURE__ */ append(semigroupArray);
  var centerVerticalClasses = /* @__PURE__ */ map28(ClassName)(["w-11/12", "rounded", "m-2"]);
  var buttonSharedClasses = /* @__PURE__ */ map28(ClassName)(["no-outline", "px-4", "py-2", "!active:border-b", "active:border-t", "disabled:opacity-50", "disabled:cursor-default", "!disabled:cursor-pointer"]);
  var buttonGroupClasses = /* @__PURE__ */ map28(ClassName)(["flex", "items-center"]);
  var buttonGroupBuilder = function(classes2) {
    return function(iprops) {
      return button(appendIProps([classes(append6(buttonSharedClasses)(classes2))])(iprops));
    };
  };
  var buttonGroup = function(iprops) {
    return div2(appendIProps([classes(buttonGroupClasses)])(iprops));
  };
  var buttonClasses = /* @__PURE__ */ map28(ClassName)(["bg-grey-50-a20", "border-grey-50-a20", "hover:!disabled:bg-grey-50-a30", "focus:bg-grey-50-a30", "text-black-20"]);
  var buttonVertical = /* @__PURE__ */ buttonGroupBuilder(/* @__PURE__ */ append6(buttonClasses)(centerVerticalClasses));

  // output/D3Tagless.Block.Expandable/index.js
  var map29 = /* @__PURE__ */ map(functorArray);
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var Collapsed = /* @__PURE__ */ function() {
    function Collapsed2() {
    }
    ;
    Collapsed2.value = new Collapsed2();
    return Collapsed2;
  }();
  var Expanded = /* @__PURE__ */ function() {
    function Expanded2() {
    }
    ;
    Expanded2.value = new Expanded2();
    return Expanded2;
  }();
  var toBoolean = function(v) {
    if (v instanceof Collapsed) {
      return false;
    }
    ;
    if (v instanceof Expanded) {
      return true;
    }
    ;
    throw new Error("Failed pattern match at D3Tagless.Block.Expandable (line 39, column 1 - line 39, column 31): " + [v.constructor.name]);
  };
  var heytingAlgebraStatus = /* @__PURE__ */ function() {
    return {
      ff: Collapsed.value,
      tt: Expanded.value,
      implies: function(a2) {
        return function(b2) {
          return disj(heytingAlgebraStatus)(not(heytingAlgebraStatus)(a2))(b2);
        };
      },
      conj: function(v) {
        return function(v1) {
          if (v instanceof Expanded && v1 instanceof Expanded) {
            return Expanded.value;
          }
          ;
          return Collapsed.value;
        };
      },
      disj: function(v) {
        return function(v1) {
          if (v instanceof Expanded) {
            return Expanded.value;
          }
          ;
          if (v1 instanceof Expanded) {
            return Expanded.value;
          }
          ;
          return Collapsed.value;
        };
      },
      not: function(v) {
        if (v instanceof Expanded) {
          return Collapsed.value;
        }
        ;
        if (v instanceof Collapsed) {
          return Expanded.value;
        }
        ;
        throw new Error("Failed pattern match at D3Tagless.Block.Expandable (line 47, column 1 - line 57, column 27): " + [v.constructor.name]);
      }
    };
  }();
  var contentSharedClasses = /* @__PURE__ */ map29(ClassName)([]);
  var contentClasses = function(status_) {
    return append7(contentSharedClasses)(function() {
      if (status_ instanceof Collapsed) {
        return map29(ClassName)(["max-h-0", "opacity-0", "w-0", "overflow-hidden", "transition-1/2-in"]);
      }
      ;
      if (status_ instanceof Expanded) {
        return map29(ClassName)(["max-h-full", "opacity-100", "transition-1/2-out"]);
      }
      ;
      throw new Error("Failed pattern match at D3Tagless.Block.Expandable (line 84, column 5 - line 96, column 8): " + [status_.constructor.name]);
    }());
  };
  var content3 = function(status_) {
    return function(iprops) {
      return div2(appendIProps([classes(contentClasses(status_))])(iprops));
    };
  };
  var content_ = function(status_) {
    return content3(status_)([]);
  };

  // output/D3Tagless.Block.FormField/index.js
  var map30 = /* @__PURE__ */ map(functorArray);
  var labelClasses = /* @__PURE__ */ map30(ClassName)(["block", "font-medium", "leading-loose", "text-black-20"]);
  var helpTextClasses = /* @__PURE__ */ append(semigroupArray)(mutedClasses)(/* @__PURE__ */ map30(ClassName)(["block", "font-light", "pt-3"]));
  var helpText = function(iprops) {
    return div2(appendIProps([classes(helpTextClasses)])(iprops));
  };
  var helpText_ = /* @__PURE__ */ helpText([]);
  var fieldClasses = /* @__PURE__ */ map30(ClassName)(["w-full"]);
  var errorTextClasses = /* @__PURE__ */ map30(ClassName)(["block", "text-red", "font-medium", "pt-3"]);
  var error4 = function(iprops) {
    return div2(appendIProps([classes(errorTextClasses)])(iprops));
  };
  var error_ = /* @__PURE__ */ error4([]);
  var field$prime = function(config) {
    return function(iprops) {
      return function(html2) {
        return div2(appendIProps([classes(fieldClasses)])(iprops))([label4([classes(labelClasses), $$for(config.inputId)])([fromPlainHTML(config.label)]), html2, error_(config.error), helpText_(config.helpText)]);
      };
    };
  };
  var field = function(config) {
    return function(iprops) {
      return function(html2) {
        return field$prime(config)(iprops)(div2([css("my-1")])(html2));
      };
    };
  };
  var field_ = function(config) {
    return field(config)([]);
  };

  // output/D3Tagless.Block.Toggle/index.js
  var map31 = /* @__PURE__ */ map(functorArray);
  var append8 = /* @__PURE__ */ append(semigroupArray);
  var type_19 = /* @__PURE__ */ type_17(isPropInputType);
  var toggleClasses = /* @__PURE__ */ map31(ClassName)(["transition-1/8", "inline-flex", "justify-center", "items-center", "content-box", "h-5", "w-5", "p-1", "rounded-full", "mr-3", "before:bg-white", "before:h-full", "before:w-full", "before:rounded-full", "before:no-content", "before:shadow"]);
  var labelClasses2 = /* @__PURE__ */ map31(ClassName)(["flex", "flex-row", "items-center", "inline-block", "py-1", "cursor-pointer", "leading-loose", "text-black-20"]);
  var inputClasses = /* @__PURE__ */ map31(ClassName)(["checked:sibling:bg-blue-88", "checked:sibling:pl-5", "!checked:sibling:bg-grey-80", "!checked:sibling:pr-5", "offscreen"]);
  var toggle = function(iprops) {
    var iprops$prime = append8(iprops)([classes(inputClasses), type_19(InputCheckbox.value)]);
    return label4([classes(labelClasses2)])([input2(iprops$prime), span3([classes(toggleClasses)])([])]);
  };

  // output/D3.Zoom/index.js
  var DefaultZoomExtent = /* @__PURE__ */ function() {
    function DefaultZoomExtent2() {
    }
    ;
    DefaultZoomExtent2.value = new DefaultZoomExtent2();
    return DefaultZoomExtent2;
  }();
  var ZoomExtent = /* @__PURE__ */ function() {
    function ZoomExtent2(value0) {
      this.value0 = value0;
    }
    ;
    ZoomExtent2.create = function(value0) {
      return new ZoomExtent2(value0);
    };
    return ZoomExtent2;
  }();
  var ScaleExtent = /* @__PURE__ */ function() {
    function ScaleExtent2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ScaleExtent2.create = function(value0) {
      return function(value1) {
        return new ScaleExtent2(value0, value1);
      };
    };
    return ScaleExtent2;
  }();

  // output/Debug/foreign.js
  var req = typeof module === "undefined" ? void 0 : module.require;
  var util = function() {
    try {
      return req === void 0 ? void 0 : req("util");
    } catch (e) {
      return void 0;
    }
  }();
  function _trace(x10, k) {
    if (util !== void 0) {
      console.log(util.inspect(x10, { depth: null, colors: true }));
    } else {
      console.log(x10);
    }
    return k({});
  }
  function _spy(tag2, x10) {
    if (util !== void 0) {
      console.log(tag2 + ":", util.inspect(x10, { depth: null, colors: true }));
    } else {
      console.log(tag2 + ":", x10);
    }
    return x10;
  }
  var now = function() {
    var perf;
    if (typeof performance !== "undefined") {
      perf = performance;
    } else if (req) {
      try {
        perf = req("perf_hooks").performance;
      } catch (e) {
      }
    }
    return function() {
      return (perf || Date).now();
    };
  }();

  // output/Debug/index.js
  var trace = function() {
    return function(a2) {
      return function(k) {
        return _trace(a2, k);
      };
    };
  };
  var spy = function() {
    return function(tag2) {
      return function(a2) {
        return _spy(tag2, a2);
      };
    };
  };

  // output/D3.Selection.Functions/index.js
  var spy2 = /* @__PURE__ */ spy();
  var foldl3 = /* @__PURE__ */ foldl(foldableArray);
  var show6 = /* @__PURE__ */ show(showElement);
  var discard6 = /* @__PURE__ */ discard(discardUnit);
  var selectionUpdateJoin = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(openSelection2) {
      return function(e) {
        return function(theData) {
          return function(keyFn) {
            var updateSelection = d3DataWithKeyFunction_(theData)(keyFn)(openSelection2);
            var exitSelection = d3GetExitSelection_(updateSelection);
            var enterSelection = d3GetEnterSelection_(updateSelection);
            return pure21({
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
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection) {
      return function(selector) {
        return pure21(d3SelectionSelectAll_(selector)(selection));
      };
    };
  };
  var selectionOpenSelection = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection) {
      return function(selector) {
        var v = spy2("open selection: ")(selector);
        return pure21(d3SelectionSelectAll_(selector)(selection));
      };
    };
  };
  var selectionOn = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection) {
      return function(v) {
        if (v instanceof Drag) {
          return pure21(unit);
        }
        ;
        if (v instanceof Zoom) {
          var v1 = function() {
            if (v.value0.extent instanceof DefaultZoomExtent) {
              return d3AttachZoomDefaultExtent_(selection)({
                scaleExtent: [v.value0.scale.value0, v.value0.scale.value1],
                name: v.value0.name,
                target: selection
              });
            }
            ;
            if (v.value0.extent instanceof ZoomExtent) {
              return d3AttachZoom_(selection)({
                extent: [[v.value0.extent.value0.left, v.value0.extent.value0.top], [v.value0.extent.value0.right, v.value0.extent.value0.bottom]],
                scaleExtent: [v.value0.scale.value0, v.value0.scale.value1],
                name: v.value0.name,
                target: selection
              });
            }
            ;
            throw new Error("Failed pattern match at D3.Selection.Functions (line 82, column 9 - line 96, column 14): " + [v.value0.extent.constructor.name]);
          }();
          return pure21(unit);
        }
        ;
        throw new Error("Failed pattern match at D3.Selection.Functions (line 65, column 1 - line 65, column 104): " + [selection.constructor.name, v.constructor.name]);
      };
    };
  };
  var selectionModifySelection = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection_) {
      return function(attributes) {
        var v = foldl3(applySelectionAttributeD3)(selection_)(attributes);
        return pure21(unit);
      };
    };
  };
  var selectionMergeSelections = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selectionA) {
      return function(selectionB) {
        return pure21(d3MergeSelectionWith_(selectionA)(selectionB));
      };
    };
  };
  var selectionJoin = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection) {
      return function(e) {
        return function(theData) {
          return function(keyFn) {
            var element3 = spy2("Join: ")(show6(e));
            var selectS = d3SelectionSelectAll_(element3)(selection);
            var dataSelection = d3DataWithKeyFunction_(theData)(keyFn)(selectS);
            var enterSelection = d3EnterAndAppend_(element3)(dataSelection);
            return pure21(enterSelection);
          };
        };
      };
    };
  };
  var selectionFilterSelection = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection_) {
      return function(selector) {
        return pure21(d3FilterSelection_(selection_)(selector));
      };
    };
  };
  var selectionAttach = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selector) {
      return pure21(d3SelectAllInDOM_(selector));
    };
  };
  var selectionAppendElement = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var discard111 = discard6(Monad0.Bind1());
    var selectionModifySelection1 = selectionModifySelection(dictSelectionM);
    var pure21 = pure(Monad0.Applicative0());
    return function(selection_) {
      return function(element3) {
        return function(attributes) {
          var appended_ = d3Append_(show6(element3))(selection_);
          return discard111(selectionModifySelection1(appended_)(attributes))(function() {
            return pure21(appended_);
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

  // output/Data.Lens.Internal.Forget/index.js
  var Forget = function(x10) {
    return x10;
  };
  var profunctorForget = {
    dimap: function(f) {
      return function(v) {
        return function(v1) {
          return function($36) {
            return v1(f($36));
          };
        };
      };
    }
  };
  var strongForget = {
    first: function(v) {
      return function($37) {
        return v(fst($37));
      };
    },
    second: function(v) {
      return function($38) {
        return v(snd($38));
      };
    },
    Profunctor0: function() {
      return profunctorForget;
    }
  };
  var choiceForget = function(dictMonoid) {
    var mempty2 = mempty(monoidFn(dictMonoid));
    return {
      left: function(v) {
        return either(v)(mempty2);
      },
      right: function(v) {
        return either(mempty2)(v);
      },
      Profunctor0: function() {
        return profunctorForget;
      }
    };
  };

  // output/Data.Profunctor.Choice/index.js
  var right = function(dict) {
    return dict.right;
  };
  var choiceFn = {
    left: function(v) {
      return function(v1) {
        if (v1 instanceof Left) {
          return new Left(v(v1.value0));
        }
        ;
        if (v1 instanceof Right) {
          return new Right(v1.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Profunctor.Choice (line 32, column 1 - line 35, column 16): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    right: /* @__PURE__ */ map(functorEither),
    Profunctor0: function() {
      return profunctorFn;
    }
  };

  // output/Data.Profunctor.Strong/index.js
  var identity11 = /* @__PURE__ */ identity(categoryFn);
  var strongFn = {
    first: function(a2b) {
      return function(v) {
        return new Tuple(a2b(v.value0), v.value1);
      };
    },
    second: /* @__PURE__ */ map(functorTuple),
    Profunctor0: function() {
      return profunctorFn;
    }
  };
  var second = function(dict) {
    return dict.second;
  };
  var first = function(dict) {
    return dict.first;
  };
  var splitStrong = function(dictCategory) {
    var composeFlipped2 = composeFlipped(dictCategory.Semigroupoid0());
    return function(dictStrong) {
      var first1 = first(dictStrong);
      var second1 = second(dictStrong);
      return function(l) {
        return function(r) {
          return composeFlipped2(first1(l))(second1(r));
        };
      };
    };
  };
  var fanout = function(dictCategory) {
    var identity1 = identity(dictCategory);
    var composeFlipped2 = composeFlipped(dictCategory.Semigroupoid0());
    var splitStrong1 = splitStrong(dictCategory);
    return function(dictStrong) {
      var dimap2 = dimap(dictStrong.Profunctor0());
      var splitStrong2 = splitStrong1(dictStrong);
      return function(l) {
        return function(r) {
          var split2 = dimap2(identity11)(function(a2) {
            return new Tuple(a2, a2);
          })(identity1);
          return composeFlipped2(split2)(splitStrong2(l)(r));
        };
      };
    };
  };

  // output/Data.Lens.Getter/index.js
  var unwrap5 = /* @__PURE__ */ unwrap();
  var identity12 = /* @__PURE__ */ identity(categoryFn);
  var view = function(l) {
    return unwrap5(l(identity12));
  };
  var viewOn = function(s) {
    return function(l) {
      return view(l)(s);
    };
  };
  var use = function(dictMonadState) {
    var gets2 = gets(dictMonadState);
    return function(p2) {
      return gets2(function(v) {
        return viewOn(v)(p2);
      });
    };
  };

  // output/Data.Lens.Lens/index.js
  var lens$prime = function(to2) {
    return function(dictStrong) {
      var dimap2 = dimap(dictStrong.Profunctor0());
      var first2 = first(dictStrong);
      return function(pab) {
        return dimap2(to2)(function(v) {
          return v.value1(v.value0);
        })(first2(pab));
      };
    };
  };
  var lens = function(get6) {
    return function(set3) {
      return function(dictStrong) {
        return lens$prime(function(s) {
          return new Tuple(get6(s), function(b2) {
            return set3(s)(b2);
          });
        })(dictStrong);
      };
    };
  };

  // output/Record/index.js
  var set = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(b2) {
            return function(r) {
              return unsafeSet(reflectSymbol2(l))(b2)(r);
            };
          };
        };
      };
    };
  };
  var get2 = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol2(l))(r);
        };
      };
    };
  };

  // output/Data.Lens.Record/index.js
  var prop3 = function(dictIsSymbol) {
    var get6 = get2(dictIsSymbol)();
    var set3 = set(dictIsSymbol)()();
    return function() {
      return function() {
        return function(l) {
          return function(dictStrong) {
            return lens(get6(l))(flip(set3(l)))(dictStrong);
          };
        };
      };
    };
  };

  // output/Data.Lens.Setter/index.js
  var over2 = function(l) {
    return l;
  };
  var set2 = function(l) {
    return function(b2) {
      return over2(l)($$const(b2));
    };
  };
  var modifying = function(dictMonadState) {
    var $$void10 = $$void(dictMonadState.Monad0().Bind1().Apply0().Functor0());
    var modify5 = modify(dictMonadState);
    return function(p2) {
      return function(f) {
        return $$void10(modify5(over2(p2)(f)));
      };
    };
  };
  var assign2 = function(dictMonadState) {
    var $$void10 = $$void(dictMonadState.Monad0().Bind1().Apply0().Functor0());
    var modify5 = modify(dictMonadState);
    return function(p2) {
      return function(b2) {
        return $$void10(modify5(set2(p2)(b2)));
      };
    };
  };

  // output/Effect.Class.Console/index.js
  var log3 = function(dictMonadEffect) {
    var $51 = liftEffect(dictMonadEffect);
    return function($52) {
      return $51(log2($52));
    };
  };

  // output/Effect.Random/foreign.js
  var random = Math.random;

  // output/Affjax/foreign.js
  function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options2) {
    return function(errback, callback) {
      var xhr = platformSpecificDriver.newXHR();
      var fixedUrl = platformSpecificDriver.fixupUrl(options2.url, xhr);
      xhr.open(options2.method || "GET", fixedUrl, true, options2.username, options2.password);
      if (options2.headers) {
        try {
          for (var i2 = 0, header3; (header3 = options2.headers[i2]) != null; i2++) {
            xhr.setRequestHeader(header3.field, header3.value);
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
          headers: xhr.getAllResponseHeaders().split("\r\n").filter(function(header4) {
            return header4.length > 0;
          }).map(function(header4) {
            var i3 = header4.indexOf(":");
            return mkHeader(header4.substring(0, i3))(header4.substring(i3 + 2));
          }),
          body: xhr.response
        });
      };
      xhr.responseType = options2.responseType;
      xhr.withCredentials = options2.withCredentials;
      xhr.timeout = options2.timeout;
      xhr.send(options2.content);
      return function(error6, cancelErrback, cancelCallback) {
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
  var ArrayView = /* @__PURE__ */ function() {
    function ArrayView2(value0) {
      this.value0 = value0;
    }
    ;
    ArrayView2.create = function(value0) {
      return new ArrayView2(value0);
    };
    return ArrayView2;
  }();
  var Blob = /* @__PURE__ */ function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  }();
  var Document = /* @__PURE__ */ function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  }();
  var $$String = /* @__PURE__ */ function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  }();
  var FormData = /* @__PURE__ */ function() {
    function FormData2(value0) {
      this.value0 = value0;
    }
    ;
    FormData2.create = function(value0) {
      return new FormData2(value0);
    };
    return FormData2;
  }();
  var FormURLEncoded = /* @__PURE__ */ function() {
    function FormURLEncoded2(value0) {
      this.value0 = value0;
    }
    ;
    FormURLEncoded2.create = function(value0) {
      return new FormURLEncoded2(value0);
    };
    return FormURLEncoded2;
  }();
  var Json = /* @__PURE__ */ function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  }();
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
  var unwrap6 = /* @__PURE__ */ unwrap();
  var Accept = /* @__PURE__ */ function() {
    function Accept2(value0) {
      this.value0 = value0;
    }
    ;
    Accept2.create = function(value0) {
      return new Accept2(value0);
    };
    return Accept2;
  }();
  var ContentType = /* @__PURE__ */ function() {
    function ContentType2(value0) {
      this.value0 = value0;
    }
    ;
    ContentType2.create = function(value0) {
      return new ContentType2(value0);
    };
    return ContentType2;
  }();
  var RequestHeader = /* @__PURE__ */ function() {
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
  }();
  var value13 = function(v) {
    if (v instanceof Accept) {
      return unwrap6(v.value0);
    }
    ;
    if (v instanceof ContentType) {
      return unwrap6(v.value0);
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value1;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
  };
  var name16 = function(v) {
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
  var identity13 = /* @__PURE__ */ identity(categoryFn);
  var $$ArrayBuffer = /* @__PURE__ */ function() {
    function $$ArrayBuffer2(value0) {
      this.value0 = value0;
    }
    ;
    $$ArrayBuffer2.create = function(value0) {
      return new $$ArrayBuffer2(value0);
    };
    return $$ArrayBuffer2;
  }();
  var Blob2 = /* @__PURE__ */ function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  }();
  var Document2 = /* @__PURE__ */ function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  }();
  var Json2 = /* @__PURE__ */ function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  }();
  var $$String2 = /* @__PURE__ */ function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  }();
  var Ignore = /* @__PURE__ */ function() {
    function Ignore2(value0) {
      this.value0 = value0;
    }
    ;
    Ignore2.create = function(value0) {
      return new Ignore2(value0);
    };
    return Ignore2;
  }();
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
  var string = /* @__PURE__ */ function() {
    return new $$String2(identity13);
  }();
  var ignore = /* @__PURE__ */ function() {
    return new Ignore(identity13);
  }();

  // output/Affjax.ResponseHeader/index.js
  var ResponseHeader = /* @__PURE__ */ function() {
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
  }();

  // output/Data.Argonaut.Core/foreign.js
  function id3(x10) {
    return x10;
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
  var encodeFormURLComponent = /* @__PURE__ */ function() {
    return runFn3(_encodeFormURLComponent)($$const(Nothing.value))(Just.create);
  }();

  // output/Data.FormURLEncoded/index.js
  var apply2 = /* @__PURE__ */ apply(applyMaybe);
  var map32 = /* @__PURE__ */ map(functorMaybe);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeMaybe);
  var toArray = function(v) {
    return v;
  };
  var encode2 = /* @__PURE__ */ function() {
    var encodePart = function(v) {
      if (v.value1 instanceof Nothing) {
        return encodeFormURLComponent(v.value0);
      }
      ;
      if (v.value1 instanceof Just) {
        return apply2(map32(function(key) {
          return function(val) {
            return key + ("=" + val);
          };
        })(encodeFormURLComponent(v.value0)))(encodeFormURLComponent(v.value1.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 16 - line 39, column 114): " + [v.constructor.name]);
    };
    var $37 = map32(joinWith("&"));
    var $38 = traverse2(encodePart);
    return function($39) {
      return $37($38(toArray($39)));
    };
  }();

  // output/Data.HTTP.Method/index.js
  var OPTIONS = /* @__PURE__ */ function() {
    function OPTIONS2() {
    }
    ;
    OPTIONS2.value = new OPTIONS2();
    return OPTIONS2;
  }();
  var GET2 = /* @__PURE__ */ function() {
    function GET3() {
    }
    ;
    GET3.value = new GET3();
    return GET3;
  }();
  var HEAD = /* @__PURE__ */ function() {
    function HEAD2() {
    }
    ;
    HEAD2.value = new HEAD2();
    return HEAD2;
  }();
  var POST2 = /* @__PURE__ */ function() {
    function POST3() {
    }
    ;
    POST3.value = new POST3();
    return POST3;
  }();
  var PUT = /* @__PURE__ */ function() {
    function PUT2() {
    }
    ;
    PUT2.value = new PUT2();
    return PUT2;
  }();
  var DELETE = /* @__PURE__ */ function() {
    function DELETE2() {
    }
    ;
    DELETE2.value = new DELETE2();
    return DELETE2;
  }();
  var TRACE = /* @__PURE__ */ function() {
    function TRACE2() {
    }
    ;
    TRACE2.value = new TRACE2();
    return TRACE2;
  }();
  var CONNECT = /* @__PURE__ */ function() {
    function CONNECT2() {
    }
    ;
    CONNECT2.value = new CONNECT2();
    return CONNECT2;
  }();
  var PROPFIND = /* @__PURE__ */ function() {
    function PROPFIND2() {
    }
    ;
    PROPFIND2.value = new PROPFIND2();
    return PROPFIND2;
  }();
  var PROPPATCH = /* @__PURE__ */ function() {
    function PROPPATCH2() {
    }
    ;
    PROPPATCH2.value = new PROPPATCH2();
    return PROPPATCH2;
  }();
  var MKCOL = /* @__PURE__ */ function() {
    function MKCOL2() {
    }
    ;
    MKCOL2.value = new MKCOL2();
    return MKCOL2;
  }();
  var COPY = /* @__PURE__ */ function() {
    function COPY2() {
    }
    ;
    COPY2.value = new COPY2();
    return COPY2;
  }();
  var MOVE = /* @__PURE__ */ function() {
    function MOVE2() {
    }
    ;
    MOVE2.value = new MOVE2();
    return MOVE2;
  }();
  var LOCK = /* @__PURE__ */ function() {
    function LOCK2() {
    }
    ;
    LOCK2.value = new LOCK2();
    return LOCK2;
  }();
  var UNLOCK = /* @__PURE__ */ function() {
    function UNLOCK2() {
    }
    ;
    UNLOCK2.value = new UNLOCK2();
    return UNLOCK2;
  }();
  var PATCH = /* @__PURE__ */ function() {
    function PATCH2() {
    }
    ;
    PATCH2.value = new PATCH2();
    return PATCH2;
  }();
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
  var pure11 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var fail2 = /* @__PURE__ */ fail(monadIdentity);
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var alt5 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var composeKleisliFlipped4 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var map33 = /* @__PURE__ */ map(functorMaybe);
  var any3 = /* @__PURE__ */ any(foldableArray)(heytingAlgebraBoolean);
  var eq3 = /* @__PURE__ */ eq(eqString);
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var map110 = /* @__PURE__ */ map(functorArray);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorAff);
  var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
  var pure13 = /* @__PURE__ */ pure(applicativeAff);
  var RequestContentError = /* @__PURE__ */ function() {
    function RequestContentError2(value0) {
      this.value0 = value0;
    }
    ;
    RequestContentError2.create = function(value0) {
      return new RequestContentError2(value0);
    };
    return RequestContentError2;
  }();
  var ResponseBodyError = /* @__PURE__ */ function() {
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
  }();
  var TimeoutError = /* @__PURE__ */ function() {
    function TimeoutError2() {
    }
    ;
    TimeoutError2.value = new TimeoutError2();
    return TimeoutError2;
  }();
  var RequestFailedError = /* @__PURE__ */ function() {
    function RequestFailedError2() {
    }
    ;
    RequestFailedError2.value = new RequestFailedError2();
    return RequestFailedError2;
  }();
  var XHROtherError = /* @__PURE__ */ function() {
    function XHROtherError2(value0) {
      this.value0 = value0;
    }
    ;
    XHROtherError2.create = function(value0) {
      return new XHROtherError2(value0);
    };
    return XHROtherError2;
  }();
  var request2 = function(driver2) {
    return function(req2) {
      var parseJSON = function(v2) {
        if (v2 === "") {
          return pure11(jsonEmptyObject);
        }
        ;
        return either(function($74) {
          return fail2(ForeignError.create($74));
        })(pure11)(jsonParser(v2));
      };
      var fromResponse = function() {
        if (req2.responseFormat instanceof $$ArrayBuffer) {
          return unsafeReadTagged2("ArrayBuffer");
        }
        ;
        if (req2.responseFormat instanceof Blob2) {
          return unsafeReadTagged2("Blob");
        }
        ;
        if (req2.responseFormat instanceof Document2) {
          return function(x10) {
            return alt5(unsafeReadTagged2("Document")(x10))(alt5(unsafeReadTagged2("XMLDocument")(x10))(unsafeReadTagged2("HTMLDocument")(x10)));
          };
        }
        ;
        if (req2.responseFormat instanceof Json2) {
          return composeKleisliFlipped4(function($75) {
            return req2.responseFormat.value0(parseJSON($75));
          })(unsafeReadTagged2("String"));
        }
        ;
        if (req2.responseFormat instanceof $$String2) {
          return unsafeReadTagged2("String");
        }
        ;
        if (req2.responseFormat instanceof Ignore) {
          return $$const(req2.responseFormat.value0(pure11(unit)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 274, column 18 - line 283, column 57): " + [req2.responseFormat.constructor.name]);
      }();
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
          return note("Body contains values that cannot be encoded as application/x-www-form-urlencoded")(map33(unsafeToForeign)(encode2(v2.value0)));
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
          if (mh instanceof Just && !any3(on(eq3)(name16)(mh.value0))(hs)) {
            return snoc(hs)(mh.value0);
          }
          ;
          return hs;
        };
      };
      var headers = function(reqContent) {
        return addHeader(map33(ContentType.create)(bindFlipped8(toMediaType)(reqContent)))(addHeader(map33(Accept.create)(toMediaType2(req2.responseFormat)))(req2.headers));
      };
      var ajaxRequest = function(v2) {
        return {
          method: print6(req2.method),
          url: req2.url,
          headers: map110(function(h) {
            return {
              field: name16(h),
              value: value13(h)
            };
          })(headers(req2.content)),
          content: v2,
          responseType: toResponseType(req2.responseFormat),
          username: toNullable(req2.username),
          password: toNullable(req2.password),
          withCredentials: req2.withCredentials,
          timeout: fromMaybe(0)(map33(function(v1) {
            return v1;
          })(req2.timeout))
        };
      };
      var send = function(content4) {
        return mapFlipped2($$try3(fromEffectFnAff(_ajax(driver2, "AffjaxTimeoutErrorMessageIdent", "AffjaxRequestFailedMessageIdent", ResponseHeader.create, ajaxRequest(content4)))))(function(v2) {
          if (v2 instanceof Right) {
            var v1 = runExcept(fromResponse(v2.value0.body));
            if (v1 instanceof Left) {
              return new Left(new ResponseBodyError(head3(v1.value0), v2.value0));
            }
            ;
            if (v1 instanceof Right) {
              return new Right({
                body: v1.value0,
                headers: v2.value0.headers,
                status: v2.value0.status,
                statusText: v2.value0.statusText
              });
            }
            ;
            throw new Error("Failed pattern match at Affjax (line 209, column 9 - line 211, column 52): " + [v1.constructor.name]);
          }
          ;
          if (v2 instanceof Left) {
            return new Left(function() {
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
            }());
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
  var defaultRequest = /* @__PURE__ */ function() {
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
  }();
  var get3 = function(driver2) {
    return function(rf) {
      return function(u2) {
        return request2(driver2)({
          method: defaultRequest.method,
          url: u2,
          headers: defaultRequest.headers,
          content: defaultRequest.content,
          username: defaultRequest.username,
          password: defaultRequest.password,
          withCredentials: defaultRequest.withCredentials,
          responseFormat: rf,
          timeout: defaultRequest.timeout
        });
      };
    };
  };

  // output/Affjax.Web/foreign.js
  var driver = {
    newXHR: function() {
      return new XMLHttpRequest();
    },
    fixupUrl: function(url) {
      return url || "/";
    }
  };

  // output/Affjax.Web/index.js
  var get4 = /* @__PURE__ */ get3(driver);

  // output/Html.Parser/foreign.js
  function getAttributes(node) {
    var entries = [];
    for (var i2 = 0; i2 < node.attributes.length; i2++) {
      let { name: name17, value: value15 } = node.attributes.item(i2);
      entries.push([name17, value15]);
    }
    return entries;
  }
  function walk(treeWalker) {
    var nodes = [];
    function handleNode(node) {
      if (["#comment", "#text"].includes(node.nodeName)) {
        var text11 = node.textContent;
        if (text11) {
          nodes.push({
            type: node.nodeName.slice(1),
            text: text11
          });
        }
      } else {
        var children2 = walk(treeWalker);
        treeWalker.currentNode = node;
        nodes.push({
          type: "element",
          name: node.localName,
          attributes: getAttributes(node),
          children: children2
        });
      }
    }
    var currentNode = treeWalker.currentNode;
    var firstChild = treeWalker.firstChild();
    if (firstChild) {
      handleNode(firstChild);
    } else {
      return nodes;
    }
    var nextSibling2 = treeWalker.nextSibling();
    while (nextSibling2) {
      handleNode(nextSibling2);
      treeWalker.currentNode = nextSibling2;
      nextSibling2 = treeWalker.nextSibling();
    }
    return nodes;
  }
  function parseFromString(elementCtor) {
    return (attributeCtor) => (textCtor) => (commentCtor) => (input3) => {
      function mapNode(node) {
        if (node.type == "element") {
          return elementCtor({
            name: node.name,
            attributes: node.attributes.map(([k, v]) => attributeCtor(k)(v)),
            children: node.children.map(mapNode)
          });
        } else {
          var ctor = node.type == "text" ? textCtor : commentCtor;
          return ctor(node.text);
        }
      }
      var doc = new DOMParser().parseFromString(input3, "text/html");
      var headNodes = walk(doc.createTreeWalker(doc.documentElement.querySelector("head")));
      var bodyNodes = walk(doc.createTreeWalker(doc.documentElement.querySelector("body")));
      return [...headNodes, ...bodyNodes].map((node) => {
        if (node.type == "element") {
          return mapNode(node);
        } else {
          var ctor = node.type == "text" ? textCtor : commentCtor;
          return ctor(node.text);
        }
      });
    };
  }

  // output/Html.Parser/index.js
  var HtmlAttribute = /* @__PURE__ */ function() {
    function HtmlAttribute2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    HtmlAttribute2.create = function(value0) {
      return function(value1) {
        return new HtmlAttribute2(value0, value1);
      };
    };
    return HtmlAttribute2;
  }();
  var HtmlElement = /* @__PURE__ */ function() {
    function HtmlElement2(value0) {
      this.value0 = value0;
    }
    ;
    HtmlElement2.create = function(value0) {
      return new HtmlElement2(value0);
    };
    return HtmlElement2;
  }();
  var HtmlText = /* @__PURE__ */ function() {
    function HtmlText2(value0) {
      this.value0 = value0;
    }
    ;
    HtmlText2.create = function(value0) {
      return new HtmlText2(value0);
    };
    return HtmlText2;
  }();
  var HtmlComment = /* @__PURE__ */ function() {
    function HtmlComment2(value0) {
      this.value0 = value0;
    }
    ;
    HtmlComment2.create = function(value0) {
      return new HtmlComment2(value0);
    };
    return HtmlComment2;
  }();
  var parse6 = function(input3) {
    return parseFromString(HtmlElement.create)(HtmlAttribute.create)(HtmlText.create)(HtmlComment.create)(input3);
  };

  // output/Html.Renderer.Halogen/index.js
  var mapFlipped3 = /* @__PURE__ */ mapFlipped(functorMaybe);
  var alt6 = /* @__PURE__ */ alt(altMaybe);
  var mapFlipped1 = /* @__PURE__ */ mapFlipped(functorArray);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable3(foldableArray);
  var map34 = /* @__PURE__ */ map(functorArray);
  var htmlAttributeToProp = function(v) {
    return attr2(v.value0)(v.value1);
  };
  var nodeToHtml = function(v) {
    return function(v1) {
      if (v1 instanceof HtmlElement) {
        return elementToHtml(v)(v1.value0);
      }
      ;
      if (v1 instanceof HtmlText) {
        return text5(v1.value0);
      }
      ;
      if (v1 instanceof HtmlComment) {
        return text5("");
      }
      ;
      throw new Error("Failed pattern match at Html.Renderer.Halogen (line 38, column 1 - line 38, column 72): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var elementToHtml = function(mParentNs) {
    return function(ele) {
      var mCurNs = mapFlipped3(find2(function(v) {
        return v.value0 === "xmlns";
      })(ele.attributes))(function(v) {
        return v.value1;
      });
      var mNs = alt6(mCurNs)(mParentNs);
      var ctor = function() {
        if (mNs instanceof Just) {
          return elementNS(mNs.value0);
        }
        ;
        if (mNs instanceof Nothing) {
          return element2;
        }
        ;
        throw new Error("Failed pattern match at Html.Renderer.Halogen (line 34, column 12 - line 36, column 28): " + [mNs.constructor.name]);
      }();
      var children2 = mapFlipped1(ele.children)(nodeToHtml(mNs));
      return ctor(ele.name)(fromFoldable5(map34(htmlAttributeToProp)(ele.attributes)))(children2);
    };
  };
  var renderToArray = function(raw) {
    return map34(nodeToHtml(Nothing.value))(parse6(raw));
  };
  var render = function(props) {
    var $28 = div2(props);
    return function($29) {
      return $28(renderToArray($29));
    };
  };
  var render_ = /* @__PURE__ */ render([]);

  // output/Stories.Utilities/foreign.js
  function highlightString_(codetext) {
    const highlightedCode = Prism.highlight(codetext, Prism.languages.purescript, "purescript");
    return highlightedCode;
  }

  // output/Stories.Utilities/index.js
  var syntaxHighlightedCode = function(codetext) {
    return [pre([class_("language-purescript")])([code_([render_(highlightString_(codetext))])])];
  };
  var classed3 = function($3) {
    return class_(ClassName($3));
  };
  var tailwindClass = classed3;
  var blurbParagraphs = function(dictFunctor) {
    var map55 = map(dictFunctor);
    return function(texts) {
      return map55(p([classes(["m-2"])]))(map55(function($4) {
        return singleton4(text5($4));
      })(texts));
    };
  };

  // output/Snippets/index.js
  var map35 = /* @__PURE__ */ map(functorArray);
  var bind5 = /* @__PURE__ */ bind(bindAff);
  var spy3 = /* @__PURE__ */ spy();
  var pure14 = /* @__PURE__ */ pure(applicativeAff);
  var Blurb = /* @__PURE__ */ function() {
    function Blurb2(value0) {
      this.value0 = value0;
    }
    ;
    Blurb2.create = function(value0) {
      return new Blurb2(value0);
    };
    return Blurb2;
  }();
  var SnippetFile = /* @__PURE__ */ function() {
    function SnippetFile2(value0) {
      this.value0 = value0;
    }
    ;
    SnippetFile2.create = function(value0) {
      return new SnippetFile2(value0);
    };
    return SnippetFile2;
  }();
  var Snippet = /* @__PURE__ */ function() {
    function Snippet2(value0) {
      this.value0 = value0;
    }
    ;
    Snippet2.create = function(value0) {
      return new Snippet2(value0);
    };
    return Snippet2;
  }();
  var PreRendered = /* @__PURE__ */ function() {
    function PreRendered2(value0) {
      this.value0 = value0;
    }
    ;
    PreRendered2.create = function(value0) {
      return new PreRendered2(value0);
    };
    return PreRendered2;
  }();
  var RenderWithState = /* @__PURE__ */ function() {
    function RenderWithState2(value0) {
      this.value0 = value0;
    }
    ;
    RenderWithState2.create = function(value0) {
      return new RenderWithState2(value0);
    };
    return RenderWithState2;
  }();
  var renderCell = function(v) {
    return function(v1) {
      if (v1 instanceof Blurb) {
        return p([classes(["m-2"])])([text5(v1.value0)]);
      }
      ;
      if (v1 instanceof Snippet) {
        return pre([class_(v1.value0.language)])([code_([render_(highlightString_(v1.value0.text))])]);
      }
      ;
      if (v1 instanceof SnippetFile) {
        return p([classes(["m-2"])])([text5("Snippet file not loaded: " + (v1.value0 + " Did you remember to call substituteSnippetCells on your Notebook?"))]);
      }
      ;
      if (v1 instanceof PreRendered) {
        return v1.value0;
      }
      ;
      if (v1 instanceof RenderWithState) {
        return v1.value0(v);
      }
      ;
      throw new Error("Failed pattern match at Snippets (line 34, column 1 - line 34, column 71): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var renderNotebook = function(state3) {
    return function(notebook) {
      return map35(renderCell(state3))(notebook);
    };
  };
  var renderNotebook_ = function(notebook) {
    return map35(renderCell(unit))(notebook);
  };
  var readSnippetFiles = function(name17) {
    return bind5(get4(string)("./code-examples/" + name17))(function(response) {
      if (response instanceof Left) {
        return spy3("couldn't read snippet, error: ")(pure14(printError(response.value0)));
      }
      ;
      if (response instanceof Right) {
        return spy3("read snippet: ")(pure14(response.value0.body));
      }
      ;
      throw new Error("Failed pattern match at Snippets (line 60, column 3 - line 62, column 52): " + [response.constructor.name]);
    });
  };
  var substituteSnippetCells = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(dictMonadAff) {
      var liftAff2 = liftAff(dictMonadAff);
      var pure111 = pure(dictMonadAff.MonadEffect0().Monad0().Applicative0());
      return function(dictMonadState) {
        return function(v) {
          if (v instanceof SnippetFile) {
            return bind18(liftAff2(readSnippetFiles(v.value0)))(function(snippetText) {
              return pure111(new Snippet({
                file: v.value0,
                text: snippetText,
                language: "language-purescript"
              }));
            });
          }
          ;
          return pure111(v);
        };
      };
    };
  };

  // output/Stories.GUP/index.js
  var pure15 = /* @__PURE__ */ pure(applicativeEffect);
  var sequence2 = /* @__PURE__ */ sequence(traversableArray)(applicativeEffect);
  var map36 = /* @__PURE__ */ map(functorArray);
  var bind15 = /* @__PURE__ */ bind(bindAff);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard7 = /* @__PURE__ */ discard(discardUnit);
  var discard12 = /* @__PURE__ */ discard7(bindAff);
  var exGeneralUpdatePattern2 = /* @__PURE__ */ exGeneralUpdatePattern(d3TaglessD3M);
  var applySecond2 = /* @__PURE__ */ applySecond(applyEffect);
  var forever2 = /* @__PURE__ */ forever(monadRecAff);
  var prop5 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "handler";
    }
  })()();
  var prop12 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "draw";
    }
  })()();
  var not5 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var prop23 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "code";
    }
  })()();
  var prop32 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "blurb";
    }
  })()();
  var Running = /* @__PURE__ */ function() {
    function Running2() {
    }
    ;
    Running2.value = new Running2();
    return Running2;
  }();
  var Paused = /* @__PURE__ */ function() {
    function Paused2() {
    }
    ;
    Paused2.value = new Paused2();
    return Paused2;
  }();
  var Initialize2 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var SetStatus = /* @__PURE__ */ function() {
    function SetStatus2(value0) {
      this.value0 = value0;
    }
    ;
    SetStatus2.create = function(value0) {
      return new SetStatus2(value0);
    };
    return SetStatus2;
  }();
  var ToggleStatus = /* @__PURE__ */ function() {
    function ToggleStatus2() {
    }
    ;
    ToggleStatus2.value = new ToggleStatus2();
    return ToggleStatus2;
  }();
  var Finalize2 = /* @__PURE__ */ function() {
    function Finalize6() {
    }
    ;
    Finalize6.value = new Finalize6();
    return Finalize6;
  }();
  var ToggleCard = /* @__PURE__ */ function() {
    function ToggleCard7(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard7.create = function(value0) {
      return new ToggleCard7(value0);
    };
    return ToggleCard7;
  }();
  var showStatus = {
    show: function(v) {
      if (v instanceof Running) {
        return "Running";
      }
      ;
      if (v instanceof Paused) {
        return "Paused";
      }
      ;
      throw new Error("Failed pattern match at Stories.GUP (line 46, column 1 - line 48, column 26): " + [v.constructor.name]);
    }
  };
  var show7 = /* @__PURE__ */ show(showStatus);
  var runUpdate = function(update3) {
    var getLetters = function() {
      var letters = toCharArray("abcdefghijklmnopqrstuvwxyz");
      var coinToss = function(c) {
        return function __do3() {
          var n = random();
          var $135 = n > 0.6;
          if ($135) {
            return new Just(c);
          }
          ;
          return Nothing.value;
        };
      };
      return function __do3() {
        var choices = sequence2(map36(coinToss)(letters))();
        return catMaybes(choices);
      };
    }();
    return bind15(liftEffect7(getLetters))(function(letters) {
      return discard12(update3(letters))(function() {
        return delay(2300);
      });
    });
  };
  var runGeneralUpdatePattern = function(dictBind) {
    var discard27 = discard7(dictBind);
    var bind23 = bind(dictBind);
    return function(dictMonadEffect) {
      var liftEffect12 = liftEffect(dictMonadEffect);
      var pure111 = pure(dictMonadEffect.Monad0().Applicative0());
      return discard27(log3(dictMonadEffect)("General Update Pattern example"))(function() {
        return bind23(liftEffect12(eval_D3M(exGeneralUpdatePattern2("div.svg-container"))))(function(update3) {
          return pure111(function(letters) {
            return liftEffect7(applySecond2(runD3M(update3(letters)))(pure15(unit)));
          });
        });
      });
    };
  };
  var pauseUpdating = function(dictBind) {
    var bind23 = bind(dictBind);
    return function(dictMonadState) {
      var gets2 = gets(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadAff) {
        var pure111 = pure(dictMonadAff.MonadEffect0().Monad0().Applicative0());
        var liftAff2 = liftAff(dictMonadAff);
        return bind23(gets2(function(v) {
          return v.fiber;
        }))(function(maybeFiber) {
          return bind23(function() {
            if (maybeFiber instanceof Nothing) {
              return pure111(unit);
            }
            ;
            if (maybeFiber instanceof Just) {
              return liftAff2(killFiber(error("Cancel fiber to suspend computation"))(maybeFiber.value0));
            }
            ;
            throw new Error("Failed pattern match at Stories.GUP (line 211, column 8 - line 213, column 100): " + [maybeFiber.constructor.name]);
          }())(function() {
            return modify_6(function(state3) {
              var $138 = {};
              for (var $139 in state3) {
                if ({}.hasOwnProperty.call(state3, $139)) {
                  $138[$139] = state3[$139];
                }
                ;
              }
              ;
              $138.status = Paused.value;
              $138.fiber = Nothing.value;
              return $138;
            });
          });
        });
      };
    };
  };
  var eqStatus = {
    eq: function(x10) {
      return function(y8) {
        if (x10 instanceof Running && y8 instanceof Running) {
          return true;
        }
        ;
        if (x10 instanceof Paused && y8 instanceof Paused) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var notEq2 = /* @__PURE__ */ notEq(eqStatus);
  var startUpdating = function(dictBind) {
    var bind23 = bind(dictBind);
    return function(dictMonadState) {
      var get6 = get(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadAff) {
        var pure111 = pure(dictMonadAff.MonadEffect0().Monad0().Applicative0());
        var liftAff2 = liftAff(dictMonadAff);
        return bind23(get6)(function(v) {
          var $144 = notEq2(v.status)(Paused.value);
          if ($144) {
            return pure111(unit);
          }
          ;
          if (v.update instanceof Nothing) {
            return pure111(unit);
          }
          ;
          if (v.update instanceof Just) {
            return bind23(liftAff2(forkAff(forever2(runUpdate(v.update.value0)))))(function(fiber) {
              return modify_6(function(state3) {
                var $146 = {};
                for (var $147 in state3) {
                  if ({}.hasOwnProperty.call(state3, $147)) {
                    $146[$147] = state3[$147];
                  }
                  ;
                }
                ;
                $146.status = Running.value;
                $146.fiber = new Just(fiber);
                return $146;
              });
            });
          }
          ;
          throw new Error("Failed pattern match at Stories.GUP (line 226, column 5 - line 230, column 77): " + [v.update.constructor.name]);
        });
      };
    };
  };
  var blurbtext = /* @__PURE__ */ blurbParagraphs(functorArray)(["This deceptively simple example shows off an aspect of screen-based data\nvisualization that has no analogue in paper visualizations: the ability to\nspecify how updates to the data should be represented.", "In this example, some letters of the alphabet are presented and then constantly\nupdated. When a letter enters at first, it falls in from the top and it is\ngreen. If its still present in the next set of letters it stays on the screen,\nbut it turns gray and moves to an alphabetically correct new position. And if\nits not present in the new data, it turns red and falls out before\ndisappearing.", "In a more meaningful example, ie with some data that you actually care about,\nthis helps give continuity, as the eye can track an individual letter thru its\narrival, update and exit phases. Even if this tracking isn't interesting in\nitself, it can lessen the fatigue of looking at updated data and it conveys a\nsense of how much the data has changed.", 'This example is called "General Update Pattern" in an early Mike Bostock\npost explaining the (then new) D3.js. You can see in the code panel how the\n"data join" contains three separate specifications, each with their own\n*transition*.']);
  var _snippets = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  }();
  var _panels = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  }();
  var _handlerCode = function(dictStrong) {
    var $167 = _snippets(dictStrong);
    var $168 = prop5($$Proxy.value)(dictStrong);
    return function($169) {
      return $167($168($169));
    };
  };
  var _handlerCode1 = /* @__PURE__ */ _handlerCode(strongFn);
  var _drawCode = function(dictStrong) {
    var $170 = _snippets(dictStrong);
    var $171 = prop12($$Proxy.value)(dictStrong);
    return function($172) {
      return $170($171($172));
    };
  };
  var _drawCode1 = /* @__PURE__ */ _drawCode(strongFn);
  var _drawCode2 = /* @__PURE__ */ _drawCode(strongForget);
  var handleAction = function(dictBind) {
    var bind23 = bind(dictBind);
    var discard27 = discard7(dictBind);
    var runGeneralUpdatePattern1 = runGeneralUpdatePattern(dictBind);
    var pauseUpdating1 = pauseUpdating(dictBind);
    var startUpdating1 = startUpdating(dictBind);
    return function(dictMonadAff) {
      var liftAff2 = liftAff(dictMonadAff);
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var runGeneralUpdatePattern2 = runGeneralUpdatePattern1(MonadEffect0);
      var pure111 = pure(MonadEffect0.Monad0().Applicative0());
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var assign4 = assign2(dictMonadState);
        var modify_6 = modify_(dictMonadState);
        var gets2 = gets(dictMonadState);
        var pauseUpdating2 = pauseUpdating1(dictMonadState)(dictMonadAff);
        var startUpdating2 = startUpdating1(dictMonadState)(dictMonadAff);
        return function(v) {
          if (v instanceof ToggleCard) {
            return modifying3(v.value0(strongFn))(not5);
          }
          ;
          if (v instanceof Initialize2) {
            return bind23(liftAff2(readSnippetFiles("GUP")))(function(text1) {
              return discard27(assign4(_drawCode1)(text1))(function() {
                return bind23(liftAff2(readSnippetFiles("GUPHandleActions")))(function(text22) {
                  return discard27(assign4(_handlerCode1)(text22))(function() {
                    return bind23(runGeneralUpdatePattern2)(function(updateFn) {
                      return bind23(liftAff2(forkAff(forever2(runUpdate(updateFn)))))(function(fiber) {
                        return modify_6(function(state3) {
                          var $154 = {};
                          for (var $155 in state3) {
                            if ({}.hasOwnProperty.call(state3, $155)) {
                              $154[$155] = state3[$155];
                            }
                            ;
                          }
                          ;
                          $154.status = Running.value;
                          $154.fiber = new Just(fiber);
                          $154.update = new Just(updateFn);
                          return $154;
                        });
                      });
                    });
                  });
                });
              });
            });
          }
          ;
          if (v instanceof SetStatus) {
            return modify_6(function(state3) {
              var $157 = {};
              for (var $158 in state3) {
                if ({}.hasOwnProperty.call(state3, $158)) {
                  $157[$158] = state3[$158];
                }
                ;
              }
              ;
              $157.status = v.value0;
              return $157;
            });
          }
          ;
          if (v instanceof ToggleStatus) {
            return bind23(gets2(function(v1) {
              return v1.status;
            }))(function(currentStatus) {
              if (currentStatus instanceof Running) {
                return pauseUpdating2;
              }
              ;
              return startUpdating2;
            });
          }
          ;
          if (v instanceof Finalize2) {
            return bind23(gets2(function(v1) {
              return v1.fiber;
            }))(function(maybeFiber) {
              return bind23(function() {
                if (maybeFiber instanceof Nothing) {
                  return pure111(unit);
                }
                ;
                if (maybeFiber instanceof Just) {
                  return liftAff2(killFiber(error("Cancelling fiber and terminating computation"))(maybeFiber.value0));
                }
                ;
                throw new Error("Failed pattern match at Stories.GUP (line 197, column 10 - line 199, column 111): " + [maybeFiber.constructor.name]);
              }())(function() {
                return modify_6(function(state3) {
                  var $164 = {};
                  for (var $165 in state3) {
                    if ({}.hasOwnProperty.call(state3, $165)) {
                      $164[$165] = state3[$165];
                    }
                    ;
                  }
                  ;
                  $164.status = Paused.value;
                  $164.fiber = Nothing.value;
                  $164.update = Nothing.value;
                  return $164;
                });
              });
            });
          }
          ;
          throw new Error("Failed pattern match at Stories.GUP (line 173, column 16 - line 201, column 87): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction1 = /* @__PURE__ */ handleAction(bindHalogenM);
  var _code = function(dictStrong) {
    var $173 = _panels(dictStrong);
    var $174 = prop23($$Proxy.value)(dictStrong);
    return function($175) {
      return $173($174($175));
    };
  };
  var _code1 = /* @__PURE__ */ _code(strongForget);
  var _blurb = function(dictStrong) {
    var $176 = _panels(dictStrong);
    var $177 = prop32($$Proxy.value)(dictStrong);
    return function($178) {
      return $176($177($178));
    };
  };
  var _blurb1 = /* @__PURE__ */ _blurb(strongForget);
  var component = function(dictMonadAff) {
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-controls")])([buttonGroup([class_("flex-col")])([buttonVertical([onClick($$const(ToggleStatus.value))])([text5(show7(state3.status))])])]), div2([tailwindClass("story-panel-about")])([field_({
        label: text5("About"),
        helpText: [],
        error: [],
        inputId: "show-blurb"
      })([toggle([id2("show-blurb"), checked2(toBoolean(view(_blurb1)(state3))), onChange(function(v) {
        return new ToggleCard(function(dictStrong) {
          return _blurb(dictStrong);
        });
      })])]), content_(view(_blurb1)(state3))(blurbtext)]), div2([tailwindClass("story-panel-code")])([field_({
        label: text5("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked2(toBoolean(view(_code1)(state3))), onChange(function(v) {
        return new ToggleCard(function(dictStrong) {
          return _code(dictStrong);
        });
      })])]), content_(view(_code1)(state3))(syntaxHighlightedCode(view(_drawCode2)(state3)))]), div2([tailwindClass("svg-container")])([])]);
    };
    var initialState = {
      status: Paused.value,
      fiber: Nothing.value,
      update: Nothing.value,
      panels: {
        blurb: Expanded.value,
        code: Collapsed.value
      },
      snippets: {
        draw: "",
        handler: ""
      }
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleAction: handleAction1(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        handleQuery: defaultEval.handleQuery,
        receive: function($179) {
          return Just.create(SetStatus.create($179));
        },
        initialize: new Just(Initialize2.value),
        finalize: new Just(Finalize2.value)
      })
    });
  };

  // output/D3.Examples.LesMis.Unsafe/index.js
  var unboxD3SimNode = function(datum) {
    return datum;
  };
  var unboxD3SimLink = function(datum) {
    return datum;
  };

  // output/D3.Scales/foreign.js
  var d3SchemeCategory10 = d3.scaleOrdinal(d3.schemeCategory10);
  function d3SchemeCategory10N_(number) {
    return d3SchemeCategory10(number);
  }
  var d3SchemePaired = d3.scaleOrdinal(d3.schemePaired);
  function d3SchemePairedN_(number) {
    return d3SchemePaired(number);
  }
  var d3SchemeDiverging10 = d3.scaleDiverging(d3.interpolateBrBG).domain([0, 250, 500]);
  var d3SchemeSequential10 = d3.scaleSequential().interpolator(d3.interpolateYlOrRd).domain([0, 5, 10]);
  function d3SchemeSequential10N_(number) {
    return d3SchemeSequential10(number);
  }

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head7, tail2) {
      this.head = head7;
      this.tail = tail2;
    };
    function finalCell(head7) {
      return new ConsCell(head7, emptyList);
    }
    function consList(x10) {
      return function(xs) {
        return new ConsCell(x10, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply4) {
      return function(map55) {
        return function(f) {
          var buildFrom = function(x10, ys) {
            return apply4(map55(consList)(f(x10)))(ys);
          };
          var go2 = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go2(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map55(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map55(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.Array.NonEmpty.Internal/index.js
  var functorNonEmptyArray = functorArray;

  // output/Data.Array.NonEmpty/index.js
  var fromJust5 = /* @__PURE__ */ fromJust();
  var toArray2 = function(v) {
    return v;
  };
  var adaptMaybe = function(f) {
    return function($123) {
      return fromJust5(f(toArray2($123)));
    };
  };
  var head5 = /* @__PURE__ */ adaptMaybe(head2);

  // output/Data.Lens.AffineTraversal/index.js
  var identity14 = /* @__PURE__ */ identity(categoryFn);
  var fanout2 = /* @__PURE__ */ fanout(categoryFn)(strongFn);
  var affineTraversal$prime = function(to2) {
    return function(dictStrong) {
      var second2 = second(dictStrong);
      return function(dictChoice) {
        var dimap2 = dimap(dictChoice.Profunctor0());
        var right2 = right(dictChoice);
        return function(pab) {
          return dimap2(to2)(function(v) {
            return either(identity14)(v.value0)(v.value1);
          })(second2(right2(pab)));
        };
      };
    };
  };
  var affineTraversal = function(set3) {
    return function(pre2) {
      return function(dictStrong) {
        return function(dictChoice) {
          return affineTraversal$prime(fanout2(set3)(pre2))(dictStrong)(dictChoice);
        };
      };
    };
  };

  // output/Data.Lens.Iso/index.js
  var coerce3 = /* @__PURE__ */ coerce();
  var iso = function(f) {
    return function(g) {
      return function(dictProfunctor) {
        var dimap2 = dimap(dictProfunctor);
        return function(pab) {
          return dimap2(f)(g)(pab);
        };
      };
    };
  };
  var coerced = function() {
    return function() {
      return function(dictProfunctor) {
        return iso(coerce3)(coerce3)(dictProfunctor);
      };
    };
  };

  // output/Data.Lens.Iso.Newtype/index.js
  var coerced2 = /* @__PURE__ */ coerced()();
  var _Newtype = function() {
    return function() {
      return function(dictProfunctor) {
        return coerced2(dictProfunctor);
      };
    };
  };

  // output/Data.Lens.Prism/index.js
  var identity15 = /* @__PURE__ */ identity(categoryFn);
  var prism = function(to2) {
    return function(fro) {
      return function(dictChoice) {
        var Profunctor0 = dictChoice.Profunctor0();
        var dimap2 = dimap(Profunctor0);
        var right2 = right(dictChoice);
        var rmap5 = rmap(Profunctor0);
        return function(pab) {
          return dimap2(fro)(either(identity15)(identity15))(right2(rmap5(to2)(pab)));
        };
      };
    };
  };

  // output/Data.Lens.Prism.Maybe/index.js
  var _Just = function(dictChoice) {
    return prism(Just.create)(maybe(new Left(Nothing.value))(Right.create))(dictChoice);
  };

  // output/Data.Set/index.js
  var foldMap3 = /* @__PURE__ */ foldMap(foldableList);
  var foldl4 = /* @__PURE__ */ foldl(foldableList);
  var foldr4 = /* @__PURE__ */ foldr(foldableList);
  var toList2 = function(v) {
    return keys(v);
  };
  var insert6 = function(dictOrd) {
    var insert13 = insert(dictOrd);
    return function(a2) {
      return function(v) {
        return insert13(a2)(unit)(v);
      };
    };
  };
  var foldableSet = {
    foldMap: function(dictMonoid) {
      var foldMap12 = foldMap3(dictMonoid);
      return function(f) {
        var $129 = foldMap12(f);
        return function($130) {
          return $129(toList2($130));
        };
      };
    },
    foldl: function(f) {
      return function(x10) {
        var $131 = foldl4(f)(x10);
        return function($132) {
          return $131(toList2($132));
        };
      };
    },
    foldr: function(f) {
      return function(x10) {
        var $133 = foldr4(f)(x10);
        return function($134) {
          return $133(toList2($134));
        };
      };
    }
  };
  var empty7 = empty2;
  var fromFoldable6 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      var insert13 = insert6(dictOrd);
      return foldl22(function(m) {
        return function(a2) {
          return insert13(a2)(m);
        };
      })(empty7);
    };
  };

  // output/Data.Lens.Index/index.js
  var indexMap = function(dictOrd) {
    var update3 = update(dictOrd);
    var lookup14 = lookup(dictOrd);
    return {
      ix: function(k) {
        return function(dictStrong) {
          return function(dictChoice) {
            var set3 = function(s) {
              return function(b2) {
                return update3(function(v) {
                  return new Just(b2);
                })(k)(s);
              };
            };
            var pre2 = function(s) {
              return maybe(new Left(s))(Right.create)(lookup14(k)(s));
            };
            return affineTraversal(set3)(pre2)(dictStrong)(dictChoice);
          };
        };
      }
    };
  };

  // output/Data.Lens.At/index.js
  var atMap = function(dictOrd) {
    var lookup14 = lookup(dictOrd);
    var $$delete7 = $$delete(dictOrd);
    var insert11 = insert(dictOrd);
    var indexMap2 = indexMap(dictOrd);
    return {
      at: function(k) {
        return function(dictStrong) {
          return lens(lookup14(k))(function(m) {
            return maybe$prime(function(v) {
              return $$delete7(k)(m);
            })(function(v) {
              return insert11(k)(v)(m);
            });
          })(dictStrong);
        };
      },
      Index0: function() {
        return indexMap2;
      }
    };
  };
  var at = function(dict) {
    return dict.at;
  };

  // output/D3.Simulation.Types/index.js
  var fromFoldable7 = /* @__PURE__ */ fromFoldable2(ordString);
  var union3 = /* @__PURE__ */ union(ordString);
  var map37 = /* @__PURE__ */ map(functorMap);
  var trace2 = /* @__PURE__ */ trace();
  var _Newtype2 = /* @__PURE__ */ _Newtype()();
  var prop6 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "velocityDecay";
    }
  })()();
  var prop33 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "status";
    }
  })()();
  var prop42 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "name";
    }
  })()();
  var spy4 = /* @__PURE__ */ spy();
  var fromFoldable1 = /* @__PURE__ */ fromFoldable7(foldableMap);
  var prop11 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "simulation";
    }
  })()();
  var prop122 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "forceLibrary";
    }
  })()();
  var prop13 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "handle_";
    }
  })()();
  var prop15 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "alphaTarget";
    }
  })()();
  var prop16 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "alphaMin";
    }
  })()();
  var prop17 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "alphaDecay";
    }
  })()();
  var prop18 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "alpha";
    }
  })()();
  var Step3 = /* @__PURE__ */ function() {
    function Step4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Step4.create = function(value0) {
      return function(value1) {
        return new Step4(value0, value1);
      };
    };
    return Step4;
  }();
  var StepTransformFFI = /* @__PURE__ */ function() {
    function StepTransformFFI2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    StepTransformFFI2.create = function(value0) {
      return function(value1) {
        return new StepTransformFFI2(value0, value1);
      };
    };
    return StepTransformFFI2;
  }();
  var Alpha = /* @__PURE__ */ function() {
    function Alpha2(value0) {
      this.value0 = value0;
    }
    ;
    Alpha2.create = function(value0) {
      return new Alpha2(value0);
    };
    return Alpha2;
  }();
  var AlphaTarget = /* @__PURE__ */ function() {
    function AlphaTarget2(value0) {
      this.value0 = value0;
    }
    ;
    AlphaTarget2.create = function(value0) {
      return new AlphaTarget2(value0);
    };
    return AlphaTarget2;
  }();
  var AlphaMin = /* @__PURE__ */ function() {
    function AlphaMin2(value0) {
      this.value0 = value0;
    }
    ;
    AlphaMin2.create = function(value0) {
      return new AlphaMin2(value0);
    };
    return AlphaMin2;
  }();
  var AlphaDecay = /* @__PURE__ */ function() {
    function AlphaDecay2(value0) {
      this.value0 = value0;
    }
    ;
    AlphaDecay2.create = function(value0) {
      return new AlphaDecay2(value0);
    };
    return AlphaDecay2;
  }();
  var VelocityDecay = /* @__PURE__ */ function() {
    function VelocityDecay2(value0) {
      this.value0 = value0;
    }
    ;
    VelocityDecay2.create = function(value0) {
      return new VelocityDecay2(value0);
    };
    return VelocityDecay2;
  }();
  var ForceManyBody = /* @__PURE__ */ function() {
    function ForceManyBody2() {
    }
    ;
    ForceManyBody2.value = new ForceManyBody2();
    return ForceManyBody2;
  }();
  var ForceCenter = /* @__PURE__ */ function() {
    function ForceCenter2() {
    }
    ;
    ForceCenter2.value = new ForceCenter2();
    return ForceCenter2;
  }();
  var ForceCollide = /* @__PURE__ */ function() {
    function ForceCollide2() {
    }
    ;
    ForceCollide2.value = new ForceCollide2();
    return ForceCollide2;
  }();
  var ForceX = /* @__PURE__ */ function() {
    function ForceX2() {
    }
    ;
    ForceX2.value = new ForceX2();
    return ForceX2;
  }();
  var ForceY = /* @__PURE__ */ function() {
    function ForceY2() {
    }
    ;
    ForceY2.value = new ForceY2();
    return ForceY2;
  }();
  var ForceRadial = /* @__PURE__ */ function() {
    function ForceRadial2() {
    }
    ;
    ForceRadial2.value = new ForceRadial2();
    return ForceRadial2;
  }();
  var RegularForce = /* @__PURE__ */ function() {
    function RegularForce2(value0) {
      this.value0 = value0;
    }
    ;
    RegularForce2.create = function(value0) {
      return new RegularForce2(value0);
    };
    return RegularForce2;
  }();
  var LinkForce = /* @__PURE__ */ function() {
    function LinkForce2() {
    }
    ;
    LinkForce2.value = new LinkForce2();
    return LinkForce2;
  }();
  var ForceActive = /* @__PURE__ */ function() {
    function ForceActive2() {
    }
    ;
    ForceActive2.value = new ForceActive2();
    return ForceActive2;
  }();
  var ForceDisabled = /* @__PURE__ */ function() {
    function ForceDisabled2() {
    }
    ;
    ForceDisabled2.value = new ForceDisabled2();
    return ForceDisabled2;
  }();
  var ForceFilter = /* @__PURE__ */ function() {
    function ForceFilter2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ForceFilter2.create = function(value0) {
      return function(value1) {
        return new ForceFilter2(value0, value1);
      };
    };
    return ForceFilter2;
  }();
  var ForceT = function(x10) {
    return x10;
  };
  var showRegularForceType = {
    show: function(v) {
      if (v instanceof ForceManyBody) {
        return "ForceManyBody";
      }
      ;
      if (v instanceof ForceCenter) {
        return "ForceCenter";
      }
      ;
      if (v instanceof ForceCollide) {
        return "ForceCollide";
      }
      ;
      if (v instanceof ForceX) {
        return "ForceX";
      }
      ;
      if (v instanceof ForceY) {
        return "ForceY";
      }
      ;
      if (v instanceof ForceRadial) {
        return "ForceRadial";
      }
      ;
      throw new Error("Failed pattern match at D3.Simulation.Types (line 221, column 1 - line 227, column 46): " + [v.constructor.name]);
    }
  };
  var toggleForceStatus = function(v) {
    if (v instanceof ForceActive) {
      return ForceDisabled.value;
    }
    ;
    if (v instanceof ForceDisabled) {
      return ForceActive.value;
    }
    ;
    throw new Error("Failed pattern match at D3.Simulation.Types (line 185, column 1 - line 185, column 48): " + [v.constructor.name]);
  };
  var showForceStatus = {
    show: function(v) {
      if (v instanceof ForceActive) {
        return "active";
      }
      ;
      if (v instanceof ForceDisabled) {
        return "inactive";
      }
      ;
      throw new Error("Failed pattern match at D3.Simulation.Types (line 181, column 1 - line 183, column 34): " + [v.constructor.name]);
    }
  };
  var show52 = /* @__PURE__ */ show(showForceStatus);
  var showMaybeForceStatus = function(v) {
    if (v instanceof Nothing) {
      return "";
    }
    ;
    if (v instanceof Just) {
      return show52(v.value0);
    }
    ;
    throw new Error("Failed pattern match at D3.Simulation.Types (line 178, column 1 - line 178, column 52): " + [v.constructor.name]);
  };
  var showForceFilter = function(v) {
    if (v instanceof Just) {
      return v.value0.value0;
    }
    ;
    if (v instanceof Nothing) {
      return " (no filter)";
    }
    ;
    throw new Error("Failed pattern match at D3.Simulation.Types (line 203, column 1 - line 203, column 47): " + [v.constructor.name]);
  };
  var onlyTheseForcesActive = function(dictFoldable) {
    var fromFoldable23 = fromFoldable7(dictFoldable);
    return function(dictFunctor) {
      var map115 = map(dictFunctor);
      return function(labels9) {
        var updatedMap = fromFoldable23(map115(function(l) {
          return new Tuple(l, ForceActive.value);
        })(labels9));
        return function(statusMap) {
          return union3(updatedMap)(map37($$const(ForceDisabled.value))(statusMap));
        };
      };
    };
  };
  var eqForceStatus = {
    eq: function(x10) {
      return function(y8) {
        if (x10 instanceof ForceActive && y8 instanceof ForceActive) {
          return true;
        }
        ;
        if (x10 instanceof ForceDisabled && y8 instanceof ForceDisabled) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var defaultConfigSimulation = {
    alpha: 1,
    alphaTarget: 0,
    alphaMin: 1e-3,
    alphaDecay: 0.0228,
    velocityDecay: 0.4
  };
  var initialSimulationState = function(forces2) {
    var v = trace2({
      simulation: "initialized",
      forceLibrary: forces2
    })(function(v1) {
      return unit;
    });
    return {
      handle_: initSimulation_(defaultConfigSimulation)(keyIsID_),
      data: {
        nodes: [],
        links: []
      },
      key: keyIsID_,
      forceLibrary: forces2,
      ticks: empty2,
      alpha: defaultConfigSimulation.alpha,
      alphaTarget: defaultConfigSimulation.alphaTarget,
      alphaMin: defaultConfigSimulation.alphaMin,
      alphaDecay: defaultConfigSimulation.alphaDecay,
      velocityDecay: defaultConfigSimulation.velocityDecay
    };
  };
  var allNodes = /* @__PURE__ */ function() {
    return Nothing.value;
  }();
  var _velocityDecay = function(dictStrong) {
    var $226 = _Newtype2(dictStrong.Profunctor0());
    var $227 = prop6($$Proxy.value)(dictStrong);
    return function($228) {
      return $226($227($228));
    };
  };
  var _status = function(dictStrong) {
    var $239 = _Newtype2(dictStrong.Profunctor0());
    var $240 = prop33($$Proxy.value)(dictStrong);
    return function($241) {
      return $239($240($241));
    };
  };
  var _status1 = /* @__PURE__ */ _status(strongForget);
  var _name = function(dictStrong) {
    var $242 = _Newtype2(dictStrong.Profunctor0());
    var $243 = prop42($$Proxy.value)(dictStrong);
    return function($244) {
      return $242($243($244));
    };
  };
  var _name1 = /* @__PURE__ */ _name(strongForget);
  var getStatusMap = function(forceMap) {
    return spy4("getStatusMap: ")(fromFoldable1(map37(function(f) {
      return new Tuple(view(_name1)(f), view(_status1)(f));
    })(forceMap)));
  };
  var _d3Simulation = function(dictStrong) {
    return prop11($$Proxy.value)(dictStrong);
  };
  var _forceLibrary = function(dictStrong) {
    var $266 = _d3Simulation(dictStrong);
    var $267 = _Newtype2(dictStrong.Profunctor0());
    var $268 = prop122($$Proxy.value)(dictStrong);
    return function($269) {
      return $266($267($268($269)));
    };
  };
  var _handle = function(dictStrong) {
    var $273 = _d3Simulation(dictStrong);
    var $274 = _Newtype2(dictStrong.Profunctor0());
    var $275 = prop13($$Proxy.value)(dictStrong);
    return function($276) {
      return $273($274($275($276)));
    };
  };
  var _alphaTarget = function(dictStrong) {
    var $280 = _Newtype2(dictStrong.Profunctor0());
    var $281 = prop15($$Proxy.value)(dictStrong);
    return function($282) {
      return $280($281($282));
    };
  };
  var _alphaMin = function(dictStrong) {
    var $283 = _Newtype2(dictStrong.Profunctor0());
    var $284 = prop16($$Proxy.value)(dictStrong);
    return function($285) {
      return $283($284($285));
    };
  };
  var _alphaDecay = function(dictStrong) {
    var $286 = _Newtype2(dictStrong.Profunctor0());
    var $287 = prop17($$Proxy.value)(dictStrong);
    return function($288) {
      return $286($287($288));
    };
  };
  var _alpha = function(dictStrong) {
    var $289 = _Newtype2(dictStrong.Profunctor0());
    var $290 = prop18($$Proxy.value)(dictStrong);
    return function($291) {
      return $289($290($291));
    };
  };

  // output/Utility/index.js
  var map38 = /* @__PURE__ */ map(functorNonEmptyArray);
  var getWindowWidthHeight = function __do2() {
    var win = windowImpl();
    var w = innerWidth(win)();
    var h = innerHeight(win)();
    return new Tuple(toNumber(w), toNumber(h));
  };
  var equalSnd = function(dictEq) {
    var eq8 = eq(dictEq);
    return function(a2) {
      return function(b2) {
        return eq8(snd(a2))(snd(b2));
      };
    };
  };
  var compareSnd = function(dictOrd) {
    var compare2 = compare(dictOrd);
    return function(a2) {
      return function(b2) {
        return compare2(snd(a2))(snd(b2));
      };
    };
  };
  var chunk = function(tuples) {
    var $$package = snd(head5(tuples));
    var contains2 = toArray2(map38(fst)(tuples));
    return new Tuple($$package, contains2);
  };

  // output/D3.Examples.LesMiserables/index.js
  var classed4 = /* @__PURE__ */ classed(toAttrString);
  var strokeColor2 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeOpacity2 = /* @__PURE__ */ strokeOpacity(toAttrNumber);
  var discard8 = /* @__PURE__ */ discard(discardUnit);
  var radius2 = /* @__PURE__ */ radius(toAttrNumber);
  var fill3 = /* @__PURE__ */ fill(toAttrStringFn);
  var strokeWidth2 = /* @__PURE__ */ strokeWidth(toAttrNumberFn);
  var strokeColor1 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var cx2 = /* @__PURE__ */ cx(toAttrNumberFn);
  var cy2 = /* @__PURE__ */ cy(toAttrNumberFn);
  var x12 = /* @__PURE__ */ x1(toAttrNumberFn);
  var y12 = /* @__PURE__ */ y1(toAttrNumberFn);
  var x22 = /* @__PURE__ */ x2(toAttrNumberFn);
  var y22 = /* @__PURE__ */ y2(toAttrNumberFn);
  var link_ = {
    source: function($57) {
      return function(v) {
        return v.source;
      }(unboxD3SimLink($57));
    },
    target: function($58) {
      return function(v) {
        return v.target;
      }(unboxD3SimLink($58));
    },
    value: function($59) {
      return function(v) {
        return v.value;
      }(unboxD3SimLink($59));
    },
    color: function($60) {
      return d3SchemeCategory10N_(toNumber(function(v) {
        return v.target.group;
      }(unboxD3SimLink($60))));
    }
  };
  var datum_ = {
    id: function($61) {
      return function(v) {
        return v.id;
      }(unboxD3SimNode($61));
    },
    x: function($62) {
      return function(v) {
        return v.x;
      }(unboxD3SimNode($62));
    },
    y: function($63) {
      return function(v) {
        return v.y;
      }(unboxD3SimNode($63));
    },
    group: function($64) {
      return function(v) {
        return v.group;
      }(unboxD3SimNode($64));
    },
    colorByGroup: function($65) {
      return d3SchemeCategory10N_(toNumber(function(v) {
        return v.group;
      }(unboxD3SimNode($65))));
    }
  };
  var draw = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard8(dictBind);
    return function(dictMonadEffect) {
      var liftEffect11 = liftEffect(dictMonadEffect);
      var pure21 = pure(dictMonadEffect.Monad0().Applicative0());
      return function(dictMonadState) {
        return function(dictSimulationM) {
          var SelectionM1 = dictSimulationM.SelectionM1();
          var attach2 = attach(SelectionM1);
          var appendTo2 = appendTo(SelectionM1);
          var setNodes2 = setNodes(dictSimulationM);
          var setLinks2 = setLinks(dictSimulationM)(eqString);
          var simpleJoin2 = simpleJoin(SelectionM1);
          var setAttributes2 = setAttributes(SelectionM1);
          var addTickFunction2 = addTickFunction(dictSimulationM);
          var on3 = on2(SelectionM1);
          var setConfigVariable4 = setConfigVariable(dictSimulationM);
          return function(model) {
            return function(selector) {
              return bind18(liftEffect11(getWindowWidthHeight))(function(v) {
                return bind18(attach2(selector))(function(v1) {
                  return bind18(appendTo2(v1)(Svg.value)([viewBox(-v.value0 / 2)(-v.value1 / 2)(v.value0)(v.value1), classed4("lesmis")]))(function(svg2) {
                    return bind18(appendTo2(svg2)(Group.value)([classed4("link"), strokeColor2("#999"), strokeOpacity2(0.6)]))(function(linksGroup) {
                      return bind18(appendTo2(svg2)(Group.value)([classed4("node"), strokeColor2("#fff"), strokeOpacity2(1.5)]))(function(nodesGroup) {
                        return bind18(setNodes2(model.nodes))(function(nodesInSim) {
                          return bind18(setLinks2(model.links)(model.nodes)(keyIsID_))(function(linksInSim) {
                            return bind18(simpleJoin2(nodesGroup)(Circle.value)(nodesInSim)(keyIsID_))(function(nodesSelection) {
                              return discard111(setAttributes2(nodesSelection)([radius2(5), fill3(datum_.colorByGroup)]))(function() {
                                return bind18(simpleJoin2(linksGroup)(Line.value)(linksInSim)(keyIsID_))(function(linksSelection) {
                                  return discard111(setAttributes2(linksSelection)([strokeWidth2(function($66) {
                                    return sqrt(link_.value($66));
                                  }), strokeColor1(link_.color)]))(function() {
                                    return discard111(addTickFunction2("nodes")(new Step3(nodesSelection, [cx2(datum_.x), cy2(datum_.y)])))(function() {
                                      return discard111(addTickFunction2("links")(new Step3(linksSelection, [x12(function($67) {
                                        return function(v2) {
                                          return v2.x;
                                        }(link_.source($67));
                                      }), y12(function($68) {
                                        return function(v2) {
                                          return v2.y;
                                        }(link_.source($68));
                                      }), x22(function($69) {
                                        return function(v2) {
                                          return v2.x;
                                        }(link_.target($69));
                                      }), y22(function($70) {
                                        return function(v2) {
                                          return v2.y;
                                        }(link_.target($70));
                                      })])))(function() {
                                        return bind18(on3(nodesSelection)(new Drag(new CustomDrag("lesmis", simdrag))))(function() {
                                          return bind18(on3(svg2)(new Zoom({
                                            extent: new ZoomExtent({
                                              top: 0,
                                              left: 0,
                                              bottom: v.value1,
                                              right: v.value0
                                            }),
                                            scale: new ScaleExtent(1, 4),
                                            name: "LesMis",
                                            target: svg2
                                          })))(function() {
                                            return discard111(setConfigVariable4(new Alpha(1)))(function() {
                                              return pure21(unit);
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

  // output/D3.Examples.LesMiserables.File/foreign.js
  function readJSONJS_(filecontents) {
    return decodeFile(filecontents);
  }
  var decodeFile = function(filecontents) {
    const json = JSON.parse(filecontents);
    const links = json.links.map((d2) => Object.create(d2));
    return { links, nodes: json.nodes };
  };

  // output/D3.Examples.LesMiserables.File/index.js
  var readGraphFromFileContents = function(v) {
    if (v instanceof Right) {
      return readJSONJS_(v.value0.body);
    }
    ;
    if (v instanceof Left) {
      return {
        links: [],
        nodes: []
      };
    }
    ;
    throw new Error("Failed pattern match at D3.Examples.LesMiserables.File (line 10, column 1 - line 10, column 92): " + [v.constructor.name]);
  };

  // output/D3.Simulation.Config/index.js
  var y4 = function(dictToAttr) {
    var $34 = AttributeSetter.create("y");
    var $35 = toAttr(dictToAttr);
    return function($36) {
      return ForceT($34($35($36)));
    };
  };
  var x4 = function(dictToAttr) {
    var $37 = AttributeSetter.create("x");
    var $38 = toAttr(dictToAttr);
    return function($39) {
      return ForceT($37($38($39)));
    };
  };
  var theta = function(dictToAttr) {
    var $40 = AttributeSetter.create("theta");
    var $41 = toAttr(dictToAttr);
    return function($42) {
      return ForceT($40($41($42)));
    };
  };
  var strength = function(dictToAttr) {
    var $45 = AttributeSetter.create("strength");
    var $46 = toAttr(dictToAttr);
    return function($47) {
      return ForceT($45($46($47)));
    };
  };
  var radius3 = function(dictToAttr) {
    var $48 = AttributeSetter.create("radius");
    var $49 = toAttr(dictToAttr);
    return function($50) {
      return ForceT($48($49($50)));
    };
  };
  var numKey = /* @__PURE__ */ function() {
    var $51 = AttributeSetter.create("keyFn");
    return function($52) {
      return ForceT($51(NumberAttr.create(Fn.create($52))));
    };
  }();
  var distanceMin = function(dictToAttr) {
    var $65 = AttributeSetter.create("distanceMin");
    var $66 = toAttr(dictToAttr);
    return function($67) {
      return ForceT($65($66($67)));
    };
  };
  var distanceMax = function(dictToAttr) {
    var $68 = AttributeSetter.create("distanceMax");
    var $69 = toAttr(dictToAttr);
    return function($70) {
      return ForceT($68($69($70)));
    };
  };
  var distance = function(dictToAttr) {
    var $71 = AttributeSetter.create("distance");
    var $72 = toAttr(dictToAttr);
    return function($73) {
      return ForceT($71($72($73)));
    };
  };

  // output/D3.Simulation.Forces/index.js
  var _status2 = /* @__PURE__ */ _status(strongFn);
  var show8 = /* @__PURE__ */ show(showRegularForceType);
  var at2 = /* @__PURE__ */ at(/* @__PURE__ */ atMap(ordString));
  var _name2 = /* @__PURE__ */ _name(strongForget);
  var map39 = /* @__PURE__ */ map(functorMap);
  var fromFoldable8 = /* @__PURE__ */ fromFoldable2(ordString);
  var unwrap7 = /* @__PURE__ */ unwrap();
  var map111 = /* @__PURE__ */ map(functorArray);
  var showType = function(v) {
    if (v instanceof LinkForce) {
      return "linkForce";
    }
    ;
    if (v instanceof RegularForce) {
      return show8(v.value0);
    }
    ;
    throw new Error("Failed pattern match at D3.Simulation.Forces (line 33, column 3 - line 35, column 29): " + [v.constructor.name]);
  };
  var removeForceFromSimulation = function(v) {
    return function(simulation_) {
      if (v.type instanceof RegularForce) {
        return setAsNullForceInSimulation_(simulation_)(v.name);
      }
      ;
      if (v.type instanceof LinkForce) {
        return unsetLinks_(simulation_);
      }
      ;
      throw new Error("Failed pattern match at D3.Simulation.Forces (line 112, column 3 - line 115, column 46): " + [v.type.constructor.name]);
    };
  };
  var putStatusMap = function(forceStatusMap) {
    return function(forceMap) {
      var update3 = function(force2) {
        var v = view(at2(view(_name2)(force2))(strongForget))(forceStatusMap);
        if (v instanceof Nothing) {
          return set2(_status2)(ForceDisabled.value)(force2);
        }
        ;
        if (v instanceof Just) {
          return set2(_status2)(v.value0)(force2);
        }
        ;
        throw new Error("Failed pattern match at D3.Simulation.Forces (line 27, column 7 - line 29, column 50): " + [v.constructor.name]);
      };
      return map39(update3)(forceMap);
    };
  };
  var putForceInSimulation = function(v) {
    return function(simulation_) {
      if (v.type instanceof RegularForce) {
        return putForceInSimulation_(simulation_)(v.name)(v.force_);
      }
      ;
      if (v.type instanceof LinkForce) {
        return putForceInSimulation_(simulation_)(v.name)(v.force_);
      }
      ;
      throw new Error("Failed pattern match at D3.Simulation.Forces (line 103, column 3 - line 107, column 80): " + [v.type.constructor.name]);
    };
  };
  var initialize = function(dictFoldable) {
    var fromFoldable16 = fromFoldable8(dictFoldable);
    return function(dictFunctor) {
      var map210 = map(dictFunctor);
      return function(forces2) {
        return fromFoldable16(map210(function(f) {
          return new Tuple(view(_name2)(f), f);
        })(forces2));
      };
    };
  };
  var createRegularForce_ = function(v) {
    if (v instanceof ForceManyBody) {
      return forceMany_(unit);
    }
    ;
    if (v instanceof ForceCenter) {
      return forceCenter_(unit);
    }
    ;
    if (v instanceof ForceCollide) {
      return forceCollideFn_(unit);
    }
    ;
    if (v instanceof ForceX) {
      return forceX_(unit);
    }
    ;
    if (v instanceof ForceY) {
      return forceY_(unit);
    }
    ;
    if (v instanceof ForceRadial) {
      return forceRadial_(unit);
    }
    ;
    throw new Error("Failed pattern match at D3.Simulation.Forces (line 167, column 23 - line 173, column 52): " + [v.constructor.name]);
  };
  var createForce_ = function(v) {
    if (v instanceof RegularForce) {
      return createRegularForce_(v.value0);
    }
    ;
    if (v instanceof LinkForce) {
      return forceLink_(unit);
    }
    ;
    throw new Error("Failed pattern match at D3.Simulation.Forces (line 161, column 3 - line 163, column 38): " + [v.constructor.name]);
  };
  var createLinkForce = function(f) {
    return function(cs) {
      return {
        type: LinkForce.value,
        name: linksForceName,
        status: ForceDisabled.value,
        filter: f,
        attributes: cs,
        force_: createForce_(LinkForce.value)
      };
    };
  };
  var createForce = function(l) {
    return function(t) {
      return function(f) {
        return function(cs) {
          return {
            type: t,
            name: l,
            status: ForceDisabled.value,
            filter: f,
            attributes: cs,
            force_: createForce_(t)
          };
        };
      };
    };
  };
  var attrFilter = function(filter$prime) {
    return function(default$prime) {
      var addFilterToStatic = function(filter5) {
        return function(value15) {
          return function($$default) {
            return function(d2) {
              var $49 = filter5(d2);
              if ($49) {
                return value15;
              }
              ;
              return $$default;
            };
          };
        };
      };
      var addFilterToFn = function(filter5) {
        return function(fn) {
          return function($$default) {
            return function(d2) {
              var $50 = filter5(d2);
              if ($50) {
                return fn(d2);
              }
              ;
              return $$default;
            };
          };
        };
      };
      return function(v) {
        if (v instanceof StringAttr && v.value0 instanceof Static) {
          return new StringAttr(new Static(v.value0.value0));
        }
        ;
        if (v instanceof StringAttr && v.value0 instanceof Fn) {
          return new StringAttr(new Fn(v.value0.value0));
        }
        ;
        if (v instanceof StringAttr && v.value0 instanceof FnI) {
          return new StringAttr(new FnI(v.value0.value0));
        }
        ;
        if (v instanceof NumberAttr && v.value0 instanceof Static) {
          return new NumberAttr(new Fn(addFilterToStatic(filter$prime)(v.value0.value0)(default$prime)));
        }
        ;
        if (v instanceof NumberAttr && v.value0 instanceof Fn) {
          return new NumberAttr(new Fn(addFilterToFn(filter$prime)(v.value0.value0)(default$prime)));
        }
        ;
        if (v instanceof NumberAttr && v.value0 instanceof FnI) {
          return new NumberAttr(new FnI(v.value0.value0));
        }
        ;
        if (v instanceof ArrayAttr && v.value0 instanceof Static) {
          return new ArrayAttr(new Static(v.value0.value0));
        }
        ;
        if (v instanceof ArrayAttr && v.value0 instanceof Fn) {
          return new ArrayAttr(new Fn(v.value0.value0));
        }
        ;
        if (v instanceof ArrayAttr && v.value0 instanceof FnI) {
          return new ArrayAttr(new FnI(v.value0.value0));
        }
        ;
        throw new Error("Failed pattern match at D3.Simulation.Forces (line 212, column 3 - line 223, column 49): " + [v.constructor.name]);
      };
    };
  };
  var setForceAttr = function(force_) {
    return function(maybeFilter) {
      return function(v) {
        if (v.value0 === "strength") {
          if (maybeFilter instanceof Nothing) {
            return setForceStrength_(force_)(unboxAttr(v.value1));
          }
          ;
          if (maybeFilter instanceof Just) {
            return setForceStrength_(force_)(unboxAttr(attrFilter(maybeFilter.value0.value1)(0)(v.value1)));
          }
          ;
          throw new Error("Failed pattern match at D3.Simulation.Forces (line 182, column 7 - line 186, column 82): " + [maybeFilter.constructor.name]);
        }
        ;
        if (v.value0 === "radius") {
          return setForceRadius_(force_)(unboxAttr(v.value1));
        }
        ;
        if (v.value0 === "theta") {
          return setForceTheta_(force_)(unboxAttr(v.value1));
        }
        ;
        if (v.value0 === "distanceMin") {
          return setForceDistanceMin_(force_)(unboxAttr(v.value1));
        }
        ;
        if (v.value0 === "distanceMax") {
          return setForceDistanceMax_(force_)(unboxAttr(v.value1));
        }
        ;
        if (v.value0 === "iterations") {
          return setForceIterations_(force_)(unboxAttr(v.value1));
        }
        ;
        if (v.value0 === "x") {
          return setForceX_(force_)(unboxAttr(v.value1));
        }
        ;
        if (v.value0 === "y") {
          return setForceY_(force_)(unboxAttr(v.value1));
        }
        ;
        if (v.value0 === "distance") {
          return setForceDistance_(force_)(unboxAttr(v.value1));
        }
        ;
        return force_;
      };
    };
  };
  var updateForceInSimulation = function(simulation) {
    return function(force2) {
      var f = unwrap7(force2);
      var v = map111(function(a2) {
        return setForceAttr(f.force_)(f.filter)(unwrap7(a2));
      })(f.attributes);
      if (f.status instanceof ForceActive) {
        return putForceInSimulation(force2)(simulation);
      }
      ;
      if (f.status instanceof ForceDisabled) {
        return removeForceFromSimulation(force2)(simulation);
      }
      ;
      throw new Error("Failed pattern match at D3.Simulation.Forces (line 96, column 5 - line 98, column 66): " + [f.status.constructor.name]);
    };
  };

  // output/D3.Simulation.Functions/index.js
  var _handle2 = /* @__PURE__ */ _handle(strongForget);
  var discard9 = /* @__PURE__ */ discard(discardUnit);
  var _d3Simulation2 = /* @__PURE__ */ _d3Simulation(strongFn);
  var _alpha2 = /* @__PURE__ */ _alpha(strongFn);
  var _forceLibrary2 = /* @__PURE__ */ _forceLibrary(strongForget);
  var toUnfoldable4 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var map40 = /* @__PURE__ */ map(functorArray);
  var _alphaTarget2 = /* @__PURE__ */ _alphaTarget(strongFn);
  var _alphaMin2 = /* @__PURE__ */ _alphaMin(strongFn);
  var _alphaDecay2 = /* @__PURE__ */ _alphaDecay(strongFn);
  var _velocityDecay2 = /* @__PURE__ */ _velocityDecay(strongFn);
  var _forceLibrary1 = /* @__PURE__ */ _forceLibrary(strongFn);
  var map112 = /* @__PURE__ */ map(functorMap);
  var eq4 = /* @__PURE__ */ eq(eqForceStatus);
  var _status3 = /* @__PURE__ */ _status(strongForget);
  var spy5 = /* @__PURE__ */ spy();
  var simulationStop = function(dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var pure21 = pure(Monad0.Applicative0());
    return bind(Monad0.Bind1())(use(dictMonadState)(_handle2))(function(handle) {
      var v = stopSimulation_(handle);
      return pure21(unit);
    });
  };
  var simulationStart = function(dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var Bind1 = Monad0.Bind1();
    var discard111 = discard9(Bind1);
    var modifying3 = modifying(dictMonadState);
    var pure21 = pure(Monad0.Applicative0());
    return bind(Bind1)(use(dictMonadState)(_handle2))(function(handle) {
      return discard111(modifying3(function($229) {
        return _d3Simulation2(_alpha2($229));
      })($$const(1)))(function() {
        return pure21(startSimulation_(handle));
      });
    });
  };
  var simulationSetVariable = function(dictMonadState) {
    var bind18 = bind(dictMonadState.Monad0().Bind1());
    var use3 = use(dictMonadState);
    var modifying3 = modifying(dictMonadState);
    return function(v) {
      return bind18(use3(_handle2))(function(handle) {
        if (v instanceof Alpha) {
          var v1 = setAlpha_(handle)(v.value0);
          return modifying3(function($230) {
            return _d3Simulation2(_alpha2($230));
          })($$const(v.value0));
        }
        ;
        if (v instanceof AlphaTarget) {
          var v1 = setAlphaTarget_(handle)(v.value0);
          return modifying3(function($231) {
            return _d3Simulation2(_alphaTarget2($231));
          })($$const(v.value0));
        }
        ;
        if (v instanceof AlphaMin) {
          var v1 = setAlphaMin_(handle)(v.value0);
          return modifying3(function($232) {
            return _d3Simulation2(_alphaMin2($232));
          })($$const(v.value0));
        }
        ;
        if (v instanceof AlphaDecay) {
          var v1 = setAlphaDecay_(handle)(v.value0);
          return modifying3(function($233) {
            return _d3Simulation2(_alphaDecay2($233));
          })($$const(v.value0));
        }
        ;
        if (v instanceof VelocityDecay) {
          var v1 = setVelocityDecay_(handle)(v.value0);
          return modifying3(function($234) {
            return _d3Simulation2(_velocityDecay2($234));
          })($$const(v.value0));
        }
        ;
        throw new Error("Failed pattern match at D3.Simulation.Functions (line 92, column 3 - line 107, column 52): " + [v.constructor.name]);
      });
    };
  };
  var simulationSetNodesFromSelection = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(dictMonadState) {
      var use3 = use(dictMonadState);
      var pure21 = pure(dictMonadState.Monad0().Applicative0());
      return function(nodeSelection2) {
        return bind18(use3(_handle2))(function(handle) {
          var v = setNodes_(handle)(d3GetSelectionData_(nodeSelection2));
          return pure21(unit);
        });
      };
    };
  };
  var simulationSetNodes = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(dictMonadState) {
      var use3 = use(dictMonadState);
      var pure21 = pure(dictMonadState.Monad0().Applicative0());
      return function(nodes) {
        return bind18(use3(_handle2))(function(handle) {
          var v = setNodes_(handle)(nodes);
          var opaqueNodes = getNodes_(handle);
          return pure21(opaqueNodes);
        });
      };
    };
  };
  var simulationSetLinksFromSelection = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(dictMonadState) {
      var use3 = use(dictMonadState);
      var pure21 = pure(dictMonadState.Monad0().Applicative0());
      return function(linkSelection) {
        return function(filterFn) {
          return bind18(use3(_handle2))(function(handle) {
            var v = setLinks_(handle)(filter2(filterFn)(d3GetSelectionData_(linkSelection)));
            return pure21(unit);
          });
        };
      };
    };
  };
  var simulationSetLinks = function(dictEq) {
    return function(dictBind) {
      var bind18 = bind(dictBind);
      return function(dictMonadState) {
        var use3 = use(dictMonadState);
        var pure21 = pure(dictMonadState.Monad0().Applicative0());
        return function(links) {
          return function(nodes) {
            return function(keyFn) {
              return bind18(use3(_handle2))(function(handle) {
                var v = setLinks_(handle)(swizzleLinks_(links)(nodes)(keyFn));
                var swizzledLinks = getLinksFromSimulation_(handle);
                return pure21(swizzledLinks);
              });
            };
          };
        };
      };
    };
  };
  var simulationOn = function(dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var bind18 = bind(Monad0.Bind1());
    var use3 = use(dictMonadState);
    var pure21 = pure(Monad0.Applicative0());
    return function(selection) {
      return function(v) {
        if (v instanceof Drag) {
          return bind18(use3(_handle2))(function(handle) {
            var v12 = function() {
              if (v.value0 instanceof DefaultDrag) {
                return simulationDrag_("default")(selection)(handle)(simdrag);
              }
              ;
              if (v.value0 instanceof NoDrag) {
                return disableDrag_(selection);
              }
              ;
              if (v.value0 instanceof CustomDrag) {
                return simulationDrag_(v.value0.value0)(selection)(handle)(v.value0.value1);
              }
              ;
              throw new Error("Failed pattern match at D3.Simulation.Functions (line 282, column 11 - line 285, column 78): " + [v.value0.constructor.name]);
            }();
            return pure21(unit);
          });
        }
        ;
        if (v instanceof Zoom) {
          var v1 = function() {
            if (v.value0.extent instanceof DefaultZoomExtent) {
              return d3AttachZoomDefaultExtent_(selection)({
                scaleExtent: [v.value0.scale.value0, v.value0.scale.value1],
                name: v.value0.name,
                target: v.value0.target
              });
            }
            ;
            if (v.value0.extent instanceof ZoomExtent) {
              return d3AttachZoom_(selection)({
                extent: [[v.value0.extent.value0.left, v.value0.extent.value0.top], [v.value0.extent.value0.right, v.value0.extent.value0.bottom]],
                scaleExtent: [v.value0.scale.value0, v.value0.scale.value1],
                name: v.value0.name,
                target: v.value0.target
              });
            }
            ;
            throw new Error("Failed pattern match at D3.Simulation.Functions (line 293, column 9 - line 307, column 14): " + [v.value0.extent.constructor.name]);
          }();
          return pure21(unit);
        }
        ;
        throw new Error("Failed pattern match at D3.Simulation.Functions (line 277, column 1 - line 279, column 51): " + [selection.constructor.name, v.constructor.name]);
      };
    };
  };
  var simulationMergeNewData = function(dictEq) {
    var elem5 = elem2(dictEq);
    return function(dictBind) {
      return function(dictMonadState) {
        var pure21 = pure(dictMonadState.Monad0().Applicative0());
        return function(nodeSelection2) {
          return function(nodeKeyFn) {
            return function(linkSelection) {
              return function(linkKeyFn) {
                return function(rawdata) {
                  var updatedNodeData = d3PreserveSimulationPositions_(nodeSelection2)(rawdata.nodes)(nodeKeyFn);
                  var nodeIDs = getIDsFromNodes_(rawdata.nodes)(nodeKeyFn);
                  var validLink = function(l) {
                    var v = getLinkIDs_(linkKeyFn)(l);
                    return elem5(v.sourceID)(nodeIDs) && elem5(v.targetID)(nodeIDs);
                  };
                  var validNewLinks = filter2(validLink)(rawdata.links);
                  var updatedLinkData = d3PreserveLinkReferences_(linkSelection)(validNewLinks);
                  var swizzledLinkData = swizzleLinks_(updatedLinkData)(updatedNodeData)(nodeKeyFn);
                  return pure21({
                    nodes: updatedNodeData,
                    links: swizzledLinkData
                  });
                };
              };
            };
          };
        };
      };
    };
  };
  var listActiveForcesInLibrary = function(forceMap) {
    return map40(fst)(filter2(function(v) {
      return eq4(view(_status3)(v.value1))(ForceActive.value);
    })(toUnfoldable4(forceMap)));
  };
  var listActiveForces = function(forceMap) {
    return map40(fst)(filter2(function(v) {
      return eq4(v.value1)(ForceActive.value);
    })(toUnfoldable4(forceMap)));
  };
  var simulationUpdateForceStatuses = function(dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind18 = bind(Bind1);
    var use3 = use(dictMonadState);
    var discard111 = discard9(Bind1);
    var modifying3 = modifying(dictMonadState);
    var pure21 = pure(Monad0.Applicative0());
    return function(forceStatuses) {
      return bind18(use3(_handle2))(function(handle) {
        var v = spy5("forceStatuses on update: ")(listActiveForces(forceStatuses));
        return discard111(modifying3(_forceLibrary1)(putStatusMap(forceStatuses)))(function() {
          return bind18(use3(_forceLibrary2))(function(forceLibrary3) {
            var v1 = spy5("forceLibrary after status update: ")(listActiveForcesInLibrary(forceLibrary3));
            var v2 = map112(updateForceInSimulation(handle))(forceLibrary3);
            return pure21(unit);
          });
        });
      });
    };
  };

  // output/D3Tagless.Instance.Simulation/index.js
  var liftA13 = /* @__PURE__ */ liftA1(applicativeEffect);
  var discard10 = /* @__PURE__ */ discard(discardUnit);
  var mapFlipped4 = /* @__PURE__ */ mapFlipped(functorArray);
  var run_D3M_Simulation = function(simulation) {
    return function(v) {
      return runStateT(v)(simulation);
    };
  };
  var monadStateD3SimM = /* @__PURE__ */ monadStateStateT(monadEffect);
  var simulationOn2 = /* @__PURE__ */ simulationOn(monadStateD3SimM);
  var simulationSetVariable2 = /* @__PURE__ */ simulationSetVariable(monadStateD3SimM);
  var monadEffD3SimM = /* @__PURE__ */ monadEffectState(monadEffectEffect);
  var monadD3SimM = /* @__PURE__ */ monadStateT(monadEffect);
  var selectionMD3Selection_D3S = {
    appendTo: function(s_) {
      return selectionAppendElement(selectionMD3Selection_D3S)(s_);
    },
    selectUnder: function(s_) {
      return selectionSelectUnder(selectionMD3Selection_D3S)(s_);
    },
    attach: function(selector) {
      return selectionAttach(selectionMD3Selection_D3S)(selector);
    },
    filterSelection: function(s_) {
      return selectionFilterSelection(selectionMD3Selection_D3S)(s_);
    },
    mergeSelections: function(s_) {
      return selectionMergeSelections(selectionMD3Selection_D3S)(s_);
    },
    setAttributes: function(s_) {
      return selectionModifySelection(selectionMD3Selection_D3S)(s_);
    },
    on: function(s_) {
      return simulationOn2(s_);
    },
    openSelection: function(s_) {
      return selectionOpenSelection(selectionMD3Selection_D3S)(s_);
    },
    simpleJoin: function(s_) {
      return selectionJoin(selectionMD3Selection_D3S)(s_);
    },
    updateJoin: function(s_) {
      return selectionUpdateJoin(selectionMD3Selection_D3S)(s_);
    },
    Monad0: function() {
      return monadD3SimM;
    }
  };
  var exec_D3M_Simulation = function(simulation) {
    return function(v) {
      return liftA13(snd)(runStateT(v)(simulation));
    };
  };
  var runWithD3_Simulation = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(dictMonadState) {
      var get6 = get(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadEffect) {
        var liftEffect11 = liftEffect(dictMonadEffect);
        return function(state_T) {
          return bind18(get6)(function(state3) {
            return bind18(liftEffect11(exec_D3M_Simulation(state3)(state_T)))(function(state$prime) {
              return modify_6(function(v) {
                return state$prime;
              });
            });
          });
        };
      };
    };
  };
  var evalEffectSimulation = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard10(dictBind);
    return function(dictMonadState) {
      var get6 = get(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadEffect) {
        var liftEffect11 = liftEffect(dictMonadEffect);
        var pure111 = pure(dictMonadEffect.Monad0().Applicative0());
        return function(state_T) {
          return bind18(get6)(function(state3) {
            return bind18(liftEffect11(run_D3M_Simulation(state3)(state_T)))(function(v) {
              return discard111(modify_6(function(v1) {
                return v.value1;
              }))(function() {
                return pure111(v.value0);
              });
            });
          });
        };
      };
    };
  };
  var bindD3SimM = /* @__PURE__ */ bindStateT(monadEffect);
  var simulationSetNodesFromSelection2 = /* @__PURE__ */ simulationSetNodesFromSelection(bindD3SimM)(monadStateD3SimM);
  var simulationSetLinksFromSelection2 = /* @__PURE__ */ simulationSetLinksFromSelection(bindD3SimM)(monadStateD3SimM);
  var bind6 = /* @__PURE__ */ bind(bindD3SimM);
  var applicativeD3SimM = /* @__PURE__ */ applicativeStateT(monadEffect);
  var pure16 = /* @__PURE__ */ pure(applicativeD3SimM);
  var simulationMD3Selection_D3 = {
    start: /* @__PURE__ */ simulationStart(monadStateD3SimM),
    stop: /* @__PURE__ */ simulationStop(monadStateD3SimM),
    setConfigVariable: function(v) {
      return simulationSetVariable2(v);
    },
    actualizeForces: /* @__PURE__ */ simulationUpdateForceStatuses(monadStateD3SimM),
    setNodes: /* @__PURE__ */ simulationSetNodes(bindD3SimM)(monadStateD3SimM),
    setLinks: function(dictEq) {
      return simulationSetLinks(dictEq)(bindD3SimM)(monadStateD3SimM);
    },
    mergeNewDataWithSim: function(dictEq) {
      var simulationMergeNewData2 = simulationMergeNewData(dictEq)(bindD3SimM)(monadStateD3SimM);
      return function(selection) {
        return simulationMergeNewData2(selection);
      };
    },
    setNodesFromSelection: function(selection) {
      return simulationSetNodesFromSelection2(selection);
    },
    setLinksFromSelection: function(selection) {
      return simulationSetLinksFromSelection2(selection);
    },
    addTickFunction: function(v) {
      return function(v1) {
        if (v1 instanceof StepTransformFFI) {
          return pure16(unit);
        }
        ;
        if (v1 instanceof Step3) {
          return bind6(simulationHandle(simulationMD3Selection_D3))(function(handle) {
            var makeTick = function(v22) {
              var v3 = mapFlipped4(v1.value1)(applySelectionAttributeD3(v1.value0));
              return unit;
            };
            var v2 = onTick_(handle)(v)(makeTick);
            return pure16(unit);
          });
        }
        ;
        throw new Error("Failed pattern match at D3Tagless.Instance.Simulation (line 84, column 1 - line 119, column 33): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    removeTickFunction: function(label6) {
      return bind6(simulationHandle(simulationMD3Selection_D3))(function(handle) {
        var v = disableTick_(handle)(label6);
        return pure16(unit);
      });
    },
    simulationHandle: /* @__PURE__ */ use(monadStateD3SimM)(/* @__PURE__ */ _handle(strongForget)),
    Monad0: function() {
      return monadD3SimM;
    },
    SelectionM1: function() {
      return selectionMD3Selection_D3S;
    }
  };

  // output/Data.Lens.Fold/index.js
  var unwrap8 = /* @__PURE__ */ unwrap();
  var foldMapOf = /* @__PURE__ */ under()()(Forget);
  var preview = function(p2) {
    var $135 = foldMapOf(p2)(function($137) {
      return First(Just.create($137));
    });
    return function($136) {
      return unwrap8($135($136));
    };
  };

  // output/Ocelot.Block.FormField/index.js
  var map41 = /* @__PURE__ */ map(functorArray);
  var labelClasses3 = /* @__PURE__ */ map41(ClassName)(["block", "font-medium", "leading-loose", "text-black-20"]);
  var helpTextClasses2 = /* @__PURE__ */ append(semigroupArray)(mutedClasses)(/* @__PURE__ */ map41(ClassName)(["block", "font-light", "pt-3"]));
  var helpText2 = function(iprops) {
    return div2(appendIProps([classes(helpTextClasses2)])(iprops));
  };
  var helpText_2 = /* @__PURE__ */ helpText2([]);
  var fieldClasses2 = /* @__PURE__ */ map41(ClassName)(["w-full", "mb-10"]);
  var errorTextClasses2 = /* @__PURE__ */ map41(ClassName)(["block", "text-red", "font-medium", "pt-3"]);
  var error5 = function(iprops) {
    return div2(appendIProps([classes(errorTextClasses2)])(iprops));
  };
  var error_2 = /* @__PURE__ */ error5([]);
  var field$prime2 = function(config) {
    return function(iprops) {
      return function(html2) {
        return div2(appendIProps([classes(fieldClasses2)])(iprops))([label4([classes(labelClasses3), $$for(config.inputId)])([fromPlainHTML(config.label)]), html2, error_2(config.error), helpText_2(config.helpText)]);
      };
    };
  };
  var field2 = function(config) {
    return function(iprops) {
      return function(html2) {
        return field$prime2(config)(iprops)(div2([css("my-1")])(html2));
      };
    };
  };
  var field_2 = function(config) {
    return field2(config)([]);
  };
  var fieldset2 = function(config) {
    return function(iprops) {
      return function(html2) {
        return div2(appendIProps([classes(fieldClasses2)])(iprops))([fieldset([])([legend([classes(labelClasses3)])([fromPlainHTML(config.label)]), div2([css("my-1")])(html2), error_2(config.error), helpText_2(config.helpText)])]);
      };
    };
  };
  var fieldset_2 = function(config) {
    return fieldset2(config)([]);
  };

  // output/Stories.LesMis/index.js
  var strength2 = /* @__PURE__ */ strength(toAttrNumber);
  var choiceForget2 = /* @__PURE__ */ choiceForget(monoidFirst);
  var not6 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var traverse3 = /* @__PURE__ */ traverse(traversableArray);
  var discard11 = /* @__PURE__ */ discard(discardUnit);
  var bind7 = /* @__PURE__ */ bind(bindD3SimM);
  var use2 = /* @__PURE__ */ use(monadStateD3SimM);
  var discard13 = /* @__PURE__ */ discard11(bindD3SimM);
  var actualizeForces2 = /* @__PURE__ */ actualizeForces(simulationMD3Selection_D3);
  var draw2 = /* @__PURE__ */ draw(bindD3SimM)(monadEffD3SimM)(monadStateD3SimM)(simulationMD3Selection_D3);
  var setConfigVariable2 = /* @__PURE__ */ setConfigVariable(simulationMD3Selection_D3);
  var start3 = /* @__PURE__ */ start2(simulationMD3Selection_D3);
  var prop7 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "code";
    }
  })()();
  var Initialize3 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var Finalize3 = /* @__PURE__ */ function() {
    function Finalize6() {
    }
    ;
    Finalize6.value = new Finalize6();
    return Finalize6;
  }();
  var ToggleCard2 = /* @__PURE__ */ function() {
    function ToggleCard7(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard7.create = function(value0) {
      return new ToggleCard7(value0);
    };
    return ToggleCard7;
  }();
  var ToggleForce = /* @__PURE__ */ function() {
    function ToggleForce3(value0) {
      this.value0 = value0;
    }
    ;
    ToggleForce3.create = function(value0) {
      return new ToggleForce3(value0);
    };
    return ToggleForce3;
  }();
  var Freeze = /* @__PURE__ */ function() {
    function Freeze2() {
    }
    ;
    Freeze2.value = new Freeze2();
    return Freeze2;
  }();
  var Reheat = /* @__PURE__ */ function() {
    function Reheat2() {
    }
    ;
    Reheat2.value = new Reheat2();
    return Reheat2;
  }();
  var forceNames = {
    manyBodyNeg: "many body negative",
    manyBodyPos: "many body positive",
    collision: "collision",
    center: "center",
    links: linksForceName
  };
  var forces = /* @__PURE__ */ function() {
    return {
      manyBodyNeg: createForce(forceNames.manyBodyNeg)(new RegularForce(ForceManyBody.value))(allNodes)([strength2(-40)]),
      manyBodyPos: createForce(forceNames.manyBodyPos)(new RegularForce(ForceManyBody.value))(allNodes)([strength2(30)]),
      collision: createForce(forceNames.collision)(new RegularForce(ForceCollide.value))(allNodes)([radius3(toAttrNumber)(4)]),
      center: createForce(forceNames.center)(new RegularForce(ForceCenter.value))(allNodes)([x4(toAttrNumber)(0), y4(toAttrNumber)(0), strength2(1)]),
      links: createLinkForce(Nothing.value)([])
    };
  }();
  var forceLibrary = /* @__PURE__ */ function() {
    return initialize(foldableArray)(functorArray)([forces.manyBodyNeg, forces.manyBodyPos, forces.collision, forces.center, forces.links]);
  }();
  var _panels2 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  }();
  var _notebook = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "notebook";
      }
    })()()($$Proxy.value);
  }();
  var _notebook1 = /* @__PURE__ */ _notebook(strongFn);
  var _forceStatuses = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "forceStatuses";
      }
    })()()($$Proxy.value);
  }();
  var _forceStatuses1 = /* @__PURE__ */ _forceStatuses(strongFn);
  var _forceStatuses2 = /* @__PURE__ */ _forceStatuses(strongForget);
  var _forceStatus = function(dictAt) {
    var at4 = at(dictAt);
    return function(dictStrong) {
      return function(dictChoice) {
        var _Just2 = _Just(dictChoice);
        return function(label6) {
          var $127 = at4(label6)(dictStrong);
          return function($128) {
            return $127(_Just2($128));
          };
        };
      };
    };
  };
  var _forceStatus1 = /* @__PURE__ */ _forceStatus(/* @__PURE__ */ atMap(ordString));
  var _forceStatus2 = /* @__PURE__ */ _forceStatus1(strongFn)(choiceFn);
  var _linksSetting = function(dictStrong) {
    var _forceStatus32 = _forceStatus1(dictStrong);
    return function(dictChoice) {
      return _forceStatus32(dictChoice)(forceNames.links);
    };
  };
  var _linksSetting1 = /* @__PURE__ */ _linksSetting(strongForget)(choiceForget2);
  var _linksSetting2 = /* @__PURE__ */ _linksSetting(strongFn)(choiceFn);
  var _manyBodyNegSetting = function(dictStrong) {
    var _forceStatus32 = _forceStatus1(dictStrong);
    return function(dictChoice) {
      return _forceStatus32(dictChoice)(forceNames.manyBodyNeg);
    };
  };
  var _manyBodyNegSetting1 = /* @__PURE__ */ _manyBodyNegSetting(strongForget)(choiceForget2);
  var _manyBodyNegSetting2 = /* @__PURE__ */ _manyBodyNegSetting(strongFn)(choiceFn);
  var _manyBodyPosSetting = function(dictStrong) {
    var _forceStatus32 = _forceStatus1(dictStrong);
    return function(dictChoice) {
      return _forceStatus32(dictChoice)(forceNames.manyBodyPos);
    };
  };
  var _manyBodyPosSetting1 = /* @__PURE__ */ _manyBodyPosSetting(strongForget)(choiceForget2);
  var _manyBodyPosSetting2 = /* @__PURE__ */ _manyBodyPosSetting(strongFn)(choiceFn);
  var _collisionSetting = function(dictStrong) {
    var _forceStatus32 = _forceStatus1(dictStrong);
    return function(dictChoice) {
      return _forceStatus32(dictChoice)(forceNames.collision);
    };
  };
  var _collisionSetting1 = /* @__PURE__ */ _collisionSetting(strongForget)(choiceForget2);
  var _collisionSetting2 = /* @__PURE__ */ _collisionSetting(strongFn)(choiceFn);
  var controls2 = function(forceStatuses) {
    return buttonGroup([class_("flex-col")])([buttonVertical([onClick($$const(new ToggleForce(forceNames.links)))])([text5("links: " + showMaybeForceStatus(preview(_linksSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(new ToggleForce(forceNames.manyBodyPos)))])([text5("many body +: " + showMaybeForceStatus(preview(_manyBodyPosSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(new ToggleForce(forceNames.manyBodyNeg)))])([text5("many body: -" + showMaybeForceStatus(preview(_manyBodyNegSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(new ToggleForce(forceNames.collision)))])([text5("collision: " + showMaybeForceStatus(preview(_collisionSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(Freeze.value))])([text5("Freeze")]), buttonVertical([onClick($$const(Reheat.value))])([text5("Reheat!")])]);
  };
  var lesMisNotebook = /* @__PURE__ */ function() {
    return [new Blurb("This example introduces a new capability, signalled by the SimulationM\n    constraint on the function. This monad runs with a D3 Simulation engine in its\n    State. This allows us to let the simulation engine do the layout, we provide\n    the nodes and (optionally) links and configure the simulation with additional\n    forces. "), new RenderWithState(controls2), new Blurb(' From the D3 docs: "This module implements a velocity Verlet numerical\n  integrator for simulating physical forces on particles. The simulation is\n  simplified: it assumes a constant unit time step \u0394t = 1 for each step, and a\n  constant unit mass m = 1 for all particles. As a result, a force F acting on a\n  particle is equivalent to a constant acceleration a over the time interval \u0394t,\n  and can be simulated simply by adding to the particle\u2019s velocity, which is then\n  added to the particle\u2019s position.""\n\n  '), new SnippetFile("LesMisScript"), new SnippetFile("LesMisHandleActions"), new SnippetFile("LesMisAccessors")];
  }();
  var toggleForceByName = function(dictMonadState) {
    var modifying3 = modifying(dictMonadState);
    var pure21 = pure(dictMonadState.Monad0().Applicative0());
    return function(name17) {
      if (name17 === forceNames.manyBodyNeg) {
        return modifying3(function($129) {
          return _forceStatuses1(_manyBodyNegSetting2($129));
        })(toggleForceStatus);
      }
      ;
      if (name17 === forceNames.manyBodyPos) {
        return modifying3(function($130) {
          return _forceStatuses1(_manyBodyPosSetting2($130));
        })(toggleForceStatus);
      }
      ;
      if (name17 === forceNames.collision) {
        return modifying3(function($131) {
          return _forceStatuses1(_collisionSetting2($131));
        })(toggleForceStatus);
      }
      ;
      if (name17 === forceNames.links) {
        return modifying3(function($132) {
          return _forceStatuses1(_linksSetting2($132));
        })(toggleForceStatus);
      }
      ;
      if (otherwise) {
        return pure21(unit);
      }
      ;
      throw new Error("Failed pattern match at Stories.LesMis (line 96, column 1 - line 96, column 70): " + [name17.constructor.name]);
    };
  };
  var handleAction2 = function(dictBind) {
    var bind18 = bind(dictBind);
    var substituteSnippetCells2 = substituteSnippetCells(dictBind);
    var discard27 = discard11(dictBind);
    var runWithD3_Simulation3 = runWithD3_Simulation(dictBind);
    return function(dictMonadAff) {
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var Applicative0 = MonadEffect0.Monad0().Applicative0();
      var traverse12 = traverse3(Applicative0);
      var substituteSnippetCells1 = substituteSnippetCells2(dictMonadAff);
      var liftAff2 = liftAff(dictMonadAff);
      var pure21 = pure(Applicative0);
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var substituteSnippetCells22 = substituteSnippetCells1(dictMonadState);
        var assign4 = assign2(dictMonadState);
        var runWithD3_Simulation1 = runWithD3_Simulation3(dictMonadState)(MonadEffect0);
        var toggleForceByName1 = toggleForceByName(dictMonadState);
        return function(v) {
          if (v instanceof ToggleCard2) {
            return modifying3(v.value0(strongFn))(not6);
          }
          ;
          if (v instanceof Initialize3) {
            return bind18(traverse12(substituteSnippetCells22)(lesMisNotebook))(function(notebook$prime) {
              return discard27(assign4(_notebook1)(notebook$prime))(function() {
                return bind18(liftAff2(get4(string)("./data/miserables.json")))(function(response) {
                  var graph = readGraphFromFileContents(response);
                  return discard27(modifying3(function() {
                    var $133 = _forceStatus2(forceNames.center);
                    return function($134) {
                      return _forceStatuses1($133($134));
                    };
                  }())($$const(ForceActive.value)))(function() {
                    return discard27(modifying3(function() {
                      var $135 = _forceStatus2(forceNames.manyBodyNeg);
                      return function($136) {
                        return _forceStatuses1($135($136));
                      };
                    }())($$const(ForceActive.value)))(function() {
                      return discard27(modifying3(function() {
                        var $137 = _forceStatus2(forceNames.collision);
                        return function($138) {
                          return _forceStatuses1($137($138));
                        };
                      }())($$const(ForceActive.value)))(function() {
                        return discard27(modifying3(function() {
                          var $139 = _forceStatus2(forceNames.links);
                          return function($140) {
                            return _forceStatuses1($139($140));
                          };
                        }())($$const(ForceActive.value)))(function() {
                          return discard27(modifying3(function() {
                            var $141 = _forceStatus2(forceNames.manyBodyPos);
                            return function($142) {
                              return _forceStatuses1($141($142));
                            };
                          }())($$const(ForceDisabled.value)))(function() {
                            return runWithD3_Simulation1(bind7(use2(_forceStatuses2))(function(statuses) {
                              return discard13(actualizeForces2(statuses))(function() {
                                return draw2(graph)("div.svg-container");
                              });
                            }));
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          }
          ;
          if (v instanceof Finalize3) {
            return pure21(unit);
          }
          ;
          if (v instanceof ToggleForce) {
            return discard27(toggleForceByName1(v.value0))(function() {
              return runWithD3_Simulation1(bind7(use2(_forceStatuses2))(function(statuses) {
                return discard13(actualizeForces2(statuses))(function() {
                  return discard13(setConfigVariable2(new Alpha(0.7)))(function() {
                    return start3;
                  });
                });
              }));
            });
          }
          ;
          if (v instanceof Freeze) {
            return runWithD3_Simulation1(setConfigVariable2(new Alpha(0)));
          }
          ;
          if (v instanceof Reheat) {
            return runWithD3_Simulation1(discard13(setConfigVariable2(new Alpha(0.7)))(function() {
              return start3;
            }));
          }
          ;
          throw new Error("Failed pattern match at Stories.LesMis (line 159, column 16 - line 196, column 12): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction12 = /* @__PURE__ */ handleAction2(bindHalogenM);
  var _code2 = function(dictStrong) {
    var $143 = _panels2(dictStrong);
    var $144 = prop7($$Proxy.value)(dictStrong);
    return function($145) {
      return $143($144($145));
    };
  };
  var _code12 = /* @__PURE__ */ _code2(strongForget);
  var component2 = function(dictMonadAff) {
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-code")])([field_2({
        label: text5("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked2(toBoolean(view(_code12)(state3))), onChange(function(v) {
        return new ToggleCard2(function(dictStrong) {
          return _code2(dictStrong);
        });
      })])]), content_(view(_code12)(state3))(renderNotebook(state3.forceStatuses)(state3.notebook))]), div2([tailwindClass("svg-container")])([])]);
    };
    var initialState = {
      simulation: initialSimulationState(forceLibrary),
      panels: {
        code: Expanded.value
      },
      notebook: lesMisNotebook,
      forceStatuses: getStatusMap(forceLibrary)
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleAction: handleAction12(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize3.value),
        finalize: new Just(Finalize3.value)
      })
    });
  };

  // output/D3.Examples.MetaTree.Unsafe/index.js
  var unboxD3TreeNode = function(v) {
    return v;
  };
  var coerceToTreeNode = unsafeCoerce2;

  // output/D3.Layouts.Hierarchical/foreign.js
  function readJSON_(filecontents) {
    return JSON.parse(filecontents);
  }

  // output/D3.Layouts.Hierarchical/index.js
  var toAttr3 = /* @__PURE__ */ toAttr(toAttrStringFn);
  var pure17 = /* @__PURE__ */ pure(applicativeAff);
  var bind8 = /* @__PURE__ */ bind(bindAff);
  var rmap4 = /* @__PURE__ */ rmap2(bifunctorEither);
  var verticalLink = /* @__PURE__ */ function() {
    return new AttrT(new AttributeSetter("d", toAttr3(linkVertical_)));
  }();
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
            return pure17({
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
  var horizontalLink = /* @__PURE__ */ function() {
    return new AttrT(new AttributeSetter("d", toAttr3(linkHorizontal_)));
  }();
  var horizontalClusterLink = function(yOffset) {
    return new AttrT(new AttributeSetter("d", toAttr3(linkClusterHorizontal_(yOffset))));
  };
  var getTreeViaAJAX = function(url) {
    return bind8(get4(string)(url))(function(result) {
      return pure17(rmap4(function(v) {
        return readJSON_(v.body);
      })(result));
    });
  };

  // output/D3.Examples.MetaTree/index.js
  var show9 = /* @__PURE__ */ show(showNumber);
  var width9 = /* @__PURE__ */ width8(toAttrNumber);
  var height9 = /* @__PURE__ */ height8(toAttrNumber);
  var classed5 = /* @__PURE__ */ classed(toAttrString);
  var fontFamily2 = /* @__PURE__ */ fontFamily(toAttrString);
  var fontSize3 = /* @__PURE__ */ fontSize(toAttrNumber);
  var discard14 = /* @__PURE__ */ discard(discardUnit);
  var strokeWidth3 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var strokeColor3 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeOpacity3 = /* @__PURE__ */ strokeOpacity(toAttrNumber);
  var fill4 = /* @__PURE__ */ fill(toAttrString);
  var radius4 = /* @__PURE__ */ radius(toAttrNumber);
  var x5 = /* @__PURE__ */ x(toAttrNumber);
  var y5 = /* @__PURE__ */ y(toAttrNumber);
  var textAnchor2 = /* @__PURE__ */ textAnchor(toAttrString);
  var text8 = /* @__PURE__ */ text6(toAttrStringFn);
  var liftEffect8 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var datum_2 = {
    x: function($55) {
      return function(v) {
        return v.x;
      }(unboxD3TreeNode(coerceToTreeNode($55)));
    },
    y: function($56) {
      return function(v) {
        return v.y;
      }(unboxD3TreeNode(coerceToTreeNode($56)));
    },
    symbol: function($57) {
      return function(v) {
        return v.data.symbol;
      }(unboxD3TreeNode(coerceToTreeNode($57)));
    },
    param1: function($58) {
      return function(v) {
        return v.data.param1;
      }(unboxD3TreeNode(coerceToTreeNode($58)));
    },
    positionXY: function(d2) {
      return "translate(" + (show9(datum_2.x(d2)) + ("," + (show9(datum_2.y(d2)) + ")")));
    }
  };
  var draw3 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard14(dictBind);
    return function(dictSelectionM) {
      var attach2 = attach(dictSelectionM);
      var appendTo2 = appendTo(dictSelectionM);
      var simpleJoin2 = simpleJoin(dictSelectionM);
      var setAttributes2 = setAttributes(dictSelectionM);
      var pure111 = pure(dictSelectionM.Monad0().Applicative0());
      return function(v) {
        return function(tree2) {
          var numberOfLevels = hNodeHeight_(tree2) + 1;
          var spacing = {
            interChild: v.value0 / 5,
            interLevel: v.value1 / numberOfLevels
          };
          var layoutFn = treeSetNodeSize_(getLayout(TidyTree.value))([spacing.interChild, spacing.interLevel]);
          var laidOutRoot_ = runLayoutFn_(layoutFn)(tree2);
          var v1 = treeMinMax_(laidOutRoot_);
          var yExtent = abs(v1.yMax - v1.yMin);
          var xExtent = abs(v1.xMax - v1.xMin);
          var vtreeYOffset = abs(v.value1 - yExtent) / 2;
          var pad = function(n) {
            return n * 1.2;
          };
          var vtreeXOffset = pad(v1.xMin);
          return bind18(attach2(".svg-container"))(function(root) {
            return bind18(appendTo2(root)(Svg.value)([viewBox(vtreeXOffset)(-vtreeYOffset)(pad(xExtent))(pad(yExtent)), preserveAspectRatio(new AspectRatio(XMin.value, YMid.value, Meet.value)), width9(v.value0), height9(v.value1), classed5("metatree")]))(function(svg2) {
              return bind18(appendTo2(svg2)(Group.value)([fontFamily2("sans-serif"), fontSize3(18)]))(function(container) {
                return bind18(appendTo2(container)(Group.value)([classed5("links")]))(function(links) {
                  return bind18(appendTo2(container)(Group.value)([classed5("nodes")]))(function(nodes) {
                    return bind18(simpleJoin2(links)(Path.value)(links_(tree2))(keyIsID_))(function(theLinks_) {
                      return discard111(setAttributes2(theLinks_)([strokeWidth3(1.5), strokeColor3("black"), strokeOpacity3(0.4), fill4("none"), verticalLink]))(function() {
                        return bind18(simpleJoin2(nodes)(Group.value)(descendants_(tree2))(keyIsID_))(function(nodeJoin_) {
                          return discard111(setAttributes2(nodeJoin_)([transform([datum_2.positionXY])]))(function() {
                            return bind18(appendTo2(nodeJoin_)(Circle.value)([fill4("blue"), radius4(20), strokeColor3("white"), strokeWidth3(3)]))(function(theNodes) {
                              return bind18(appendTo2(nodeJoin_)(Text2.value)([x5(0), y5(3), textAnchor2("middle"), text8(datum_2.symbol), fill4("white")]))(function(labelsWhite) {
                                return bind18(appendTo2(nodeJoin_)(Text2.value)([x5(22), y5(3), textAnchor2("start"), text8(datum_2.param1), fill4("gray")]))(function(labelsGray) {
                                  return pure111(svg2);
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
          });
        };
      };
    };
  };
  var draw1 = /* @__PURE__ */ draw3(bindD3M)(d3TaglessD3M);
  var drawTree = function(treeModel) {
    return liftEffect8(function __do3() {
      var widthHeight = getWindowWidthHeight();
      var tree2 = hierarchyFromJSON_(treeModel.json);
      var v = runD3M(draw1(widthHeight)(tree2))();
      return unit;
    });
  };

  // output/D3.Examples.Tree.Draw/index.js
  var eq12 = /* @__PURE__ */ eq(eqTreeLayout);
  var append9 = /* @__PURE__ */ append(semigroupArray);
  var classed6 = /* @__PURE__ */ classed(toAttrString);
  var width10 = /* @__PURE__ */ width8(toAttrNumber);
  var height10 = /* @__PURE__ */ height8(toAttrNumber);
  var fontFamily3 = /* @__PURE__ */ fontFamily(toAttrString);
  var fontSize4 = /* @__PURE__ */ fontSize(toAttrNumber);
  var discard15 = /* @__PURE__ */ discard(discardUnit);
  var strokeWidth4 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var strokeColor4 = /* @__PURE__ */ strokeColor(toAttrString);
  var strokeOpacity4 = /* @__PURE__ */ strokeOpacity(toAttrNumber);
  var fill5 = /* @__PURE__ */ fill(toAttrString);
  var fill1 = /* @__PURE__ */ fill(toAttrStringFn);
  var radius5 = /* @__PURE__ */ radius(toAttrNumber);
  var dy2 = /* @__PURE__ */ dy(toAttrNumber);
  var x6 = /* @__PURE__ */ x(toAttrNumberFn);
  var textAnchor3 = /* @__PURE__ */ textAnchor(toAttrStringFn);
  var text9 = /* @__PURE__ */ text6(toAttrStringFn);
  var treeDatum_ = {
    depth: function($50) {
      return function(v) {
        return v.depth;
      }(unboxD3TreeNode(coerceToTreeNode($50)));
    },
    height: function($51) {
      return function(v) {
        return v.height;
      }(unboxD3TreeNode(coerceToTreeNode($51)));
    },
    x: function($52) {
      return function(v) {
        return v.x;
      }(unboxD3TreeNode(coerceToTreeNode($52)));
    },
    y: function($53) {
      return function(v) {
        return v.y;
      }(unboxD3TreeNode(coerceToTreeNode($53)));
    },
    name: function($54) {
      return function(v) {
        return v.data.name;
      }(unboxD3TreeNode(coerceToTreeNode($54)));
    },
    value: function($55) {
      return getHierarchyValue_(coerceToTreeNode($55));
    },
    hasChildren: function($56) {
      return hasChildren_(coerceToTreeNode($56));
    },
    textAnchor: function(l) {
      return function(d2) {
        if (l instanceof Radial) {
          var $42 = treeDatum_.hasChildren(d2) === treeDatum_.x(d2) < pi;
          if ($42) {
            return "start";
          }
          ;
          return "end";
        }
        ;
        var $43 = treeDatum_.hasChildren(d2);
        if ($43) {
          return "start";
        }
        ;
        return "end";
      };
    },
    textX: function(l) {
      return function(d2) {
        if (l instanceof Radial) {
          var $45 = treeDatum_.hasChildren(d2) === treeDatum_.x(d2) < pi;
          if ($45) {
            return 6;
          }
          ;
          return -6;
        }
        ;
        var $46 = treeDatum_.hasChildren(d2);
        if ($46) {
          return 6;
        }
        ;
        return -6;
      };
    },
    onRHS: function(l) {
      return function(d2) {
        var $47 = eq12(l)(Radial.value) && treeDatum_.x(d2) >= pi;
        if ($47) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var draw4 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard15(dictBind);
    return function(dictSelectionM) {
      var attach2 = attach(dictSelectionM);
      var appendTo2 = appendTo(dictSelectionM);
      var simpleJoin2 = simpleJoin(dictSelectionM);
      var setAttributes2 = setAttributes(dictSelectionM);
      var pure21 = pure(dictSelectionM.Monad0().Applicative0());
      return function(config) {
        return function(tree2) {
          return bind18(attach2(config.selector))(function(root) {
            return bind18(appendTo2(root)(Svg.value)(append9(config.viewbox)([classed6("tree"), width10(config.svg.width), height10(config.svg.height)])))(function(svg2) {
              return bind18(appendTo2(svg2)(Group.value)([fontFamily3("sans-serif"), fontSize4(10)]))(function(container) {
                return bind18(appendTo2(container)(Group.value)([classed6("links")]))(function(links) {
                  return bind18(appendTo2(container)(Group.value)([classed6("nodes")]))(function(nodes) {
                    return bind18(simpleJoin2(links)(Path.value)(links_(tree2))(keyIsID_))(function(theLinks_) {
                      return discard111(setAttributes2(theLinks_)([strokeWidth4(1.5), strokeColor4(config.color), strokeOpacity4(0.4), fill5("none"), config.linkPath]))(function() {
                        return bind18(simpleJoin2(nodes)(Group.value)(descendants_(tree2))(keyIsID_))(function(nodeJoin_) {
                          return discard111(setAttributes2(nodeJoin_)(config.nodeTransform))(function() {
                            return bind18(appendTo2(nodeJoin_)(Circle.value)([fill1(function(v) {
                              var $49 = treeDatum_.hasChildren(v);
                              if ($49) {
                                return "#999";
                              }
                              ;
                              return "#555";
                            }), radius5(2.5), strokeColor4("white")]))(function(theNodes) {
                              return bind18(appendTo2(nodeJoin_)(Text2.value)([dy2(0.31), x6(treeDatum_.textX(config.layout)), textAnchor3(treeDatum_.textAnchor(config.layout)), text9(treeDatum_.name), fill5(config.color)]))(function(theLabels) {
                                return pure21(svg2);
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

  // output/D3Tagless.Capabilities.MetaTree/foreign.js
  function pruneEmptyChildren(node) {
    prune(node);
    return node;
  }
  prune = function(node) {
    if (node.children.length == 0) {
      delete node.children;
    } else {
      node.children.forEach((child) => prune(child));
    }
    ;
  };

  // output/D3Tagless.Capabilities.MetaTree/index.js
  var show10 = /* @__PURE__ */ show(showElement);
  var show14 = /* @__PURE__ */ show(showSelectionAttribute);
  var show23 = /* @__PURE__ */ show(showString);
  var map42 = /* @__PURE__ */ map(functorArray);
  var lookup7 = /* @__PURE__ */ lookup(ordInt);
  var insert8 = /* @__PURE__ */ insert(ordInt);
  var Empty2 = /* @__PURE__ */ function() {
    function Empty3() {
    }
    ;
    Empty3.value = new Empty3();
    return Empty3;
  }();
  var AttachNode = /* @__PURE__ */ function() {
    function AttachNode2(value0) {
      this.value0 = value0;
    }
    ;
    AttachNode2.create = function(value0) {
      return new AttachNode2(value0);
    };
    return AttachNode2;
  }();
  var SelectUnderNode = /* @__PURE__ */ function() {
    function SelectUnderNode2(value0) {
      this.value0 = value0;
    }
    ;
    SelectUnderNode2.create = function(value0) {
      return new SelectUnderNode2(value0);
    };
    return SelectUnderNode2;
  }();
  var AppendNode = /* @__PURE__ */ function() {
    function AppendNode2(value0) {
      this.value0 = value0;
    }
    ;
    AppendNode2.create = function(value0) {
      return new AppendNode2(value0);
    };
    return AppendNode2;
  }();
  var FilterNode = /* @__PURE__ */ function() {
    function FilterNode2(value0) {
      this.value0 = value0;
    }
    ;
    FilterNode2.create = function(value0) {
      return new FilterNode2(value0);
    };
    return FilterNode2;
  }();
  var ModifyNode = /* @__PURE__ */ function() {
    function ModifyNode2(value0) {
      this.value0 = value0;
    }
    ;
    ModifyNode2.create = function(value0) {
      return new ModifyNode2(value0);
    };
    return ModifyNode2;
  }();
  var JoinSimpleNode = /* @__PURE__ */ function() {
    function JoinSimpleNode2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    JoinSimpleNode2.create = function(value0) {
      return function(value1) {
        return new JoinSimpleNode2(value0, value1);
      };
    };
    return JoinSimpleNode2;
  }();
  var UpdateJoinNode = /* @__PURE__ */ function() {
    function UpdateJoinNode2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateJoinNode2.create = function(value0) {
      return new UpdateJoinNode2(value0);
    };
    return UpdateJoinNode2;
  }();
  var OpenJoinNode = /* @__PURE__ */ function() {
    function OpenJoinNode2(value0) {
      this.value0 = value0;
    }
    ;
    OpenJoinNode2.create = function(value0) {
      return new OpenJoinNode2(value0);
    };
    return OpenJoinNode2;
  }();
  var JoinSimpleWithKeyFunctionNode = /* @__PURE__ */ function() {
    function JoinSimpleWithKeyFunctionNode2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    JoinSimpleWithKeyFunctionNode2.create = function(value0) {
      return function(value1) {
        return new JoinSimpleWithKeyFunctionNode2(value0, value1);
      };
    };
    return JoinSimpleWithKeyFunctionNode2;
  }();
  var SplitJoinCloseWithKeyFunctionNode = /* @__PURE__ */ function() {
    function SplitJoinCloseWithKeyFunctionNode2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SplitJoinCloseWithKeyFunctionNode2.create = function(value0) {
      return function(value1) {
        return new SplitJoinCloseWithKeyFunctionNode2(value0, value1);
      };
    };
    return SplitJoinCloseWithKeyFunctionNode2;
  }();
  var OnNode = /* @__PURE__ */ function() {
    function OnNode2(value0) {
      this.value0 = value0;
    }
    ;
    OnNode2.create = function(value0) {
      return new OnNode2(value0);
    };
    return OnNode2;
  }();
  var AttrNode = /* @__PURE__ */ function() {
    function AttrNode2(value0) {
      this.value0 = value0;
    }
    ;
    AttrNode2.create = function(value0) {
      return new AttrNode2(value0);
    };
    return AttrNode2;
  }();
  var OrderNode = /* @__PURE__ */ function() {
    function OrderNode2(value0) {
      this.value0 = value0;
    }
    ;
    OrderNode2.create = function(value0) {
      return new OrderNode2(value0);
    };
    return OrderNode2;
  }();
  var OnEventNode = /* @__PURE__ */ function() {
    function OnEventNode2(value0) {
      this.value0 = value0;
    }
    ;
    OnEventNode2.create = function(value0) {
      return new OnEventNode2(value0);
    };
    return OnEventNode2;
  }();
  var TransitionNode = /* @__PURE__ */ function() {
    function TransitionNode2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TransitionNode2.create = function(value0) {
      return function(value1) {
        return new TransitionNode2(value0, value1);
      };
    };
    return TransitionNode2;
  }();
  var RemoveNode = /* @__PURE__ */ function() {
    function RemoveNode2() {
    }
    ;
    RemoveNode2.value = new RemoveNode2();
    return RemoveNode2;
  }();
  var ScriptTree = /* @__PURE__ */ function() {
    function ScriptTree2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ScriptTree2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ScriptTree2(value0, value1, value22);
        };
      };
    };
    return ScriptTree2;
  }();
  var tag = function(s) {
    return "<" + (s + ">");
  };
  var showAsSymbol = function(v) {
    if (v instanceof Empty2) {
      return {
        name: "Empty",
        symbol: "",
        param1: "",
        param2: ""
      };
    }
    ;
    if (v instanceof RemoveNode) {
      return {
        name: "Remove",
        symbol: "x",
        param1: "",
        param2: ""
      };
    }
    ;
    if (v instanceof AttachNode) {
      return {
        name: "Attach",
        symbol: "a",
        param1: "",
        param2: ""
      };
    }
    ;
    if (v instanceof SelectUnderNode) {
      return {
        name: "SelectUnder",
        symbol: "s",
        param1: tag(v.value0),
        param2: ""
      };
    }
    ;
    if (v instanceof AppendNode) {
      return {
        name: "Append",
        symbol: "+",
        param1: tag(show10(v.value0)),
        param2: ""
      };
    }
    ;
    if (v instanceof FilterNode) {
      return {
        name: "Filter",
        symbol: "/",
        param1: tag(v.value0),
        param2: ""
      };
    }
    ;
    if (v instanceof ModifyNode) {
      return {
        name: "Modify",
        symbol: "->",
        param1: "",
        param2: ""
      };
    }
    ;
    if (v instanceof JoinSimpleNode) {
      return {
        name: "JoinSimple",
        symbol: "<+>",
        param1: tag(show10(v.value0)),
        param2: ""
      };
    }
    ;
    if (v instanceof UpdateJoinNode) {
      return {
        name: "SplitJoinClose",
        symbol: "<+>",
        param1: tag(show10(v.value0)),
        param2: ""
      };
    }
    ;
    if (v instanceof OpenJoinNode) {
      return {
        name: "SplitJoinClose",
        symbol: "<+>",
        param1: tag(v.value0),
        param2: ""
      };
    }
    ;
    if (v instanceof JoinSimpleWithKeyFunctionNode) {
      return {
        name: "JoinSimpleK",
        symbol: "<+>",
        param1: tag(show10(v.value0)),
        param2: ""
      };
    }
    ;
    if (v instanceof SplitJoinCloseWithKeyFunctionNode) {
      return {
        name: "SplitJoinCloseK",
        symbol: "<+>",
        param1: tag(show10(v.value0)),
        param2: ""
      };
    }
    ;
    if (v instanceof OnNode && v.value0 instanceof Zoom) {
      return {
        name: "Zoom",
        symbol: "z",
        param1: "",
        param2: ""
      };
    }
    ;
    if (v instanceof OnNode && v.value0 instanceof Drag) {
      return {
        name: "Drag",
        symbol: "drag",
        param1: "",
        param2: ""
      };
    }
    ;
    if (v instanceof AttrNode) {
      return {
        name: "Attr",
        symbol: "attr",
        param1: show14(v.value0),
        param2: ""
      };
    }
    ;
    if (v instanceof OrderNode) {
      return {
        name: "Order",
        symbol: "order",
        param1: show23(v.value0),
        param2: ""
      };
    }
    ;
    if (v instanceof OnEventNode) {
      return {
        name: "OnEvent",
        symbol: "on",
        param1: "",
        param2: ""
      };
    }
    ;
    if (v instanceof TransitionNode) {
      return {
        name: "Transition",
        symbol: "T",
        param1: "",
        param2: ""
      };
    }
    ;
    throw new Error("Failed pattern match at D3Tagless.Capabilities.MetaTree (line 69, column 3 - line 87, column 112): " + [v.constructor.name]);
  };
  var scriptTreeToJSON = function(v) {
    var go2 = function(id4) {
      var children2 = map42(snd)(filter2(function(v12) {
        return v12.value0 === id4;
      })(v.value2));
      var v1 = showAsSymbol(fromMaybe(Empty2.value)(lookup7(id4)(v.value1)));
      return {
        name: v1.name,
        symbol: v1.symbol,
        param1: v1.param1,
        param2: v1.param2,
        children: map42(go2)(children2)
      };
    };
    return pruneEmptyChildren(go2(0));
  };
  var monadStateD3MetaTreeM = /* @__PURE__ */ monadStateStateT(monadEffect);
  var get5 = /* @__PURE__ */ get(monadStateD3MetaTreeM);
  var modify_3 = /* @__PURE__ */ modify_(monadStateD3MetaTreeM);
  var monadD3MetaTreeM = /* @__PURE__ */ monadStateT(monadEffect);
  var initialMetaTree = /* @__PURE__ */ function() {
    return new ScriptTree(0, empty2, []);
  }();
  var runMetaTree = function(v) {
    return runStateT(v)(initialMetaTree);
  };
  var bindD3MetaTreeM = /* @__PURE__ */ bindStateT(monadEffect);
  var bind9 = /* @__PURE__ */ bind(bindD3MetaTreeM);
  var discard16 = /* @__PURE__ */ discard(discardUnit)(bindD3MetaTreeM);
  var applicativeD3MetaTreeM = /* @__PURE__ */ applicativeStateT(monadEffect);
  var pure18 = /* @__PURE__ */ pure(applicativeD3MetaTreeM);
  var insertInScriptTree = function(parentID) {
    return function(v) {
      if (v instanceof TransitionNode) {
        return bind9(get5)(function(v1) {
          return discard16(modify_3(function(s) {
            return new ScriptTree(v1.value0 + 1 | 0, insert8(v1.value0)(v)(v1.value1), cons3(new Tuple(parentID, v1.value0 + 1 | 0))(v1.value2));
          }))(function() {
            return pure18(unit);
          });
        });
      }
      ;
      return bind9(get5)(function(v1) {
        return discard16(modify_3(function(s) {
          return new ScriptTree(v1.value0 + 1 | 0, insert8(v1.value0)(v)(v1.value1), cons3(new Tuple(parentID, v1.value0 + 1 | 0))(v1.value2));
        }))(function() {
          return pure18(unit);
        });
      });
    };
  };
  var d3Tagless = {
    appendTo: function(nodeID) {
      return function(element3) {
        return function(attributes) {
          return discard16(insertInScriptTree(nodeID)(new AppendNode(element3)))(function() {
            return bind9(get5)(function(v) {
              return pure18(v.value0);
            });
          });
        };
      };
    },
    selectUnder: function(nodeID) {
      return function(selector) {
        return discard16(insertInScriptTree(nodeID)(new SelectUnderNode(selector)))(function() {
          return bind9(get5)(function(v) {
            return pure18(v.value0);
          });
        });
      };
    },
    attach: function(selector) {
      return discard16(insertInScriptTree(0)(new AttachNode(selector)))(function() {
        return pure18(1);
      });
    },
    filterSelection: function(nodeID) {
      return function(selector) {
        return discard16(insertInScriptTree(nodeID)(new FilterNode(selector)))(function() {
          return bind9(get5)(function(v) {
            return pure18(v.value0);
          });
        });
      };
    },
    mergeSelections: function(a2) {
      return function(b2) {
        return bind9(get5)(function(v) {
          return pure18(v.value0);
        });
      };
    },
    setAttributes: function(nodeID) {
      return function(attributes) {
        return discard16(insertInScriptTree(nodeID)(new ModifyNode(attributes)))(function() {
          return pure18(unit);
        });
      };
    },
    on: function(nodeID) {
      return function(behavior) {
        return discard16(insertInScriptTree(nodeID)(new OnNode(behavior)))(function() {
          return pure18(unit);
        });
      };
    },
    openSelection: function(selection) {
      return function(selector) {
        return bind9(get5)(function(v) {
          return pure18(v.value0);
        });
      };
    },
    simpleJoin: function(nodeID) {
      return function(e) {
        return function(ds) {
          return function(k) {
            return bind9(get5)(function(v) {
              return discard16(insertInScriptTree(nodeID)(new JoinSimpleWithKeyFunctionNode(e, k)))(function() {
                return pure18(v.value0);
              });
            });
          };
        };
      };
    },
    updateJoin: function(nodeID) {
      return function(e) {
        return function(ds) {
          return function(k) {
            return bind9(get5)(function(v) {
              return discard16(insertInScriptTree(nodeID)(new UpdateJoinNode(e)))(function() {
                return pure18({
                  enter: v.value0,
                  exit: v.value0,
                  update: v.value0
                });
              });
            });
          };
        };
      };
    },
    Monad0: function() {
      return monadD3MetaTreeM;
    }
  };

  // output/D3Tagless.Capabilities.String/foreign.js
  function showAddTransition_(selection) {
    return (transition) => {
      if (transition.name == "") {
        const statement1 = `	d3addTransition: ${selection}.transition(${transition})`;
        var statement2 = "";
        var statement3 = "";
        if (transition.duration != 0) {
          statement2 = `transition.duration(${transition.duration})`;
        }
        if (transition.delay != 0) {
          statement3 = `		transition.delay(${transition.delay})`;
        }
        return statement1 + statement2 + statement3;
      } else {
        return `	d3addNamedTransition: ${selection}.transition(${transition})`;
      }
    };
  }
  function showRemoveSelection_(selection) {
    return `	d3Remove: ${selection}.remove()`;
  }
  function showSetAttr_(name17) {
    return (value15) => (selection) => {
      return `	${selection}.attr(${name17}, ${value15})`;
    };
  }
  function showSetText_(value15) {
    return (selection) => {
      return `	${selection}.text(${value15})`;
    };
  }
  function showSetHTML_(value15) {
    return (selection) => {
      return `	${selection}.html(${value15})`;
    };
  }
  function showSetProperty_(value15) {
    return (selection) => {
      return `	${selection}.property(${value15})`;
    };
  }
  function showSetOrdering_(ordering) {
    return (selection) => {
      return `	${selection}.${ordering}()`;
    };
  }

  // output/D3Tagless.Capabilities.String/index.js
  var show11 = /* @__PURE__ */ show(showOrderingAttribute);
  var show15 = /* @__PURE__ */ show(showString);
  var show24 = /* @__PURE__ */ show(showMouseEvent);
  var show33 = /* @__PURE__ */ show(showElement);
  var runPrinter = function(v) {
    return function(initialString) {
      return runStateT(v)(initialString);
    };
  };
  var monadStateD3PrinterM = /* @__PURE__ */ monadStateStateT(monadEffect);
  var modify_4 = /* @__PURE__ */ modify_(monadStateD3PrinterM);
  var monadD3PrinterM = /* @__PURE__ */ monadStateT(monadEffect);
  var bindD3PrinterM = /* @__PURE__ */ bindStateT(monadEffect);
  var discard17 = /* @__PURE__ */ discard(discardUnit)(bindD3PrinterM);
  var applySelectionAttributeString = function(selection) {
    return function(v) {
      if (v instanceof AttrT) {
        return showSetAttr_(v.value0.value0)(unboxAttr(v.value0.value1))(selection);
      }
      ;
      if (v instanceof TextT) {
        return showSetText_(unboxAttr(v.value0.value1))(selection);
      }
      ;
      if (v instanceof PropertyT) {
        return showSetProperty_(unboxAttr(v.value0.value1))(selection);
      }
      ;
      if (v instanceof HTMLT) {
        return showSetHTML_(unboxAttr(v.value0.value1))(selection);
      }
      ;
      if (v instanceof RemoveT) {
        return showRemoveSelection_(selection);
      }
      ;
      if (v instanceof OrderingT) {
        return showSetOrdering_(show11(v.value0))(selection);
      }
      ;
      if (v instanceof TransitionT) {
        var tString = showAddTransition_(selection)(v.value1);
        return foldl2(applySelectionAttributeString)(tString)(v.value0);
      }
      ;
      if (v instanceof OnT) {
        return show15("event handler for ") + (show24(v.value0) + " has been set");
      }
      ;
      if (v instanceof OnT$prime) {
        return show15("effectful event handler for ") + (show24(v.value0) + " has been set");
      }
      ;
      throw new Error("Failed pattern match at D3Tagless.Capabilities.String (line 80, column 3 - line 98, column 75): " + [v.constructor.name]);
    };
  };
  var applicativeD3PrinterM = /* @__PURE__ */ applicativeStateT(monadEffect);
  var pure19 = /* @__PURE__ */ pure(applicativeD3PrinterM);
  var d3Tagless2 = {
    attach: function(selector) {
      return discard17(modify_4(function(s) {
        return s + ("\nattaching to " + (selector + " in DOM\n"));
      }))(function() {
        return pure19("attach");
      });
    },
    selectUnder: function(selection) {
      return function(selector) {
        return discard17(modify_4(function(s) {
          return s + ("\nsub-selection " + (selector + "\n"));
        }))(function() {
          return pure19("attach");
        });
      };
    },
    appendTo: function(selection) {
      return function(element3) {
        return function(attributes) {
          var attributeString = foldl2(applySelectionAttributeString)(selection)(attributes);
          return discard17(modify_4(function(s) {
            return s + ("\nappending " + (show33(element3) + (" to " + (selection + ("\n" + attributeString)))));
          }))(function() {
            return pure19("append");
          });
        };
      };
    },
    filterSelection: function(selection) {
      return function(selector) {
        return discard17(modify_4(function(s) {
          return s + ("\nfiltering selection using " + (show15(selector) + "\n"));
        }))(function() {
          return pure19("filter");
        });
      };
    },
    mergeSelections: function(a2) {
      return function(b2) {
        return discard17(modify_4(function(s) {
          return s + "merging selections\n";
        }))(function() {
          return pure19("merge");
        });
      };
    },
    setAttributes: function(selection) {
      return function(attributes) {
        var attributeString = foldl2(applySelectionAttributeString)(selection)(attributes);
        return discard17(modify_4(function(s) {
          return s + ("\nmodifying " + (selection + ("\n" + attributeString)));
        }))(function() {
          return pure19(unit);
        });
      };
    },
    on: function(selection) {
      return function(v) {
        if (v instanceof Drag) {
          return discard17(modify_4(function(s) {
            return s + ("\nadding drag behavior to " + selection);
          }))(function() {
            return pure19(unit);
          });
        }
        ;
        if (v instanceof Zoom) {
          return discard17(modify_4(function(s) {
            return s + ("\nadding drag behavior to " + selection);
          }))(function() {
            return pure19(unit);
          });
        }
        ;
        throw new Error("Failed pattern match at D3Tagless.Capabilities.String (line 30, column 1 - line 74, column 60): " + [selection.constructor.name, v.constructor.name]);
      };
    },
    openSelection: function(selection) {
      return function(selector) {
        return discard17(modify_4(function(s) {
          return s + ("\nopening a selection using " + show15(selector));
        }))(function() {
          return pure19("openSelection");
        });
      };
    },
    simpleJoin: function(selection) {
      return function(e) {
        return function(ds) {
          return function(k) {
            return discard17(modify_4(function(s) {
              return s + ("\nentering a " + (show33(e) + " for each datum"));
            }))(function() {
              return pure19("join");
            });
          };
        };
      };
    },
    updateJoin: function(selection) {
      return function(e) {
        return function(ds) {
          return function(k) {
            return discard17(modify_4(function(s) {
              return s + ("\nentering a " + (show33(e) + " for each datum"));
            }))(function() {
              return pure19({
                enter: "enter",
                exit: "exit",
                update: "update"
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

  // output/D3.Examples.Tree.Configure/index.js
  var show16 = /* @__PURE__ */ show(showNumber);
  var eq5 = /* @__PURE__ */ eq(eqTreeLayout);
  var liftEffect9 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var rotateRadialLabels = function(d2) {
    return "rotate(" + (function() {
      var $27 = treeDatum_.onRHS(Radial.value)(d2);
      if ($27) {
        return "180";
      }
      ;
      return "0";
    }() + ")");
  };
  var radialTranslate = function(d2) {
    return "translate(" + (show16(treeDatum_.y(d2)) + ",0)");
  };
  var radialRotate = function(x10) {
    return show16(x10 * 180 / pi - 90);
  };
  var radialRotateCommon = function(d2) {
    return "rotate(" + (radialRotate(treeDatum_.x(d2)) + ")");
  };
  var positionXYreflected = function(d2) {
    return "translate(" + (show16(treeDatum_.y(d2)) + ("," + (show16(treeDatum_.x(d2)) + ")")));
  };
  var positionXY = function(d2) {
    return "translate(" + (show16(treeDatum_.x(d2)) + ("," + (show16(treeDatum_.y(d2)) + ")")));
  };
  var configureAndRunScript = function(dictBind) {
    var draw5 = draw4(dictBind);
    return function(dictSelectionM) {
      var draw12 = draw5(dictSelectionM);
      return function(v) {
        return function(model) {
          return function(selector) {
            var svg2 = {
              width: v.value0,
              height: v.value1
            };
            var root = hierarchyFromJSON_(model.json);
            var numberOfLevels = hNodeHeight_(root) + 1;
            var spacing = function() {
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Horizontal) {
                return {
                  interChild: 10,
                  interLevel: svg2.width / numberOfLevels
                };
              }
              ;
              if (model.treeType instanceof Dendrogram && model.treeLayout instanceof Vertical) {
                return {
                  interChild: 10,
                  interLevel: svg2.height / numberOfLevels
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
                  interLevel: svg2.width / numberOfLevels
                };
              }
              ;
              if (model.treeType instanceof TidyTree && model.treeLayout instanceof Vertical) {
                return {
                  interChild: 10,
                  interLevel: svg2.height / numberOfLevels
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
            }();
            var layout = function() {
              var $33 = eq5(model.treeLayout)(Radial.value);
              if ($33) {
                return treeSetSeparation_(treeSetSize_(getLayout(model.treeType))([2 * pi, svg2.width / 2 - 100]))(radialSeparation);
              }
              ;
              return treeSetNodeSize_(getLayout(model.treeType))([spacing.interChild, spacing.interLevel]);
            }();
            var laidOutRoot_ = runLayoutFn_(layout)(root);
            var v1 = treeMinMax_(laidOutRoot_);
            var yExtent = abs(v1.yMax - v1.yMin);
            var xExtent = abs(v1.xMax - v1.xMin);
            var vtreeYOffset = abs(v.value1 - yExtent) / 2;
            var radialExtent = 2 * v1.yMax;
            var pad = function(n) {
              return n * 1.2;
            };
            var nodeTransform = function() {
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 116, column 7 - line 123, column 108): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            }();
            var linkPath = function() {
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 106, column 7 - line 113, column 71): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            }();
            var viewbox = function() {
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 97, column 7 - line 103, column 78): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            }();
            var color = d3SchemeCategory10N_(function() {
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 126, column 7 - line 133, column 38): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            }());
            return draw12({
              spacing,
              viewbox,
              selector,
              linkPath,
              nodeTransform,
              color,
              layout: model.treeLayout,
              svg: svg2
            })(laidOutRoot_);
          };
        };
      };
    };
  };
  var configureAndRunScript1 = /* @__PURE__ */ configureAndRunScript(bindD3M)(d3TaglessD3M);
  var configureAndRunScript2 = /* @__PURE__ */ configureAndRunScript(bindD3MetaTreeM)(d3Tagless);
  var configureAndRunScript3 = /* @__PURE__ */ configureAndRunScript(bindD3PrinterM)(d3Tagless2);
  var drawTree2 = function(treeModel) {
    return function(selector) {
      return liftEffect9(function __do3() {
        var widthHeight = getWindowWidthHeight();
        var v = runD3M(configureAndRunScript1(widthHeight)(treeModel)(selector))();
        return unit;
      });
    };
  };
  var getMetaTreeJSON = function(treeModel) {
    return liftEffect9(function __do3() {
      var widthHeight = getWindowWidthHeight();
      var metaScript = runMetaTree(configureAndRunScript2(widthHeight)(treeModel)("MetaTree root> "))();
      var v = snd(metaScript);
      var v1 = toUnfoldable5(v.value1);
      var treeified = snd(metaScript);
      return scriptTreeToJSON(treeified);
    });
  };
  var getPrintTree = function(treeModel) {
    return liftEffect9(function __do3() {
      var widthHeight = getWindowWidthHeight();
      var printedScript = runPrinter(configureAndRunScript3(widthHeight)(treeModel)("Printer Interpreter Root> "))("Tree Script")();
      return snd(printedScript);
    });
  };

  // output/D3Tagless.Utility/index.js
  var spy6 = /* @__PURE__ */ spy();
  var removeExistingSVG = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(rootSelector) {
      var root = d3SelectFirstInDOM_(rootSelector);
      var previous = d3SelectionSelect_(rootSelector + " svg")(root);
      return pure21(function() {
        var v = d3SelectionIsEmpty_(previous);
        if (v) {
          return spy6("no previous SVG to remove")(previous);
        }
        ;
        if (!v) {
          return spy6("removed previous SVG")(d3RemoveSelection_(previous));
        }
        ;
        throw new Error("Failed pattern match at D3Tagless.Utility (line 18, column 5 - line 20, column 72): " + [v.constructor.name]);
      }());
    };
  };

  // output/Stories.MetaTree/index.js
  var bindFlipped9 = /* @__PURE__ */ bindFlipped(bindAff);
  var makeModel2 = /* @__PURE__ */ makeModel(bindAff)(monadEffectAff);
  var prop8 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "handler";
    }
  })()();
  var prop14 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "evaluator";
    }
  })()();
  var prop24 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "draw";
    }
  })()();
  var not7 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var discard18 = /* @__PURE__ */ discard(discardUnit);
  var removeExistingSVG2 = /* @__PURE__ */ removeExistingSVG(d3TaglessD3M);
  var prop34 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "code";
    }
  })()();
  var append10 = /* @__PURE__ */ append(semigroupArray);
  var Initialize4 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var ToggleCard3 = /* @__PURE__ */ function() {
    function ToggleCard7(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard7.create = function(value0) {
      return new ToggleCard7(value0);
    };
    return ToggleCard7;
  }();
  var drawMetaTree = function(json) {
    return bindFlipped9(drawTree)(bindFlipped9(makeModel2(TidyTree.value)(Vertical.value))(bindFlipped9(getMetaTreeJSON)(makeModel2(TidyTree.value)(Radial.value)(json))));
  };
  var blurbtext2 = [/* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text5("The way this library works is by creating an embedded DSL in PureScript\n          which can be interpreted to cause a visualization to come into\n          existence...typically in your browser, as an SVG.\n          ")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text5("\n          The primary interpreter that is provided, the one that powers all of the\n          other demos here except these two, turns the statements of this eDSL into D3\n          actions.\n          ")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text5("\n          However, other interpreters are possible. This page shows two of them, both\n          quite rudimentary but showing some powerful ideas which could be taken a lot\n          further.\n          ")]), /* @__PURE__ */ h2([/* @__PURE__ */ classes(["text-2xl", "p-2"])])([/* @__PURE__ */ text5("MetaTree")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text5('\n          The first one, called here "MetaTree" turns a "script" written in the DSL\n          into a syntax tree and then renders the resulting tree using the other,\n          D3-based, interpreter. The result is a kind of x-ray of the script, one which\n          visually describes the structure you are producing. Because interaction is easy\n          to add to DOM-based visualizations such as D3 this could also be a basis for\n          a point-and-click manner for writing visualizations, or perhaps for editing and\n          adapting them. \n          ')]), /* @__PURE__ */ h2([/* @__PURE__ */ classes(["text-2xl", "p-2"])])([/* @__PURE__ */ text5("Printer")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text5('\n          The second example shows that the "script" can be interpreted into a textual\n          form. This could be the basis for documentation or even transpilation. In\n          principle, it is possible to emit the JavaScript / D3 version of the script\n          via this mechanism, but the current implementation is only a proof-of-concept\n          and is not elaborated to that extent.\n          \n          ')])];
  var _snippets2 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  }();
  var _panels3 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  }();
  var _handlerCode2 = function(dictStrong) {
    var $82 = _snippets2(dictStrong);
    var $83 = prop8($$Proxy.value)(dictStrong);
    return function($84) {
      return $82($83($84));
    };
  };
  var _handlerCode12 = /* @__PURE__ */ _handlerCode2(strongFn);
  var _handlerCode22 = /* @__PURE__ */ _handlerCode2(strongForget);
  var _evaluatorCode = function(dictStrong) {
    var $85 = _snippets2(dictStrong);
    var $86 = prop14($$Proxy.value)(dictStrong);
    return function($87) {
      return $85($86($87));
    };
  };
  var _evaluatorCode1 = /* @__PURE__ */ _evaluatorCode(strongFn);
  var _evaluatorCode2 = /* @__PURE__ */ _evaluatorCode(strongForget);
  var _drawCode3 = function(dictStrong) {
    var $88 = _snippets2(dictStrong);
    var $89 = prop24($$Proxy.value)(dictStrong);
    return function($90) {
      return $88($89($90));
    };
  };
  var _drawCode12 = /* @__PURE__ */ _drawCode3(strongFn);
  var _drawCode22 = /* @__PURE__ */ _drawCode3(strongForget);
  var handleAction3 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard18(dictBind);
    return function(dictMonadAff) {
      var liftAff2 = liftAff(dictMonadAff);
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var liftEffect11 = liftEffect(MonadEffect0);
      var pure21 = pure(MonadEffect0.Monad0().Applicative0());
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var assign4 = assign2(dictMonadState);
        return function(v) {
          if (v instanceof ToggleCard3) {
            return modifying3(v.value0(strongFn))(not7);
          }
          ;
          if (v instanceof Initialize4) {
            return bind18(liftAff2(readSnippetFiles("MetaTreeDraw")))(function(text11) {
              return discard111(assign4(_drawCode12)(text11))(function() {
                return bind18(liftAff2(readSnippetFiles("MetaTreeEvaluator")))(function(text1) {
                  return discard111(assign4(_evaluatorCode1)(text1))(function() {
                    return bind18(liftAff2(readSnippetFiles("MetaTreeHandleActions")))(function(text22) {
                      return discard111(assign4(_handlerCode12)(text22))(function() {
                        return bind18(liftEffect11(eval_D3M(removeExistingSVG2("div.d3story"))))(function(detached) {
                          return bind18(liftAff2(getTreeViaAJAX("./data/flare-2.json")))(function(treeJSON) {
                            return discard111(function() {
                              if (treeJSON instanceof Left) {
                                return pure21(unit);
                              }
                              ;
                              if (treeJSON instanceof Right) {
                                return bind18(liftAff2(drawMetaTree(treeJSON.value0)))(function() {
                                  return pure21(unit);
                                });
                              }
                              ;
                              throw new Error("Failed pattern match at Stories.MetaTree (line 129, column 5 - line 133, column 18): " + [treeJSON.constructor.name]);
                            }())(function() {
                              return pure21(unit);
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          }
          ;
          throw new Error("Failed pattern match at Stories.MetaTree (line 115, column 16 - line 134, column 14): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction13 = /* @__PURE__ */ handleAction3(bindHalogenM);
  var _code3 = function(dictStrong) {
    var $91 = _panels3(dictStrong);
    var $92 = prop34($$Proxy.value)(dictStrong);
    return function($93) {
      return $91($92($93));
    };
  };
  var _code13 = /* @__PURE__ */ _code3(strongForget);
  var component3 = function(dictMonadAff) {
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-about")])([h1([classes(["text-3xl", "p-2"])])([text5("Meta and Printer Interpreters")]), div_(blurbtext2)]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text5("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked2(toBoolean(view(_code13)(state3))), onChange(function(v) {
        return new ToggleCard3(function(dictStrong) {
          return _code3(dictStrong);
        });
      })])]), content_(view(_code13)(state3))(append10(syntaxHighlightedCode(view(_evaluatorCode2)(state3)))(append10(syntaxHighlightedCode(view(_drawCode22)(state3)))(syntaxHighlightedCode(view(_handlerCode22)(state3)))))]), div2([tailwindClass("svg-container")])([])]);
    };
    var initialState = {
      tree: Nothing.value,
      panels: {
        blurb: Expanded.value,
        code: Collapsed.value
      },
      snippets: {
        draw: "",
        evaluator: "",
        handler: ""
      }
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleAction: handleAction13(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize4.value),
        finalize: defaultEval.finalize
      })
    });
  };

  // output/Stories.PrintTree/index.js
  var prop9 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "print";
    }
  })()();
  var prop19 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "handler";
    }
  })()();
  var not8 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var removeExistingSVG3 = /* @__PURE__ */ removeExistingSVG(d3TaglessD3M);
  var discard19 = /* @__PURE__ */ discard(discardUnit);
  var bindFlipped10 = /* @__PURE__ */ bindFlipped(bindAff);
  var makeModel3 = /* @__PURE__ */ makeModel(bindAff)(monadEffectAff);
  var prop35 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "code";
    }
  })()();
  var prop43 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "blurb";
    }
  })()();
  var Initialize5 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var ToggleCard4 = /* @__PURE__ */ function() {
    function ToggleCard7(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard7.create = function(value0) {
      return new ToggleCard7(value0);
    };
    return ToggleCard7;
  }();
  var blurbtext3 = "Id sint laboris reprehenderit officia anim nisi consectetur voluptate enim.\n  Commodo cillum minim nisi laborum eiusmod veniam ullamco id ex fugiat eu anim.\n  Irure est aute laborum duis. Lorem dolore id sunt incididunt ut ea. Nostrud\n  enim officia nisi anim consequat cupidatat consectetur consequat ex excepteur.\n  Lorem nisi in reprehenderit ex adipisicing magna elit aute sunt. Cillum non\n  Lorem minim duis culpa ullamco aute ex minim. Mollit anim in nisi tempor enim\n  exercitation dolore. Veniam consequat minim nostrud amet duis dolore tempor\n  voluptate quis culpa. Laborum dolor pariatur ut est cupidatat elit deserunt\n  occaecat tempor aliquip anim. \n  \n  Velit irure ea voluptate ipsum ex exercitation\n  dolore voluptate reprehenderit sit anim sunt. Anim fugiat ad ut qui cillum\n  tempor occaecat et deserunt nostrud non ipsum. Id non qui mollit culpa elit\n  cillum ipsum excepteur adipisicing qui. Incididunt adipisicing sit incididunt\n  consequat minim id do exercitation cupidatat est sunt mollit. Anim ut ullamco\n  enim culpa. Adipisicing ad non esse laboris anim consequat ut velit esse\n  consequat tempor. Commodo magna esse ullamco ipsum et ipsum minim dolore esse\n  veniam ea commodo labore. Nulla deserunt id ad anim anim proident labore\n  occaecat sint esse nostrud. Duis velit nostrud ullamco cillum cillum Lorem\n  cupidatat irure.";
  var _snippets3 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  }();
  var _panels4 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  }();
  var _print = function(dictStrong) {
    var $90 = _panels4(dictStrong);
    var $91 = prop9($$Proxy.value)(dictStrong);
    return function($92) {
      return $90($91($92));
    };
  };
  var _print1 = /* @__PURE__ */ _print(strongForget);
  var _handlerCode3 = function(dictStrong) {
    var $93 = _snippets3(dictStrong);
    var $94 = prop19($$Proxy.value)(dictStrong);
    return function($95) {
      return $93($94($95));
    };
  };
  var _handlerCode13 = /* @__PURE__ */ _handlerCode3(strongFn);
  var _handlerCode23 = /* @__PURE__ */ _handlerCode3(strongForget);
  var handleAction4 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard19(dictBind);
    return function(dictMonadAff) {
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var liftEffect11 = liftEffect(MonadEffect0);
      var liftAff2 = liftAff(dictMonadAff);
      var pure21 = pure(MonadEffect0.Monad0().Applicative0());
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var assign4 = assign2(dictMonadState);
        var modify_6 = modify_(dictMonadState);
        return function(v) {
          if (v instanceof ToggleCard4) {
            return modifying3(v.value0(strongFn))(not8);
          }
          ;
          if (v instanceof Initialize5) {
            return bind18(liftEffect11(eval_D3M(removeExistingSVG3("div.svg-container"))))(function(detached) {
              return bind18(liftAff2(readSnippetFiles("PrintTreeHandleActions")))(function(text11) {
                return discard111(assign4(_handlerCode13)(text11))(function() {
                  return bind18(liftAff2(getTreeViaAJAX("./data/flare-2.json")))(function(treeJSON) {
                    return discard111(function() {
                      if (treeJSON instanceof Left) {
                        return pure21(unit);
                      }
                      ;
                      if (treeJSON instanceof Right) {
                        return bind18(liftAff2(bindFlipped10(getPrintTree)(makeModel3(TidyTree.value)(Radial.value)(treeJSON.value0))))(function(textRep) {
                          return discard111(modify_6(function(st) {
                            var $86 = {};
                            for (var $87 in st) {
                              if ({}.hasOwnProperty.call(st, $87)) {
                                $86[$87] = st[$87];
                              }
                              ;
                            }
                            ;
                            $86.tree = textRep;
                            return $86;
                          }))(function() {
                            return pure21(unit);
                          });
                        });
                      }
                      ;
                      throw new Error("Failed pattern match at Stories.PrintTree (line 143, column 5 - line 148, column 18): " + [treeJSON.constructor.name]);
                    }())(function() {
                      return pure21(unit);
                    });
                  });
                });
              });
            });
          }
          ;
          throw new Error("Failed pattern match at Stories.PrintTree (line 132, column 16 - line 149, column 14): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction14 = /* @__PURE__ */ handleAction4(bindHalogenM);
  var _code4 = function(dictStrong) {
    var $99 = _panels4(dictStrong);
    var $100 = prop35($$Proxy.value)(dictStrong);
    return function($101) {
      return $99($100($101));
    };
  };
  var _code14 = /* @__PURE__ */ _code4(strongForget);
  var _blurb2 = function(dictStrong) {
    var $102 = _panels4(dictStrong);
    var $103 = prop43($$Proxy.value)(dictStrong);
    return function($104) {
      return $102($103($104));
    };
  };
  var _blurb12 = /* @__PURE__ */ _blurb2(strongForget);
  var component4 = function(dictMonadAff) {
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-controls")])([text5("Les Mis")]), div2([tailwindClass("story-panel-about")])([field_2({
        label: text5("About"),
        helpText: [],
        error: [],
        inputId: "show-blurb"
      })([toggle([id2("show-blurb"), checked2(toBoolean(view(_blurb12)(state3))), onChange(function(v) {
        return new ToggleCard4(function(dictStrong) {
          return _blurb2(dictStrong);
        });
      })])]), content_(view(_blurb12)(state3))([text5(blurbtext3)])]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text5("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked2(toBoolean(view(_code14)(state3))), onChange(function(v) {
        return new ToggleCard4(function(dictStrong) {
          return _code4(dictStrong);
        });
      })])]), content_(view(_code14)(state3))(syntaxHighlightedCode(view(_handlerCode23)(state3)))]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text5("Output"),
        helpText: [],
        error: [],
        inputId: "show-print"
      })([toggle([id2("show-print"), checked2(toBoolean(view(_print1)(state3))), onChange(function(v) {
        return new ToggleCard4(function(dictStrong) {
          return _print(dictStrong);
        });
      })])]), content_(view(_print1)(state3))([code_([text5(state3.tree)])])])]);
    };
    var initialState = {
      tree: "",
      panels: {
        blurb: Collapsed.value,
        code: Collapsed.value,
        print: Expanded.value
      },
      snippets: {
        draw: "",
        handler: ""
      }
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleAction: handleAction14(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize5.value),
        finalize: defaultEval.finalize
      })
    });
  };

  // output/D3.Examples.Spago.Model/foreign.js
  var spotlitSelection;
  var spotlitNode;
  var sourcesSelection;
  var targetSelection;
  var spotlitID;
  var spotlit = false;
  spotlightNeighbours_ = (simulation, id4, nodetype) => {
    spotlit = true;
    spotlitID = id4;
    simulation.stop();
    svg = d3.select("div.svg-container svg");
    nodeSelection = svg.selectAll("g.nodes g");
    spotlitSelection = nodeSelection.filter((d2, i2) => d2.id == id4);
    spotlitNode = spotlitSelection.node();
    spotlitNode.__data__.fx = spotlitNode.__data__.fx || spotlitNode.__data__.x;
    spotlitNode.__data__.fy = spotlitNode.__data__.fy || spotlitNode.__data__.y;
    const targets = spotlitNode.__data__.links.targets;
    const sources = spotlitNode.__data__.links.sources;
    sourcesSelection = nodeSelection.filter((d2, i2) => sources.includes(d2.id));
    targetSelection = nodeSelection.filter((d2, i2) => targets.includes(d2.id));
    svg.classed("spotlight", true);
    sourcesSelection.classed("source", true);
    targetSelection.classed("target", true);
    spotlitSelection.classed("spotlight", true);
    spotlitSelection.classed("source target", false);
    simulation.force("collide", d3.forceCollide().radius((d2) => sources.includes(d2.id) || targets.includes(d2.id) ? d2.r * 4 : d2.id === d2.containerID ? 10 : d2.r));
    simulation.alpha(1).restart();
  };
  unSpotlightNeighbours_ = (simulation) => {
    simulation.stop();
    svg.classed("spotlight", false);
    spotlitNode.__data__.fx = null;
    spotlitNode.__data__.fy = null;
    spotlitSelection.classed("spotlight", false);
    sourcesSelection.classed("source", false);
    targetSelection.classed("target", false);
    simulation.force("collide", d3.forceCollide().radius((d2) => d2.id === d2.containerID ? 10 : d2.r));
    simulation.restart();
    spotlit = false;
  };

  // output/D3.Examples.Spago.Files/foreign.js
  function readSpago_Raw_JSON_(modulesBody) {
    return (packagesBody) => (lsdepsBody) => (locBody) => {
      const modules = decodeModulesFile(modulesBody);
      const packages = decodePackagesFile(packagesBody);
      const lsDeps = decodeLsDepsFile(lsdepsBody);
      const loc = decodeLOCFile(locBody);
      return { modules, packages, lsDeps, loc };
    };
  }
  var decodeModulesFile = function(filecontents) {
    const json = JSON.parse(filecontents);
    const modules = Object.keys(json).map((key) => {
      return { key, depends: json[key].depends, path: json[key].path };
    });
    return modules;
  };
  var decodePackagesFile = function(filecontents) {
    const json = JSON.parse(filecontents);
    const packages = Object.keys(json).map((key) => {
      return { key, depends: json[key].depends };
    });
    return packages;
  };
  var decodeLOCFile = function(filecontents) {
    const json = JSON.parse(filecontents);
    return json.loc;
  };
  var decodeLsDepsFile = function(filecontents) {
    const jsonlines = splitIntoLines(filecontents);
    jsonlines.length = jsonlines.length - 1;
    var objectArray = jsonlines.map((d2) => JSON.parse(d2));
    return objectArray;
  };
  function splitIntoLines(str) {
    return str.split(/\r\n|[\n\v\f\r\u0085\u2028\u2029]/);
  }

  // output/D3.Examples.Spago.Files/index.js
  var fromFoldable9 = /* @__PURE__ */ fromFoldable2(ordString)(foldableArray);
  var mapFlipped5 = /* @__PURE__ */ mapFlipped(functorArray);
  var append11 = /* @__PURE__ */ append(semigroupArray);
  var map43 = /* @__PURE__ */ map(functorArray);
  var fromFoldableWith2 = /* @__PURE__ */ fromFoldableWith(ordString)(foldableArray);
  var lookup8 = /* @__PURE__ */ lookup(ordString);
  var bind10 = /* @__PURE__ */ bind(bindMaybe);
  var sum2 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var equalSnd2 = /* @__PURE__ */ equalSnd(eqString);
  var compareSnd2 = /* @__PURE__ */ compareSnd(ordString);
  var fromFoldable12 = /* @__PURE__ */ fromFoldable2(ordInt)(foldableArray);
  var PackageInfo = /* @__PURE__ */ function() {
    function PackageInfo2(value0) {
      this.value0 = value0;
    }
    ;
    PackageInfo2.create = function(value0) {
      return new PackageInfo2(value0);
    };
    return PackageInfo2;
  }();
  var IsModule = /* @__PURE__ */ function() {
    function IsModule2(value0) {
      this.value0 = value0;
    }
    ;
    IsModule2.create = function(value0) {
      return new IsModule2(value0);
    };
    return IsModule2;
  }();
  var IsPackage = /* @__PURE__ */ function() {
    function IsPackage2(value0) {
      this.value0 = value0;
    }
    ;
    IsPackage2.create = function(value0) {
      return new IsPackage2(value0);
    };
    return IsPackage2;
  }();
  var M2M_Tree = /* @__PURE__ */ function() {
    function M2M_Tree2() {
    }
    ;
    M2M_Tree2.value = new M2M_Tree2();
    return M2M_Tree2;
  }();
  var M2M_Graph = /* @__PURE__ */ function() {
    function M2M_Graph2() {
    }
    ;
    M2M_Graph2.value = new M2M_Graph2();
    return M2M_Graph2;
  }();
  var P2P = /* @__PURE__ */ function() {
    function P2P2() {
    }
    ;
    P2P2.value = new P2P2();
    return P2P2;
  }();
  var M2P = /* @__PURE__ */ function() {
    function M2P2() {
    }
    ;
    M2P2.value = new M2P2();
    return M2P2;
  }();
  var eqLinkType = {
    eq: function(x10) {
      return function(y8) {
        if (x10 instanceof M2M_Tree && y8 instanceof M2M_Tree) {
          return true;
        }
        ;
        if (x10 instanceof M2M_Graph && y8 instanceof M2M_Graph) {
          return true;
        }
        ;
        if (x10 instanceof P2P && y8 instanceof P2P) {
          return true;
        }
        ;
        if (x10 instanceof M2P && y8 instanceof M2P) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq13 = /* @__PURE__ */ eq(eqLinkType);
  var showNodeType = {
    show: function(v) {
      if (v instanceof IsModule) {
        return "module";
      }
      ;
      if (v instanceof IsPackage) {
        return "package";
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Files (line 293, column 1 - line 295, column 47): " + [v.constructor.name]);
    }
  };
  var showLinkType = {
    show: function(v) {
      if (v instanceof M2M_Tree) {
        return "M2M-Tree";
      }
      ;
      if (v instanceof M2M_Graph) {
        return "M2M-Graph";
      }
      ;
      if (v instanceof P2P) {
        return "P2P";
      }
      ;
      if (v instanceof M2P) {
        return "module to package dependency";
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Files (line 296, column 1 - line 300, column 44): " + [v.constructor.name]);
    }
  };
  var isP2P_Link = function(v) {
    return eq13(v.linktype)(P2P.value);
  };
  var isM2P_Link = function(v) {
    return eq13(v.linktype)(M2P.value);
  };
  var isM2M_Tree_Link = function(v) {
    return eq13(v.linktype)(M2M_Tree.value);
  };
  var isM2M_Graph_Link = function(v) {
    return eq13(v.linktype)(M2M_Graph.value);
  };
  var getGraphJSONData = function(v) {
    var path2LOC = fromFoldable9(mapFlipped5(v.loc)(function(o) {
      return new Tuple(o.path, o.loc);
    }));
    var names = append11(map43(function(v1) {
      return v1.key;
    })(v.modules))(map43(function(v1) {
      return v1.key;
    })(v.packages));
    var makeModuleToPackageLink = function(m) {
      return {
        source: m.id,
        target: m.containerID,
        linktype: M2P.value,
        inSim: true
      };
    };
    var makeLink = function(linktype) {
      return function(v1) {
        return {
          source: v1.value0,
          target: v1.value1,
          linktype,
          inSim: true
        };
      };
    };
    var ids = range2(0)(length4(names) - 1 | 0);
    var name2ID = fromFoldableWith2(function(v1) {
      return function(v2) {
        return v1;
      };
    })(zip(names)(ids));
    var getId = function(s) {
      return fromMaybe(0)(lookup8(s)(name2ID));
    };
    var makeNodeFromModuleJSONPL = function(m) {
      var id4 = getId(m.key);
      return {
        id: id4,
        name: m.key,
        containerID: getId(m["package"]),
        containerName: m["package"],
        loc: m.loc,
        nodetype: new IsModule(m.path),
        inSim: true,
        links: {
          targets: map43(getId)(m.depends),
          sources: [],
          treeChildren: [],
          inPackage: [],
          outPackage: [],
          contains: []
        },
        connected: false,
        showChildren: false,
        containsMany: false,
        treeXY: nullImpl,
        treeDepth: nullImpl,
        gridXY: nullImpl
      };
    };
    var foldDepends = function(b2) {
      return function(a2) {
        var id4 = getId(a2.key);
        var makeTuple = function(s) {
          return new Tuple(id4, getId(s));
        };
        return append11(map43(makeTuple)(a2.depends))(b2);
      };
    };
    var moduleLinks = map43(makeLink(M2M_Graph.value))(foldl2(foldDepends)([])(v.modules));
    var packageLinks = map43(makeLink(P2P.value))(foldl2(foldDepends)([])(v.packages));
    var depsMap = fromFoldable9(mapFlipped5(v.lsDeps)(function(d2) {
      return new Tuple(d2.packageName, {
        version: d2.version,
        repo: d2.repo.contents
      });
    }));
    var makeNodeFromPackageJSONCL = function(p2) {
      var repo = fromMaybe({
        version: "not found",
        repo: "not found"
      })(lookup8(p2.key)(depsMap));
      var id4 = getId(p2.key);
      return {
        id: id4,
        name: p2.key,
        inSim: true,
        nodetype: new IsPackage(new PackageInfo(repo)),
        containerID: id4,
        containerName: p2.key,
        loc: p2.loc,
        links: {
          targets: map43(getId)(p2.depends),
          sources: [],
          treeChildren: [],
          inPackage: [],
          outPackage: [],
          contains: map43(getId)(p2.contains)
        },
        connected: true,
        showChildren: true,
        containsMany: length4(p2.contains) > 1,
        treeXY: nullImpl,
        treeDepth: nullImpl,
        gridXY: nullImpl
      };
    };
    var addPackageInfo = function(v1) {
      var packageName = function() {
        var pieces = split("/")(v1.path);
        return bind10(index2(pieces)(0))(function(root) {
          return bind10(index2(pieces)(1))(function(packageString) {
            if (root === ".spago") {
              return new Just(packageString);
            }
            ;
            if (root === "src") {
              return new Just("my-project");
            }
            ;
            return Nothing.value;
          });
        });
      }();
      var $$package = fromMaybe("")(packageName);
      return {
        key: v1.key,
        depends: v1.depends,
        path: v1.path,
        "package": $$package
      };
    };
    var addLOCInfo = function(v1) {
      var linecount = fromMaybe(10)(lookup8(v1.path)(path2LOC));
      return {
        key: v1.key,
        depends: v1.depends,
        path: v1.path,
        "package": v1["package"],
        loc: linecount
      };
    };
    var modulesPL = map43(function($89) {
      return addLOCInfo(addPackageInfo($89));
    })(v.modules);
    var mapNamesToModules = fromFoldable9(map43(function(m) {
      return new Tuple(m.key, m);
    })(modulesPL));
    var maybeModules = function(ms) {
      return catMaybes(map43(function(k) {
        return lookup8(k)(mapNamesToModules);
      })(ms));
    };
    var rollUpLOC = function(ms) {
      return sum2(map43(function(v1) {
        return v1.loc;
      })(maybeModules(ms)));
    };
    var moduleNodes = map43(makeNodeFromModuleJSONPL)(modulesPL);
    var modulePackageLinks = map43(makeModuleToPackageLink)(moduleNodes);
    var links = append11(moduleLinks)(append11(packageLinks)(modulePackageLinks));
    var getSourceLinks = function(v1) {
      var sources = foldl2(function(acc) {
        return function(v2) {
          var $73 = v1.id === v2.target;
          if ($73) {
            return cons3(v2.source)(acc);
          }
          ;
          return acc;
        };
      })([])(links);
      return new Tuple(v1.id, sources);
    };
    var modulePackageTuples = map43(function(m) {
      return new Tuple(m.key, m["package"]);
    })(modulesPL);
    var packageContains = map43(chunk)(groupBy(equalSnd2)(sortBy(compareSnd2)(modulePackageTuples)));
    var packageContainsMap = fromFoldable9(packageContains);
    var packageLOC = map43(function(v1) {
      return new Tuple(v1.value0, rollUpLOC(v1.value1));
    })(packageContains);
    var packageLOCMap = fromFoldable9(packageLOC);
    var addRollUpLOC = function(v1) {
      return {
        key: v1.key,
        depends: v1.depends,
        contains: v1.contains,
        loc: fromMaybe(0)(lookup8(v1.key)(packageLOCMap))
      };
    };
    var addContains = function(v1) {
      return {
        key: v1.key,
        depends: v1.depends,
        contains: fromMaybe([])(lookup8(v1.key)(packageContainsMap))
      };
    };
    var packagesCL = map43(function($90) {
      return addRollUpLOC(addContains($90));
    })(v.packages);
    var packageNodes = map43(makeNodeFromPackageJSONCL)(packagesCL);
    var nodes = append11(moduleNodes)(packageNodes);
    var id2Node = fromFoldable12(mapFlipped5(nodes)(function(node) {
      return new Tuple(node.id, node);
    }));
    var sourceLinksMap = fromFoldable12(map43(getSourceLinks)(nodes));
    return {
      links,
      nodes,
      moduleNodes,
      packageNodes,
      moduleLinks,
      packageLinks,
      modulePackageLinks,
      sourceLinksMap,
      name2ID,
      id2Node,
      id2Name: empty2,
      id2Package: empty2,
      id2LOC: empty2
    };
  };

  // output/D3.Examples.Spago.Unsafe/index.js
  var unboxD3SimNode2 = function(datum) {
    return datum;
  };
  var unboxD3SimLink2 = function(datum) {
    return datum;
  };

  // output/Data.Graph/index.js
  var map113 = /* @__PURE__ */ map(functorMaybe);
  var Graph = function(x10) {
    return x10;
  };
  var lookup9 = function(dictOrd) {
    var lookup14 = lookup(dictOrd);
    return function(k) {
      return function(v) {
        return map113(fst)(lookup14(k)(v));
      };
    };
  };
  var fromMap2 = Graph;

  // output/D3.Examples.Spago.Model/index.js
  var lookup10 = /* @__PURE__ */ lookup(ordInt);
  var map44 = /* @__PURE__ */ map(functorArray);
  var append14 = /* @__PURE__ */ append(semigroupArray);
  var fromFoldable11 = /* @__PURE__ */ fromFoldable(foldableArray);
  var insert9 = /* @__PURE__ */ insert(ordInt);
  var mapFlipped6 = /* @__PURE__ */ mapFlipped(functorArray);
  var show17 = /* @__PURE__ */ show(showLinkType);
  var append15 = /* @__PURE__ */ append(semigroupString);
  var fromFoldable13 = /* @__PURE__ */ fromFoldable2(ordInt)(foldableArray);
  var spy7 = /* @__PURE__ */ spy();
  var foldlWithIndex3 = /* @__PURE__ */ foldlWithIndex(foldableWithIndexArray);
  var eq22 = /* @__PURE__ */ eq(eqLinkType);
  var show18 = /* @__PURE__ */ show(showInt);
  var show25 = /* @__PURE__ */ show(showNumber);
  var show34 = /* @__PURE__ */ show(showNodeType);
  var upgradeSpagoNodeData = function(sourcesMap) {
    return function(node) {
      return {
        links: {
          sources: fromMaybe([])(lookup10(node.id)(sourcesMap)),
          contains: node.links.contains,
          inPackage: node.links.inPackage,
          outPackage: node.links.outPackage,
          targets: node.links.targets,
          treeChildren: node.links.treeChildren
        },
        id: node.id,
        cluster: node.containerID,
        connected: node.connected,
        showChildren: function() {
          if (node.nodetype instanceof IsPackage) {
            return true;
          }
          ;
          if (node.nodetype instanceof IsModule) {
            return false;
          }
          ;
          throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 207, column 20 - line 209, column 45): " + [node.nodetype.constructor.name]);
        }(),
        containerID: node.containerID,
        containerName: node.containerName,
        containsMany: node.containsMany,
        focusX: 0,
        focusY: 0,
        fx: nullImpl,
        fy: nullImpl,
        inSim: true,
        loc: node.loc,
        name: node.name,
        nodetype: node.nodetype,
        r: sqrt(node.loc),
        treeXY: nullImpl,
        treeDepth: nullImpl,
        gridXY: nullImpl,
        vx: 0,
        vy: 0,
        x: 0,
        y: 0
      };
    };
  };
  var unpinAllNodes = function(nodes) {
    var unpin2 = function(v) {
      return {
        id: v.id,
        links: v.links,
        connected: v.connected,
        showChildren: v.showChildren,
        containerID: v.containerID,
        containerName: v.containerName,
        containsMany: v.containsMany,
        inSim: v.inSim,
        loc: v.loc,
        name: v.name,
        nodetype: v.nodetype,
        treeXY: v.treeXY,
        treeDepth: v.treeDepth,
        gridXY: v.gridXY,
        x: v.x,
        y: v.y,
        vx: v.vx,
        vy: v.vy,
        fx: nullImpl,
        fy: nullImpl,
        cluster: v.cluster,
        focusX: v.focusX,
        focusY: v.focusY,
        r: v.r
      };
    };
    return map44(unpin2)(nodes);
  };
  var setTreeXYIncludingLeaves = function(v) {
    return function(v1) {
      return {
        id: v.id,
        links: {
          treeChildren: v1.childIDs,
          contains: v.links.contains,
          inPackage: v.links.inPackage,
          outPackage: v.links.outPackage,
          sources: v.links.sources,
          targets: v.links.targets
        },
        connected: true,
        showChildren: v.showChildren,
        containerID: v.containerID,
        containerName: v.containerName,
        containsMany: v.containsMany,
        inSim: v.inSim,
        loc: v.loc,
        name: v.name,
        nodetype: v.nodetype,
        treeXY: notNull({
          x: v1.x,
          y: v1.y
        }),
        treeDepth: notNull(v1.depth),
        gridXY: v.gridXY,
        x: v.x,
        y: v.y,
        vx: v.vx,
        vy: v.vy,
        fx: v.fx,
        fy: v.fy,
        cluster: v.cluster,
        focusX: v.focusX,
        focusY: v.focusY,
        r: v.r
      };
    };
  };
  var scalePoint = function(xFactor) {
    return function(yFactor) {
      return function(xy) {
        return {
          x: xy.x * xFactor,
          y: xy.y * yFactor
        };
      };
    };
  };
  var offsetXY = function(offset) {
    return function(xy) {
      return {
        x: xy.x + offset.x,
        y: xy.y + offset.y
      };
    };
  };
  var numberToGridPoint = function(columns) {
    return function(i2) {
      var d2 = toNumber(i2);
      var c = toNumber(columns);
      var x10 = remainder(d2)(c);
      var y8 = floor(d2 / c);
      return {
        x: x10,
        y: y8
      };
    };
  };
  var makeGraph = function(nodes) {
    var addNode = function(acc) {
      return function(node) {
        var depends = fromFoldable11(node.links.targets);
        return insert9(node.id)(new Tuple(node, depends))(acc);
      };
    };
    var graphMap = foldl2(addNode)(empty2)(nodes);
    return fromMap2(graphMap);
  };
  var makeSpagoGraphModel = function(json) {
    var v = getGraphJSONData(json);
    return {
      links: v.links,
      nodes: mapFlipped6(v.nodes)(upgradeSpagoNodeData(v.sourceLinksMap)),
      graph: makeGraph(v.nodes),
      tree: Nothing.value,
      maps: {
        name2ID: v.name2ID,
        id2Name: v.id2Name,
        id2Node: v.id2Node,
        id2Package: v.id2Package,
        id2LOC: v.id2LOC,
        id2TreeData: empty2
      }
    };
  };
  var link_2 = {
    source: function($198) {
      return function(v) {
        return v.source;
      }(unboxD3SimLink2($198));
    },
    target: function($199) {
      return function(v) {
        return v.target;
      }(unboxD3SimLink2($199));
    },
    linkType: function($200) {
      return function(v) {
        return v.linktype;
      }(unboxD3SimLink2($200));
    },
    linkClass: function($201) {
      return show17(function(v) {
        return v.linktype;
      }(unboxD3SimLink2($201)));
    },
    linkClass2: /* @__PURE__ */ function() {
      var $202 = append15("updated ");
      return function($203) {
        return $202(show17(function(v) {
          return v.linktype;
        }(unboxD3SimLink2($203))));
      };
    }(),
    color: function($204) {
      return d3SchemeCategory10N_(toNumber(function(v) {
        return v.target.containerID;
      }(unboxD3SimLink2($204))));
    }
  };
  var sourcePackageIs = function(name17) {
    return function(link3) {
      return link_2.source(link3).name === name17;
    };
  };
  var isUsedModule = function(v) {
    if (v.nodetype instanceof IsPackage) {
      return false;
    }
    ;
    if (v.nodetype instanceof IsModule) {
      if (v.connected) {
        return true;
      }
      ;
      return false;
    }
    ;
    throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 193, column 3 - line 197, column 31): " + [v.nodetype.constructor.name]);
  };
  var treeNodesToTreeXY_R = function(nodes) {
    var setXYtoTreeXY = function(v) {
      var treeXY = fromMaybe({
        x: v.x,
        y: v.y
      })(toMaybe(v.treeXY));
      var radialTranslate2 = function(p2) {
        var x10 = p2.x * cos(p2.y);
        var y8 = p2.x * sin(p2.y);
        return {
          x: x10,
          y: y8
        };
      };
      var radialXY = radialTranslate2(treeXY);
      return {
        id: v.id,
        links: v.links,
        connected: v.connected,
        showChildren: v.showChildren,
        containerID: v.containerID,
        containerName: v.containerName,
        containsMany: v.containsMany,
        inSim: v.inSim,
        loc: v.loc,
        name: v.name,
        nodetype: v.nodetype,
        treeXY: v.treeXY,
        treeDepth: v.treeDepth,
        gridXY: v.gridXY,
        x: v.x,
        y: v.y,
        vx: v.vx,
        vy: v.vy,
        fx: notNull(radialXY.x),
        fy: notNull(radialXY.y),
        cluster: v.cluster,
        focusX: v.focusX,
        focusY: v.focusY,
        r: v.r
      };
    };
    var partitioned = partition(isUsedModule)(nodes);
    return append14(partitioned.no)(map44(setXYtoTreeXY)(partitioned.yes));
  };
  var isPackageOrVisibleModule = function(id4) {
    return function(v) {
      if (v.nodetype instanceof IsModule) {
        return v.containerID === id4;
      }
      ;
      if (v.nodetype instanceof IsPackage) {
        return true;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 183, column 3 - line 185, column 26): " + [v.nodetype.constructor.name]);
    };
  };
  var isPackage = function(v) {
    if (v.nodetype instanceof IsModule) {
      return false;
    }
    ;
    if (v.nodetype instanceof IsPackage) {
      return true;
    }
    ;
    throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 178, column 3 - line 180, column 26): " + [v.nodetype.constructor.name]);
  };
  var moduleNodesToContainerXY = function(nodes) {
    var partitioned = partition(isPackage)(nodes);
    var packagesIndexMap = fromFoldable13(foldl2(function(b2) {
      return function(v) {
        return cons3(new Tuple(v.id, v.gridXY))(b2);
      };
    })([])(partitioned.yes));
    var setModuleGridXY = function(v) {
      var v1 = lookup10(v.containerID)(packagesIndexMap);
      if (v1 instanceof Nothing) {
        return spy7("container gridXY not found")(v);
      }
      ;
      if (v1 instanceof Just) {
        var v2 = toMaybe(v1.value0);
        if (v2 instanceof Nothing) {
          return {
            containerID: v.containerID,
            x: 0,
            y: 0,
            gridXY: v1.value0,
            cluster: v.cluster,
            connected: v.connected,
            containerName: v.containerName,
            containsMany: v.containsMany,
            focusX: v.focusX,
            focusY: v.focusY,
            fx: v.fx,
            fy: v.fy,
            id: v.id,
            inSim: v.inSim,
            links: v.links,
            loc: v.loc,
            name: v.name,
            nodetype: v.nodetype,
            r: v.r,
            showChildren: v.showChildren,
            treeDepth: v.treeDepth,
            treeXY: v.treeXY,
            vx: v.vx,
            vy: v.vy
          };
        }
        ;
        if (v2 instanceof Just) {
          return {
            containerID: v.containerID,
            x: v2.value0.x,
            y: v2.value0.y,
            gridXY: v1.value0,
            cluster: v.cluster,
            connected: v.connected,
            containerName: v.containerName,
            containsMany: v.containsMany,
            focusX: v.focusX,
            focusY: v.focusY,
            fx: v.fx,
            fy: v.fy,
            id: v.id,
            inSim: v.inSim,
            links: v.links,
            loc: v.loc,
            name: v.name,
            nodetype: v.nodetype,
            r: v.r,
            showChildren: v.showChildren,
            treeDepth: v.treeDepth,
            treeXY: v.treeXY,
            vx: v.vx,
            vy: v.vy
          };
        }
        ;
        throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 289, column 11 - line 291, column 76): " + [v2.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 286, column 7 - line 291, column 76): " + [v1.constructor.name]);
    };
    var modulesWithGrid = map44(setModuleGridXY)(partitioned.no);
    return append14(modulesWithGrid)(partitioned.yes);
  };
  var packageNodesToGridXY = function(nodes) {
    var partitioned = partition(isPackage)(nodes);
    var packageCount = length4(partitioned.yes);
    var columns = floor2(ceil(sqrt(toNumber(packageCount))));
    var offset = -(toNumber(columns) / 2);
    var packagesWithGrid = function() {
      var setGridXY = function(v) {
        return function(i2) {
          var gridXY = scalePoint(200)(200)(offsetXY({
            x: offset,
            y: offset
          })(numberToGridPoint(columns)(i2)));
          return {
            gridXY: notNull(gridXY),
            cluster: v.cluster,
            connected: v.connected,
            containerID: v.containerID,
            containerName: v.containerName,
            containsMany: v.containsMany,
            focusX: v.focusX,
            focusY: v.focusY,
            fx: v.fx,
            fy: v.fy,
            id: v.id,
            inSim: v.inSim,
            links: v.links,
            loc: v.loc,
            name: v.name,
            nodetype: v.nodetype,
            r: v.r,
            showChildren: v.showChildren,
            treeDepth: v.treeDepth,
            treeXY: v.treeXY,
            vx: v.vx,
            vy: v.vy,
            x: v.x,
            y: v.y
          };
        };
      };
      return foldlWithIndex3(function(i2) {
        return function(b2) {
          return function(a2) {
            return cons3(setGridXY(a2)(i2))(b2);
          };
        };
      })([])(partitioned.yes);
    }();
    return append14(partitioned.no)(packagesWithGrid);
  };
  var isP2P_Link_ = function(l) {
    return eq22(link_2.linkType(l))(P2P.value);
  };
  var isM2P_Link_ = function(l) {
    return eq22(link_2.linkType(l))(M2P.value);
  };
  var isM2M_Tree_Link_ = function(l) {
    return eq22(link_2.linkType(l))(M2M_Tree.value);
  };
  var isM2M_Graph_Link_ = function(l) {
    return eq22(link_2.linkType(l))(M2M_Graph.value);
  };
  var initialRadius = 10;
  var initialAngle = /* @__PURE__ */ function() {
    return pi * (3 - sqrt(5));
  }();
  var setForPhyllotaxis = function(index5) {
    return function(v) {
      var i2 = toNumber(index5);
      var radius9 = initialRadius * sqrt(0.5 + i2);
      var angle = i2 * initialAngle;
      return {
        id: v.id,
        links: v.links,
        connected: v.connected,
        showChildren: v.showChildren,
        containerID: v.containerID,
        containerName: v.containerName,
        containsMany: v.containsMany,
        inSim: v.inSim,
        loc: v.loc,
        name: v.name,
        nodetype: v.nodetype,
        treeXY: v.treeXY,
        treeDepth: v.treeDepth,
        gridXY: v.gridXY,
        x: radius9 * cos(angle),
        y: radius9 * sin(angle),
        vx: v.vx,
        vy: v.vy,
        fx: v.fx,
        fy: v.fy,
        cluster: v.cluster,
        focusX: v.focusX,
        focusY: v.focusY,
        r: v.r
      };
    };
  };
  var nodesToPhyllotaxis = function(predicate) {
    return function(nodes) {
      var partitioned = partition(predicate)(nodes);
      return append14(partitioned.no)(mapWithIndex3(setForPhyllotaxis)(partitioned.yes));
    };
  };
  var packagesNodesToPhyllotaxis = /* @__PURE__ */ nodesToPhyllotaxis(isPackage);
  var fixNamedNodeTo = function(label6) {
    return function(point) {
      return function(nodes) {
        var fixNamedNode$prime = function(v) {
          var $169 = v.name === label6;
          if ($169) {
            return spy7("fixing a node to: ")({
              name: v.name,
              fx: notNull(point.x),
              fy: notNull(point.y),
              cluster: v.cluster,
              connected: v.connected,
              containerID: v.containerID,
              containerName: v.containerName,
              containsMany: v.containsMany,
              focusX: v.focusX,
              focusY: v.focusY,
              gridXY: v.gridXY,
              id: v.id,
              inSim: v.inSim,
              links: v.links,
              loc: v.loc,
              nodetype: v.nodetype,
              r: v.r,
              showChildren: v.showChildren,
              treeDepth: v.treeDepth,
              treeXY: v.treeXY,
              vx: v.vx,
              vy: v.vy,
              x: v.x,
              y: v.y
            });
          }
          ;
          return v;
        };
        return map44(fixNamedNode$prime)(nodes);
      };
    };
  };
  var datum_3 = {
    radius: function($205) {
      return function(v) {
        return v.r;
      }(unboxD3SimNode2($205));
    },
    id: function($206) {
      return function(v) {
        return v.id;
      }(unboxD3SimNode2($206));
    },
    loc: function($207) {
      return function(v) {
        return v.loc;
      }(unboxD3SimNode2($207));
    },
    containerID: function($208) {
      return function(v) {
        return v.containerID;
      }(unboxD3SimNode2($208));
    },
    containerName: function($209) {
      return function(v) {
        return v.containerName;
      }(unboxD3SimNode2($209));
    },
    name: function($210) {
      return function(v) {
        return v.name;
      }(unboxD3SimNode2($210));
    },
    x: function($211) {
      return function(v) {
        return v.x;
      }(unboxD3SimNode2($211));
    },
    y: function($212) {
      return function(v) {
        return v.y;
      }(unboxD3SimNode2($212));
    },
    fx: function($213) {
      return function(v) {
        return v.fx;
      }(unboxD3SimNode2($213));
    },
    fy: function($214) {
      return function(v) {
        return v.fy;
      }(unboxD3SimNode2($214));
    },
    treeXY: function($215) {
      return function(v) {
        return v.treeXY;
      }(unboxD3SimNode2($215));
    },
    treeDepth: function($216) {
      return function(v) {
        return v.treeDepth;
      }(unboxD3SimNode2($216));
    },
    gridXY: function($217) {
      return function(v) {
        return v.gridXY;
      }(unboxD3SimNode2($217));
    },
    nodetype: function($218) {
      return function(v) {
        return v.nodetype;
      }(unboxD3SimNode2($218));
    },
    cluster: function($219) {
      return function(v) {
        return v.cluster;
      }(unboxD3SimNode2($219));
    },
    links: function($220) {
      return function(v) {
        return v.links;
      }(unboxD3SimNode2($220));
    },
    connected: function($221) {
      return function(v) {
        return v.connected;
      }(unboxD3SimNode2($221));
    },
    nameAndID: function(d2) {
      return unboxD3SimNode2(d2).name + (" " + show18(unboxD3SimNode2(d2).id));
    },
    indexAndID: function(d2) {
      return unboxD3SimNode2(d2).name + (" " + (show18(getIndexFromDatum_(d2)) + (" " + show18(unboxD3SimNode2(d2).id))));
    },
    namePos: function(d2) {
      return "(" + (show25(floor(datum_3.x(d2))) + ("," + (show25(floor(datum_3.y(d2))) + ")")));
    },
    gridPoint: function(d2) {
      return fromMaybe({
        x: datum_3.x(d2),
        y: datum_3.y(d2)
      })(toMaybe(datum_3.gridXY(d2)));
    },
    gridPointX: function(d2) {
      return function(v) {
        return v.x;
      }(datum_3.gridPoint(d2));
    },
    gridPointY: function(d2) {
      return function(v) {
        return v.y;
      }(datum_3.gridPoint(d2));
    },
    treePoint: function(d2) {
      return fromMaybe({
        x: datum_3.x(d2),
        y: datum_3.y(d2)
      })(toMaybe(datum_3.treeXY(d2)));
    },
    treePointX: function(d2) {
      return function(v) {
        return v.x;
      }(datum_3.treePoint(d2));
    },
    treePointY: function(d2) {
      return function(v) {
        return v.y;
      }(datum_3.treePoint(d2));
    },
    indexFunction: function($222) {
      return function(v) {
        return v.id;
      }(unboxD3SimNode2($222));
    },
    positionLabel: function(d2) {
      var v = datum_3.nodetype(d2);
      if (v instanceof IsModule) {
        return -datum_3.radius(d2);
      }
      ;
      if (v instanceof IsPackage) {
        return 0;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 109, column 11 - line 111, column 33): " + [v.constructor.name]);
    },
    collideRadius: function(d2) {
      var $175 = datum_3.id(d2) === datum_3.containerID(d2);
      if ($175) {
        return 10;
      }
      ;
      return datum_3.radius(d2);
    },
    collideRadiusBig: function(d2) {
      return datum_3.radius(d2) + 10;
    },
    nodeClass: function(d2) {
      return show34(datum_3.nodetype(d2)) + (" " + (datum_3.containerName(d2) + (" " + (datum_3.name(d2) + function() {
        var $176 = datum_3.connected(d2);
        if ($176) {
          return " connected";
        }
        ;
        return "";
      }()))));
    },
    "nodeClass'": function(d2) {
      return "updated" + (show34(datum_3.nodetype(d2)) + (" " + (datum_3.containerName(d2) + (" " + (datum_3.name(d2) + function() {
        var $177 = datum_3.connected(d2);
        if ($177) {
          return " connected";
        }
        ;
        return "";
      }())))));
    },
    colorByGroup: function(d2) {
      return d3SchemeCategory10N_(toNumber(datum_3.cluster(d2)));
    },
    colorByDepth: function(d2) {
      var v = toMaybe(datum_3.treeDepth(d2));
      if (v instanceof Nothing) {
        return "none";
      }
      ;
      if (v instanceof Just) {
        return d3SchemeSequential10N_(toNumber(v.value0));
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 129, column 13 - line 131, column 68): " + [v.constructor.name]);
    },
    fillByUsage: function(d2) {
      var $180 = datum_3.connected(d2);
      if ($180) {
        return datum_3.colorByGroup(d2);
      }
      ;
      return "none";
    },
    strokeByUsage: function(d2) {
      var $181 = datum_3.connected(d2);
      if ($181) {
        return "none";
      }
      ;
      return datum_3.colorByGroup(d2);
    },
    colorByUsage: function(d2) {
      var $182 = datum_3.connected(d2);
      if ($182) {
        return "red";
      }
      ;
      return "blue";
    },
    opacityByType: function(d2) {
      var $183 = datum_3.isPackage(d2);
      if ($183) {
        return 0.4;
      }
      ;
      return 0.7;
    },
    translateNode: function(d2) {
      return "translate(" + (show25(datum_3.x(d2)) + ("," + (show25(datum_3.y(d2)) + ")")));
    },
    isNamed: function(name17) {
      return function(v) {
        return function(d2) {
          return datum_3.name(d2) === name17;
        };
      };
    },
    isPackage: function(d2) {
      var v = datum_3.nodetype(d2);
      if (v instanceof IsModule) {
        return false;
      }
      ;
      if (v instanceof IsPackage) {
        return true;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 146, column 13 - line 148, column 36): " + [v.constructor.name]);
    },
    isModule: function(d2) {
      var v = datum_3.nodetype(d2);
      if (v instanceof IsModule) {
        return true;
      }
      ;
      if (v instanceof IsPackage) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 150, column 13 - line 152, column 37): " + [v.constructor.name]);
    },
    isUnusedModule: function(d2) {
      var v = datum_3.nodetype(d2);
      if (v instanceof IsPackage) {
        return false;
      }
      ;
      if (v instanceof IsModule) {
        var $192 = datum_3.connected(d2);
        if ($192) {
          return false;
        }
        ;
        return true;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 154, column 13 - line 158, column 41): " + [v.constructor.name]);
    },
    isUsedModule: function(d2) {
      var v = datum_3.nodetype(d2);
      if (v instanceof IsPackage) {
        return false;
      }
      ;
      if (v instanceof IsModule) {
        var $196 = datum_3.connected(d2);
        if ($196) {
          return true;
        }
        ;
        return false;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 161, column 13 - line 165, column 42): " + [v.constructor.name]);
    },
    treeChildren: function(d2) {
      return datum_3.links(d2).treeChildren;
    },
    isTreeParent: function(d2) {
      return !$$null3(datum_3.treeChildren(d2));
    }
  };
  var convertFilesToGraphModel = function(moduleJSON) {
    return function(packageJSON) {
      return function(lsdepJSON) {
        return function(locJSON) {
          return makeSpagoGraphModel(readSpago_Raw_JSON_(moduleJSON.body)(packageJSON.body)(lsdepJSON.body)(locJSON.body));
        };
      };
    };
  };
  var allNodes2 = /* @__PURE__ */ $$const(true);

  // output/D3.Examples.Spago.Draw.Attributes/index.js
  var classed7 = /* @__PURE__ */ classed(toAttrStringFn);
  var radius6 = /* @__PURE__ */ radius(toAttrNumberFn);
  var fill6 = /* @__PURE__ */ fill(toAttrStringFn);
  var strokeColor5 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var strokeWidth5 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var classed1 = /* @__PURE__ */ classed(toAttrString);
  var x7 = /* @__PURE__ */ x(toAttrNumber);
  var text10 = /* @__PURE__ */ text6(toAttrStringFn);
  var width11 = /* @__PURE__ */ width8(toAttrNumber);
  var height11 = /* @__PURE__ */ height8(toAttrNumber);
  var cursor2 = /* @__PURE__ */ cursor(toAttrString);
  var opacity2 = /* @__PURE__ */ opacity(toAttrNumberFn);
  var y6 = /* @__PURE__ */ y(toAttrNumberFn);
  var textAnchor4 = /* @__PURE__ */ textAnchor(toAttrString);
  var updateAttrs = /* @__PURE__ */ function() {
    return [classed7(datum_3.nodeClass), transform$prime(datum_3.translateNode)];
  }();
  var treeSceneAttributes = /* @__PURE__ */ function() {
    return {
      circles: [radius6(datum_3.radius), fill6(datum_3.colorByDepth), strokeColor5(datum_3.colorByGroup), strokeWidth5(3)],
      labels: [classed1("label"), x7(4), y(toAttrNumber)(2), text10(datum_3.name)]
    };
  }();
  var svgAttrs = function(w) {
    return function(h) {
      return [viewBox(-w / 2.1)(-h / 2.05)(w)(h), classed1("overlay"), width11(w), height11(h), cursor2("grab")];
    };
  };
  var graphSceneAttributes = /* @__PURE__ */ function() {
    return {
      circles: [radius6(datum_3.radius), fill6(datum_3.colorByGroup), opacity2(datum_3.opacityByType)],
      labels: [classed1("label"), x7(0.2), y6(datum_3.positionLabel), textAnchor4("middle"), text10(datum_3.name)]
    };
  }();
  var enterAttrs = /* @__PURE__ */ function() {
    return [classed7(datum_3.nodeClass), transform$prime(datum_3.translateNode)];
  }();
  var clusterSceneAttributes = /* @__PURE__ */ function() {
    return {
      circles: [radius6(datum_3.radius), fill6(datum_3.fillByUsage), strokeColor5(datum_3.strokeByUsage), strokeWidth5(3), opacity2(datum_3.opacityByType)],
      labels: [classed1("label"), x7(0.2), y6(datum_3.positionLabel), textAnchor4("middle"), text10(datum_3.name)]
    };
  }();

  // output/Stories.Spago.Actions/index.js
  var NodeClick = /* @__PURE__ */ function() {
    function NodeClick2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NodeClick2.create = function(value0) {
      return function(value1) {
        return new NodeClick2(value0, value1);
      };
    };
    return NodeClick2;
  }();
  var TopLevelCSS = /* @__PURE__ */ function() {
    function TopLevelCSS2(value0) {
      this.value0 = value0;
    }
    ;
    TopLevelCSS2.create = function(value0) {
      return new TopLevelCSS2(value0);
    };
    return TopLevelCSS2;
  }();
  var GraphStyle = /* @__PURE__ */ function() {
    function GraphStyle2(value0) {
      this.value0 = value0;
    }
    ;
    GraphStyle2.create = function(value0) {
      return new GraphStyle2(value0);
    };
    return GraphStyle2;
  }();
  var PackageGrid = /* @__PURE__ */ function() {
    function PackageGrid2() {
    }
    ;
    PackageGrid2.value = new PackageGrid2();
    return PackageGrid2;
  }();
  var PackageGraph = /* @__PURE__ */ function() {
    function PackageGraph2() {
    }
    ;
    PackageGraph2.value = new PackageGraph2();
    return PackageGraph2;
  }();
  var ModuleTree = /* @__PURE__ */ function() {
    function ModuleTree2(value0) {
      this.value0 = value0;
    }
    ;
    ModuleTree2.create = function(value0) {
      return new ModuleTree2(value0);
    };
    return ModuleTree2;
  }();
  var LayerSwarm = /* @__PURE__ */ function() {
    function LayerSwarm2() {
    }
    ;
    LayerSwarm2.value = new LayerSwarm2();
    return LayerSwarm2;
  }();
  var LinkShowFilter = /* @__PURE__ */ function() {
    function LinkShowFilter2(value0) {
      this.value0 = value0;
    }
    ;
    LinkShowFilter2.create = function(value0) {
      return new LinkShowFilter2(value0);
    };
    return LinkShowFilter2;
  }();
  var LinkForceFilter = /* @__PURE__ */ function() {
    function LinkForceFilter2(value0) {
      this.value0 = value0;
    }
    ;
    LinkForceFilter2.create = function(value0) {
      return new LinkForceFilter2(value0);
    };
    return LinkForceFilter2;
  }();
  var NodeFilter = /* @__PURE__ */ function() {
    function NodeFilter2(value0) {
      this.value0 = value0;
    }
    ;
    NodeFilter2.create = function(value0) {
      return new NodeFilter2(value0);
    };
    return NodeFilter2;
  }();
  var Initialize6 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var Finalize4 = /* @__PURE__ */ function() {
    function Finalize6() {
    }
    ;
    Finalize6.value = new Finalize6();
    return Finalize6;
  }();
  var Scene = /* @__PURE__ */ function() {
    function Scene2(value0) {
      this.value0 = value0;
    }
    ;
    Scene2.create = function(value0) {
      return new Scene2(value0);
    };
    return Scene2;
  }();
  var ToggleForce2 = /* @__PURE__ */ function() {
    function ToggleForce3(value0) {
      this.value0 = value0;
    }
    ;
    ToggleForce3.create = function(value0) {
      return new ToggleForce3(value0);
    };
    return ToggleForce3;
  }();
  var Filter = /* @__PURE__ */ function() {
    function Filter2(value0) {
      this.value0 = value0;
    }
    ;
    Filter2.create = function(value0) {
      return new Filter2(value0);
    };
    return Filter2;
  }();
  var ChangeStyling = /* @__PURE__ */ function() {
    function ChangeStyling2(value0) {
      this.value0 = value0;
    }
    ;
    ChangeStyling2.create = function(value0) {
      return new ChangeStyling2(value0);
    };
    return ChangeStyling2;
  }();
  var ChangeSimConfig = /* @__PURE__ */ function() {
    function ChangeSimConfig2(value0) {
      this.value0 = value0;
    }
    ;
    ChangeSimConfig2.create = function(value0) {
      return new ChangeSimConfig2(value0);
    };
    return ChangeSimConfig2;
  }();
  var StopSim = /* @__PURE__ */ function() {
    function StopSim2() {
    }
    ;
    StopSim2.value = new StopSim2();
    return StopSim2;
  }();
  var StartSim = /* @__PURE__ */ function() {
    function StartSim2() {
    }
    ;
    StartSim2.value = new StartSim2();
    return StartSim2;
  }();
  var EventFromVizualization = /* @__PURE__ */ function() {
    function EventFromVizualization2(value0) {
      this.value0 = value0;
    }
    ;
    EventFromVizualization2.create = function(value0) {
      return new EventFromVizualization2(value0);
    };
    return EventFromVizualization2;
  }();
  var ToggleChildrenOfNode = /* @__PURE__ */ function() {
    function ToggleChildrenOfNode2(value0) {
      this.value0 = value0;
    }
    ;
    ToggleChildrenOfNode2.create = function(value0) {
      return new ToggleChildrenOfNode2(value0);
    };
    return ToggleChildrenOfNode2;
  }();
  var SpotlightNode = /* @__PURE__ */ function() {
    function SpotlightNode2(value0) {
      this.value0 = value0;
    }
    ;
    SpotlightNode2.create = function(value0) {
      return new SpotlightNode2(value0);
    };
    return SpotlightNode2;
  }();
  var UnToggleChildrenOfNode = /* @__PURE__ */ function() {
    function UnToggleChildrenOfNode2(value0) {
      this.value0 = value0;
    }
    ;
    UnToggleChildrenOfNode2.create = function(value0) {
      return new UnToggleChildrenOfNode2(value0);
    };
    return UnToggleChildrenOfNode2;
  }();

  // output/D3.Examples.Spago.Draw/index.js
  var show19 = /* @__PURE__ */ show(showElement);
  var discard20 = /* @__PURE__ */ discard(discardUnit);
  var classed8 = /* @__PURE__ */ classed(toAttrStringFn);
  var strokeColor6 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var classed12 = /* @__PURE__ */ classed(toAttrString);
  var x13 = /* @__PURE__ */ x1(toAttrNumberFn);
  var y13 = /* @__PURE__ */ y1(toAttrNumberFn);
  var x23 = /* @__PURE__ */ x2(toAttrNumberFn);
  var y23 = /* @__PURE__ */ y2(toAttrNumberFn);
  var updateSimulation = function(dictEq) {
    return function(dictBind) {
      var bind18 = bind(dictBind);
      var discard111 = discard20(dictBind);
      var $$void10 = $$void(dictBind.Apply0().Functor0());
      return function(dictMonadEffect) {
        var pure21 = pure(dictMonadEffect.Monad0().Applicative0());
        return function(dictSelectionM) {
          var openSelection2 = openSelection(dictSelectionM);
          var updateJoin2 = updateJoin(dictSelectionM);
          var appendTo2 = appendTo(dictSelectionM);
          var setAttributes2 = setAttributes(dictSelectionM);
          var selectUnder2 = selectUnder(dictSelectionM);
          var mergeSelections2 = mergeSelections(dictSelectionM);
          var on3 = on2(dictSelectionM);
          return function(dictSimulationM) {
            var mergeNewDataWithSim2 = mergeNewDataWithSim(dictSimulationM)(dictEq);
            var setNodesFromSelection2 = setNodesFromSelection(dictSimulationM);
            var setLinksFromSelection2 = setLinksFromSelection(dictSimulationM);
            var addTickFunction2 = addTickFunction(dictSimulationM);
            return function(v) {
              return function(v1) {
                if (v.selections.nodes instanceof Just && v.selections.links instanceof Just) {
                  return bind18(openSelection2(v.selections.nodes.value0)(show19(Group.value)))(function(node) {
                    return bind18(openSelection2(v.selections.links.value0)(show19(Line.value)))(function(link3) {
                      return bind18(mergeNewDataWithSim2(node)(keyIsID_)(link3)(keyIsID_)(v.rawdata))(function(merged) {
                        return bind18(updateJoin2(node)(Group.value)(merged.nodes)(keyIsID_))(function(node$prime) {
                          return bind18(appendTo2(node$prime.enter)(Group.value)(enterAttrs))(function(nodeEnter) {
                            return bind18(appendTo2(nodeEnter)(Circle.value)(v1.circles))(function() {
                              return discard111($$void10(appendTo2(nodeEnter)(Text2.value)(v1.labels)))(function() {
                                return discard111(setAttributes2(node$prime.exit)([remove]))(function() {
                                  return discard111(setAttributes2(node$prime.update)(updateAttrs))(function() {
                                    return bind18(selectUnder2(node$prime.update)(show19(Circle.value)))(function(updateCirclesSelection) {
                                      return discard111(setAttributes2(updateCirclesSelection)(v1.circles))(function() {
                                        return bind18(selectUnder2(node$prime.update)(show19(Text2.value)))(function(updateLabelsSelection) {
                                          return discard111(setAttributes2(updateLabelsSelection)(v1.labels))(function() {
                                            return bind18(mergeSelections2(nodeEnter)(node$prime.update))(function(mergedNodeSelection) {
                                              return discard111($$void10(on3(mergedNodeSelection)(new Drag(new CustomDrag("spago", simdrag)))))(function() {
                                                return bind18(updateJoin2(link3)(Line.value)(merged.links)(keyIsID_))(function(link$prime) {
                                                  return bind18(appendTo2(link$prime.enter)(Line.value)([classed8(link_2.linkClass), strokeColor6(link_2.color)]))(function(linkEnter) {
                                                    return discard111(setAttributes2(linkEnter)([classed12("enter")]))(function() {
                                                      return discard111(setAttributes2(link$prime.exit)([remove]))(function() {
                                                        return discard111(setAttributes2(link$prime.update)([classed12("update")]))(function() {
                                                          return bind18(mergeSelections2(linkEnter)(link$prime.update))(function(mergedlinksShown) {
                                                            return discard111(setNodesFromSelection2(mergedNodeSelection))(function() {
                                                              return discard111(setLinksFromSelection2(mergedlinksShown)(v.linksWithForce))(function() {
                                                                return discard111(addTickFunction2("nodes")(new Step3(mergedNodeSelection, [transform$prime(datum_3.translateNode)])))(function() {
                                                                  return addTickFunction2("links")(new Step3(mergedlinksShown, [x13(function($64) {
                                                                    return function(v2) {
                                                                      return v2.x;
                                                                    }(link_2.source($64));
                                                                  }), y13(function($65) {
                                                                    return function(v2) {
                                                                      return v2.y;
                                                                    }(link_2.source($65));
                                                                  }), x23(function($66) {
                                                                    return function(v2) {
                                                                      return v2.x;
                                                                    }(link_2.target($66));
                                                                  }), y23(function($67) {
                                                                    return function(v2) {
                                                                      return v2.y;
                                                                    }(link_2.target($67));
                                                                  })]));
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
                    });
                  });
                }
                ;
                return pure21(unit);
              };
            };
          };
        };
      };
    };
  };
  var initialize2 = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(dictMonadEffect) {
      var liftEffect11 = liftEffect(dictMonadEffect);
      var pure21 = pure(dictMonadEffect.Monad0().Applicative0());
      return function(dictSimulationM) {
        return function(dictSelectionM) {
          var attach2 = attach(dictSelectionM);
          var appendTo2 = appendTo(dictSelectionM);
          var on3 = on2(dictSelectionM);
          return bind18(liftEffect11(getWindowWidthHeight))(function(v) {
            return bind18(attach2("div.svg-container"))(function(root) {
              return bind18(appendTo2(root)(Svg.value)(svgAttrs(v.value0)(v.value1)))(function(svg2) {
                return bind18(appendTo2(svg2)(Group.value)([]))(function(inner) {
                  return bind18(on3(inner)(new Drag(DefaultDrag.value)))(function() {
                    return bind18(on3(svg2)(new Zoom({
                      extent: new ZoomExtent({
                        top: 0,
                        left: 0,
                        bottom: v.value1,
                        right: v.value0
                      }),
                      scale: new ScaleExtent(0.1, 4),
                      name: "spago",
                      target: inner
                    })))(function() {
                      return bind18(appendTo2(inner)(Group.value)([classed12("links")]))(function(linksGroup) {
                        return bind18(appendTo2(inner)(Group.value)([classed12("nodes")]))(function(nodesGroup) {
                          return pure21({
                            nodes: new Just(nodesGroup),
                            links: new Just(linksGroup)
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
  var getVizEventFromClick = function(e) {
    return function(d2) {
      return function(t) {
        return new NodeClick(datum_3.nodetype(d2), datum_3.id(d2));
      };
    };
  };

  // output/D3.Data.Graph/index.js
  var bind11 = /* @__PURE__ */ bind(bindMaybe);
  var mapFlipped7 = /* @__PURE__ */ mapFlipped(functorArray);
  var append16 = /* @__PURE__ */ append(semigroupArray);
  var getReachableNodes = function(dictOrd) {
    var lookup14 = lookup9(dictOrd);
    var elem5 = elem2(dictOrd.Eq0());
    return function(id4) {
      return function(graph) {
        var processNextOpenDepPath = function(searchRecord) {
          return bind11(uncons(searchRecord.openDepPaths))(function(x10) {
            return bind11(head2(x10.head))(function(firstID) {
              return bind11(lookup14(firstID)(graph))(function(firstNode) {
                var newDeps = partition(function(d2) {
                  return !elem5(d2)(searchRecord.nodes);
                })(firstNode.links.targets);
                var newOpenDepPaths = mapFlipped7(newDeps.yes)(function(d2) {
                  return cons3(d2)(x10.head);
                });
                var prunedLinks = mapFlipped7(newDeps.no)(function(d2) {
                  return new Tuple(firstID, d2);
                });
                var $12 = $$null3(newOpenDepPaths);
                if ($12) {
                  return new Just({
                    openDepPaths: x10.tail,
                    closedDepPaths: cons3(x10.head)(searchRecord.closedDepPaths),
                    redundantLinks: append16(searchRecord.redundantLinks)(prunedLinks),
                    dependencyTree: searchRecord.dependencyTree,
                    nodes: searchRecord.nodes
                  });
                }
                ;
                return new Just({
                  openDepPaths: append16(x10.tail)(newOpenDepPaths),
                  nodes: append16(searchRecord.nodes)(newDeps.yes),
                  redundantLinks: append16(searchRecord.redundantLinks)(prunedLinks),
                  closedDepPaths: searchRecord.closedDepPaths,
                  dependencyTree: searchRecord.dependencyTree
                });
              });
            });
          });
        };
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.openDepPaths.length === 0) {
              $tco_done = true;
              return v;
            }
            ;
            var v1 = processNextOpenDepPath(v);
            if (v1 instanceof Nothing) {
              $tco_done = true;
              return v;
            }
            ;
            if (v1 instanceof Just) {
              $copy_v = v1.value0;
              return;
            }
            ;
            throw new Error("Failed pattern match at D3.Data.Graph (line 26, column 7 - line 28, column 49): " + [v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2({
          nodes: [],
          openDepPaths: [[id4]],
          closedDepPaths: [],
          redundantLinks: [],
          dependencyTree: Nothing.value
        });
      };
    };
  };

  // output/Data.Tree/index.js
  var Node = /* @__PURE__ */ function() {
    function Node2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return new Node2(value0, value1);
      };
    };
    return Node2;
  }();

  // output/D3.Examples.Spago.Tree/index.js
  var lookup11 = /* @__PURE__ */ lookup(ordInt);
  var map45 = /* @__PURE__ */ map(functorArray);
  var fromFoldable14 = /* @__PURE__ */ fromFoldable(foldableArray);
  var foldl5 = /* @__PURE__ */ foldl(foldableList);
  var fromFoldable15 = /* @__PURE__ */ fromFoldable3(foldableSet);
  var fromFoldable22 = /* @__PURE__ */ fromFoldable6(foldableList)(/* @__PURE__ */ ordTuple(ordInt)(ordInt));
  var insert10 = /* @__PURE__ */ insert(ordInt);
  var map114 = /* @__PURE__ */ map(functorList);
  var getReachableNodes2 = /* @__PURE__ */ getReachableNodes(ordInt);
  var elem4 = /* @__PURE__ */ elem2(eqInt);
  var elem1 = /* @__PURE__ */ elem2(/* @__PURE__ */ eqTuple(eqInt)(eqInt));
  var append17 = /* @__PURE__ */ append(semigroupArray);
  var tupleToLink = function(linktype) {
    return function(v) {
      return {
        source: v.value0,
        target: v.value1,
        linktype,
        inSim: true
      };
    };
  };
  var setNodeXY_ForHorizontalTree = function(nodes) {
    return function(treeDerivedDataMap) {
      var updateXY = function(v) {
        var v1 = lookup11(v.id)(treeDerivedDataMap);
        if (v1 instanceof Nothing) {
          return v;
        }
        ;
        if (v1 instanceof Just) {
          var $46 = {
            x: v1.value0.y - 1200,
            y: v1.value0.x
          };
          return setTreeXYIncludingLeaves(v)({
            x: $46.x,
            y: $46.y,
            depth: v1.value0.depth,
            isTreeLeaf: v1.value0.isTreeLeaf,
            childIDs: v1.value0.childIDs
          });
        }
        ;
        throw new Error("Failed pattern match at D3.Examples.Spago.Tree (line 107, column 7 - line 114, column 134): " + [v1.constructor.name]);
      };
      return map45(updateXY)(nodes);
    };
  };
  var pathsAsLists = function(paths) {
    return fromFoldable14(map45(function($76) {
      return fromFoldable14(reverse2($76));
    })(paths));
  };
  var path2Tuples = function($copy_acc) {
    return function($copy_v) {
      var $tco_var_acc = $copy_acc;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(acc, v) {
        if (v instanceof Nil) {
          $tco_done = true;
          return acc;
        }
        ;
        if (v instanceof Cons && v.value1 instanceof Nil) {
          $tco_done = true;
          return acc;
        }
        ;
        if (v instanceof Cons && v.value1 instanceof Cons) {
          $tco_var_acc = new Cons(new Tuple(v.value0, v.value1.value0), acc);
          $copy_v = new Cons(v.value1.value0, v.value1.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at D3.Examples.Spago.Tree (line 146, column 1 - line 146, column 93): " + [acc.constructor.name, v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_acc, $copy_v);
      }
      ;
      return $tco_result;
    };
  };
  var makeTreeLinkTuples = function(closedPaths) {
    var linkTuples = foldl5(path2Tuples)(Nil.value)(closedPaths);
    return fromFoldable15(fromFoldable22(linkTuples));
  };
  var getTreeDerivedData = function(root) {
    return foldl2(function(acc) {
      return function(v) {
        return insert10(v.data.id)({
          x: v.x,
          y: v.y,
          depth: v.depth,
          isTreeLeaf: hasChildren_(v),
          childIDs: map45(function(v1) {
            return v1.id;
          })(getHierarchyChildren_(v))
        })(acc);
      };
    })(empty2)(descendants_(root));
  };
  var changeLinkType = function(linktype) {
    return function(v) {
      var $70 = {};
      for (var $71 in v) {
        if ({}.hasOwnProperty.call(v, $71)) {
          $70[$71] = v[$71];
        }
        ;
      }
      ;
      $70.linktype = linktype;
      return $70;
    };
  };
  var buildTree = function(rootID) {
    return function(treelinks) {
      var unwrap9 = function(v) {
        return v;
      };
      var linksWhoseSourceIs = function(id4) {
        return fromFoldable14(map45(function(v) {
          return v.target;
        })(filter2(function(l) {
          return l.source === id4;
        })(map45(unwrap9)(treelinks))));
      };
      var go2 = function(childID) {
        return new Node(childID, map114(go2)(linksWhoseSourceIs(childID)));
      };
      return new Node(rootID, map114(go2)(linksWhoseSourceIs(rootID)));
    };
  };
  var treeReduction = function(rootID) {
    return function(model) {
      var reachable = getReachableNodes2(rootID)(model.graph);
      var treenodes = partition(function(v) {
        return elem4(v.id)(reachable.nodes) || v.id === rootID;
      })(model.nodes);
      var prunedTreeLinks = map45(tupleToLink(M2M_Graph.value))(reachable.redundantLinks);
      var onlyTreelinks = makeTreeLinkTuples(pathsAsLists(reachable.closedDepPaths));
      var partitionedLinks = partition(function(v) {
        return elem1(new Tuple(v.source, v.target))(onlyTreelinks);
      })(model.links);
      var treelinks = map45(changeLinkType(M2M_Tree.value))(partitionedLinks.yes);
      var onlyPackageLinks = filter2(isP2P_Link)(model.links);
      var links = append17(treelinks)(append17(prunedTreeLinks)(onlyPackageLinks));
      var idTree = buildTree(rootID)(treelinks);
      var jsontree = makeD3TreeJSONFromTreeID(idTree)(model.maps.id2Node);
      var rootTree = hierarchyFromJSON_(jsontree);
      var numberOfLevels = hNodeHeight_(rootTree) + 1;
      var layout = treeSetNodeSize_(getLayout(TidyTree.value))([8, 4e3 / numberOfLevels]);
      var sortedTree = treeSortForTree_Spago(rootTree);
      var laidOutRoot_ = runLayoutFn_(layout)(sortedTree);
      var tree2 = new Tuple(rootID, laidOutRoot_);
      var treeDerivedDataMap = getTreeDerivedData(laidOutRoot_);
      var positionedNodes = setNodeXY_ForHorizontalTree(treenodes.yes)(treeDerivedDataMap);
      return {
        links,
        nodes: append17(positionedNodes)(treenodes.no),
        graph: model.graph,
        tree: new Just(tree2),
        maps: {
          name2ID: model.maps.name2ID,
          id2Name: model.maps.id2Name,
          id2Node: model.maps.id2Node,
          id2Package: model.maps.id2Package,
          id2LOC: model.maps.id2LOC,
          id2TreeData: treeDerivedDataMap
        }
      };
    };
  };

  // output/Stories.Spago.Forces/index.js
  var strength3 = /* @__PURE__ */ strength(toAttrNumber);
  var x8 = /* @__PURE__ */ x4(toAttrNumber);
  var y7 = /* @__PURE__ */ y4(toAttrNumber);
  var radius7 = /* @__PURE__ */ radius3(toAttrNumberFn);
  var theta2 = /* @__PURE__ */ theta(toAttrNumber);
  var distanceMin2 = /* @__PURE__ */ distanceMin(toAttrNumber);
  var distanceMax2 = /* @__PURE__ */ distanceMax(toAttrNumber);
  var x14 = /* @__PURE__ */ x4(toAttrNumberFn);
  var y14 = /* @__PURE__ */ y4(toAttrNumberFn);
  var radius1 = /* @__PURE__ */ radius3(toAttrNumber);
  var forceLibrary2 = /* @__PURE__ */ function() {
    var usedModulesOnly = new Just(new ForceFilter("used modules only", datum_3.isUsedModule));
    var useGridXY = function(d2) {
      return function(v) {
        return datum_3.gridPoint(d2);
      };
    };
    var unusedModulesOnly = new Just(new ForceFilter("unused modules only", datum_3.isUnusedModule));
    var treeXY = function(d2) {
      return function(v) {
        return datum_3.treePoint(d2);
      };
    };
    var treeExceptLeaves = new Just(new ForceFilter("tree parent nodes only", datum_3.isTreeParent));
    var packagesOnly = new Just(new ForceFilter("all packages", datum_3.isPackage));
    var modulesOnly = new Just(new ForceFilter("all modules", datum_3.isModule));
    var centerXY = function(v) {
      return function(v1) {
        return {
          x: 0,
          y: 0
        };
      };
    };
    return initialize(foldableArray)(functorArray)([createForce("center")(new RegularForce(ForceCenter.value))(allNodes)([strength3(0.5), x8(0), y7(0)]), createForce("x")(new RegularForce(ForceX.value))(allNodes)([strength3(0.05), x8(0)]), createForce("y")(new RegularForce(ForceY.value))(allNodes)([strength3(0.07), y7(0)]), createForce("collide1")(new RegularForce(ForceCollide.value))(allNodes)([strength3(1), radius7(datum_3.collideRadius)]), createForce("collide2")(new RegularForce(ForceCollide.value))(allNodes)([strength3(0.7), radius7(datum_3.collideRadiusBig)]), createForce("charge1")(new RegularForce(ForceManyBody.value))(allNodes)([strength3(-50), theta2(0.9), distanceMin2(1), distanceMax2(infinity)]), createForce("charge2")(new RegularForce(ForceManyBody.value))(allNodes)([strength3(-100), theta2(0.9), distanceMin2(1), distanceMax2(400)]), createForce("chargetree")(new RegularForce(ForceManyBody.value))(treeExceptLeaves)([strength3(-100), theta2(0.9), distanceMin2(1), distanceMax2(400)]), createForce("clusterx_M")(new RegularForce(ForceX.value))(modulesOnly)([strength3(0.2), x14(datum_3.gridPointX)]), createForce("clustery_M")(new RegularForce(ForceY.value))(modulesOnly)([strength3(0.2), y14(datum_3.gridPointY)]), createForce("clusterx_P")(new RegularForce(ForceX.value))(packagesOnly)([strength3(0.8), x14(datum_3.gridPointX)]), createForce("clustery_P")(new RegularForce(ForceY.value))(packagesOnly)([strength3(0.8), y14(datum_3.gridPointY)]), createForce("htreeNodesX")(new RegularForce(ForceX.value))(new Just(new ForceFilter("tree only", function(d2) {
      return datum_3.connected(d2);
    })))([strength3(0.4), x14(datum_3.treePointX)]), createForce("htreeNodesY")(new RegularForce(ForceY.value))(new Just(new ForceFilter("tree only", function(d2) {
      return datum_3.connected(d2);
    })))([strength3(0.4), y14(datum_3.treePointY)]), createForce("vtreeNodesX")(new RegularForce(ForceX.value))(new Just(new ForceFilter("tree only", function(d2) {
      return datum_3.connected(d2);
    })))([strength3(0.4), x14(datum_3.treePointY)]), createForce("vtreeNodesY")(new RegularForce(ForceY.value))(new Just(new ForceFilter("tree only", function(d2) {
      return datum_3.connected(d2);
    })))([strength3(0.4), y14(datum_3.treePointX)]), createForce("packageOrbit")(new RegularForce(ForceRadial.value))(packagesOnly)([strength3(0.7), x8(0), y7(0), radius1(500)]), createForce("unusedOrbit")(new RegularForce(ForceRadial.value))(unusedModulesOnly)([strength3(0.8), x8(0), y7(0), radius1(900)]), createForce("moduleOrbit")(new RegularForce(ForceRadial.value))(usedModulesOnly)([strength3(0.8), x8(0), y7(0), radius1(600)]), createLinkForce(Nothing.value)([strength3(0.5), distance(toAttrNumber)(0), numKey(function($19) {
      return toNumber(datum_3.id($19));
    })])]);
  }();

  // output/D3Tagless.Block.Card/index.js
  var map46 = /* @__PURE__ */ map(functorArray);
  var innerCardClasses = /* @__PURE__ */ map46(ClassName)(["m-6"]);
  var innerCard = function(iprops) {
    return div2(appendIProps([classes(innerCardClasses)])(iprops));
  };
  var innerCard_ = /* @__PURE__ */ innerCard([]);
  var baseCardClasses = /* @__PURE__ */ map46(ClassName)(["bg-white", "mb-6", "rounded", "clearfix"]);
  var baseCard = function(iprops) {
    return div2(appendIProps([classes(baseCardClasses)])(iprops));
  };
  var card = function(iprops) {
    return function(html2) {
      return baseCard(iprops)([innerCard_(html2)]);
    };
  };
  var card_ = /* @__PURE__ */ card([]);

  // output/Ocelot.Block.Button/index.js
  var map47 = /* @__PURE__ */ map(functorArray);
  var append18 = /* @__PURE__ */ append(semigroupArray);
  var rightClasses = /* @__PURE__ */ map47(ClassName)(["rounded-r"]);
  var leftClasses = /* @__PURE__ */ map47(ClassName)(["mr-px", "rounded-l"]);
  var centerClasses = /* @__PURE__ */ map47(ClassName)(["mr-px"]);
  var buttonSharedClasses2 = /* @__PURE__ */ map47(ClassName)(["no-outline", "px-4", "py-2", "!active:border-b", "active:border-t", "disabled:opacity-50", "disabled:cursor-default", "!disabled:cursor-pointer"]);
  var buttonPrimaryClasses = /* @__PURE__ */ map47(ClassName)(["bg-blue-88", "border-blue-88", "hover:!disabled:bg-blue-82", "focus:bg-blue-82", "text-white"]);
  var buttonGroupClasses2 = /* @__PURE__ */ map47(ClassName)(["flex", "items-center"]);
  var buttonGroupBuilder2 = function(classes2) {
    return function(iprops) {
      return button(appendIProps([classes(append18(buttonSharedClasses2)(classes2))])(iprops));
    };
  };
  var buttonPrimaryCenter = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append18(buttonPrimaryClasses)(centerClasses));
  var buttonPrimaryLeft = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append18(buttonPrimaryClasses)(leftClasses));
  var buttonPrimaryRight = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append18(buttonPrimaryClasses)(rightClasses));
  var buttonGroup2 = function(iprops) {
    return div2(appendIProps([classes(buttonGroupClasses2)])(iprops));
  };
  var buttonGroup_ = /* @__PURE__ */ buttonGroup2([]);
  var buttonClasses2 = /* @__PURE__ */ map47(ClassName)(["bg-grey-50-a20", "border-grey-50-a20", "hover:!disabled:bg-grey-50-a30", "focus:bg-grey-50-a30", "text-black-20"]);
  var buttonLeft = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append18(buttonClasses2)(leftClasses));
  var buttonRight = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append18(buttonClasses2)(rightClasses));
  var buttonCenter = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append18(buttonClasses2)(centerClasses));

  // output/Ocelot.Block.Checkbox/index.js
  var map48 = /* @__PURE__ */ map(functorArray);
  var append19 = /* @__PURE__ */ append(semigroupArray);
  var type_20 = /* @__PURE__ */ type_17(isPropInputType);
  var labelClasses4 = /* @__PURE__ */ map48(ClassName)(["flex", "flex-row", "inline-block", "py-2", "cursor-pointer", "text-black-20", "items-center", "text-left"]);
  var inputClasses2 = /* @__PURE__ */ map48(ClassName)(["!disabled:sibling:bg-white", "disabled:sibling:bg-grey-95", "checked:sibling:before:opacity-100", "checked:sibling:before:scale-1", "checked:!disabled:sibling:border-blue-88", "focus:sibling:border-blue-88", "!checked:sibling:before:opacity-0", "!checked:sibling:before:scale-0", "!focus:hover:!checked:!disabled:sibling:border-grey-70", "focus:sibling:shadow", "checked:!disabled:sibling:before:bg-blue-88", "checked:disabled:sibling:before:bg-grey-80", "checked:disabled:sibling:border-grey-80", "offscreen", "checked:sibling:after:opacity-100", "checked:sibling:after:scale-1", "!checked:sibling:after:opacity-0", "!checked:sibling:after:scale-0"]);
  var checkboxClasses = /* @__PURE__ */ map48(ClassName)(["relative", "content-box", "border-2", "border-solid", "h-5", "w-5", "flex-none", "no-content", "mr-3", "rounded", "before:transition-1/4-bounce", "before:absolute", "before:h-full", "before:w-full", "before:no-content", "after:transition-1/4-bounce", "after:absolute", "after:w-full", "after:h-2", "after:border-l-2", "after:border-b-2", "after:border-white", "after:no-content", "after:rotate-315", "after:shadow"]);
  var checkbox = function(iprops) {
    return function(inprops) {
      return function(html2) {
        return label4(appendIProps([classes(labelClasses4)])(iprops))(append19([input2(appendIProps([classes(inputClasses2), type_20(InputCheckbox.value)])(inprops)), span3([classes(checkboxClasses)])([])])(html2));
      };
    };
  };
  var checkbox_ = /* @__PURE__ */ checkbox([]);

  // output/Ocelot.Block.Builder/index.js
  var blockBuilder = function(elem5) {
    return function(classes2) {
      return function(iprops) {
        return elem5(appendIProps([classes(classes2)])(iprops));
      };
    };
  };

  // output/Ocelot.Block.Table/index.js
  var map49 = /* @__PURE__ */ map(functorArray);
  var tableClasses = /* @__PURE__ */ map49(ClassName)(["w-full", "text-left", "border-collapse"]);
  var table2 = /* @__PURE__ */ blockBuilder(table)(tableClasses);
  var table_2 = /* @__PURE__ */ table2([]);
  var row_ = tr_;
  var headerClasses = /* @__PURE__ */ map49(ClassName)(["bg-grey-90", "py-4", "px-5", "font-medium", "text-black-20"]);
  var header2 = /* @__PURE__ */ blockBuilder(th)(headerClasses);
  var cellClasses = /* @__PURE__ */ map49(ClassName)(["bg-white", "p-5", "min-h-20", "border-b", "border-grey-95"]);
  var cell = /* @__PURE__ */ blockBuilder(td)(cellClasses);
  var cell_ = /* @__PURE__ */ cell([]);

  // output/Stories.Spago.State/index.js
  var x9 = /* @__PURE__ */ x(toAttrNumber);
  var _handle3 = /* @__PURE__ */ _handle(strongForget);
  var prop10 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "staging";
    }
  })()();
  var prop110 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "scene";
    }
  })()();
  var prop25 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "attributes";
    }
  })()();
  var prop36 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "rawdata";
    }
  })()();
  var prop44 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "nodes";
    }
  })()();
  var prop52 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "nodeInitializerFunctions";
    }
  })()();
  var prop62 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "model";
    }
  })()();
  var prop72 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "linksWithForce";
    }
  })()();
  var prop82 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "linksShown";
    }
  })()();
  var prop92 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "linksActive";
    }
  })()();
  var prop102 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "links";
    }
  })()();
  var prop123 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "forceStatuses";
    }
  })()();
  var at3 = /* @__PURE__ */ at(/* @__PURE__ */ atMap(ordString));
  var prop132 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "selections";
    }
  })()();
  var prop142 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "cssClass";
    }
  })()();
  var prop152 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "chooseNodes";
    }
  })()();
  var prop162 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "callback";
    }
  })()();
  var initialScene = function(forceLibrary3) {
    return {
      chooseNodes: isPackage,
      linksShown: $$const(false),
      linksActive: $$const(false),
      forceStatuses: getStatusMap(forceLibrary3),
      cssClass: "",
      attributes: clusterSceneAttributes,
      callback: x9(0),
      nodeInitializerFunctions: []
    };
  };
  var getSimulationVariables = function(state3) {
    var handle = view(_handle3)(state3);
    return readSimulationVariables(handle);
  };
  var _staging = function(dictStrong) {
    return prop10($$Proxy.value)(dictStrong);
  };
  var _scene = function(dictStrong) {
    return prop110($$Proxy.value)(dictStrong);
  };
  var _sceneAttributes = function(dictStrong) {
    var $125 = _scene(dictStrong);
    var $126 = prop25($$Proxy.value)(dictStrong);
    return function($127) {
      return $125($126($127));
    };
  };
  var _rawdata = function(dictStrong) {
    return prop36($$Proxy.value)(dictStrong);
  };
  var _nodes = function(dictStrong) {
    return prop44($$Proxy.value)(dictStrong);
  };
  var _stagingNodes = function(dictStrong) {
    var _staging12 = _staging(dictStrong);
    var _rawdata1 = _rawdata(dictStrong);
    var _nodes1 = _nodes(dictStrong);
    return function(dictChoice) {
      return function($128) {
        return _staging12(_rawdata1(_nodes1($128)));
      };
    };
  };
  var _nodeInitializerFunctions = function(dictStrong) {
    var $129 = _scene(dictStrong);
    var $130 = prop52($$Proxy.value)(dictStrong);
    return function($131) {
      return $129($130($131));
    };
  };
  var _model = function(dictStrong) {
    return prop62($$Proxy.value)(dictStrong);
  };
  var _modelNodes = function(dictStrong) {
    var _model1 = _model(dictStrong);
    var _nodes1 = _nodes(dictStrong);
    return function(dictChoice) {
      var $132 = _Just(dictChoice);
      return function($133) {
        return _model1($132(_nodes1($133)));
      };
    };
  };
  var _linksWithForce = function(dictStrong) {
    return prop72($$Proxy.value)(dictStrong);
  };
  var _stagingLinkFilter = function(dictStrong) {
    var $134 = _staging(dictStrong);
    var $135 = _linksWithForce(dictStrong);
    return function($136) {
      return $134($135($136));
    };
  };
  var _linksShown = function(dictStrong) {
    var $137 = _scene(dictStrong);
    var $138 = prop82($$Proxy.value)(dictStrong);
    return function($139) {
      return $137($138($139));
    };
  };
  var _linksActive = function(dictStrong) {
    var $140 = _scene(dictStrong);
    var $141 = prop92($$Proxy.value)(dictStrong);
    return function($142) {
      return $140($141($142));
    };
  };
  var _links = function(dictStrong) {
    return prop102($$Proxy.value)(dictStrong);
  };
  var _modelLinks = function(dictStrong) {
    var _model1 = _model(dictStrong);
    var _links1 = _links(dictStrong);
    return function(dictChoice) {
      var $143 = _Just(dictChoice);
      return function($144) {
        return _model1($143(_links1($144)));
      };
    };
  };
  var _stagingLinks = function(dictStrong) {
    var _staging12 = _staging(dictStrong);
    var _rawdata1 = _rawdata(dictStrong);
    var _links1 = _links(dictStrong);
    return function(dictChoice) {
      return function($145) {
        return _staging12(_rawdata1(_links1($145)));
      };
    };
  };
  var _forceStatuses3 = function(dictStrong) {
    var $146 = _scene(dictStrong);
    var $147 = prop123($$Proxy.value)(dictStrong);
    return function($148) {
      return $146($147($148));
    };
  };
  var _forceStatus3 = function(dictStrong) {
    var _forceStatuses13 = _forceStatuses3(dictStrong);
    return function(dictChoice) {
      var _Just2 = _Just(dictChoice);
      return function(label6) {
        var $149 = at3(label6)(dictStrong);
        return function($150) {
          return _forceStatuses13($149(_Just2($150)));
        };
      };
    };
  };
  var _enterselections = function(dictStrong) {
    return prop132($$Proxy.value)(dictStrong);
  };
  var _cssClass = function(dictStrong) {
    var $151 = _scene(dictStrong);
    var $152 = prop142($$Proxy.value)(dictStrong);
    return function($153) {
      return $151($152($153));
    };
  };
  var _chooseNodes = function(dictStrong) {
    var $154 = _scene(dictStrong);
    var $155 = prop152($$Proxy.value)(dictStrong);
    return function($156) {
      return $154($155($156));
    };
  };
  var _callback = function(dictStrong) {
    var $157 = _scene(dictStrong);
    var $158 = prop162($$Proxy.value)(dictStrong);
    return function($159) {
      return $157($158($159));
    };
  };

  // output/UIGuide.Block.Backdrop/index.js
  var map50 = /* @__PURE__ */ map(functorArray);
  var append20 = /* @__PURE__ */ append(semigroupArray);
  var backdropClasses = /* @__PURE__ */ map50(ClassName)(["p-6", "flex", "flex-1"]);
  var backdropDefaultClasses = /* @__PURE__ */ append20(backdropClasses)(/* @__PURE__ */ map50(ClassName)(["bg-grey-95"]));
  var backdrop = function(iprops) {
    return function(html2) {
      return div2(appendIProps([classes(backdropDefaultClasses)])(iprops))(html2);
    };
  };
  var backdrop_ = /* @__PURE__ */ backdrop([]);

  // output/Stories.Spago.HTML/index.js
  var type_21 = /* @__PURE__ */ type_17(isPropInputType);
  var value14 = /* @__PURE__ */ value12(isPropString);
  var map51 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var _forceLibrary3 = /* @__PURE__ */ _forceLibrary(strongForget);
  var eq6 = /* @__PURE__ */ eq(eqForceStatus);
  var append110 = /* @__PURE__ */ append(semigroupArray);
  var _cssClass2 = /* @__PURE__ */ _cssClass(strongForget);
  var show20 = /* @__PURE__ */ show(showInt);
  var choiceForget3 = /* @__PURE__ */ choiceForget(monoidArray);
  var _stagingLinks2 = /* @__PURE__ */ _stagingLinks(strongForget)(choiceForget3);
  var _stagingNodes2 = /* @__PURE__ */ _stagingNodes(strongForget)(choiceForget3);
  var show110 = /* @__PURE__ */ show(showNumber);
  var slider = function(dictShow) {
    var show26 = show(dictShow);
    return function(config) {
      var toScale = function(s) {
        return toNumber(fromMaybe(0)(fromString(s))) / 100;
      };
      return [onValueInput(function($24) {
        return ChangeSimConfig.create(config["var"](toScale($24)));
      }), type_21(InputRange.value), id2(config.id), class_("scaling-slider"), min5(config.min), max6(config.max), step4(new Step(config.step)), value14(show26(config.value))];
    };
  };
  var slider1 = /* @__PURE__ */ slider(showNumber);
  var renderTableForces = function(state3) {
    var tableData = map51(snd)(toUnfoldable6(view(_forceLibrary3)(state3)));
    var renderHeader = row_([header2([css("w-10")])([text5("Active")]), header2([css("w-2/3 text-left")])([text5("Details")]), header2([css("w-2/3 text-left")])([text5("Acting on...")])]);
    var renderData = function(v) {
      return [cell_([checkbox_([checked2(eq6(v.status)(ForceActive.value)), onChecked($$const(new ToggleForce2(v.name)))])([])]), cell([css("text-left")])([div_([text5(v.name + ("\n" + showType(v.type)))])]), cell([css("text-left")])([text5(showForceFilter(v.filter))])];
    };
    var renderBody = map51(row_)(map51(renderData)(tableData));
    var renderTable = table_2(append110([renderHeader])(renderBody));
    return div_([div2([tailwindClass("text-sm")])([backdrop_([div_([h2_([text5("Control which forces are acting")]), renderTable])])])]);
  };
  var renderSimState = function(state3) {
    return div2([classes(["m-6"])])([caption_2([text5("Simulation state")]), p_([text5("class: " + view(_cssClass2)(state3))]), p_([text5("link count: " + show20(length4(view(_stagingLinks2)(state3))))]), p_([text5("node count:" + show20(length4(view(_stagingNodes2)(state3))))])]);
  };
  var renderSimControls = function(state3) {
    var params = getSimulationVariables(state3);
    return div2([classes(["m-6"])])([subHeading_([text5("Simulation controls")]), div2([classes(["mb-6"])])([contentHeading_([text5("Scenes")]), buttonGroup_([buttonPrimaryLeft([onClick($$const(new Scene(PackageGrid.value)))])([text5("Package Grid")]), buttonPrimaryRight([onClick($$const(new Scene(PackageGraph.value)))])([text5("Package Graph")])])]), div2([classes(["mb-6"])])([buttonGroup_([buttonPrimaryLeft([onClick($$const(new Scene(new ModuleTree(Horizontal.value))))])([text5("Horiz. Tree")]), buttonPrimaryCenter([onClick($$const(new Scene(new ModuleTree(Vertical.value))))])([text5("Vert. Tree")]), buttonPrimaryRight([onClick($$const(new Scene(new ModuleTree(Radial.value))))])([text5("Radial Tree")])])]), div2([classes(["mb-6"])])([buttonGroup_([buttonPrimaryLeft([onClick($$const(new Scene(LayerSwarm.value)))])([text5("LayerSwarm")])])]), div2([classes(["mb-6"])])([contentHeading_([text5("Params")]), input2(slider1({
      "var": Alpha.create,
      id: "alpha-slider",
      min: 0,
      max: 100,
      step: 10,
      value: params.alpha * 100
    })), caption_2([text5("Alpha: " + show110(params.alpha))]), input2(slider1({
      "var": AlphaTarget.create,
      id: "alphatarget-slider",
      min: 0,
      max: 100,
      step: 10,
      value: params.alphaTarget * 100
    })), caption_2([text5("AlphaTarget: " + show110(params.alphaTarget))])]), div2([classes(["mb-6"])])([buttonGroup_([buttonPrimaryLeft([onClick($$const(StopSim.value))])([text5("Stop")]), buttonPrimaryCenter([onClick($$const(new ChangeSimConfig(new AlphaTarget(0.3))))])([text5("Heat")]), buttonPrimaryCenter([onClick($$const(new ChangeSimConfig(new AlphaTarget(0))))])([text5("Cool")]), buttonPrimaryRight([onClick($$const(StartSim.value))])([text5("Start")])])]), div2([classes(["mb-6"])])([contentHeading_([text5("Which nodes should be displayed?")]), buttonGroup_([buttonLeft([onClick($$const(new Filter(new NodeFilter(isPackage))))])([text5("Packages")]), buttonCenter([onClick($$const(new Filter(new NodeFilter($$const(true)))))])([text5("Both")]), buttonCenter([onClick($$const(new Filter(new NodeFilter($$const(false)))))])([text5("Neither")]), buttonRight([onClick($$const(new Filter(new NodeFilter(isUsedModule))))])([text5("Modules")])])]), div2([classes(["mb-6"])])([contentHeading_([text5("Put links into simulation")]), buttonGroup_([buttonLeft([onClick($$const(new Filter(new LinkShowFilter(isM2M_Tree_Link))))])([text5("Treelink")]), buttonCenter([onClick($$const(new Filter(new LinkShowFilter(isM2M_Graph_Link))))])([text5("Graphlink")]), buttonCenter([onClick($$const(new Filter(new LinkShowFilter(isM2P_Link))))])([text5("M2P")]), buttonCenter([onClick($$const(new Filter(new LinkShowFilter(isP2P_Link))))])([text5("P2P")]), buttonRight([onClick($$const(new Filter(new LinkShowFilter($$const(false)))))])([text5("none")])])]), div2([classes(["mb-6"])])([contentHeading_([text5("Limit only these links to exert force?")]), buttonGroup_([buttonLeft([onClick($$const(new Filter(new LinkForceFilter(isM2M_Tree_Link_))))])([text5("Treelink")]), buttonCenter([onClick($$const(new Filter(new LinkForceFilter(isM2M_Graph_Link_))))])([text5("Graphlink")]), buttonCenter([onClick($$const(new Filter(new LinkForceFilter(isM2P_Link_))))])([text5("M2P")]), buttonRight([onClick($$const(new Filter(new LinkForceFilter(isP2P_Link_))))])([text5("P2P")])])]), div2([classes(["mb-6"])])([contentHeading_([text5("D3 attributes chosen")]), buttonGroup_([buttonLeft([onClick($$const(new ChangeStyling(new GraphStyle(clusterSceneAttributes))))])([text5("Clusters")]), buttonCenter([onClick($$const(new ChangeStyling(new GraphStyle(graphSceneAttributes))))])([text5("Graph")]), buttonRight([onClick($$const(new ChangeStyling(new GraphStyle(treeSceneAttributes))))])([text5("Tree")])])]), div2([classes(["mb-6"])])([contentHeading_([text5("Stylesheet")]), buttonGroup_([buttonLeft([onClick($$const(new ChangeStyling(new TopLevelCSS("cluster"))))])([text5("Clusters")]), buttonCenter([onClick($$const(new ChangeStyling(new TopLevelCSS("graph"))))])([text5("Graph")]), buttonCenter([onClick($$const(new ChangeStyling(new TopLevelCSS("tree"))))])([text5("Tree")]), buttonRight([onClick($$const(new ChangeStyling(new TopLevelCSS("none"))))])([text5("None")])])])]);
  };
  var blurbtext4 = /* @__PURE__ */ function() {
    var titleClasses = map51(ClassName)(["font-bold text-2xl"]);
    var title4 = h2([classes(titleClasses)])([text5("About this Example")]);
    var paraTexts = map51(function(s) {
      return [text5(s)];
    })(["This example synthesizes a complex dependency graph from the optional JSON\n        graph outputs of the PureScript compiler, together with the package\n        dependencies from Spago and adds simple line-count per module to give an\n        idea of the size of each one.", "With this dataset, operated on by the physics simulation engine, we can\n      explore different aspects of the project dependencies. The layout can be\n      entirely driven by forces and relationships or partially or totally laid-out\n      using algorithms.", "For example, a dependency tree starting at the Main module can be laid-out as\n      a radial tree and either fixed in that position or allowed to move under the\n      influences of other forces.", "Un-connected modules (which are only present because something in their\n      package has been required) can be hidden or clustered separately.", "Modules can be clustered on their packages and the packages can be positioned\n      on a simple grid or arranged in a circle by a radial force that applies only\n      to them.", "Clicking on a module highlights it and its immediate dependents and\n      dependencies. Clicking outside the highlighted module undoes the\n      highlighting."]);
    var paraClasses = map51(ClassName)(["m-4 "]);
    var paras = map51(p([classes(paraClasses)]))(paraTexts);
    return div_(cons3(title4)(paras));
  }();
  var render2 = function(state3) {
    return div2([tailwindClass("story-container spago")])([div2([tailwindClass("story-panel-about")])([card_([blurbtext4]), renderSimControls(state3), renderSimState(state3), renderTableForces(state3)]), div2([tailwindClass("svg-container " + view(_cssClass2)(state3))])([])]);
  };

  // output/Stories.Spago/index.js
  var _linksShown2 = /* @__PURE__ */ _linksShown(strongForget);
  var _linksActive2 = /* @__PURE__ */ _linksActive(strongForget);
  var _chooseNodes2 = /* @__PURE__ */ _chooseNodes(strongForget);
  var _nodeInitializerFunctions2 = /* @__PURE__ */ _nodeInitializerFunctions(strongForget);
  var discard21 = /* @__PURE__ */ discard(discardUnit);
  var _stagingLinks3 = /* @__PURE__ */ _stagingLinks(strongFn)(choiceFn);
  var choiceForget4 = /* @__PURE__ */ choiceForget(monoidArray);
  var _modelLinks2 = /* @__PURE__ */ _modelLinks(strongForget)(choiceForget4);
  var _stagingLinkFilter2 = /* @__PURE__ */ _stagingLinkFilter(strongFn);
  var _modelNodes2 = /* @__PURE__ */ _modelNodes(strongForget)(choiceForget4);
  var _stagingNodes3 = /* @__PURE__ */ _stagingNodes(strongFn)(choiceFn);
  var liftEffect10 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var _staging2 = /* @__PURE__ */ _staging(strongForget);
  var _callback2 = /* @__PURE__ */ _callback(strongForget);
  var _sceneAttributes2 = /* @__PURE__ */ _sceneAttributes(strongForget);
  var _forceStatuses4 = /* @__PURE__ */ _forceStatuses3(strongForget);
  var discard110 = /* @__PURE__ */ discard21(bindD3SimM);
  var stop2 = /* @__PURE__ */ stop(simulationMD3Selection_D3);
  var actualizeForces3 = /* @__PURE__ */ actualizeForces(simulationMD3Selection_D3);
  var updateSimulation2 = /* @__PURE__ */ updateSimulation(eqInt)(bindD3SimM)(monadEffD3SimM)(selectionMD3Selection_D3S)(simulationMD3Selection_D3);
  var setConfigVariable3 = /* @__PURE__ */ setConfigVariable(simulationMD3Selection_D3);
  var start4 = /* @__PURE__ */ start2(simulationMD3Selection_D3);
  var bind16 = /* @__PURE__ */ bind(bindMaybe);
  var lookup13 = /* @__PURE__ */ lookup(ordString);
  var pure20 = /* @__PURE__ */ pure(applicativeMaybe);
  var bind17 = /* @__PURE__ */ bind(bindAff);
  var apply3 = /* @__PURE__ */ apply(applyEither);
  var map52 = /* @__PURE__ */ map(functorEither);
  var pure110 = /* @__PURE__ */ pure(applicativeAff);
  var bind22 = /* @__PURE__ */ bind(bindHalogenM);
  var discard23 = /* @__PURE__ */ discard21(bindHalogenM);
  var modifying2 = /* @__PURE__ */ modifying(monadStateHalogenM);
  var _model2 = /* @__PURE__ */ _model(strongFn);
  var evalEffectSimulation2 = /* @__PURE__ */ evalEffectSimulation(bindHalogenM)(monadStateHalogenM);
  var initialize3 = /* @__PURE__ */ initialize2(bindD3SimM)(monadEffD3SimM)(simulationMD3Selection_D3)(selectionMD3Selection_D3S);
  var _staging1 = /* @__PURE__ */ _staging(strongFn);
  var _enterselections2 = /* @__PURE__ */ _enterselections(strongFn);
  var _nodes2 = /* @__PURE__ */ _nodes(strongFn);
  var _links2 = /* @__PURE__ */ _links(strongFn);
  var $$void9 = /* @__PURE__ */ $$void(functorHalogenM);
  var assign3 = /* @__PURE__ */ assign2(monadStateHalogenM);
  var _callback1 = /* @__PURE__ */ _callback(strongFn);
  var pure23 = /* @__PURE__ */ pure(applicativeHalogenM);
  var _chooseNodes1 = /* @__PURE__ */ _chooseNodes(strongFn);
  var runWithD3_Simulation2 = /* @__PURE__ */ runWithD3_Simulation(bindHalogenM)(monadStateHalogenM);
  var _linksShown1 = /* @__PURE__ */ _linksShown(strongFn);
  var _linksActive1 = /* @__PURE__ */ _linksActive(strongFn);
  var _cssClass3 = /* @__PURE__ */ _cssClass(strongFn);
  var _sceneAttributes1 = /* @__PURE__ */ _sceneAttributes(strongFn);
  var _forceStatuses12 = /* @__PURE__ */ _forceStatuses3(strongFn);
  var onlyTheseForcesActive2 = /* @__PURE__ */ onlyTheseForcesActive(foldableArray)(functorArray);
  var _nodeInitializerFunctions1 = /* @__PURE__ */ _nodeInitializerFunctions(strongFn);
  var _forceStatus4 = /* @__PURE__ */ _forceStatus3(strongFn)(choiceFn);
  var stageDataFromModel = function(dictMonadState) {
    var Bind1 = dictMonadState.Monad0().Bind1();
    var bind32 = bind(Bind1);
    var use3 = use(dictMonadState);
    var discard32 = discard21(Bind1);
    var assign1 = assign2(dictMonadState);
    return bind32(get(dictMonadState))(function(state3) {
      return bind32(use3(_linksShown2))(function(linksShown) {
        return bind32(use3(_linksActive2))(function(linksActive) {
          return bind32(use3(_chooseNodes2))(function(chooseNodes) {
            return bind32(use3(_nodeInitializerFunctions2))(function(nodeInitializerFunctions) {
              return discard32(assign1(_stagingLinks3)(filter2(linksShown)(view(_modelLinks2)(state3))))(function() {
                return discard32(assign1(_stagingLinkFilter2)(linksActive))(function() {
                  var rawnodes = filter2(chooseNodes)(view(_modelNodes2)(state3));
                  var initializedNodes = foldl2(function(b2) {
                    return function(a2) {
                      return a2(b2);
                    };
                  })(rawnodes)(nodeInitializerFunctions);
                  return assign1(_stagingNodes3)(initializedNodes);
                });
              });
            });
          });
        });
      });
    });
  };
  var simulationEvent = function(l) {
    return onMouseEventEffectful(MouseClick.value)(function(e) {
      return function(d2) {
        return function(t) {
          return liftEffect10(notify(l)(new EventFromVizualization(getVizEventFromClick(e)(d2)(t))));
        };
      };
    });
  };
  var runSimulation = function(dictMonadEffect) {
    var Bind1 = dictMonadEffect.Monad0().Bind1();
    var discard32 = discard21(Bind1);
    var bind32 = bind(Bind1);
    var runWithD3_Simulation1 = runWithD3_Simulation(Bind1);
    return function(dictMonadState) {
      var use3 = use(dictMonadState);
      var runWithD3_Simulation22 = runWithD3_Simulation1(dictMonadState)(dictMonadEffect);
      return discard32(stageDataFromModel(dictMonadState))(function() {
        return bind32(use3(_staging2))(function(staging) {
          return bind32(use3(_callback2))(function(callback) {
            return bind32(use3(_sceneAttributes2))(function(sceneAttributes) {
              return bind32(use3(_forceStatuses4))(function(forceStatuses) {
                var attributesWithCallback = {
                  circles: cons3(callback)(sceneAttributes.circles),
                  labels: sceneAttributes.labels
                };
                return runWithD3_Simulation22(discard110(stop2)(function() {
                  return discard110(actualizeForces3(forceStatuses))(function() {
                    return discard110(updateSimulation2(staging)(attributesWithCallback))(function() {
                      return discard110(setConfigVariable3(new Alpha(1)))(function() {
                        return start4;
                      });
                    });
                  });
                }));
              });
            });
          });
        });
      });
    };
  };
  var addTreeToModel = function(rootName) {
    return function(maybeModel) {
      return bind16(maybeModel)(function(model) {
        return bind16(lookup13(rootName)(model.maps.name2ID))(function(rootID) {
          return pure20(treeReduction(rootID)(model));
        });
      });
    };
  };
  var readModelData = /* @__PURE__ */ function() {
    return bind17(get4(string)("./data/spago-data/modules.json"))(function(moduleJSON) {
      return bind17(get4(string)("./data/spago-data/packages.json"))(function(packageJSON) {
        return bind17(get4(string)("./data/spago-data/lsdeps.jsonlines"))(function(lsdepJSON) {
          return bind17(get4(string)("./data/spago-data/LOC.json"))(function(locJSON) {
            var model = hush(apply3(apply3(apply3(map52(convertFilesToGraphModel)(moduleJSON))(packageJSON))(lsdepJSON))(locJSON));
            return pure110(addTreeToModel("Main")(model));
          });
        });
      });
    });
  }();
  var handleAction5 = function(dictMonadAff) {
    var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
    var monadEffectHalogenM2 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    var evalEffectSimulation1 = evalEffectSimulation2(monadEffectHalogenM2);
    var liftEffect12 = liftEffect(monadEffectHalogenM2);
    var runSimulation1 = runSimulation(monadEffectHalogenM2)(monadStateHalogenM);
    var runWithD3_Simulation1 = runWithD3_Simulation2(monadEffectHalogenM2);
    return function(v) {
      if (v instanceof Initialize6) {
        return bind22(liftAff2(readModelData))(function(v1) {
          return discard23(modifying2(_model2)($$const(v1)))(function() {
            return bind22(evalEffectSimulation1(initialize3))(function(openSelections) {
              return discard23(modifying2(function($143) {
                return _staging1(_enterselections2(_nodes2($143)));
              })($$const(openSelections.nodes)))(function() {
                return discard23(modifying2(function($144) {
                  return _staging1(_enterselections2(_links2($144)));
                })($$const(openSelections.links)))(function() {
                  return bind22(liftEffect12(create3))(function(v2) {
                    return discard23($$void9(subscribe2(v2.emitter)))(function() {
                      return discard23(assign3(_callback1)(simulationEvent(v2.listener)))(function() {
                        return pure23(unit);
                      });
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof Finalize4) {
        return pure23(unit);
      }
      ;
      if (v instanceof EventFromVizualization) {
        if (v.value0.value0 instanceof IsPackage) {
          return handleAction5(dictMonadAff)(new ToggleChildrenOfNode(v.value0.value1));
        }
        ;
        if (v.value0.value0 instanceof IsModule) {
          return handleAction5(dictMonadAff)(new SpotlightNode(v.value0.value1));
        }
        ;
        throw new Error("Failed pattern match at Stories.Spago (line 90, column 5 - line 92, column 68): " + [v.value0.constructor.name]);
      }
      ;
      if (v instanceof ToggleChildrenOfNode) {
        return discard23(assign3(_chooseNodes1)(isPackageOrVisibleModule(v.value0)))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof UnToggleChildrenOfNode) {
        return discard23(assign3(_chooseNodes1)(isPackage))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof SpotlightNode) {
        return runWithD3_Simulation1(stop2);
      }
      ;
      if (v instanceof Scene && v.value0 instanceof PackageGrid) {
        return discard23(assign3(_chooseNodes1)(allNodes2))(function() {
          return discard23(assign3(_linksShown1)(isM2P_Link))(function() {
            return discard23(assign3(_linksActive1)($$const(true)))(function() {
              return discard23(assign3(_cssClass3)("cluster"))(function() {
                return discard23(assign3(_sceneAttributes1)(clusterSceneAttributes))(function() {
                  return discard23(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["clusterx_P", "clustery_P", "clusterx_M", "clustery_M", "collide2"])))(function() {
                    return discard23(assign3(_nodeInitializerFunctions1)([unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY]))(function() {
                      return runSimulation1;
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof Scene && v.value0 instanceof PackageGraph) {
        return discard23(assign3(_chooseNodes1)(isPackage))(function() {
          return discard23(assign3(_linksShown1)(isP2P_Link))(function() {
            return discard23(assign3(_linksActive1)(sourcePackageIs("my-project")))(function() {
              return discard23(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["center", "collide2", "charge2", "packageOrbit", linksForceName])))(function() {
                return discard23(assign3(_cssClass3)("graph"))(function() {
                  return discard23(assign3(_sceneAttributes1)(graphSceneAttributes))(function() {
                    return discard23(assign3(_nodeInitializerFunctions1)([unpinAllNodes, packagesNodesToPhyllotaxis, fixNamedNodeTo("my-project")({
                      x: 0,
                      y: 0
                    })]))(function() {
                      return runSimulation1;
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof Scene && v.value0 instanceof LayerSwarm) {
        return discard23(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard23(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard23(assign3(_linksActive1)($$const(true)))(function() {
              return discard23(assign3(_cssClass3)("tree"))(function() {
                return discard23(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard23(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["htreeNodesX", "collide1", "y", linksForceName])))(function() {
                    return discard23(assign3(_nodeInitializerFunctions1)([unpinAllNodes]))(function() {
                      return runSimulation1;
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof Scene && (v.value0 instanceof ModuleTree && v.value0.value0 instanceof Radial)) {
        return discard23(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard23(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard23(assign3(_linksActive1)($$const(true)))(function() {
              return discard23(assign3(_cssClass3)("tree radial"))(function() {
                return discard23(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard23(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["center", "collide2", "chargetree", "charge2", linksForceName])))(function() {
                    return discard23(assign3(_nodeInitializerFunctions1)([unpinAllNodes, treeNodesToTreeXY_R, fixNamedNodeTo("Main")({
                      x: 0,
                      y: 0
                    })]))(function() {
                      return runSimulation1;
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof Scene && (v.value0 instanceof ModuleTree && v.value0.value0 instanceof Horizontal)) {
        return discard23(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard23(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard23(assign3(_linksActive1)($$const(false)))(function() {
              return discard23(assign3(_cssClass3)("tree horizontal"))(function() {
                return discard23(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard23(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["htreeNodesX", "htreeNodesY", "charge1", "collide2"])))(function() {
                    return discard23(assign3(_nodeInitializerFunctions1)([unpinAllNodes]))(function() {
                      return runSimulation1;
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof Scene && (v.value0 instanceof ModuleTree && v.value0.value0 instanceof Vertical)) {
        return discard23(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard23(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard23(assign3(_linksActive1)($$const(false)))(function() {
              return discard23(assign3(_cssClass3)("tree vertical"))(function() {
                return discard23(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard23(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["vtreeNodesX", "vtreeNodesY", "charge1", "collide2"])))(function() {
                    return discard23(assign3(_nodeInitializerFunctions1)([unpinAllNodes]))(function() {
                      return runSimulation1;
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof ToggleForce2) {
        return discard23(modifying2(_forceStatus4(v.value0))(toggleForceStatus))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof Filter && v.value0 instanceof LinkShowFilter) {
        return discard23(assign3(_linksShown1)(v.value0.value0))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof Filter && v.value0 instanceof LinkForceFilter) {
        return discard23(assign3(_linksActive1)(v.value0.value0))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof Filter && v.value0 instanceof NodeFilter) {
        return discard23(assign3(_chooseNodes1)(v.value0.value0))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof ChangeStyling && v.value0 instanceof TopLevelCSS) {
        return assign3(_cssClass3)(v.value0.value0);
      }
      ;
      if (v instanceof ChangeStyling && v.value0 instanceof GraphStyle) {
        return discard23(assign3(_sceneAttributes1)(v.value0.value0))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof ChangeSimConfig) {
        return runWithD3_Simulation1(setConfigVariable3(v.value0));
      }
      ;
      if (v instanceof StartSim) {
        return runWithD3_Simulation1(discard110(setConfigVariable3(new Alpha(1)))(function() {
          return start4;
        }));
      }
      ;
      if (v instanceof StopSim) {
        return runWithD3_Simulation1(discard110(setConfigVariable3(new Alpha(0)))(function() {
          return stop2;
        }));
      }
      ;
      throw new Error("Failed pattern match at Stories.Spago (line 65, column 16 - line 209, column 11): " + [v.constructor.name]);
    };
  };
  var component5 = function(dictMonadAff) {
    var initialState = {
      model: Nothing.value,
      staging: {
        selections: {
          nodes: Nothing.value,
          links: Nothing.value
        },
        linksWithForce: $$const(true),
        rawdata: {
          nodes: [],
          links: []
        }
      },
      simulation: initialSimulationState(forceLibrary2),
      scene: initialScene(forceLibrary2)
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render2,
      "eval": mkEval({
        handleAction: handleAction5(dictMonadAff),
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize6.value),
        finalize: new Just(Finalize4.value)
      })
    });
  };

  // output/D3.Examples.ThreeLittleCircles/index.js
  var fill7 = /* @__PURE__ */ fill(toAttrString);
  var cx3 = /* @__PURE__ */ cx(toAttrNumberFnI);
  var cy3 = /* @__PURE__ */ cy(toAttrNumber);
  var radius8 = /* @__PURE__ */ radius(toAttrNumber);
  var classed9 = /* @__PURE__ */ classed(toAttrString);
  var discard24 = /* @__PURE__ */ discard(discardUnit);
  var strokeColor7 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var strokeWidth6 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var cy1 = /* @__PURE__ */ cy(toAttrNumberFn);
  var xFromIndex = function(v) {
    return function(i2) {
      return i2 * 100;
    };
  };
  var drawThreeCircles = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind18 = bind(Bind1);
    var attach2 = attach(dictSelectionM);
    var appendTo2 = appendTo(dictSelectionM);
    var simpleJoin2 = simpleJoin(dictSelectionM);
    var discard111 = discard24(Bind1);
    var setAttributes2 = setAttributes(dictSelectionM);
    var pure21 = pure(Monad0.Applicative0());
    return function(selector) {
      var circleAttributes = [fill7("green"), cx3(xFromIndex), cy3(50), radius8(20)];
      return bind18(attach2(selector))(function(root) {
        return bind18(appendTo2(root)(Svg.value)([viewBox(-100)(-100)(650)(650), classed9("d3svg gup")]))(function(svg2) {
          return bind18(appendTo2(svg2)(Group.value)([]))(function(circleGroup) {
            return bind18(simpleJoin2(circleGroup)(Circle.value)([32, 57, 293])(keyIsID_))(function(circles) {
              return discard111(setAttributes2(circles)(circleAttributes))(function() {
                return pure21(circles);
              });
            });
          });
        });
      });
    };
  };
  var datum_4 = {
    x: function(v) {
      return function(i2) {
        return toNumber(i2) * 20;
      };
    },
    y: function(d2) {
      return 100 - toNumber(d2) / 5;
    },
    color: function(d2) {
      return d3SchemePairedN_(toNumber(d2) / 100);
    }
  };
  var drawWithData = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind18 = bind(Bind1);
    var attach2 = attach(dictSelectionM);
    var appendTo2 = appendTo(dictSelectionM);
    var simpleJoin2 = simpleJoin(dictSelectionM);
    var discard111 = discard24(Bind1);
    var setAttributes2 = setAttributes(dictSelectionM);
    var pure21 = pure(Monad0.Applicative0());
    return function(circleData) {
      return function(selector) {
        var circleAttributes = [strokeColor7(datum_4.color), strokeWidth6(3), fill7("none"), cx3(datum_4.x), cy1(datum_4.y), radius8(10)];
        return bind18(attach2(selector))(function(root) {
          return bind18(appendTo2(root)(Svg.value)([viewBox(-100)(-100)(650)(650), classed9("d3svg gup")]))(function(svg2) {
            return bind18(appendTo2(svg2)(Group.value)([]))(function(circleGroup) {
              return bind18(simpleJoin2(circleGroup)(Circle.value)(circleData)(keyIsID_))(function(circles) {
                return discard111(setAttributes2(circles)(circleAttributes))(function() {
                  return pure21(circles);
                });
              });
            });
          });
        });
      };
    };
  };

  // output/Stories.ThreeLittleCircles/index.js
  var prop20 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "parabola";
    }
  })()();
  var prop111 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "simple";
    }
  })()();
  var not9 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var traverse4 = /* @__PURE__ */ traverse(traversableArray);
  var discard25 = /* @__PURE__ */ discard(discardUnit);
  var drawThreeCircles2 = /* @__PURE__ */ drawThreeCircles(d3TaglessD3M);
  var removeExistingSVG4 = /* @__PURE__ */ removeExistingSVG(d3TaglessD3M);
  var drawWithData2 = /* @__PURE__ */ drawWithData(d3TaglessD3M);
  var Initialize7 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var Finalize5 = /* @__PURE__ */ function() {
    function Finalize6() {
    }
    ;
    Finalize6.value = new Finalize6();
    return Finalize6;
  }();
  var ToggleCard5 = /* @__PURE__ */ function() {
    function ToggleCard7(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard7.create = function(value0) {
      return new ToggleCard7(value0);
    };
    return ToggleCard7;
  }();
  var ToggleExample = /* @__PURE__ */ function() {
    function ToggleExample2() {
    }
    ;
    ToggleExample2.value = new ToggleExample2();
    return ToggleExample2;
  }();
  var simple = /* @__PURE__ */ function() {
    return [new Blurb("Simplest possible example, just to show syntax."), new SnippetFile("TLCSimple"), new Blurb("Click the button to see a slightly more realistic example."), new PreRendered(buttonVertical([onClick($$const(ToggleExample.value))])([text5("Simple")]))];
  }();
  var parabola = /* @__PURE__ */ function() {
    return [new Blurb("This extends the super-simple model in the direction one would go for a more real-world example."), new SnippetFile("TLCParabola"), new Blurb("In this example, the data is passed in and must match the type\n  specified in the Model. Because the data loses its type information when\n  put into D3 we recover the type of Datum and Index using a couple of\n  functions to wrap unsafeCoerce. These functions are then used to write\n  any attribute setters that are derived from the data elements themselves,\n  or their indices"), new SnippetFile("TLCDatum_"), new Blurb("Again, we're just showing syntax and shape of the DSL here: it's still extremely simple, and the Model,\n  datum_ and so on might not be needed for such a simple example."), new PreRendered(buttonVertical([onClick($$const(ToggleExample.value))])([text5("Parabola")]))];
  }();
  var _notebooks = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "notebooks";
      }
    })()()($$Proxy.value);
  }();
  var _parabola = function(dictStrong) {
    var $73 = _notebooks(dictStrong);
    var $74 = prop20($$Proxy.value)(dictStrong);
    return function($75) {
      return $73($74($75));
    };
  };
  var _parabola1 = /* @__PURE__ */ _parabola(strongFn);
  var _parabola2 = /* @__PURE__ */ _parabola(strongForget);
  var _simple = function(dictStrong) {
    var $76 = _notebooks(dictStrong);
    var $77 = prop111($$Proxy.value)(dictStrong);
    return function($78) {
      return $76($77($78));
    };
  };
  var _simple1 = /* @__PURE__ */ _simple(strongFn);
  var _simple2 = /* @__PURE__ */ _simple(strongForget);
  var handleAction6 = function(dictBind) {
    var bind18 = bind(dictBind);
    var substituteSnippetCells2 = substituteSnippetCells(dictBind);
    var discard111 = discard25(dictBind);
    var $$void10 = $$void(dictBind.Apply0().Functor0());
    return function(dictMonadAff) {
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var Applicative0 = MonadEffect0.Monad0().Applicative0();
      var traverse12 = traverse4(Applicative0);
      var substituteSnippetCells1 = substituteSnippetCells2(dictMonadAff);
      var liftEffect11 = liftEffect(MonadEffect0);
      var pure21 = pure(Applicative0);
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var substituteSnippetCells22 = substituteSnippetCells1(dictMonadState);
        var assign4 = assign2(dictMonadState);
        var gets2 = gets(dictMonadState);
        var modify_6 = modify_(dictMonadState);
        return function(v) {
          if (v instanceof ToggleCard5) {
            return modifying3(v.value0(strongFn))(not9);
          }
          ;
          if (v instanceof Initialize7) {
            return bind18(traverse12(substituteSnippetCells22)(simple))(function(simple$prime) {
              return discard111(assign4(_simple1)(simple$prime))(function() {
                return bind18(traverse12(substituteSnippetCells22)(parabola))(function(parabola$prime) {
                  return discard111(assign4(_parabola1)(parabola$prime))(function() {
                    return bind18(liftEffect11(eval_D3M(drawThreeCircles2("div.svg-container"))))(function() {
                      return pure21(unit);
                    });
                  });
                });
              });
            });
          }
          ;
          if (v instanceof ToggleExample) {
            return bind18(gets2(function(v1) {
              return v1.toggle;
            }))(function(toggle2) {
              var toggle$prime = !toggle2;
              return discard111($$void10(liftEffect11(eval_D3M(removeExistingSVG4("div.svg-container")))))(function() {
                return discard111($$void10(liftEffect11(eval_D3M(function() {
                  if (toggle$prime) {
                    return drawThreeCircles2("div.svg-container");
                  }
                  ;
                  return drawWithData2([310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310])("div.svg-container");
                }()))))(function() {
                  return modify_6(function(s) {
                    var $69 = {};
                    for (var $70 in s) {
                      if ({}.hasOwnProperty.call(s, $70)) {
                        $69[$70] = s[$70];
                      }
                      ;
                    }
                    ;
                    $69.toggle = toggle$prime;
                    return $69;
                  });
                });
              });
            });
          }
          ;
          if (v instanceof Finalize5) {
            return pure21(unit);
          }
          ;
          throw new Error("Failed pattern match at Stories.ThreeLittleCircles (line 90, column 16 - line 112, column 24): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction15 = /* @__PURE__ */ handleAction6(bindHalogenM);
  var _code5 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "code";
      }
    })()()($$Proxy.value);
  }();
  var component6 = function(dictMonadAff) {
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-code")])([field_({
        label: text5("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked2(toBoolean(state3.code)), onChange(function(v) {
        return new ToggleCard5(function(dictStrong) {
          return _code5(dictStrong);
        });
      })])]), content_(state3.code)(function() {
        if (state3.toggle) {
          return renderNotebook_(view(_simple2)(state3));
        }
        ;
        return renderNotebook_(view(_parabola2)(state3));
      }())]), div2([tailwindClass("svg-container")])([])]);
    };
    var initialState = {
      toggle: true,
      code: Expanded.value,
      notebooks: {
        simple,
        parabola
      }
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleAction: handleAction15(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize7.value),
        finalize: new Just(Finalize5.value)
      })
    });
  };

  // output/Ocelot.Block.Radio/index.js
  var map53 = /* @__PURE__ */ map(functorArray);
  var append21 = /* @__PURE__ */ append(semigroupArray);
  var type_22 = /* @__PURE__ */ type_17(isPropInputType);
  var radioClasses = /* @__PURE__ */ map53(ClassName)(["inline-flex", "justify-center", "items-center", "content-box", "border-2", "border-solid", "h-4", "w-4", "p-1", "flex-none", "no-content", "rounded-full", "mr-3", "before:transition-1/4-bounce", "before:h-full", "before:w-full", "before:bg-blue-88", "before:no-content", "before:rounded-full", "before:shadow"]);
  var labelClasses5 = /* @__PURE__ */ map53(ClassName)(["flex", "flex-row", "inline-block", "py-2", "cursor-pointer", "text-black-20", "items-center", "text-left"]);
  var inputClasses3 = /* @__PURE__ */ map53(ClassName)(["!disabled:sibling:bg-white", "disabled:sibling:bg-grey-95", "checked:sibling:before:opacity-100", "checked:sibling:before:scale-1", "checked:!disabled:sibling:border-blue-88", "focus:sibling:border-blue-88", "!checked:sibling:before:opacity-0", "!checked:sibling:before:scale-0", "!focus:hover:!checked:!disabled:sibling:border-grey-70", "focus:sibling:shadow", "checked:!disabled:sibling:before:bg-blue-88", "checked:disabled:sibling:before:bg-grey-80", "checked:disabled:sibling:border-grey-80", "offscreen"]);
  var radio = function(iprops) {
    return function(inprops) {
      return function(html2) {
        return label4(appendIProps([classes(labelClasses5)])(iprops))(append21([input2(appendIProps([classes(inputClasses3), type_22(InputRadio.value)])(inprops)), span3([classes(radioClasses)])([])])(html2));
      };
    };
  };

  // output/Stories.Trees/index.js
  var prop21 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "draw";
    }
  })()();
  var not10 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var removeExistingSVG5 = /* @__PURE__ */ removeExistingSVG(d3TaglessD3M);
  var discard26 = /* @__PURE__ */ discard(discardUnit);
  var makeModel4 = /* @__PURE__ */ makeModel(bindAff)(monadEffectAff);
  var prop112 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "code";
    }
  })()();
  var prop26 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "blurb";
    }
  })()();
  var Initialize8 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var SetLayout = /* @__PURE__ */ function() {
    function SetLayout2(value0) {
      this.value0 = value0;
    }
    ;
    SetLayout2.create = function(value0) {
      return new SetLayout2(value0);
    };
    return SetLayout2;
  }();
  var SetType = /* @__PURE__ */ function() {
    function SetType2(value0) {
      this.value0 = value0;
    }
    ;
    SetType2.create = function(value0) {
      return new SetType2(value0);
    };
    return SetType2;
  }();
  var ToggleCard6 = /* @__PURE__ */ function() {
    function ToggleCard7(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard7.create = function(value0) {
      return new ToggleCard7(value0);
    };
    return ToggleCard7;
  }();
  var blurbtext5 = /* @__PURE__ */ blurbParagraphs(functorArray)(["An abstract data type like a tree can be rendered in a number of different\n    ways including at least the 6 variations shown here, arising from a\n    combination of three layout orientations (Horizontal, Vertical and Radial)\n    and to layout types (TidyTree or Dendrogram)", "Each format has it's uses, TidyTree forms are generally more compact and\n  will often be preferred.", "In addition to the six options shown here (which have fundamentally the\n  same structure in the DOM) there are radically different representations such\n  as Sunflowers and TreeMaps which can be used to show the same hierarchical\n  data in ways that serve different purposes or make different aspects of the\n  data salient.", "The code shown in this example makes use of higher order functions to\n  parameterize the drawing, thus enabling one function to encode all six\n  forms."]);
  var _snippets4 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  }();
  var _panels5 = /* @__PURE__ */ function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  }();
  var _drawCode4 = function(dictStrong) {
    var $97 = _snippets4(dictStrong);
    var $98 = prop21($$Proxy.value)(dictStrong);
    return function($99) {
      return $97($98($99));
    };
  };
  var _drawCode13 = /* @__PURE__ */ _drawCode4(strongFn);
  var _drawCode23 = /* @__PURE__ */ _drawCode4(strongForget);
  var handleAction7 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard26(dictBind);
    return function(dictMonadAff) {
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var liftEffect11 = liftEffect(MonadEffect0);
      var liftAff2 = liftAff(dictMonadAff);
      var pure21 = pure(MonadEffect0.Monad0().Applicative0());
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var assign4 = assign2(dictMonadState);
        var modify_6 = modify_(dictMonadState);
        var get6 = get(dictMonadState);
        return function(v) {
          if (v instanceof ToggleCard6) {
            return modifying3(v.value0(strongFn))(not10);
          }
          ;
          if (v instanceof Initialize8) {
            return bind18(liftEffect11(eval_D3M(removeExistingSVG5("div.svg-container"))))(function(detached) {
              return bind18(liftAff2(readSnippetFiles("TreeDraw")))(function(text1) {
                return discard111(assign4(_drawCode13)(text1))(function() {
                  return bind18(liftAff2(getTreeViaAJAX("./data/flare-2.json")))(function(treeJSON) {
                    return discard111(function() {
                      if (treeJSON instanceof Left) {
                        return pure21(unit);
                      }
                      ;
                      if (treeJSON instanceof Right) {
                        return bind18(liftAff2(makeModel4(TidyTree.value)(Vertical.value)(treeJSON.value0)))(function(model) {
                          return bind18(liftAff2(drawTree2(model)("div.svg-container")))(function() {
                            return discard111(modify_6(function(st) {
                              var $77 = {};
                              for (var $78 in st) {
                                if ({}.hasOwnProperty.call(st, $78)) {
                                  $77[$78] = st[$78];
                                }
                                ;
                              }
                              ;
                              $77.tree = new Just(model);
                              return $77;
                            }))(function() {
                              return pure21(unit);
                            });
                          });
                        });
                      }
                      ;
                      throw new Error("Failed pattern match at Stories.Trees (line 180, column 5 - line 186, column 18): " + [treeJSON.constructor.name]);
                    }())(function() {
                      return pure21(unit);
                    });
                  });
                });
              });
            });
          }
          ;
          if (v instanceof SetLayout) {
            return bind18(liftEffect11(eval_D3M(removeExistingSVG5("div.svg-container"))))(function(detached) {
              return bind18(get6)(function(v1) {
                if (v1.tree instanceof Nothing) {
                  return pure21(unit);
                }
                ;
                if (v1.tree instanceof Just) {
                  var updated = {
                    treeLayout: v.value0,
                    json: v1.tree.value0.json,
                    svgConfig: v1.tree.value0.svgConfig,
                    treeLayoutFn: v1.tree.value0.treeLayoutFn,
                    treeType: v1.tree.value0.treeType
                  };
                  return bind18(liftAff2(drawTree2(updated)("div.svg-container")))(function() {
                    return discard111(modify_6(function(st) {
                      var $83 = {};
                      for (var $84 in st) {
                        if ({}.hasOwnProperty.call(st, $84)) {
                          $83[$84] = st[$84];
                        }
                        ;
                      }
                      ;
                      $83.tree = new Just(updated);
                      return $83;
                    }))(function() {
                      return pure21(unit);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Stories.Trees (line 193, column 5 - line 199, column 18): " + [v1.tree.constructor.name]);
              });
            });
          }
          ;
          if (v instanceof SetType) {
            return bind18(liftEffect11(eval_D3M(removeExistingSVG5("div.svg-container"))))(function(detached) {
              return bind18(get6)(function(v1) {
                if (v1.tree instanceof Nothing) {
                  return pure21(unit);
                }
                ;
                if (v1.tree instanceof Just) {
                  var updated = {
                    treeType: v.value0,
                    json: v1.tree.value0.json,
                    svgConfig: v1.tree.value0.svgConfig,
                    treeLayout: v1.tree.value0.treeLayout,
                    treeLayoutFn: v1.tree.value0.treeLayoutFn
                  };
                  return bind18(liftAff2(drawTree2(updated)("div.svg-container")))(function() {
                    return discard111(modify_6(function(st) {
                      var $91 = {};
                      for (var $92 in st) {
                        if ({}.hasOwnProperty.call(st, $92)) {
                          $91[$92] = st[$92];
                        }
                        ;
                      }
                      ;
                      $91.tree = new Just(updated);
                      return $91;
                    }))(function() {
                      return pure21(unit);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Stories.Trees (line 205, column 5 - line 211, column 18): " + [v1.tree.constructor.name]);
              });
            });
          }
          ;
          throw new Error("Failed pattern match at Stories.Trees (line 169, column 16 - line 211, column 18): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction16 = /* @__PURE__ */ handleAction7(bindHalogenM);
  var _code6 = function(dictStrong) {
    var $100 = _panels5(dictStrong);
    var $101 = prop112($$Proxy.value)(dictStrong);
    return function($102) {
      return $100($101($102));
    };
  };
  var _code15 = /* @__PURE__ */ _code6(strongForget);
  var _blurb3 = function(dictStrong) {
    var $103 = _panels5(dictStrong);
    var $104 = prop26($$Proxy.value)(dictStrong);
    return function($105) {
      return $103($104($105));
    };
  };
  var _blurb13 = /* @__PURE__ */ _blurb3(strongForget);
  var component7 = function(dictMonadAff) {
    var initialState = {
      tree: Nothing.value,
      panels: {
        blurb: Collapsed.value,
        code: Collapsed.value
      },
      snippets: {
        draw: ""
      }
    };
    var controlsRadio = div2([css("flex-1")])([fieldset_2({
      label: text5("Tree orientation"),
      inputId: "radio-vertical",
      helpText: [],
      error: []
    })([div2([css("flex-1")])([radio([css("pr-6")])([name15("tree-layout"), checked2(true), onClick($$const(new SetLayout(Vertical.value)))])([text5("Vertical")]), radio([css("pr-6")])([name15("tree-layout"), onClick($$const(new SetLayout(Horizontal.value)))])([text5("Horizontal")]), radio([css("pr-6")])([name15("tree-layout"), onClick($$const(new SetLayout(Radial.value)))])([text5("Radial")])])]), fieldset_2({
      label: text5("Tree topology"),
      inputId: "radio-vertical",
      helpText: [],
      error: []
    })([div2([css("flex-1")])([radio([css("pr-6")])([name15("tree-type"), checked2(true), onClick($$const(new SetType(TidyTree.value)))])([text5("TidyTree")]), radio([css("pr-6")])([name15("tree-type"), onClick($$const(new SetType(Dendrogram.value)))])([text5("Dendrogram")])])])]);
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-controls")])([controlsRadio]), div2([tailwindClass("story-panel-about")])([field_2({
        label: text5("About"),
        helpText: [],
        error: [],
        inputId: "show-blurb"
      })([toggle([id2("show-blurb"), checked2(toBoolean(view(_blurb13)(state3))), onChange(function(v) {
        return new ToggleCard6(function(dictStrong) {
          return _blurb3(dictStrong);
        });
      })])]), content_(view(_blurb13)(state3))(blurbtext5)]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text5("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked2(toBoolean(view(_code15)(state3))), onChange(function(v) {
        return new ToggleCard6(function(dictStrong) {
          return _code6(dictStrong);
        });
      })])]), content_(view(_code15)(state3))(syntaxHighlightedCode(view(_drawCode23)(state3)))]), div2([tailwindClass("svg-container")])([])]);
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleAction: handleAction16(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize8.value),
        finalize: defaultEval.finalize
      })
    });
  };

  // output/Main/index.js
  var append22 = /* @__PURE__ */ append(semigroupArray);
  var map54 = /* @__PURE__ */ map(functorArray);
  var slot_2 = /* @__PURE__ */ slot_();
  var slot_1 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "index";
    }
  })(ordUnit);
  var slot_22 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "circles";
    }
  })(ordUnit);
  var slot_3 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "gup";
    }
  })(ordUnit);
  var slot_4 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "trees";
    }
  })(ordUnit);
  var slot_5 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "metatree";
    }
  })(ordUnit);
  var slot_6 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "printtree";
    }
  })(ordUnit);
  var slot_7 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "lesmis";
    }
  })(ordUnit);
  var slot_8 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "spago";
    }
  })(ordUnit);
  var modify_5 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var None3 = /* @__PURE__ */ function() {
    function None4() {
    }
    ;
    None4.value = new None4();
    return None4;
  }();
  var ExampleCircles = /* @__PURE__ */ function() {
    function ExampleCircles2() {
    }
    ;
    ExampleCircles2.value = new ExampleCircles2();
    return ExampleCircles2;
  }();
  var ExampleGUP = /* @__PURE__ */ function() {
    function ExampleGUP2() {
    }
    ;
    ExampleGUP2.value = new ExampleGUP2();
    return ExampleGUP2;
  }();
  var ExampleTrees = /* @__PURE__ */ function() {
    function ExampleTrees2() {
    }
    ;
    ExampleTrees2.value = new ExampleTrees2();
    return ExampleTrees2;
  }();
  var ExampleLesMis = /* @__PURE__ */ function() {
    function ExampleLesMis2() {
    }
    ;
    ExampleLesMis2.value = new ExampleLesMis2();
    return ExampleLesMis2;
  }();
  var ExampleMetaTree = /* @__PURE__ */ function() {
    function ExampleMetaTree2() {
    }
    ;
    ExampleMetaTree2.value = new ExampleMetaTree2();
    return ExampleMetaTree2;
  }();
  var ExamplePrinter = /* @__PURE__ */ function() {
    function ExamplePrinter2() {
    }
    ;
    ExamplePrinter2.value = new ExamplePrinter2();
    return ExamplePrinter2;
  }();
  var ExampleSpago = /* @__PURE__ */ function() {
    function ExampleSpago2() {
    }
    ;
    ExampleSpago2.value = new ExampleSpago2();
    return ExampleSpago2;
  }();
  var Initialize9 = /* @__PURE__ */ function() {
    function Initialize10() {
    }
    ;
    Initialize10.value = new Initialize10();
    return Initialize10;
  }();
  var Example = /* @__PURE__ */ function() {
    function Example2(value0) {
      this.value0 = value0;
    }
    ;
    Example2.create = function(value0) {
      return new Example2(value0);
    };
    return Example2;
  }();
  var eqExampleType = {
    eq: function(x10) {
      return function(y8) {
        if (x10 instanceof None3 && y8 instanceof None3) {
          return true;
        }
        ;
        if (x10 instanceof ExampleCircles && y8 instanceof ExampleCircles) {
          return true;
        }
        ;
        if (x10 instanceof ExampleGUP && y8 instanceof ExampleGUP) {
          return true;
        }
        ;
        if (x10 instanceof ExampleTrees && y8 instanceof ExampleTrees) {
          return true;
        }
        ;
        if (x10 instanceof ExampleLesMis && y8 instanceof ExampleLesMis) {
          return true;
        }
        ;
        if (x10 instanceof ExampleMetaTree && y8 instanceof ExampleMetaTree) {
          return true;
        }
        ;
        if (x10 instanceof ExamplePrinter && y8 instanceof ExamplePrinter) {
          return true;
        }
        ;
        if (x10 instanceof ExampleSpago && y8 instanceof ExampleSpago) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq7 = /* @__PURE__ */ eq(eqExampleType);
  var showExampleType = {
    show: function(v) {
      if (v instanceof None3) {
        return "No example selected";
      }
      ;
      if (v instanceof ExampleCircles) {
        return "Three Little Circles";
      }
      ;
      if (v instanceof ExampleGUP) {
        return "General Update Pattern";
      }
      ;
      if (v instanceof ExampleTrees) {
        return "Trees";
      }
      ;
      if (v instanceof ExampleLesMis) {
        return "LesMis";
      }
      ;
      if (v instanceof ExampleMetaTree) {
        return "MetaTree";
      }
      ;
      if (v instanceof ExamplePrinter) {
        return "Printer";
      }
      ;
      if (v instanceof ExampleSpago) {
        return "Spago";
      }
      ;
      throw new Error("Failed pattern match at Main (line 65, column 10 - line 73, column 31): " + [v.constructor.name]);
    }
  };
  var show21 = /* @__PURE__ */ show(showExampleType);
  var _trees = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var _spago = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var _printtree = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var _metatree = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var _lesmis = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var _index = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var _gup = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var _circles = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var parent2 = function(dictMonadAff) {
    var component8 = component5(dictMonadAff);
    var component1 = component6(dictMonadAff);
    var component22 = component(dictMonadAff);
    var component32 = component7(dictMonadAff);
    var component42 = component3(dictMonadAff);
    var component52 = component4(dictMonadAff);
    var component62 = component2(dictMonadAff);
    var renderExampleNav = function(current) {
      return function(example) {
        return li([class_("mb-3")])([a([classes(append22(linkClasses)(function() {
          var $53 = eq7(current)(example);
          if ($53) {
            return ["font-medium"];
          }
          ;
          return [];
        }())), onClick($$const(new Example(example)))])([text5(show21(example))])]);
      };
    };
    var renderNavGroup = function(currentExample) {
      return div2([class_("text-base overflow-y-auto")])([caption_2([text5("Simple examples")]), ul([class_("list-reset")])(map54(renderExampleNav(currentExample))([ExampleCircles.value, ExampleGUP.value, ExampleTrees.value, ExampleLesMis.value])), caption_2([text5("Alternate interpreters")]), ul([class_("list-reset")])(map54(renderExampleNav(currentExample))([ExampleMetaTree.value, ExamplePrinter.value])), caption_2([text5("Halogen Application")]), ul([class_("list-reset")])(map54(renderExampleNav(currentExample))([ExampleSpago.value]))]);
    };
    var renderSidebar = function(currentExample) {
      return backdrop([tailwindClass("story-sidebar")])([div2([class_("flex-1 p-2 overflow-y-auto")])([img([class_("w-24 mb-8 p-2 bg-white"), src9("PSD3-logo.png")]), nav([class_("text-base overflow-y-auto")])([renderNavGroup(currentExample)])])]);
    };
    var renderExample = function(v) {
      if (v instanceof None3) {
        return slot_1(_index)(unit)(component8)(unit);
      }
      ;
      if (v instanceof ExampleCircles) {
        return slot_22(_circles)(unit)(component1)(unit);
      }
      ;
      if (v instanceof ExampleGUP) {
        return slot_3(_gup)(unit)(component22)(Paused.value);
      }
      ;
      if (v instanceof ExampleTrees) {
        return slot_4(_trees)(unit)(component32)(unit);
      }
      ;
      if (v instanceof ExampleMetaTree) {
        return slot_5(_metatree)(unit)(component42)(unit);
      }
      ;
      if (v instanceof ExamplePrinter) {
        return slot_6(_printtree)(unit)(component52)(unit);
      }
      ;
      if (v instanceof ExampleLesMis) {
        return slot_7(_lesmis)(unit)(component62)(unit);
      }
      ;
      if (v instanceof ExampleSpago) {
        return slot_8(_spago)(unit)(component8)(unit);
      }
      ;
      throw new Error("Failed pattern match at Main (line 149, column 5 - line 159, column 71): " + [v.constructor.name]);
    };
    var render3 = function(currentExample) {
      return body_([div2([tailwindClass("app-container")])([renderSidebar(currentExample), renderExample(currentExample)])]);
    };
    var initialState = function(v) {
      return ExampleSpago.value;
    };
    var handleAction8 = function(v) {
      if (v instanceof Initialize9) {
        return modify_5(function(v1) {
          return None3.value;
        });
      }
      ;
      if (v instanceof Example) {
        return modify_5(function(v1) {
          return v.value0;
        });
      }
      ;
      throw new Error("Failed pattern match at Main (line 164, column 18 - line 166, column 39): " + [v.constructor.name]);
    };
    return mkComponent({
      initialState,
      render: render3,
      "eval": mkEval({
        handleAction: handleAction8,
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize9.value),
        finalize: defaultEval.finalize
      })
    });
  };
  var parent1 = /* @__PURE__ */ parent2(monadAffAff);
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(parent1)(unit)(body2);
  }));

  // <stdin>
  main2();
})();

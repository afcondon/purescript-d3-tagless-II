(() => {
  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
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
    var map18 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map18(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map18 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map18($$const(x))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map18 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map18($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure13 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure13(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure13 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure13(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure13 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply2(pure13(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
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
    return function(eq3) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq3 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
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

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
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
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
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
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Data.Monoid/index.js
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
    var eq3 = eq(dictEq);
    return function(dictEq1) {
      var eq13 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq3(x.value0)(y.value0) && eq13(x.value1)(y.value1);
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
        compare: function(x) {
          return function(y) {
            var v = compare2(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x.value1)(y.value1);
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
    var eq3 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq3(x.value0)(y.value0);
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
    var bind6 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind6(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind6 = bind(dictMonad.Bind1());
    var pure9 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind6(f)(function(f$prime) {
          return bind6(a2)(function(a$prime) {
            return pure9(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left(error4))();
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
    function Supervisor(util) {
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
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
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
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step4 = aff;
      var fail2 = null;
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
                fail2 = util.left(e);
                step4 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step4)) {
                status = RETURN;
                fail2 = step4;
                step4 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step4 = util.fromRight(step4);
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
                    step4 = util.right(step4._1);
                  } else {
                    status = STEP_BIND;
                    step4 = step4._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step4 = runSync(util.left, util.right, step4._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step4 = runAsync(util.left, step4._1, function(result2) {
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
                  fail2 = util.left(step4._1);
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
                  tmp = Fiber(util, supervisor, step4._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step4._1) {
                    tmp.run();
                  }
                  step4 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step4 = sequential3(util, supervisor, step4._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step4 = interrupt || fail2 || step4;
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
                    } else if (fail2) {
                      status = CONTINUE;
                      step4 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  // We cannot resume from an unmasked interrupt or exception.
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step4 = util.fromRight(step4);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step4);
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
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step4 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step4 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step4 = attempt._1.completed(util.fromRight(step4))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                    status = CONTINUE;
                    step4 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step4 = attempt._1;
                    fail2 = attempt._2;
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
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step4) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step4);
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
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step4 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
                }
                status = RETURN;
                step4 = null;
                fail2 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step4 = null;
                fail2 = null;
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
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step4 = par2;
        var head3 = null;
        var tail = null;
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
              if (head3 === null) {
                break loop;
              }
              step4 = head3._2;
              if (tail === null) {
                head3 = null;
              } else {
                head3 = tail._1;
                tail = tail._2;
              }
              break;
            case MAP:
              step4 = step4._2;
              break;
            case APPLY:
            case ALT:
              if (head3) {
                tail = new Aff2(CONS, head3, tail);
              }
              head3 = step4;
              step4 = step4._1;
              break;
          }
        }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head3, tail) {
        var fail2, step4, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step4 = null;
        } else {
          step4 = result;
          fail2 = null;
        }
        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head3 === null) {
            cb(fail2 || step4)();
            return;
          }
          if (head3._3 !== EMPTY) {
            return;
          }
          switch (head3.tag) {
            case MAP:
              if (fail2 === null) {
                head3._3 = util.right(head3._1(util.fromRight(step4)));
                step4 = head3._3;
              } else {
                head3._3 = fail2;
              }
              break;
            case APPLY:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (fail2) {
                head3._3 = fail2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, fail2 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(fail2, null, null);
                    } else {
                      join3(fail2, tail._1, tail._2);
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
                step4 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head3._3 = step4;
              }
              break;
            case ALT:
              lhs = head3._1._3;
              rhs = head3._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail2 = step4 === lhs ? rhs : lhs;
                step4 = null;
                head3._3 = fail2;
              } else {
                head3._3 = step4;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, step4 === lhs ? head3._2 : head3._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(step4, null, null);
                    } else {
                      join3(step4, tail._1, tail._2);
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
          if (tail === null) {
            head3 = null;
          } else {
            head3 = tail._1;
            tail = tail._2;
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
        var head3 = null;
        var tail = null;
        var tmp, fid;
        loop: while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step4.tag) {
                case MAP:
                  if (head3) {
                    tail = new Aff2(CONS, head3, tail);
                  }
                  head3 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                  step4 = step4._2;
                  break;
                case APPLY:
                  if (head3) {
                    tail = new Aff2(CONS, head3, tail);
                  }
                  head3 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                  step4 = step4._1;
                  break;
                case ALT:
                  if (head3) {
                    tail = new Aff2(CONS, head3, tail);
                  }
                  head3 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                  step4 = step4._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step4;
                  step4 = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
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
              if (head3 === null) {
                break loop;
              }
              if (head3._1 === EMPTY) {
                head3._1 = step4;
                status = CONTINUE;
                step4 = head3._2;
                head3._2 = EMPTY;
              } else {
                head3._2 = step4;
                step4 = head3;
                if (tail === null) {
                  head3 = null;
                } else {
                  head3 = tail._1;
                  tail = tail._2;
                }
              }
          }
        }
        root = step4;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
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
        var newKills = kill2(error4, root, cb2);
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
    function sequential3(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
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
        return Aff.Bind(aff, function(value12) {
          return Aff.Pure(f(value12));
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
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
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
    var map18 = map(Monad0.Bind1().Apply0().Functor0());
    var pure9 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map18(Right.create)(a2))(function($52) {
        return pure9(Left.create($52));
      });
    };
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
        return function __do2() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!(function __do3() {
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

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
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

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure9 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure9(unit));
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
      var append6 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append6(f(x))(acc);
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

  // output/Control.Parallel/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
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
      return parTraverse_1(dictFoldable)(identity4);
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
  var $runtime_lazy2 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x) {
    return x;
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
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
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
      return map4(effectCanceler)(v.join(k));
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
          return map4(effectCanceler)(v.kill(e, k));
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

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
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
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map5 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map5(toMaybe);
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
        return function(target7) {
          return function() {
            return target7.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target7) {
          return function() {
            return target7.removeEventListener(type, listener, useCapture);
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
  var map6 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = /* @__PURE__ */ (function() {
    var $2 = map6((function() {
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
  function _read(nothing, just, value12) {
    var tag = Object.prototype.toString.call(value12);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value12);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

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
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom1);
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

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }
  function location(window2) {
    return function() {
      return window2.location;
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
  var map7 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2((function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    })())(document))(windowImpl)))(function(mel) {
      return pure3(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map7(toEventTarget)(windowImpl)();
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
    var empty7 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty7);
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
  var NonEmptyList = function(x) {
    return x;
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
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
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
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
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
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max6.key, max6.value, m.value3), ctx))(m.value0)));
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
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max6.key, max6.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max6.key, max6.value, m.value6), ctx))(m.value3)));
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
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty2;
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
  var replicateFill = function(count) {
    return function(value12) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value12);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value12) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value12;
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

  // output/Data.Array/index.js
  var map12 = /* @__PURE__ */ map(functorMaybe);
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var index2 = /* @__PURE__ */ (function() {
    return indexImpl(Just.create)(Nothing.value);
  })();
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
          return fromJust2(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };

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
  var map8 = /* @__PURE__ */ map(functorArray);
  var map13 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ (function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
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
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map8(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map8(map13(go2))(v2.value3));
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
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
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
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy3 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
  function typeOf(value12) {
    return typeof value12;
  }
  var isArray = Array.isArray || function(value12) {
    return Object.prototype.toString.call(value12) === "[object Array]";
  };

  // output/Data.List.NonEmpty/index.js
  var singleton4 = /* @__PURE__ */ (function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  })();
  var cons3 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var singleton5 = function(c) {
    return c;
  };
  var length5 = function(s) {
    return s.length;
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

  // output/Foreign.Object/foreign.js
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
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
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
  var $runtime_lazy4 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
              return function __do2() {
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
  var HTML = function(x) {
    return x;
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
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name15, props, children2);
        };
      };
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
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
    var pure9 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure9(func.value0),
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
                return goLeft(dictApplicative)(fStack)(cons3(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
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
    var apply2 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply2(fStack.value0.func)(gVal);
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
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure9 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure9(v.value1.value0.value0));
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
        return go2(new Tuple(Nil.value, singleton4(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity5);
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
  var snoc2 = function(v) {
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
        return new CatCons(v.value0, snoc2(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr3 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl2 = function($copy_v) {
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
                return foldl2(function(x) {
                  return function(i2) {
                    return i2(x);
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
  var append2 = link;
  var semigroupCatList = {
    append: append2
  };
  var snoc3 = function(cat) {
    return function(a2) {
      return append2(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy5 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var append3 = /* @__PURE__ */ append(semigroupCatList);
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
          return new Free(v22.value0, append3(v22.value1)(r));
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
        return new Free(v.value0, snoc3(v.value1)(k));
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
    var map18 = map(Monad0.Bind1().Apply0().Functor0());
    var pure13 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map18(Done.create)(pure13(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map18(function($199) {
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
  var bind3 = /* @__PURE__ */ bind(bindEffect);
  var append4 = /* @__PURE__ */ append(semigroupArray);
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
  var create3 = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_2(function(v) {
            return append4(v)([k]);
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
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
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
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
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
  var HalogenM = function(x) {
    return x;
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
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ (function() {
    function Initialize3(value0) {
      this.value0 = value0;
    }
    ;
    Initialize3.create = function(value0) {
      return new Initialize3(value0);
    };
    return Initialize3;
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
  var $runtime_lazy6 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
  var map9 = /* @__PURE__ */ map(functorHalogenM);
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
          var $45 = map9(maybe(v.value1(unit))(g));
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
  var section = /* @__PURE__ */ element2("section");
  var span3 = /* @__PURE__ */ element2("span");
  var ul = /* @__PURE__ */ element2("ul");
  var ul_ = /* @__PURE__ */ ul([]);
  var div2 = /* @__PURE__ */ element2("div");
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
            return function(component6) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot22(label5)(p2)(component6)(input3)($$const(Nothing.value))));
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
            return function(component6) {
              return function(input3) {
                return function(outputQuery) {
                  return widget(new ComponentSlot(componentSlot22(label5)(p2)(component6)(input3)(function($11) {
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
  var initDriverState = function(component6) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
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
              component: component6,
              state: component6.initialState(input3),
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
  var map11 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var map14 = /* @__PURE__ */ map(functorAff);
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
      return function __do2() {
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
  var evalQ = function(render6) {
    return function(ref2) {
      return function(q2) {
        return bind12(liftEffect4(read(ref2)))(function(v) {
          return evalM(render6)(ref2)(v["component"]["eval"](new Query(map11(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render6) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind12(liftEffect4(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel2(bind12(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render6)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map14(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
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
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render6(v2.lifecycleHandlers)(ref2)))(function() {
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
                  return handleAff(evalF(render6)(ref2)(new Action(act)));
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
                var $118 = evalM(render6)(ref2);
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
                    return bind12(fork3($$finally(liftEffect4(function __do2() {
                      modify_2($$delete2(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render6)(ref2)(v1.value0))))(function(fiber) {
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
  var evalF = function(render6) {
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
            return evalM(render6)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
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
  var map15 = /* @__PURE__ */ map(functorEffect);
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
    return function __do2() {
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
    return function __do2() {
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
    return function(component6) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render6)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_2(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect5(function __do2() {
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
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped6(unDriverStateX((function() {
                    var $62 = render6(lchs);
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
                  return function __do2() {
                    var childrenIn = map15(slot3.pop)(read(childrenInRef))();
                    var $$var2 = (function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)((function() {
                              var $64 = maybe(pure12(unit))(handler3);
                              return function($65) {
                                return $64(slot3.output($65));
                              };
                            })())();
                            return handleAff(evalM(render6)(st.selfRef)(st["component"]["eval"](new Receive(slot3.input, unit))))();
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
                    var isDuplicate = map15(function($68) {
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
        var render6 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map15(isNothing)(read(v.pendingHandlers))();
              when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty3)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = (function() {
                var $69 = queueOrRun(v.pendingHandlers);
                var $70 = evalF(render6)(v.selfRef);
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
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do3() {
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
                  children: children2
                };
              }))();
              return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do3() {
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
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render6)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_2(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do3() {
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
                return evalQ(render6)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
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
            return handleLifecycle(lchs)(function __do2() {
              var sio = create3();
              var dsx = bindFlipped6(read)(runComponent(lchs)((function() {
                var $77 = notify(sio.listener);
                return function($78) {
                  return liftEffect5($77($78));
                };
              })())(i2)(component6))();
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
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
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
  var map16 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ (function() {
    var $6 = map16(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  })();
  var nextSibling = /* @__PURE__ */ (function() {
    var $15 = map16(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  })();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy7 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
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
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map17 = /* @__PURE__ */ map(functorEffect);
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
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document2) {
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
          var patch = $lazy_patch(91);
          var render6 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render6;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document2
        };
      };
    };
  };
  var renderSpec = function(document2) {
    return function(container) {
      var render6 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document2);
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
                return function __do2() {
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
        render: render6,
        renderChild: identity6,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component6) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map17(toDocument)(bindFlipped7(document)(windowImpl))))(function(document2) {
          return runUI(renderSpec(document2)(element3))(component6)(i2);
        });
      };
    };
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
    eq: function(x) {
      return function(y) {
        if (x instanceof BasicChart && y instanceof BasicChart) {
          return true;
        }
        ;
        if (x instanceof AdvancedLayout && y instanceof AdvancedLayout) {
          return true;
        }
        ;
        if (x instanceof Interactive2 && y instanceof Interactive2) {
          return true;
        }
        ;
        if (x instanceof Interpreter && y instanceof Interpreter) {
          return true;
        }
        ;
        if (x instanceof Application && y instanceof Application) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/V2.Data.Examples/index.js
  var eq12 = /* @__PURE__ */ eq(eqCategory);
  var allExamples = /* @__PURE__ */ (function() {
    return [{
      id: "line-chart",
      title: "Line Chart",
      description: "Basic line chart with linear scales, axes, and path generation",
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
      difficulty: Advanced.value,
      category: Application.value,
      tags: ["force", "network", "interactive", "real-world"],
      thumbnail: "assets/thumbnails/spago.svg",
      hasInteractivity: true,
      hasComparison: false
    }];
  })();
  var getExample = function(id2) {
    return find2(function(ex) {
      return ex.id === id2;
    })(allExamples);
  };
  var getExamplesByCategory = function(cat) {
    return filter2(function(ex) {
      return eq12(ex.category)(cat);
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

  // output/Data.String.CodePoints/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
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
  var fromCharCode2 = /* @__PURE__ */ (function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton5($75($76));
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
  var singleton6 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons4(v1);
      if (v2 instanceof Just) {
        return singleton6(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
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
    if (v instanceof NotFound) {
      return "#/not-found";
    }
    ;
    throw new Error("Failed pattern match at V2.Router (line 28, column 1 - line 28, column 31): " + [v.constructor.name]);
  };
  var parseRoute = function(hash2) {
    var path = (function() {
      var $12 = length5(hash2) > 0 && take3(1)(hash2) === "#";
      if ($12) {
        return drop4(1)(hash2);
      }
      ;
      return hash2;
    })();
    var segments = split("/")(path);
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
      throw new Error("Failed pattern match at V2.Router (line 22, column 23 - line 24, column 25): " + [v1.constructor.name]);
    }
    ;
    return NotFound.value;
  };

  // output/V2.Components.Navigation/index.js
  var append5 = /* @__PURE__ */ append(semigroupArray);
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
      return false;
    };
  };
  var navLink = function(route) {
    return function(label5) {
      return function(currentRoute) {
        return li([classes(["navigation__item"])])([a([href4(routeToHash(route)), classes(append5(["navigation__link"])((function() {
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
    return nav([classes(["navigation"])])([div2([classes(["navigation__container"])])([a([href4(routeToHash(Home.value)), classes(["navigation__brand"])])([img([src9("../v1/PSD3-logo.png"), alt5("PureScript Tagless D3"), classes(["navigation__logo"])]), span3([classes(["navigation__brand-text"])])([text5("PureScript Tagless D3")])]), ul([classes(["navigation__links"])])([navLink(Home.value)("Home")(currentRoute), navLink(Gallery.value)("Examples")(currentRoute), li([classes(["navigation__item"])])([a([href4("https://github.com/afcondon/purescript-d3-tagless"), target5("_blank"), rel4("noopener noreferrer"), classes(["navigation__link"])])([text5("GitHub")])])])])]);
  };
  var component2 = /* @__PURE__ */ mkComponent({
    initialState: /* @__PURE__ */ identity(categoryFn),
    render: render2,
    "eval": /* @__PURE__ */ mkEval(defaultEval)
  });

  // output/V2.Pages.ExampleDetail/index.js
  var render3 = function(state3) {
    var v = getExample(state3.exampleId);
    if (v instanceof Nothing) {
      return div2([classes(["example-detail"])])([div2([classes(["example-detail__not-found"])])([h1_([text5("Example Not Found")]), p_([text5("Could not find example with ID: " + state3.exampleId)]), a([href4(routeToHash(Gallery.value)), classes(["example-detail__back-link"])])([text5("\u2190 Back to Gallery")])])]);
    }
    ;
    if (v instanceof Just) {
      return div2([classes(["example-detail"])])([div2([classes(["example-detail__header"])])([a([href4(routeToHash(Gallery.value)), classes(["example-detail__back-link"])])([text5("\u2190 Back to Gallery")]), h1([classes(["example-detail__title"])])([text5(v.value0.title)]), p([classes(["example-detail__description"])])([text5(v.value0.description)])]), div2([classes(["example-detail__content"])])([div2([classes(["example-detail__placeholder"])])([h2_([text5("Coming Soon")]), p_([text5("This is a placeholder for the full example detail view.")]), p_([text5("In Phase 2, this will include:")]), ul_([li_([text5("Split-pane layout with code and visualization")]), li_([text5("Syntax-highlighted code")]), li_([text5("Live, interactive visualization")]), li_([text5("D3 JavaScript comparison")]), li_([text5("Export and share options")])]), p_([text5("For now, please visit "), a([href4("../v1/"), target5("_blank")])([text5("V1")]), text5(" to see the working examples.")])])])]);
    }
    ;
    throw new Error("Failed pattern match at V2.Pages.ExampleDetail (line 33, column 3 - line 94, column 10): " + [v.constructor.name]);
  };
  var component3 = /* @__PURE__ */ mkComponent({
    initialState: function(exampleId) {
      return {
        exampleId
      };
    },
    render: render3,
    "eval": /* @__PURE__ */ mkEval(defaultEval)
  });

  // output/V2.Pages.Home/index.js
  var feature = function(emoji) {
    return function(title4) {
      return function(description) {
        return div2([classes(["home__feature"])])([div2([classes(["home__feature-icon"])])([text5(emoji)]), h3([classes(["home__feature-title"])])([text5(title4)]), p([classes(["home__feature-description"])])([text5(description)])]);
      };
    };
  };
  var render4 = function(v) {
    return div2([classes(["home"])])([section([classes(["home__hero"])])([div2([classes(["home__hero-content"])])([h1([classes(["home__title"])])([text5("PureScript Tagless D3")]), p([classes(["home__subtitle"])])([text5("Type-safe, composable data visualization")]), p([classes(["home__description"])])([text5("A PureScript library implementing a Finally Tagless embedded DSL for building interactive data visualizations with D3.js")]), div2([classes(["home__cta"])])([a([href4(routeToHash(Gallery.value)), classes(["home__button", "home__button--primary"])])([text5("View Examples")]), a([href4("https://github.com/afcondon/purescript-d3-tagless"), target5("_blank"), rel4("noopener noreferrer"), classes(["home__button", "home__button--secondary"])])([text5("View on GitHub")])])])]), section([classes(["home__features"])])([h2([classes(["home__section-title"])])([text5("Key Features")]), div2([classes(["home__features-grid"])])([feature("\u{1F3A8}")("Type-Safe")("Strong type safety with PureScript's type system prevents runtime errors"), feature("\u{1F527}")("Composable")("Build complex visualizations from simple, reusable components"), feature("\u{1F3AD}")("Multiple Interpreters")("Finally Tagless pattern enables different interpretations of the same DSL"), feature("\u{1F4CA}")("D3-Powered")("Leverages D3.js for battle-tested visualization rendering"), feature("\u26A1")("Interactive")("Support for drag, zoom, and other interactive behaviors"), feature("\u{1F4DA}")("Well-Documented")("Comprehensive examples with side-by-side D3 JavaScript comparisons")])]), section([classes(["home__explanation"])])([h2([classes(["home__section-title"])])([text5("What is Finally Tagless?")]), p([classes(["home__text"])])([text5("The Finally Tagless pattern separates the description of a computation from its interpretation. In this library, you describe visualizations using a domain-specific language (DSL), and the same description can be:")]), ul([classes(["home__list"])])([li_([text5("Rendered as an actual D3 visualization")]), li_([text5("Visualized as a tree showing the DSL structure (MetaTree interpreter)")]), li_([text5("Converted to documentation or code (String interpreter)")])]), p([classes(["home__text"])])([text5("This powerful abstraction makes code more testable, reusable, and easier to reason about.")])])]);
  };
  var component4 = /* @__PURE__ */ mkComponent({
    initialState: function(v) {
      return unit;
    },
    render: render4,
    "eval": /* @__PURE__ */ mkEval(defaultEval)
  });

  // output/V2.Main/index.js
  var bind5 = /* @__PURE__ */ bind(bindHalogenM);
  var liftEffect7 = /* @__PURE__ */ liftEffect(/* @__PURE__ */ monadEffectHalogenM(monadEffectAff));
  var bind15 = /* @__PURE__ */ bind(bindEffect);
  var discard5 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var modify_4 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var slot_2 = /* @__PURE__ */ slot_();
  var slot_1 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "home";
    }
  })(ordUnit);
  var slot2 = /* @__PURE__ */ slot()({
    reflectSymbol: function() {
      return "gallery";
    }
  })(ordUnit);
  var slot_22 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "exampleDetail";
    }
  })(ordUnit);
  var slot_3 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "navigation";
    }
  })(ordUnit);
  var Initialize2 = /* @__PURE__ */ (function() {
    function Initialize3() {
    }
    ;
    Initialize3.value = new Initialize3();
    return Initialize3;
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
  var handleAction2 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof Initialize2) {
        $tco_done = true;
        return bind5(liftEffect7(bind15(bind15(windowImpl)(location))(hash)))(function(currentHash) {
          var route = parseRoute(currentHash);
          return discard5(modify_4(function(v1) {
            var $30 = {};
            for (var $31 in v1) {
              if ({}.hasOwnProperty.call(v1, $31)) {
                $30[$31] = v1[$31];
              }
              ;
            }
            ;
            $30.currentRoute = route;
            return $30;
          }))(function() {
            return liftEffect7(function __do2() {
              var win = windowImpl();
              var target7 = toEventTarget(win);
              var listener = eventListener(function(v1) {
                return function __do3() {
                  var newHash = bind15(bind15(windowImpl)(location))(hash)();
                  return unit;
                };
              })();
              return addEventListener("hashchange")(listener)(false)(target7)();
            });
          });
        });
      }
      ;
      if (v instanceof Navigate) {
        $tco_done = true;
        return modify_4(function(v1) {
          var $33 = {};
          for (var $34 in v1) {
            if ({}.hasOwnProperty.call(v1, $34)) {
              $33[$34] = v1[$34];
            }
            ;
          }
          ;
          $33.currentRoute = v.value0;
          return $33;
        });
      }
      ;
      if (v instanceof HandleGalleryOutput) {
        $copy_v = new Navigate(new Example(v.value0));
        return;
      }
      ;
      throw new Error("Failed pattern match at V2.Main (line 98, column 16 - line 121, column 48): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var _navigation = /* @__PURE__ */ (function() {
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
      return slot_1(_home)(unit)(component4)(unit);
    }
    ;
    if (route instanceof Gallery) {
      return slot2(_gallery)(unit)(component)(unit)(HandleGalleryOutput.create);
    }
    ;
    if (route instanceof Example) {
      return slot_22(_exampleDetail)(unit)(component3)(route.value0);
    }
    ;
    if (route instanceof NotFound) {
      return div2([classes(["not-found"])])([h1_([text5("404 - Page Not Found")]), p_([text5("The page you're looking for doesn't exist.")]), a([href4(routeToHash(Home.value))])([text5("Go Home")])]);
    }
    ;
    throw new Error("Failed pattern match at V2.Main (line 77, column 20 - line 95, column 8): " + [route.constructor.name]);
  };
  var render5 = function(state3) {
    return div2([classes(["app"])])([slot_3(_navigation)(unit)(component2)(state3.currentRoute), main([classes(["app__main"])])([renderPage(state3.currentRoute)])]);
  };
  var component5 = /* @__PURE__ */ (function() {
    return mkComponent({
      initialState: function(v) {
        return {
          currentRoute: Home.value
        };
      },
      render: render5,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction2,
        initialize: new Just(Initialize2.value)
      })
    });
  })();
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(component5)(unit)(body2);
  }));

  // <stdin>
  main2();
})();

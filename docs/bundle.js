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
        return function(x15) {
          return f(g(x15));
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
    identity: function(x15) {
      return x15;
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
      return function(x15) {
        return function(y10) {
          return f(g(x15))(g(y10));
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
    var map116 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map116(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map116 = map(dictFunctor);
    return function(f) {
      return function(x15) {
        return map116($$const(x15))(f);
      };
    };
  };
  var voidRight = function(dictFunctor) {
    var map116 = map(dictFunctor);
    return function(x15) {
      return map116($$const(x15));
    };
  };
  var functorArray = {
    map: arrayMap
  };
  var flap = function(dictFunctor) {
    var map116 = map(dictFunctor);
    return function(ff2) {
      return function(x15) {
        return map116(function(f) {
          return f(x15);
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
    var map54 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map54($$const(identity2))(a2))(b2);
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
    pure: function(x15) {
      return [x15];
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
        return function(x15) {
          return function(y10) {
            return x15 < y10 ? lt : x15 === y10 ? eq8 : gt;
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
  var unsafeGet = function(label5) {
    return function(rec) {
      return rec[label5];
    };
  };
  var unsafeSet = function(label5) {
    return function(value16) {
      return function(rec) {
        var copy2 = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy2[key] = rec[key];
          }
        }
        copy2[label5] = value16;
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
    return function(x15) {
      return function(y10) {
        return eq2(eq32(x15)(y10))(false);
      };
    };
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
        var empty9 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty9;
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
          return function(x15) {
            return append111(f(x15))(g(x15));
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
    var eq8 = eq(dictEq);
    return function(dictEq1) {
      var eq14 = eq(dictEq1);
      return {
        eq: function(x15) {
          return function(y10) {
            return eq8(x15.value0)(y10.value0) && eq14(x15.value1)(y10.value1);
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
        compare: function(x15) {
          return function(y10) {
            var v = compare2(x15.value0)(y10.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x15.value1)(y10.value1);
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
  var applicativeMaybe = /* @__PURE__ */ (function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  })();
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

  // output/Data.Array/foreign.js
  var range = function(start6) {
    return function(end) {
      var step5 = start6 > end ? -1 : 1;
      var result = new Array(step5 * (end - start6) + 1);
      var i2 = start6, n = 0;
      while (i2 !== end) {
        result[n++] = i2;
        i2 += step5;
      }
      result[n] = i2;
      return result;
    };
  };
  var replicateFill = function(count) {
    return function(value16) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value16);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value16) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value16;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ (function() {
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
  })();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty9) {
    return function(next2) {
      return function(xs) {
        return xs.length === 0 ? empty9({}) : next2(xs[0])(xs.slice(1));
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
  var reverse = function(l) {
    return l.slice().reverse();
  };
  var filter = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var partition = function(f) {
    return function(xs) {
      var yes = [];
      var no = [];
      for (var i2 = 0; i2 < xs.length; i2++) {
        var x15 = xs[i2];
        if (f(x15))
          yes.push(x15);
        else
          no.push(x15);
      }
      return { yes, no };
    };
  };
  var sortByImpl = /* @__PURE__ */ (function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to2) {
      var mid;
      var i2;
      var j;
      var k;
      var x15;
      var y10;
      var c;
      mid = from2 + (to2 - from2 >> 1);
      if (mid - from2 > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to2 - mid > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to2);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to2) {
        x15 = xs2[i2];
        y10 = xs2[j];
        c = fromOrdering(compare2(x15)(y10));
        if (c > 0) {
          xs1[k++] = y10;
          ++j;
        } else {
          xs1[k++] = x15;
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
          if (xs.length < 2) return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  })();
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
  var any = function(p2) {
    return function(xs) {
      var len = xs.length;
      for (var i2 = 0; i2 < len; i2++) {
        if (p2(xs[i2])) return true;
      }
      return false;
    };
  };
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

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
  var hush = /* @__PURE__ */ (function() {
    return either($$const(Nothing.value))(Just.create);
  })();
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

  // output/Data.Identity/index.js
  var Identity = function(x15) {
    return x15;
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
  var $runtime_lazy = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
  var $runtime_lazy2 = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
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
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy2("applyST", "Control.Monad.ST.Internal", function() {
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
  var map5 = /* @__PURE__ */ map(functorST);
  var not2 = /* @__PURE__ */ not(heytingAlgebraBoolean);
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var Iterator = /* @__PURE__ */ (function() {
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
  })();
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
      return function(array2) {
        return function __do3() {
          var $$break = newSTRef(false)();
          while (map5(not2)(read2($$break))()) {
            (function __do4() {
              var mx = peek(iter)();
              if (mx instanceof Just && p2(mx.value0)) {
                push(mx.value0)(array2)();
                return $$void3(next(iter))();
              }
              ;
              return $$void3(write2(true)($$break))();
            })();
          }
          ;
          return {};
        };
      };
    };
  };
  var iterator = function(f) {
    return map5(Iterator.create(f))(newSTRef(0));
  };
  var iterate = function(iter) {
    return function(f) {
      return function __do3() {
        var $$break = newSTRef(false)();
        while (map5(not2)(read2($$break))()) {
          (function __do4() {
            var mx = next(iter)();
            if (mx instanceof Just) {
              return f(mx.value0)();
            }
            ;
            if (mx instanceof Nothing) {
              return $$void3(write2(true)($$break))();
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

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Bifunctor/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    var bimap1 = bimap(dictBifunctor);
    return function(f) {
      return bimap1(f)(identity4);
    };
  };
  var rmap = function(dictBifunctor) {
    return bimap(dictBifunctor)(identity4);
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
  var First = function(x15) {
    return x15;
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
  var monoidFirst = /* @__PURE__ */ (function() {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupFirst;
      }
    };
  })();

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x15) {
    return x15;
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

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x15) {
    return x15;
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
        return foldr22(function($454) {
          return applySecond3(f($454));
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
                acc: append23(v.acc)(append23(sep)(v1))
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
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x15) {
          return function(acc) {
            return append23(f(x15))(acc);
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
  var any2 = function(dictFoldable) {
    var foldMap22 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap22(monoidDisj(dictHeytingAlgebra)));
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
    return function(apply4) {
      return function(map54) {
        return function(pure21) {
          return function(f) {
            return function(array4) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure21([]);
                  case 1:
                    return map54(array1)(f(array4[bot]));
                  case 2:
                    return apply4(map54(array2)(f(array4[bot])))(f(array4[bot + 1]));
                  case 3:
                    return apply4(apply4(map54(array3)(f(array4[bot])))(f(array4[bot + 1])))(f(array4[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply4(map54(concat2)(go2(bot, pivot)))(go2(pivot, top3));
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

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value16 = b2;
              while (true) {
                var maybe2 = f(value16);
                if (isNothing2(maybe2)) return result;
                var tuple = fromJust6(maybe2);
                result.push(fst2(tuple));
                value16 = snd2(tuple);
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
              var value16 = b2;
              while (true) {
                var tuple = f(value16);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2)) return result;
                value16 = fromJust6(maybe2);
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

  // output/Data.Array/index.js
  var map6 = /* @__PURE__ */ map(functorST);
  var when2 = /* @__PURE__ */ when(applicativeST);
  var $$void4 = /* @__PURE__ */ $$void(functorST);
  var intercalate1 = /* @__PURE__ */ intercalate2(foldableArray);
  var map1 = /* @__PURE__ */ map(functorMaybe);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zip = /* @__PURE__ */ (function() {
    return zipWith(Tuple.create);
  })();
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var uncons = /* @__PURE__ */ (function() {
    return unconsImpl($$const(Nothing.value))(function(x15) {
      return function(xs) {
        return new Just({
          head: x15,
          tail: xs
        });
      };
    });
  })();
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
    return function(x15) {
      return withArray(push(x15))(xs)();
    };
  };
  var singleton2 = function(a2) {
    return [a2];
  };
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var nubByEq = function(eq23) {
    return function(xs) {
      return (function __do3() {
        var arr = newSTArray();
        foreach(xs)(function(x15) {
          return function __do4() {
            var e = map6((function() {
              var $179 = any(function(v) {
                return eq23(v)(x15);
              });
              return function($180) {
                return !$179($180);
              };
            })())(unsafeFreeze(arr))();
            return when2(e)($$void4(push(x15)(arr)))();
          };
        })();
        return unsafeFreeze(arr)();
      })();
    };
  };
  var mapWithIndex = function(f) {
    return function(xs) {
      return zipWith(f)(range(0)(length(xs) - 1 | 0))(xs);
    };
  };
  var intercalate3 = function(dictMonoid) {
    return intercalate1(dictMonoid);
  };
  var index = /* @__PURE__ */ (function() {
    return indexImpl(Just.create)(Nothing.value);
  })();
  var head = function(xs) {
    return index(xs)(0);
  };
  var groupBy = function(op) {
    return function(xs) {
      return (function __do3() {
        var result = newSTArray();
        var iter = iterator(function(v) {
          return index(xs)(v);
        })();
        iterate(iter)(function(x15) {
          return $$void4(function __do4() {
            var sub1 = newSTArray();
            push(x15)(sub1)();
            pushWhile(op(x15))(iter)(sub1)();
            var grp = unsafeFreeze(sub1)();
            return push(grp)(result)();
          });
        })();
        return unsafeFreeze(result)();
      })();
    };
  };
  var fromFoldable = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var findIndex = /* @__PURE__ */ (function() {
    return findIndexImpl(Just.create)(Nothing.value);
  })();
  var find2 = function(f) {
    return function(xs) {
      return map1(unsafeIndex1(xs))(findIndex(f)(xs));
    };
  };
  var elemIndex = function(dictEq) {
    var eq23 = eq(dictEq);
    return function(x15) {
      return findIndex(function(v) {
        return eq23(v)(x15);
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
  var cons2 = function(x15) {
    return function(xs) {
      return append2([x15])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap((function() {
      var $187 = maybe([])(singleton2);
      return function($188) {
        return $187(f($188));
      };
    })());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));

  // output/Data.String.CodeUnits/foreign.js
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton3 = function(c) {
    return c;
  };
  var length2 = function(s) {
    return s.length;
  };
  var take = function(n) {
    return function(s) {
      return s.substr(0, n);
    };
  };
  var drop = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

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

  // output/Foreign.Object/foreign.js
  var empty2 = {};
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
          return function(d5) {
            return fn(a2, b2, c, d5);
          };
        };
      };
    };
  };

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
  var mapWithIndex2 = function(dict) {
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
  var mapWithIndex3 = /* @__PURE__ */ mapWithIndex2(functorWithIndexArray);
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
          return function(x15) {
            return function(acc) {
              return append23(f(i2)(x15))(acc);
            };
          };
        })(mempty2);
      };
    };
  };
  var foldableWithIndexArray = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $291 = foldr8(function(v) {
          return function(y10) {
            return f(v.value0)(v.value1)(y10);
          };
        })(z);
        var $292 = mapWithIndex3(Tuple.create);
        return function($293) {
          return $291($292($293));
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $294 = foldl8(function(y10) {
          return function(v) {
            return f(v.value0)(y10)(v.value1);
          };
        })(z);
        var $295 = mapWithIndex3(Tuple.create);
        return function($296) {
          return $294($295($296));
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

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };

  // output/Foreign.Object/index.js
  var lookup = /* @__PURE__ */ (function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  })();

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton = /* @__PURE__ */ (function() {
    function InputButton2() {
    }
    ;
    InputButton2.value = new InputButton2();
    return InputButton2;
  })();
  var InputCheckbox = /* @__PURE__ */ (function() {
    function InputCheckbox2() {
    }
    ;
    InputCheckbox2.value = new InputCheckbox2();
    return InputCheckbox2;
  })();
  var InputColor = /* @__PURE__ */ (function() {
    function InputColor2() {
    }
    ;
    InputColor2.value = new InputColor2();
    return InputColor2;
  })();
  var InputDate = /* @__PURE__ */ (function() {
    function InputDate2() {
    }
    ;
    InputDate2.value = new InputDate2();
    return InputDate2;
  })();
  var InputDatetimeLocal = /* @__PURE__ */ (function() {
    function InputDatetimeLocal2() {
    }
    ;
    InputDatetimeLocal2.value = new InputDatetimeLocal2();
    return InputDatetimeLocal2;
  })();
  var InputEmail = /* @__PURE__ */ (function() {
    function InputEmail2() {
    }
    ;
    InputEmail2.value = new InputEmail2();
    return InputEmail2;
  })();
  var InputFile = /* @__PURE__ */ (function() {
    function InputFile2() {
    }
    ;
    InputFile2.value = new InputFile2();
    return InputFile2;
  })();
  var InputHidden = /* @__PURE__ */ (function() {
    function InputHidden2() {
    }
    ;
    InputHidden2.value = new InputHidden2();
    return InputHidden2;
  })();
  var InputImage = /* @__PURE__ */ (function() {
    function InputImage2() {
    }
    ;
    InputImage2.value = new InputImage2();
    return InputImage2;
  })();
  var InputMonth = /* @__PURE__ */ (function() {
    function InputMonth2() {
    }
    ;
    InputMonth2.value = new InputMonth2();
    return InputMonth2;
  })();
  var InputNumber = /* @__PURE__ */ (function() {
    function InputNumber2() {
    }
    ;
    InputNumber2.value = new InputNumber2();
    return InputNumber2;
  })();
  var InputPassword = /* @__PURE__ */ (function() {
    function InputPassword2() {
    }
    ;
    InputPassword2.value = new InputPassword2();
    return InputPassword2;
  })();
  var InputRadio = /* @__PURE__ */ (function() {
    function InputRadio2() {
    }
    ;
    InputRadio2.value = new InputRadio2();
    return InputRadio2;
  })();
  var InputRange = /* @__PURE__ */ (function() {
    function InputRange2() {
    }
    ;
    InputRange2.value = new InputRange2();
    return InputRange2;
  })();
  var InputReset = /* @__PURE__ */ (function() {
    function InputReset2() {
    }
    ;
    InputReset2.value = new InputReset2();
    return InputReset2;
  })();
  var InputSearch = /* @__PURE__ */ (function() {
    function InputSearch2() {
    }
    ;
    InputSearch2.value = new InputSearch2();
    return InputSearch2;
  })();
  var InputSubmit = /* @__PURE__ */ (function() {
    function InputSubmit2() {
    }
    ;
    InputSubmit2.value = new InputSubmit2();
    return InputSubmit2;
  })();
  var InputTel = /* @__PURE__ */ (function() {
    function InputTel2() {
    }
    ;
    InputTel2.value = new InputTel2();
    return InputTel2;
  })();
  var InputText = /* @__PURE__ */ (function() {
    function InputText2() {
    }
    ;
    InputText2.value = new InputText2();
    return InputText2;
  })();
  var InputTime = /* @__PURE__ */ (function() {
    function InputTime2() {
    }
    ;
    InputTime2.value = new InputTime2();
    return InputTime2;
  })();
  var InputUrl = /* @__PURE__ */ (function() {
    function InputUrl2() {
    }
    ;
    InputUrl2.value = new InputUrl2();
    return InputUrl2;
  })();
  var InputWeek = /* @__PURE__ */ (function() {
    function InputWeek2() {
    }
    ;
    InputWeek2.value = new InputWeek2();
    return InputWeek2;
  })();
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
  var Any = /* @__PURE__ */ (function() {
    function Any2() {
    }
    ;
    Any2.value = new Any2();
    return Any2;
  })();
  var Step = /* @__PURE__ */ (function() {
    function Step4(value0) {
      this.value0 = value0;
    }
    ;
    Step4.create = function(value0) {
      return new Step4(value0);
    };
    return Step4;
  })();
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

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x15) {
    return x15;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Halogen.VDom.Machine/index.js
  var Step2 = /* @__PURE__ */ (function() {
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
  })();
  var unStep = unsafeCoerce2;
  var step = function(v, a2) {
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
  var map7 = /* @__PURE__ */ map(functorArray);
  var map12 = /* @__PURE__ */ map(functorTuple);
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
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map7(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map7(map12(go2))(v2.value3));
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
  function addEventListener(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

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

  // output/Web.DOM.ParentNode/index.js
  var map8 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map8(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.DOM.Element/index.js
  var toNode = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy3 = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
        var res = step(state3.widget, vdom.value0);
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
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy3("patchText", "Halogen.VDom.DOM", function() {
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
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy3("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
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
          var res = step(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children22 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children22
        };
        return mkStep(new Step2(state3.node, nextState, $lazy_patchElem(172), haltElem));
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
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
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
          var res = step(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children22 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children22,
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
    var node = toNode(el);
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
      length: length(ch1)
    };
    return mkStep(new Step2(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode(el);
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
    return mkStep(new Step2(node, state3, patchElem, haltElem));
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
  function typeOf(value16) {
    return typeof value16;
  }
  function tagOf(value16) {
    return Object.prototype.toString.call(value16).slice(8, -1);
  }
  var isArray = Array.isArray || function(value16) {
    return Object.prototype.toString.call(value16) === "[object Array]";
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
    var map54 = map(Monad0.Bind1().Apply0().Functor0());
    var pure21 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map54(Right.create)(a2))(function($52) {
        return pure21(Left.create($52));
      });
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
  var map9 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x15) {
    return x15;
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
    var map116 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map116(map9(f)));
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
  var fromStringAs = /* @__PURE__ */ (function() {
    return fromStringAsImpl(Just.create)(Nothing.value);
  })();
  var fromString = /* @__PURE__ */ fromStringAs(10);
  var fromNumber = /* @__PURE__ */ (function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  })();
  var unsafeClamp = function(x15) {
    if (!isFiniteImpl(x15)) {
      return 0;
    }
    ;
    if (x15 >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x15 <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x15));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x15.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

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
  var singleton4 = function(dictPlus) {
    var empty9 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty9);
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
  var NonEmptyList = function(x15) {
    return x15;
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
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append23(acc);
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
  var reverse2 = /* @__PURE__ */ (function() {
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
  var $$null3 = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var fromFoldable2 = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
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

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ (function() {
    var $200 = singleton4(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  })();
  var head2 = function(v) {
    return v.value0;
  };
  var cons3 = function(y10) {
    return function(v) {
      return new NonEmpty(y10, new Cons(v.value0, v.value1));
    };
  };

  // output/Foreign/index.js
  var show3 = /* @__PURE__ */ show(showString);
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
      return function(value16) {
        if (tagOf(value16) === tag2) {
          return pure111(unsafeFromForeign(value16));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag2, tagOf(value16)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag2.constructor.name, value16.constructor.name]);
      };
    };
  };
  var readBoolean = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("Boolean");
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener2(type2) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type2, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener2(type2) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type2, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy4 = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
            return removeEventListener(v1.value0, fst(handler3), el);
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
        var v = lookup("ref")(state3.props);
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
            addEventListener(v2.value0, listener, el);
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

  // output/Web.HTML.Common/index.js
  var ClassName = function(x15) {
    return x15;
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x15) {
    return x15;
  };
  var widget = function($28) {
    return HTML(Widget.create($28));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text = function($29) {
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
  var handler = /* @__PURE__ */ (function() {
    return Handler.create;
  })();
  var element = function(ns) {
    return function(name17) {
      return function(props) {
        return function(children3) {
          return new Elem(ns, name17, props, children3);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Halogen.HTML.Properties/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var prop4 = /* @__PURE__ */ prop2(isPropNumber);
  var src = /* @__PURE__ */ prop22("src");
  var step2 = /* @__PURE__ */ prop2(isPropStepValue)("step");
  var type_ = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var name2 = /* @__PURE__ */ prop22("name");
  var min3 = /* @__PURE__ */ prop4("min");
  var max3 = /* @__PURE__ */ prop4("max");
  var id2 = /* @__PURE__ */ prop22("id");
  var $$for = /* @__PURE__ */ prop22("htmlFor");
  var classes = /* @__PURE__ */ (function() {
    var $32 = prop22("className");
    var $33 = joinWith(" ");
    var $34 = map(functorArray)(unwrap2);
    return function($35) {
      return $32($33($34($35)));
    };
  })();
  var class_ = /* @__PURE__ */ (function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap2($37));
    };
  })();
  var checked = /* @__PURE__ */ prop1("checked");
  var attr2 = /* @__PURE__ */ (function() {
    return attr(Nothing.value);
  })();

  // output/DemoApp.UI.Properties/index.js
  var lmap2 = /* @__PURE__ */ lmap(bifunctorTuple);
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var rmap2 = /* @__PURE__ */ rmap(bifunctorTuple);
  var elem3 = /* @__PURE__ */ elem2(eqString);
  var not3 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean));
  var pure2 = /* @__PURE__ */ pure(applicativeArray);
  var map10 = /* @__PURE__ */ map(functorArray);
  var startsWith = function(str0) {
    return function(str1) {
      return str0 === take(length2(str0))(str1);
    };
  };
  var extract3 = /* @__PURE__ */ (function() {
    var f = function(v) {
      return function(v1) {
        if (v1 instanceof Property && v1.value0 === "className") {
          return lmap2(function(v2) {
            return append12(v2)(split(" ")(v1.value1));
          })(v);
        }
        ;
        return rmap2(function(v2) {
          return append12(v2)([v1]);
        })(v);
      };
    };
    return foldl2(f)(new Tuple([], []));
  })();
  var css = function($41) {
    return class_(ClassName($41));
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
    throw new Error("Failed pattern match at DemoApp.UI.Properties (line 103, column 1 - line 105, column 12): " + [str.constructor.name]);
  };
  var append$prime = function(v) {
    return function(v1) {
      if (v1 === "") {
        return v;
      }
      ;
      return v + ("-" + v1);
    };
  };
  var classifyOverflow = function(str) {
    if (startsWith("x-")(str)) {
      return append$prime("horizontal")(classifyOverflow(drop(2)(str)));
    }
    ;
    if (startsWith("y-")(str)) {
      return append$prime("vertical")(classifyOverflow(drop(2)(str)));
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
    throw new Error("Failed pattern match at DemoApp.UI.Properties (line 116, column 1 - line 118, column 12): " + [str.constructor.name]);
  };
  var classify = function(str) {
    if (startsWith("p")(str) && not3($$null2)(classifySide(drop(1)(str)))) {
      return append$prime("padding")(classifySide(drop(1)(str)));
    }
    ;
    if (startsWith("m")(str) && not3($$null2)(classifySide(drop(1)(str)))) {
      return append$prime("margin")(classifySide(drop(1)(str)));
    }
    ;
    if (startsWith("-m")(str) && not3($$null2)(classifySide(drop(2)(str)))) {
      return append$prime("margin")(classifySide(drop(2)(str)));
    }
    ;
    if (startsWith("min-")(str)) {
      return append$prime("min")(classify(drop(4)(str)));
    }
    ;
    if (startsWith("max-")(str)) {
      return append$prime("max")(classify(drop(4)(str)));
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
    if (startsWith("overflow-")(str) && classifyOverflow(drop(9)(str)) !== drop(9)(str)) {
      return append$prime("overflow")(classifyOverflow(drop(9)(str)));
    }
    ;
    if (otherwise) {
      return str;
    }
    ;
    throw new Error("Failed pattern match at DemoApp.UI.Properties (line 85, column 1 - line 87, column 12): " + [str.constructor.name]);
  };
  var appendIProps = function(ip) {
    return function(ip$prime) {
      var v = extract3(ip);
      var v1 = extract3(ip$prime);
      var classNames = pure2(classes(map10(ClassName)(nubByEq(function(c) {
        return function(c$prime) {
          return classify(c) === classify(c$prime);
        };
      })(append12(v1.value0)(v.value0)))));
      return append12(v.value1)(append12(v1.value1)(classNames));
    };
  };

  // output/Halogen.HTML.Elements/index.js
  var pure3 = /* @__PURE__ */ pure(applicativeMaybe);
  var elementNS = function($15) {
    return element(pure3($15));
  };
  var element2 = /* @__PURE__ */ (function() {
    return element(Nothing.value);
  })();
  var fieldset = /* @__PURE__ */ element2("fieldset");
  var h1 = /* @__PURE__ */ element2("h1");
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var h3 = /* @__PURE__ */ element2("h3");
  var h4 = /* @__PURE__ */ element2("h4");
  var img = function(props) {
    return element2("img")(props)([]);
  };
  var input = function(props) {
    return element2("input")(props)([]);
  };
  var label = /* @__PURE__ */ element2("label");
  var legend = /* @__PURE__ */ element2("legend");
  var li = /* @__PURE__ */ element2("li");
  var nav = /* @__PURE__ */ element2("nav");
  var p = /* @__PURE__ */ element2("p");
  var p_ = /* @__PURE__ */ p([]);
  var pre = /* @__PURE__ */ element2("pre");
  var span2 = /* @__PURE__ */ element2("span");
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

  // output/DemoApp.UI.Backdrop/index.js
  var map11 = /* @__PURE__ */ map(functorArray);
  var append3 = /* @__PURE__ */ append(semigroupArray);
  var backdropClasses = /* @__PURE__ */ map11(ClassName)(["p-6", "flex", "flex-1"]);
  var backdropDefaultClasses = /* @__PURE__ */ append3(backdropClasses)(/* @__PURE__ */ map11(ClassName)(["bg-grey-95"]));
  var backdrop = function(iprops) {
    return function(html2) {
      return div2(appendIProps([classes(backdropDefaultClasses)])(iprops))(html2);
    };
  };
  var backdrop_ = /* @__PURE__ */ backdrop([]);

  // output/DemoApp.UI.Format/index.js
  var map13 = /* @__PURE__ */ map(functorArray);
  var subHeadingClasses = /* @__PURE__ */ map13(ClassName)(["text-xl", "font-medium", "leading-loose", "flex", "items-center", "mb-6"]);
  var subHeading = function(iprops) {
    return function(html2) {
      return h2(appendIProps([classes(subHeadingClasses)])(iprops))(html2);
    };
  };
  var subHeading_ = /* @__PURE__ */ subHeading([]);
  var mutedClasses = /* @__PURE__ */ map13(ClassName)(["text-grey-50"]);
  var linkClasses = /* @__PURE__ */ map13(ClassName)(["text-blue-75", "hover:text-blue-65", "no-underline", "font-medium", "cursor-pointer"]);
  var contentHeadingClasses = /* @__PURE__ */ map13(ClassName)(["mb-6", "text-lg", "font-normal", "leading-loose", "flex", "items-center"]);
  var contentHeading = function(iprops) {
    return h3(appendIProps([classes(contentHeadingClasses)])(iprops));
  };
  var contentHeading_ = /* @__PURE__ */ contentHeading([]);
  var captionClasses = /* @__PURE__ */ map13(ClassName)(["block", "font-light", "mb-6", "text-grey-70", "text-sm", "tracking-wide", "uppercase"]);
  var caption = function(iprops) {
    return h4(appendIProps([classes(captionClasses)])(iprops));
  };
  var caption_ = /* @__PURE__ */ caption([]);

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
    function runSync(left3, right3, eff) {
      try {
        return right3(eff());
      } catch (error6) {
        return left3(error6);
      }
    }
    function runAsync(left3, eff, k) {
      try {
        return eff(k)();
      } catch (error6) {
        k(left3(error6))();
        return nonCanceler2;
      }
    }
    var Scheduler = (function() {
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
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.
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
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.
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
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail3) {
                      status = CONTINUE;
                      step5 = attempt._2(util2.fromLeft(fail3));
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
                      step5 = util2.fromRight(step5);
                    }
                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.
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
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.
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
      var root2 = EMPTY;
      function kill2(error6, par2, cb2) {
        var step5 = par2;
        var head7 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop: while (true) {
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
        loop: while (true) {
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
        loop: while (true) {
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
        root2 = step5;
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
        var newKills = kill2(error6, root2, cb2);
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
        return Aff.Bind(aff, function(value16) {
          return Aff.Pure(f(value16));
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
    return function(right3, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer2 = setDelay(ms, cb(right3()));
          return function() {
            return Aff.Sync(function() {
              return right3(clearDelay(ms, timer2));
            });
          };
        };
      });
    };
  })();
  var _sequential = Aff.Seq;

  // output/Data.Profunctor/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
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
  var rmap3 = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(b2c) {
      return dimap1(identity6)(b2c);
    };
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
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

  // output/Effect.Aff/index.js
  var $runtime_lazy5 = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var pure4 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var map14 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x15) {
    return x15;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map15 = /* @__PURE__ */ map(functorAff);
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
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy5("applyAff", "Effect.Aff", function() {
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
      return map14(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map15(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void5(v.kill(e, $$const(pure4(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map14(effectCanceler)(v.kill(e, k));
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
      return $$void5(runAff(k)(aff));
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
  var $lazy_applicativeParAff = /* @__PURE__ */ $runtime_lazy5("applicativeParAff", "Effect.Aff", function() {
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
      var bind18 = bind(dictMonad.Bind1());
      var pure21 = pure(dictMonad.Applicative0());
      return function(m) {
        return function(s) {
          return bind18(m)(function(x15) {
            return pure21(new Tuple(x15, s));
          });
        };
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransStateT);
  var functorStateT = function(dictFunctor) {
    var map54 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map54(function(v1) {
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
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
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
  var map16 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = /* @__PURE__ */ (function() {
    var $2 = map16((function() {
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
  function _read(nothing, just, value16) {
    var tag2 = Object.prototype.toString.call(value16);
    if (tag2.indexOf("[object HTML") === 0 && tag2.indexOf("Element]") === tag2.length - 8) {
      return just(value16);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode2 = unsafeCoerce2;
  var fromElement = function(x15) {
    return _read(Nothing.value, Just.create, x15);
  };

  // output/Effect.Uncurried/foreign.js
  var mkEffectFn3 = function mkEffectFn32(fn) {
    return function(a2, b2, c) {
      return fn(a2)(b2)(c)();
    };
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
  var input2 = "input";
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";

  // output/Halogen.Aff.Util/index.js
  var bind2 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure5 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map17 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2((function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    })())(document2))(windowImpl)))(function(mel) {
      return pure5(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do3() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document2)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map17(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener2(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener2(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
    return bind2(selectElement("body"))(function(body2) {
      return maybe(throwError2(error("Could not find body")))(pure5)(body2);
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
  var singleton6 = function(k) {
    return function(v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };
  var toUnfoldable2 = function(dictUnfoldable) {
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
              $copy_v = new Cons(v.value0.value0, new Cons(singleton6(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, v.value1)));
              return;
            }
            ;
            if (v.value0 instanceof Three) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton6(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, new Cons(singleton6(v.value0.value4)(v.value0.value5), new Cons(v.value0.value6, v.value1)))));
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
  var lookup2 = function(dictOrd) {
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
                var max9 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max9.key, max9.value, m.value3), ctx))(m.value0)));
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
                var max9 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max9.key, max9.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max9 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max9.key, max9.value, m.value6), ctx))(m.value3)));
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
  var keys2 = /* @__PURE__ */ (function() {
    return foldrWithIndex2(function(k) {
      return function(v) {
        return function(acc) {
          return new Cons(k, acc);
        };
      };
    })(Nil.value);
  })();
  var empty3 = /* @__PURE__ */ (function() {
    return Leaf.value;
  })();
  var fromFoldable3 = function(dictOrd) {
    var insert13 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert13(v.value0)(v.value1)(m);
        };
      })(empty3);
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
    var lookup14 = lookup2(dictOrd);
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
        })(empty3);
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
                return alter1((function() {
                  var $936 = maybe(v)(f(v));
                  return function($937) {
                    return Just.create($936($937));
                  };
                })())(k)(m);
              };
            };
          };
          return foldlWithIndex2(go2)(m2)(m1);
        };
      };
    };
  };
  var union2 = function(dictOrd) {
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
  var lookup1 = /* @__PURE__ */ lookup2(ordTuple2);
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
  var lookup3 = function() {
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
  var empty4 = empty3;

  // output/Control.Applicative.Free/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
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
      return function(x15) {
        return mkAp(new Pure(f))(x15);
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
        $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
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
        var foldl6 = function($copy_v) {
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
              var v = uncons3(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl6(function(x15) {
                  return function(i2) {
                    return i2(x15);
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
      return new Just(new Tuple(v.value0, (function() {
        var $66 = $$null4(v.value1);
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
  var append4 = link;
  var semigroupCatList = {
    append: append4
  };
  var snoc4 = function(cat) {
    return function(a2) {
      return append4(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy6 = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var append5 = /* @__PURE__ */ append(semigroupCatList);
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
          return new Free(v22.value0, append5(v22.value1)(r));
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
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy6("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure6 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure6($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map116 = map(Monad0.Bind1().Apply0().Functor0());
    var pure111 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map116(Done.create)(pure111(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map116(function($199) {
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
  var append6 = /* @__PURE__ */ append(semigroupArray);
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
            return append6(v)([k]);
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
  var SubscriptionId = function(x15) {
    return x15;
  };
  var ForkId = function(x15) {
    return x15;
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
  var HalogenM = function(x15) {
    return x15;
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
    function Initialize11(value0) {
      this.value0 = value0;
    }
    ;
    Initialize11.create = function(value0) {
      return new Initialize11(value0);
    };
    return Initialize11;
  })();
  var Finalize = /* @__PURE__ */ (function() {
    function Finalize7(value0) {
      this.value0 = value0;
    }
    ;
    Finalize7.create = function(value0) {
      return new Finalize7(value0);
    };
    return Finalize7;
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
  var $runtime_lazy7 = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
    var $lazy_patchThunk = $runtime_lazy7("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step2(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step(state3.vdom, toVDom(runThunk(t2)));
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
  var map18 = /* @__PURE__ */ map(functorHalogenM);
  var pure7 = /* @__PURE__ */ pure(applicativeHalogenM);
  var lookup4 = /* @__PURE__ */ lookup3();
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
          var $45 = map18(maybe(v.value1(unit))(g));
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
      handleAction: $$const(pure7(unit)),
      handleQuery: $$const(pure7(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  })();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      var lookup14 = lookup4(dictIsSymbol);
      var pop12 = pop3(dictIsSymbol);
      var insert13 = insert3(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup14(dictOrd);
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

  // output/Halogen.HTML/index.js
  var componentSlot2 = /* @__PURE__ */ componentSlot();
  var slot_ = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component9) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot22(label5)(p2)(component9)(input3)($$const(Nothing.value))));
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
  function unsafeReadPropImpl(f, s, key, value16) {
    return value16 == null ? f : s(value16[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail3 = fail(dictMonad);
    var pure21 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value16) {
        return unsafeReadPropImpl(fail3(new TypeMismatch("object", typeOf(value16))), pure21, k, value16);
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
  var map20 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map20(Action.create)(f(ev));
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
  var onClick = /* @__PURE__ */ (function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  })();
  var addForeignPropHandler = function(key) {
    return function(prop29) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped3(reader)(readProp2(prop29))(unsafeToForeign(a2));
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
  var onValueInput = /* @__PURE__ */ addForeignPropHandler(input2)("value")(readString2);

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
  var initDriverState = function(component9) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do3() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty4)();
            var childrenOut = $$new(empty4)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty3))();
            var forks = $$new(empty3)();
            var ds = {
              component: component9,
              state: component9.initialState(input3),
              refs: empty3,
              children: empty4,
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
  var lookup5 = /* @__PURE__ */ lookup2(ordSubscriptionId);
  var bind12 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard3(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
  var pure8 = /* @__PURE__ */ pure(applicativeAff);
  var map21 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var map110 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map22 = /* @__PURE__ */ map(functorMaybe);
  var insert4 = /* @__PURE__ */ insert(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert12 = /* @__PURE__ */ insert(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup12 = /* @__PURE__ */ lookup2(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup2(ordString);
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
                return pure8(result);
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
          return evalM(render3)(ref2)(v["component"]["eval"](new Query(map21(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
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
                return map110(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
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
                  return pure8(v3.value0);
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
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render3(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure8(v3.value0);
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
                      return pure8(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure8(v1.value1);
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
                    return pure8(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp((function() {
                var $118 = evalM(render3)(ref2);
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
                    }))(evalM(render3)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_2(insert12(fid)(fiber))(v2.forks))))(function() {
                        return pure8(v1.value1(fid));
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
                    return pure8(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                    return pure8(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return pure8(v1.value1(lookup22(v1.value0)(v2.refs)));
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
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var map23 = /* @__PURE__ */ map(functorEffect);
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
          return handleAff($58(reverse2($59)));
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
      return write(empty3)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component9) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render3)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_2(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse2(handlers.initializers)))(function() {
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
                    var $62 = render3(lchs);
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
                return unComponentSlot(function(slot) {
                  return function __do3() {
                    var childrenIn = map23(slot.pop)(read(childrenInRef))();
                    var $$var2 = (function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do4() {
                            flip(write)(st.handlerRef)((function() {
                              var $64 = maybe(pure12(unit))(handler3);
                              return function($65) {
                                return $64(slot.output($65));
                              };
                            })())();
                            return handleAff(evalM(render3)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)((function() {
                          var $66 = maybe(pure12(unit))(handler3);
                          return function($67) {
                            return $66(slot.output($67));
                          };
                        })())(slot.input)(slot.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    })();
                    var isDuplicate = map23(function($68) {
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
                        return pure9(renderSpec2.renderChild(v.value0));
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
              var shouldProcessHandlers = map23(isNothing)(read(v.pendingHandlers))();
              when3(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty4)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = (function() {
                var $69 = queueOrRun(v.pendingHandlers);
                var $70 = evalF(render3)(v.selfRef);
                return function($71) {
                  return $69($$void7($70($71)));
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
              return when3(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do4() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23((function() {
                    var $75 = traverse_5(fork4);
                    return function($76) {
                      return handleAff($75(reverse2($76)));
                    };
                  })())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $51 = maybe(false)($$null3)(mmore);
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
              var dsx = bindFlipped6(read)(runComponent(lchs)((function() {
                var $77 = notify(sio.listener);
                return function($78) {
                  return liftEffect5($77($78));
                };
              })())(i2)(component9))();
              return unDriverStateX(function(st) {
                return pure9({
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
  var map24 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ (function() {
    var $6 = map24(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  })();
  var nextSibling = /* @__PURE__ */ (function() {
    var $15 = map24(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  })();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy8 = function(name17, moduleName, init4) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2) return val;
      if (state3 === 1) throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init4();
      state3 = 2;
      return val;
    };
  };
  var $$void8 = /* @__PURE__ */ $$void(functorEffect);
  var pure10 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var not4 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map25 = /* @__PURE__ */ map(functorEffect);
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
        return pure10(unit);
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
                  var step$prime = step(st.value0, slot.value0);
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
                  $$void8(appendChild(node)(toNode2(container)))();
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
                  var machine$prime = step(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when4(not4(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent3))();
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
  var runUI2 = function(component9) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map25(toDocument)(bindFlipped7(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element3))(component9)(i2);
        });
      };
    };
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
  var attributeLabel = function(v) {
    return v.value0;
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
  function d3GetSelectionData_(selection2) {
    return selection2.data();
  }
  function d3FilterSelection_(selection2) {
    return (selector) => selection2.filter(selector);
  }
  function d3LowerSelection_(selection2) {
    return selection2.lower();
  }
  function d3MergeSelectionWith_(enter) {
    return (update3) => {
      return enter.merge(update3);
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
  function d3SelectFirstInDOM_(selector) {
    return d3.select(selector);
  }
  function d3SelectionIsEmpty_(selection2) {
    return selection2.empty();
  }
  function d3SelectionSelect_(selector) {
    return (selection2) => {
      return selection2.select(selector);
    };
  }
  function d3SelectionSelectAll_(selector) {
    return (selection2) => {
      return selection2.selectAll(selector);
    };
  }
  function d3SetAttr_(name17) {
    return (value16) => (selection2) => {
      return selection2.attr(name17, value16);
    };
  }
  function d3SetHTML_(value16) {
    return (selection2) => {
      return selection2.html(value16);
    };
  }
  function d3SetProperty_(value16) {
    return (selection2) => {
      return selection2.property(value16);
    };
  }
  function d3SetText_(value16) {
    return (selection2) => {
      return selection2.text(value16);
    };
  }
  function d3SortSelection_(selection2) {
    return (compare2) => selection2.sort(compare2);
  }
  function simulationDrag_(label5) {
    return (selection2) => (simulation) => (dragFn) => selection2.call(dragFn(label5, simulation));
  }
  function disableDrag_(selection2) {
    return selection2.on(".drag", null);
  }
  function getIndexFromDatum_(datum2) {
    return typeof datum2.index == `undefined` ? "?" : datum2.index;
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
  function simdrag(label5, simulation) {
    function dragstarted(event) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }
    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }
    function dragended(event) {
      if (!event.active) simulation.alphaTarget(0);
      event.subject.fx = null;
      event.subject.fy = null;
    }
    return d3.drag().on("start." + label5, dragstarted).on("drag." + label5, dragged).on("end." + label5, dragended);
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
    return d3.forceLink().id((d5) => d5.id);
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
  function keyIsID_(d5) {
    return d5.id;
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
    return (label5) => simulation.force(label5, null);
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
  unpin = (d5) => {
    d5.fx = null;
    d5.fy = null;
    return d5;
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
  function d3PreserveSimulationPositions_(selection2) {
    return (nodedata) => (keyFn) => {
      const oldNodeMap = new Map(selection2.data().map((d5) => [keyFn(d5), d5]));
      const newNodeMap = new Map(nodedata.map((d5) => [keyFn(d5), d5]));
      console.log(`FFI: d3PreserveSimulationPositions_ given ${nodedata.length} nodes, in selection ${selection2.data().length}`);
      let updatedNodeData = nodedata.map((d5) => {
        let id5 = keyFn(d5);
        let newNode = newNodeMap.get(id5);
        let shell = {};
        if (newNode) {
          console.log(`FFI: copying fx/fy from incoming node to old object (if present)`);
          shell = { fx: newNode.fx, fy: newNode.fy, gridXY: newNode.gridXY, updated: true };
        }
        return Object.assign(oldNodeMap.get(id5) || d5, shell);
      });
      return updatedNodeData;
    };
  }
  function d3PreserveLinkReferences_(link4) {
    return (links) => {
      const old = new Map(link4.data().map((d5) => [getLinkID_(d5), d5]));
      let updatedLinkData = links.map((d5) => Object.assign(old.get(getLinkID_(d5)) || d5, {}));
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
      const nodeById = new Map(simNodes.map((d5) => [keyFn(d5), d5]));
      const swizzledLinks = links.filter((link4, index5, arr) => {
        if (typeof link4.source !== "object") {
          link4.source = nodeById.get(link4.source);
        } else {
          link4.source = nodeById.get(keyFn(link4.source));
        }
        if (typeof link4.target !== "object") {
          link4.target = nodeById.get(link4.target);
        } else {
          link4.target = nodeById.get(keyFn(link4.target));
        }
        if (typeof link4.source === "undefined" || link4.target === "undefined") {
          return false;
        } else {
          link4.id = keyFn(link4.source) + "-" + keyFn(link4.target);
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
    return (link4) => {
      const sourceID = typeof link4.source == `object` ? keyFn(link4.source) : link4.source;
      const targetID = typeof link4.target == `object` ? keyFn(link4.target) : link4.target;
      return sourceID + "-" + targetID;
    };
  }
  function getLinkIDs_(keyFn) {
    return (link4) => {
      const sourceID = typeof link4.source == `object` ? keyFn(link4.source) : link4.source;
      const targetID = typeof link4.target == `object` ? keyFn(link4.target) : link4.target;
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
    return (label5) => (force2) => {
      simulation.force(label5, force2);
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
  function hasChildren_(d5) {
    return d5.children === "undefined" ? false : true;
  }
  function getHierarchyValue_(d5) {
    return d5.value === "undefined" ? null : d5.value;
  }
  function getHierarchyChildren_(d5) {
    return !d5.children ? [] : d5.children;
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
  function treeSortForTree_Spago(root2) {
    return root2.sum(function(d5) {
      return d5.value;
    }).sort(function(a2, b2) {
      const result = b2.height - a2.height || a2.data.name.localeCompare(b2.data.name);
      return result;
    });
  }
  function treeMinMax_(root2) {
    let max_x = -Infinity;
    let min_x = Infinity;
    let max_y = -Infinity;
    let min_y = Infinity;
    root2.each((d5) => {
      if (d5.x > max_x) max_x = d5.x;
      if (d5.y > max_y) max_y = d5.y;
      if (d5.x < min_x) min_x = d5.x;
      if (d5.y < min_y) min_y = d5.y;
    });
    return { xMin: min_x, xMax: max_x, yMin: min_y, yMax: max_y };
  }
  var linkHorizontal_ = d3.linkHorizontal().x((d5) => d5.y).y((d5) => d5.x);
  var linkHorizontal2_ = d3.linkHorizontal().x((d5) => d5.x).y((d5) => d5.y);
  var linkVertical_ = d3.linkVertical().x((d5) => d5.x).y((d5) => d5.y);
  function linkClusterHorizontal_(levelSpacing) {
    return (d5) => `M${d5.target.y}, ${d5.target.x}
   C${d5.source.y + levelSpacing / 2},${d5.target.x}
   ${d5.source.y + levelSpacing / 2},${d5.source.x}
   ${d5.source.y},${d5.source.x}`;
  }
  function linkClusterVertical_(levelSpacing) {
    return (d5) => `M${d5.target.x}, ${d5.target.y}
   C${d5.target.x}, ${d5.source.y + levelSpacing / 2}
   ${d5.source.x},${d5.source.y + levelSpacing / 2}
   ${d5.source.x},${d5.source.y}`;
  }
  function linkRadial_(angleFn) {
    return (radiusFn) => d3.linkRadial().angle(angleFn).radius(radiusFn);
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

  // output/D3.Data.Tree/foreign.js
  function idTreeLeaf_(obj) {
    const treeObj = Object.assign({}, obj);
    treeObj.isTreeLeaf = true;
    return treeObj;
  }
  function idTreeParent_(obj) {
    return (children3) => {
      const treeObj = Object.assign({}, obj);
      treeObj.isTreeLeaf = false;
      treeObj.children = children3;
      return treeObj;
    };
  }
  var emptyTreeJson_ = {};

  // output/D3.Data.Tree/index.js
  var lookup6 = /* @__PURE__ */ lookup2(ordInt);
  var map26 = /* @__PURE__ */ map(functorArray);
  var fromFoldable4 = /* @__PURE__ */ fromFoldable(foldableList);
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
  var makeD3TreeJSONFromTreeID = function(root2) {
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
      return go2(root2);
    };
  };
  var eqTreeLayout = {
    eq: function(x15) {
      return function(y10) {
        if (x15 instanceof Radial && y10 instanceof Radial) {
          return true;
        }
        ;
        if (x15 instanceof Horizontal && y10 instanceof Horizontal) {
          return true;
        }
        ;
        if (x15 instanceof Vertical && y10 instanceof Vertical) {
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
  var DefaultDrag = /* @__PURE__ */ (function() {
    function DefaultDrag2() {
    }
    ;
    DefaultDrag2.value = new DefaultDrag2();
    return DefaultDrag2;
  })();
  var NoDrag = /* @__PURE__ */ (function() {
    function NoDrag2() {
    }
    ;
    NoDrag2.value = new NoDrag2();
    return NoDrag2;
  })();
  var CustomDrag = /* @__PURE__ */ (function() {
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
        return selectionOn_(v)(show4(v1.value0))(v1.value1);
      }
      ;
      if (v1 instanceof OnT$prime) {
        return selectionOn_(v)(show4(v1.value0))(v1.value1);
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
  var map27 = /* @__PURE__ */ map(functorArray);
  var show5 = /* @__PURE__ */ show(showNumber);
  var append13 = /* @__PURE__ */ append(semigroupArray);
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
    function None4() {
    }
    ;
    None4.value = new None4();
    return None4;
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
      return show32(v.value0) + (show22(v.value1) + (" " + show13(v.value2)));
    }
  };
  var y2 = function(dictToAttr) {
    var $109 = AttributeSetter.create("y2");
    var $110 = toAttr(dictToAttr);
    return function($111) {
      return AttrT.create($109($110($111)));
    };
  };
  var y1 = function(dictToAttr) {
    var $112 = AttributeSetter.create("y1");
    var $113 = toAttr(dictToAttr);
    return function($114) {
      return AttrT.create($112($113($114)));
    };
  };
  var y = function(dictToAttr) {
    var $115 = AttributeSetter.create("y");
    var $116 = toAttr(dictToAttr);
    return function($117) {
      return AttrT.create($115($116($117)));
    };
  };
  var x2 = function(dictToAttr) {
    var $118 = AttributeSetter.create("x2");
    var $119 = toAttr(dictToAttr);
    return function($120) {
      return AttrT.create($118($119($120)));
    };
  };
  var x1 = function(dictToAttr) {
    var $121 = AttributeSetter.create("x1");
    var $122 = toAttr(dictToAttr);
    return function($123) {
      return AttrT.create($121($122($123)));
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
          var vb = intercalate4(" ")(map27(show5)([xo, yo, w, h]));
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
        return [new TransitionT(append13(v.value0)(v1), v.value1)];
      }
      ;
      return cons2(v)(v1);
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
  var opacity = function(dictToAttr) {
    var $168 = AttributeSetter.create("opacity");
    var $169 = toAttr(dictToAttr);
    return function($170) {
      return AttrT.create($168($169($170)));
    };
  };
  var onMouseEventEffectful = function(event) {
    return function(listener) {
      return new OnT$prime(event, mkEffectFn3(listener));
    };
  };
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
  var cursor = function(dictToAttr) {
    var $201 = AttributeSetter.create("cursor");
    var $202 = toAttr(dictToAttr);
    return function($203) {
      return AttrT.create($201($202($203)));
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
  var setSankeyData = function(dict) {
    return dict.setSankeyData;
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
      var transition2 = transitionWithDuration(2e3);
      var update3 = andThen2([classed2("update"), fill2("gray"), y3(200)])(to(transition2)([x3(xFromIndex2)]));
      var exit = andThen2([classed2("exit"), fill2("brown")])(to(transition2)([y3(400), remove]));
      var enter = andThen2([classed2("enter"), fill2("green"), x3(xFromIndex2), y3(0), text7(function($24) {
        return singleton3(datumIsChar($24));
      }), fontSize2(96)])(to(transition2)([y3(200)]));
      return bind18(attach2(selector))(function(root2) {
        return bind18(appendTo2(root2)(Svg.value)([viewBox(0)(0)(650)(650), classed2("d3svg gup")]))(function(svg2) {
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
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var centerVerticalClasses = /* @__PURE__ */ map28(ClassName)(["w-11/12", "rounded", "m-2"]);
  var buttonSharedClasses = /* @__PURE__ */ map28(ClassName)(["no-outline", "px-4", "py-2", "!active:border-b", "active:border-t", "disabled:opacity-50", "disabled:cursor-default", "!disabled:cursor-pointer"]);
  var buttonGroupClasses = /* @__PURE__ */ map28(ClassName)(["flex", "items-center"]);
  var buttonGroupBuilder = function(classes2) {
    return function(iprops) {
      return button(appendIProps([classes(append7(buttonSharedClasses)(classes2))])(iprops));
    };
  };
  var buttonGroup = function(iprops) {
    return div2(appendIProps([classes(buttonGroupClasses)])(iprops));
  };
  var buttonClasses = /* @__PURE__ */ map28(ClassName)(["bg-grey-50-a20", "border-grey-50-a20", "hover:!disabled:bg-grey-50-a30", "focus:bg-grey-50-a30", "text-black-20"]);
  var buttonVertical = /* @__PURE__ */ buttonGroupBuilder(/* @__PURE__ */ append7(buttonClasses)(centerVerticalClasses));

  // output/D3Tagless.Block.Expandable/index.js
  var map29 = /* @__PURE__ */ map(functorArray);
  var append8 = /* @__PURE__ */ append(semigroupArray);
  var Collapsed = /* @__PURE__ */ (function() {
    function Collapsed2() {
    }
    ;
    Collapsed2.value = new Collapsed2();
    return Collapsed2;
  })();
  var Expanded = /* @__PURE__ */ (function() {
    function Expanded2() {
    }
    ;
    Expanded2.value = new Expanded2();
    return Expanded2;
  })();
  var toBoolean = function(v) {
    if (v instanceof Collapsed) {
      return false;
    }
    ;
    if (v instanceof Expanded) {
      return true;
    }
    ;
    throw new Error("Failed pattern match at D3Tagless.Block.Expandable (line 38, column 1 - line 38, column 31): " + [v.constructor.name]);
  };
  var heytingAlgebraStatus = /* @__PURE__ */ (function() {
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
        throw new Error("Failed pattern match at D3Tagless.Block.Expandable (line 46, column 1 - line 56, column 27): " + [v.constructor.name]);
      }
    };
  })();
  var contentSharedClasses = /* @__PURE__ */ map29(ClassName)([]);
  var contentClasses = function(status_) {
    return append8(contentSharedClasses)((function() {
      if (status_ instanceof Collapsed) {
        return map29(ClassName)(["max-h-0", "opacity-0", "w-0", "overflow-hidden", "transition-1/2-in"]);
      }
      ;
      if (status_ instanceof Expanded) {
        return map29(ClassName)(["max-h-full", "opacity-100", "transition-1/2-out"]);
      }
      ;
      throw new Error("Failed pattern match at D3Tagless.Block.Expandable (line 83, column 5 - line 95, column 8): " + [status_.constructor.name]);
    })());
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
        return div2(appendIProps([classes(fieldClasses)])(iprops))([label([classes(labelClasses), $$for(config.inputId)])([fromPlainHTML(config.label)]), html2, error_(config.error), helpText_(config.helpText)]);
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
  var append9 = /* @__PURE__ */ append(semigroupArray);
  var type_19 = /* @__PURE__ */ type_(isPropInputType);
  var toggleClasses = /* @__PURE__ */ map31(ClassName)(["transition-1/8", "inline-flex", "justify-center", "items-center", "content-box", "h-5", "w-5", "p-1", "rounded-full", "mr-3", "before:bg-white", "before:h-full", "before:w-full", "before:rounded-full", "before:no-content", "before:shadow"]);
  var labelClasses2 = /* @__PURE__ */ map31(ClassName)(["flex", "flex-row", "items-center", "inline-block", "py-1", "cursor-pointer", "leading-loose", "text-black-20"]);
  var inputClasses = /* @__PURE__ */ map31(ClassName)(["checked:sibling:bg-blue-88", "checked:sibling:pl-5", "!checked:sibling:bg-grey-80", "!checked:sibling:pr-5", "offscreen"]);
  var toggle = function(iprops) {
    var iprops$prime = append9(iprops)([classes(inputClasses), type_19(InputCheckbox.value)]);
    return label([classes(labelClasses2)])([input(iprops$prime), span2([classes(toggleClasses)])([])]);
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
  var ScaleExtent = /* @__PURE__ */ (function() {
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
  function _trace(x15, k) {
    if (util !== void 0) {
      console.log(util.inspect(x15, { depth: null, colors: true }));
    } else {
      console.log(x15);
    }
    return k({});
  }
  function _spy(tag2, x15) {
    if (util !== void 0) {
      console.log(tag2 + ":", util.inspect(x15, { depth: null, colors: true }));
    } else {
      console.log(tag2 + ":", x15);
    }
    return x15;
  }
  var now = (function() {
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
    return function(selection2) {
      return function(selector) {
        return pure21(d3SelectionSelectAll_(selector)(selection2));
      };
    };
  };
  var selectionOpenSelection = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(selection2) {
      return function(selector) {
        var v = spy2("open selection: ")(selector);
        return pure21(d3SelectionSelectAll_(selector)(selection2));
      };
    };
  };
  var selectionOn = function(dictSelectionM) {
    var pure21 = pure(dictSelectionM.Monad0().Applicative0());
    return function(v) {
      return function(v1) {
        if (v1 instanceof Drag) {
          return pure21(unit);
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
          return pure21(unit);
        }
        ;
        throw new Error("Failed pattern match at D3.Selection.Functions (line 65, column 1 - line 65, column 104): " + [v.constructor.name, v1.constructor.name]);
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
    return function(selection2) {
      return function(e) {
        return function(theData) {
          return function(keyFn) {
            var element3 = spy2("Join: ")(show6(e));
            var selectS = d3SelectionSelectAll_(element3)(selection2);
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
  var Forget = function(x15) {
    return x15;
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
  var lens = function(get8) {
    return function(set5) {
      return function(dictStrong) {
        return lens$prime(function(s) {
          return new Tuple(get8(s), function(b2) {
            return set5(s)(b2);
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
    var get8 = get2(dictIsSymbol)();
    var set5 = set(dictIsSymbol)()();
    return function() {
      return function() {
        return function(l) {
          return function(dictStrong) {
            return lens(get8(l))(flip(set5(l)))(dictStrong);
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
  var unwrap6 = /* @__PURE__ */ unwrap();
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
    return new $$String2(identity13);
  })();
  var ignore = /* @__PURE__ */ (function() {
    return new Ignore(identity13);
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
  function id3(x15) {
    return x15;
  }
  function stringify(j) {
    return JSON.stringify(j);
  }

  // output/Data.Argonaut.Core/index.js
  var jsonEmptyObject = /* @__PURE__ */ id3(empty2);

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
  var map32 = /* @__PURE__ */ map(functorMaybe);
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
  var pure11 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var fail2 = /* @__PURE__ */ fail(monadIdentity);
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var alt5 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var composeKleisliFlipped4 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var map33 = /* @__PURE__ */ map(functorMaybe);
  var any3 = /* @__PURE__ */ any2(foldableArray)(heytingAlgebraBoolean);
  var eq3 = /* @__PURE__ */ eq(eqString);
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var map111 = /* @__PURE__ */ map(functorArray);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorAff);
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
          return pure11(jsonEmptyObject);
        }
        ;
        return either(function($74) {
          return fail2(ForeignError.create($74));
        })(pure11)(jsonParser(v2));
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
          return function(x15) {
            return alt5(unsafeReadTagged2("Document")(x15))(alt5(unsafeReadTagged2("XMLDocument")(x15))(unsafeReadTagged2("HTMLDocument")(x15)));
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
          headers: map111(function(h) {
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
  var get3 = function(driver2) {
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
      let { name: name17, value: value16 } = node.attributes.item(i2);
      entries.push([name17, value16]);
    }
    return entries;
  }
  function walk(treeWalker) {
    var nodes = [];
    function handleNode(node) {
      if (["#comment", "#text"].includes(node.nodeName)) {
        var text12 = node.textContent;
        if (text12) {
          nodes.push({
            type: node.nodeName.slice(1),
            text: text12
          });
        }
      } else {
        var children3 = walk(treeWalker);
        treeWalker.currentNode = node;
        nodes.push({
          type: "element",
          name: node.localName,
          attributes: getAttributes(node),
          children: children3
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
      var headNodes = walk(
        doc.createTreeWalker(doc.documentElement.querySelector("head"))
      );
      var bodyNodes = walk(
        doc.createTreeWalker(doc.documentElement.querySelector("body"))
      );
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
  var HtmlAttribute = /* @__PURE__ */ (function() {
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
  })();
  var HtmlElement = /* @__PURE__ */ (function() {
    function HtmlElement2(value0) {
      this.value0 = value0;
    }
    ;
    HtmlElement2.create = function(value0) {
      return new HtmlElement2(value0);
    };
    return HtmlElement2;
  })();
  var HtmlText = /* @__PURE__ */ (function() {
    function HtmlText2(value0) {
      this.value0 = value0;
    }
    ;
    HtmlText2.create = function(value0) {
      return new HtmlText2(value0);
    };
    return HtmlText2;
  })();
  var HtmlComment = /* @__PURE__ */ (function() {
    function HtmlComment2(value0) {
      this.value0 = value0;
    }
    ;
    HtmlComment2.create = function(value0) {
      return new HtmlComment2(value0);
    };
    return HtmlComment2;
  })();
  var parse6 = function(input3) {
    return parseFromString(HtmlElement.create)(HtmlAttribute.create)(HtmlText.create)(HtmlComment.create)(input3);
  };

  // output/Html.Renderer.Halogen/index.js
  var mapFlipped3 = /* @__PURE__ */ mapFlipped(functorMaybe);
  var alt6 = /* @__PURE__ */ alt(altMaybe);
  var mapFlipped1 = /* @__PURE__ */ mapFlipped(functorArray);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable(foldableArray);
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
        return text(v1.value0);
      }
      ;
      if (v1 instanceof HtmlComment) {
        return text("");
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
      var ctor = (function() {
        if (mNs instanceof Just) {
          return elementNS(mNs.value0);
        }
        ;
        if (mNs instanceof Nothing) {
          return element2;
        }
        ;
        throw new Error("Failed pattern match at Html.Renderer.Halogen (line 34, column 12 - line 36, column 28): " + [mNs.constructor.name]);
      })();
      var children3 = mapFlipped1(ele.children)(nodeToHtml(mNs));
      return ctor(ele.name)(fromFoldable5(map34(htmlAttributeToProp)(ele.attributes)))(children3);
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
    var map54 = map(dictFunctor);
    return function(texts) {
      return map54(p([classes(["m-2"])]))(map54(function($4) {
        return singleton2(text($4));
      })(texts));
    };
  };

  // output/Snippets/index.js
  var map35 = /* @__PURE__ */ map(functorArray);
  var bind5 = /* @__PURE__ */ bind(bindAff);
  var spy3 = /* @__PURE__ */ spy();
  var pure14 = /* @__PURE__ */ pure(applicativeAff);
  var Blurb = /* @__PURE__ */ (function() {
    function Blurb2(value0) {
      this.value0 = value0;
    }
    ;
    Blurb2.create = function(value0) {
      return new Blurb2(value0);
    };
    return Blurb2;
  })();
  var SnippetFile = /* @__PURE__ */ (function() {
    function SnippetFile2(value0) {
      this.value0 = value0;
    }
    ;
    SnippetFile2.create = function(value0) {
      return new SnippetFile2(value0);
    };
    return SnippetFile2;
  })();
  var Snippet = /* @__PURE__ */ (function() {
    function Snippet2(value0) {
      this.value0 = value0;
    }
    ;
    Snippet2.create = function(value0) {
      return new Snippet2(value0);
    };
    return Snippet2;
  })();
  var PreRendered = /* @__PURE__ */ (function() {
    function PreRendered2(value0) {
      this.value0 = value0;
    }
    ;
    PreRendered2.create = function(value0) {
      return new PreRendered2(value0);
    };
    return PreRendered2;
  })();
  var RenderWithState = /* @__PURE__ */ (function() {
    function RenderWithState2(value0) {
      this.value0 = value0;
    }
    ;
    RenderWithState2.create = function(value0) {
      return new RenderWithState2(value0);
    };
    return RenderWithState2;
  })();
  var renderCell = function(v) {
    return function(v1) {
      if (v1 instanceof Blurb) {
        return p([classes(["m-2"])])([text(v1.value0)]);
      }
      ;
      if (v1 instanceof Snippet) {
        return pre([class_(v1.value0.language)])([code_([render_(highlightString_(v1.value0.text))])]);
      }
      ;
      if (v1 instanceof SnippetFile) {
        return p([classes(["m-2"])])([text("Snippet file not loaded: " + (v1.value0 + " Did you remember to call substituteSnippetCells on your Notebook?"))]);
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
  var Running = /* @__PURE__ */ (function() {
    function Running2() {
    }
    ;
    Running2.value = new Running2();
    return Running2;
  })();
  var Paused = /* @__PURE__ */ (function() {
    function Paused2() {
    }
    ;
    Paused2.value = new Paused2();
    return Paused2;
  })();
  var Initialize2 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var SetStatus = /* @__PURE__ */ (function() {
    function SetStatus2(value0) {
      this.value0 = value0;
    }
    ;
    SetStatus2.create = function(value0) {
      return new SetStatus2(value0);
    };
    return SetStatus2;
  })();
  var ToggleStatus = /* @__PURE__ */ (function() {
    function ToggleStatus2() {
    }
    ;
    ToggleStatus2.value = new ToggleStatus2();
    return ToggleStatus2;
  })();
  var Finalize2 = /* @__PURE__ */ (function() {
    function Finalize7() {
    }
    ;
    Finalize7.value = new Finalize7();
    return Finalize7;
  })();
  var ToggleCard = /* @__PURE__ */ (function() {
    function ToggleCard8(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard8.create = function(value0) {
      return new ToggleCard8(value0);
    };
    return ToggleCard8;
  })();
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
    var getLetters = (function() {
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
    })();
    return bind15(liftEffect7(getLetters))(function(letters) {
      return discard12(update3(letters))(function() {
        return delay(2300);
      });
    });
  };
  var runGeneralUpdatePattern = function(dictBind) {
    var discard29 = discard7(dictBind);
    var bind23 = bind(dictBind);
    return function(dictMonadEffect) {
      var liftEffect12 = liftEffect(dictMonadEffect);
      var pure111 = pure(dictMonadEffect.Monad0().Applicative0());
      return discard29(log3(dictMonadEffect)("General Update Pattern example"))(function() {
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
          return bind23((function() {
            if (maybeFiber instanceof Nothing) {
              return pure111(unit);
            }
            ;
            if (maybeFiber instanceof Just) {
              return liftAff2(killFiber(error("Cancel fiber to suspend computation"))(maybeFiber.value0));
            }
            ;
            throw new Error("Failed pattern match at Stories.GUP (line 211, column 8 - line 213, column 100): " + [maybeFiber.constructor.name]);
          })())(function() {
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
    eq: function(x15) {
      return function(y10) {
        if (x15 instanceof Running && y10 instanceof Running) {
          return true;
        }
        ;
        if (x15 instanceof Paused && y10 instanceof Paused) {
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
      var get8 = get(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadAff) {
        var pure111 = pure(dictMonadAff.MonadEffect0().Monad0().Applicative0());
        var liftAff2 = liftAff(dictMonadAff);
        return bind23(get8)(function(v) {
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
  var _snippets = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  })();
  var _panels = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  })();
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
    var discard29 = discard7(dictBind);
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
              return discard29(assign4(_drawCode1)(text1))(function() {
                return bind23(liftAff2(readSnippetFiles("GUPHandleActions")))(function(text22) {
                  return discard29(assign4(_handlerCode1)(text22))(function() {
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
              return bind23((function() {
                if (maybeFiber instanceof Nothing) {
                  return pure111(unit);
                }
                ;
                if (maybeFiber instanceof Just) {
                  return liftAff2(killFiber(error("Cancelling fiber and terminating computation"))(maybeFiber.value0));
                }
                ;
                throw new Error("Failed pattern match at Stories.GUP (line 197, column 10 - line 199, column 111): " + [maybeFiber.constructor.name]);
              })())(function() {
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
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-controls")])([buttonGroup([class_("flex-col")])([buttonVertical([onClick($$const(ToggleStatus.value))])([text(show7(state3.status))])])]), div2([tailwindClass("story-panel-about")])([field_({
        label: text("About"),
        helpText: [],
        error: [],
        inputId: "show-blurb"
      })([toggle([id2("show-blurb"), checked(toBoolean(view(_blurb1)(state3))), onChange(function(v) {
        return new ToggleCard(function(dictStrong) {
          return _blurb(dictStrong);
        });
      })])]), content_(view(_blurb1)(state3))(blurbtext)]), div2([tailwindClass("story-panel-code")])([field_({
        label: text("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked(toBoolean(view(_code1)(state3))), onChange(function(v) {
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
        handleQuery: defaultEval.handleQuery,
        handleAction: handleAction1(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        initialize: new Just(Initialize2.value),
        finalize: new Just(Finalize2.value),
        receive: function($179) {
          return Just.create(SetStatus.create($179));
        }
      })
    });
  };

  // output/D3.Examples.LesMis.Unsafe/index.js
  var unboxD3SimNode = function(datum2) {
    return datum2;
  };
  var unboxD3SimLink = function(datum2) {
    return datum2;
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
  var head5 = /* @__PURE__ */ adaptMaybe(head);

  // output/Data.Lens.AffineTraversal/index.js
  var identity14 = /* @__PURE__ */ identity(categoryFn);
  var fanout2 = /* @__PURE__ */ fanout(categoryFn)(strongFn);
  var affineTraversal$prime = function(to2) {
    return function(dictStrong) {
      var second2 = second(dictStrong);
      return function(dictChoice) {
        var dimap2 = dimap(dictChoice.Profunctor0());
        var right3 = right(dictChoice);
        return function(pab) {
          return dimap2(to2)(function(v) {
            return either(identity14)(v.value0)(v.value1);
          })(second2(right3(pab)));
        };
      };
    };
  };
  var affineTraversal = function(set5) {
    return function(pre2) {
      return function(dictStrong) {
        return function(dictChoice) {
          return affineTraversal$prime(fanout2(set5)(pre2))(dictStrong)(dictChoice);
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
        var right3 = right(dictChoice);
        var rmap5 = rmap3(Profunctor0);
        return function(pab) {
          return dimap2(fro)(either(identity15)(identity15))(right3(rmap5(to2)(pab)));
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
    return keys2(v);
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
      return function(x15) {
        var $131 = foldl4(f)(x15);
        return function($132) {
          return $131(toList2($132));
        };
      };
    },
    foldr: function(f) {
      return function(x15) {
        var $133 = foldr4(f)(x15);
        return function($134) {
          return $133(toList2($134));
        };
      };
    }
  };
  var empty7 = empty3;
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
    var lookup14 = lookup2(dictOrd);
    return {
      ix: function(k) {
        return function(dictStrong) {
          return function(dictChoice) {
            var set5 = function(s) {
              return function(b2) {
                return update3(function(v) {
                  return new Just(b2);
                })(k)(s);
              };
            };
            var pre2 = function(s) {
              return maybe(new Left(s))(Right.create)(lookup14(k)(s));
            };
            return affineTraversal(set5)(pre2)(dictStrong)(dictChoice);
          };
        };
      }
    };
  };

  // output/Data.Lens.At/index.js
  var atMap = function(dictOrd) {
    var lookup14 = lookup2(dictOrd);
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
  var fromFoldable7 = /* @__PURE__ */ fromFoldable3(ordString);
  var union3 = /* @__PURE__ */ union2(ordString);
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
  var Step3 = /* @__PURE__ */ (function() {
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
  })();
  var StepTransformFFI = /* @__PURE__ */ (function() {
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
  })();
  var Alpha = /* @__PURE__ */ (function() {
    function Alpha2(value0) {
      this.value0 = value0;
    }
    ;
    Alpha2.create = function(value0) {
      return new Alpha2(value0);
    };
    return Alpha2;
  })();
  var AlphaTarget = /* @__PURE__ */ (function() {
    function AlphaTarget2(value0) {
      this.value0 = value0;
    }
    ;
    AlphaTarget2.create = function(value0) {
      return new AlphaTarget2(value0);
    };
    return AlphaTarget2;
  })();
  var AlphaMin = /* @__PURE__ */ (function() {
    function AlphaMin2(value0) {
      this.value0 = value0;
    }
    ;
    AlphaMin2.create = function(value0) {
      return new AlphaMin2(value0);
    };
    return AlphaMin2;
  })();
  var AlphaDecay = /* @__PURE__ */ (function() {
    function AlphaDecay2(value0) {
      this.value0 = value0;
    }
    ;
    AlphaDecay2.create = function(value0) {
      return new AlphaDecay2(value0);
    };
    return AlphaDecay2;
  })();
  var VelocityDecay = /* @__PURE__ */ (function() {
    function VelocityDecay2(value0) {
      this.value0 = value0;
    }
    ;
    VelocityDecay2.create = function(value0) {
      return new VelocityDecay2(value0);
    };
    return VelocityDecay2;
  })();
  var ForceManyBody = /* @__PURE__ */ (function() {
    function ForceManyBody2() {
    }
    ;
    ForceManyBody2.value = new ForceManyBody2();
    return ForceManyBody2;
  })();
  var ForceCenter = /* @__PURE__ */ (function() {
    function ForceCenter2() {
    }
    ;
    ForceCenter2.value = new ForceCenter2();
    return ForceCenter2;
  })();
  var ForceCollide = /* @__PURE__ */ (function() {
    function ForceCollide2() {
    }
    ;
    ForceCollide2.value = new ForceCollide2();
    return ForceCollide2;
  })();
  var ForceX = /* @__PURE__ */ (function() {
    function ForceX2() {
    }
    ;
    ForceX2.value = new ForceX2();
    return ForceX2;
  })();
  var ForceY = /* @__PURE__ */ (function() {
    function ForceY2() {
    }
    ;
    ForceY2.value = new ForceY2();
    return ForceY2;
  })();
  var ForceRadial = /* @__PURE__ */ (function() {
    function ForceRadial2() {
    }
    ;
    ForceRadial2.value = new ForceRadial2();
    return ForceRadial2;
  })();
  var RegularForce = /* @__PURE__ */ (function() {
    function RegularForce2(value0) {
      this.value0 = value0;
    }
    ;
    RegularForce2.create = function(value0) {
      return new RegularForce2(value0);
    };
    return RegularForce2;
  })();
  var LinkForce = /* @__PURE__ */ (function() {
    function LinkForce2() {
    }
    ;
    LinkForce2.value = new LinkForce2();
    return LinkForce2;
  })();
  var ForceActive = /* @__PURE__ */ (function() {
    function ForceActive2() {
    }
    ;
    ForceActive2.value = new ForceActive2();
    return ForceActive2;
  })();
  var ForceDisabled = /* @__PURE__ */ (function() {
    function ForceDisabled2() {
    }
    ;
    ForceDisabled2.value = new ForceDisabled2();
    return ForceDisabled2;
  })();
  var ForceFilter = /* @__PURE__ */ (function() {
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
  })();
  var ForceT = function(x15) {
    return x15;
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
      var map116 = map(dictFunctor);
      return function(labels9) {
        var updatedMap = fromFoldable23(map116(function(l) {
          return new Tuple(l, ForceActive.value);
        })(labels9));
        return function(statusMap) {
          return union3(updatedMap)(map37($$const(ForceDisabled.value))(statusMap));
        };
      };
    };
  };
  var eqForceStatus = {
    eq: function(x15) {
      return function(y10) {
        if (x15 instanceof ForceActive && y10 instanceof ForceActive) {
          return true;
        }
        ;
        if (x15 instanceof ForceDisabled && y10 instanceof ForceDisabled) {
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
      ticks: empty3,
      alpha: defaultConfigSimulation.alpha,
      alphaTarget: defaultConfigSimulation.alphaTarget,
      alphaMin: defaultConfigSimulation.alphaMin,
      alphaDecay: defaultConfigSimulation.alphaDecay,
      velocityDecay: defaultConfigSimulation.velocityDecay
    };
  };
  var allNodes = /* @__PURE__ */ (function() {
    return Nothing.value;
  })();
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
      return (function(v) {
        return v.source;
      })(unboxD3SimLink($57));
    },
    target: function($58) {
      return (function(v) {
        return v.target;
      })(unboxD3SimLink($58));
    },
    value: function($59) {
      return (function(v) {
        return v.value;
      })(unboxD3SimLink($59));
    },
    color: function($60) {
      return d3SchemeCategory10N_(toNumber((function(v) {
        return v.target.group;
      })(unboxD3SimLink($60))));
    }
  };
  var datum_ = {
    id: function($61) {
      return (function(v) {
        return v.id;
      })(unboxD3SimNode($61));
    },
    x: function($62) {
      return (function(v) {
        return v.x;
      })(unboxD3SimNode($62));
    },
    y: function($63) {
      return (function(v) {
        return v.y;
      })(unboxD3SimNode($63));
    },
    group: function($64) {
      return (function(v) {
        return v.group;
      })(unboxD3SimNode($64));
    },
    colorByGroup: function($65) {
      return d3SchemeCategory10N_(toNumber((function(v) {
        return v.group;
      })(unboxD3SimNode($65))));
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
                                        return (function(v2) {
                                          return v2.x;
                                        })(link_.source($67));
                                      }), y12(function($68) {
                                        return (function(v2) {
                                          return v2.y;
                                        })(link_.source($68));
                                      }), x22(function($69) {
                                        return (function(v2) {
                                          return v2.x;
                                        })(link_.target($69));
                                      }), y22(function($70) {
                                        return (function(v2) {
                                          return v2.y;
                                        })(link_.target($70));
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
    const links = json.links.map((d5) => Object.create(d5));
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
  var numKey = /* @__PURE__ */ (function() {
    var $51 = AttributeSetter.create("keyFn");
    return function($52) {
      return ForceT($51(NumberAttr.create(Fn.create($52))));
    };
  })();
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
  var fromFoldable8 = /* @__PURE__ */ fromFoldable3(ordString);
  var unwrap7 = /* @__PURE__ */ unwrap();
  var map112 = /* @__PURE__ */ map(functorArray);
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
      var addFilterToStatic = function(filter6) {
        return function(value16) {
          return function($$default) {
            return function(d5) {
              var $49 = filter6(d5);
              if ($49) {
                return value16;
              }
              ;
              return $$default;
            };
          };
        };
      };
      var addFilterToFn = function(filter6) {
        return function(fn) {
          return function($$default) {
            return function(d5) {
              var $50 = filter6(d5);
              if ($50) {
                return fn(d5);
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
      var v = map112(function(a2) {
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
  var toUnfoldable4 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var map40 = /* @__PURE__ */ map(functorArray);
  var _alphaTarget2 = /* @__PURE__ */ _alphaTarget(strongFn);
  var _alphaMin2 = /* @__PURE__ */ _alphaMin(strongFn);
  var _alphaDecay2 = /* @__PURE__ */ _alphaDecay(strongFn);
  var _velocityDecay2 = /* @__PURE__ */ _velocityDecay(strongFn);
  var _forceLibrary1 = /* @__PURE__ */ _forceLibrary(strongFn);
  var map113 = /* @__PURE__ */ map(functorMap);
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
      return discard111(modifying3(function($231) {
        return _d3Simulation2(_alpha2($231));
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
          return modifying3(function($232) {
            return _d3Simulation2(_alpha2($232));
          })($$const(v.value0));
        }
        ;
        if (v instanceof AlphaTarget) {
          var v1 = setAlphaTarget_(handle)(v.value0);
          return modifying3(function($233) {
            return _d3Simulation2(_alphaTarget2($233));
          })($$const(v.value0));
        }
        ;
        if (v instanceof AlphaMin) {
          var v1 = setAlphaMin_(handle)(v.value0);
          return modifying3(function($234) {
            return _d3Simulation2(_alphaMin2($234));
          })($$const(v.value0));
        }
        ;
        if (v instanceof AlphaDecay) {
          var v1 = setAlphaDecay_(handle)(v.value0);
          return modifying3(function($235) {
            return _d3Simulation2(_alphaDecay2($235));
          })($$const(v.value0));
        }
        ;
        if (v instanceof VelocityDecay) {
          var v1 = setVelocityDecay_(handle)(v.value0);
          return modifying3(function($236) {
            return _d3Simulation2(_velocityDecay2($236));
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
            var v = setLinks_(handle)(filter(filterFn)(d3GetSelectionData_(linkSelection)));
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
    return function(v) {
      return function(v1) {
        if (v1 instanceof Drag) {
          return bind18(use3(_handle2))(function(handle) {
            var v22 = (function() {
              if (v1.value0 instanceof DefaultDrag) {
                return simulationDrag_("default")(v)(handle)(simdrag);
              }
              ;
              if (v1.value0 instanceof NoDrag) {
                return disableDrag_(v);
              }
              ;
              if (v1.value0 instanceof CustomDrag) {
                return simulationDrag_(v1.value0.value0)(v)(handle)(v1.value0.value1);
              }
              ;
              throw new Error("Failed pattern match at D3.Simulation.Functions (line 282, column 11 - line 285, column 78): " + [v1.value0.constructor.name]);
            })();
            return pure21(unit);
          });
        }
        ;
        if (v1 instanceof Zoom) {
          var v2 = (function() {
            if (v1.value0.extent instanceof DefaultZoomExtent) {
              return d3AttachZoomDefaultExtent_(v)({
                scaleExtent: [v1.value0.scale.value0, v1.value0.scale.value1],
                name: v1.value0.name,
                target: v1.value0.target
              });
            }
            ;
            if (v1.value0.extent instanceof ZoomExtent) {
              return d3AttachZoom_(v)({
                extent: [[v1.value0.extent.value0.left, v1.value0.extent.value0.top], [v1.value0.extent.value0.right, v1.value0.extent.value0.bottom]],
                scaleExtent: [v1.value0.scale.value0, v1.value0.scale.value1],
                name: v1.value0.name,
                target: v1.value0.target
              });
            }
            ;
            throw new Error("Failed pattern match at D3.Simulation.Functions (line 293, column 9 - line 307, column 14): " + [v1.value0.extent.constructor.name]);
          })();
          return pure21(unit);
        }
        ;
        throw new Error("Failed pattern match at D3.Simulation.Functions (line 277, column 1 - line 279, column 51): " + [v.constructor.name, v1.constructor.name]);
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
                  var validNewLinks = filter(validLink)(rawdata.links);
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
    return map40(fst)(filter(function(v) {
      return eq4(view(_status3)(v.value1))(ForceActive.value);
    })(toUnfoldable4(forceMap)));
  };
  var listActiveForces = function(forceMap) {
    return map40(fst)(filter(function(v) {
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
            var v2 = map113(updateForceInSimulation(handle))(forceLibrary3);
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
      var get8 = get(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadEffect) {
        var liftEffect11 = liftEffect(dictMonadEffect);
        return function(state_T) {
          return bind18(get8)(function(state3) {
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
      var get8 = get(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadEffect) {
        var liftEffect11 = liftEffect(dictMonadEffect);
        var pure111 = pure(dictMonadEffect.Monad0().Applicative0());
        return function(state_T) {
          return bind18(get8)(function(state3) {
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
      return function(selection2) {
        return simulationMergeNewData2(selection2);
      };
    },
    setNodesFromSelection: function(selection2) {
      return simulationSetNodesFromSelection2(selection2);
    },
    setLinksFromSelection: function(selection2) {
      return simulationSetLinksFromSelection2(selection2);
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
    removeTickFunction: function(label5) {
      return bind6(simulationHandle(simulationMD3Selection_D3))(function(handle) {
        var v = disableTick_(handle)(label5);
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

  // output/DemoApp.UI.FormField/index.js
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
        return div2(appendIProps([classes(fieldClasses2)])(iprops))([label([classes(labelClasses3), $$for(config.inputId)])([fromPlainHTML(config.label)]), html2, error_2(config.error), helpText_2(config.helpText)]);
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
  var Initialize3 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var Finalize3 = /* @__PURE__ */ (function() {
    function Finalize7() {
    }
    ;
    Finalize7.value = new Finalize7();
    return Finalize7;
  })();
  var ToggleCard2 = /* @__PURE__ */ (function() {
    function ToggleCard8(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard8.create = function(value0) {
      return new ToggleCard8(value0);
    };
    return ToggleCard8;
  })();
  var ToggleForce = /* @__PURE__ */ (function() {
    function ToggleForce3(value0) {
      this.value0 = value0;
    }
    ;
    ToggleForce3.create = function(value0) {
      return new ToggleForce3(value0);
    };
    return ToggleForce3;
  })();
  var Freeze = /* @__PURE__ */ (function() {
    function Freeze2() {
    }
    ;
    Freeze2.value = new Freeze2();
    return Freeze2;
  })();
  var Reheat = /* @__PURE__ */ (function() {
    function Reheat2() {
    }
    ;
    Reheat2.value = new Reheat2();
    return Reheat2;
  })();
  var forceNames = {
    manyBodyNeg: "many body negative",
    manyBodyPos: "many body positive",
    collision: "collision",
    center: "center",
    links: linksForceName
  };
  var forces = /* @__PURE__ */ (function() {
    return {
      manyBodyNeg: createForce(forceNames.manyBodyNeg)(new RegularForce(ForceManyBody.value))(allNodes)([strength2(-40)]),
      manyBodyPos: createForce(forceNames.manyBodyPos)(new RegularForce(ForceManyBody.value))(allNodes)([strength2(30)]),
      collision: createForce(forceNames.collision)(new RegularForce(ForceCollide.value))(allNodes)([radius3(toAttrNumber)(4)]),
      center: createForce(forceNames.center)(new RegularForce(ForceCenter.value))(allNodes)([x4(toAttrNumber)(0), y4(toAttrNumber)(0), strength2(1)]),
      links: createLinkForce(Nothing.value)([])
    };
  })();
  var forceLibrary = /* @__PURE__ */ (function() {
    return initialize(foldableArray)(functorArray)([forces.manyBodyNeg, forces.manyBodyPos, forces.collision, forces.center, forces.links]);
  })();
  var _panels2 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  })();
  var _notebook = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "notebook";
      }
    })()()($$Proxy.value);
  })();
  var _notebook1 = /* @__PURE__ */ _notebook(strongFn);
  var _forceStatuses = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "forceStatuses";
      }
    })()()($$Proxy.value);
  })();
  var _forceStatuses1 = /* @__PURE__ */ _forceStatuses(strongFn);
  var _forceStatuses2 = /* @__PURE__ */ _forceStatuses(strongForget);
  var _forceStatus = function(dictAt) {
    var at4 = at(dictAt);
    return function(dictStrong) {
      return function(dictChoice) {
        var _Just2 = _Just(dictChoice);
        return function(label5) {
          var $127 = at4(label5)(dictStrong);
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
    return buttonGroup([class_("flex-col")])([buttonVertical([onClick($$const(new ToggleForce(forceNames.links)))])([text("links: " + showMaybeForceStatus(preview(_linksSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(new ToggleForce(forceNames.manyBodyPos)))])([text("many body +: " + showMaybeForceStatus(preview(_manyBodyPosSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(new ToggleForce(forceNames.manyBodyNeg)))])([text("many body: -" + showMaybeForceStatus(preview(_manyBodyNegSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(new ToggleForce(forceNames.collision)))])([text("collision: " + showMaybeForceStatus(preview(_collisionSetting1)(forceStatuses)))]), buttonVertical([onClick($$const(Freeze.value))])([text("Freeze")]), buttonVertical([onClick($$const(Reheat.value))])([text("Reheat!")])]);
  };
  var lesMisNotebook = /* @__PURE__ */ (function() {
    return [new Blurb("This example introduces a new capability, signalled by the SimulationM\n    constraint on the function. This monad runs with a D3 Simulation engine in its\n    State. This allows us to let the simulation engine do the layout, we provide\n    the nodes and (optionally) links and configure the simulation with additional\n    forces. "), new RenderWithState(controls2), new Blurb(' From the D3 docs: "This module implements a velocity Verlet numerical\n  integrator for simulating physical forces on particles. The simulation is\n  simplified: it assumes a constant unit time step \u0394t = 1 for each step, and a\n  constant unit mass m = 1 for all particles. As a result, a force F acting on a\n  particle is equivalent to a constant acceleration a over the time interval \u0394t,\n  and can be simulated simply by adding to the particle\u2019s velocity, which is then\n  added to the particle\u2019s position.""\n\n  '), new SnippetFile("LesMisScript"), new SnippetFile("LesMisHandleActions"), new SnippetFile("LesMisAccessors")];
  })();
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
    var discard29 = discard11(dictBind);
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
              return discard29(assign4(_notebook1)(notebook$prime))(function() {
                return bind18(liftAff2(get4(string)("./data/miserables.json")))(function(response) {
                  var graph = readGraphFromFileContents(response);
                  return discard29(modifying3((function() {
                    var $133 = _forceStatus2(forceNames.center);
                    return function($134) {
                      return _forceStatuses1($133($134));
                    };
                  })())($$const(ForceActive.value)))(function() {
                    return discard29(modifying3((function() {
                      var $135 = _forceStatus2(forceNames.manyBodyNeg);
                      return function($136) {
                        return _forceStatuses1($135($136));
                      };
                    })())($$const(ForceActive.value)))(function() {
                      return discard29(modifying3((function() {
                        var $137 = _forceStatus2(forceNames.collision);
                        return function($138) {
                          return _forceStatuses1($137($138));
                        };
                      })())($$const(ForceActive.value)))(function() {
                        return discard29(modifying3((function() {
                          var $139 = _forceStatus2(forceNames.links);
                          return function($140) {
                            return _forceStatuses1($139($140));
                          };
                        })())($$const(ForceActive.value)))(function() {
                          return discard29(modifying3((function() {
                            var $141 = _forceStatus2(forceNames.manyBodyPos);
                            return function($142) {
                              return _forceStatuses1($141($142));
                            };
                          })())($$const(ForceDisabled.value)))(function() {
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
            return discard29(toggleForceByName1(v.value0))(function() {
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
        label: text("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked(toBoolean(view(_code12)(state3))), onChange(function(v) {
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
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        handleAction: handleAction12(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
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
  var rmap4 = /* @__PURE__ */ rmap(bifunctorEither);
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
  var horizontalLink = /* @__PURE__ */ (function() {
    return new AttrT(new AttributeSetter("d", toAttr3(linkHorizontal_)));
  })();
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
      return (function(v) {
        return v.x;
      })(unboxD3TreeNode(coerceToTreeNode($55)));
    },
    y: function($56) {
      return (function(v) {
        return v.y;
      })(unboxD3TreeNode(coerceToTreeNode($56)));
    },
    symbol: function($57) {
      return (function(v) {
        return v.data.symbol;
      })(unboxD3TreeNode(coerceToTreeNode($57)));
    },
    param1: function($58) {
      return (function(v) {
        return v.data.param1;
      })(unboxD3TreeNode(coerceToTreeNode($58)));
    },
    positionXY: function(d5) {
      return "translate(" + (show9(datum_2.x(d5)) + ("," + (show9(datum_2.y(d5)) + ")")));
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
          return bind18(attach2(".svg-container"))(function(root2) {
            return bind18(appendTo2(root2)(Svg.value)([viewBox(vtreeXOffset)(-vtreeYOffset)(pad(xExtent))(pad(yExtent)), preserveAspectRatio(new AspectRatio(XMin.value, YMid.value, Meet.value)), width9(v.value0), height9(v.value1), classed5("metatree")]))(function(svg2) {
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
  var append10 = /* @__PURE__ */ append(semigroupArray);
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
      return function(d5) {
        if (l instanceof Radial) {
          var $42 = treeDatum_.hasChildren(d5) === treeDatum_.x(d5) < pi;
          if ($42) {
            return "start";
          }
          ;
          return "end";
        }
        ;
        var $43 = treeDatum_.hasChildren(d5);
        if ($43) {
          return "start";
        }
        ;
        return "end";
      };
    },
    textX: function(l) {
      return function(d5) {
        if (l instanceof Radial) {
          var $45 = treeDatum_.hasChildren(d5) === treeDatum_.x(d5) < pi;
          if ($45) {
            return 6;
          }
          ;
          return -6;
        }
        ;
        var $46 = treeDatum_.hasChildren(d5);
        if ($46) {
          return 6;
        }
        ;
        return -6;
      };
    },
    onRHS: function(l) {
      return function(d5) {
        var $47 = eq12(l)(Radial.value) && treeDatum_.x(d5) >= pi;
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
          return bind18(attach2(config.selector))(function(root2) {
            return bind18(appendTo2(root2)(Svg.value)(append10(config.viewbox)([classed6("tree"), width10(config.svg.width), height10(config.svg.height)])))(function(svg2) {
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
      node.children.forEach(
        (child) => prune(child)
      );
    }
    ;
  };

  // output/D3Tagless.Capabilities.MetaTree/index.js
  var show10 = /* @__PURE__ */ show(showElement);
  var show14 = /* @__PURE__ */ show(showSelectionAttribute);
  var show23 = /* @__PURE__ */ show(showString);
  var map42 = /* @__PURE__ */ map(functorArray);
  var lookup7 = /* @__PURE__ */ lookup2(ordInt);
  var insert8 = /* @__PURE__ */ insert(ordInt);
  var Empty2 = /* @__PURE__ */ (function() {
    function Empty3() {
    }
    ;
    Empty3.value = new Empty3();
    return Empty3;
  })();
  var AttachNode = /* @__PURE__ */ (function() {
    function AttachNode2(value0) {
      this.value0 = value0;
    }
    ;
    AttachNode2.create = function(value0) {
      return new AttachNode2(value0);
    };
    return AttachNode2;
  })();
  var SelectUnderNode = /* @__PURE__ */ (function() {
    function SelectUnderNode2(value0) {
      this.value0 = value0;
    }
    ;
    SelectUnderNode2.create = function(value0) {
      return new SelectUnderNode2(value0);
    };
    return SelectUnderNode2;
  })();
  var AppendNode = /* @__PURE__ */ (function() {
    function AppendNode2(value0) {
      this.value0 = value0;
    }
    ;
    AppendNode2.create = function(value0) {
      return new AppendNode2(value0);
    };
    return AppendNode2;
  })();
  var FilterNode = /* @__PURE__ */ (function() {
    function FilterNode2(value0) {
      this.value0 = value0;
    }
    ;
    FilterNode2.create = function(value0) {
      return new FilterNode2(value0);
    };
    return FilterNode2;
  })();
  var ModifyNode = /* @__PURE__ */ (function() {
    function ModifyNode2(value0) {
      this.value0 = value0;
    }
    ;
    ModifyNode2.create = function(value0) {
      return new ModifyNode2(value0);
    };
    return ModifyNode2;
  })();
  var JoinSimpleNode = /* @__PURE__ */ (function() {
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
  })();
  var UpdateJoinNode = /* @__PURE__ */ (function() {
    function UpdateJoinNode2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateJoinNode2.create = function(value0) {
      return new UpdateJoinNode2(value0);
    };
    return UpdateJoinNode2;
  })();
  var OpenJoinNode = /* @__PURE__ */ (function() {
    function OpenJoinNode2(value0) {
      this.value0 = value0;
    }
    ;
    OpenJoinNode2.create = function(value0) {
      return new OpenJoinNode2(value0);
    };
    return OpenJoinNode2;
  })();
  var JoinSimpleWithKeyFunctionNode = /* @__PURE__ */ (function() {
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
  })();
  var SplitJoinCloseWithKeyFunctionNode = /* @__PURE__ */ (function() {
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
  })();
  var OnNode = /* @__PURE__ */ (function() {
    function OnNode2(value0) {
      this.value0 = value0;
    }
    ;
    OnNode2.create = function(value0) {
      return new OnNode2(value0);
    };
    return OnNode2;
  })();
  var AttrNode = /* @__PURE__ */ (function() {
    function AttrNode2(value0) {
      this.value0 = value0;
    }
    ;
    AttrNode2.create = function(value0) {
      return new AttrNode2(value0);
    };
    return AttrNode2;
  })();
  var OrderNode = /* @__PURE__ */ (function() {
    function OrderNode2(value0) {
      this.value0 = value0;
    }
    ;
    OrderNode2.create = function(value0) {
      return new OrderNode2(value0);
    };
    return OrderNode2;
  })();
  var OnEventNode = /* @__PURE__ */ (function() {
    function OnEventNode2(value0) {
      this.value0 = value0;
    }
    ;
    OnEventNode2.create = function(value0) {
      return new OnEventNode2(value0);
    };
    return OnEventNode2;
  })();
  var TransitionNode = /* @__PURE__ */ (function() {
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
  })();
  var RemoveNode = /* @__PURE__ */ (function() {
    function RemoveNode2() {
    }
    ;
    RemoveNode2.value = new RemoveNode2();
    return RemoveNode2;
  })();
  var ScriptTree = /* @__PURE__ */ (function() {
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
  })();
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
    var go2 = function(id5) {
      var children3 = map42(snd)(filter(function(v12) {
        return v12.value0 === id5;
      })(v.value2));
      var v1 = showAsSymbol(fromMaybe(Empty2.value)(lookup7(id5)(v.value1)));
      return {
        name: v1.name,
        symbol: v1.symbol,
        param1: v1.param1,
        param2: v1.param2,
        children: map42(go2)(children3)
      };
    };
    return pruneEmptyChildren(go2(0));
  };
  var monadStateD3MetaTreeM = /* @__PURE__ */ monadStateStateT(monadEffect);
  var get5 = /* @__PURE__ */ get(monadStateD3MetaTreeM);
  var modify_3 = /* @__PURE__ */ modify_(monadStateD3MetaTreeM);
  var monadD3MetaTreeM = /* @__PURE__ */ monadStateT(monadEffect);
  var initialMetaTree = /* @__PURE__ */ (function() {
    return new ScriptTree(0, empty3, []);
  })();
  var runMetaTree = function(v) {
    return runStateT(v)(initialMetaTree);
  };
  var bindD3MetaTreeM = /* @__PURE__ */ bindStateT(monadEffect);
  var bind9 = /* @__PURE__ */ bind(bindD3MetaTreeM);
  var discard16 = /* @__PURE__ */ discard(discardUnit)(bindD3MetaTreeM);
  var applicativeD3MetaTreeM = /* @__PURE__ */ applicativeStateT(monadEffect);
  var pure18 = /* @__PURE__ */ pure(applicativeD3MetaTreeM);
  var insertInScriptTree = function(v) {
    return function(v1) {
      if (v1 instanceof TransitionNode) {
        return bind9(get5)(function(v2) {
          return discard16(modify_3(function(s) {
            return new ScriptTree(v2.value0 + 1 | 0, insert8(v2.value0)(v1)(v2.value1), cons2(new Tuple(v, v2.value0 + 1 | 0))(v2.value2));
          }))(function() {
            return pure18(unit);
          });
        });
      }
      ;
      return bind9(get5)(function(v2) {
        return discard16(modify_3(function(s) {
          return new ScriptTree(v2.value0 + 1 | 0, insert8(v2.value0)(v1)(v2.value1), cons2(new Tuple(v, v2.value0 + 1 | 0))(v2.value2));
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
    openSelection: function(selection2) {
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
  function showAddTransition_(selection2) {
    return (transition2) => {
      if (transition2.name == "") {
        const statement1 = `	d3addTransition: ${selection2}.transition(${transition2})`;
        var statement2 = "";
        var statement3 = "";
        if (transition2.duration != 0) {
          statement2 = `transition.duration(${transition2.duration})`;
        }
        if (transition2.delay != 0) {
          statement3 = `		transition.delay(${transition2.delay})`;
        }
        return statement1 + statement2 + statement3;
      } else {
        return `	d3addNamedTransition: ${selection2}.transition(${transition2})`;
      }
    };
  }
  function showRemoveSelection_(selection2) {
    return `	d3Remove: ${selection2}.remove()`;
  }
  function showSetAttr_(name17) {
    return (value16) => (selection2) => {
      return `	${selection2}.attr(${name17}, ${value16})`;
    };
  }
  function showSetText_(value16) {
    return (selection2) => {
      return `	${selection2}.text(${value16})`;
    };
  }
  function showSetHTML_(value16) {
    return (selection2) => {
      return `	${selection2}.html(${value16})`;
    };
  }
  function showSetProperty_(value16) {
    return (selection2) => {
      return `	${selection2}.property(${value16})`;
    };
  }
  function showSetOrdering_(ordering) {
    return (selection2) => {
      return `	${selection2}.${ordering}()`;
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
  var applySelectionAttributeString = function(selection2) {
    return function(v) {
      if (v instanceof AttrT) {
        return showSetAttr_(v.value0.value0)(unboxAttr(v.value0.value1))(selection2);
      }
      ;
      if (v instanceof TextT) {
        return showSetText_(unboxAttr(v.value0.value1))(selection2);
      }
      ;
      if (v instanceof PropertyT) {
        return showSetProperty_(unboxAttr(v.value0.value1))(selection2);
      }
      ;
      if (v instanceof HTMLT) {
        return showSetHTML_(unboxAttr(v.value0.value1))(selection2);
      }
      ;
      if (v instanceof RemoveT) {
        return showRemoveSelection_(selection2);
      }
      ;
      if (v instanceof OrderingT) {
        return showSetOrdering_(show11(v.value0))(selection2);
      }
      ;
      if (v instanceof TransitionT) {
        var tString = showAddTransition_(selection2)(v.value1);
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
    selectUnder: function(selection2) {
      return function(selector) {
        return discard17(modify_4(function(s) {
          return s + ("\nsub-selection " + (selector + "\n"));
        }))(function() {
          return pure19("attach");
        });
      };
    },
    appendTo: function(selection2) {
      return function(element3) {
        return function(attributes) {
          var attributeString = foldl2(applySelectionAttributeString)(selection2)(attributes);
          return discard17(modify_4(function(s) {
            return s + ("\nappending " + (show33(element3) + (" to " + (selection2 + ("\n" + attributeString)))));
          }))(function() {
            return pure19("append");
          });
        };
      };
    },
    filterSelection: function(selection2) {
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
    setAttributes: function(selection2) {
      return function(attributes) {
        var attributeString = foldl2(applySelectionAttributeString)(selection2)(attributes);
        return discard17(modify_4(function(s) {
          return s + ("\nmodifying " + (selection2 + ("\n" + attributeString)));
        }))(function() {
          return pure19(unit);
        });
      };
    },
    on: function(v) {
      return function(v1) {
        if (v1 instanceof Drag) {
          return discard17(modify_4(function(s) {
            return s + ("\nadding drag behavior to " + v);
          }))(function() {
            return pure19(unit);
          });
        }
        ;
        if (v1 instanceof Zoom) {
          return discard17(modify_4(function(s) {
            return s + ("\nadding drag behavior to " + v);
          }))(function() {
            return pure19(unit);
          });
        }
        ;
        throw new Error("Failed pattern match at D3Tagless.Capabilities.String (line 30, column 1 - line 74, column 60): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    openSelection: function(selection2) {
      return function(selector) {
        return discard17(modify_4(function(s) {
          return s + ("\nopening a selection using " + show15(selector));
        }))(function() {
          return pure19("openSelection");
        });
      };
    },
    simpleJoin: function(selection2) {
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
    updateJoin: function(selection2) {
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
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var rotateRadialLabels = function(d5) {
    return "rotate(" + ((function() {
      var $27 = treeDatum_.onRHS(Radial.value)(d5);
      if ($27) {
        return "180";
      }
      ;
      return "0";
    })() + ")");
  };
  var radialTranslate = function(d5) {
    return "translate(" + (show16(treeDatum_.y(d5)) + ",0)");
  };
  var radialRotate = function(x15) {
    return show16(x15 * 180 / pi - 90);
  };
  var radialRotateCommon = function(d5) {
    return "rotate(" + (radialRotate(treeDatum_.x(d5)) + ")");
  };
  var positionXYreflected = function(d5) {
    return "translate(" + (show16(treeDatum_.y(d5)) + ("," + (show16(treeDatum_.x(d5)) + ")")));
  };
  var positionXY = function(d5) {
    return "translate(" + (show16(treeDatum_.x(d5)) + ("," + (show16(treeDatum_.y(d5)) + ")")));
  };
  var configureAndRunScript = function(dictBind) {
    var draw7 = draw4(dictBind);
    return function(dictSelectionM) {
      var draw12 = draw7(dictSelectionM);
      return function(v) {
        return function(model) {
          return function(selector) {
            var svg2 = {
              width: v.value0,
              height: v.value1
            };
            var root2 = hierarchyFromJSON_(model.json);
            var numberOfLevels = hNodeHeight_(root2) + 1;
            var spacing = (function() {
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
            })();
            var layout = (function() {
              var $33 = eq5(model.treeLayout)(Radial.value);
              if ($33) {
                return treeSetSeparation_(treeSetSize_(getLayout(model.treeType))([2 * pi, svg2.width / 2 - 100]))(radialSeparation);
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 116, column 7 - line 123, column 108): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 106, column 7 - line 113, column 71): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 97, column 7 - line 103, column 78): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
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
              throw new Error("Failed pattern match at D3.Examples.Tree.Configure (line 126, column 7 - line 133, column 38): " + [model.treeType.constructor.name, model.treeLayout.constructor.name]);
            })());
            return draw12({
              spacing,
              viewbox,
              selector,
              linkPath,
              nodeTransform,
              color: color2,
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
      var root2 = d3SelectFirstInDOM_(rootSelector);
      var previous = d3SelectionSelect_(rootSelector + " svg")(root2);
      return pure21((function() {
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
      })());
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
  var append11 = /* @__PURE__ */ append(semigroupArray);
  var Initialize4 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var ToggleCard3 = /* @__PURE__ */ (function() {
    function ToggleCard8(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard8.create = function(value0) {
      return new ToggleCard8(value0);
    };
    return ToggleCard8;
  })();
  var drawMetaTree = function(json) {
    return bindFlipped9(drawTree)(bindFlipped9(makeModel2(TidyTree.value)(Vertical.value))(bindFlipped9(getMetaTreeJSON)(makeModel2(TidyTree.value)(Radial.value)(json))));
  };
  var blurbtext2 = [/* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text("The way this library works is by creating an embedded DSL in PureScript\n          which can be interpreted to cause a visualization to come into\n          existence...typically in your browser, as an SVG.\n          ")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text("\n          The primary interpreter that is provided, the one that powers all of the\n          other demos here except these two, turns the statements of this eDSL into D3\n          actions.\n          ")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text("\n          However, other interpreters are possible. This page shows two of them, both\n          quite rudimentary but showing some powerful ideas which could be taken a lot\n          further.\n          ")]), /* @__PURE__ */ h2([/* @__PURE__ */ classes(["text-2xl", "p-2"])])([/* @__PURE__ */ text("MetaTree")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text('\n          The first one, called here "MetaTree" turns a "script" written in the DSL\n          into a syntax tree and then renders the resulting tree using the other,\n          D3-based, interpreter. The result is a kind of x-ray of the script, one which\n          visually describes the structure you are producing. Because interaction is easy\n          to add to DOM-based visualizations such as D3 this could also be a basis for\n          a point-and-click manner for writing visualizations, or perhaps for editing and\n          adapting them. \n          ')]), /* @__PURE__ */ h2([/* @__PURE__ */ classes(["text-2xl", "p-2"])])([/* @__PURE__ */ text("Printer")]), /* @__PURE__ */ p([/* @__PURE__ */ classes(["p-2"])])([/* @__PURE__ */ text('\n          The second example shows that the "script" can be interpreted into a textual\n          form. This could be the basis for documentation or even transpilation. In\n          principle, it is possible to emit the JavaScript / D3 version of the script\n          via this mechanism, but the current implementation is only a proof-of-concept\n          and is not elaborated to that extent.\n          \n          ')])];
  var _snippets2 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  })();
  var _panels3 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  })();
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
            return bind18(liftAff2(readSnippetFiles("MetaTreeDraw")))(function(text12) {
              return discard111(assign4(_drawCode12)(text12))(function() {
                return bind18(liftAff2(readSnippetFiles("MetaTreeEvaluator")))(function(text1) {
                  return discard111(assign4(_evaluatorCode1)(text1))(function() {
                    return bind18(liftAff2(readSnippetFiles("MetaTreeHandleActions")))(function(text22) {
                      return discard111(assign4(_handlerCode12)(text22))(function() {
                        return bind18(liftEffect11(eval_D3M(removeExistingSVG2("div.d3story"))))(function(detached) {
                          return bind18(liftAff2(getTreeViaAJAX("./data/flare-2.json")))(function(treeJSON) {
                            return discard111((function() {
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
                            })())(function() {
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
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-about")])([h1([classes(["text-3xl", "p-2"])])([text("Meta and Printer Interpreters")]), div_(blurbtext2)]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked(toBoolean(view(_code13)(state3))), onChange(function(v) {
        return new ToggleCard3(function(dictStrong) {
          return _code3(dictStrong);
        });
      })])]), content_(view(_code13)(state3))(append11(syntaxHighlightedCode(view(_evaluatorCode2)(state3)))(append11(syntaxHighlightedCode(view(_drawCode22)(state3)))(syntaxHighlightedCode(view(_handlerCode22)(state3)))))]), div2([tailwindClass("svg-container")])([])]);
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
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction13(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        initialize: new Just(Initialize4.value)
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
  var Initialize5 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var ToggleCard4 = /* @__PURE__ */ (function() {
    function ToggleCard8(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard8.create = function(value0) {
      return new ToggleCard8(value0);
    };
    return ToggleCard8;
  })();
  var blurbtext3 = "Id sint laboris reprehenderit officia anim nisi consectetur voluptate enim.\n  Commodo cillum minim nisi laborum eiusmod veniam ullamco id ex fugiat eu anim.\n  Irure est aute laborum duis. Lorem dolore id sunt incididunt ut ea. Nostrud\n  enim officia nisi anim consequat cupidatat consectetur consequat ex excepteur.\n  Lorem nisi in reprehenderit ex adipisicing magna elit aute sunt. Cillum non\n  Lorem minim duis culpa ullamco aute ex minim. Mollit anim in nisi tempor enim\n  exercitation dolore. Veniam consequat minim nostrud amet duis dolore tempor\n  voluptate quis culpa. Laborum dolor pariatur ut est cupidatat elit deserunt\n  occaecat tempor aliquip anim. \n  \n  Velit irure ea voluptate ipsum ex exercitation\n  dolore voluptate reprehenderit sit anim sunt. Anim fugiat ad ut qui cillum\n  tempor occaecat et deserunt nostrud non ipsum. Id non qui mollit culpa elit\n  cillum ipsum excepteur adipisicing qui. Incididunt adipisicing sit incididunt\n  consequat minim id do exercitation cupidatat est sunt mollit. Anim ut ullamco\n  enim culpa. Adipisicing ad non esse laboris anim consequat ut velit esse\n  consequat tempor. Commodo magna esse ullamco ipsum et ipsum minim dolore esse\n  veniam ea commodo labore. Nulla deserunt id ad anim anim proident labore\n  occaecat sint esse nostrud. Duis velit nostrud ullamco cillum cillum Lorem\n  cupidatat irure.";
  var _snippets3 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  })();
  var _panels4 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  })();
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
              return bind18(liftAff2(readSnippetFiles("PrintTreeHandleActions")))(function(text12) {
                return discard111(assign4(_handlerCode13)(text12))(function() {
                  return bind18(liftAff2(getTreeViaAJAX("./data/flare-2.json")))(function(treeJSON) {
                    return discard111((function() {
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
                    })())(function() {
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
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-controls")])([text("Les Mis")]), div2([tailwindClass("story-panel-about")])([field_2({
        label: text("About"),
        helpText: [],
        error: [],
        inputId: "show-blurb"
      })([toggle([id2("show-blurb"), checked(toBoolean(view(_blurb12)(state3))), onChange(function(v) {
        return new ToggleCard4(function(dictStrong) {
          return _blurb2(dictStrong);
        });
      })])]), content_(view(_blurb12)(state3))([text(blurbtext3)])]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked(toBoolean(view(_code14)(state3))), onChange(function(v) {
        return new ToggleCard4(function(dictStrong) {
          return _code4(dictStrong);
        });
      })])]), content_(view(_code14)(state3))(syntaxHighlightedCode(view(_handlerCode23)(state3)))]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text("Output"),
        helpText: [],
        error: [],
        inputId: "show-print"
      })([toggle([id2("show-print"), checked(toBoolean(view(_print1)(state3))), onChange(function(v) {
        return new ToggleCard4(function(dictStrong) {
          return _print(dictStrong);
        });
      })])]), content_(view(_print1)(state3))([code_([text(state3.tree)])])])]);
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
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction14(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        initialize: new Just(Initialize5.value)
      })
    });
  };

  // output/D3.Examples.Sankey.Model/index.js
  var energyData = {
    nodes: [{
      name: "Agricultural 'waste'"
    }, {
      name: "Bio-conversion"
    }, {
      name: "Liquid"
    }, {
      name: "Losses"
    }, {
      name: "Solid"
    }, {
      name: "Gas"
    }, {
      name: "Biofuel imports"
    }, {
      name: "Biomass imports"
    }, {
      name: "Coal imports"
    }, {
      name: "Coal"
    }, {
      name: "Coal reserves"
    }, {
      name: "District heating"
    }, {
      name: "Industry"
    }, {
      name: "Heating and cooling - commercial"
    }, {
      name: "Heating and cooling - homes"
    }, {
      name: "Electricity grid"
    }, {
      name: "Over generation / exports"
    }, {
      name: "H2 conversion"
    }, {
      name: "Road transport"
    }, {
      name: "Agriculture"
    }, {
      name: "Rail transport"
    }, {
      name: "Lighting & appliances - commercial"
    }, {
      name: "Lighting & appliances - homes"
    }, {
      name: "Gas imports"
    }, {
      name: "Ngas"
    }, {
      name: "Gas reserves"
    }, {
      name: "Thermal generation"
    }, {
      name: "Geothermal"
    }, {
      name: "H2"
    }, {
      name: "Hydro"
    }, {
      name: "International shipping"
    }, {
      name: "Domestic aviation"
    }, {
      name: "International aviation"
    }, {
      name: "National navigation"
    }, {
      name: "Marine algae"
    }, {
      name: "Nuclear"
    }, {
      name: "Oil imports"
    }, {
      name: "Oil"
    }, {
      name: "Oil reserves"
    }, {
      name: "Other waste"
    }, {
      name: "Pumped heat"
    }, {
      name: "Solar PV"
    }, {
      name: "Solar Thermal"
    }, {
      name: "Solar"
    }, {
      name: "Tidal"
    }, {
      name: "UK land based bioenergy"
    }, {
      name: "Wave"
    }, {
      name: "Wind"
    }],
    links: [{
      source: 0,
      target: 1,
      value: 124.729
    }, {
      source: 1,
      target: 2,
      value: 0.597
    }, {
      source: 1,
      target: 3,
      value: 26.862
    }, {
      source: 1,
      target: 4,
      value: 280.322
    }, {
      source: 1,
      target: 5,
      value: 81.144
    }, {
      source: 6,
      target: 2,
      value: 35
    }, {
      source: 7,
      target: 4,
      value: 35
    }, {
      source: 8,
      target: 9,
      value: 11.606
    }, {
      source: 10,
      target: 9,
      value: 63.965
    }, {
      source: 9,
      target: 4,
      value: 75.571
    }, {
      source: 11,
      target: 12,
      value: 10.639
    }, {
      source: 11,
      target: 13,
      value: 22.505
    }, {
      source: 11,
      target: 14,
      value: 46.184
    }, {
      source: 15,
      target: 16,
      value: 104.453
    }, {
      source: 15,
      target: 14,
      value: 113.726
    }, {
      source: 15,
      target: 17,
      value: 27.14
    }, {
      source: 15,
      target: 12,
      value: 342.165
    }, {
      source: 15,
      target: 18,
      value: 37.797
    }, {
      source: 15,
      target: 19,
      value: 4.412
    }, {
      source: 15,
      target: 13,
      value: 40.858
    }, {
      source: 15,
      target: 3,
      value: 56.691
    }, {
      source: 15,
      target: 20,
      value: 7.863
    }, {
      source: 15,
      target: 21,
      value: 90.008
    }, {
      source: 15,
      target: 22,
      value: 93.494
    }, {
      source: 23,
      target: 24,
      value: 40.719
    }, {
      source: 25,
      target: 24,
      value: 82.233
    }, {
      source: 5,
      target: 13,
      value: 0.129
    }, {
      source: 5,
      target: 3,
      value: 1.401
    }, {
      source: 5,
      target: 26,
      value: 151.891
    }, {
      source: 5,
      target: 19,
      value: 2.096
    }, {
      source: 5,
      target: 12,
      value: 48.58
    }, {
      source: 27,
      target: 15,
      value: 7.013
    }, {
      source: 17,
      target: 28,
      value: 20.897
    }, {
      source: 17,
      target: 3,
      value: 6.242
    }, {
      source: 28,
      target: 18,
      value: 20.897
    }, {
      source: 29,
      target: 15,
      value: 6.995
    }, {
      source: 2,
      target: 12,
      value: 121.066
    }, {
      source: 2,
      target: 30,
      value: 128.69
    }, {
      source: 2,
      target: 18,
      value: 135.835
    }, {
      source: 2,
      target: 31,
      value: 14.458
    }, {
      source: 2,
      target: 32,
      value: 206.267
    }, {
      source: 2,
      target: 19,
      value: 3.64
    }, {
      source: 2,
      target: 33,
      value: 33.218
    }, {
      source: 2,
      target: 20,
      value: 4.413
    }, {
      source: 34,
      target: 1,
      value: 4.375
    }, {
      source: 24,
      target: 5,
      value: 122.952
    }, {
      source: 35,
      target: 26,
      value: 839.978
    }, {
      source: 36,
      target: 37,
      value: 504.287
    }, {
      source: 38,
      target: 37,
      value: 107.703
    }, {
      source: 37,
      target: 2,
      value: 611.99
    }, {
      source: 39,
      target: 4,
      value: 56.587
    }, {
      source: 39,
      target: 1,
      value: 77.81
    }, {
      source: 40,
      target: 14,
      value: 193.026
    }, {
      source: 40,
      target: 13,
      value: 70.672
    }, {
      source: 41,
      target: 15,
      value: 59.901
    }, {
      source: 42,
      target: 14,
      value: 19.263
    }, {
      source: 43,
      target: 42,
      value: 19.263
    }, {
      source: 43,
      target: 41,
      value: 59.901
    }, {
      source: 4,
      target: 19,
      value: 0.882
    }, {
      source: 4,
      target: 26,
      value: 400.12
    }, {
      source: 4,
      target: 12,
      value: 46.477
    }, {
      source: 26,
      target: 15,
      value: 525.531
    }, {
      source: 26,
      target: 3,
      value: 787.129
    }, {
      source: 26,
      target: 11,
      value: 79.329
    }, {
      source: 44,
      target: 15,
      value: 9.452
    }, {
      source: 45,
      target: 1,
      value: 182.01
    }, {
      source: 46,
      target: 15,
      value: 19.013
    }, {
      source: 47,
      target: 15,
      value: 289.366
    }]
  };

  // output/D3.Examples.Sankey.Unsafe/index.js
  var unboxSankeyNode = unsafeCoerce2;
  var unboxSankeyLink = unsafeCoerce2;

  // node_modules/internmap/src/index.js
  var InternMap = class extends Map {
    constructor(entries, key = keyof) {
      super();
      Object.defineProperties(this, { _intern: { value: /* @__PURE__ */ new Map() }, _key: { value: key } });
      if (entries != null) for (const [key2, value16] of entries) this.set(key2, value16);
    }
    get(key) {
      return super.get(intern_get(this, key));
    }
    has(key) {
      return super.has(intern_get(this, key));
    }
    set(key, value16) {
      return super.set(intern_set(this, key), value16);
    }
    delete(key) {
      return super.delete(intern_delete(this, key));
    }
  };
  function intern_get({ _intern, _key }, value16) {
    const key = _key(value16);
    return _intern.has(key) ? _intern.get(key) : value16;
  }
  function intern_set({ _intern, _key }, value16) {
    const key = _key(value16);
    if (_intern.has(key)) return _intern.get(key);
    _intern.set(key, value16);
    return value16;
  }
  function intern_delete({ _intern, _key }, value16) {
    const key = _key(value16);
    if (_intern.has(key)) {
      value16 = _intern.get(key);
      _intern.delete(key);
    }
    return value16;
  }
  function keyof(value16) {
    return value16 !== null && typeof value16 === "object" ? value16.valueOf() : value16;
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
      var name17 = "", i2 = t.indexOf(".");
      if (i2 >= 0) name17 = t.slice(i2 + 1), t = t.slice(0, i2);
      if (t && !types.hasOwnProperty(t)) throw new Error("unknown type: " + t);
      return { type: t, name: name17 };
    });
  }
  Dispatch.prototype = dispatch.prototype = {
    constructor: Dispatch,
    on: function(typename, callback) {
      var _ = this._, T = parseTypenames(typename + "", _), t, i2 = -1, n = T.length;
      if (arguments.length < 2) {
        while (++i2 < n) if ((t = (typename = T[i2]).type) && (t = get6(_[t], typename.name))) return t;
        return;
      }
      if (callback != null && typeof callback !== "function") throw new Error("invalid callback: " + callback);
      while (++i2 < n) {
        if (t = (typename = T[i2]).type) _[t] = set3(_[t], typename.name, callback);
        else if (callback == null) for (t in _) _[t] = set3(_[t], typename.name, null);
      }
      return this;
    },
    copy: function() {
      var copy2 = {}, _ = this._;
      for (var t in _) copy2[t] = _[t].slice();
      return new Dispatch(copy2);
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
  function get6(type2, name17) {
    for (var i2 = 0, n = type2.length, c; i2 < n; ++i2) {
      if ((c = type2[i2]).name === name17) {
        return c.value;
      }
    }
  }
  function set3(type2, name17, callback) {
    for (var i2 = 0, n = type2.length; i2 < n; ++i2) {
      if (type2[i2].name === name17) {
        type2[i2] = noop, type2 = type2.slice(0, i2).concat(type2.slice(i2 + 1));
        break;
      }
    }
    if (callback != null) type2.push({ name: name17, value: callback });
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
  function namespace_default(name17) {
    var prefix = name17 += "", i2 = prefix.indexOf(":");
    if (i2 >= 0 && (prefix = name17.slice(0, i2)) !== "xmlns") name17 = name17.slice(i2 + 1);
    return namespaces_default.hasOwnProperty(prefix) ? { space: namespaces_default[prefix], local: name17 } : name17;
  }

  // node_modules/d3-selection/src/creator.js
  function creatorInherit(name17) {
    return function() {
      var document3 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml && document3.documentElement.namespaceURI === xhtml ? document3.createElement(name17) : document3.createElementNS(uri, name17);
    };
  }
  function creatorFixed(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default(name17) {
    var fullname = namespace_default(name17);
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
  function array(x15) {
    return x15 == null ? [] : Array.isArray(x15) ? x15 : Array.from(x15);
  }

  // node_modules/d3-selection/src/selectorAll.js
  function empty8() {
    return [];
  }
  function selectorAll_default(selector) {
    return selector == null ? empty8 : function() {
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
  var filter5 = Array.prototype.filter;
  function children2() {
    return Array.from(this.children);
  }
  function childrenFilter(match) {
    return function() {
      return filter5.call(this.children, match);
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
  function sparse_default(update3) {
    return new Array(update3.length);
  }

  // node_modules/d3-selection/src/selection/enter.js
  function enter_default() {
    return new Selection(this._enter || this._groups.map(sparse_default), this._parents);
  }
  function EnterNode(parent3, datum2) {
    this.ownerDocument = parent3.ownerDocument;
    this.namespaceURI = parent3.namespaceURI;
    this._next = null;
    this._parent = parent3;
    this.__data__ = datum2;
  }
  EnterNode.prototype = {
    constructor: EnterNode,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next2) {
      return this._parent.insertBefore(child, next2);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-selection/src/constant.js
  function constant_default(x15) {
    return function() {
      return x15;
    };
  }

  // node_modules/d3-selection/src/selection/data.js
  function bindIndex(parent3, group3, enter, update3, exit, data) {
    var i2 = 0, node, groupLength = group3.length, dataLength = data.length;
    for (; i2 < dataLength; ++i2) {
      if (node = group3[i2]) {
        node.__data__ = data[i2];
        update3[i2] = node;
      } else {
        enter[i2] = new EnterNode(parent3, data[i2]);
      }
    }
    for (; i2 < groupLength; ++i2) {
      if (node = group3[i2]) {
        exit[i2] = node;
      }
    }
  }
  function bindKey(parent3, group3, enter, update3, exit, data, key) {
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
      keyValue = key.call(parent3, data[i2], i2, data) + "";
      if (node = nodeByKeyValue.get(keyValue)) {
        update3[i2] = node;
        node.__data__ = data[i2];
        nodeByKeyValue.delete(keyValue);
      } else {
        enter[i2] = new EnterNode(parent3, data[i2]);
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
  function data_default(value16, key) {
    if (!arguments.length) return Array.from(this, datum);
    var bind18 = key ? bindKey : bindIndex, parents = this._parents, groups = this._groups;
    if (typeof value16 !== "function") value16 = constant_default(value16);
    for (var m = groups.length, update3 = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
      var parent3 = parents[j], group3 = groups[j], groupLength = group3.length, data = arraylike(value16.call(parent3, parent3 && parent3.__data__, j, parents)), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update3[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind18(parent3, group3, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next2; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1) i1 = i0 + 1;
          while (!(next2 = updateGroup[i1]) && ++i1 < dataLength) ;
          previous._next = next2 || null;
        }
      }
    }
    update3 = new Selection(update3, parents);
    update3._enter = enter;
    update3._exit = exit;
    return update3;
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
    var enter = this.enter(), update3 = this, exit = this.exit();
    if (typeof onenter === "function") {
      enter = onenter(enter);
      if (enter) enter = enter.selection();
    } else {
      enter = enter.append(onenter + "");
    }
    if (onupdate != null) {
      update3 = onupdate(update3);
      if (update3) update3 = update3.selection();
    }
    if (onexit == null) exit.remove();
    else onexit(exit);
    return enter && update3 ? enter.merge(update3).order() : update3;
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
      for (var group3 = groups[j], i2 = group3.length - 1, next2 = group3[i2], node; --i2 >= 0; ) {
        if (node = group3[i2]) {
          if (next2 && node.compareDocumentPosition(next2) ^ 4) next2.parentNode.insertBefore(node, next2);
          next2 = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/sort.js
  function sort_default(compare2) {
    if (!compare2) compare2 = ascending;
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
  function ascending(a2, b2) {
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
    let size5 = 0;
    for (const node of this) ++size5;
    return size5;
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
  function attrRemove(name17) {
    return function() {
      this.removeAttribute(name17);
    };
  }
  function attrRemoveNS(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant(name17, value16) {
    return function() {
      this.setAttribute(name17, value16);
    };
  }
  function attrConstantNS(fullname, value16) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value16);
    };
  }
  function attrFunction(name17, value16) {
    return function() {
      var v = value16.apply(this, arguments);
      if (v == null) this.removeAttribute(name17);
      else this.setAttribute(name17, v);
    };
  }
  function attrFunctionNS(fullname, value16) {
    return function() {
      var v = value16.apply(this, arguments);
      if (v == null) this.removeAttributeNS(fullname.space, fullname.local);
      else this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default(name17, value16) {
    var fullname = namespace_default(name17);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value16 == null ? fullname.local ? attrRemoveNS : attrRemove : typeof value16 === "function" ? fullname.local ? attrFunctionNS : attrFunction : fullname.local ? attrConstantNS : attrConstant)(fullname, value16));
  }

  // node_modules/d3-selection/src/window.js
  function window_default(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-selection/src/selection/style.js
  function styleRemove(name17) {
    return function() {
      this.style.removeProperty(name17);
    };
  }
  function styleConstant(name17, value16, priority) {
    return function() {
      this.style.setProperty(name17, value16, priority);
    };
  }
  function styleFunction(name17, value16, priority) {
    return function() {
      var v = value16.apply(this, arguments);
      if (v == null) this.style.removeProperty(name17);
      else this.style.setProperty(name17, v, priority);
    };
  }
  function style_default(name17, value16, priority) {
    return arguments.length > 1 ? this.each((value16 == null ? styleRemove : typeof value16 === "function" ? styleFunction : styleConstant)(name17, value16, priority == null ? "" : priority)) : styleValue(this.node(), name17);
  }
  function styleValue(node, name17) {
    return node.style.getPropertyValue(name17) || window_default(node).getComputedStyle(node, null).getPropertyValue(name17);
  }

  // node_modules/d3-selection/src/selection/property.js
  function propertyRemove(name17) {
    return function() {
      delete this[name17];
    };
  }
  function propertyConstant(name17, value16) {
    return function() {
      this[name17] = value16;
    };
  }
  function propertyFunction(name17, value16) {
    return function() {
      var v = value16.apply(this, arguments);
      if (v == null) delete this[name17];
      else this[name17] = v;
    };
  }
  function property_default(name17, value16) {
    return arguments.length > 1 ? this.each((value16 == null ? propertyRemove : typeof value16 === "function" ? propertyFunction : propertyConstant)(name17, value16)) : this.node()[name17];
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
    add: function(name17) {
      var i2 = this._names.indexOf(name17);
      if (i2 < 0) {
        this._names.push(name17);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name17) {
      var i2 = this._names.indexOf(name17);
      if (i2 >= 0) {
        this._names.splice(i2, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name17) {
      return this._names.indexOf(name17) >= 0;
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
  function classedFunction(names, value16) {
    return function() {
      (value16.apply(this, arguments) ? classedAdd : classedRemove)(this, names);
    };
  }
  function classed_default(name17, value16) {
    var names = classArray(name17 + "");
    if (arguments.length < 2) {
      var list = classList2(this.node()), i2 = -1, n = names.length;
      while (++i2 < n) if (!list.contains(names[i2])) return false;
      return true;
    }
    return this.each((typeof value16 === "function" ? classedFunction : value16 ? classedTrue : classedFalse)(names, value16));
  }

  // node_modules/d3-selection/src/selection/text.js
  function textRemove() {
    this.textContent = "";
  }
  function textConstant(value16) {
    return function() {
      this.textContent = value16;
    };
  }
  function textFunction(value16) {
    return function() {
      var v = value16.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default(value16) {
    return arguments.length ? this.each(value16 == null ? textRemove : (typeof value16 === "function" ? textFunction : textConstant)(value16)) : this.node().textContent;
  }

  // node_modules/d3-selection/src/selection/html.js
  function htmlRemove() {
    this.innerHTML = "";
  }
  function htmlConstant(value16) {
    return function() {
      this.innerHTML = value16;
    };
  }
  function htmlFunction(value16) {
    return function() {
      var v = value16.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default(value16) {
    return arguments.length ? this.each(value16 == null ? htmlRemove : (typeof value16 === "function" ? htmlFunction : htmlConstant)(value16)) : this.node().innerHTML;
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
  function append_default(name17) {
    var create5 = typeof name17 === "function" ? name17 : creator_default(name17);
    return this.select(function() {
      return this.appendChild(create5.apply(this, arguments));
    });
  }

  // node_modules/d3-selection/src/selection/insert.js
  function constantNull() {
    return null;
  }
  function insert_default(name17, before) {
    var create5 = typeof name17 === "function" ? name17 : creator_default(name17), select5 = before == null ? constantNull : typeof before === "function" ? before : selector_default(before);
    return this.select(function() {
      return this.insertBefore(create5.apply(this, arguments), select5.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-selection/src/selection/remove.js
  function remove2() {
    var parent3 = this.parentNode;
    if (parent3) parent3.removeChild(this);
  }
  function remove_default() {
    return this.each(remove2);
  }

  // node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow() {
    var clone2 = this.cloneNode(false), parent3 = this.parentNode;
    return parent3 ? parent3.insertBefore(clone2, this.nextSibling) : clone2;
  }
  function selection_cloneDeep() {
    var clone2 = this.cloneNode(true), parent3 = this.parentNode;
    return parent3 ? parent3.insertBefore(clone2, this.nextSibling) : clone2;
  }
  function clone_default(deep) {
    return this.select(deep ? selection_cloneDeep : selection_cloneShallow);
  }

  // node_modules/d3-selection/src/selection/datum.js
  function datum_default(value16) {
    return arguments.length ? this.property("__data__", value16) : this.node().__data__;
  }

  // node_modules/d3-selection/src/selection/on.js
  function contextListener(listener) {
    return function(event) {
      listener.call(this, event, this.__data__);
    };
  }
  function parseTypenames2(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name17 = "", i2 = t.indexOf(".");
      if (i2 >= 0) name17 = t.slice(i2 + 1), t = t.slice(0, i2);
      return { type: t, name: name17 };
    });
  }
  function onRemove(typename) {
    return function() {
      var on3 = this.__on;
      if (!on3) return;
      for (var j = 0, i2 = -1, m = on3.length, o; j < m; ++j) {
        if (o = on3[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
        } else {
          on3[++i2] = o;
        }
      }
      if (++i2) on3.length = i2;
      else delete this.__on;
    };
  }
  function onAdd(typename, value16, options2) {
    return function() {
      var on3 = this.__on, o, listener = contextListener(value16);
      if (on3) for (var j = 0, m = on3.length; j < m; ++j) {
        if ((o = on3[j]).type === typename.type && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
          this.addEventListener(o.type, o.listener = listener, o.options = options2);
          o.value = value16;
          return;
        }
      }
      this.addEventListener(typename.type, listener, options2);
      o = { type: typename.type, name: typename.name, value: value16, listener, options: options2 };
      if (!on3) this.__on = [o];
      else on3.push(o);
    };
  }
  function on_default(typename, value16, options2) {
    var typenames = parseTypenames2(typename + ""), i2, n = typenames.length, t;
    if (arguments.length < 2) {
      var on3 = this.node().__on;
      if (on3) for (var j = 0, m = on3.length, o; j < m; ++j) {
        for (i2 = 0, o = on3[j]; i2 < n; ++i2) {
          if ((t = typenames[i2]).type === o.type && t.name === o.name) {
            return o.value;
          }
        }
      }
      return;
    }
    on3 = value16 ? onAdd : onRemove;
    for (i2 = 0; i2 < n; ++i2) this.each(on3(typenames[i2], value16, options2));
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
  function extend2(parent3, definition) {
    var prototype = Object.create(parent3.prototype);
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
  function color(format) {
    var m, l;
    format = (format + "").trim().toLowerCase();
    return (m = reHex.exec(format)) ? (l = m[1].length, m = parseInt(m[1], 16), l === 6 ? rgbn(m) : l === 3 ? new Rgb(m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, (m & 15) << 4 | m & 15, 1) : l === 8 ? rgba(m >> 24 & 255, m >> 16 & 255, m >> 8 & 255, (m & 255) / 255) : l === 4 ? rgba(m >> 12 & 15 | m >> 8 & 240, m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, ((m & 15) << 4 | m & 15) / 255) : null) : (m = reRgbInteger.exec(format)) ? new Rgb(m[1], m[2], m[3], 1) : (m = reRgbPercent.exec(format)) ? new Rgb(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, 1) : (m = reRgbaInteger.exec(format)) ? rgba(m[1], m[2], m[3], m[4]) : (m = reRgbaPercent.exec(format)) ? rgba(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, m[4]) : (m = reHslPercent.exec(format)) ? hsla(m[1], m[2] / 100, m[3] / 100, 1) : (m = reHslaPercent.exec(format)) ? hsla(m[1], m[2] / 100, m[3] / 100, m[4]) : named.hasOwnProperty(format) ? rgbn(named[format]) : format === "transparent" ? new Rgb(NaN, NaN, NaN, 0) : null;
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
  function rgb(r, g, b2, opacity3) {
    return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b2, opacity3 == null ? 1 : opacity3);
  }
  function Rgb(r, g, b2, opacity3) {
    this.r = +r;
    this.g = +g;
    this.b = +b2;
    this.opacity = +opacity3;
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
  function clampa(opacity3) {
    return isNaN(opacity3) ? 1 : Math.max(0, Math.min(1, opacity3));
  }
  function clampi(value16) {
    return Math.max(0, Math.min(255, Math.round(value16) || 0));
  }
  function hex(value16) {
    value16 = clampi(value16);
    return (value16 < 16 ? "0" : "") + value16.toString(16);
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
    var r = o.r / 255, g = o.g / 255, b2 = o.b / 255, min8 = Math.min(r, g, b2), max9 = Math.max(r, g, b2), h = NaN, s = max9 - min8, l = (max9 + min8) / 2;
    if (s) {
      if (r === max9) h = (g - b2) / s + (g < b2) * 6;
      else if (g === max9) h = (b2 - r) / s + 2;
      else h = (r - g) / s + 4;
      s /= l < 0.5 ? max9 + min8 : 2 - max9 - min8;
      h *= 60;
    } else {
      s = l > 0 && l < 1 ? 0 : h;
    }
    return new Hsl(h, s, l, o.opacity);
  }
  function hsl(h, s, l, opacity3) {
    return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity3 == null ? 1 : opacity3);
  }
  function Hsl(h, s, l, opacity3) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity3;
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
  function clamph(value16) {
    value16 = (value16 || 0) % 360;
    return value16 < 0 ? value16 + 360 : value16;
  }
  function clampt(value16) {
    return Math.max(0, Math.min(1, value16 || 0));
  }
  function hsl2rgb(h, m1, m2) {
    return (h < 60 ? m1 + (m2 - m1) * h / 60 : h < 180 ? m2 : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60 : m1) * 255;
  }

  // node_modules/d3-interpolate/src/basis.js
  function basis(t1, v0, v1, v2, v3) {
    var t2 = t1 * t1, t3 = t2 * t1;
    return ((1 - 3 * t1 + 3 * t2 - t3) * v0 + (4 - 6 * t2 + 3 * t3) * v1 + (1 + 3 * t1 + 3 * t2 - 3 * t3) * v2 + t3 * v3) / 6;
  }
  function basis_default(values2) {
    var n = values2.length - 1;
    return function(t) {
      var i2 = t <= 0 ? t = 0 : t >= 1 ? (t = 1, n - 1) : Math.floor(t * n), v1 = values2[i2], v2 = values2[i2 + 1], v0 = i2 > 0 ? values2[i2 - 1] : 2 * v1 - v2, v3 = i2 < n - 1 ? values2[i2 + 2] : 2 * v2 - v1;
      return basis((t - i2 / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/basisClosed.js
  function basisClosed_default(values2) {
    var n = values2.length;
    return function(t) {
      var i2 = Math.floor(((t %= 1) < 0 ? ++t : t) * n), v0 = values2[(i2 + n - 1) % n], v1 = values2[i2 % n], v2 = values2[(i2 + 1) % n], v3 = values2[(i2 + 2) % n];
      return basis((t - i2 / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/constant.js
  var constant_default2 = (x15) => () => x15;

  // node_modules/d3-interpolate/src/color.js
  function linear(a2, d5) {
    return function(t) {
      return a2 + t * d5;
    };
  }
  function exponential(a2, b2, y10) {
    return a2 = Math.pow(a2, y10), b2 = Math.pow(b2, y10) - a2, y10 = 1 / y10, function(t) {
      return Math.pow(a2 + t * b2, y10);
    };
  }
  function gamma(y10) {
    return (y10 = +y10) === 1 ? nogamma : function(a2, b2) {
      return b2 - a2 ? exponential(a2, b2, y10) : constant_default2(isNaN(a2) ? b2 : a2);
    };
  }
  function nogamma(a2, b2) {
    var d5 = b2 - a2;
    return d5 ? linear(a2, d5) : constant_default2(isNaN(a2) ? b2 : a2);
  }

  // node_modules/d3-interpolate/src/rgb.js
  var rgb_default = (function rgbGamma(y10) {
    var color2 = gamma(y10);
    function rgb2(start6, end) {
      var r = color2((start6 = rgb(start6)).r, (end = rgb(end)).r), g = color2(start6.g, end.g), b2 = color2(start6.b, end.b), opacity3 = nogamma(start6.opacity, end.opacity);
      return function(t) {
        start6.r = r(t);
        start6.g = g(t);
        start6.b = b2(t);
        start6.opacity = opacity3(t);
        return start6 + "";
      };
    }
    rgb2.gamma = rgbGamma;
    return rgb2;
  })(1);
  function rgbSpline(spline) {
    return function(colors) {
      var n = colors.length, r = new Array(n), g = new Array(n), b2 = new Array(n), i2, color2;
      for (i2 = 0; i2 < n; ++i2) {
        color2 = rgb(colors[i2]);
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

  // node_modules/d3-interpolate/src/number.js
  function number_default(a2, b2) {
    return a2 = +a2, b2 = +b2, function(t) {
      return a2 * (1 - t) + b2 * t;
    };
  }

  // node_modules/d3-interpolate/src/string.js
  var reA = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g;
  var reB = new RegExp(reA.source, "g");
  function zero2(b2) {
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
    return s.length < 2 ? q2[0] ? one2(q2[0].x) : zero2(b2) : (b2 = q2.length, function(t) {
      for (var i3 = 0, o; i3 < b2; ++i3) s[(o = q2[i3]).i] = o.x(t);
      return s.join("");
    });
  }

  // node_modules/d3-interpolate/src/transform/decompose.js
  var degrees = 180 / Math.PI;
  var identity16 = {
    translateX: 0,
    translateY: 0,
    rotate: 0,
    skewX: 0,
    scaleX: 1,
    scaleY: 1
  };
  function decompose_default(a2, b2, c, d5, e, f) {
    var scaleX, scaleY, skewX;
    if (scaleX = Math.sqrt(a2 * a2 + b2 * b2)) a2 /= scaleX, b2 /= scaleX;
    if (skewX = a2 * c + b2 * d5) c -= a2 * skewX, d5 -= b2 * skewX;
    if (scaleY = Math.sqrt(c * c + d5 * d5)) c /= scaleY, d5 /= scaleY, skewX /= scaleY;
    if (a2 * d5 < b2 * c) a2 = -a2, b2 = -b2, skewX = -skewX, scaleX = -scaleX;
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
  function parseCss(value16) {
    const m = new (typeof DOMMatrix === "function" ? DOMMatrix : WebKitCSSMatrix)(value16 + "");
    return m.isIdentity ? identity16 : decompose_default(m.a, m.b, m.c, m.d, m.e, m.f);
  }
  function parseSvg(value16) {
    if (value16 == null) return identity16;
    if (!svgNode) svgNode = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svgNode.setAttribute("transform", value16);
    if (!(value16 = svgNode.transform.baseVal.consolidate())) return identity16;
    value16 = value16.matrix;
    return decompose_default(value16.a, value16.b, value16.c, value16.d, value16.e, value16.f);
  }

  // node_modules/d3-interpolate/src/transform/index.js
  function interpolateTransform(parse7, pxComma, pxParen, degParen) {
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
      a2 = parse7(a2), b2 = parse7(b2);
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
  function now2() {
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
      time3 = (time3 == null ? now2() : +time3) + (delay2 == null ? 0 : +delay2);
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
    now2();
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
  function schedule_default(node, name17, id5, index5, group3, timing) {
    var schedules = node.__transition;
    if (!schedules) node.__transition = {};
    else if (id5 in schedules) return;
    create4(node, id5, {
      name: name17,
      index: index5,
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
  function init3(node, id5) {
    var schedule = get7(node, id5);
    if (schedule.state > CREATED) throw new Error("too late; already scheduled");
    return schedule;
  }
  function set4(node, id5) {
    var schedule = get7(node, id5);
    if (schedule.state > STARTED) throw new Error("too late; already running");
    return schedule;
  }
  function get7(node, id5) {
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
      self.timer.restart(start6, self.delay, self.time);
      if (self.delay <= elapsed) start6(elapsed - self.delay);
    }
    function start6(elapsed) {
      var i2, j, n, o;
      if (self.state !== SCHEDULED) return stop3();
      for (i2 in schedules) {
        o = schedules[i2];
        if (o.name !== self.name) continue;
        if (o.state === STARTED) return timeout_default(start6);
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
      var t = elapsed < self.duration ? self.ease.call(null, elapsed / self.duration) : (self.timer.restart(stop3), self.state = ENDING, 1), i2 = -1, n = tween.length;
      while (++i2 < n) {
        tween[i2].call(node, t);
      }
      if (self.state === ENDING) {
        self.on.call("end", node, node.__data__, self.index, self.group);
        stop3();
      }
    }
    function stop3() {
      self.state = ENDED;
      self.timer.stop();
      delete schedules[id5];
      for (var i2 in schedules) return;
      delete node.__transition;
    }
  }

  // node_modules/d3-transition/src/interrupt.js
  function interrupt_default(node, name17) {
    var schedules = node.__transition, schedule, active, empty9 = true, i2;
    if (!schedules) return;
    name17 = name17 == null ? null : name17 + "";
    for (i2 in schedules) {
      if ((schedule = schedules[i2]).name !== name17) {
        empty9 = false;
        continue;
      }
      active = schedule.state > STARTING && schedule.state < ENDING;
      schedule.state = ENDED;
      schedule.timer.stop();
      schedule.on.call(active ? "interrupt" : "cancel", node, node.__data__, schedule.index, schedule.group);
      delete schedules[i2];
    }
    if (empty9) delete node.__transition;
  }

  // node_modules/d3-transition/src/selection/interrupt.js
  function interrupt_default2(name17) {
    return this.each(function() {
      interrupt_default(this, name17);
    });
  }

  // node_modules/d3-transition/src/transition/tween.js
  function tweenRemove(id5, name17) {
    var tween0, tween1;
    return function() {
      var schedule = set4(this, id5), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = tween0 = tween;
        for (var i2 = 0, n = tween1.length; i2 < n; ++i2) {
          if (tween1[i2].name === name17) {
            tween1 = tween1.slice();
            tween1.splice(i2, 1);
            break;
          }
        }
      }
      schedule.tween = tween1;
    };
  }
  function tweenFunction(id5, name17, value16) {
    var tween0, tween1;
    if (typeof value16 !== "function") throw new Error();
    return function() {
      var schedule = set4(this, id5), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = (tween0 = tween).slice();
        for (var t = { name: name17, value: value16 }, i2 = 0, n = tween1.length; i2 < n; ++i2) {
          if (tween1[i2].name === name17) {
            tween1[i2] = t;
            break;
          }
        }
        if (i2 === n) tween1.push(t);
      }
      schedule.tween = tween1;
    };
  }
  function tween_default(name17, value16) {
    var id5 = this._id;
    name17 += "";
    if (arguments.length < 2) {
      var tween = get7(this.node(), id5).tween;
      for (var i2 = 0, n = tween.length, t; i2 < n; ++i2) {
        if ((t = tween[i2]).name === name17) {
          return t.value;
        }
      }
      return null;
    }
    return this.each((value16 == null ? tweenRemove : tweenFunction)(id5, name17, value16));
  }
  function tweenValue(transition2, name17, value16) {
    var id5 = transition2._id;
    transition2.each(function() {
      var schedule = set4(this, id5);
      (schedule.value || (schedule.value = {}))[name17] = value16.apply(this, arguments);
    });
    return function(node) {
      return get7(node, id5).value[name17];
    };
  }

  // node_modules/d3-transition/src/transition/interpolate.js
  function interpolate_default(a2, b2) {
    var c;
    return (typeof b2 === "number" ? number_default : b2 instanceof color ? rgb_default : (c = color(b2)) ? (b2 = c, rgb_default) : string_default)(a2, b2);
  }

  // node_modules/d3-transition/src/transition/attr.js
  function attrRemove2(name17) {
    return function() {
      this.removeAttribute(name17);
    };
  }
  function attrRemoveNS2(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant2(name17, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttribute(name17);
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
  function attrFunction2(name17, interpolate, value16) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value16(this), string1;
      if (value1 == null) return void this.removeAttribute(name17);
      string0 = this.getAttribute(name17);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attrFunctionNS2(fullname, interpolate, value16) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value16(this), string1;
      if (value1 == null) return void this.removeAttributeNS(fullname.space, fullname.local);
      string0 = this.getAttributeNS(fullname.space, fullname.local);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attr_default2(name17, value16) {
    var fullname = namespace_default(name17), i2 = fullname === "transform" ? interpolateTransformSvg : interpolate_default;
    return this.attrTween(name17, typeof value16 === "function" ? (fullname.local ? attrFunctionNS2 : attrFunction2)(fullname, i2, tweenValue(this, "attr." + name17, value16)) : value16 == null ? (fullname.local ? attrRemoveNS2 : attrRemove2)(fullname) : (fullname.local ? attrConstantNS2 : attrConstant2)(fullname, i2, value16));
  }

  // node_modules/d3-transition/src/transition/attrTween.js
  function attrInterpolate(name17, i2) {
    return function(t) {
      this.setAttribute(name17, i2.call(this, t));
    };
  }
  function attrInterpolateNS(fullname, i2) {
    return function(t) {
      this.setAttributeNS(fullname.space, fullname.local, i2.call(this, t));
    };
  }
  function attrTweenNS(fullname, value16) {
    var t0, i0;
    function tween() {
      var i2 = value16.apply(this, arguments);
      if (i2 !== i0) t0 = (i0 = i2) && attrInterpolateNS(fullname, i2);
      return t0;
    }
    tween._value = value16;
    return tween;
  }
  function attrTween(name17, value16) {
    var t0, i0;
    function tween() {
      var i2 = value16.apply(this, arguments);
      if (i2 !== i0) t0 = (i0 = i2) && attrInterpolate(name17, i2);
      return t0;
    }
    tween._value = value16;
    return tween;
  }
  function attrTween_default(name17, value16) {
    var key = "attr." + name17;
    if (arguments.length < 2) return (key = this.tween(key)) && key._value;
    if (value16 == null) return this.tween(key, null);
    if (typeof value16 !== "function") throw new Error();
    var fullname = namespace_default(name17);
    return this.tween(key, (fullname.local ? attrTweenNS : attrTween)(fullname, value16));
  }

  // node_modules/d3-transition/src/transition/delay.js
  function delayFunction(id5, value16) {
    return function() {
      init3(this, id5).delay = +value16.apply(this, arguments);
    };
  }
  function delayConstant(id5, value16) {
    return value16 = +value16, function() {
      init3(this, id5).delay = value16;
    };
  }
  function delay_default(value16) {
    var id5 = this._id;
    return arguments.length ? this.each((typeof value16 === "function" ? delayFunction : delayConstant)(id5, value16)) : get7(this.node(), id5).delay;
  }

  // node_modules/d3-transition/src/transition/duration.js
  function durationFunction(id5, value16) {
    return function() {
      set4(this, id5).duration = +value16.apply(this, arguments);
    };
  }
  function durationConstant(id5, value16) {
    return value16 = +value16, function() {
      set4(this, id5).duration = value16;
    };
  }
  function duration_default(value16) {
    var id5 = this._id;
    return arguments.length ? this.each((typeof value16 === "function" ? durationFunction : durationConstant)(id5, value16)) : get7(this.node(), id5).duration;
  }

  // node_modules/d3-transition/src/transition/ease.js
  function easeConstant(id5, value16) {
    if (typeof value16 !== "function") throw new Error();
    return function() {
      set4(this, id5).ease = value16;
    };
  }
  function ease_default(value16) {
    var id5 = this._id;
    return arguments.length ? this.each(easeConstant(id5, value16)) : get7(this.node(), id5).ease;
  }

  // node_modules/d3-transition/src/transition/easeVarying.js
  function easeVarying(id5, value16) {
    return function() {
      var v = value16.apply(this, arguments);
      if (typeof v !== "function") throw new Error();
      set4(this, id5).ease = v;
    };
  }
  function easeVarying_default(value16) {
    if (typeof value16 !== "function") throw new Error();
    return this.each(easeVarying(this._id, value16));
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
  function start4(name17) {
    return (name17 + "").trim().split(/^|\s+/).every(function(t) {
      var i2 = t.indexOf(".");
      if (i2 >= 0) t = t.slice(0, i2);
      return !t || t === "start";
    });
  }
  function onFunction(id5, name17, listener) {
    var on0, on1, sit = start4(name17) ? init3 : set4;
    return function() {
      var schedule = sit(this, id5), on3 = schedule.on;
      if (on3 !== on0) (on1 = (on0 = on3).copy()).on(name17, listener);
      schedule.on = on1;
    };
  }
  function on_default2(name17, listener) {
    var id5 = this._id;
    return arguments.length < 2 ? get7(this.node(), id5).on.on(name17) : this.each(onFunction(id5, name17, listener));
  }

  // node_modules/d3-transition/src/transition/remove.js
  function removeFunction(id5) {
    return function() {
      var parent3 = this.parentNode;
      for (var i2 in this.__transition) if (+i2 !== id5) return;
      if (parent3) parent3.removeChild(this);
    };
  }
  function remove_default2() {
    return this.on("end.remove", removeFunction(this._id));
  }

  // node_modules/d3-transition/src/transition/select.js
  function select_default2(select5) {
    var name17 = this._name, id5 = this._id;
    if (typeof select5 !== "function") select5 = selector_default(select5);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = new Array(n), node, subnode, i2 = 0; i2 < n; ++i2) {
        if ((node = group3[i2]) && (subnode = select5.call(node, node.__data__, i2, group3))) {
          if ("__data__" in node) subnode.__data__ = node.__data__;
          subgroup[i2] = subnode;
          schedule_default(subgroup[i2], name17, id5, i2, subgroup, get7(node, id5));
        }
      }
    }
    return new Transition(subgroups, this._parents, name17, id5);
  }

  // node_modules/d3-transition/src/transition/selectAll.js
  function selectAll_default2(select5) {
    var name17 = this._name, id5 = this._id;
    if (typeof select5 !== "function") select5 = selectorAll_default(select5);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          for (var children3 = select5.call(node, node.__data__, i2, group3), child, inherit2 = get7(node, id5), k = 0, l = children3.length; k < l; ++k) {
            if (child = children3[k]) {
              schedule_default(child, name17, id5, k, children3, inherit2);
            }
          }
          subgroups.push(children3);
          parents.push(node);
        }
      }
    }
    return new Transition(subgroups, parents, name17, id5);
  }

  // node_modules/d3-transition/src/transition/selection.js
  var Selection2 = selection_default.prototype.constructor;
  function selection_default2() {
    return new Selection2(this._groups, this._parents);
  }

  // node_modules/d3-transition/src/transition/style.js
  function styleNull(name17, interpolate) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name17), string1 = (this.style.removeProperty(name17), styleValue(this, name17));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : interpolate0 = interpolate(string00 = string0, string10 = string1);
    };
  }
  function styleRemove2(name17) {
    return function() {
      this.style.removeProperty(name17);
    };
  }
  function styleConstant2(name17, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = styleValue(this, name17);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function styleFunction2(name17, interpolate, value16) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name17), value1 = value16(this), string1 = value1 + "";
      if (value1 == null) string1 = value1 = (this.style.removeProperty(name17), styleValue(this, name17));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function styleMaybeRemove(id5, name17) {
    var on0, on1, listener0, key = "style." + name17, event = "end." + key, remove3;
    return function() {
      var schedule = set4(this, id5), on3 = schedule.on, listener = schedule.value[key] == null ? remove3 || (remove3 = styleRemove2(name17)) : void 0;
      if (on3 !== on0 || listener0 !== listener) (on1 = (on0 = on3).copy()).on(event, listener0 = listener);
      schedule.on = on1;
    };
  }
  function style_default2(name17, value16, priority) {
    var i2 = (name17 += "") === "transform" ? interpolateTransformCss : interpolate_default;
    return value16 == null ? this.styleTween(name17, styleNull(name17, i2)).on("end.style." + name17, styleRemove2(name17)) : typeof value16 === "function" ? this.styleTween(name17, styleFunction2(name17, i2, tweenValue(this, "style." + name17, value16))).each(styleMaybeRemove(this._id, name17)) : this.styleTween(name17, styleConstant2(name17, i2, value16), priority).on("end.style." + name17, null);
  }

  // node_modules/d3-transition/src/transition/styleTween.js
  function styleInterpolate(name17, i2, priority) {
    return function(t) {
      this.style.setProperty(name17, i2.call(this, t), priority);
    };
  }
  function styleTween(name17, value16, priority) {
    var t, i0;
    function tween() {
      var i2 = value16.apply(this, arguments);
      if (i2 !== i0) t = (i0 = i2) && styleInterpolate(name17, i2, priority);
      return t;
    }
    tween._value = value16;
    return tween;
  }
  function styleTween_default(name17, value16, priority) {
    var key = "style." + (name17 += "");
    if (arguments.length < 2) return (key = this.tween(key)) && key._value;
    if (value16 == null) return this.tween(key, null);
    if (typeof value16 !== "function") throw new Error();
    return this.tween(key, styleTween(name17, value16, priority == null ? "" : priority));
  }

  // node_modules/d3-transition/src/transition/text.js
  function textConstant2(value16) {
    return function() {
      this.textContent = value16;
    };
  }
  function textFunction2(value16) {
    return function() {
      var value1 = value16(this);
      this.textContent = value1 == null ? "" : value1;
    };
  }
  function text_default2(value16) {
    return this.tween("text", typeof value16 === "function" ? textFunction2(tweenValue(this, "text", value16)) : textConstant2(value16 == null ? "" : value16 + ""));
  }

  // node_modules/d3-transition/src/transition/textTween.js
  function textInterpolate(i2) {
    return function(t) {
      this.textContent = i2.call(this, t);
    };
  }
  function textTween(value16) {
    var t0, i0;
    function tween() {
      var i2 = value16.apply(this, arguments);
      if (i2 !== i0) t0 = (i0 = i2) && textInterpolate(i2);
      return t0;
    }
    tween._value = value16;
    return tween;
  }
  function textTween_default(value16) {
    var key = "text";
    if (arguments.length < 1) return (key = this.tween(key)) && key._value;
    if (value16 == null) return this.tween(key, null);
    if (typeof value16 !== "function") throw new Error();
    return this.tween(key, textTween(value16));
  }

  // node_modules/d3-transition/src/transition/transition.js
  function transition_default() {
    var name17 = this._name, id0 = this._id, id1 = newId();
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          var inherit2 = get7(node, id0);
          schedule_default(node, name17, id1, i2, group3, {
            time: inherit2.time + inherit2.delay + inherit2.duration,
            delay: 0,
            duration: inherit2.duration,
            ease: inherit2.ease
          });
        }
      }
    }
    return new Transition(groups, this._parents, name17, id1);
  }

  // node_modules/d3-transition/src/transition/end.js
  function end_default() {
    var on0, on1, that = this, id5 = that._id, size5 = that.size();
    return new Promise(function(resolve, reject) {
      var cancel = { value: reject }, end = { value: function() {
        if (--size5 === 0) resolve();
      } };
      that.each(function() {
        var schedule = set4(this, id5), on3 = schedule.on;
        if (on3 !== on0) {
          on1 = (on0 = on3).copy();
          on1._.cancel.push(cancel);
          on1._.interrupt.push(cancel);
          on1._.end.push(end);
        }
        schedule.on = on1;
      });
      if (size5 === 0) resolve();
    });
  }

  // node_modules/d3-transition/src/transition/index.js
  var id4 = 0;
  function Transition(groups, parents, name17, id5) {
    this._groups = groups;
    this._parents = parents;
    this._name = name17;
    this._id = id5;
  }
  function transition(name17) {
    return selection_default().transition(name17);
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
  function transition_default2(name17) {
    var id5, timing;
    if (name17 instanceof Transition) {
      id5 = name17._id, name17 = name17._name;
    } else {
      id5 = newId(), (timing = defaultTiming).time = now2(), name17 = name17 == null ? null : name17 + "";
    }
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i2 = 0; i2 < n; ++i2) {
        if (node = group3[i2]) {
          schedule_default(node, name17, id5, i2, group3, timing || inherit(node, id5));
        }
      }
    }
    return new Transition(groups, this._parents, name17, id5);
  }

  // node_modules/d3-transition/src/selection/index.js
  selection_default.prototype.interrupt = interrupt_default2;
  selection_default.prototype.transition = transition_default2;

  // node_modules/d3-brush/src/brush.js
  var { abs: abs2, max: max7, min: min6 } = Math;
  function number1(e) {
    return [+e[0], +e[1]];
  }
  function number2(e) {
    return [number1(e[0]), number1(e[1])];
  }
  var X = {
    name: "x",
    handles: ["w", "e"].map(type),
    input: function(x15, e) {
      return x15 == null ? null : [[+x15[0], e[0][1]], [+x15[1], e[1][1]]];
    },
    output: function(xy) {
      return xy && [xy[0][0], xy[1][0]];
    }
  };
  var Y = {
    name: "y",
    handles: ["n", "s"].map(type),
    input: function(y10, e) {
      return y10 == null ? null : [[e[0][0], +y10[0]], [e[1][0], +y10[1]]];
    },
    output: function(xy) {
      return xy && [xy[0][1], xy[1][1]];
    }
  };
  var XY = {
    name: "xy",
    handles: ["n", "w", "e", "s", "nw", "ne", "sw", "se"].map(type),
    input: function(xy) {
      return xy == null ? null : number2(xy);
    },
    output: function(xy) {
      return xy;
    }
  };
  function type(t) {
    return { type: t };
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

  // node_modules/d3-scale/src/ordinal.js
  var implicit = Symbol("implicit");
  function ordinal() {
    var index5 = new InternMap(), domain = [], range3 = [], unknown = implicit;
    function scale(d5) {
      let i2 = index5.get(d5);
      if (i2 === void 0) {
        if (unknown !== implicit) return unknown;
        index5.set(d5, i2 = domain.push(d5) - 1);
      }
      return range3[i2 % range3.length];
    }
    scale.domain = function(_) {
      if (!arguments.length) return domain.slice();
      domain = [], index5 = new InternMap();
      for (const value16 of _) {
        if (index5.has(value16)) continue;
        index5.set(value16, domain.push(value16) - 1);
      }
      return scale;
    };
    scale.range = function(_) {
      return arguments.length ? (range3 = Array.from(_), scale) : range3.slice();
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    scale.copy = function() {
      return ordinal(domain, range3).unknown(unknown);
    };
    initRange.apply(scale, arguments);
    return scale;
  }

  // node_modules/d3-scale-chromatic/src/colors.js
  function colors_default(specifier) {
    var n = specifier.length / 6 | 0, colors = new Array(n), i2 = 0;
    while (i2 < n) colors[i2] = "#" + specifier.slice(i2 * 6, ++i2 * 6);
    return colors;
  }

  // node_modules/d3-scale-chromatic/src/categorical/category10.js
  var category10_default = colors_default("1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf");

  // node_modules/d3-zoom/src/transform.js
  function Transform(k, x15, y10) {
    this.k = k;
    this.x = x15;
    this.y = y10;
  }
  Transform.prototype = {
    constructor: Transform,
    scale: function(k) {
      return k === 1 ? this : new Transform(this.k * k, this.x, this.y);
    },
    translate: function(x15, y10) {
      return x15 === 0 & y10 === 0 ? this : new Transform(this.k, this.x + this.k * x15, this.y + this.k * y10);
    },
    apply: function(point) {
      return [point[0] * this.k + this.x, point[1] * this.k + this.y];
    },
    applyX: function(x15) {
      return x15 * this.k + this.x;
    },
    applyY: function(y10) {
      return y10 * this.k + this.y;
    },
    invert: function(location2) {
      return [(location2[0] - this.x) / this.k, (location2[1] - this.y) / this.k];
    },
    invertX: function(x15) {
      return (x15 - this.x) / this.k;
    },
    invertY: function(y10) {
      return (y10 - this.y) / this.k;
    },
    rescaleX: function(x15) {
      return x15.copy().domain(x15.range().map(this.invertX, this).map(x15.invert, x15));
    },
    rescaleY: function(y10) {
      return y10.copy().domain(y10.range().map(this.invertY, this).map(y10.invert, y10));
    },
    toString: function() {
      return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
    }
  };
  var identity17 = new Transform(1, 0, 0);
  transform2.prototype = Transform.prototype;
  function transform2(node) {
    while (!node.__zoom) if (!(node = node.parentNode)) return identity17;
    return node.__zoom;
  }

  // node_modules/d3-sankey/node_modules/d3-array/src/max.js
  function max8(values2, valueof) {
    let max9;
    if (valueof === void 0) {
      for (const value16 of values2) {
        if (value16 != null && (max9 < value16 || max9 === void 0 && value16 >= value16)) {
          max9 = value16;
        }
      }
    } else {
      let index5 = -1;
      for (let value16 of values2) {
        if ((value16 = valueof(value16, ++index5, values2)) != null && (max9 < value16 || max9 === void 0 && value16 >= value16)) {
          max9 = value16;
        }
      }
    }
    return max9;
  }

  // node_modules/d3-sankey/node_modules/d3-array/src/min.js
  function min7(values2, valueof) {
    let min8;
    if (valueof === void 0) {
      for (const value16 of values2) {
        if (value16 != null && (min8 > value16 || min8 === void 0 && value16 >= value16)) {
          min8 = value16;
        }
      }
    } else {
      let index5 = -1;
      for (let value16 of values2) {
        if ((value16 = valueof(value16, ++index5, values2)) != null && (min8 > value16 || min8 === void 0 && value16 >= value16)) {
          min8 = value16;
        }
      }
    }
    return min8;
  }

  // node_modules/d3-sankey/node_modules/d3-array/src/sum.js
  function sum2(values2, valueof) {
    let sum4 = 0;
    if (valueof === void 0) {
      for (let value16 of values2) {
        if (value16 = +value16) {
          sum4 += value16;
        }
      }
    } else {
      let index5 = -1;
      for (let value16 of values2) {
        if (value16 = +valueof(value16, ++index5, values2)) {
          sum4 += value16;
        }
      }
    }
    return sum4;
  }

  // node_modules/d3-sankey/src/align.js
  function justify(node, n) {
    return node.sourceLinks.length ? node.depth : n - 1;
  }

  // node_modules/d3-sankey/src/constant.js
  function constant(x15) {
    return function() {
      return x15;
    };
  }

  // node_modules/d3-sankey/src/sankey.js
  function ascendingSourceBreadth(a2, b2) {
    return ascendingBreadth(a2.source, b2.source) || a2.index - b2.index;
  }
  function ascendingTargetBreadth(a2, b2) {
    return ascendingBreadth(a2.target, b2.target) || a2.index - b2.index;
  }
  function ascendingBreadth(a2, b2) {
    return a2.y0 - b2.y0;
  }
  function value14(d5) {
    return d5.value;
  }
  function defaultId(d5) {
    return d5.index;
  }
  function defaultNodes(graph) {
    return graph.nodes;
  }
  function defaultLinks(graph) {
    return graph.links;
  }
  function find4(nodeById, id5) {
    const node = nodeById.get(id5);
    if (!node) throw new Error("missing: " + id5);
    return node;
  }
  function computeLinkBreadths({ nodes }) {
    for (const node of nodes) {
      let y0 = node.y0;
      let y15 = y0;
      for (const link4 of node.sourceLinks) {
        link4.y0 = y0 + link4.width / 2;
        y0 += link4.width;
      }
      for (const link4 of node.targetLinks) {
        link4.y1 = y15 + link4.width / 2;
        y15 += link4.width;
      }
    }
  }
  function Sankey() {
    let x0 = 0, y0 = 0, x15 = 1, y15 = 1;
    let dx = 24;
    let dy4 = 8, py;
    let id5 = defaultId;
    let align = justify;
    let sort2;
    let linkSort;
    let nodes = defaultNodes;
    let links = defaultLinks;
    let iterations = 6;
    function sankey() {
      const graph = { nodes: nodes.apply(null, arguments), links: links.apply(null, arguments) };
      computeNodeLinks(graph);
      computeNodeValues(graph);
      computeNodeDepths(graph);
      computeNodeHeights(graph);
      computeNodeBreadths(graph);
      computeLinkBreadths(graph);
      return graph;
    }
    sankey.update = function(graph) {
      computeLinkBreadths(graph);
      return graph;
    };
    sankey.nodeId = function(_) {
      return arguments.length ? (id5 = typeof _ === "function" ? _ : constant(_), sankey) : id5;
    };
    sankey.nodeAlign = function(_) {
      return arguments.length ? (align = typeof _ === "function" ? _ : constant(_), sankey) : align;
    };
    sankey.nodeSort = function(_) {
      return arguments.length ? (sort2 = _, sankey) : sort2;
    };
    sankey.nodeWidth = function(_) {
      return arguments.length ? (dx = +_, sankey) : dx;
    };
    sankey.nodePadding = function(_) {
      return arguments.length ? (dy4 = py = +_, sankey) : dy4;
    };
    sankey.nodes = function(_) {
      return arguments.length ? (nodes = typeof _ === "function" ? _ : constant(_), sankey) : nodes;
    };
    sankey.links = function(_) {
      return arguments.length ? (links = typeof _ === "function" ? _ : constant(_), sankey) : links;
    };
    sankey.linkSort = function(_) {
      return arguments.length ? (linkSort = _, sankey) : linkSort;
    };
    sankey.size = function(_) {
      return arguments.length ? (x0 = y0 = 0, x15 = +_[0], y15 = +_[1], sankey) : [x15 - x0, y15 - y0];
    };
    sankey.extent = function(_) {
      return arguments.length ? (x0 = +_[0][0], x15 = +_[1][0], y0 = +_[0][1], y15 = +_[1][1], sankey) : [[x0, y0], [x15, y15]];
    };
    sankey.iterations = function(_) {
      return arguments.length ? (iterations = +_, sankey) : iterations;
    };
    function computeNodeLinks({ nodes: nodes2, links: links2 }) {
      for (const [i2, node] of nodes2.entries()) {
        node.index = i2;
        node.sourceLinks = [];
        node.targetLinks = [];
      }
      const nodeById = new Map(nodes2.map((d5, i2) => [id5(d5, i2, nodes2), d5]));
      for (const [i2, link4] of links2.entries()) {
        link4.index = i2;
        let { source: source2, target: target6 } = link4;
        if (typeof source2 !== "object") source2 = link4.source = find4(nodeById, source2);
        if (typeof target6 !== "object") target6 = link4.target = find4(nodeById, target6);
        source2.sourceLinks.push(link4);
        target6.targetLinks.push(link4);
      }
      if (linkSort != null) {
        for (const { sourceLinks, targetLinks } of nodes2) {
          sourceLinks.sort(linkSort);
          targetLinks.sort(linkSort);
        }
      }
    }
    function computeNodeValues({ nodes: nodes2 }) {
      for (const node of nodes2) {
        node.value = node.fixedValue === void 0 ? Math.max(sum2(node.sourceLinks, value14), sum2(node.targetLinks, value14)) : node.fixedValue;
      }
    }
    function computeNodeDepths({ nodes: nodes2 }) {
      const n = nodes2.length;
      let current = new Set(nodes2);
      let next2 = /* @__PURE__ */ new Set();
      let x16 = 0;
      while (current.size) {
        for (const node of current) {
          node.depth = x16;
          for (const { target: target6 } of node.sourceLinks) {
            next2.add(target6);
          }
        }
        if (++x16 > n) throw new Error("circular link");
        current = next2;
        next2 = /* @__PURE__ */ new Set();
      }
    }
    function computeNodeHeights({ nodes: nodes2 }) {
      const n = nodes2.length;
      let current = new Set(nodes2);
      let next2 = /* @__PURE__ */ new Set();
      let x16 = 0;
      while (current.size) {
        for (const node of current) {
          node.height = x16;
          for (const { source: source2 } of node.targetLinks) {
            next2.add(source2);
          }
        }
        if (++x16 > n) throw new Error("circular link");
        current = next2;
        next2 = /* @__PURE__ */ new Set();
      }
    }
    function computeNodeLayers({ nodes: nodes2 }) {
      const x16 = max8(nodes2, (d5) => d5.depth) + 1;
      const kx = (x15 - x0 - dx) / (x16 - 1);
      const columns = new Array(x16);
      for (const node of nodes2) {
        const i2 = Math.max(0, Math.min(x16 - 1, Math.floor(align.call(null, node, x16))));
        node.layer = i2;
        node.x0 = x0 + i2 * kx;
        node.x1 = node.x0 + dx;
        if (columns[i2]) columns[i2].push(node);
        else columns[i2] = [node];
      }
      if (sort2) for (const column of columns) {
        column.sort(sort2);
      }
      return columns;
    }
    function initializeNodeBreadths(columns) {
      const ky = min7(columns, (c) => (y15 - y0 - (c.length - 1) * py) / sum2(c, value14));
      for (const nodes2 of columns) {
        let y10 = y0;
        for (const node of nodes2) {
          node.y0 = y10;
          node.y1 = y10 + node.value * ky;
          y10 = node.y1 + py;
          for (const link4 of node.sourceLinks) {
            link4.width = link4.value * ky;
          }
        }
        y10 = (y15 - y10 + py) / (nodes2.length + 1);
        for (let i2 = 0; i2 < nodes2.length; ++i2) {
          const node = nodes2[i2];
          node.y0 += y10 * (i2 + 1);
          node.y1 += y10 * (i2 + 1);
        }
        reorderLinks(nodes2);
      }
    }
    function computeNodeBreadths(graph) {
      const columns = computeNodeLayers(graph);
      py = Math.min(dy4, (y15 - y0) / (max8(columns, (c) => c.length) - 1));
      initializeNodeBreadths(columns);
      for (let i2 = 0; i2 < iterations; ++i2) {
        const alpha = Math.pow(0.99, i2);
        const beta = Math.max(1 - alpha, (i2 + 1) / iterations);
        relaxRightToLeft(columns, alpha, beta);
        relaxLeftToRight(columns, alpha, beta);
      }
    }
    function relaxLeftToRight(columns, alpha, beta) {
      for (let i2 = 1, n = columns.length; i2 < n; ++i2) {
        const column = columns[i2];
        for (const target6 of column) {
          let y10 = 0;
          let w = 0;
          for (const { source: source2, value: value16 } of target6.targetLinks) {
            let v = value16 * (target6.layer - source2.layer);
            y10 += targetTop(source2, target6) * v;
            w += v;
          }
          if (!(w > 0)) continue;
          let dy5 = (y10 / w - target6.y0) * alpha;
          target6.y0 += dy5;
          target6.y1 += dy5;
          reorderNodeLinks(target6);
        }
        if (sort2 === void 0) column.sort(ascendingBreadth);
        resolveCollisions(column, beta);
      }
    }
    function relaxRightToLeft(columns, alpha, beta) {
      for (let n = columns.length, i2 = n - 2; i2 >= 0; --i2) {
        const column = columns[i2];
        for (const source2 of column) {
          let y10 = 0;
          let w = 0;
          for (const { target: target6, value: value16 } of source2.sourceLinks) {
            let v = value16 * (target6.layer - source2.layer);
            y10 += sourceTop(source2, target6) * v;
            w += v;
          }
          if (!(w > 0)) continue;
          let dy5 = (y10 / w - source2.y0) * alpha;
          source2.y0 += dy5;
          source2.y1 += dy5;
          reorderNodeLinks(source2);
        }
        if (sort2 === void 0) column.sort(ascendingBreadth);
        resolveCollisions(column, beta);
      }
    }
    function resolveCollisions(nodes2, alpha) {
      const i2 = nodes2.length >> 1;
      const subject = nodes2[i2];
      resolveCollisionsBottomToTop(nodes2, subject.y0 - py, i2 - 1, alpha);
      resolveCollisionsTopToBottom(nodes2, subject.y1 + py, i2 + 1, alpha);
      resolveCollisionsBottomToTop(nodes2, y15, nodes2.length - 1, alpha);
      resolveCollisionsTopToBottom(nodes2, y0, 0, alpha);
    }
    function resolveCollisionsTopToBottom(nodes2, y10, i2, alpha) {
      for (; i2 < nodes2.length; ++i2) {
        const node = nodes2[i2];
        const dy5 = (y10 - node.y0) * alpha;
        if (dy5 > 1e-6) node.y0 += dy5, node.y1 += dy5;
        y10 = node.y1 + py;
      }
    }
    function resolveCollisionsBottomToTop(nodes2, y10, i2, alpha) {
      for (; i2 >= 0; --i2) {
        const node = nodes2[i2];
        const dy5 = (node.y1 - y10) * alpha;
        if (dy5 > 1e-6) node.y0 -= dy5, node.y1 -= dy5;
        y10 = node.y0 - py;
      }
    }
    function reorderNodeLinks({ sourceLinks, targetLinks }) {
      if (linkSort === void 0) {
        for (const { source: { sourceLinks: sourceLinks2 } } of targetLinks) {
          sourceLinks2.sort(ascendingTargetBreadth);
        }
        for (const { target: { targetLinks: targetLinks2 } } of sourceLinks) {
          targetLinks2.sort(ascendingSourceBreadth);
        }
      }
    }
    function reorderLinks(nodes2) {
      if (linkSort === void 0) {
        for (const { sourceLinks, targetLinks } of nodes2) {
          sourceLinks.sort(ascendingTargetBreadth);
          targetLinks.sort(ascendingSourceBreadth);
        }
      }
    }
    function targetTop(source2, target6) {
      let y10 = source2.y0 - (source2.sourceLinks.length - 1) * py / 2;
      for (const { target: node, width: width13 } of source2.sourceLinks) {
        if (node === target6) break;
        y10 += width13 + py;
      }
      for (const { source: node, width: width13 } of target6.targetLinks) {
        if (node === source2) break;
        y10 -= width13;
      }
      return y10;
    }
    function sourceTop(source2, target6) {
      let y10 = target6.y0 - (target6.targetLinks.length - 1) * py / 2;
      for (const { source: node, width: width13 } of target6.targetLinks) {
        if (node === source2) break;
        y10 += width13 + py;
      }
      for (const { target: node, width: width13 } of source2.sourceLinks) {
        if (node === target6) break;
        y10 -= width13;
      }
      return y10;
    }
    return sankey;
  }

  // node_modules/d3-sankey/node_modules/d3-path/src/path.js
  var pi2 = Math.PI;
  var tau = 2 * pi2;
  var epsilon = 1e-6;
  var tauEpsilon = tau - epsilon;
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
    moveTo: function(x15, y10) {
      this._ += "M" + (this._x0 = this._x1 = +x15) + "," + (this._y0 = this._y1 = +y10);
    },
    closePath: function() {
      if (this._x1 !== null) {
        this._x1 = this._x0, this._y1 = this._y0;
        this._ += "Z";
      }
    },
    lineTo: function(x15, y10) {
      this._ += "L" + (this._x1 = +x15) + "," + (this._y1 = +y10);
    },
    quadraticCurveTo: function(x15, y15, x16, y10) {
      this._ += "Q" + +x15 + "," + +y15 + "," + (this._x1 = +x16) + "," + (this._y1 = +y10);
    },
    bezierCurveTo: function(x15, y15, x24, y24, x16, y10) {
      this._ += "C" + +x15 + "," + +y15 + "," + +x24 + "," + +y24 + "," + (this._x1 = +x16) + "," + (this._y1 = +y10);
    },
    arcTo: function(x15, y15, x24, y24, r) {
      x15 = +x15, y15 = +y15, x24 = +x24, y24 = +y24, r = +r;
      var x0 = this._x1, y0 = this._y1, x21 = x24 - x15, y21 = y24 - y15, x01 = x0 - x15, y01 = y0 - y15, l01_2 = x01 * x01 + y01 * y01;
      if (r < 0) throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + (this._x1 = x15) + "," + (this._y1 = y15);
      } else if (!(l01_2 > epsilon)) ;
      else if (!(Math.abs(y01 * x21 - y21 * x01) > epsilon) || !r) {
        this._ += "L" + (this._x1 = x15) + "," + (this._y1 = y15);
      } else {
        var x20 = x24 - x0, y20 = y24 - y0, l21_2 = x21 * x21 + y21 * y21, l20_2 = x20 * x20 + y20 * y20, l21 = Math.sqrt(l21_2), l01 = Math.sqrt(l01_2), l = r * Math.tan((pi2 - Math.acos((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2), t01 = l / l01, t21 = l / l21;
        if (Math.abs(t01 - 1) > epsilon) {
          this._ += "L" + (x15 + t01 * x01) + "," + (y15 + t01 * y01);
        }
        this._ += "A" + r + "," + r + ",0,0," + +(y01 * x20 > x01 * y20) + "," + (this._x1 = x15 + t21 * x21) + "," + (this._y1 = y15 + t21 * y21);
      }
    },
    arc: function(x15, y10, r, a0, a1, ccw) {
      x15 = +x15, y10 = +y10, r = +r, ccw = !!ccw;
      var dx = r * Math.cos(a0), dy4 = r * Math.sin(a0), x0 = x15 + dx, y0 = y10 + dy4, cw = 1 ^ ccw, da = ccw ? a0 - a1 : a1 - a0;
      if (r < 0) throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + x0 + "," + y0;
      } else if (Math.abs(this._x1 - x0) > epsilon || Math.abs(this._y1 - y0) > epsilon) {
        this._ += "L" + x0 + "," + y0;
      }
      if (!r) return;
      if (da < 0) da = da % tau + tau;
      if (da > tauEpsilon) {
        this._ += "A" + r + "," + r + ",0,1," + cw + "," + (x15 - dx) + "," + (y10 - dy4) + "A" + r + "," + r + ",0,1," + cw + "," + (this._x1 = x0) + "," + (this._y1 = y0);
      } else if (da > epsilon) {
        this._ += "A" + r + "," + r + ",0," + +(da >= pi2) + "," + cw + "," + (this._x1 = x15 + r * Math.cos(a1)) + "," + (this._y1 = y10 + r * Math.sin(a1));
      }
    },
    rect: function(x15, y10, w, h) {
      this._ += "M" + (this._x0 = this._x1 = +x15) + "," + (this._y0 = this._y1 = +y10) + "h" + +w + "v" + +h + "h" + -w + "Z";
    },
    toString: function() {
      return this._;
    }
  };
  var path_default = path;

  // node_modules/d3-sankey/node_modules/d3-shape/src/constant.js
  function constant_default5(x15) {
    return function constant2() {
      return x15;
    };
  }

  // node_modules/d3-sankey/node_modules/d3-shape/src/point.js
  function x7(p2) {
    return p2[0];
  }
  function y6(p2) {
    return p2[1];
  }

  // node_modules/d3-sankey/node_modules/d3-shape/src/array.js
  var slice3 = Array.prototype.slice;

  // node_modules/d3-sankey/node_modules/d3-shape/src/link/index.js
  function linkSource(d5) {
    return d5.source;
  }
  function linkTarget(d5) {
    return d5.target;
  }
  function link3(curve) {
    var source2 = linkSource, target6 = linkTarget, x15 = x7, y10 = y6, context = null;
    function link4() {
      var buffer, argv = slice3.call(arguments), s = source2.apply(this, argv), t = target6.apply(this, argv);
      if (!context) context = buffer = path_default();
      curve(context, +x15.apply(this, (argv[0] = s, argv)), +y10.apply(this, argv), +x15.apply(this, (argv[0] = t, argv)), +y10.apply(this, argv));
      if (buffer) return context = null, buffer + "" || null;
    }
    link4.source = function(_) {
      return arguments.length ? (source2 = _, link4) : source2;
    };
    link4.target = function(_) {
      return arguments.length ? (target6 = _, link4) : target6;
    };
    link4.x = function(_) {
      return arguments.length ? (x15 = typeof _ === "function" ? _ : constant_default5(+_), link4) : x15;
    };
    link4.y = function(_) {
      return arguments.length ? (y10 = typeof _ === "function" ? _ : constant_default5(+_), link4) : y10;
    };
    link4.context = function(_) {
      return arguments.length ? (context = _ == null ? null : _, link4) : context;
    };
    return link4;
  }
  function curveHorizontal(context, x0, y0, x15, y15) {
    context.moveTo(x0, y0);
    context.bezierCurveTo(x0 = (x0 + x15) / 2, y0, x0, y15, x15, y15);
  }
  function linkHorizontal() {
    return link3(curveHorizontal);
  }

  // node_modules/d3-sankey/src/sankeyLinkHorizontal.js
  function horizontalSource(d5) {
    return [d5.source.x1, d5.y0];
  }
  function horizontalTarget(d5) {
    return [d5.target.x0, d5.y1];
  }
  function sankeyLinkHorizontal_default() {
    return linkHorizontal().source(horizontalSource).target(horizontalTarget);
  }

  // output/D3.Layouts.Sankey.Functions/foreign.js
  var sankeySetData_ = (data) => (width13) => (height13) => () => {
    const sankeyGenerator = Sankey().nodeWidth(15).nodePadding(10).extent([[1, 1], [width13 - 1, height13 - 5]]);
    const dataCopy = {
      nodes: data.nodes.map((n) => ({ ...n })),
      links: data.links.map((l) => ({ ...l }))
    };
    const graph = sankeyGenerator(dataCopy);
    const colorScale = ordinal(category10_default);
    graph.nodes.forEach((node) => {
      node.color = colorScale(node.name);
    });
    graph.links.forEach((link4) => {
      link4.width = link4.width || Math.max(1, link4.value);
      link4.color = link4.source.color || colorScale(link4.source.name);
    });
    return graph;
  };
  var sankeyLinkPath_ = sankeyLinkHorizontal_default();

  // output/D3.Layouts.Sankey.Functions/index.js
  var sankeySetData = function(dictMonadEffect) {
    var liftEffect11 = liftEffect(dictMonadEffect);
    return function(data_2) {
      return function(width13) {
        return function(height13) {
          return liftEffect11(sankeySetData_(data_2)(width13)(height13));
        };
      };
    };
  };

  // output/D3.Examples.SankeyDiagram/index.js
  var classed7 = /* @__PURE__ */ classed(toAttrString);
  var discard20 = /* @__PURE__ */ discard(discardUnit);
  var fill6 = /* @__PURE__ */ fill(toAttrString);
  var strokeWidth5 = /* @__PURE__ */ strokeWidth(toAttrNumberFn);
  var strokeOpacity5 = /* @__PURE__ */ strokeOpacity(toAttrNumber);
  var d4 = /* @__PURE__ */ d2(toAttrStringFn);
  var strokeColor5 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var x8 = /* @__PURE__ */ x(toAttrNumberFn);
  var y7 = /* @__PURE__ */ y(toAttrNumberFn);
  var width11 = /* @__PURE__ */ width8(toAttrNumberFn);
  var height11 = /* @__PURE__ */ height8(toAttrNumberFn);
  var fill12 = /* @__PURE__ */ fill(toAttrStringFn);
  var fillOpacity2 = /* @__PURE__ */ fillOpacity(toAttrNumber);
  var dy3 = /* @__PURE__ */ dy(toAttrNumber);
  var textAnchor4 = /* @__PURE__ */ textAnchor(toAttrStringFn);
  var text10 = /* @__PURE__ */ text6(toAttrStringFn);
  var node_ = {
    name: function($59) {
      return (function(v) {
        return v.name;
      })(unboxSankeyNode($59));
    },
    x0: function($60) {
      return (function(v) {
        return v.x0;
      })(unboxSankeyNode($60));
    },
    y0: function($61) {
      return (function(v) {
        return v.y0;
      })(unboxSankeyNode($61));
    },
    x1: function($62) {
      return (function(v) {
        return v.x1;
      })(unboxSankeyNode($62));
    },
    y1: function($63) {
      return (function(v) {
        return v.y1;
      })(unboxSankeyNode($63));
    },
    value: function($64) {
      return (function(v) {
        return v.value;
      })(unboxSankeyNode($64));
    },
    color: function($65) {
      return (function(v) {
        return v.color;
      })(unboxSankeyNode($65));
    }
  };
  var link_2 = {
    source: function($66) {
      return (function(v) {
        return v.source;
      })(unboxSankeyLink($66));
    },
    target: function($67) {
      return (function(v) {
        return v.target;
      })(unboxSankeyLink($67));
    },
    value: function($68) {
      return (function(v) {
        return v.value;
      })(unboxSankeyLink($68));
    },
    width: function($69) {
      return (function(v) {
        return v.width;
      })(unboxSankeyLink($69));
    },
    color: function($70) {
      return (function(v) {
        return v.color;
      })(unboxSankeyLink($70));
    },
    path: function($71) {
      return (function(v) {
        return v.path;
      })(unboxSankeyLink($71));
    }
  };
  var keyForNode = function(d1) {
    return node_.name(d1);
  };
  var keyForLink = function(d1) {
    return node_.name(link_2.source(d1)) + ("-" + node_.name(link_2.target(d1)));
  };
  var draw5 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard20(dictBind);
    return function(dictMonadEffect) {
      var liftEffect11 = liftEffect(dictMonadEffect);
      var pure21 = pure(dictMonadEffect.Monad0().Applicative0());
      return function(dictMonadState) {
        return function(dictSankeyM) {
          var SelectionM1 = dictSankeyM.SelectionM1();
          var attach2 = attach(SelectionM1);
          var appendTo2 = appendTo(SelectionM1);
          var setSankeyData2 = setSankeyData(dictSankeyM);
          var simpleJoin2 = simpleJoin(SelectionM1);
          var setAttributes2 = setAttributes(SelectionM1);
          return function(sankeyData) {
            return function(selector) {
              return bind18(liftEffect11(getWindowWidthHeight))(function(v) {
                return bind18(attach2(selector))(function(v1) {
                  return bind18(appendTo2(v1)(Svg.value)([viewBox(0)(0)(v.value0)(v.value1), classed7("sankey")]))(function(svg2) {
                    return bind18(appendTo2(svg2)(Group.value)([classed7("links")]))(function(linksGroup) {
                      return bind18(appendTo2(svg2)(Group.value)([classed7("nodes")]))(function(nodesGroup) {
                        return bind18(appendTo2(svg2)(Group.value)([classed7("labels")]))(function(labelsGroup) {
                          return bind18(setSankeyData2(sankeyData)(v.value0)(v.value1))(function(layoutResult) {
                            return bind18(simpleJoin2(linksGroup)(Path.value)(layoutResult.links)(keyForLink))(function(linksSelection) {
                              return discard111(setAttributes2(linksSelection)([classed7("sankey-link"), fill6("none"), strokeWidth5(link_2.width), strokeOpacity5(0.5), d4(sankeyLinkPath_), strokeColor5(link_2.color)]))(function() {
                                return bind18(simpleJoin2(nodesGroup)(Rect.value)(layoutResult.nodes)(keyForNode))(function(nodesSelection) {
                                  return discard111(setAttributes2(nodesSelection)([classed7("sankey-node"), x8(node_.x0), y7(node_.y0), width11(function(n) {
                                    return node_.x1(n) - node_.x0(n);
                                  }), height11(function(n) {
                                    return node_.y1(n) - node_.y0(n);
                                  }), fill12(node_.color), fillOpacity2(0.8)]))(function() {
                                    return bind18(simpleJoin2(labelsGroup)(Text2.value)(layoutResult.nodes)(keyForNode))(function(labelsSelection) {
                                      return discard111(setAttributes2(labelsSelection)([classed7("sankey-label"), x8(function(n) {
                                        var $55 = node_.x0(n) < v.value0 / 2;
                                        if ($55) {
                                          return node_.x1(n) + 6;
                                        }
                                        ;
                                        return node_.x0(n) - 6;
                                      }), y7(function(n) {
                                        return (node_.y0(n) + node_.y1(n)) / 2;
                                      }), dy3(4), textAnchor4(function(n) {
                                        var $56 = node_.x0(n) < v.value0 / 2;
                                        if ($56) {
                                          return "start";
                                        }
                                        ;
                                        return "end";
                                      }), text10(node_.name)]))(function() {
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
            };
          };
        };
      };
    };
  };

  // output/D3.Layouts.Sankey.Types/foreign.js
  var initialSankeyLayoutState = Sankey().nodeWidth(15).nodePadding(10);

  // output/D3Tagless.Instance.Sankey/index.js
  var liftA14 = /* @__PURE__ */ liftA1(applicativeEffect);
  var monadStateD3SankeyM = /* @__PURE__ */ monadStateStateT(monadEffect);
  var monadEffD3SankeyM = /* @__PURE__ */ monadEffectState(monadEffectEffect);
  var sankeySetData2 = /* @__PURE__ */ sankeySetData(monadEffD3SankeyM);
  var monadD3SankeyM = /* @__PURE__ */ monadStateT(monadEffect);
  var selectionMD3Selection_D3S2 = {
    appendTo: function(s_) {
      return selectionAppendElement(selectionMD3Selection_D3S2)(s_);
    },
    selectUnder: function(s_) {
      return selectionSelectUnder(selectionMD3Selection_D3S2)(s_);
    },
    attach: function(selector) {
      return selectionAttach(selectionMD3Selection_D3S2)(selector);
    },
    filterSelection: function(s_) {
      return selectionFilterSelection(selectionMD3Selection_D3S2)(s_);
    },
    mergeSelections: function(s_) {
      return selectionMergeSelections(selectionMD3Selection_D3S2)(s_);
    },
    setAttributes: function(s_) {
      return selectionModifySelection(selectionMD3Selection_D3S2)(s_);
    },
    on: function(s_) {
      return selectionOn(selectionMD3Selection_D3S2)(s_);
    },
    openSelection: function(s_) {
      return selectionOpenSelection(selectionMD3Selection_D3S2)(s_);
    },
    simpleJoin: function(s_) {
      return selectionJoin(selectionMD3Selection_D3S2)(s_);
    },
    updateJoin: function(s_) {
      return selectionUpdateJoin(selectionMD3Selection_D3S2)(s_);
    },
    Monad0: function() {
      return monadD3SankeyM;
    }
  };
  var sankeyMD3Selection_D3Sank = {
    setSankeyData: function(data_2) {
      return function(width13) {
        return function(height13) {
          return sankeySetData2(data_2)(width13)(height13);
        };
      };
    },
    Monad0: function() {
      return monadD3SankeyM;
    },
    SelectionM1: function() {
      return selectionMD3Selection_D3S2;
    }
  };
  var exec_D3M_Sankey = function(state3) {
    return function(v) {
      return liftA14(snd)(runStateT(v)(state3));
    };
  };
  var runWithD3_Sankey = function(dictBind) {
    var bind18 = bind(dictBind);
    return function(dictMonadState) {
      var get8 = get(dictMonadState);
      var modify_6 = modify_(dictMonadState);
      return function(dictMonadEffect) {
        var liftEffect11 = liftEffect(dictMonadEffect);
        return function(state_T) {
          return bind18(get8)(function(state3) {
            return bind18(liftEffect11(exec_D3M_Sankey(state3)(state_T)))(function(state$prime) {
              return modify_6(function(v) {
                return state$prime;
              });
            });
          });
        };
      };
    };
  };
  var bindD3SankeyM = /* @__PURE__ */ bindStateT(monadEffect);

  // output/Stories.Sankey/index.js
  var prop10 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "draw";
    }
  })()();
  var not9 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var discard21 = /* @__PURE__ */ discard(discardUnit);
  var draw6 = /* @__PURE__ */ draw5(bindD3SankeyM)(monadEffD3SankeyM)(monadStateD3SankeyM)(sankeyMD3Selection_D3Sank);
  var prop110 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "code";
    }
  })()();
  var prop25 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "blurb";
    }
  })()();
  var Initialize6 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var Finalize4 = /* @__PURE__ */ (function() {
    function Finalize7() {
    }
    ;
    Finalize7.value = new Finalize7();
    return Finalize7;
  })();
  var ToggleCard5 = /* @__PURE__ */ (function() {
    function ToggleCard8(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard8.create = function(value0) {
      return new ToggleCard8(value0);
    };
    return ToggleCard8;
  })();
  var blurbtext4 = /* @__PURE__ */ blurbParagraphs(functorArray)(["Sankey diagrams visualize the flow of resources, energy, costs, or other\n    quantities through a system. The width of each connection (link) is\n    proportional to the flow quantity, making it easy to see dominant flows at\n    a glance.", "This example shows energy flows in the UK energy system. The diagram uses\n    D3's Sankey layout algorithm to automatically position nodes and compute\n    smooth flow paths between them. Nodes are arranged in columns, and the\n    algorithm minimizes crossing connections where possible.", "The implementation follows the Finally Tagless pattern used throughout this\n    library. The SankeyM capability extends SelectionM with layout-specific\n    operations for processing nodes and links through the Sankey algorithm.", "Common uses for Sankey diagrams include: energy and material flow analysis,\n    cost breakdowns, website traffic visualization, and any scenario where you\n    need to show how quantities are distributed and transformed through a\n    multi-stage process."]);
  var _snippets4 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  })();
  var _panels5 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  })();
  var _drawCode4 = function(dictStrong) {
    var $69 = _snippets4(dictStrong);
    var $70 = prop10($$Proxy.value)(dictStrong);
    return function($71) {
      return $69($70($71));
    };
  };
  var _drawCode13 = /* @__PURE__ */ _drawCode4(strongFn);
  var _drawCode23 = /* @__PURE__ */ _drawCode4(strongForget);
  var handleAction5 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard21(dictBind);
    var runWithD3_Sankey2 = runWithD3_Sankey(dictBind);
    return function(dictMonadAff) {
      var liftAff2 = liftAff(dictMonadAff);
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var pure21 = pure(MonadEffect0.Monad0().Applicative0());
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var assign4 = assign2(dictMonadState);
        var runWithD3_Sankey1 = runWithD3_Sankey2(dictMonadState)(MonadEffect0);
        return function(v) {
          if (v instanceof ToggleCard5) {
            return modifying3(v.value0(strongFn))(not9);
          }
          ;
          if (v instanceof Initialize6) {
            return bind18(liftAff2(readSnippetFiles("SankeyDraw")))(function(text1) {
              return discard111(assign4(_drawCode13)(text1))(function() {
                return runWithD3_Sankey1(draw6(energyData)("div.svg-container"));
              });
            });
          }
          ;
          if (v instanceof Finalize4) {
            return pure21(unit);
          }
          ;
          throw new Error("Failed pattern match at Stories.Sankey (line 114, column 16 - line 124, column 24): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction15 = /* @__PURE__ */ handleAction5(bindHalogenM);
  var _code5 = function(dictStrong) {
    var $72 = _panels5(dictStrong);
    var $73 = prop110($$Proxy.value)(dictStrong);
    return function($74) {
      return $72($73($74));
    };
  };
  var _code15 = /* @__PURE__ */ _code5(strongForget);
  var _blurb3 = function(dictStrong) {
    var $75 = _panels5(dictStrong);
    var $76 = prop25($$Proxy.value)(dictStrong);
    return function($77) {
      return $75($76($77));
    };
  };
  var _blurb13 = /* @__PURE__ */ _blurb3(strongForget);
  var component5 = function(dictMonadAff) {
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-about")])([field_2({
        label: text("About"),
        helpText: [],
        error: [],
        inputId: "show-blurb"
      })([toggle([id2("show-blurb"), checked(toBoolean(view(_blurb13)(state3))), onChange(function(v) {
        return new ToggleCard5(function(dictStrong) {
          return _blurb3(dictStrong);
        });
      })])]), content_(view(_blurb13)(state3))(blurbtext4)]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked(toBoolean(view(_code15)(state3))), onChange(function(v) {
        return new ToggleCard5(function(dictStrong) {
          return _code5(dictStrong);
        });
      })])]), content_(view(_code15)(state3))(syntaxHighlightedCode(view(_drawCode23)(state3)))]), div2([tailwindClass("svg-container")])([])]);
    };
    var initialState = {
      sankeyLayout: initialSankeyLayoutState,
      panels: {
        blurb: Collapsed.value,
        code: Collapsed.value
      },
      snippets: {
        draw: ""
      }
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        handleAction: handleAction15(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        initialize: new Just(Initialize6.value),
        finalize: new Just(Finalize4.value)
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
  spotlightNeighbours_ = (simulation, id5, nodetype) => {
    spotlit = true;
    spotlitID = id5;
    simulation.stop();
    svg = d3.select("div.svg-container svg");
    nodeSelection = svg.selectAll("g.nodes g");
    spotlitSelection = nodeSelection.filter((d5, i2) => d5.id == id5);
    spotlitNode = spotlitSelection.node();
    spotlitNode.__data__.fx = spotlitNode.__data__.fx || spotlitNode.__data__.x;
    spotlitNode.__data__.fy = spotlitNode.__data__.fy || spotlitNode.__data__.y;
    const targets = spotlitNode.__data__.links.targets;
    const sources = spotlitNode.__data__.links.sources;
    sourcesSelection = nodeSelection.filter((d5, i2) => sources.includes(d5.id));
    targetSelection = nodeSelection.filter((d5, i2) => targets.includes(d5.id));
    svg.classed("spotlight", true);
    sourcesSelection.classed("source", true);
    targetSelection.classed("target", true);
    spotlitSelection.classed("spotlight", true);
    spotlitSelection.classed("source target", false);
    simulation.force(
      "collide",
      d3.forceCollide().radius(
        (d5) => sources.includes(d5.id) || targets.includes(d5.id) ? d5.r * 4 : d5.id === d5.containerID ? 10 : d5.r
      )
    );
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
    simulation.force(
      "collide",
      d3.forceCollide().radius((d5) => d5.id === d5.containerID ? 10 : d5.r)
    );
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
    var objectArray = jsonlines.map((d5) => JSON.parse(d5));
    return objectArray;
  };
  function splitIntoLines(str) {
    return str.split(/\r\n|[\n\v\f\r\u0085\u2028\u2029]/);
  }

  // output/D3.Examples.Spago.Files/index.js
  var fromFoldable9 = /* @__PURE__ */ fromFoldable3(ordString)(foldableArray);
  var mapFlipped5 = /* @__PURE__ */ mapFlipped(functorArray);
  var append14 = /* @__PURE__ */ append(semigroupArray);
  var map43 = /* @__PURE__ */ map(functorArray);
  var fromFoldableWith2 = /* @__PURE__ */ fromFoldableWith(ordString)(foldableArray);
  var lookup8 = /* @__PURE__ */ lookup2(ordString);
  var bind10 = /* @__PURE__ */ bind(bindMaybe);
  var sum3 = /* @__PURE__ */ sum(foldableArray)(semiringNumber);
  var equalSnd2 = /* @__PURE__ */ equalSnd(eqString);
  var compareSnd2 = /* @__PURE__ */ compareSnd(ordString);
  var fromFoldable12 = /* @__PURE__ */ fromFoldable3(ordInt)(foldableArray);
  var PackageInfo = /* @__PURE__ */ (function() {
    function PackageInfo2(value0) {
      this.value0 = value0;
    }
    ;
    PackageInfo2.create = function(value0) {
      return new PackageInfo2(value0);
    };
    return PackageInfo2;
  })();
  var IsModule = /* @__PURE__ */ (function() {
    function IsModule2(value0) {
      this.value0 = value0;
    }
    ;
    IsModule2.create = function(value0) {
      return new IsModule2(value0);
    };
    return IsModule2;
  })();
  var IsPackage = /* @__PURE__ */ (function() {
    function IsPackage2(value0) {
      this.value0 = value0;
    }
    ;
    IsPackage2.create = function(value0) {
      return new IsPackage2(value0);
    };
    return IsPackage2;
  })();
  var M2M_Tree = /* @__PURE__ */ (function() {
    function M2M_Tree2() {
    }
    ;
    M2M_Tree2.value = new M2M_Tree2();
    return M2M_Tree2;
  })();
  var M2M_Graph = /* @__PURE__ */ (function() {
    function M2M_Graph2() {
    }
    ;
    M2M_Graph2.value = new M2M_Graph2();
    return M2M_Graph2;
  })();
  var P2P = /* @__PURE__ */ (function() {
    function P2P2() {
    }
    ;
    P2P2.value = new P2P2();
    return P2P2;
  })();
  var M2P = /* @__PURE__ */ (function() {
    function M2P2() {
    }
    ;
    M2P2.value = new M2P2();
    return M2P2;
  })();
  var eqLinkType = {
    eq: function(x15) {
      return function(y10) {
        if (x15 instanceof M2M_Tree && y10 instanceof M2M_Tree) {
          return true;
        }
        ;
        if (x15 instanceof M2M_Graph && y10 instanceof M2M_Graph) {
          return true;
        }
        ;
        if (x15 instanceof P2P && y10 instanceof P2P) {
          return true;
        }
        ;
        if (x15 instanceof M2P && y10 instanceof M2P) {
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
    var names = append14(map43(function(v1) {
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
    var ids = range(0)(length(names) - 1 | 0);
    var name2ID = fromFoldableWith2(function(v1) {
      return function(v2) {
        return v1;
      };
    })(zip(names)(ids));
    var getId = function(s) {
      return fromMaybe(0)(lookup8(s)(name2ID));
    };
    var makeNodeFromModuleJSONPL = function(m) {
      var id5 = getId(m.key);
      return {
        id: id5,
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
        var id5 = getId(a2.key);
        var makeTuple = function(s) {
          return new Tuple(id5, getId(s));
        };
        return append14(map43(makeTuple)(a2.depends))(b2);
      };
    };
    var moduleLinks = map43(makeLink(M2M_Graph.value))(foldl2(foldDepends)([])(v.modules));
    var packageLinks = map43(makeLink(P2P.value))(foldl2(foldDepends)([])(v.packages));
    var depsMap = fromFoldable9(mapFlipped5(v.lsDeps)(function(d5) {
      return new Tuple(d5.packageName, {
        version: d5.version,
        repo: d5.repo.contents
      });
    }));
    var makeNodeFromPackageJSONCL = function(p2) {
      var repo = fromMaybe({
        version: "not found",
        repo: "not found"
      })(lookup8(p2.key)(depsMap));
      var id5 = getId(p2.key);
      return {
        id: id5,
        name: p2.key,
        inSim: true,
        nodetype: new IsPackage(new PackageInfo(repo)),
        containerID: id5,
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
        containsMany: length(p2.contains) > 1,
        treeXY: nullImpl,
        treeDepth: nullImpl,
        gridXY: nullImpl
      };
    };
    var addPackageInfo = function(v1) {
      var packageName = (function() {
        var pieces = split("/")(v1.path);
        return bind10(index(pieces)(0))(function(root2) {
          return bind10(index(pieces)(1))(function(packageString) {
            if (root2 === ".spago") {
              return new Just(packageString);
            }
            ;
            if (root2 === "src") {
              return new Just("my-project");
            }
            ;
            return Nothing.value;
          });
        });
      })();
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
      return sum3(map43(function(v1) {
        return v1.loc;
      })(maybeModules(ms)));
    };
    var moduleNodes = map43(makeNodeFromModuleJSONPL)(modulesPL);
    var modulePackageLinks = map43(makeModuleToPackageLink)(moduleNodes);
    var links = append14(moduleLinks)(append14(packageLinks)(modulePackageLinks));
    var getSourceLinks = function(v1) {
      var sources = foldl2(function(acc) {
        return function(v2) {
          var $73 = v1.id === v2.target;
          if ($73) {
            return cons2(v2.source)(acc);
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
    var nodes = append14(moduleNodes)(packageNodes);
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
      id2Name: empty3,
      id2Package: empty3,
      id2LOC: empty3
    };
  };

  // output/D3.Examples.Spago.Unsafe/index.js
  var unboxD3SimNode2 = function(datum2) {
    return datum2;
  };
  var unboxD3SimLink2 = function(datum2) {
    return datum2;
  };

  // output/Data.Graph/index.js
  var map114 = /* @__PURE__ */ map(functorMaybe);
  var Graph = function(x15) {
    return x15;
  };
  var lookup9 = function(dictOrd) {
    var lookup14 = lookup2(dictOrd);
    return function(k) {
      return function(v) {
        return map114(fst)(lookup14(k)(v));
      };
    };
  };
  var fromMap2 = Graph;

  // output/D3.Examples.Spago.Model/index.js
  var lookup10 = /* @__PURE__ */ lookup2(ordInt);
  var map44 = /* @__PURE__ */ map(functorArray);
  var append15 = /* @__PURE__ */ append(semigroupArray);
  var fromFoldable11 = /* @__PURE__ */ fromFoldable2(foldableArray);
  var insert9 = /* @__PURE__ */ insert(ordInt);
  var mapFlipped6 = /* @__PURE__ */ mapFlipped(functorArray);
  var show17 = /* @__PURE__ */ show(showLinkType);
  var append16 = /* @__PURE__ */ append(semigroupString);
  var fromFoldable13 = /* @__PURE__ */ fromFoldable3(ordInt)(foldableArray);
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
          contains: node.links.contains,
          inPackage: node.links.inPackage,
          outPackage: node.links.outPackage,
          targets: node.links.targets,
          treeChildren: node.links.treeChildren,
          sources: fromMaybe([])(lookup10(node.id)(sourcesMap))
        },
        id: node.id,
        cluster: node.containerID,
        connected: node.connected,
        showChildren: (function() {
          if (node.nodetype instanceof IsPackage) {
            return true;
          }
          ;
          if (node.nodetype instanceof IsModule) {
            return false;
          }
          ;
          throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 207, column 20 - line 209, column 45): " + [node.nodetype.constructor.name]);
        })(),
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
        cluster: v.cluster,
        focusX: v.focusX,
        focusY: v.focusY,
        r: v.r,
        fx: nullImpl,
        fy: nullImpl
      };
    };
    return map44(unpin2)(nodes);
  };
  var setTreeXYIncludingLeaves = function(v) {
    return function(v1) {
      return {
        id: v.id,
        showChildren: v.showChildren,
        containerID: v.containerID,
        containerName: v.containerName,
        containsMany: v.containsMany,
        inSim: v.inSim,
        loc: v.loc,
        name: v.name,
        nodetype: v.nodetype,
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
        r: v.r,
        treeXY: notNull({
          x: v1.x,
          y: v1.y
        }),
        treeDepth: notNull(v1.depth),
        connected: true,
        links: {
          contains: v.links.contains,
          inPackage: v.links.inPackage,
          outPackage: v.links.outPackage,
          sources: v.links.sources,
          targets: v.links.targets,
          treeChildren: v1.childIDs
        }
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
      var d5 = toNumber(i2);
      var c = toNumber(columns);
      var x15 = remainder(d5)(c);
      var y10 = floor(d5 / c);
      return {
        x: x15,
        y: y10
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
    var graphMap = foldl2(addNode)(empty3)(nodes);
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
        id2TreeData: empty3
      }
    };
  };
  var link_3 = {
    source: function($198) {
      return (function(v) {
        return v.source;
      })(unboxD3SimLink2($198));
    },
    target: function($199) {
      return (function(v) {
        return v.target;
      })(unboxD3SimLink2($199));
    },
    linkType: function($200) {
      return (function(v) {
        return v.linktype;
      })(unboxD3SimLink2($200));
    },
    linkClass: function($201) {
      return show17((function(v) {
        return v.linktype;
      })(unboxD3SimLink2($201)));
    },
    linkClass2: /* @__PURE__ */ (function() {
      var $202 = append16("updated ");
      return function($203) {
        return $202(show17((function(v) {
          return v.linktype;
        })(unboxD3SimLink2($203))));
      };
    })(),
    color: function($204) {
      return d3SchemeCategory10N_(toNumber((function(v) {
        return v.target.containerID;
      })(unboxD3SimLink2($204))));
    }
  };
  var sourcePackageIs = function(name17) {
    return function(link4) {
      return link_3.source(link4).name === name17;
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
        var x15 = p2.x * cos(p2.y);
        var y10 = p2.x * sin(p2.y);
        return {
          x: x15,
          y: y10
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
        cluster: v.cluster,
        focusX: v.focusX,
        focusY: v.focusY,
        r: v.r,
        fx: notNull(radialXY.x),
        fy: notNull(radialXY.y)
      };
    };
    var partitioned = partition(isUsedModule)(nodes);
    return append15(partitioned.no)(map44(setXYtoTreeXY)(partitioned.yes));
  };
  var isPackageOrVisibleModule = function(id5) {
    return function(v) {
      if (v.nodetype instanceof IsModule) {
        return v.containerID === id5;
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
        return cons2(new Tuple(v.id, v.gridXY))(b2);
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
            vy: v.vy,
            x: 0,
            y: 0,
            gridXY: v1.value0
          };
        }
        ;
        if (v2 instanceof Just) {
          return {
            containerID: v.containerID,
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
            vy: v.vy,
            x: v2.value0.x,
            y: v2.value0.y,
            gridXY: v1.value0
          };
        }
        ;
        throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 289, column 11 - line 291, column 76): " + [v2.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 286, column 7 - line 291, column 76): " + [v1.constructor.name]);
    };
    var modulesWithGrid = map44(setModuleGridXY)(partitioned.no);
    return append15(modulesWithGrid)(partitioned.yes);
  };
  var packageNodesToGridXY = function(nodes) {
    var partitioned = partition(isPackage)(nodes);
    var packageCount = length(partitioned.yes);
    var columns = floor2(ceil(sqrt(toNumber(packageCount))));
    var offset = -(toNumber(columns) / 2);
    var packagesWithGrid = (function() {
      var setGridXY = function(v) {
        return function(i2) {
          var gridXY = scalePoint(200)(200)(offsetXY({
            x: offset,
            y: offset
          })(numberToGridPoint(columns)(i2)));
          return {
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
            y: v.y,
            gridXY: notNull(gridXY)
          };
        };
      };
      return foldlWithIndex3(function(i2) {
        return function(b2) {
          return function(a2) {
            return cons2(setGridXY(a2)(i2))(b2);
          };
        };
      })([])(partitioned.yes);
    })();
    return append15(partitioned.no)(packagesWithGrid);
  };
  var isP2P_Link_ = function(l) {
    return eq22(link_3.linkType(l))(P2P.value);
  };
  var isM2P_Link_ = function(l) {
    return eq22(link_3.linkType(l))(M2P.value);
  };
  var isM2M_Tree_Link_ = function(l) {
    return eq22(link_3.linkType(l))(M2M_Tree.value);
  };
  var isM2M_Graph_Link_ = function(l) {
    return eq22(link_3.linkType(l))(M2M_Graph.value);
  };
  var initialRadius = 10;
  var initialAngle = /* @__PURE__ */ (function() {
    return pi * (3 - sqrt(5));
  })();
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
        vx: v.vx,
        vy: v.vy,
        fx: v.fx,
        fy: v.fy,
        cluster: v.cluster,
        focusX: v.focusX,
        focusY: v.focusY,
        r: v.r,
        x: radius9 * cos(angle),
        y: radius9 * sin(angle)
      };
    };
  };
  var nodesToPhyllotaxis = function(predicate) {
    return function(nodes) {
      var partitioned = partition(predicate)(nodes);
      return append15(partitioned.no)(mapWithIndex(setForPhyllotaxis)(partitioned.yes));
    };
  };
  var packagesNodesToPhyllotaxis = /* @__PURE__ */ nodesToPhyllotaxis(isPackage);
  var fixNamedNodeTo = function(label5) {
    return function(point) {
      return function(nodes) {
        var fixNamedNode$prime = function(v) {
          var $169 = v.name === label5;
          if ($169) {
            return spy7("fixing a node to: ")({
              name: v.name,
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
              y: v.y,
              fx: notNull(point.x),
              fy: notNull(point.y)
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
      return (function(v) {
        return v.r;
      })(unboxD3SimNode2($205));
    },
    id: function($206) {
      return (function(v) {
        return v.id;
      })(unboxD3SimNode2($206));
    },
    loc: function($207) {
      return (function(v) {
        return v.loc;
      })(unboxD3SimNode2($207));
    },
    containerID: function($208) {
      return (function(v) {
        return v.containerID;
      })(unboxD3SimNode2($208));
    },
    containerName: function($209) {
      return (function(v) {
        return v.containerName;
      })(unboxD3SimNode2($209));
    },
    name: function($210) {
      return (function(v) {
        return v.name;
      })(unboxD3SimNode2($210));
    },
    x: function($211) {
      return (function(v) {
        return v.x;
      })(unboxD3SimNode2($211));
    },
    y: function($212) {
      return (function(v) {
        return v.y;
      })(unboxD3SimNode2($212));
    },
    fx: function($213) {
      return (function(v) {
        return v.fx;
      })(unboxD3SimNode2($213));
    },
    fy: function($214) {
      return (function(v) {
        return v.fy;
      })(unboxD3SimNode2($214));
    },
    treeXY: function($215) {
      return (function(v) {
        return v.treeXY;
      })(unboxD3SimNode2($215));
    },
    treeDepth: function($216) {
      return (function(v) {
        return v.treeDepth;
      })(unboxD3SimNode2($216));
    },
    gridXY: function($217) {
      return (function(v) {
        return v.gridXY;
      })(unboxD3SimNode2($217));
    },
    nodetype: function($218) {
      return (function(v) {
        return v.nodetype;
      })(unboxD3SimNode2($218));
    },
    cluster: function($219) {
      return (function(v) {
        return v.cluster;
      })(unboxD3SimNode2($219));
    },
    links: function($220) {
      return (function(v) {
        return v.links;
      })(unboxD3SimNode2($220));
    },
    connected: function($221) {
      return (function(v) {
        return v.connected;
      })(unboxD3SimNode2($221));
    },
    nameAndID: function(d5) {
      return unboxD3SimNode2(d5).name + (" " + show18(unboxD3SimNode2(d5).id));
    },
    indexAndID: function(d5) {
      return unboxD3SimNode2(d5).name + (" " + (show18(getIndexFromDatum_(d5)) + (" " + show18(unboxD3SimNode2(d5).id))));
    },
    namePos: function(d5) {
      return "(" + (show25(floor(datum_3.x(d5))) + ("," + (show25(floor(datum_3.y(d5))) + ")")));
    },
    gridPoint: function(d5) {
      return fromMaybe({
        x: datum_3.x(d5),
        y: datum_3.y(d5)
      })(toMaybe(datum_3.gridXY(d5)));
    },
    gridPointX: function(d5) {
      return (function(v) {
        return v.x;
      })(datum_3.gridPoint(d5));
    },
    gridPointY: function(d5) {
      return (function(v) {
        return v.y;
      })(datum_3.gridPoint(d5));
    },
    treePoint: function(d5) {
      return fromMaybe({
        x: datum_3.x(d5),
        y: datum_3.y(d5)
      })(toMaybe(datum_3.treeXY(d5)));
    },
    treePointX: function(d5) {
      return (function(v) {
        return v.x;
      })(datum_3.treePoint(d5));
    },
    treePointY: function(d5) {
      return (function(v) {
        return v.y;
      })(datum_3.treePoint(d5));
    },
    indexFunction: function($222) {
      return (function(v) {
        return v.id;
      })(unboxD3SimNode2($222));
    },
    positionLabel: function(d5) {
      var v = datum_3.nodetype(d5);
      if (v instanceof IsModule) {
        return -datum_3.radius(d5);
      }
      ;
      if (v instanceof IsPackage) {
        return 0;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 109, column 11 - line 111, column 33): " + [v.constructor.name]);
    },
    collideRadius: function(d5) {
      var $175 = datum_3.id(d5) === datum_3.containerID(d5);
      if ($175) {
        return 10;
      }
      ;
      return datum_3.radius(d5);
    },
    collideRadiusBig: function(d5) {
      return datum_3.radius(d5) + 10;
    },
    nodeClass: function(d5) {
      return show34(datum_3.nodetype(d5)) + (" " + (datum_3.containerName(d5) + (" " + (datum_3.name(d5) + (function() {
        var $176 = datum_3.connected(d5);
        if ($176) {
          return " connected";
        }
        ;
        return "";
      })()))));
    },
    "nodeClass'": function(d5) {
      return "updated" + (show34(datum_3.nodetype(d5)) + (" " + (datum_3.containerName(d5) + (" " + (datum_3.name(d5) + (function() {
        var $177 = datum_3.connected(d5);
        if ($177) {
          return " connected";
        }
        ;
        return "";
      })())))));
    },
    colorByGroup: function(d5) {
      return d3SchemeCategory10N_(toNumber(datum_3.cluster(d5)));
    },
    colorByDepth: function(d5) {
      var v = toMaybe(datum_3.treeDepth(d5));
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
    fillByUsage: function(d5) {
      var $180 = datum_3.connected(d5);
      if ($180) {
        return datum_3.colorByGroup(d5);
      }
      ;
      return "none";
    },
    strokeByUsage: function(d5) {
      var $181 = datum_3.connected(d5);
      if ($181) {
        return "none";
      }
      ;
      return datum_3.colorByGroup(d5);
    },
    colorByUsage: function(d5) {
      var $182 = datum_3.connected(d5);
      if ($182) {
        return "red";
      }
      ;
      return "blue";
    },
    opacityByType: function(d5) {
      var $183 = datum_3.isPackage(d5);
      if ($183) {
        return 0.4;
      }
      ;
      return 0.7;
    },
    translateNode: function(d5) {
      return "translate(" + (show25(datum_3.x(d5)) + ("," + (show25(datum_3.y(d5)) + ")")));
    },
    isNamed: function(name17) {
      return function(v) {
        return function(d5) {
          return datum_3.name(d5) === name17;
        };
      };
    },
    isPackage: function(d5) {
      var v = datum_3.nodetype(d5);
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
    isModule: function(d5) {
      var v = datum_3.nodetype(d5);
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
    isUnusedModule: function(d5) {
      var v = datum_3.nodetype(d5);
      if (v instanceof IsPackage) {
        return false;
      }
      ;
      if (v instanceof IsModule) {
        var $192 = datum_3.connected(d5);
        if ($192) {
          return false;
        }
        ;
        return true;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 154, column 13 - line 158, column 41): " + [v.constructor.name]);
    },
    isUsedModule: function(d5) {
      var v = datum_3.nodetype(d5);
      if (v instanceof IsPackage) {
        return false;
      }
      ;
      if (v instanceof IsModule) {
        var $196 = datum_3.connected(d5);
        if ($196) {
          return true;
        }
        ;
        return false;
      }
      ;
      throw new Error("Failed pattern match at D3.Examples.Spago.Model (line 161, column 13 - line 165, column 42): " + [v.constructor.name]);
    },
    treeChildren: function(d5) {
      return datum_3.links(d5).treeChildren;
    },
    isTreeParent: function(d5) {
      return !$$null(datum_3.treeChildren(d5));
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
  var classed8 = /* @__PURE__ */ classed(toAttrStringFn);
  var radius6 = /* @__PURE__ */ radius(toAttrNumberFn);
  var fill7 = /* @__PURE__ */ fill(toAttrStringFn);
  var strokeColor6 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var strokeWidth6 = /* @__PURE__ */ strokeWidth(toAttrNumber);
  var classed1 = /* @__PURE__ */ classed(toAttrString);
  var x9 = /* @__PURE__ */ x(toAttrNumber);
  var text11 = /* @__PURE__ */ text6(toAttrStringFn);
  var width12 = /* @__PURE__ */ width8(toAttrNumber);
  var height12 = /* @__PURE__ */ height8(toAttrNumber);
  var cursor2 = /* @__PURE__ */ cursor(toAttrString);
  var opacity2 = /* @__PURE__ */ opacity(toAttrNumberFn);
  var y8 = /* @__PURE__ */ y(toAttrNumberFn);
  var textAnchor5 = /* @__PURE__ */ textAnchor(toAttrString);
  var updateAttrs = /* @__PURE__ */ (function() {
    return [classed8(datum_3.nodeClass), transform$prime(datum_3.translateNode)];
  })();
  var treeSceneAttributes = /* @__PURE__ */ (function() {
    return {
      circles: [radius6(datum_3.radius), fill7(datum_3.colorByDepth), strokeColor6(datum_3.colorByGroup), strokeWidth6(3)],
      labels: [classed1("label"), x9(4), y(toAttrNumber)(2), text11(datum_3.name)]
    };
  })();
  var svgAttrs = function(w) {
    return function(h) {
      return [viewBox(-w / 2.1)(-h / 2.05)(w)(h), classed1("overlay"), width12(w), height12(h), cursor2("grab")];
    };
  };
  var graphSceneAttributes = /* @__PURE__ */ (function() {
    return {
      circles: [radius6(datum_3.radius), fill7(datum_3.colorByGroup), opacity2(datum_3.opacityByType)],
      labels: [classed1("label"), x9(0.2), y8(datum_3.positionLabel), textAnchor5("middle"), text11(datum_3.name)]
    };
  })();
  var enterAttrs = /* @__PURE__ */ (function() {
    return [classed8(datum_3.nodeClass), transform$prime(datum_3.translateNode)];
  })();
  var clusterSceneAttributes = /* @__PURE__ */ (function() {
    return {
      circles: [radius6(datum_3.radius), fill7(datum_3.fillByUsage), strokeColor6(datum_3.strokeByUsage), strokeWidth6(3), opacity2(datum_3.opacityByType)],
      labels: [classed1("label"), x9(0.2), y8(datum_3.positionLabel), textAnchor5("middle"), text11(datum_3.name)]
    };
  })();

  // output/Stories.Spago.Actions/index.js
  var NodeClick = /* @__PURE__ */ (function() {
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
  })();
  var TopLevelCSS = /* @__PURE__ */ (function() {
    function TopLevelCSS2(value0) {
      this.value0 = value0;
    }
    ;
    TopLevelCSS2.create = function(value0) {
      return new TopLevelCSS2(value0);
    };
    return TopLevelCSS2;
  })();
  var GraphStyle = /* @__PURE__ */ (function() {
    function GraphStyle2(value0) {
      this.value0 = value0;
    }
    ;
    GraphStyle2.create = function(value0) {
      return new GraphStyle2(value0);
    };
    return GraphStyle2;
  })();
  var PackageGrid = /* @__PURE__ */ (function() {
    function PackageGrid2() {
    }
    ;
    PackageGrid2.value = new PackageGrid2();
    return PackageGrid2;
  })();
  var PackageGraph = /* @__PURE__ */ (function() {
    function PackageGraph2() {
    }
    ;
    PackageGraph2.value = new PackageGraph2();
    return PackageGraph2;
  })();
  var ModuleTree = /* @__PURE__ */ (function() {
    function ModuleTree2(value0) {
      this.value0 = value0;
    }
    ;
    ModuleTree2.create = function(value0) {
      return new ModuleTree2(value0);
    };
    return ModuleTree2;
  })();
  var LayerSwarm = /* @__PURE__ */ (function() {
    function LayerSwarm2() {
    }
    ;
    LayerSwarm2.value = new LayerSwarm2();
    return LayerSwarm2;
  })();
  var LinkShowFilter = /* @__PURE__ */ (function() {
    function LinkShowFilter2(value0) {
      this.value0 = value0;
    }
    ;
    LinkShowFilter2.create = function(value0) {
      return new LinkShowFilter2(value0);
    };
    return LinkShowFilter2;
  })();
  var LinkForceFilter = /* @__PURE__ */ (function() {
    function LinkForceFilter2(value0) {
      this.value0 = value0;
    }
    ;
    LinkForceFilter2.create = function(value0) {
      return new LinkForceFilter2(value0);
    };
    return LinkForceFilter2;
  })();
  var NodeFilter = /* @__PURE__ */ (function() {
    function NodeFilter2(value0) {
      this.value0 = value0;
    }
    ;
    NodeFilter2.create = function(value0) {
      return new NodeFilter2(value0);
    };
    return NodeFilter2;
  })();
  var Initialize7 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var Finalize5 = /* @__PURE__ */ (function() {
    function Finalize7() {
    }
    ;
    Finalize7.value = new Finalize7();
    return Finalize7;
  })();
  var Scene = /* @__PURE__ */ (function() {
    function Scene2(value0) {
      this.value0 = value0;
    }
    ;
    Scene2.create = function(value0) {
      return new Scene2(value0);
    };
    return Scene2;
  })();
  var ToggleForce2 = /* @__PURE__ */ (function() {
    function ToggleForce3(value0) {
      this.value0 = value0;
    }
    ;
    ToggleForce3.create = function(value0) {
      return new ToggleForce3(value0);
    };
    return ToggleForce3;
  })();
  var Filter = /* @__PURE__ */ (function() {
    function Filter2(value0) {
      this.value0 = value0;
    }
    ;
    Filter2.create = function(value0) {
      return new Filter2(value0);
    };
    return Filter2;
  })();
  var ChangeStyling = /* @__PURE__ */ (function() {
    function ChangeStyling2(value0) {
      this.value0 = value0;
    }
    ;
    ChangeStyling2.create = function(value0) {
      return new ChangeStyling2(value0);
    };
    return ChangeStyling2;
  })();
  var ChangeSimConfig = /* @__PURE__ */ (function() {
    function ChangeSimConfig2(value0) {
      this.value0 = value0;
    }
    ;
    ChangeSimConfig2.create = function(value0) {
      return new ChangeSimConfig2(value0);
    };
    return ChangeSimConfig2;
  })();
  var StopSim = /* @__PURE__ */ (function() {
    function StopSim2() {
    }
    ;
    StopSim2.value = new StopSim2();
    return StopSim2;
  })();
  var StartSim = /* @__PURE__ */ (function() {
    function StartSim2() {
    }
    ;
    StartSim2.value = new StartSim2();
    return StartSim2;
  })();
  var EventFromVizualization = /* @__PURE__ */ (function() {
    function EventFromVizualization2(value0) {
      this.value0 = value0;
    }
    ;
    EventFromVizualization2.create = function(value0) {
      return new EventFromVizualization2(value0);
    };
    return EventFromVizualization2;
  })();
  var ToggleChildrenOfNode = /* @__PURE__ */ (function() {
    function ToggleChildrenOfNode2(value0) {
      this.value0 = value0;
    }
    ;
    ToggleChildrenOfNode2.create = function(value0) {
      return new ToggleChildrenOfNode2(value0);
    };
    return ToggleChildrenOfNode2;
  })();
  var SpotlightNode = /* @__PURE__ */ (function() {
    function SpotlightNode2(value0) {
      this.value0 = value0;
    }
    ;
    SpotlightNode2.create = function(value0) {
      return new SpotlightNode2(value0);
    };
    return SpotlightNode2;
  })();
  var UnToggleChildrenOfNode = /* @__PURE__ */ (function() {
    function UnToggleChildrenOfNode2(value0) {
      this.value0 = value0;
    }
    ;
    UnToggleChildrenOfNode2.create = function(value0) {
      return new UnToggleChildrenOfNode2(value0);
    };
    return UnToggleChildrenOfNode2;
  })();

  // output/D3.Examples.Spago.Draw/index.js
  var show19 = /* @__PURE__ */ show(showElement);
  var discard23 = /* @__PURE__ */ discard(discardUnit);
  var classed9 = /* @__PURE__ */ classed(toAttrStringFn);
  var strokeColor7 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var classed12 = /* @__PURE__ */ classed(toAttrString);
  var x13 = /* @__PURE__ */ x1(toAttrNumberFn);
  var y13 = /* @__PURE__ */ y1(toAttrNumberFn);
  var x23 = /* @__PURE__ */ x2(toAttrNumberFn);
  var y23 = /* @__PURE__ */ y2(toAttrNumberFn);
  var updateSimulation = function(dictEq) {
    return function(dictBind) {
      var bind18 = bind(dictBind);
      var discard111 = discard23(dictBind);
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
                    return bind18(openSelection2(v.selections.links.value0)(show19(Line.value)))(function(link4) {
                      return bind18(mergeNewDataWithSim2(node)(keyIsID_)(link4)(keyIsID_)(v.rawdata))(function(merged) {
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
                                                return bind18(updateJoin2(link4)(Line.value)(merged.links)(keyIsID_))(function(link$prime) {
                                                  return bind18(appendTo2(link$prime.enter)(Line.value)([classed9(link_3.linkClass), strokeColor7(link_3.color)]))(function(linkEnter) {
                                                    return discard111(setAttributes2(linkEnter)([classed12("enter")]))(function() {
                                                      return discard111(setAttributes2(link$prime.exit)([remove]))(function() {
                                                        return discard111(setAttributes2(link$prime.update)([classed12("update")]))(function() {
                                                          return bind18(mergeSelections2(linkEnter)(link$prime.update))(function(mergedlinksShown) {
                                                            return discard111(setNodesFromSelection2(mergedNodeSelection))(function() {
                                                              return discard111(setLinksFromSelection2(mergedlinksShown)(v.linksWithForce))(function() {
                                                                return discard111(addTickFunction2("nodes")(new Step3(mergedNodeSelection, [transform$prime(datum_3.translateNode)])))(function() {
                                                                  return addTickFunction2("links")(new Step3(mergedlinksShown, [x13(function($64) {
                                                                    return (function(v2) {
                                                                      return v2.x;
                                                                    })(link_3.source($64));
                                                                  }), y13(function($65) {
                                                                    return (function(v2) {
                                                                      return v2.y;
                                                                    })(link_3.source($65));
                                                                  }), x23(function($66) {
                                                                    return (function(v2) {
                                                                      return v2.x;
                                                                    })(link_3.target($66));
                                                                  }), y23(function($67) {
                                                                    return (function(v2) {
                                                                      return v2.y;
                                                                    })(link_3.target($67));
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
            return bind18(attach2("div.svg-container"))(function(root2) {
              return bind18(appendTo2(root2)(Svg.value)(svgAttrs(v.value0)(v.value1)))(function(svg2) {
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
    return function(d5) {
      return function(t) {
        return new NodeClick(datum_3.nodetype(d5), datum_3.id(d5));
      };
    };
  };

  // output/D3.Data.Graph/index.js
  var bind11 = /* @__PURE__ */ bind(bindMaybe);
  var mapFlipped7 = /* @__PURE__ */ mapFlipped(functorArray);
  var append17 = /* @__PURE__ */ append(semigroupArray);
  var getReachableNodes = function(dictOrd) {
    var lookup14 = lookup9(dictOrd);
    var elem5 = elem2(dictOrd.Eq0());
    return function(id5) {
      return function(graph) {
        var processNextOpenDepPath = function(searchRecord) {
          return bind11(uncons(searchRecord.openDepPaths))(function(x15) {
            return bind11(head(x15.head))(function(firstID) {
              return bind11(lookup14(firstID)(graph))(function(firstNode) {
                var newDeps = partition(function(d5) {
                  return !elem5(d5)(searchRecord.nodes);
                })(firstNode.links.targets);
                var newOpenDepPaths = mapFlipped7(newDeps.yes)(function(d5) {
                  return cons2(d5)(x15.head);
                });
                var prunedLinks = mapFlipped7(newDeps.no)(function(d5) {
                  return new Tuple(firstID, d5);
                });
                var $12 = $$null(newOpenDepPaths);
                if ($12) {
                  return new Just({
                    dependencyTree: searchRecord.dependencyTree,
                    nodes: searchRecord.nodes,
                    openDepPaths: x15.tail,
                    closedDepPaths: cons2(x15.head)(searchRecord.closedDepPaths),
                    redundantLinks: append17(searchRecord.redundantLinks)(prunedLinks)
                  });
                }
                ;
                return new Just({
                  closedDepPaths: searchRecord.closedDepPaths,
                  dependencyTree: searchRecord.dependencyTree,
                  openDepPaths: append17(x15.tail)(newOpenDepPaths),
                  nodes: append17(searchRecord.nodes)(newDeps.yes),
                  redundantLinks: append17(searchRecord.redundantLinks)(prunedLinks)
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
          openDepPaths: [[id5]],
          closedDepPaths: [],
          redundantLinks: [],
          dependencyTree: Nothing.value
        });
      };
    };
  };

  // output/Data.Tree/index.js
  var Node = /* @__PURE__ */ (function() {
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
  })();

  // output/D3.Examples.Spago.Tree/index.js
  var lookup11 = /* @__PURE__ */ lookup2(ordInt);
  var map45 = /* @__PURE__ */ map(functorArray);
  var fromFoldable14 = /* @__PURE__ */ fromFoldable2(foldableArray);
  var foldl5 = /* @__PURE__ */ foldl(foldableList);
  var fromFoldable15 = /* @__PURE__ */ fromFoldable(foldableSet);
  var fromFoldable22 = /* @__PURE__ */ fromFoldable6(foldableList)(/* @__PURE__ */ ordTuple(ordInt)(ordInt));
  var insert10 = /* @__PURE__ */ insert(ordInt);
  var map115 = /* @__PURE__ */ map(functorList);
  var getReachableNodes2 = /* @__PURE__ */ getReachableNodes(ordInt);
  var elem4 = /* @__PURE__ */ elem2(eqInt);
  var elem1 = /* @__PURE__ */ elem2(/* @__PURE__ */ eqTuple(eqInt)(eqInt));
  var append18 = /* @__PURE__ */ append(semigroupArray);
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
          var $47 = {
            x: v1.value0.y - 1200,
            y: v1.value0.x
          };
          return setTreeXYIncludingLeaves(v)({
            x: $47.x,
            y: $47.y,
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
    return fromFoldable14(map45(function($77) {
      return fromFoldable14(reverse($77));
    })(paths));
  };
  var path2Tuples = function($copy_v) {
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
        if (v1 instanceof Cons && v1.value1 instanceof Nil) {
          $tco_done = true;
          return v;
        }
        ;
        if (v1 instanceof Cons && v1.value1 instanceof Cons) {
          $tco_var_v = new Cons(new Tuple(v1.value0, v1.value1.value0), v);
          $copy_v1 = new Cons(v1.value1.value0, v1.value1.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at D3.Examples.Spago.Tree (line 146, column 1 - line 146, column 93): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var makeTreeLinkTuples = function(closedPaths) {
    var linkTuples = foldl5(path2Tuples)(Nil.value)(closedPaths);
    return fromFoldable15(fromFoldable22(linkTuples));
  };
  var getTreeDerivedData = function(root2) {
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
    })(empty3)(descendants_(root2));
  };
  var changeLinkType = function(linktype) {
    return function(v) {
      var $71 = {};
      for (var $72 in v) {
        if ({}.hasOwnProperty.call(v, $72)) {
          $71[$72] = v[$72];
        }
        ;
      }
      ;
      $71.linktype = linktype;
      return $71;
    };
  };
  var buildTree = function(rootID) {
    return function(treelinks) {
      var unwrap9 = function(v) {
        return v;
      };
      var linksWhoseSourceIs = function(id5) {
        return fromFoldable14(map45(function(v) {
          return v.target;
        })(filter(function(l) {
          return l.source === id5;
        })(map45(unwrap9)(treelinks))));
      };
      var go2 = function(childID) {
        return new Node(childID, map115(go2)(linksWhoseSourceIs(childID)));
      };
      return new Node(rootID, map115(go2)(linksWhoseSourceIs(rootID)));
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
      var onlyPackageLinks = filter(isP2P_Link)(model.links);
      var links = append18(treelinks)(append18(prunedTreeLinks)(onlyPackageLinks));
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
        graph: model.graph,
        links,
        nodes: append18(positionedNodes)(treenodes.no),
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
  var x10 = /* @__PURE__ */ x4(toAttrNumber);
  var y9 = /* @__PURE__ */ y4(toAttrNumber);
  var radius7 = /* @__PURE__ */ radius3(toAttrNumberFn);
  var theta2 = /* @__PURE__ */ theta(toAttrNumber);
  var distanceMin2 = /* @__PURE__ */ distanceMin(toAttrNumber);
  var distanceMax2 = /* @__PURE__ */ distanceMax(toAttrNumber);
  var x14 = /* @__PURE__ */ x4(toAttrNumberFn);
  var y14 = /* @__PURE__ */ y4(toAttrNumberFn);
  var radius1 = /* @__PURE__ */ radius3(toAttrNumber);
  var forceLibrary2 = /* @__PURE__ */ (function() {
    var usedModulesOnly = new Just(new ForceFilter("used modules only", datum_3.isUsedModule));
    var useGridXY = function(d5) {
      return function(v) {
        return datum_3.gridPoint(d5);
      };
    };
    var unusedModulesOnly = new Just(new ForceFilter("unused modules only", datum_3.isUnusedModule));
    var treeXY = function(d5) {
      return function(v) {
        return datum_3.treePoint(d5);
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
    return initialize(foldableArray)(functorArray)([createForce("center")(new RegularForce(ForceCenter.value))(allNodes)([strength3(0.5), x10(0), y9(0)]), createForce("x")(new RegularForce(ForceX.value))(allNodes)([strength3(0.05), x10(0)]), createForce("y")(new RegularForce(ForceY.value))(allNodes)([strength3(0.07), y9(0)]), createForce("collide1")(new RegularForce(ForceCollide.value))(allNodes)([strength3(1), radius7(datum_3.collideRadius)]), createForce("collide2")(new RegularForce(ForceCollide.value))(allNodes)([strength3(0.7), radius7(datum_3.collideRadiusBig)]), createForce("charge1")(new RegularForce(ForceManyBody.value))(allNodes)([strength3(-50), theta2(0.9), distanceMin2(1), distanceMax2(infinity)]), createForce("charge2")(new RegularForce(ForceManyBody.value))(allNodes)([strength3(-100), theta2(0.9), distanceMin2(1), distanceMax2(400)]), createForce("chargetree")(new RegularForce(ForceManyBody.value))(treeExceptLeaves)([strength3(-100), theta2(0.9), distanceMin2(1), distanceMax2(400)]), createForce("clusterx_M")(new RegularForce(ForceX.value))(modulesOnly)([strength3(0.2), x14(datum_3.gridPointX)]), createForce("clustery_M")(new RegularForce(ForceY.value))(modulesOnly)([strength3(0.2), y14(datum_3.gridPointY)]), createForce("clusterx_P")(new RegularForce(ForceX.value))(packagesOnly)([strength3(0.8), x14(datum_3.gridPointX)]), createForce("clustery_P")(new RegularForce(ForceY.value))(packagesOnly)([strength3(0.8), y14(datum_3.gridPointY)]), createForce("htreeNodesX")(new RegularForce(ForceX.value))(new Just(new ForceFilter("tree only", function(d5) {
      return datum_3.connected(d5);
    })))([strength3(0.4), x14(datum_3.treePointX)]), createForce("htreeNodesY")(new RegularForce(ForceY.value))(new Just(new ForceFilter("tree only", function(d5) {
      return datum_3.connected(d5);
    })))([strength3(0.4), y14(datum_3.treePointY)]), createForce("vtreeNodesX")(new RegularForce(ForceX.value))(new Just(new ForceFilter("tree only", function(d5) {
      return datum_3.connected(d5);
    })))([strength3(0.4), x14(datum_3.treePointY)]), createForce("vtreeNodesY")(new RegularForce(ForceY.value))(new Just(new ForceFilter("tree only", function(d5) {
      return datum_3.connected(d5);
    })))([strength3(0.4), y14(datum_3.treePointX)]), createForce("packageOrbit")(new RegularForce(ForceRadial.value))(packagesOnly)([strength3(0.7), x10(0), y9(0), radius1(500)]), createForce("unusedOrbit")(new RegularForce(ForceRadial.value))(unusedModulesOnly)([strength3(0.8), x10(0), y9(0), radius1(900)]), createForce("moduleOrbit")(new RegularForce(ForceRadial.value))(usedModulesOnly)([strength3(0.8), x10(0), y9(0), radius1(600)]), createLinkForce(Nothing.value)([strength3(0.5), distance(toAttrNumber)(0), numKey(function($19) {
      return toNumber(datum_3.id($19));
    })])]);
  })();

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

  // output/DemoApp.UI.Button/index.js
  var map47 = /* @__PURE__ */ map(functorArray);
  var append19 = /* @__PURE__ */ append(semigroupArray);
  var rightClasses = /* @__PURE__ */ map47(ClassName)(["rounded-r"]);
  var leftClasses = /* @__PURE__ */ map47(ClassName)(["mr-px", "rounded-l"]);
  var centerClasses = /* @__PURE__ */ map47(ClassName)(["mr-px"]);
  var buttonSharedClasses2 = /* @__PURE__ */ map47(ClassName)(["no-outline", "px-4", "py-2", "!active:border-b", "active:border-t", "disabled:opacity-50", "disabled:cursor-default", "!disabled:cursor-pointer"]);
  var buttonPrimaryClasses = /* @__PURE__ */ map47(ClassName)(["bg-blue-88", "border-blue-88", "hover:!disabled:bg-blue-82", "focus:bg-blue-82", "text-white"]);
  var buttonGroupClasses2 = /* @__PURE__ */ map47(ClassName)(["flex", "items-center"]);
  var buttonGroupBuilder2 = function(classes2) {
    return function(iprops) {
      return button(appendIProps([classes(append19(buttonSharedClasses2)(classes2))])(iprops));
    };
  };
  var buttonPrimaryCenter = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append19(buttonPrimaryClasses)(centerClasses));
  var buttonPrimaryLeft = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append19(buttonPrimaryClasses)(leftClasses));
  var buttonPrimaryRight = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append19(buttonPrimaryClasses)(rightClasses));
  var buttonGroup2 = function(iprops) {
    return div2(appendIProps([classes(buttonGroupClasses2)])(iprops));
  };
  var buttonGroup_ = /* @__PURE__ */ buttonGroup2([]);
  var buttonClasses2 = /* @__PURE__ */ map47(ClassName)(["bg-grey-50-a20", "border-grey-50-a20", "hover:!disabled:bg-grey-50-a30", "focus:bg-grey-50-a30", "text-black-20"]);
  var buttonLeft = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append19(buttonClasses2)(leftClasses));
  var buttonRight = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append19(buttonClasses2)(rightClasses));
  var buttonCenter = /* @__PURE__ */ buttonGroupBuilder2(/* @__PURE__ */ append19(buttonClasses2)(centerClasses));

  // output/DemoApp.UI.Checkbox/index.js
  var map48 = /* @__PURE__ */ map(functorArray);
  var append20 = /* @__PURE__ */ append(semigroupArray);
  var type_20 = /* @__PURE__ */ type_(isPropInputType);
  var labelClasses4 = /* @__PURE__ */ map48(ClassName)(["flex", "flex-row", "inline-block", "py-2", "cursor-pointer", "text-black-20", "items-center", "text-left"]);
  var inputClasses2 = /* @__PURE__ */ map48(ClassName)(["!disabled:sibling:bg-white", "disabled:sibling:bg-grey-95", "checked:sibling:before:opacity-100", "checked:sibling:before:scale-1", "checked:!disabled:sibling:border-blue-88", "focus:sibling:border-blue-88", "!checked:sibling:before:opacity-0", "!checked:sibling:before:scale-0", "!focus:hover:!checked:!disabled:sibling:border-grey-70", "focus:sibling:shadow", "checked:!disabled:sibling:before:bg-blue-88", "checked:disabled:sibling:before:bg-grey-80", "checked:disabled:sibling:border-grey-80", "offscreen", "checked:sibling:after:opacity-100", "checked:sibling:after:scale-1", "!checked:sibling:after:opacity-0", "!checked:sibling:after:scale-0"]);
  var checkboxClasses = /* @__PURE__ */ map48(ClassName)(["relative", "content-box", "border-2", "border-solid", "h-5", "w-5", "flex-none", "no-content", "mr-3", "rounded", "before:transition-1/4-bounce", "before:absolute", "before:h-full", "before:w-full", "before:no-content", "after:transition-1/4-bounce", "after:absolute", "after:w-full", "after:h-2", "after:border-l-2", "after:border-b-2", "after:border-white", "after:no-content", "after:rotate-315", "after:shadow"]);
  var checkbox = function(iprops) {
    return function(inprops) {
      return function(html2) {
        return label(appendIProps([classes(labelClasses4)])(iprops))(append20([input(appendIProps([classes(inputClasses2), type_20(InputCheckbox.value)])(inprops)), span2([classes(checkboxClasses)])([])])(html2));
      };
    };
  };
  var checkbox_ = /* @__PURE__ */ checkbox([]);

  // output/DemoApp.UI.Builder/index.js
  var blockBuilder = function(elem5) {
    return function(classes2) {
      return function(iprops) {
        return elem5(appendIProps([classes(classes2)])(iprops));
      };
    };
  };

  // output/DemoApp.UI.Table/index.js
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
  var x11 = /* @__PURE__ */ x(toAttrNumber);
  var _handle3 = /* @__PURE__ */ _handle(strongForget);
  var prop20 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "staging";
    }
  })()();
  var prop111 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "scene";
    }
  })()();
  var prop26 = /* @__PURE__ */ prop3({
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
      callback: x11(0),
      nodeInitializerFunctions: []
    };
  };
  var getSimulationVariables = function(state3) {
    var handle = view(_handle3)(state3);
    return readSimulationVariables(handle);
  };
  var _staging = function(dictStrong) {
    return prop20($$Proxy.value)(dictStrong);
  };
  var _scene = function(dictStrong) {
    return prop111($$Proxy.value)(dictStrong);
  };
  var _sceneAttributes = function(dictStrong) {
    var $125 = _scene(dictStrong);
    var $126 = prop26($$Proxy.value)(dictStrong);
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
      return function(label5) {
        var $149 = at3(label5)(dictStrong);
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

  // output/Stories.Spago.HTML/index.js
  var type_21 = /* @__PURE__ */ type_(isPropInputType);
  var value15 = /* @__PURE__ */ value(isPropString);
  var map50 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
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
      }), type_21(InputRange.value), id2(config.id), class_("scaling-slider"), min3(config.min), max3(config.max), step2(new Step(config.step)), value15(show26(config.value))];
    };
  };
  var slider1 = /* @__PURE__ */ slider(showNumber);
  var renderTableForces = function(state3) {
    var tableData = map50(snd)(toUnfoldable6(view(_forceLibrary3)(state3)));
    var renderHeader = row_([header2([css("w-10")])([text("Active")]), header2([css("w-2/3 text-left")])([text("Details")]), header2([css("w-2/3 text-left")])([text("Acting on...")])]);
    var renderData = function(v) {
      return [cell_([checkbox_([checked(eq6(v.status)(ForceActive.value)), onChecked($$const(new ToggleForce2(v.name)))])([])]), cell([css("text-left")])([div_([text(v.name + ("\n" + showType(v.type)))])]), cell([css("text-left")])([text(showForceFilter(v.filter))])];
    };
    var renderBody = map50(row_)(map50(renderData)(tableData));
    var renderTable = table_2(append110([renderHeader])(renderBody));
    return div_([div2([tailwindClass("text-sm")])([backdrop_([div_([h2_([text("Control which forces are acting")]), renderTable])])])]);
  };
  var renderSimState = function(state3) {
    return div2([classes(["m-6"])])([caption_([text("Simulation state")]), p_([text("class: " + view(_cssClass2)(state3))]), p_([text("link count: " + show20(length(view(_stagingLinks2)(state3))))]), p_([text("node count:" + show20(length(view(_stagingNodes2)(state3))))])]);
  };
  var renderSimControls = function(state3) {
    var params = getSimulationVariables(state3);
    return div2([classes(["m-6"])])([subHeading_([text("Simulation controls")]), div2([classes(["mb-6"])])([contentHeading_([text("Scenes")]), buttonGroup_([buttonPrimaryLeft([onClick($$const(new Scene(PackageGrid.value)))])([text("Package Grid")]), buttonPrimaryRight([onClick($$const(new Scene(PackageGraph.value)))])([text("Package Graph")])])]), div2([classes(["mb-6"])])([buttonGroup_([buttonPrimaryLeft([onClick($$const(new Scene(new ModuleTree(Horizontal.value))))])([text("Horiz. Tree")]), buttonPrimaryCenter([onClick($$const(new Scene(new ModuleTree(Vertical.value))))])([text("Vert. Tree")]), buttonPrimaryRight([onClick($$const(new Scene(new ModuleTree(Radial.value))))])([text("Radial Tree")])])]), div2([classes(["mb-6"])])([buttonGroup_([buttonPrimaryLeft([onClick($$const(new Scene(LayerSwarm.value)))])([text("LayerSwarm")])])]), div2([classes(["mb-6"])])([contentHeading_([text("Params")]), input(slider1({
      "var": Alpha.create,
      id: "alpha-slider",
      min: 0,
      max: 100,
      step: 10,
      value: params.alpha * 100
    })), caption_([text("Alpha: " + show110(params.alpha))]), input(slider1({
      "var": AlphaTarget.create,
      id: "alphatarget-slider",
      min: 0,
      max: 100,
      step: 10,
      value: params.alphaTarget * 100
    })), caption_([text("AlphaTarget: " + show110(params.alphaTarget))])]), div2([classes(["mb-6"])])([buttonGroup_([buttonPrimaryLeft([onClick($$const(StopSim.value))])([text("Stop")]), buttonPrimaryCenter([onClick($$const(new ChangeSimConfig(new AlphaTarget(0.3))))])([text("Heat")]), buttonPrimaryCenter([onClick($$const(new ChangeSimConfig(new AlphaTarget(0))))])([text("Cool")]), buttonPrimaryRight([onClick($$const(StartSim.value))])([text("Start")])])]), div2([classes(["mb-6"])])([contentHeading_([text("Which nodes should be displayed?")]), buttonGroup_([buttonLeft([onClick($$const(new Filter(new NodeFilter(isPackage))))])([text("Packages")]), buttonCenter([onClick($$const(new Filter(new NodeFilter($$const(true)))))])([text("Both")]), buttonCenter([onClick($$const(new Filter(new NodeFilter($$const(false)))))])([text("Neither")]), buttonRight([onClick($$const(new Filter(new NodeFilter(isUsedModule))))])([text("Modules")])])]), div2([classes(["mb-6"])])([contentHeading_([text("Put links into simulation")]), buttonGroup_([buttonLeft([onClick($$const(new Filter(new LinkShowFilter(isM2M_Tree_Link))))])([text("Treelink")]), buttonCenter([onClick($$const(new Filter(new LinkShowFilter(isM2M_Graph_Link))))])([text("Graphlink")]), buttonCenter([onClick($$const(new Filter(new LinkShowFilter(isM2P_Link))))])([text("M2P")]), buttonCenter([onClick($$const(new Filter(new LinkShowFilter(isP2P_Link))))])([text("P2P")]), buttonRight([onClick($$const(new Filter(new LinkShowFilter($$const(false)))))])([text("none")])])]), div2([classes(["mb-6"])])([contentHeading_([text("Limit only these links to exert force?")]), buttonGroup_([buttonLeft([onClick($$const(new Filter(new LinkForceFilter(isM2M_Tree_Link_))))])([text("Treelink")]), buttonCenter([onClick($$const(new Filter(new LinkForceFilter(isM2M_Graph_Link_))))])([text("Graphlink")]), buttonCenter([onClick($$const(new Filter(new LinkForceFilter(isM2P_Link_))))])([text("M2P")]), buttonRight([onClick($$const(new Filter(new LinkForceFilter(isP2P_Link_))))])([text("P2P")])])]), div2([classes(["mb-6"])])([contentHeading_([text("D3 attributes chosen")]), buttonGroup_([buttonLeft([onClick($$const(new ChangeStyling(new GraphStyle(clusterSceneAttributes))))])([text("Clusters")]), buttonCenter([onClick($$const(new ChangeStyling(new GraphStyle(graphSceneAttributes))))])([text("Graph")]), buttonRight([onClick($$const(new ChangeStyling(new GraphStyle(treeSceneAttributes))))])([text("Tree")])])]), div2([classes(["mb-6"])])([contentHeading_([text("Stylesheet")]), buttonGroup_([buttonLeft([onClick($$const(new ChangeStyling(new TopLevelCSS("cluster"))))])([text("Clusters")]), buttonCenter([onClick($$const(new ChangeStyling(new TopLevelCSS("graph"))))])([text("Graph")]), buttonCenter([onClick($$const(new ChangeStyling(new TopLevelCSS("tree"))))])([text("Tree")]), buttonRight([onClick($$const(new ChangeStyling(new TopLevelCSS("none"))))])([text("None")])])])]);
  };
  var blurbtext5 = /* @__PURE__ */ (function() {
    var titleClasses = map50(ClassName)(["font-bold text-2xl"]);
    var title4 = h2([classes(titleClasses)])([text("About this Example")]);
    var paraTexts = map50(function(s) {
      return [text(s)];
    })(["This example synthesizes a complex dependency graph from the optional JSON\n        graph outputs of the PureScript compiler, together with the package\n        dependencies from Spago and adds simple line-count per module to give an\n        idea of the size of each one.", "With this dataset, operated on by the physics simulation engine, we can\n      explore different aspects of the project dependencies. The layout can be\n      entirely driven by forces and relationships or partially or totally laid-out\n      using algorithms.", "For example, a dependency tree starting at the Main module can be laid-out as\n      a radial tree and either fixed in that position or allowed to move under the\n      influences of other forces.", "Un-connected modules (which are only present because something in their\n      package has been required) can be hidden or clustered separately.", "Modules can be clustered on their packages and the packages can be positioned\n      on a simple grid or arranged in a circle by a radial force that applies only\n      to them.", "Clicking on a module highlights it and its immediate dependents and\n      dependencies. Clicking outside the highlighted module undoes the\n      highlighting."]);
    var paraClasses = map50(ClassName)(["m-4 "]);
    var paras = map50(p([classes(paraClasses)]))(paraTexts);
    return div_(cons2(title4)(paras));
  })();
  var render2 = function(state3) {
    return div2([tailwindClass("story-container spago")])([div2([tailwindClass("story-panel-about")])([card_([blurbtext5]), renderSimControls(state3), renderSimState(state3), renderTableForces(state3)]), div2([tailwindClass("svg-container " + view(_cssClass2)(state3))])([])]);
  };

  // output/Stories.Spago/index.js
  var _linksShown2 = /* @__PURE__ */ _linksShown(strongForget);
  var _linksActive2 = /* @__PURE__ */ _linksActive(strongForget);
  var _chooseNodes2 = /* @__PURE__ */ _chooseNodes(strongForget);
  var _nodeInitializerFunctions2 = /* @__PURE__ */ _nodeInitializerFunctions(strongForget);
  var discard24 = /* @__PURE__ */ discard(discardUnit);
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
  var discard110 = /* @__PURE__ */ discard24(bindD3SimM);
  var stop2 = /* @__PURE__ */ stop(simulationMD3Selection_D3);
  var actualizeForces3 = /* @__PURE__ */ actualizeForces(simulationMD3Selection_D3);
  var updateSimulation2 = /* @__PURE__ */ updateSimulation(eqInt)(bindD3SimM)(monadEffD3SimM)(selectionMD3Selection_D3S)(simulationMD3Selection_D3);
  var setConfigVariable3 = /* @__PURE__ */ setConfigVariable(simulationMD3Selection_D3);
  var start5 = /* @__PURE__ */ start2(simulationMD3Selection_D3);
  var bind16 = /* @__PURE__ */ bind(bindMaybe);
  var lookup13 = /* @__PURE__ */ lookup2(ordString);
  var pure20 = /* @__PURE__ */ pure(applicativeMaybe);
  var bind17 = /* @__PURE__ */ bind(bindAff);
  var apply3 = /* @__PURE__ */ apply(applyEither);
  var map51 = /* @__PURE__ */ map(functorEither);
  var pure110 = /* @__PURE__ */ pure(applicativeAff);
  var bind22 = /* @__PURE__ */ bind(bindHalogenM);
  var discard25 = /* @__PURE__ */ discard24(bindHalogenM);
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
    var discard32 = discard24(Bind1);
    var assign1 = assign2(dictMonadState);
    return bind32(get(dictMonadState))(function(state3) {
      return bind32(use3(_linksShown2))(function(linksShown) {
        return bind32(use3(_linksActive2))(function(linksActive) {
          return bind32(use3(_chooseNodes2))(function(chooseNodes) {
            return bind32(use3(_nodeInitializerFunctions2))(function(nodeInitializerFunctions) {
              return discard32(assign1(_stagingLinks3)(filter(linksShown)(view(_modelLinks2)(state3))))(function() {
                return discard32(assign1(_stagingLinkFilter2)(linksActive))(function() {
                  var rawnodes = filter(chooseNodes)(view(_modelNodes2)(state3));
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
      return function(d5) {
        return function(t) {
          return liftEffect10(notify(l)(new EventFromVizualization(getVizEventFromClick(e)(d5)(t))));
        };
      };
    });
  };
  var runSimulation = function(dictMonadEffect) {
    var Bind1 = dictMonadEffect.Monad0().Bind1();
    var discard32 = discard24(Bind1);
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
                  labels: sceneAttributes.labels,
                  circles: cons2(callback)(sceneAttributes.circles)
                };
                return runWithD3_Simulation22(discard110(stop2)(function() {
                  return discard110(actualizeForces3(forceStatuses))(function() {
                    return discard110(updateSimulation2(staging)(attributesWithCallback))(function() {
                      return discard110(setConfigVariable3(new Alpha(1)))(function() {
                        return start5;
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
  var readModelData = /* @__PURE__ */ (function() {
    return bind17(get4(string)("./data/spago-data/modules.json"))(function(moduleJSON) {
      return bind17(get4(string)("./data/spago-data/packages.json"))(function(packageJSON) {
        return bind17(get4(string)("./data/spago-data/lsdeps.jsonlines"))(function(lsdepJSON) {
          return bind17(get4(string)("./data/spago-data/LOC.json"))(function(locJSON) {
            var model = hush(apply3(apply3(apply3(map51(convertFilesToGraphModel)(moduleJSON))(packageJSON))(lsdepJSON))(locJSON));
            return pure110(addTreeToModel("Main")(model));
          });
        });
      });
    });
  })();
  var handleAction6 = function(dictMonadAff) {
    var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
    var monadEffectHalogenM2 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    var evalEffectSimulation1 = evalEffectSimulation2(monadEffectHalogenM2);
    var liftEffect12 = liftEffect(monadEffectHalogenM2);
    var runSimulation1 = runSimulation(monadEffectHalogenM2)(monadStateHalogenM);
    var runWithD3_Simulation1 = runWithD3_Simulation2(monadEffectHalogenM2);
    return function(v) {
      if (v instanceof Initialize7) {
        return bind22(liftAff2(readModelData))(function(v1) {
          return discard25(modifying2(_model2)($$const(v1)))(function() {
            return bind22(evalEffectSimulation1(initialize3))(function(openSelections) {
              return discard25(modifying2(function($143) {
                return _staging1(_enterselections2(_nodes2($143)));
              })($$const(openSelections.nodes)))(function() {
                return discard25(modifying2(function($144) {
                  return _staging1(_enterselections2(_links2($144)));
                })($$const(openSelections.links)))(function() {
                  return bind22(liftEffect12(create3))(function(v2) {
                    return discard25($$void9(subscribe2(v2.emitter)))(function() {
                      return discard25(assign3(_callback1)(simulationEvent(v2.listener)))(function() {
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
      if (v instanceof Finalize5) {
        return pure23(unit);
      }
      ;
      if (v instanceof EventFromVizualization) {
        if (v.value0.value0 instanceof IsPackage) {
          return handleAction6(dictMonadAff)(new ToggleChildrenOfNode(v.value0.value1));
        }
        ;
        if (v.value0.value0 instanceof IsModule) {
          return handleAction6(dictMonadAff)(new SpotlightNode(v.value0.value1));
        }
        ;
        throw new Error("Failed pattern match at Stories.Spago (line 90, column 5 - line 92, column 68): " + [v.value0.constructor.name]);
      }
      ;
      if (v instanceof ToggleChildrenOfNode) {
        return discard25(assign3(_chooseNodes1)(isPackageOrVisibleModule(v.value0)))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof UnToggleChildrenOfNode) {
        return discard25(assign3(_chooseNodes1)(isPackage))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof SpotlightNode) {
        return runWithD3_Simulation1(stop2);
      }
      ;
      if (v instanceof Scene && v.value0 instanceof PackageGrid) {
        return discard25(assign3(_chooseNodes1)(allNodes2))(function() {
          return discard25(assign3(_linksShown1)(isM2P_Link))(function() {
            return discard25(assign3(_linksActive1)($$const(true)))(function() {
              return discard25(assign3(_cssClass3)("cluster"))(function() {
                return discard25(assign3(_sceneAttributes1)(clusterSceneAttributes))(function() {
                  return discard25(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["clusterx_P", "clustery_P", "clusterx_M", "clustery_M", "collide2"])))(function() {
                    return discard25(assign3(_nodeInitializerFunctions1)([unpinAllNodes, packageNodesToGridXY, moduleNodesToContainerXY]))(function() {
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
        return discard25(assign3(_chooseNodes1)(isPackage))(function() {
          return discard25(assign3(_linksShown1)(isP2P_Link))(function() {
            return discard25(assign3(_linksActive1)(sourcePackageIs("my-project")))(function() {
              return discard25(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["center", "collide2", "charge2", "packageOrbit", linksForceName])))(function() {
                return discard25(assign3(_cssClass3)("graph"))(function() {
                  return discard25(assign3(_sceneAttributes1)(graphSceneAttributes))(function() {
                    return discard25(assign3(_nodeInitializerFunctions1)([unpinAllNodes, packagesNodesToPhyllotaxis, fixNamedNodeTo("my-project")({
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
        return discard25(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard25(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard25(assign3(_linksActive1)($$const(true)))(function() {
              return discard25(assign3(_cssClass3)("tree"))(function() {
                return discard25(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard25(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["htreeNodesX", "collide1", "y", linksForceName])))(function() {
                    return discard25(assign3(_nodeInitializerFunctions1)([unpinAllNodes]))(function() {
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
        return discard25(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard25(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard25(assign3(_linksActive1)($$const(true)))(function() {
              return discard25(assign3(_cssClass3)("tree radial"))(function() {
                return discard25(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard25(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["center", "collide2", "chargetree", "charge2", linksForceName])))(function() {
                    return discard25(assign3(_nodeInitializerFunctions1)([unpinAllNodes, treeNodesToTreeXY_R, fixNamedNodeTo("Main")({
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
        return discard25(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard25(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard25(assign3(_linksActive1)($$const(false)))(function() {
              return discard25(assign3(_cssClass3)("tree horizontal"))(function() {
                return discard25(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard25(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["htreeNodesX", "htreeNodesY", "charge1", "collide2"])))(function() {
                    return discard25(assign3(_nodeInitializerFunctions1)([unpinAllNodes]))(function() {
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
        return discard25(assign3(_chooseNodes1)(isUsedModule))(function() {
          return discard25(assign3(_linksShown1)(isM2M_Tree_Link))(function() {
            return discard25(assign3(_linksActive1)($$const(false)))(function() {
              return discard25(assign3(_cssClass3)("tree vertical"))(function() {
                return discard25(assign3(_sceneAttributes1)(treeSceneAttributes))(function() {
                  return discard25(modifying2(_forceStatuses12)(onlyTheseForcesActive2(["vtreeNodesX", "vtreeNodesY", "charge1", "collide2"])))(function() {
                    return discard25(assign3(_nodeInitializerFunctions1)([unpinAllNodes]))(function() {
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
        return discard25(modifying2(_forceStatus4(v.value0))(toggleForceStatus))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof Filter && v.value0 instanceof LinkShowFilter) {
        return discard25(assign3(_linksShown1)(v.value0.value0))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof Filter && v.value0 instanceof LinkForceFilter) {
        return discard25(assign3(_linksActive1)(v.value0.value0))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof Filter && v.value0 instanceof NodeFilter) {
        return discard25(assign3(_chooseNodes1)(v.value0.value0))(function() {
          return runSimulation1;
        });
      }
      ;
      if (v instanceof ChangeStyling && v.value0 instanceof TopLevelCSS) {
        return assign3(_cssClass3)(v.value0.value0);
      }
      ;
      if (v instanceof ChangeStyling && v.value0 instanceof GraphStyle) {
        return discard25(assign3(_sceneAttributes1)(v.value0.value0))(function() {
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
          return start5;
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
  var component6 = function(dictMonadAff) {
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
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        handleAction: handleAction6(dictMonadAff),
        initialize: new Just(Initialize7.value),
        finalize: new Just(Finalize5.value)
      })
    });
  };

  // output/D3.Examples.ThreeLittleCircles/index.js
  var fill8 = /* @__PURE__ */ fill(toAttrString);
  var cx3 = /* @__PURE__ */ cx(toAttrNumberFnI);
  var cy3 = /* @__PURE__ */ cy(toAttrNumber);
  var radius8 = /* @__PURE__ */ radius(toAttrNumber);
  var classed10 = /* @__PURE__ */ classed(toAttrString);
  var discard26 = /* @__PURE__ */ discard(discardUnit);
  var strokeColor8 = /* @__PURE__ */ strokeColor(toAttrStringFn);
  var strokeWidth7 = /* @__PURE__ */ strokeWidth(toAttrNumber);
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
    var discard111 = discard26(Bind1);
    var setAttributes2 = setAttributes(dictSelectionM);
    var pure21 = pure(Monad0.Applicative0());
    return function(selector) {
      var circleAttributes = [fill8("green"), cx3(xFromIndex), cy3(50), radius8(20)];
      return bind18(attach2(selector))(function(root2) {
        return bind18(appendTo2(root2)(Svg.value)([viewBox(-100)(-100)(650)(650), classed10("d3svg gup")]))(function(svg2) {
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
    y: function(d5) {
      return 100 - toNumber(d5) / 5;
    },
    color: function(d5) {
      return d3SchemePairedN_(toNumber(d5) / 100);
    }
  };
  var drawWithData = function(dictSelectionM) {
    var Monad0 = dictSelectionM.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind18 = bind(Bind1);
    var attach2 = attach(dictSelectionM);
    var appendTo2 = appendTo(dictSelectionM);
    var simpleJoin2 = simpleJoin(dictSelectionM);
    var discard111 = discard26(Bind1);
    var setAttributes2 = setAttributes(dictSelectionM);
    var pure21 = pure(Monad0.Applicative0());
    return function(circleData) {
      return function(selector) {
        var circleAttributes = [strokeColor8(datum_4.color), strokeWidth7(3), fill8("none"), cx3(datum_4.x), cy1(datum_4.y), radius8(10)];
        return bind18(attach2(selector))(function(root2) {
          return bind18(appendTo2(root2)(Svg.value)([viewBox(-100)(-100)(650)(650), classed10("d3svg gup")]))(function(svg2) {
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
  var prop21 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "parabola";
    }
  })()();
  var prop112 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "simple";
    }
  })()();
  var not10 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var traverse4 = /* @__PURE__ */ traverse(traversableArray);
  var discard27 = /* @__PURE__ */ discard(discardUnit);
  var drawThreeCircles2 = /* @__PURE__ */ drawThreeCircles(d3TaglessD3M);
  var removeExistingSVG4 = /* @__PURE__ */ removeExistingSVG(d3TaglessD3M);
  var drawWithData2 = /* @__PURE__ */ drawWithData(d3TaglessD3M);
  var Initialize8 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var Finalize6 = /* @__PURE__ */ (function() {
    function Finalize7() {
    }
    ;
    Finalize7.value = new Finalize7();
    return Finalize7;
  })();
  var ToggleCard6 = /* @__PURE__ */ (function() {
    function ToggleCard8(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard8.create = function(value0) {
      return new ToggleCard8(value0);
    };
    return ToggleCard8;
  })();
  var ToggleExample = /* @__PURE__ */ (function() {
    function ToggleExample2() {
    }
    ;
    ToggleExample2.value = new ToggleExample2();
    return ToggleExample2;
  })();
  var simple = /* @__PURE__ */ (function() {
    return [new Blurb("Simplest possible example, just to show syntax."), new SnippetFile("TLCSimple"), new Blurb("Click the button to see a slightly more realistic example."), new PreRendered(buttonVertical([onClick($$const(ToggleExample.value))])([text("Simple")]))];
  })();
  var parabola = /* @__PURE__ */ (function() {
    return [new Blurb("This extends the super-simple model in the direction one would go for a more real-world example."), new SnippetFile("TLCParabola"), new Blurb("In this example, the data is passed in and must match the type\n  specified in the Model. Because the data loses its type information when\n  put into D3 we recover the type of Datum and Index using a couple of\n  functions to wrap unsafeCoerce. These functions are then used to write\n  any attribute setters that are derived from the data elements themselves,\n  or their indices"), new SnippetFile("TLCDatum_"), new Blurb("Again, we're just showing syntax and shape of the DSL here: it's still extremely simple, and the Model,\n  datum_ and so on might not be needed for such a simple example."), new PreRendered(buttonVertical([onClick($$const(ToggleExample.value))])([text("Parabola")]))];
  })();
  var _notebooks = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "notebooks";
      }
    })()()($$Proxy.value);
  })();
  var _parabola = function(dictStrong) {
    var $73 = _notebooks(dictStrong);
    var $74 = prop21($$Proxy.value)(dictStrong);
    return function($75) {
      return $73($74($75));
    };
  };
  var _parabola1 = /* @__PURE__ */ _parabola(strongFn);
  var _parabola2 = /* @__PURE__ */ _parabola(strongForget);
  var _simple = function(dictStrong) {
    var $76 = _notebooks(dictStrong);
    var $77 = prop112($$Proxy.value)(dictStrong);
    return function($78) {
      return $76($77($78));
    };
  };
  var _simple1 = /* @__PURE__ */ _simple(strongFn);
  var _simple2 = /* @__PURE__ */ _simple(strongForget);
  var handleAction7 = function(dictBind) {
    var bind18 = bind(dictBind);
    var substituteSnippetCells2 = substituteSnippetCells(dictBind);
    var discard111 = discard27(dictBind);
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
          if (v instanceof ToggleCard6) {
            return modifying3(v.value0(strongFn))(not10);
          }
          ;
          if (v instanceof Initialize8) {
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
                return discard111($$void10(liftEffect11(eval_D3M((function() {
                  if (toggle$prime) {
                    return drawThreeCircles2("div.svg-container");
                  }
                  ;
                  return drawWithData2([310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310])("div.svg-container");
                })()))))(function() {
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
          if (v instanceof Finalize6) {
            return pure21(unit);
          }
          ;
          throw new Error("Failed pattern match at Stories.ThreeLittleCircles (line 90, column 16 - line 112, column 24): " + [v.constructor.name]);
        };
      };
    };
  };
  var handleAction16 = /* @__PURE__ */ handleAction7(bindHalogenM);
  var _code6 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "code";
      }
    })()()($$Proxy.value);
  })();
  var component7 = function(dictMonadAff) {
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-code")])([field_({
        label: text("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked(toBoolean(state3.code)), onChange(function(v) {
        return new ToggleCard6(function(dictStrong) {
          return _code6(dictStrong);
        });
      })])]), content_(state3.code)((function() {
        if (state3.toggle) {
          return renderNotebook_(view(_simple2)(state3));
        }
        ;
        return renderNotebook_(view(_parabola2)(state3));
      })())]), div2([tailwindClass("svg-container")])([])]);
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
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        handleAction: handleAction16(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        initialize: new Just(Initialize8.value),
        finalize: new Just(Finalize6.value)
      })
    });
  };

  // output/DemoApp.UI.Radio/index.js
  var map52 = /* @__PURE__ */ map(functorArray);
  var append21 = /* @__PURE__ */ append(semigroupArray);
  var type_22 = /* @__PURE__ */ type_(isPropInputType);
  var radioClasses = /* @__PURE__ */ map52(ClassName)(["inline-flex", "justify-center", "items-center", "content-box", "border-2", "border-solid", "h-4", "w-4", "p-1", "flex-none", "no-content", "rounded-full", "mr-3", "before:transition-1/4-bounce", "before:h-full", "before:w-full", "before:bg-blue-88", "before:no-content", "before:rounded-full", "before:shadow"]);
  var labelClasses5 = /* @__PURE__ */ map52(ClassName)(["flex", "flex-row", "inline-block", "py-2", "cursor-pointer", "text-black-20", "items-center", "text-left"]);
  var inputClasses3 = /* @__PURE__ */ map52(ClassName)(["!disabled:sibling:bg-white", "disabled:sibling:bg-grey-95", "checked:sibling:before:opacity-100", "checked:sibling:before:scale-1", "checked:!disabled:sibling:border-blue-88", "focus:sibling:border-blue-88", "!checked:sibling:before:opacity-0", "!checked:sibling:before:scale-0", "!focus:hover:!checked:!disabled:sibling:border-grey-70", "focus:sibling:shadow", "checked:!disabled:sibling:before:bg-blue-88", "checked:disabled:sibling:before:bg-grey-80", "checked:disabled:sibling:border-grey-80", "offscreen"]);
  var radio = function(iprops) {
    return function(inprops) {
      return function(html2) {
        return label(appendIProps([classes(labelClasses5)])(iprops))(append21([input(appendIProps([classes(inputClasses3), type_22(InputRadio.value)])(inprops)), span2([classes(radioClasses)])([])])(html2));
      };
    };
  };

  // output/Stories.Trees/index.js
  var prop27 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "draw";
    }
  })()();
  var not11 = /* @__PURE__ */ not(heytingAlgebraStatus);
  var removeExistingSVG5 = /* @__PURE__ */ removeExistingSVG(d3TaglessD3M);
  var discard28 = /* @__PURE__ */ discard(discardUnit);
  var makeModel4 = /* @__PURE__ */ makeModel(bindAff)(monadEffectAff);
  var prop113 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "code";
    }
  })()();
  var prop28 = /* @__PURE__ */ prop3({
    reflectSymbol: function() {
      return "blurb";
    }
  })()();
  var Initialize9 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
  })();
  var SetLayout = /* @__PURE__ */ (function() {
    function SetLayout2(value0) {
      this.value0 = value0;
    }
    ;
    SetLayout2.create = function(value0) {
      return new SetLayout2(value0);
    };
    return SetLayout2;
  })();
  var SetType = /* @__PURE__ */ (function() {
    function SetType2(value0) {
      this.value0 = value0;
    }
    ;
    SetType2.create = function(value0) {
      return new SetType2(value0);
    };
    return SetType2;
  })();
  var ToggleCard7 = /* @__PURE__ */ (function() {
    function ToggleCard8(value0) {
      this.value0 = value0;
    }
    ;
    ToggleCard8.create = function(value0) {
      return new ToggleCard8(value0);
    };
    return ToggleCard8;
  })();
  var blurbtext6 = /* @__PURE__ */ blurbParagraphs(functorArray)(["An abstract data type like a tree can be rendered in a number of different\n    ways including at least the 6 variations shown here, arising from a\n    combination of three layout orientations (Horizontal, Vertical and Radial)\n    and to layout types (TidyTree or Dendrogram)", "Each format has it's uses, TidyTree forms are generally more compact and\n  will often be preferred.", "In addition to the six options shown here (which have fundamentally the\n  same structure in the DOM) there are radically different representations such\n  as Sunflowers and TreeMaps which can be used to show the same hierarchical\n  data in ways that serve different purposes or make different aspects of the\n  data salient.", "The code shown in this example makes use of higher order functions to\n  parameterize the drawing, thus enabling one function to encode all six\n  forms."]);
  var _snippets5 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "snippets";
      }
    })()()($$Proxy.value);
  })();
  var _panels6 = /* @__PURE__ */ (function() {
    return prop3({
      reflectSymbol: function() {
        return "panels";
      }
    })()()($$Proxy.value);
  })();
  var _drawCode5 = function(dictStrong) {
    var $97 = _snippets5(dictStrong);
    var $98 = prop27($$Proxy.value)(dictStrong);
    return function($99) {
      return $97($98($99));
    };
  };
  var _drawCode14 = /* @__PURE__ */ _drawCode5(strongFn);
  var _drawCode24 = /* @__PURE__ */ _drawCode5(strongForget);
  var handleAction8 = function(dictBind) {
    var bind18 = bind(dictBind);
    var discard111 = discard28(dictBind);
    return function(dictMonadAff) {
      var MonadEffect0 = dictMonadAff.MonadEffect0();
      var liftEffect11 = liftEffect(MonadEffect0);
      var liftAff2 = liftAff(dictMonadAff);
      var pure21 = pure(MonadEffect0.Monad0().Applicative0());
      return function(dictMonadState) {
        var modifying3 = modifying(dictMonadState);
        var assign4 = assign2(dictMonadState);
        var modify_6 = modify_(dictMonadState);
        var get8 = get(dictMonadState);
        return function(v) {
          if (v instanceof ToggleCard7) {
            return modifying3(v.value0(strongFn))(not11);
          }
          ;
          if (v instanceof Initialize9) {
            return bind18(liftEffect11(eval_D3M(removeExistingSVG5("div.svg-container"))))(function(detached) {
              return bind18(liftAff2(readSnippetFiles("TreeDraw")))(function(text1) {
                return discard111(assign4(_drawCode14)(text1))(function() {
                  return bind18(liftAff2(getTreeViaAJAX("./data/flare-2.json")))(function(treeJSON) {
                    return discard111((function() {
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
                    })())(function() {
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
              return bind18(get8)(function(v1) {
                if (v1.tree instanceof Nothing) {
                  return pure21(unit);
                }
                ;
                if (v1.tree instanceof Just) {
                  var updated = {
                    json: v1.tree.value0.json,
                    svgConfig: v1.tree.value0.svgConfig,
                    treeLayoutFn: v1.tree.value0.treeLayoutFn,
                    treeType: v1.tree.value0.treeType,
                    treeLayout: v.value0
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
              return bind18(get8)(function(v1) {
                if (v1.tree instanceof Nothing) {
                  return pure21(unit);
                }
                ;
                if (v1.tree instanceof Just) {
                  var updated = {
                    json: v1.tree.value0.json,
                    svgConfig: v1.tree.value0.svgConfig,
                    treeLayout: v1.tree.value0.treeLayout,
                    treeLayoutFn: v1.tree.value0.treeLayoutFn,
                    treeType: v.value0
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
  var handleAction17 = /* @__PURE__ */ handleAction8(bindHalogenM);
  var _code7 = function(dictStrong) {
    var $100 = _panels6(dictStrong);
    var $101 = prop113($$Proxy.value)(dictStrong);
    return function($102) {
      return $100($101($102));
    };
  };
  var _code16 = /* @__PURE__ */ _code7(strongForget);
  var _blurb4 = function(dictStrong) {
    var $103 = _panels6(dictStrong);
    var $104 = prop28($$Proxy.value)(dictStrong);
    return function($105) {
      return $103($104($105));
    };
  };
  var _blurb14 = /* @__PURE__ */ _blurb4(strongForget);
  var component8 = function(dictMonadAff) {
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
      label: text("Tree orientation"),
      inputId: "radio-vertical",
      helpText: [],
      error: []
    })([div2([css("flex-1")])([radio([css("pr-6")])([name2("tree-layout"), checked(true), onClick($$const(new SetLayout(Vertical.value)))])([text("Vertical")]), radio([css("pr-6")])([name2("tree-layout"), onClick($$const(new SetLayout(Horizontal.value)))])([text("Horizontal")]), radio([css("pr-6")])([name2("tree-layout"), onClick($$const(new SetLayout(Radial.value)))])([text("Radial")])])]), fieldset_2({
      label: text("Tree topology"),
      inputId: "radio-vertical",
      helpText: [],
      error: []
    })([div2([css("flex-1")])([radio([css("pr-6")])([name2("tree-type"), checked(true), onClick($$const(new SetType(TidyTree.value)))])([text("TidyTree")]), radio([css("pr-6")])([name2("tree-type"), onClick($$const(new SetType(Dendrogram.value)))])([text("Dendrogram")])])])]);
    var render3 = function(state3) {
      return div2([tailwindClass("story-container")])([div2([tailwindClass("story-panel-controls")])([controlsRadio]), div2([tailwindClass("story-panel-about")])([field_2({
        label: text("About"),
        helpText: [],
        error: [],
        inputId: "show-blurb"
      })([toggle([id2("show-blurb"), checked(toBoolean(view(_blurb14)(state3))), onChange(function(v) {
        return new ToggleCard7(function(dictStrong) {
          return _blurb4(dictStrong);
        });
      })])]), content_(view(_blurb14)(state3))(blurbtext6)]), div2([tailwindClass("story-panel-code")])([field_2({
        label: text("(hide this panel if screen too small)"),
        helpText: [],
        error: [],
        inputId: "show-code"
      })([toggle([id2("show-code"), checked(toBoolean(view(_code16)(state3))), onChange(function(v) {
        return new ToggleCard7(function(dictStrong) {
          return _code7(dictStrong);
        });
      })])]), content_(view(_code16)(state3))(syntaxHighlightedCode(view(_drawCode24)(state3)))]), div2([tailwindClass("svg-container")])([])]);
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render3,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction17(monadAffHalogenM(dictMonadAff))(monadStateHalogenM),
        initialize: new Just(Initialize9.value)
      })
    });
  };

  // output/Main/index.js
  var append22 = /* @__PURE__ */ append(semigroupArray);
  var map53 = /* @__PURE__ */ map(functorArray);
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
      return "sankey";
    }
  })(ordUnit);
  var slot_9 = /* @__PURE__ */ slot_2({
    reflectSymbol: function() {
      return "spago";
    }
  })(ordUnit);
  var modify_5 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var None3 = /* @__PURE__ */ (function() {
    function None4() {
    }
    ;
    None4.value = new None4();
    return None4;
  })();
  var ExampleCircles = /* @__PURE__ */ (function() {
    function ExampleCircles2() {
    }
    ;
    ExampleCircles2.value = new ExampleCircles2();
    return ExampleCircles2;
  })();
  var ExampleGUP = /* @__PURE__ */ (function() {
    function ExampleGUP2() {
    }
    ;
    ExampleGUP2.value = new ExampleGUP2();
    return ExampleGUP2;
  })();
  var ExampleTrees = /* @__PURE__ */ (function() {
    function ExampleTrees2() {
    }
    ;
    ExampleTrees2.value = new ExampleTrees2();
    return ExampleTrees2;
  })();
  var ExampleLesMis = /* @__PURE__ */ (function() {
    function ExampleLesMis2() {
    }
    ;
    ExampleLesMis2.value = new ExampleLesMis2();
    return ExampleLesMis2;
  })();
  var ExampleMetaTree = /* @__PURE__ */ (function() {
    function ExampleMetaTree2() {
    }
    ;
    ExampleMetaTree2.value = new ExampleMetaTree2();
    return ExampleMetaTree2;
  })();
  var ExamplePrinter = /* @__PURE__ */ (function() {
    function ExamplePrinter2() {
    }
    ;
    ExamplePrinter2.value = new ExamplePrinter2();
    return ExamplePrinter2;
  })();
  var ExampleSankey = /* @__PURE__ */ (function() {
    function ExampleSankey2() {
    }
    ;
    ExampleSankey2.value = new ExampleSankey2();
    return ExampleSankey2;
  })();
  var ExampleSpago = /* @__PURE__ */ (function() {
    function ExampleSpago2() {
    }
    ;
    ExampleSpago2.value = new ExampleSpago2();
    return ExampleSpago2;
  })();
  var Initialize10 = /* @__PURE__ */ (function() {
    function Initialize11() {
    }
    ;
    Initialize11.value = new Initialize11();
    return Initialize11;
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
  var eqExampleType = {
    eq: function(x15) {
      return function(y10) {
        if (x15 instanceof None3 && y10 instanceof None3) {
          return true;
        }
        ;
        if (x15 instanceof ExampleCircles && y10 instanceof ExampleCircles) {
          return true;
        }
        ;
        if (x15 instanceof ExampleGUP && y10 instanceof ExampleGUP) {
          return true;
        }
        ;
        if (x15 instanceof ExampleTrees && y10 instanceof ExampleTrees) {
          return true;
        }
        ;
        if (x15 instanceof ExampleLesMis && y10 instanceof ExampleLesMis) {
          return true;
        }
        ;
        if (x15 instanceof ExampleMetaTree && y10 instanceof ExampleMetaTree) {
          return true;
        }
        ;
        if (x15 instanceof ExamplePrinter && y10 instanceof ExamplePrinter) {
          return true;
        }
        ;
        if (x15 instanceof ExampleSankey && y10 instanceof ExampleSankey) {
          return true;
        }
        ;
        if (x15 instanceof ExampleSpago && y10 instanceof ExampleSpago) {
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
      if (v instanceof ExampleSankey) {
        return "Sankey";
      }
      ;
      throw new Error("Failed pattern match at Main (line 68, column 10 - line 77, column 32): " + [v.constructor.name]);
    }
  };
  var show21 = /* @__PURE__ */ show(showExampleType);
  var _trees = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _spago = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _sankey = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _printtree = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _metatree = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _lesmis = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _index = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _gup = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var _circles = /* @__PURE__ */ (function() {
    return $$Proxy.value;
  })();
  var parent2 = function(dictMonadAff) {
    var component9 = component6(dictMonadAff);
    var component1 = component7(dictMonadAff);
    var component22 = component(dictMonadAff);
    var component32 = component8(dictMonadAff);
    var component42 = component3(dictMonadAff);
    var component52 = component4(dictMonadAff);
    var component62 = component2(dictMonadAff);
    var component72 = component5(dictMonadAff);
    var renderExampleNav = function(current) {
      return function(example) {
        return li([class_("mb-3")])([a([classes(append22(linkClasses)((function() {
          var $57 = eq7(current)(example);
          if ($57) {
            return ["font-medium"];
          }
          ;
          return [];
        })())), onClick($$const(new Example(example)))])([text(show21(example))])]);
      };
    };
    var renderNavGroup = function(currentExample) {
      return div2([class_("text-base overflow-y-auto")])([caption_([text("Simple examples")]), ul([class_("list-reset")])(map53(renderExampleNav(currentExample))([ExampleCircles.value, ExampleGUP.value, ExampleTrees.value, ExampleLesMis.value, ExampleSankey.value])), caption_([text("Alternate interpreters")]), ul([class_("list-reset")])(map53(renderExampleNav(currentExample))([ExampleMetaTree.value, ExamplePrinter.value])), caption_([text("Halogen Application")]), ul([class_("list-reset")])(map53(renderExampleNav(currentExample))([ExampleSpago.value]))]);
    };
    var renderSidebar = function(currentExample) {
      return backdrop([tailwindClass("story-sidebar")])([div2([class_("flex-1 p-2 overflow-y-auto")])([img([class_("w-24 mb-8 p-2 bg-white"), src("PSD3-logo.png")]), nav([class_("text-base overflow-y-auto")])([renderNavGroup(currentExample)])])]);
    };
    var renderExample = function(v) {
      if (v instanceof None3) {
        return slot_1(_index)(unit)(component9)(unit);
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
      if (v instanceof ExampleSankey) {
        return slot_8(_sankey)(unit)(component72)(unit);
      }
      ;
      if (v instanceof ExampleSpago) {
        return slot_9(_spago)(unit)(component9)(unit);
      }
      ;
      throw new Error("Failed pattern match at Main (line 153, column 5 - line 164, column 71): " + [v.constructor.name]);
    };
    var render3 = function(currentExample) {
      return body_([div2([tailwindClass("app-container")])([renderSidebar(currentExample), renderExample(currentExample)])]);
    };
    var initialState = function(v) {
      return ExampleSpago.value;
    };
    var handleAction9 = function(v) {
      if (v instanceof Initialize10) {
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
      throw new Error("Failed pattern match at Main (line 169, column 18 - line 171, column 39): " + [v.constructor.name]);
    };
    return mkComponent({
      initialState,
      render: render3,
      "eval": mkEval({
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        finalize: defaultEval.finalize,
        handleAction: handleAction9,
        initialize: new Just(Initialize10.value)
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

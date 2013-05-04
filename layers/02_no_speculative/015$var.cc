//// implicit gensyms: read $vars as unique syms

// Variables defined in macros can cause subtle bugs:
//   mac bad-swap(x y)
//     `(let tmp ,x             # tmp can be captured
//        (,x = ,y)
//        (,y = tmp))
//
//   (withs (a 3 b 4)    (bad-swap a b)    (list a b))
//   => (4 3)   # seems ok
//
//   (withs (a 3 tmp 4)  (bad-swap a tmp)  (list a tmp))
//   => (3 4)   # oops
//
// To avoid such bugs, use an implicit gensym:
//   mac good-swap(x y)
//     `(let $tmp ,x
//        (,x = ,y)
//        (,y = $tmp))
//
// When reading this definition, occurrences of $tmp are replaced with a
// unique symbol like tmp2417. Occurrences of $tmp elsewhere will get a
// *different* unique symbol.

// Design considered the following:
//  simple implementation
//    so uniqueness isn't perfectly guaranteed, just overwhelmingly likely
//  $var expansion is separate from tokenization
//  runs after infix transformation, so $+ => +147 isn't turned into '(+ 147)
//  don't expand $vars in compiled primitives

// Design alternative: older lisps use explicit gensyms, which make for less
// concise macros:
//   mac old-swap(x y)
//     let tmp (uniq 'tmp)      # uniq was called gensym in older lisps
//       `(let ,tmp ,x
//          (,x = ,y)
//          (,y = ,tmp))

Cell* transformDollarVars(Cell* input) {
  Table map;  // transform $vars identically within each top-level expression
  return transformDollarVars(input, map);
}

Cell* transformDollarVars(Cell* input, Table& map) {
  if (isSym(input) && toString(input)[0] == '$') {
    if (!map[input])
      map[mkref(input)] = mkref(genSym(newSym(toString(input).substr(1))));
    return map[input];
  }

  if (!isCons(input)) return input;   // no tables or compiledFns in static code
  setCar(input, transformDollarVars(car(input), map));
  setCdr(input, transformDollarVars(cdr(input), map));
  return input;
}

Cell* genSym(Cell* x) {
  static long counter = 0;
  ostringstream os;
  os << (x == nil ? "sym" : toString(x)) << ++counter;
  return newSym(os.str());
}

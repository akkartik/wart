//// implicit gensyms: $vars turn into a unique sym in every top-level form

// Variables defined in macros can cause subtle bugs:
//   mac bad-swap(x y)
//     `(let tmp ,x         # tmp can be captured
//        (,x = ,y)
//        (,y = tmp))
//
//   (withs (a 3 b 4)
//     (bad-swap a b)
//     (list a b))
//   => (4 3)               # seems ok
//
//   (withs (a 3 tmp 4)
//     (bad-swap a tmp)
//     (list a tmp))
//   => (3 4)               # oops
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
//  runs after infix transformation, so $+ => +147 isn't turned into '(+ 147)
//  don't expand $vars in compiled primitives; we won't know what to lookup()

// Alternative: explicit gensyms make for less concise macros:
//   mac old-swap(x y)
//     let tmp (uniq)
//       `(let ,tmp ,x
//          (,x = ,y)
//          (,y = ,tmp))

cell* transform_dollar_vars(cell* input) {
  table map;  // transform $vars identically within each top-level expression
  return transform_dollar_vars(input, map);
}

cell* transform_dollar_vars(cell* input, table& map) {
  if (is_sym(input) && to_string(input)[0] == '$') {
    if (!map[input])
      map[mkref(input)] = mkref(gensym(new_sym(to_string(input).substr(1))));
    return map[input];
  }

  if (!is_cons(input)) return input;  // no tables or compiledfns in static code
  set_car(input, transform_dollar_vars(car(input), map));
  set_cdr(input, transform_dollar_vars(cdr(input), map));
  return input;
}

cell* gensym(cell* x) {
  static long counter = 0;
  ostringstream os;
  os << (x == nil ? "sym" : to_string(x)) << ++counter;
  return new_sym(os.str());
}

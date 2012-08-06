//// implicit gensyms: $vars turn into unique syms before they're eval'd

Cell* transformDollarVars(Cell* input, Table& map) {
  if (isSym(input) && toString(input)[0] == '$') {
    if (!map[input])
      map[mkref(input)] = mkref(genSym(newSym(toString(input).substr(1)))); // against destruction of map
    return map[input];
  }

  if (!isCons(input)) return input; // no tables or compiledFns in static code
  setCar(input, transformDollarVars(car(input), map));
  setCdr(input, transformDollarVars(cdr(input), map));
  return input;
}

Cell* transform_dollarVars(Cell* input) {
  Table map; // expand $vars identically within each top-level expression
  return transformDollarVars(input, map);
}

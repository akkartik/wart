//// implicit gensyms: $vars turn into unique syms before they're eval'd

Cell* transform_dollarVars(Cell* input) {
  Table map; // expand $vars identically within each top-level expression
  return expandDollarVars(input, map);
}

Cell* expandDollarVars(Cell* input, Table& map) {
  if (isSym(input) && toString(input)[0] == '$') {
    if (!map[input])
      map[mkref(input)] = mkref(genSym(newSym(toString(input).substr(1)))); // against destruction of map
    return map[input];
  }

  if (!isCons(input)) return input; // no tables or compiledFns in static code
  setCar(input, expandDollarVars(car(input), map));
  setCdr(input, expandDollarVars(cdr(input), map));
  return input;
}

Cell* genSym(Cell* x) {
  static long counter = 0;
  ostringstream os;
  os << (x == nil ? "sym" : toString(x)) << ++counter;
  return newSym(os.str());
}

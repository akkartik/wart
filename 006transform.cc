//// manage phases that transform cell trees

                                  Cell* genSym(Cell* x) {
                                    static long counter = 0;
                                    ostringstream os;
                                    os << (x == nil ? L"sym" : toString(x)) << ++counter;
                                    return newSym(os.str());
                                  }

Cell* transformDollarVars(Cell* input, Table& map) {
  if (isSym(input) && toString(input)[0] == L'$') {
    if (!map[input])
      map[input] = mkref(genSym(nil)); // map is temporary
    return map[input];
  }

  if (!isCons(input)) return input; // no tables or primFuncs in static code
  setCar(input, transformDollarVars(car(input), map));
  setCdr(input, transformDollarVars(cdr(input), map));
  return input;
}

Cell* transform_dollarVars(Cell* input) {
  Table map; // expand $vars identically within each top-level expression
  return transformDollarVars(input, map);
}



typedef Cell* (*transformer)(Cell*);
const transformer transforms[] = {
  #include "transform_list"
};

Cell* transform(Cell* cell) {
  for (unsigned int i=0; i < sizeof(transforms)/sizeof(transforms[0]); ++i)
    cell = (*transforms[i])(cell);
  return cell;
}

list<Cell*> transform(list<Cell*> input) {
  list<Cell*> result;
  for (list<Cell*>::iterator p = input.begin(); p != input.end(); ++p)
    result.push_back(transform(*p));
  return result;
}

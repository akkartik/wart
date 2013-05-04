//// user-defined types

// Design considered the following:
//  functions matching for specific types
//  extensible coercion between arbitrary types
//  arbitrary types in function position

Cell* newObject(string type, Cell* rep) {
  return newCons(sym_object, newCons(newSym(type), newCons(rep)));
}

bool isObject(Cell* x) {
  return car(x) == sym_object;
}

Cell* rep(Cell* x) {
  if (!isObject(x)) return x;
  return car(cdr(cdr(x)));
}

Cell* type(Cell* x) {
  if (x == nil) return nil;
  switch(x->type) {
  case INTEGER:
  case FLOAT:
    return sym_number;
  case SYMBOL:
    return sym_symbol;
  case STRING:
    return sym_string;
  case TABLE:
    return sym_table;
  case COMPILED_FN:
    return sym_function;
  case CONS:
    if (isObject(x))
      return car(cdr(x));
    return sym_list;
  default:
    RAISE << "Undefined type: " << x->type << endl << DIE;
    return nil;   // never reached
  }
}

//// user-defined types and coercion

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

// extensible coerce based on Coercions table
// always mkrefs its result
Cell* coerceQuoted(Cell* x, Cell* destType, Cell* coercions) {
  Cell* typ = type(x);
  if (typ == destType)
    return mkref(x);

  if (coercions == nil) RAISE << "Coercions not initialized yet\n";
  if (!isTable(coercions)) RAISE << "Coercions not a table\n";
  Cell* tmp = get(coercions, destType);
  if (tmp == nil) RAISE << "Coercions for " << destType << " not initialized\n";
  if (!isTable(coercions)) RAISE << "Coercions for " << destType << " not a table\n";
  Cell* coercer = get(tmp, typ);
  if (coercer == nil) {
    RAISE << "can't coerce " << typ << " " << x << " to " << destType << endl;
    return nil;
  }
  Cell* expr = newCons(coercer, newCons(newCons(sym_quote, x)));
  Cell* result = eval(expr);
  rmref(expr);
  return result;  // already mkref'd
}

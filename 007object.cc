//// user-defined types and coercion

Cell* newObject(string type, Cell* rep) {
  return newCons(newSym("object"), newCons(newSym(type), newCons(rep)));
}

bool isObject(Cell* x) {
  return car(x) == newSym("object");
}

Cell* rep(Cell* x) {
  return car(cdr(cdr(x)));
}

Cell* type(Cell* x) {
  if (x == nil) return nil;
  switch(x->type) {
  case INTEGER:
  case FLOAT:
    return newSym("number");
  case SYMBOL:
    return newSym("symbol");
  case STRING:
    return newSym("string");
  case TABLE:
    return newSym("table");
  case COMPILED_FN:
    return newSym("function");
  case CONS:
    if (isObject(x))
      return car(cdr(x));
    return newSym("list");
  default:
    RAISE << "Undefined type: " << x->type << endl << DIE;
    return nil; // never reached
  }
}

// extensible coerce based on coercions* table
// always mkrefs its result
Cell* coerceQuoted(Cell* x, Cell* destType, Cell* coercions) {
  Cell* typ = type(x);
  if (typ == destType)
    return mkref(x);

  if (coercions == nil) RAISE << "coercions* not initialized yet\n";
  if (!isTable(coercions)) RAISE << "coercions* not a table\n";
  Cell* tmp = get(coercions, destType);
  if (tmp == nil) RAISE << "coercions* for " << destType << " not initialized\n";
  if (!isTable(coercions)) RAISE << "coercions* for " << destType << " not a table\n";
  Cell* coercer = get(tmp, typ);
  if (coercer == nil) {
    RAISE << "can't coerce " << typ << " " << x << " to " << destType << endl;
    return nil;
  }
  Cell* expr = newCons(coercer, newCons(newCons(newSym("'"), x)));
  Cell* result = eval(expr);
  rmref(expr);
  return result; // already mkref'd
}

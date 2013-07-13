//// user-defined types and coercion

// Design considered the following:
//  functions matching for specific types
//  extensible coercion between arbitrary types
//  arbitrary types in function position

cell* new_object(string type, cell* rep) {
  return new_cons(sym_object, new_cons(new_sym(type), new_cons(rep)));
}

bool is_object(cell* x) {
  return car(x) == sym_object;
}

cell* rep(cell* x) {
  if (!is_object(x)) return x;
  return car(cdr(cdr(x)));
}

cell* type(cell* x) {
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
    if (is_object(x))
      return car(cdr(x));
    return sym_list;
  default:
    RAISE << "Undefined type: " << x->type << '\n' << die();
    return nil;
  }
}

// extensible coerce based on Coercions table
// always mkrefs its result
cell* coerce_quoted(cell* x, cell* dest_type, cell* coercions) {
  cell* typ = type(x);
  if (typ == dest_type)
    return mkref(x);

  if (coercions == nil) RAISE << "Coercions not initialized yet\n";
  if (!is_table(coercions)) RAISE << "Coercions not a table\n";
  cell* tmp = get(coercions, dest_type);
  if (tmp == nil) RAISE << "Coercions for " << dest_type << " not initialized when eval'ing " << x << '\n';
  if (!is_table(coercions)) RAISE << "Coercions for " << dest_type << " not a table\n";
  cell* coercer = get(tmp, typ);
  if (coercer == nil) {
    RAISE << "can't coerce " << typ << " " << x << " to " << dest_type << '\n';
    return nil;
  }
  cell* expr = new_cons(coercer, new_cons(new_cons(sym_quote, x)));
  cell* result = eval(expr);
  rmref(expr);
  return result;  // already mkref'd
}

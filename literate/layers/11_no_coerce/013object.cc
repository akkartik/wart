//// user-defined types

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
    return nil;   // never reached
  }
}

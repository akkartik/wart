//// creating functions; calling them with args

// Design considered the following:
//  implicit eval: by default (f arg1 arg2 arg3) evals all the elems, then passes the args to f
//  functions are just data: car => (object function {params: l, body: #compiled})
//  user-defined functions are easy to reflect on: (fn (x) 34) => (object function {params: (x), body: (34)})
//  quote to suppress eval in expressions: 'abc => abc
//  quoted params suppress arg eval: ((fn '(x) x) abc) => abc
//  varargs functions: ((fn params params) 1 2 3) => (1 2 3)
//  varargs functions with some named params: ((fn (fmt ... rest) (printf fmt rest)) "%d%d" 34 35) => "3435"
//  passing in lists to functions: ((fn ((x y)) (+ x y)) '(3 4)) => 7

cell* eval(cell* expr) {
  new_trace_frame("eval");
  if (!expr)
    RAISE << "eval: cell should never be NULL\n" << die();

  trace("eval") << expr;
  if (expr == nil) {
    trace("eval") << "nil branch";
    trace("eval") << "=> nil";
    return nil;
  }

  if (is_keyword_sym(expr)) {
    trace("eval") << "keyword sym";
    trace("eval") << "=> " << expr;
    return mkref(expr);
  }

  if (is_sym(expr)) {
    trace("eval") << "sym";
    cell* result = lookup(expr);
    trace("eval") << "=> " << result;
    return mkref(result);
  }

  if (is_atom(expr)) {
    trace("eval") << "literal";
    trace("eval") << "=> " << expr;
    return mkref(expr);
  }

  if (is_quoted(expr)) {
    trace("eval") << "quote";
    trace("eval") << "=> " << cdr(expr);
    return mkref(cdr(expr));
  }

  cell* result = eval_primitive(car(expr), cdr(expr));
  if (result) {
    trace("eval") << "compiled fn";
    trace("eval") << "=> " << result;
    return mkref(result);
  }

  // expr is a call
  TEMP(fn, eval(car(expr)));
  if (!is_fn(fn))
    RAISE << "Not a call: " << expr << '\n'
        << "Perhaps you need to split the line in two.\n";

  // eval its args, create new bindings
  eval_bind_all(sig(fn), cdr(expr));

  result = nil;
  // eval all forms in body, save result of final form
  for (cell* form = body(fn); form != nil; form=cdr(form))
    update(result, eval(car(form)));
  trace("eval") << "=> " << result;
  return result;
}

cell* eval_primitive(cell* f, cell* args) {
  if (f == new_sym("fn")) {
    cell* f = new_table();
    set(f, sym_sig, car(args));
    set(f, sym_body, cdr(args));
    return mkref(f);
  }

  if (f == new_sym("eval")) {
    TEMP(arg, eval(car(args)));
    return eval(arg);
  }

  if (f == new_sym("if")) {
    TEMP(check, eval(car(args)));
    return (check != nil) ? eval(car(cdr(args))) : eval(car(cdr(cdr(args))));
  }

  if (f == new_sym("not")) {
    TEMP(arg, eval(car(args)));
    return (arg == nil) ? mkref(new_num(1)) : nil;
  }

  if (f == new_sym("=")) {
    TEMP(x, eval(car(args)));
    TEMP(y, eval(car(cdr(args))));
    if (x == nil && y == nil)
      return mkref(new_num(1));
    else if (x == nil || y == nil)
      return nil;
    else if (x == y)
      return mkref(x);
    else if (x->type == FLOAT || y->type == FLOAT)
      return equal_floats(to_float(x), to_float(y)) ? mkref(x) : nil;
    else if (is_string(x) && is_string(y) && to_string(x) == to_string(y))
      return mkref(x);
    else
      return nil;
  }

  if (f == new_sym("<-")) {
    cell* var = car(args);
    if (!is_sym(var)) {
      RAISE << "can't assign to non-sym " << var << '\n';
      return nil;
    }
    cell* val = eval(car(cdr(args)));
    new_binding(var, val);
    return val;   // already mkref'd
  }

  // lists
  if (f == new_sym("cons")) {
    TEMP(a, eval(car(args)));
    TEMP(b, eval(car(cdr(args))));
    return mkref(new_cons(a, b));
  }
  if (f == new_sym("car")) {
    TEMP(arg, eval(car(args)));
    return mkref(car(arg));
  }
  if (f == new_sym("cdr")) {
    TEMP(arg, eval(car(args)));
    return mkref(cdr(arg));
  }

  // numbers
  if (f == new_sym("+")) {
    TEMP(x, eval(car(args)));
    TEMP(y, eval(car(cdr(args))));
    if (x->type == FLOAT || y->type == FLOAT)
      return mkref(new_num(to_float(x) + to_float(y)));
    else
      return mkref(new_num(to_int(x) + to_int(y)));
  }
  if (f == new_sym("-")) {
    TEMP(x, eval(car(args)));
    TEMP(y, eval(car(cdr(args))));
    if (x->type == FLOAT || y->type == FLOAT)
      return new_num(to_float(x) - to_float(y));
    else
      return new_num(to_int(x) - to_int(y));
  }
  if (f == new_sym("*")) {
    TEMP(x, eval(car(args)));
    TEMP(y, eval(car(cdr(args))));
    return nil;
    if (x->type == FLOAT || y->type == FLOAT)
      return new_num(to_float(x) * to_float(y));
    else
      return new_num(to_int(x) * to_int(y));
  }
  if (f == new_sym("/")) {
    TEMP(x, eval(car(args)));
    TEMP(y, eval(car(cdr(args))));
    return new_num(to_float(x) / to_float(y));
  }
  if (f == new_sym("%")) {
    TEMP(x, eval(car(args)));
    TEMP(y, eval(car(cdr(args))));
    return new_num(to_int(x) % to_int(y));
  }
  if (f == new_sym("<")) {
    TEMP(x, eval(car(args)));
    TEMP(y, eval(car(cdr(args))));
    if (x == nil || y == nil)
      return nil;
    else if (to_float(x) < to_float(y))
      return mkref(y);
    else
      return nil;
  }
  if (f == new_sym("int")) {
    TEMP(arg, eval(car(args)));
    return mkref(new_num(to_int(arg)));
  }
  return NULL;
}

// bind params to args in new_scope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
void eval_bind_all(cell* params, cell* args) {
  if (params == nil)
    ;

  else if (is_sym(params)) {
    TEMP(val, eval_all(args));
    bind_params(params, val);
  }

  else if (!is_cons(params))
    ;

  else {
    TEMP(val, eval(car(args)));
    bind_params(car(params), val);
    eval_bind_all(cdr(params), cdr(args));
  }
}

void bind_params(cell* params, cell* args) {
  if (params == nil)
    ;

  else if (is_sym(params))
    new_binding(params, args);

  else if (!is_cons(params))
    ;

  else if (args != nil && !is_cons(args))
    bind_params(params, nil);

  else {
    bind_params(car(params), car(args));
    bind_params(cdr(params), cdr(args));
  }
}

cell* eval_all(cell* args) {
  if (!is_cons(args))
    return eval(args);
  cell* p_result = new_cell(), *curr = p_result;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    TEMP(val, eval(car(args)));
    add_cons(curr, val);
  }
  return drop_ptr(p_result);
}



//// helpers

bool is_quoted(cell* cell) {
  return is_cons(cell) && car(cell) == sym_quote;
}

cell* strip_quote(cell* cell) {
  return is_quoted(cell) ? cdr(cell) : cell;
}

bool is_keyword_sym(cell* x) {
  if (!is_sym(x)) return false;
  string name = to_string(x);
  if (name == ":") return false;
  return name[0] == ':';
}

// fn = {sig => .., body => ..}
//  both are optional
bool is_fn(cell* x) {
  return is_table(x);
}

cell* sig(cell* fn) {
  return get(fn, sym_sig);
}

cell* body(cell* fn) {
  return get(fn, sym_body);
}

cell* quote(cell* x) {
  return new_cons(sym_quote, x);
}

cell* quote_all(cell* x) {
  cell* result = new_cell(), *curr = result;
  for (cell* iter = x; iter != nil; iter=cdr(iter), curr=cdr(curr))
    add_cons(curr, quote(car(iter)));
  return drop_ptr(result);
}

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
  if (!expr)
    RAISE << "eval: cell should never be NULL" << endl << die();

  if (expr == nil)
    return nil;

  if (is_keyword_sym(expr))
    return mkref(expr);

  if (is_sym(expr))
    return mkref(lookup(expr));

  if (is_atom(expr))
    return mkref(expr);

  if (is_object(expr))
    return mkref(expr);

  if (is_quoted(expr))
    return mkref(cdr(expr));

  // expr is a call
  cell* fn = eval(car(expr));
  if (!is_fn(fn))
    RAISE << "Not a call: " << expr << endl
        << "Perhaps you need to split the line in two." << endl;

  // eval its args, create new bindings
  list<cell*> vars_bound;
  eval_bind_all(sig(fn), cdr(expr), vars_bound);

  cell* result = nil;
  if (is_compiledfn(body(fn))) {
    result = to_compiledfn(body(fn))();  // all Compiledfns must mkref result
  }
  else {
    // eval all forms in body, save result of final form
    for (cell* form = impl(fn); form != nil; form=cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }
  }

  for (list<cell*>::iterator p = vars_bound.begin(); p != vars_bound.end(); ++p)
    end_dynamic_scope(*p);
  rmref(fn);
  return result;  // already mkref'd
}

// bind params to args in new_scope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
void eval_bind_all(cell* params, cell* args, list<cell*>& vars_bound) {
  if (params == nil)
    return;

  cell* args2 = NULL;
  if (is_quoted(params)) {
    params = strip_quote(params);
    args2 = quote_all(args);
  }
  else {
    args2 = mkref(args);
  }

  if (is_sym(params)) {
    cell* val = eval_all(args2);
    bind_params(params, val, vars_bound);
    rmref(val);
  }

  else if (!is_cons(params))
    ;

  else {
    eval_bind_param(car(params), car(args2), vars_bound);
    eval_bind_all(cdr(params), cdr(args2), vars_bound);
  }
  rmref(args2);
}

void eval_bind_param(cell* param, cell* arg, list<cell*>& vars_bound) {
  cell* arg2 = NULL;
  if (is_quoted(param)) {
    param = strip_quote(param);
    arg2 = mkref(new_cons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  cell* val = eval(arg2);
  bind_params(param, val, vars_bound);
  rmref(val);
  rmref(arg2);
}

void bind_params(cell* params, cell* args, list<cell*>& vars_bound) {
  if (is_quoted(params))
    bind_params(strip_quote(params), args, vars_bound);

  else if (params == nil)
    ;

  else if (is_sym(params)) {
    new_dynamic_scope(params, args);
    vars_bound.push_back(params);
  }

  else if (!is_cons(params))
    ;

  else if (args != nil && !is_cons(args))
    bind_params(params, nil, vars_bound);

  else {
    bind_params(car(params), car(args), vars_bound);
    bind_params(cdr(params), cdr(args), vars_bound);
  }
}

cell* eval_all(cell* args) {
  if (!is_cons(args))
    return eval(args);
  cell* p_result = new_cell(), *curr = p_result;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    cell* val = eval(car(args));
    add_cons(curr, val);
    rmref(val);
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

// fn = (object function {sig => .., body => ..})
bool is_fn(cell* x) {
  return is_cons(x) && type(x) == sym_function;
}

cell* sig(cell* fn) {
  return get(rep(fn), sym_sig);
}

cell* body(cell* fn) {
  return get(rep(fn), sym_body);
}

cell* impl(cell* fn) {
  cell* impl = get(rep(fn), sym_optimized_body);
  return (impl != nil) ? impl : body(fn);
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

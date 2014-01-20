//// creating functions; calling them with args

// Design considered the following:
//  implicit eval: by default (f arg1 arg2 arg3) evals all the elems, then passes the args to f
//  functions are just data: car => (object function {params: l, body: #compiled})
//  user-defined functions are easy to reflect on: (fn (x) 34) => (object function {params: (x), body: (34)})
//  functions create a private scope, can continue to access outer bindings (lexical scope)
//  user-defined functions can't see caller environment
//  quote to suppress eval in expressions: 'abc => abc
//  quoted params suppress arg eval: ((fn '(x) x) abc) => abc
//  varargs functions: ((fn params params) 1 2 3) => (1 2 3)
//  varargs functions with some named params: ((fn (fmt ... rest) (printf fmt rest)) "%d%d" 34 35) => "3435"
//  passing in lists to functions: ((fn ((x y)) (+ x y)) '(3 4)) => 7

cell* eval(cell* expr) {
  return eval(expr, Curr_lexical_scope);
}

cell* eval(cell* expr, cell* scope) {
  new_trace_frame("eval");
  if (!expr) {
    RAISE << "eval: cell should never be NULL\n" << die();
    return nil;
  }

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

  if (is_object(expr)) {
    trace("eval") << "object";
    trace("eval") << "=> " << expr;
    return mkref(expr);
  }

  if (is_quoted(expr)) {
    trace("eval") << "quote";
    trace("eval") << "=> " << cdr(expr);
    return mkref(cdr(expr));
  }

  // expr is a call
  TEMP(fn, eval(car(expr), scope));
  if (!is_fn(fn))
    RAISE << "Not a call: " << expr << '\n'
        << "Perhaps you need to split the line in two.\n";

  // eval its args in the caller's lexical environment
  cell* new_scope = new_table();
  eval_bind_all(sig(fn), cdr(expr), scope, new_scope);

  // swap in the function's lexical environment
  new_dynamic_scope(CURR_LEXICAL_SCOPE, is_compiledfn(body(fn)) ? scope : env(fn));
  add_lexical_scope(new_scope);

  cell* result = nil;
  if (is_compiledfn(body(fn))) {
    trace("eval") << "compiled fn";
    result = to_compiledfn(body(fn))();  // all compiledfns mkref their result
  }
  else {
    trace("eval") << "fn";
    // eval all forms in body, save result of final form
    for (cell* form = impl(fn); form != nil; form=cdr(form))
      update(result, eval(car(form)));
  }

  end_lexical_scope();  // implicitly rmrefs new_scope
  end_dynamic_scope(CURR_LEXICAL_SCOPE);

  trace("eval") << "=> " << result;
  return result;  // already mkref'd
}

// bind params to args in new_scope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
void eval_bind_all(cell* params, cell* args, cell* scope, cell* new_scope) {
  trace("eval/bind/all") << params << " <-> " << args;
  if (params == nil)
    return;

  TEMP(args2, nil);
  if (is_quoted(params)) {
    params = strip_quote(params);
    args2 = quote_all(args);  // already mkref'd
    trace("eval/bind/all") << "stripping quote: " << params << " <-> " << args2;
  }
  else {
    args2 = mkref(args);
    trace("eval/bind/all") << " args nrefs: " << args2->nrefs;
  }

  if (is_sym(params)) {
    TEMP(val, eval_all(args2, scope));
    bind_params(params, val, new_scope);
  }

  else if (!is_cons(params))
    ;

  else {
    eval_bind_param(car(params), car(args2), scope, new_scope);
    eval_bind_all(cdr(params), cdr(args2), scope, new_scope);
  }
}

void eval_bind_param(cell* param, cell* arg, cell* scope, cell* new_scope) {
  TEMP(arg2, nil);
  if (is_quoted(param)) {
    param = strip_quote(param);
    arg2 = mkref(new_cons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  TEMP(val, eval(arg2, scope));
  bind_params(param, val, new_scope);
}

void bind_params(cell* params, cell* args, cell* new_scope) {
  trace("eval/bind/one") << params << " <-> " << args;
  if (is_quoted(params))
    bind_params(strip_quote(params), args, new_scope);

  else if (params == nil)
    ;

  else if (is_sym(params)) {
    trace("eval/bind/one") << "binding " << params << " to " << args;
    add_lexical_binding(params, args, new_scope);
  }

  else if (!is_cons(params))
    ;

  else if (args != nil && !is_cons(args))
    bind_params(params, nil, new_scope);

  else {
    bind_params(car(params), car(args), new_scope);
    bind_params(cdr(params), cdr(args), new_scope);
  }
}

cell* eval_all(cell* args, cell* scope) {
  if (!is_cons(args))
    return eval(args, scope);
  cell* p_result = new_cell(), *curr = p_result;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    TEMP(val, eval(car(args), scope));
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

// fn = (object function {sig => .., body => .., env => ..})
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

cell* env(cell* fn) {
  return get(rep(fn), sym_env);
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

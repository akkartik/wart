//// creating functions and macros; calling them with args including @spliced and :keywords

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
//  callers can pass in just needed args: ((fn (a b c) (list a b)) 3) => (3 nil)
//
//  list templates: backquote to suppress eval, unquote to reenable eval inside backquote. `(+ ,a ,b)
//  ability to splice multiple elements into lists: ,@vars inside backquote, @vars otherwise
//  macros need to access caller environment

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

  if (expr == sym_false) {
    trace("eval") << "false";
    trace("eval") << "=> " << expr;
    return mkref(expr);
  }

  if (is_sym(expr)) {
    trace("eval") << "sym";
    cell* result = lookup(expr, scope);
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

  if (is_backquoted(expr)) {
    cell* result = process_unquotes(cdr(expr), 1, scope);
    trace("eval") << "backquote";
    trace("eval") << "=> " << result;
    return result;  // already mkref'd
  }

  if (is_already_evald(expr))
    return mkref(strip_already_evald(expr));

  // expr is a call
  TEMP(fn, to_fn(eval(car(expr), scope)));
  if (!is_fn(fn))
    RAISE << "Not a call: " << expr << '\n'
        << "Perhaps you need to split the line in two.\n";

  // eval its args in the caller's lexical environment
  TEMP(spliced_args, splice_args(cdr(expr), scope, fn));
  trace("unevald_args") << spliced_args;
  TEMP(new_scope, mkref(new_table()));
  eval_bind_all(sig(fn), spliced_args, scope, new_scope);

  // swap in the function's lexical environment
  new_dynamic_scope(CURR_LEXICAL_SCOPE, is_compiledfn(body(fn)) ? scope : env(fn));
  add_lexical_scope(new_scope);
  add_lexical_binding(sym_caller_scope, scope);

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
    ;

  else if (is_sym(strip_quote(params))) {
    cell* dummy = NULL;
    eval_bind_rest(params, args, &dummy, scope, new_scope);
  }

  else if (!is_cons(strip_quote(params)))
    ;

  else if (is_quoted(params))
    bind_params(params, args, NULL, new_scope);

  else {
    eval_bind_param(car(params), car(args), scope, new_scope);
    eval_bind_all(cdr(params), cdr(args), scope, new_scope);
  }
}

void eval_bind_rest(cell* param, cell* args, cell** cached_val, cell* scope, cell* new_scope) {
  trace("eval/bind/rest") << param << " <-> " << args;

  if (is_quoted(param))
    bind_params(param, args, NULL, new_scope);

  else if (is_cons(param))
    eval_bind_all(param, args, scope, new_scope);

  else {
    *cached_val = eval_all(args, scope);
    bind_params(param, *cached_val, args, new_scope);
    rmref(*cached_val);
  }
}

void eval_bind_param(cell* param, cell* arg, cell* scope, cell* new_scope) {
  trace("eval/bind/param") << param << " <-> " << arg;

  if (is_quoted(param))
    bind_params(param, arg, NULL, new_scope);

  else {
    TEMP(val, eval(arg, scope));
    bind_params(param, val, arg, new_scope);
  }
}

// unevald_args might be NULL if params is quoted
void bind_params(cell* params, cell* args, cell* unevald_args, cell* new_scope) {
  trace("eval/bind/one") << params << " <-> " << args;
  if (is_quoted(params)) {
    TEMP(correct_args, mkref(unevald_args ? unevald_args : args));
    bind_params(strip_quote(params), correct_args, NULL, new_scope);
  }

  else if (params == nil)
    ;

  else if (is_sym(params)) {
    trace("eval/bind/one") << "binding " << params << " to " << args;
    add_lexical_binding(params, args, new_scope);
  }

  else if (!is_cons(params))
    ;

  else {
    bind_params(car(params), car(args), unevald_args && is_cons(unevald_args) ? car(unevald_args) : unevald_args, new_scope);
    bind_params(cdr(params), cdr(args), unevald_args && is_cons(unevald_args) ? cdr(unevald_args) : unevald_args, new_scope);
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



//// eval @exprs and inline them into args

cell* splice_args(cell* args, cell* scope, cell* fn) {
  cell *p_result = new_cell(), *tip = p_result;
  for (cell* curr = args; curr != nil; curr=cdr(curr)) {
    if (!is_spliced(car(curr))) {
      add_cons(tip, car(curr));
      tip=cdr(tip);
      continue;
    }

    if (is_macro(fn))
      RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)\n";
    TEMP(x, unsplice(car(curr), scope));
    for (cell* curr2 = x; curr2 != nil; curr2=cdr(curr2), tip=cdr(tip))
      add_cons(tip, tag_already_evald(car(curr2)));
  }
  trace("splice") << cdr(p_result);
  return drop_ptr(p_result);
}

cell* unsplice(cell* arg, cell* scope) {
  return eval(cdr(arg), scope);
}

cell* tag_already_evald(cell* cell) {
  if (is_keyword_sym(cell)) return cell;
  return new_cons(sym_already_evald, cell);
}

bool is_already_evald(cell* cell) {
  return is_cons(cell) && car(cell) == sym_already_evald;
}

cell* strip_already_evald(cell* cell) {
  trace("already_evald") << "stripping from " << cell;
  while (is_already_evald(cell))
    cell = cdr(cell);
  return cell;
}



//// backquoted exprs

cell* process_unquotes(cell* x, long depth, cell* scope) {
  new_trace_frame("backquote");
  trace("backquote") << x << " " << depth;
  if (!is_cons(x)) {
    trace("backquote") << "atom: " << x;
    return mkref(x);
  }

  if (!has_unquote_splice(x)
      && unquote_depth(x) == depth) {
    cell* result = eval(strip_unquote(x), scope);
    trace("backquote") << "eval: " << result;
    return result;  // already mkref'd
  }
  else if (has_unquote_splice(car(x))
           && unquote_depth(car(x)) == depth) {
    TEMP(splice, eval(strip_unquote(car(x)), scope));
    trace("backquote") << "splice: " << splice;
    TEMP(rest, process_unquotes(cdr(x), depth, scope));
    if (splice == nil) return mkref(rest);

    // always splice in a copy
    cell* result = copy_list(splice);
    append(result, rest);
    return mkref(result);
  }
  else if (unquote_depth(x) > 0) {
    trace("backquote") << "not deep enough: " << x;
    return mkref(x);
  }

  if (is_backquoted(x)) {
    TEMP(rest, process_unquotes(cdr(x), depth+1, scope));
    cell* result = new_cons(car(x), rest);
    trace("backquote") << "backquote: " << result;
    return mkref(result);
  }

  TEMP(head, process_unquotes(car(x), depth, scope));
  TEMP(tail, process_unquotes(cdr(x), depth, scope));
  cell* result = new_cons(head, tail);
  trace("backquote") << "=> " << result;
  return mkref(result);
}

long unquote_depth(cell* x) {
  if (is_backquoted(x))
    return unquote_depth(cdr(x))-1;
  if (is_unquoted(x))
    return unquote_depth(cdr(x))+1;
  if (is_unquote_spliced(x))
    return unquote_depth(cdr(x))+1;
  return 0;
}

cell* strip_unquote(cell* x) {
  if (is_backquoted(x) &&
      (is_unquoted(cdr(x)) || is_unquote_spliced(cdr(x))))
    return strip_unquote(cdr(cdr(x)));
  if (is_unquoted(x) || is_unquote_spliced(x))
    return strip_unquote(cdr(x));
  return x;
}

bool has_unquote_splice(cell* x) {
  if (is_unquote_spliced(x))
    return true;
  if (is_quoted(x)
      || is_backquoted(x)
      || is_unquoted(x))
    return has_unquote_splice(cdr(x));
  return false;
}



//// helpers

bool is_quoted(cell* cell) {
  return is_cons(cell) && car(cell) == sym_quote;
}

bool is_backquoted(cell* cell) {
  return is_cons(cell) && car(cell) == sym_backquote;
}

cell* strip_quote(cell* cell) {
  return is_quoted(cell) ? cdr(cell) : cell;
}

bool is_unquoted(cell* arg) {
  return is_cons(arg) && car(arg) == sym_unquote;
}

bool is_spliced(cell* arg) {
  return is_cons(arg) && car(arg) == sym_splice;
}

bool is_unquote_spliced(cell* arg) {
  return is_cons(arg) && car(arg) == sym_unquote_splice;
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

// x must have a dangling ref which will be moved to the result
cell* to_fn(cell* x) {
  if (x == nil || is_fn(x)) return x;
  lease_cell lease(x);
  if (!lookup_dynamic_binding(sym_Coercions)) {
    RAISE << "tried to call " << x << '\n' << die();
    return nil;
  }
  cell* result = coerce_quoted(x, sym_function, lookup(sym_Coercions));
  return result;
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

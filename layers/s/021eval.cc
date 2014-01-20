//// creating functions and macros; calling them with args including @spliced

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
//  @splicing args into macro calls just like regular functions
//
//  do as much work as possible even if all vars aren't correctly setup
//    eval returns a special incomplete_eval object on unbound vars
//    expressions on incomplete_eval objects become incomplete_eval themselves, the boundary rippling outward
//      (cons (object incomplete_eval x) 3) => (object incomplete_eval (cons x 3))
//    'speculatively turns incomplete objects into non-rippling incomplete_eval_data objects
//      beware function wrappers around 'speculatively; arg eval can get subtle
//    eval turns incomplete_eval_data objects back into incompletes, so eval can be retried
//
//  support symbolic_eval mode as a primitive for optimizations
//    symbolic_eval_args returns bindings for a call without actually evaluating args

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
    cell* result = lookup(expr, scope, /*keep_already_evald*/in_macro());
    trace("eval") << "=> " << result;
    return mkref(result);
  }

  if (is_atom(expr)) {
    trace("eval") << "literal";
    trace("eval") << "=> " << expr;
    return mkref(expr);
  }

  if (is_incomplete_eval(expr)) {
    trace("eval") << "incomplete_eval";
    trace("eval") << "=> " << expr;
    return eval(rep(expr), scope);
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
    return mkref(in_macro() ? expr : strip_already_evald(expr));

  // expr is a call
  TEMP(fn, to_fn(eval(car(expr), scope)));
  if (is_incomplete_eval(fn)) {
    cell* result = new_object("incomplete_eval", new_cons(rep(fn), cdr(expr)));
    trace("eval") << "incomplete_eval fn";
    trace("eval") << "=> " << result;
    return mkref(result);
  }
  if (!is_fn(fn))
    RAISE << "Not a call: " << expr << '\n'
        << "Perhaps you need to split the line in two.\n";

  // eval its args in the caller's lexical environment
  TEMP(spliced_args, splice_args(cdr(expr), scope, fn));
  trace("unevald_args") << spliced_args;
  TEMP(new_scope, mkref(new_table()));
  eval_bind_all(sig(fn), spliced_args, scope, new_scope, is_macro(fn));

  if (car(expr) != new_sym("speculatively")
      && any_incomplete_eval(new_scope)) {
    cell* result = ripple_incomplete_eval(fn, new_scope);
    trace("eval") << "ripple";
    trace("eval") << "=> " << result;
    return mkref(result);
  }

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
void eval_bind_all(cell* params, cell* args, cell* scope, cell* new_scope, bool is_macro) {
  trace("eval/bind/all") << params << " <-> " << args;
  if (params == nil)
    ;

  else if (is_sym(strip_quote(params))) {
    cell* dummy = NULL;
    eval_bind_rest(params, args, &dummy, scope, new_scope, is_macro);
  }

  else if (!is_cons(strip_quote(params)))
    ;

  else if (is_quoted(params))
    bind_params(params, args, NULL, new_scope, is_macro, false, true);

  else {
    eval_bind_param(car(params), car(args), scope, new_scope, is_macro);
    eval_bind_all(cdr(params), cdr(args), scope, new_scope, is_macro);
  }
}

void eval_bind_rest(cell* param, cell* args, cell** cached_val, cell* scope, cell* new_scope, bool is_macro) {
  trace("eval/bind/rest") << param << " <-> " << args;

  if (is_quoted(param))
    bind_params(param, args, NULL, new_scope, is_macro, false, true);

  else if (is_cons(param))
    eval_bind_all(param, args, scope, new_scope, is_macro);

  else {
    *cached_val = eval_all(args, scope);
    bind_params(param, *cached_val, args, new_scope, is_macro, false, true);
    rmref(*cached_val);
  }
}

void eval_bind_param(cell* param, cell* arg, cell* scope, cell* new_scope, bool is_macro) {
  trace("eval/bind/param") << param << " <-> " << arg;

  if (is_quoted(param))
    bind_params(param, arg, NULL, new_scope, is_macro, false, /*can't be rest*/false);

  else {
    TEMP(val, eval_arg(arg, scope));
    if (is_incomplete_eval(val) && is_cons(param))
      add_lexical_binding(param, val, new_scope);
    else
      bind_params(param, val, arg, new_scope, is_macro, false, /*can't be rest*/false);
  }
}

// unevald_args might be NULL if params is quoted
void bind_params(cell* params, cell* args, cell* unevald_args, cell* new_scope, bool is_macro, bool params_quoted, bool might_be_rest) {
  trace("eval/bind/one") << params << " <-> " << args;
  if (is_quoted(params)) {
    TEMP(correct_args, pick_and_maybe_quote(args, unevald_args));
    bind_params(strip_quote(params), correct_args, NULL, new_scope, is_macro, true, might_be_rest);
  }

  else if (params == nil)
    ;

  else if (is_sym(params)) {
    trace("eval/bind/one") << "binding " << params << " to " << args;
    // don't bother suppressing eval if param is quoted in a non-macro
    // (matters in definitions generated by macex)
    TEMP(correct_args, mkref(args));
    if (!is_macro && params_quoted && might_be_rest)
      update(correct_args, strip_all_already_evald(args));
    add_lexical_binding(params, correct_args, new_scope);
  }

  else if (!is_cons(params))
    ;

  else {
    bind_params(car(params), car(args), unevald_args && is_cons(unevald_args) ? car(unevald_args) : unevald_args, new_scope, is_macro, params_quoted, /*can't be rest*/false);
    bind_params(cdr(params), cdr(args), unevald_args && is_cons(unevald_args) ? cdr(unevald_args) : unevald_args, new_scope, is_macro, params_quoted, might_be_rest);
  }
}

//// eval args - while respecting already_evald and Do_symbolic_eval

cell* eval_all(cell* args, cell* scope) {
  if (!is_cons(args))
    return eval_arg(args, scope);
  cell* p_result = new_cell(), *curr = p_result;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    TEMP(val, eval_arg(car(args), scope));
    add_cons(curr, val);
  }
  return drop_ptr(p_result);
}

stack<bool> Do_symbolic_eval;

// eval, but always strip '' regardless of in_macro()
cell* eval_arg(cell* arg, cell* scope) {
  trace("already_evald") << "eval_arg " << arg;
  if (is_already_evald(arg)) return mkref(strip_already_evald(arg));
  if (!Do_symbolic_eval.empty() && Do_symbolic_eval.top()) return mkref(arg);
  return eval(arg, scope);
}

COMPILE_FN(symbolic_eval_args, compiledfn_symbolic_eval_args, "($expr)",
  cell* expr = lookup("$expr");
  cell* fn = car(expr);
  if (!is_fn(fn)) {
    RAISE << "Not a call: " << expr << '\n';
    return nil;
  }
  TEMP(spliced_args, splice_args(cdr(expr), Curr_lexical_scope, fn));
  cell* bindings = new_table();
  Do_symbolic_eval.push(true);
    eval_bind_all(sig(fn), spliced_args, Curr_lexical_scope, bindings, is_macro(fn));
  Do_symbolic_eval.pop();
  return mkref(bindings);
)

cell* pick_and_maybe_quote(cell* args, cell* unevald_args) {
  cell* result = unevald_args ? unevald_args : args;
  if (!Do_symbolic_eval.empty() && Do_symbolic_eval.top())
    result = new_cons(sym_quote, result);
  return mkref(result);
}



//// eval @exprs and inline them into args
// tag them with '' (already eval'd) so they can be used with macros

cell* splice_args(cell* args, cell* scope, cell* fn) {
  cell *p_result = new_cell(), *tip = p_result;
  for (cell* curr = args; curr != nil; curr=cdr(curr)) {
    if (!is_spliced(car(curr))) {
      add_cons(tip, car(curr));
      tip=cdr(tip);
      continue;
    }

    if (is_macro(fn) && !contains(body(fn), sym_backquote))
      RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)\n";
    TEMP(x, unsplice(car(curr), scope));
    if (is_incomplete_eval(x))
      add_cons(tip, new_cons(sym_splice, rep(x)));
    else
      for (cell* curr2 = x; curr2 != nil; curr2=cdr(curr2), tip=cdr(tip))
        add_cons(tip, tag_already_evald(car(curr2)));
  }
  trace("splice") << cdr(p_result);
  return drop_ptr(p_result);
}

cell* unsplice(cell* arg, cell* scope) {
  return eval(cdr(arg), scope);
}

// supporting @ in macro calls
stack<bool> In_macro;

// keep sync'd with mac
bool is_macro(cell* fn) {
  if (!is_object(fn)) return false;
  if (type(fn) != sym_function) return false;
  if (!is_quoted(sig(fn))) return false;
  cell* forms = body(fn);
  if (!is_cons(forms)) return false;
  if (cdr(forms) != nil) return false;
  cell* form = car(forms);
  if (!is_cons(form)) return false;
  if (car(form) != sym_eval) return false;
  if (car(cdr(cdr(form))) != sym_caller_scope) return false;
  if (cdr(cdr(cdr(form))) != nil) return false;
  return true;
}

bool in_macro() {
  if (In_macro.empty()) In_macro.push(false);
  return In_macro.top();
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

// when In_macro did we encounter ''?
bool Skipped_already_evald = false;

cell* process_unquotes(cell* x, long depth, cell* scope) {
  new_trace_frame("backquote");
  trace("backquote") << x << " " << depth;
  if (!is_cons(x)) {
    trace("backquote") << "atom: " << x;
    return mkref(x);
  }

  if (!has_unquote_splice(x)
      && unquote_depth(x) == depth) {
    Skipped_already_evald = false;
    cell* result = eval(strip_unquote(x), scope);
    trace("backquote") << "eval: " << result;
    if (Skipped_already_evald) {
      result = push_cons(sym_already_evald, result);
      trace("already_evald") << "push => " << result;
    }
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

cell* maybe_strip_already_evald(bool keep_already_evald, cell* x) {
  trace("already_evald") << "maybe_strip_already_evald " << keep_already_evald << " " << x;
  trace("already_evald") << "Skipped_already_evald used to be " << Skipped_already_evald;
  Skipped_already_evald = is_already_evald(x);
  trace("already_evald") << "Skipped_already_evald is now " << Skipped_already_evald;
  return keep_already_evald ? x : strip_already_evald(x);
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



//// support for partial-eval

bool is_incomplete_eval(cell* x) {
  return type(x) == sym_incomplete_eval;
}

bool any_incomplete_eval(cell* scope) {
  cell_map table = to_table(scope)->value;
  for (cell_map::iterator p = table.begin(); p != table.end(); ++p)
    if (p->second && is_incomplete_eval(p->second))
      return true;
  return false;
}

cell* ripple_incomplete_eval(cell* f, cell* scope) {
  cell *args=new_cell(), *curr=args;
  cell_map table = to_table(scope)->value;
  for (cell* params = strip_quote(sig(f)); params != nil && (!is_cons(params) || !is_object(params)); params=cdr(params), curr=cdr(curr)) {
    cell* param = car(params);
    if (is_quoted(param))
      param = strip_quote(param);
    if (!table[param])
      RAISE << "No binding for " << param << '\n' << die();
    if (is_incomplete_eval(table[param]))
      add_cons(curr, rep(table[param]));
    else
      add_cons(curr, tag_already_evald(table[param]));
  }
  TEMP(result_args, drop_ptr(args));
  return new_object("incomplete_eval", new_cons(f, result_args));
}

COMPILE_FN(speculatively, compiledfn_speculatively, "($x)",
  cell* x = lookup("$x");
  if (!is_incomplete_eval(x)) return mkref(x);
  return mkref(new_object("incomplete_eval_data", rep(x)));
)



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
  if (is_incomplete_eval(x)) return x;
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

cell* strip_all_already_evald(cell* x) {
  if (is_already_evald(x)) return mkref(strip_already_evald(x));
  if (!contains_already_evald(x)) return mkref(x);
  cell* result = new_cell(), *curr = result;
  for (cell* iter = x; iter != nil; iter=cdr(iter), curr=cdr(curr)) {
    if (!is_cons(iter) || is_already_evald(iter)) {
      set_cdr(curr, strip_already_evald(iter));
      break;
    }
    add_cons(curr, strip_already_evald(car(iter)));
  }
  return drop_ptr(result);
}

bool contains_already_evald(cell* x) {
  if (!is_cons(x)) return false;
  if (is_already_evald(x)) return true;
  if (is_quoted(x)) return false;
  if (is_object(x)) return false;
  return is_already_evald(car(x))
      || contains_already_evald(cdr(x));
}

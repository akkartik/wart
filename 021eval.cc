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
//  aliases for naming list args as well as their parts: ((fn (l | (head ... tail)) (prn l " starts with " head)) '(1 2 3))
//  functions can reorder args using keyords: ((fn (a b c) b) :b 3 1 2) => 3
//  functions can document keyword args with a different, aliased name: ((fn (a b|returning c) b) :returning 3 1 2) => 3
//  callers can pass in keyword args to nested functions
//  callers can pass in just needed args: ((fn (a b c) (list a b)) :b 3) => (nil 3)
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

  if (is_sym(expr)) {
    trace("eval") << "sym";
    cell* result = lookup(expr, scope, keep_already_evald());
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
    return mkref(keep_already_evald() ? expr : strip_already_evald(expr));

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
  TEMP(ordered_args, reorder_keyword_args(spliced_args, sig(fn)));
  TEMP(new_scope, mkref(new_table()));
  eval_bind_all(sig(fn), ordered_args, scope, new_scope);

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
    cell* dummy = NULL;
    eval_bind_rest(params, args2, &dummy, scope, new_scope);
  }

  else if (!is_cons(params))
    ;

  else if (is_alias(params))
    eval_bind_rest_aliases(params, args2, scope, new_scope);

  else {
    eval_bind_param(car(params), car(args2), scope, new_scope);
    eval_bind_all(cdr(params), cdr(args2), scope, new_scope);
  }
}

void eval_bind_rest(cell* param, cell* args, cell** cached_val, cell* scope, cell* new_scope) {
  trace("eval/bind/rest") << param << " <-> " << args;
  if (is_cons(param))
    eval_bind_all(param, args, scope, new_scope);

  else {
    *cached_val = eval_all(args, scope);
    bind_params(param, *cached_val, args, new_scope);
    rmref(*cached_val);
  }
}

void eval_bind_param(cell* param, cell* arg, cell* scope, cell* new_scope) {
  trace("eval/bind/param") << param << " <-> " << arg;
  TEMP(arg2, nil);
  if (is_quoted(param)) {
    param = strip_quote(param);
    arg2 = mkref(new_cons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  if (is_alias(param))
    eval_bind_aliases(param, arg2, scope, new_scope);

  else {
    TEMP(val, eval_arg(arg2, scope));
    if (is_incomplete_eval(val) && is_cons(param))
      add_lexical_binding(param, val, new_scope);
    else
      bind_params(param, val, arg2, new_scope);
  }
}

void eval_bind_rest_aliases(cell* params /* (| ...) */, cell* args, cell* scope, cell* new_scope) {
  trace("eval/bind/rest_aliases") << params << " <-> " << args;
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  cell* cached_val = NULL;  // to ensure we don't multiply-eval
  for (cell* aliases = cdr(params); aliases != nil; aliases=cdr(aliases)) {
    if (cached_val)
      bind_params(car(aliases), cached_val, args, new_scope);
    else
      eval_bind_rest(car(aliases), args, &cached_val, scope, new_scope);
  }
}

void eval_bind_aliases(cell* params /* (| ...) */, cell* arg, cell* scope, cell* new_scope) {
  trace("eval/bind/aliases") << params << " <-> " << arg;
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  cell* cached_val = NULL;  // to ensure we don't multiply-eval
  for (cell *aliases=cdr(params), *alias=car(aliases); aliases != nil; aliases=cdr(aliases),alias=car(aliases)) {
    if (is_quoted(alias))
      // TODO: take out strip_quote?
      bind_params(strip_quote(alias), arg, NULL, new_scope);
    else if (cached_val)
      bind_params(alias, cached_val, arg, new_scope);
    else if (is_alias(alias))
      eval_bind_aliases(alias, arg, scope, new_scope);
    else {
      cached_val = eval_arg(arg, scope);
      bind_params(alias, cached_val, arg, new_scope);
      rmref(cached_val);
    }
  }
}

void bind_params(cell* params, cell* args, cell* unevald_args, cell* new_scope) {
  trace("eval/bind/one") << params << " <-> " << args;
  if (is_quoted(params)) {
    if (unevald_args)
      bind_params(strip_quote(params), unevald_args, NULL, new_scope);
    else
      bind_params(strip_quote(params), args, NULL, new_scope);
  }

  else if (params == nil)
    ;

  else if (is_sym(params)) {
    trace("eval/bind/one") << "binding " << params << " to " << args;
    add_lexical_binding(params, args, new_scope);
  }

  else if (!is_cons(params))
    ;

  else if (!is_alias(params) && args != nil && !is_cons(args))
    bind_params(params, nil, nil, new_scope);

  else if (is_alias(params))
    bind_aliases(params, args, unevald_args, new_scope);

  else {
    TEMP(ordered_args, reorder_keyword_args(args, params));
    bind_params(car(params), car(ordered_args), unevald_args && is_cons(unevald_args) ? car(unevald_args) : unevald_args, new_scope);
    bind_params(cdr(params), cdr(ordered_args), unevald_args && is_cons(unevald_args) ? cdr(unevald_args) : unevald_args, new_scope);
  }
}

void bind_aliases(cell* params /* (| ...) */, cell* arg, cell* unevald_arg, cell* new_scope) {
  trace("eval/bind/one_aliases") << params << " <-> " << arg;
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  for (cell* aliases=cdr(params); aliases != nil; aliases=cdr(aliases))
    if (!unsafe_get(new_scope, car(aliases)))  // skip duplicate destructured aliases
      bind_params(car(aliases), arg, unevald_arg, new_scope);
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

// eval, but always strip '' regardless of keep_already_evald()
cell* eval_arg(cell* arg, cell* scope) {
  trace("already_evald") << "eval_arg " << arg;
  if (is_already_evald(arg)) return mkref(strip_already_evald(arg));
  if (Do_symbolic_eval.empty()) Do_symbolic_eval.push(false);
  if (Do_symbolic_eval.top()) return mkref(arg);
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
  TEMP(ordered_args, reorder_keyword_args(spliced_args, sig(fn)));
  Do_symbolic_eval.push(true);
    cell* bindings = mkref(new_table());
    eval_bind_all(sig(fn), ordered_args, Curr_lexical_scope, bindings);
  Do_symbolic_eval.pop();
  return bindings;
)



//// process :keyword args and reorder args to param order -- respecting param aliases

cell* reorder_keyword_args(cell* args, cell* params) {
  if (!is_cons(strip_quote(params))) {
    trace("ordered_args") << "unchanged: " << args;
    return mkref(args);
  }

  TEMP(keyword_args, mkref(new_table()));
  TEMP(non_keyword_args, extract_keyword_args(params, args, keyword_args));
  cell* result = args_in_param_order(params, non_keyword_args, keyword_args);

  trace("ordered_args") << "=> " << result;
  return result;  // already mkref'd
}

// extract keyword args into the keyword_args table and return the remaining non-keyword args
cell* extract_keyword_args(cell* params, cell* args, cell* keyword_args) {
  cell *p_non_keyword_args = new_cell(), *curr = p_non_keyword_args;
  while (is_cons(args)) {
    bool is_rest = false;
    cell* kparam = keyword_param(car(args), params, is_rest);
    if (kparam == NULL) {
      add_cons(curr, car(args));
      curr=cdr(curr);
      args=cdr(args);
      continue;
    }
    args = cdr(args);  // skip keyword arg
    if (!is_rest) {
      set_all_aliases(keyword_args, kparam, car(args));
      args = cdr(args);
    }
    else {
      cell* end_rest = next_keyword(args, params);
      TEMP(rest_args, snip(args, end_rest));
      set_all_aliases(keyword_args, kparam, rest_args);
      args = end_rest;
    }
  }
  set_cdr(curr, args);  // in case improper list
  return drop_ptr(p_non_keyword_args);
}

void set_all_aliases(cell* keyword_args, cell* param, cell* arg) {
  if (is_alias(param))
    for (cell* p = cdr(param); p != nil; p=cdr(p))
      unsafe_set(keyword_args, car(p), arg, false);
  else
    unsafe_set(keyword_args, param, arg, false);
}

cell* next_keyword(cell* args, cell* params) {
  for (args=cdr(args); args != nil; args=cdr(args)) {
    bool dummy;
    if (keyword_param(car(args), params, dummy))
      return args;
  }
  return nil;
}

cell* snip(cell* x, cell* next) {
  if (next == nil) return mkref(x);
  cell* p_result = new_cell();
  for (cell* curr = p_result; x != next; x=cdr(x),curr=cdr(curr))
    add_cons(curr, car(x));
  return drop_ptr(p_result);
}

cell* args_in_param_order(cell* params, cell* non_keyword_args, cell* keyword_args) {
  cell *p_reconstituted_args = new_cell(), *curr = p_reconstituted_args;
  for (params=strip_quote(params); params != nil; curr=cdr(curr), params=strip_quote(cdr(params))) {
    if (is_cons(params) && !is_alias(params)) {
      cell* param = strip_quote(car(params));
      if (is_alias(param))
        param = car(cdr(param));
      cell* keyword_value = unsafe_get(keyword_args, param);
      if (keyword_value) {
        add_cons(curr, keyword_value);
      }
      else {
        add_cons(curr, car(non_keyword_args));
        non_keyword_args = cdr(non_keyword_args);
      }
    }
    else {
      // rest param
      cell* param = is_alias(params) ? car(cdr(params)) : params;
      cell* keyword_value = unsafe_get(keyword_args, param);
      set_cdr(curr, keyword_value ? keyword_value : non_keyword_args);
      break;
    }
  }
  if (non_keyword_args != nil)
    set_cdr(curr, non_keyword_args);  // any remaining args
  return drop_ptr(p_reconstituted_args);
}

// return the appropriate param if arg is a valid keyword arg, or NULL if not
// handle param aliases; :a => (| a b)
// set is_rest if params is a rest param/alias
// doesn't look inside destructured params
cell* keyword_param(cell* arg, cell* params, bool& is_rest) {
  is_rest = false;
  if (!is_keyword_sym(arg)) return NULL;
  TEMP(candidate, mkref(new_sym(to_string(arg).substr(1))));
  for (params=strip_quote(params); params != nil; params=strip_quote(cdr(params))) {
    cell* param = (is_cons(params) && !is_alias(params))
                  ? strip_quote(car(params))
                  : params;  // rest param/alias
    if (param == candidate
        // ignore destructuring
        || (is_alias(param) && param_alias_match(cdr(param), candidate))) {
      if (param == params) is_rest = true;
      return param;
    }
  }
  return NULL;
}

bool param_alias_match(cell* aliases, cell* candidate) {
  for (; aliases != nil; aliases=cdr(aliases)) {
    if (car(aliases) == candidate)
      return true;
  }
  return false;
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
  if (!is_quoted(sig(fn))) return false;
  cell* forms = body(fn);
  if (cdr(forms) != nil) return false;
  cell* form = car(forms);
  if (car(form) != sym_eval) return false;
  if (car(cdr(cdr(form))) != sym_caller_scope) return false;
  if (cdr(cdr(cdr(form))) != nil) return false;
  return true;
}

bool keep_already_evald() {
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

  if (unquote_depth(x) == depth) {
    Skipped_already_evald = false;
    cell* result = eval(strip_unquote(x), scope);
    trace("backquote") << "eval: " << result;
    if (Skipped_already_evald) {
      result = push_cons(sym_already_evald, result);
      trace("already_evald") << "push => " << result;
    }
    return result;  // already mkref'd
  }
  else if (unquote_splice_depth(car(x)) == depth) {
    TEMP(splice, eval(strip_unquote_splice(car(x)), scope));
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
  if (is_unquoted(x))
    return unquote_depth(cdr(x))+1;
  return 0;
}

cell* strip_unquote(cell* x) {
  if (is_unquoted(x))
    return strip_unquote(cdr(x));
  return x;
}

long unquote_splice_depth(cell* x) {
  if (is_unquote_spliced(x))
    return 1;
  if (is_unquoted(x))
    return unquote_splice_depth(cdr(x))+1;
  return 1000;  // never try to splice
}

cell* strip_unquote_splice(cell* x) {
  return cdr(strip_unquote(x));
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
    if (is_alias(param))
      param = car(cdr(param));
    if (!table[param]) continue;
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

cell* to_fn(cell* x) {
  if (x == nil || is_fn(x)) return x;
  if (is_incomplete_eval(x)) return x;
  lease_cell lease(x);  // we assume x is already mkref'd
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

bool is_alias(cell* l) {
  return is_cons(l) && car(l) == sym_param_alias;
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

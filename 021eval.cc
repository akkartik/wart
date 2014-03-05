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
//  functions can reorder args using keywords: ((fn (a b c) b) :b 3 1 2) => 3
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
  TEMP(new_scope, mkref(new_table()));
  bind_params(strip_quote(sig(fn)), is_quoted(sig(fn)), spliced_args, scope, new_scope, is_macro(fn));

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

  end_lexical_scope();
  end_dynamic_scope(CURR_LEXICAL_SCOPE);

  trace("eval") << "=> " << result;
  return result;  // already mkref'd
}

// bind params in sequence to args in new_scope, taking into account:
//  keyword args
//  quoted params (eval'ing args in scope as necessary)
//  destructured params
//  aliased params
//  as-params naming whole and parts
void bind_params(cell* params, bool is_params_quoted, cell* args, cell* scope, cell* new_scope, bool is_macro) {
  trace("bind") << params << " <-> " << args;
  if (args != nil && !is_cons(args))
    bind_param(params, is_params_quoted, args, scope, new_scope);
  else
    bind_params_at(params, params, is_params_quoted, args, args, scope, new_scope, is_macro);
}

// Bind the param at p_params inside params to the appropriate arg inside
// args, and recurse.
void bind_params_at(cell* params, cell* p_params, bool is_params_quoted, cell* args, cell* p_args, cell* scope, cell* new_scope, bool is_macro) {
  trace("bind") << params << p_params << " <-> " << args << p_args << '\n';
  if (p_params == nil) return;

  if (p_args != nil && !is_cons(p_args)) {
    RAISE << "trying to bind against non-cons " << p_args << " like a cons\n";
    return;
  }

  p_args = skip_keyword_args(p_args, params);
  trace("bind") << "skipping ahead to " << p_args << '\n';

  if (is_quoted(p_params) && is_params_quoted) {
    RAISE << "Can't doubly-quote params " << params << '\n';
    return;
  }

  if (is_quoted(p_params)
      || (is_sym(p_params) && is_params_quoted)) {
    p_params = strip_quote(p_params);
    trace("bind") << "quoted rest " << p_params << '\n';
    TEMP(rest_args, mkref(p_args));
    cell* p_keyword_arg = find_keyword_arg(p_params, args);
    if (p_keyword_arg) {
      update(rest_args, snip(cdr(p_keyword_arg),
                             next_keyword_arg(cdr(p_keyword_arg), params)));
      trace("bind") << "quoted rest keyword " << rest_args << '\n';
    }
    if (!is_macro)
      update(rest_args, strip_all_already_evald(rest_args));
    add_lexical_binding(p_params, rest_args, new_scope);
    return;
  }

  if (is_sym(p_params)) {
    trace("bind") << "rest " << p_params << '\n';
    TEMP(rest_args, mkref(p_args));
    cell* p_keyword_arg = find_keyword_arg(p_params, args);
    if (p_keyword_arg) {
      trace("bind") << "until next keyword after " << p_keyword_arg << '\n';
      update(rest_args, snip(cdr(p_keyword_arg),
                             next_keyword_arg(cdr(p_keyword_arg), params)));
      trace("bind") << "rest keyword " << rest_args << '\n';
    }
    TEMP(val, eval_all(rest_args, scope));
    add_lexical_binding(p_params, val, new_scope);
    return;
  }

  if (is_alias(p_params)) {
    trace("bind") << "rest alias " << p_params << '\n';
    if (cdr(cdr(p_params)) == nil)
      RAISE << "just one param alias: " << p_params << ". Are you sure?\n";
    TEMP(rest_args, mkref(p_args));
    cell* p_keyword_arg = find_any_keyword_arg(cdr(p_params), args);
    if (p_keyword_arg) {
      trace("bind") << "until next keyword after " << p_keyword_arg << '\n';
      update(rest_args, snip(cdr(p_keyword_arg),
                             next_keyword_arg(cdr(p_keyword_arg), params)));
      trace("bind") << "rest alias keyword " << rest_args << '\n';
    }
    TEMP(val, nil);
    bool eval_done = false;
    for (cell* aliases = cdr(p_params); aliases != nil; aliases=cdr(aliases)) {
      cell* alias = car(aliases);
      if (is_cons(strip_quote(alias)) && cdr(aliases) != nil)
        RAISE << "only the last alias can contain multiple names " << p_params << '\n';
      else if (is_params_quoted && is_quoted(alias))
        RAISE << "can't doubly-quote param alias " << p_params << '\n';
      else {
        if (is_quoted(alias)) {
          if (is_cons(strip_quote(alias)) && is_cons(rest_args)) {
            trace("bind") << "quoted destructured rest alias (as-param) " << alias << '\n';
            bind_params(alias, true, rest_args, scope, new_scope, is_macro);
          }
          else {
            trace("bind") << "quoted rest alias " << alias << '\n';
            add_lexical_binding(strip_quote(alias), rest_args, new_scope);
          }
        }
        else if (is_sym(alias)) {
          trace("bind") << "rest alias sym " << alias << '\n';
          if (!eval_done) {
            if (is_params_quoted)
              update(val, mkref(rest_args));
            else
              update(val, eval_all(rest_args, scope));
            eval_done = true;
          }
          add_lexical_binding(alias, val, new_scope);
        }
        else if (is_cons(alias)) {
          trace("bind") << "multiple rest aliases " << alias << '\n';
          // subtly distinct from a destructured alias that's not in rest
          // position. In (a ... (| b (c d))) you don't eval an arg before
          // destructuring its components like in (a (| b (c d)))
          trace("bind") << "rest alias cons (as-param) " << alias << '\n';
          bind_params(alias, is_params_quoted, is_cons(rest_args) ? rest_args : nil, scope, new_scope, is_macro);
        }
        else {
          RAISE << "unknown alias in " << p_params << '\n';
        }
      }
    }
    return;
  }

  cell* param = car(p_params);
  if (is_quoted(param) && is_params_quoted) {
    RAISE << "can't doubly-quote params " << params << '\n';
    return;
  }

  if (is_alias(param)) {
    trace("bind") << "alias " << param << '\n';
    if (cdr(cdr(param)) == nil)
      RAISE << "just one param alias: " << param << ". Are you sure?\n";
    cell* p_keyword_arg = find_any_keyword_arg(cdr(param), args);
    if (p_keyword_arg) {
      bind_aliases(param, is_params_quoted, car(cdr(p_keyword_arg)), scope, new_scope, is_macro);
      bind_params_at(params, cdr(p_params), is_params_quoted, args, p_args, scope, new_scope, is_macro);
    }
    else {
      bind_aliases(param, is_params_quoted, car(p_args), scope, new_scope, is_macro);
      bind_params_at(params, cdr(p_params), is_params_quoted, args, cdr(p_args), scope, new_scope, is_macro);
    }
    return;
  }

  if ((is_quoted(param) && is_sym(strip_quote(param)))
      || (is_sym(param) && is_params_quoted)) {
    param = strip_quote(param);
    trace("bind") << "quoted " << param << '\n';
    // TODO: should we strip_already_evald on arg if !macro?
    cell* p_keyword_arg = find_keyword_arg(param, args);
    if (p_keyword_arg) {
      add_lexical_binding(param, car(cdr(p_keyword_arg)), new_scope);
      bind_params_at(params, cdr(p_params), is_params_quoted, args, p_args, scope, new_scope, is_macro);
    }
    else {
      add_lexical_binding(param, car(p_args), new_scope);
      if (cdr(p_args) != nil && !is_cons(cdr(p_args)))
        bind_param(cdr(p_params), is_params_quoted, cdr(p_args), scope, new_scope);
      else
        bind_params_at(params, cdr(p_params), is_params_quoted, args, cdr(p_args), scope, new_scope, is_macro);
    }
    return;
  }

  // destructured params don't support keywords on the outside
  if ((is_quoted(param) && is_cons(strip_quote(param)))
      || (is_cons(param) && is_params_quoted)) {
    param = strip_quote(param);
    trace("bind") << "quoted destructured " << param << '\n';
    bind_params(param, true, car(p_args), scope, new_scope, is_macro);
    bind_params_at(params, cdr(p_params), is_params_quoted, args, cdr(p_args), scope, new_scope, is_macro);
    return;
  }

  if (is_cons(param)) {
    trace("bind") << "destructured " << param << '\n';
    TEMP(val, eval_arg(car(p_args), scope));
    bind_params(param, true, val, scope, new_scope, is_macro);
    bind_params_at(params, cdr(p_params), is_params_quoted, args, cdr(p_args), scope, new_scope, is_macro);
    return;
  }

  trace("bind") << "regular " << param << '\n';
  cell* p_keyword_arg = find_keyword_arg(param, args);
  if (p_keyword_arg) {
    trace("bind") << "regular keyword arg " << car(cdr(p_keyword_arg)) << '\n';
    TEMP(val, eval_arg(car(cdr(p_keyword_arg)), scope));
    trace("bind") << "after eval " << val << '\n';
    add_lexical_binding(param, val, new_scope);
    bind_params_at(params, cdr(p_params), is_params_quoted, args, p_args, scope, new_scope, is_macro);
  }
  else {
    TEMP(val, eval_arg(car(p_args), scope));
    trace("bind") << "after eval " << val << '\n';
    add_lexical_binding(param, val, new_scope);
    bind_params_at(params, cdr(p_params), is_params_quoted, args, cdr(p_args), scope, new_scope, is_macro);
  }
}

void bind_aliases(cell* param, bool is_params_quoted, cell* arg, cell* scope, cell* new_scope, bool is_macro) {
  TEMP(val, nil);
  bool eval_done = false;
  for (cell* aliases = cdr(param); aliases != nil; aliases=cdr(aliases)) {
    cell* alias = car(aliases);
    if (is_cons(strip_quote(alias)) && cdr(aliases) != nil)
      RAISE << "only the last alias can contain multiple names " << param << '\n';
    else if (is_params_quoted && is_quoted(alias))
      RAISE << "can't doubly-quote param alias " << param << '\n';
    else {
      if (is_quoted(alias) || is_params_quoted) { // TODO: !is_sym(alias)
        trace("bind") << "quoted alias " << alias << '\n';
        add_lexical_binding(strip_quote(alias), arg, new_scope);
      }
      else if (is_sym(alias)) {
        trace("bind") << "alias sym " << alias << '\n';
        if (!eval_done) {
          update(val, eval_arg(arg, scope));
          eval_done = true;
        }
        if (!unsafe_get(new_scope, alias))  // skip duplicate aliases without warning
          add_lexical_binding(alias, val, new_scope);
      }
      else if (is_alias(alias)) {
        trace("bind") << "nested alias (as-param) " << alias << '\n';
        bind_aliases(alias, is_params_quoted, arg, scope, new_scope, is_macro);
      }
      else if (is_cons(alias)) {
        trace("bind") << "destructured alias (as-param) " << alias << '\n';
        if (!eval_done) {
          update(val, eval_arg(arg, scope));
          eval_done = true;
        }
        bind_params(alias, true, is_cons(val) ? val : nil, scope, new_scope, is_macro);
      }
      else {
        RAISE << "unknown alias in " << param << '\n';
      }
    }
  }
}

cell* find_any_keyword_arg(cell* aliases, cell* args) {
  cell* result = NULL;
  for (; aliases != nil; aliases=cdr(aliases)) {
    if (!is_cons(strip_quote(car(aliases)))) {
      cell* curr = find_keyword_arg(strip_quote(car(aliases)), args);
      if (curr && result) RAISE << "conflicting keyword args in " << args << '\n';
      if (curr) result = curr;
    }
    else {
      if (cdr(aliases) != nil)
        RAISE << "only the last alias can contain multiple names " << aliases << '\n';
      // no keyword args for destructured params
    }
  }
  return result;
}

void bind_param(cell* param, bool is_params_quoted, cell* arg, cell* scope, cell* new_scope) {
  if (is_params_quoted || is_quoted(param)) {
    if (is_params_quoted && is_quoted(param))
      RAISE << "Can't doubly-quote param " << param << '\n';
    add_lexical_binding(strip_quote(param), arg, new_scope);
  }
  else if (is_sym(param)) {
    TEMP(val, eval_arg(arg, scope));
    add_lexical_binding(param, val, new_scope);
  }
  else if (is_alias(param))
    for (cell* aliases = cdr(param); aliases != nil; aliases=cdr(aliases))
      bind_param(car(aliases), is_params_quoted, arg, scope, new_scope);
  else
    RAISE << "don't know how to bind " << param << " to " << arg;
}

cell* find_keyword_arg(cell* param, cell* args) {
  if (!is_sym(param)) return NULL;
  cell* keyword_sym = new_sym(":"+to_string(param));
  for (; args != nil; args=cdr(args))
    if (keyword_sym == (is_cons(args) ? car(args) : args))
      return args;
  return NULL;
}

cell* skip_keyword_args(cell* args, cell* params) {
  trace("bind") << "skip keyword? " << args << "\n";
  if (!is_keyword_sym(car(args))) return args;
  TEMP(maybe_param, mkref(new_sym(to_string(car(args)).substr(1))));
  if (!contains_non_destructured_param(params, maybe_param)) {
    trace("bind") << "done skipping " << args << '\n';
    return args;
  }
  if (is_rest_param(maybe_param, params)) {
    trace("bind") << "skipping rest keyword args";
    return skip_keyword_args(next_keyword_arg(cdr(args), params), params);
  }
  trace("bind") << "skipping keyword arg" << car(args) << ' ' << car(cdr(args));
  return skip_keyword_args(cdr(cdr(args)), params);
}

bool contains_non_destructured_param(cell* params, cell* sym) {
  trace("bind") << "match? " << sym << " in " << params << '\n';
  if (params == nil) return false;
  if (is_quoted(params)) params = strip_quote(params);
  if (params == sym) return true;
  if (!is_cons(params)) return false;
  if (is_alias(params)) {
    trace("bind") << "match rest alias\n";
    return param_alias_match(cdr(params), sym)
           || contains_non_destructured_param(cdr(params), sym);
  }
  cell* param = strip_quote(car(params));
  if (sym == param) return true;
  if (is_alias(param)) {
    trace("bind") << "match alias\n";
    return param_alias_match(cdr(param), sym)
           || contains_non_destructured_param(cdr(params), sym);
  }
  return contains_non_destructured_param(cdr(params), sym);
}

cell* next_keyword_arg(cell* args, cell* params) {
  if (args == nil)
    return args;
  if (!is_keyword_sym(car(args)))
    return next_keyword_arg(cdr(args), params);
  trace("bind") << "at keyword sym " << args << '\n';
  TEMP(maybe_param, mkref(new_sym(to_string(car(args)).substr(1))));
  trace("bind") << "scanning for " << maybe_param << '\n';
  if (contains_non_destructured_param(params, maybe_param))
    return args;
  return next_keyword_arg(cdr(args), params);
}

bool is_rest_param(cell* param, cell* params) {
  if (param == nil) return false;
  cell* curr = params;
  while (is_cons(curr) && !is_alias(curr)) {
    curr = cdr(curr);
  }
  if (is_alias(curr))
    return param_alias_match(cdr(curr), param);
  return curr == param;
}

bool param_alias_match(cell* aliases, cell* candidate) {
  for (; aliases != nil; aliases=cdr(aliases)) {
    if (car(aliases) == candidate)
      return true;
  }
  return false;
}

cell* snip(cell* x, cell* next) {
  if (next == nil) return mkref(x);
  cell* p_result = new_cell();
  for (cell* curr = p_result; x != next; x=cdr(x),curr=cdr(curr))
    add_cons(curr, car(x));
  return drop_ptr(p_result);
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
    bind_params(strip_quote(sig(fn)), is_quoted(sig(fn)), spliced_args, Curr_lexical_scope, bindings, is_macro(fn));
  Do_symbolic_eval.pop();
  return mkref(bindings);
)



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
    if (is_alias(param))
      param = car(cdr(param));
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
  return coerce_quoted(x, sym_function, lookup(sym_Coercions));
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

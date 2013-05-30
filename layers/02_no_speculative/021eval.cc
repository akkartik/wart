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
//  support Do_symbolic_eval mode as a primitive for optimizations
//    symbolic_eval_args returns bindings for a call without actually evaluating args

cell* eval(cell* expr) {
  return eval(expr, Curr_lexical_scope);
}

long Eval_count = 0;

cell* eval(cell* expr, cell* scope) {
  ++Eval_count;
  if (!expr)
    RAISE << "eval: cell should never be NULL\n" << die();

  if (expr == nil)
    return nil;

  if (is_keyword_sym(expr))
    return mkref(expr);

  if (is_sym(expr))
    return mkref(lookup(expr, scope, keep_already_evald()));

  if (is_atom(expr))
    return mkref(expr);

  if (is_object(expr))
    return mkref(expr);

  if (is_quoted(expr))
    return mkref(cdr(expr));

  if (is_backquoted(expr))
    return process_unquotes(cdr(expr), 1, scope);  // already mkref'd

  if (is_already_evald(expr))
    return mkref(keep_already_evald() ? expr : strip_already_evald(expr));

  // expr is a call
  cell* fn = to_fn(eval(car(expr), scope));
  if (!is_fn(fn))
    RAISE << "Not a call: " << expr << '\n'
        << "Perhaps you need to split the line in two.\n";

  // eval its args in the caller's lexical environment
  cell* spliced_args = splice_args(cdr(expr), scope, fn);
  cell* ordered_args = reorder_keyword_args(spliced_args, sig(fn));
  cell* new_scope = new_table();
  eval_bind_all(sig(fn), ordered_args, scope, new_scope);

  // swap in the function's lexical environment
  new_dynamic_scope(CURR_LEXICAL_SCOPE, is_compiledfn(body(fn)) ? scope : env(fn));
  add_lexical_scope(new_scope);
  add_lexical_binding(sym_caller_scope, scope);

  cell* result = nil;
  if (is_compiledfn(body(fn))) {
    result = to_compiledfn(body(fn))();  // all Compiledfns must mkref result
  }
  else {
    // eval all forms in body, save result of final form
    for (cell* form = impl(fn); form != nil; form=cdr(form)) {
      rmref(result);
      result = eval(car(form), Curr_lexical_scope);
    }
  }

  end_lexical_scope();  // implicitly rmrefs new_scope
  end_dynamic_scope(CURR_LEXICAL_SCOPE);
  rmref(ordered_args);
  rmref(spliced_args);
  rmref(fn);
  return result;  // already mkref'd
}

// bind params to args in new_scope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
//  aliased params
void eval_bind_all(cell* params, cell* args, cell* scope, cell* new_scope) {
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
  rmref(args2);
}

void eval_bind_rest(cell* param, cell* args, cell** cached_val, cell* scope, cell* new_scope) {
  if (is_cons(param))
    eval_bind_all(param, args, scope, new_scope);

  else {
    *cached_val = eval_all(args, scope);
    bind_params(param, *cached_val, args, new_scope);
    rmref(*cached_val);
  }
}

void eval_bind_param(cell* param, cell* arg, cell* scope, cell* new_scope) {
  cell* arg2 = NULL;
  if (is_quoted(param)) {
    param = strip_quote(param);
    arg2 = mkref(new_cons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  if (is_alias(param))
    eval_bind_aliases(param, arg2, scope, new_scope);

  else {
    cell* val = eval_arg(arg2, scope);
    bind_params(param, val, arg2, new_scope);
    rmref(val);
  }
  rmref(arg2);
}

void eval_bind_rest_aliases(cell* params /* (| ...) */, cell* args, cell* scope, cell* new_scope) {
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  cell* cached_val = NULL;   // to ensure we don't multiply-eval
  for (cell* aliases = cdr(params); aliases != nil; aliases=cdr(aliases)) {
    if (cached_val)
      bind_params(car(aliases), cached_val, args, new_scope);
    else
      eval_bind_rest(car(aliases), args, &cached_val, scope, new_scope);
  }
}

void eval_bind_aliases(cell* params /* (| ...) */, cell* arg, cell* scope, cell* new_scope) {
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  cell* cached_val = NULL;   // to ensure we don't multiply-eval
  for (cell *aliases=cdr(params), *alias=car(aliases); aliases != nil; aliases=cdr(aliases),alias=car(aliases)) {
    if (is_quoted(alias))
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

// NULL unevald_args => args are already quoted
void bind_params(cell* params, cell* args, cell* unevald_args, cell* new_scope) {
  if (is_quoted(params)) {
    if (unevald_args)
      bind_params(strip_quote(params), unevald_args, NULL, new_scope);
    else
      bind_params(strip_quote(params), args, NULL, new_scope);
  }

  else if (params == nil)
    ;

  else if (is_sym(params))
    add_lexical_binding(params, args, new_scope);

  else if (!is_cons(params))
    ;

  else if (!is_alias(params) && args != nil && !is_cons(args))
    bind_params(params, nil, nil, new_scope);

  else if (is_alias(params))
    bind_aliases(params, args, unevald_args, new_scope);

  else {
    cell* ordered_args = reorder_keyword_args(args, params);
    bind_params(car(params), car(ordered_args), unevald_args && is_cons(unevald_args) ? car(unevald_args) : unevald_args, new_scope);
    bind_params(cdr(params), cdr(ordered_args), unevald_args && is_cons(unevald_args) ? cdr(unevald_args) : unevald_args, new_scope);
    rmref(ordered_args);
  }
}

void bind_aliases(cell* params /* (| ...) */, cell* arg, cell* unevald_arg, cell* new_scope) {
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  for (cell* aliases=cdr(params); aliases != nil; aliases=cdr(aliases))
    if (!unsafe_get(new_scope, car(aliases))) // skip duplicate destructured aliases
      bind_params(car(aliases), arg, unevald_arg, new_scope);
}

//// eval args - while respecting already_evald and Do_symbolic_eval

cell* eval_all(cell* args, cell* scope) {
  if (!is_cons(args))
    return eval_arg(args, scope);
  cell* p_result = new_cell(), *curr = p_result;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    cell* val = eval_arg(car(args), scope);
    add_cons(curr, val);
    rmref(val);
  }
  return drop_ptr(p_result);
}

stack<bool> Do_symbolic_eval;

// eval, but always strip '' regardless of keep_already_evald()
cell* eval_arg(cell* arg, cell* scope) {
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
  cell* spliced_args = splice_args(cdr(expr), Curr_lexical_scope, fn);
  cell* ordered_args = reorder_keyword_args(spliced_args, sig(fn));
  Do_symbolic_eval.push(true);
    cell* bindings = mkref(new_table());
    eval_bind_all(sig(fn), ordered_args, Curr_lexical_scope, bindings);
  Do_symbolic_eval.pop();
  rmref(ordered_args);
  rmref(spliced_args);
  return bindings;
)



//// process :keyword args and reorder args to param order -- respecting param aliases

cell* reorder_keyword_args(cell* args, cell* params) {
  if (!is_cons(strip_quote(params))) return mkref(args);

  cell_map keyword_args;  // all values will be refcounted.
  cell* non_keyword_args = extract_keyword_args(params, args, keyword_args);
  cell* result = args_in_param_order(params, non_keyword_args, keyword_args);   rmref(non_keyword_args);

  for (cell_map::iterator p = keyword_args.begin(); p != keyword_args.end(); ++p)
    if (p->second) rmref(p->second);
  return result;  // already mkref'd
}

// extract keyword args into the cell_map provided; return non-keyword args
// always mkref what you insert into the cell_map
cell* extract_keyword_args(cell* params, cell* args, cell_map& keyword_args) {
  cell *p_non_keyword_args = new_cell(), *curr = p_non_keyword_args;
  for (; is_cons(args); args=cdr(args)) {
    cell* kparam = keyword_param(car(args), params);
    if (kparam == nil) {
      add_cons(curr, car(args));
      curr=cdr(curr);
    }
    // keyword arg for rest param alias
    else if (is_cons(kparam) && cdr(kparam) == nil
             && is_alias(car(kparam))) {
      args = cdr(args);   // skip keyword arg
      cell* end_rest = next_keyword(args, params);
      cell* rest_args = snip(args, end_rest);
      for (cell* p = cdr(car(kparam)); p != nil; p=cdr(p))
        keyword_args[car(p)] = mkref(rest_args);
      rmref(rest_args);
      rmref(kparam);
      args = end_rest;
    }
    // keyword arg for param alias
    else if (is_alias(kparam)) {
      args = cdr(args);   // skip keyword arg
      for (cell* p = cdr(kparam); p != nil; p=cdr(p))
        keyword_args[car(p)] = mkref(car(args));
    }
    // simple rest keyword arg
    else if (is_cons(kparam)) {   // rest keyword arg
      args = cdr(args);
      cell* end_rest = next_keyword(args, params);
      keyword_args[car(kparam)] = snip(args, end_rest);  // already mkref'd
      rmref(kparam);
      args = end_rest;
    }
    // simple keyword arg
    else {
      args = cdr(args);   // skip keyword arg
      keyword_args[kparam] = mkref(car(args));
    }
  }
  if (!is_cons(args))  // improper list
    set_cdr(curr, args);
  return drop_ptr(p_non_keyword_args);
}

cell* next_keyword(cell* args, cell* params) {
  for (args=cdr(args); args != nil; args=cdr(args)) {
    if (keyword_param(car(args), params) != nil)
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

cell* args_in_param_order(cell* params, cell* non_keyword_args, cell_map& keyword_args) {
  cell *p_reconstituted_args = new_cell(), *curr = p_reconstituted_args;
  for (params=strip_quote(params); params != nil; curr=cdr(curr), params=strip_quote(cdr(params))) {
    if (!is_cons(params)) {
      set_cdr(curr, keyword_args[params] ? keyword_args[params] : non_keyword_args);
      break;
    }

    if (is_alias(params)) {
      if (keyword_args[car(cdr(params))]) {
        set_cdr(curr, keyword_args[car(cdr(params))]);
        break;
      }
      else {
        set_cdr(curr, non_keyword_args);
        break;
      }
    }

    cell* param = strip_quote(car(params));
    if (is_alias(param))
      param = car(cdr(param));

    if (keyword_args[param]) {
      add_cons(curr, keyword_args[param]);
    }
    else {
      add_cons(curr, car(non_keyword_args));
      non_keyword_args = cdr(non_keyword_args);
    }
  }
  if (non_keyword_args != nil)
    set_cdr(curr, non_keyword_args);   // any remaining args
  return drop_ptr(p_reconstituted_args);
}

// return the appropriate param if arg is a valid keyword arg
// handle param aliases; :a => (| a b)
// respond to rest keyword args with (rest-param)
// combining the two: respond to rest param aliases with ((| do body))
// doesn't look inside destructured params
cell* keyword_param(cell* arg, cell* params) {
  if (!is_keyword_sym(arg)) return nil;
  cell* candidate = new_sym(to_string(arg).substr(1));
  for (params=strip_quote(params); params != nil; params=strip_quote(cdr(params))) {
    if (!is_cons(params)) { // rest param
      if (params == candidate)
        return new_cons(candidate);
    }
    else if (is_alias(params)) {   // rest param aliases
      if (param_alias_match(cdr(params), candidate))
        return new_cons(params);
    }
    // ignore destructuring except param aliases
    else if (is_alias(strip_quote(car(params)))) {
      if (param_alias_match(cdr(strip_quote(car(params))), candidate))
        return strip_quote(car(params));
    }
    else if (strip_quote(car(params)) == candidate) {
      return candidate;
    }
  }
  return nil;
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
    cell* x = unsplice(car(curr), scope);
    for (cell* curr2 = x; curr2 != nil; curr2=cdr(curr2), tip=cdr(tip))
      add_cons(tip, tag_already_evald(car(curr2)));
    rmref(x);
  }
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
  while (is_already_evald(cell))
    cell = cdr(cell);
  return cell;
}



//// backquoted exprs

// when In_macro did we encounter ''?
bool Skipped_already_evald = false;

cell* process_unquotes(cell* x, long depth, cell* scope) {
  if (!is_cons(x)) return mkref(x);

  if (unquote_depth(x) == depth) {
    Skipped_already_evald = false;
    cell* result = eval(strip_unquote(x), scope);
    return Skipped_already_evald ? push_cons(sym_already_evald, result) : result;
  }
  else if (unquote_splice_depth(car(x)) == depth) {
    cell* result = eval(strip_unquote_splice(car(x)), scope);
    cell* splice = process_unquotes(cdr(x), depth, scope);
    if (result == nil) return splice;

    // always splice in a copy
    cell* resultcopy = copy_list(result);
    rmref(result);
    append(resultcopy, splice);
    rmref(splice);
    return mkref(resultcopy);
  }
  else if (unquote_depth(x) > 0) {
    return mkref(x);
  }

  if (is_backquoted(x)) {
    cell* result = new_cons(car(x), process_unquotes(cdr(x), depth+1, scope));
    rmref(cdr(result));
    return mkref(result);
  }

  cell* result = new_cons(process_unquotes(car(x), depth, scope),
                         process_unquotes(cdr(x), depth, scope));
  rmref(car(result));
  rmref(cdr(result));
  return mkref(result);
}

cell* maybe_strip_already_evald(bool keep_already_evald, cell* x) {
  Skipped_already_evald = is_already_evald(x);
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
  if (!lookup_dynamic_binding(sym_Coercions))
    RAISE << "tried to call " << x << '\n' << die();
  cell* result = coerce_quoted(x, sym_function, lookup(sym_Coercions));   rmref(x);
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

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
//
//  list templates: backquote to suppress eval, unquote to reenable eval inside backquote. `(+ ,a ,b)
//  ability to splice multiple elements into lists: ,@vars inside backquote, @vars otherwise
//  macros need to access caller environment
//  @splicing args into macro calls just like regular functions

cell* eval(cell* expr) {
  return eval(expr, Curr_lexical_scope);
}

cell* eval(cell* expr, cell* scope) {
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
  cell* new_scope = new_table();
  eval_bind_all(sig(fn), spliced_args, scope, new_scope);

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
  rmref(spliced_args);
  rmref(fn);
  return result;  // already mkref'd
}

// bind params to args in new_scope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
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
    cell* val = eval_all(args2, scope);
    bind_params(params, val, new_scope);
    rmref(val);
  }

  else if (!is_cons(params))
    ;

  else {
    eval_bind_param(car(params), car(args2), scope, new_scope);
    eval_bind_all(cdr(params), cdr(args2), scope, new_scope);
  }
  rmref(args2);
}

void eval_bind_param(cell* param, cell* arg, cell* scope, cell* new_scope) {
  cell* arg2 = NULL;
  if (is_quoted(param)) {
    param = strip_quote(param);
    arg2 = mkref(new_cons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  cell* val = eval_arg(arg2, scope);
  bind_params(param, val, new_scope);
  rmref(val);
  rmref(arg2);
}

void bind_params(cell* params, cell* args, cell* new_scope) {
  if (is_quoted(params))
    bind_params(strip_quote(params), args, new_scope);

  else if (params == nil)
    ;

  else if (is_sym(params))
    add_lexical_binding(params, args, new_scope);

  else if (!is_cons(params))
    ;

  else if (args != nil && !is_cons(args))
    bind_params(params, nil, new_scope);

  else {
    bind_params(car(params), car(args), new_scope);
    bind_params(cdr(params), cdr(args), new_scope);
  }
}

//// eval args - while respecting already_evald

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

// eval, but always strip '' regardless of keep_already_evald()
cell* eval_arg(cell* arg, cell* scope) {
  if (is_already_evald(arg)) return mkref(strip_already_evald(arg));
  return eval(arg, scope);
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

cell* quote(cell* x) {
  return new_cons(sym_quote, x);
}

cell* quote_all(cell* x) {
  cell* result = new_cell(), *curr = result;
  for (cell* iter = x; iter != nil; iter=cdr(iter), curr=cdr(curr))
    add_cons(curr, quote(car(iter)));
  return drop_ptr(result);
}

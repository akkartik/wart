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

Cell* eval(Cell* expr) {
  return eval(expr, currLexicalScope);
}

Cell* eval(Cell* expr, Cell* scope) {
  if (!expr)
    RAISE << "eval: cell should never be NULL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isColonSym(expr))
    return mkref(expr);

  if (isSym(expr))
    return mkref(lookup(expr, scope));

  if (isAtom(expr))
    return mkref(expr);

  if (isObject(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  // expr is a call
  Cell* fn = toFn(eval(car(expr), scope));
  if (!isFn(fn))
    RAISE << "Not a call: " << expr << endl
        << "Perhaps you need to split the line in two." << endl;

  // eval its args in the caller's lexical environment
  Cell* newScope = newTable();
  evalBindAll(sig(fn), cdr(expr), scope, newScope);

  // swap in the function's lexical environment
  newDynamicScope(CURR_LEXICAL_SCOPE, isCompiledFn(body(fn)) ? scope : env(fn));
  addLexicalScope(newScope);

  Cell* result = nil;
  if (isCompiledFn(body(fn))) {
    result = toCompiledFn(body(fn))();  // all compiledFns must mkref result
  }
  else {
    // eval all forms in body, save result of final form
    for (Cell* form = impl(fn); form != nil; form=cdr(form)) {
      rmref(result);
      result = eval(car(form), currLexicalScope);
    }
  }

  endLexicalScope();  // implicitly rmrefs newScope
  endDynamicScope(CURR_LEXICAL_SCOPE);
  rmref(fn);
  return result;  // already mkref'd
}

// bind params to args in newScope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
void evalBindAll(Cell* params, Cell* args, Cell* scope, Cell* newScope) {
  if (params == nil)
    return;

  Cell* args2 = NULL;
  if (isQuoted(params)) {
    params = stripQuote(params);
    args2 = quoteAll(args);
  }
  else {
    args2 = mkref(args);
  }

  if (isSym(params)) {
    Cell* val = evalAll(args2, scope);
    bindParams(params, val, newScope);
    rmref(val);
  }

  else if (!isCons(params))
    ;

  else {
    evalBindParam(car(params), car(args2), scope, newScope);
    evalBindAll(cdr(params), cdr(args2), scope, newScope);
  }
  rmref(args2);
}

void evalBindParam(Cell* param, Cell* arg, Cell* scope, Cell* newScope) {
  Cell* arg2 = NULL;
  if (isQuoted(param)) {
    param = stripQuote(param);
    arg2 = mkref(newCons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  Cell* val = eval(arg2, scope);
  bindParams(param, val, newScope);
  rmref(val);
  rmref(arg2);
}

void bindParams(Cell* params, Cell* args, Cell* newScope) {
  if (isQuoted(params))
    bindParams(stripQuote(params), args, newScope);

  else if (params == nil)
    ;

  else if (isSym(params))
    addLexicalBinding(params, args, newScope);

  else if (!isCons(params))
    ;

  else if (args != nil && !isCons(args))
    bindParams(params, nil, newScope);

  else {
    bindParams(car(params), car(args), newScope);
    bindParams(cdr(params), cdr(args), newScope);
  }
}

Cell* evalAll(Cell* args, Cell* scope) {
  if (!isCons(args))
    return eval(args, scope);
  Cell* pResult = newCell(), *curr = pResult;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    Cell* val = eval(car(args), scope);
    addCons(curr, val);
    rmref(val);
  }
  return dropPtr(pResult);
}



//// helpers

bool isQuoted(Cell* cell) {
  return isCons(cell) && car(cell) == sym_quote;
}

Cell* stripQuote(Cell* cell) {
  return isQuoted(cell) ? cdr(cell) : cell;
}

bool isColonSym(Cell* x) {
  if (!isSym(x)) return false;
  string name = toString(x);
  if (name == ":") return false;
  return name[0] == ':';
}

// fn = (object function {sig => .., body => .., env => ..})
bool isFn(Cell* x) {
  return isCons(x) && type(x) == sym_function;
}

Cell* toFn(Cell* x) {
  if (x == nil || isFn(x)) return x;
  if (!lookupDynamicBinding(sym_Coercions))
    RAISE << "tried to call " << x << endl << DIE;
  Cell* result = coerceQuoted(x, sym_function, lookup(sym_Coercions));   rmref(x);
  return result;
}

Cell* sig(Cell* fn) {
  return get(rep(fn), sym_sig);
}

Cell* body(Cell* fn) {
  return get(rep(fn), sym_body);
}

Cell* impl(Cell* fn) {
  Cell* impl = get(rep(fn), sym_optimized_body);
  return (impl != nil) ? impl : body(fn);
}

Cell* env(Cell* fn) {
  return get(rep(fn), sym_env);
}

Cell* quote(Cell* x) {
  return newCons(sym_quote, x);
}

Cell* quoteAll(Cell* x) {
  Cell* result = newCell(), *curr = result;
  for (Cell* iter = x; iter != nil; iter=cdr(iter), curr=cdr(curr))
    addCons(curr, quote(car(iter)));
  return dropPtr(result);
}

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

Cell* eval(Cell* expr) {
  if (!expr)
    RAISE << "eval: cell should never be NULL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isColonSym(expr))
    return mkref(expr);

  if (isSym(expr))
    return mkref(lookup(expr));

  if (isAtom(expr))
    return mkref(expr);

  if (isObject(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  // expr is a call
  Cell* fn = eval(car(expr));
  if (!isFn(fn))
    RAISE << "Not a call: " << expr << endl
        << "Perhaps you need to split the line in two." << endl;

  // eval its args, create new bindings
  list<Cell*> varsBound;
  evalBindAll(sig(fn), cdr(expr), varsBound);

  Cell* result = nil;
  if (isCompiledFn(body(fn))) {
    result = toCompiledFn(body(fn))();  // all compiledFns must mkref result
  }
  else {
    // eval all forms in body, save result of final form
    for (Cell* form = impl(fn); form != nil; form=cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }
  }

  for (list<Cell*>::iterator p = varsBound.begin(); p != varsBound.end(); ++p)
    endDynamicScope(*p);
  rmref(fn);
  return result;  // already mkref'd
}

// bind params to args in newScope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
void evalBindAll(Cell* params, Cell* args, list<Cell*>& varsBound) {
  if (params == nil)
    return ;

  Cell* args2 = NULL;
  if (isQuoted(params)) {
    params = stripQuote(params);
    args2 = quoteAll(args);
  }
  else {
    args2 = mkref(args);
  }

  if (isSym(params)) {
    Cell* dummy = NULL;
    evalBindRest(params, args2, &dummy, varsBound);
  }

  else if (!isCons(params))
    ;

  else {
    evalBindParam(car(params), car(args2), varsBound);
    evalBindAll(cdr(params), cdr(args2), varsBound);
  }
  rmref(args2);
}

void evalBindRest(Cell* param, Cell* args, Cell** cachedVal, list<Cell*>& varsBound) {
  if (isCons(param))
    evalBindAll(param, args, varsBound);

  else {
    *cachedVal = evalAll(args);
    bindParams(param, *cachedVal, args, varsBound);
    rmref(*cachedVal);
  }
}

void evalBindParam(Cell* param, Cell* arg, list<Cell*>& varsBound) {
  Cell* arg2 = NULL;
  if (isQuoted(param)) {
    param = stripQuote(param);
    arg2 = mkref(newCons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  Cell* val = eval(arg2);
  bindParams(param, val, arg2, varsBound);
  rmref(val);
  rmref(arg2);
}

// NULL unevaldArgs => args are already quoted
void bindParams(Cell* params, Cell* args, Cell* unevaldArgs, list<Cell*>& varsBound) {
  if (isQuoted(params)) {
    if (unevaldArgs)
      bindParams(stripQuote(params), unevaldArgs, NULL, varsBound);
    else
      bindParams(stripQuote(params), args, NULL, varsBound);
  }

  else if (params == nil)
    ;

  else if (isSym(params)) {
    newDynamicScope(params, args);
    varsBound.push_back(params);
  }

  else if (!isCons(params))
    ;

  else if (args != nil && !isCons(args))
    bindParams(params, nil, nil, varsBound);

  else {
    bindParams(car(params), car(args), unevaldArgs && isCons(unevaldArgs) ? car(unevaldArgs) : unevaldArgs, varsBound);
    bindParams(cdr(params), cdr(args), unevaldArgs && isCons(unevaldArgs) ? cdr(unevaldArgs) : unevaldArgs, varsBound);
  }
}

Cell* evalAll(Cell* args) {
  if (!isCons(args))
    return eval(args);
  Cell* pResult = newCell(), *curr = pResult;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    Cell* val = eval(car(args));
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

// fn = (object function {sig => .., body => ..})
bool isFn(Cell* x) {
  return isCons(x) && type(x) == sym_function;
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

Cell* quote(Cell* x) {
  return newCons(sym_quote, x);
}

Cell* quoteAll(Cell* x) {
  Cell* result = newCell(), *curr = result;
  for (Cell* iter = x; iter != nil; iter=cdr(iter), curr=cdr(curr))
    addCons(curr, quote(car(iter)));
  return dropPtr(result);
}

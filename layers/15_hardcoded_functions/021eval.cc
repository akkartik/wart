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

  Cell* result = evalPrimitive(car(expr), cdr(expr));
  if (result) return result;  // already mkref'd

  // expr is a call
  Cell* fn = eval(car(expr));
  if (!isFn(fn))
    RAISE << "Not a call: " << expr << endl
        << "Perhaps you need to split the line in two." << endl;

  // eval its args, create new bindings
  list<Cell*> varsBound;
  evalBindAll(sig(fn), cdr(expr), varsBound);

  result = nil;
  // eval all forms in body, save result of final form
  for (Cell* form = body(fn); form != nil; form=cdr(form)) {
    rmref(result);
    result = eval(car(form));
  }

  for (list<Cell*>::iterator p = varsBound.begin(); p != varsBound.end(); ++p)
    endDynamicScope(*p);
  rmref(fn);
  return result;  // already mkref'd
}

Cell* evalPrimitive(Cell* f, Cell* args) {
  if (f == newSym("fn")) {
    Cell* f = newTable();
    set(f, sym_sig, car(args));
    set(f, sym_body, cdr(args));
    return mkref(f);
  }

  if (f == newSym("eval")) {
    Cell* arg = eval(car(args));
    Cell* result = eval(arg);
    rmref(arg);
    return result;  // already mkref'd
  }

  if (f == newSym("if")) {
    Cell* check = eval(car(args));
    Cell* result = (check != nil) ? eval(car(cdr(args))) : eval(car(cdr(cdr(args))));
    rmref(check);
    return result;  // already mkref'd
  }

  if (f == newSym("not")) {
    Cell* arg = eval(car(args));
    Cell* result = (arg == nil) ? newNum(1) : nil;
    rmref(arg);
    return mkref(result);
  }

  if (f == newSym("=")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    Cell* result = nil;
    if (x == nil && y == nil)
      result = mkref(newNum(1));
    else if (x == nil || y == nil)
      result = nil;
    else if (x == y)
      result = mkref(x);
    else if (x->type == FLOAT || y->type == FLOAT)
      result = (equalFloats(toFloat(x), toFloat(y)) ? mkref(x) : nil);
    else if (isString(x) && isString(y) && toString(x) == toString(y))
      result = mkref(x);
    else
      result = nil;
    rmref(x);
    rmref(y);
    return result;  // already mkref'd
  }

  if (f == newSym("<-")) {
    Cell* var = car(args);
    if (!isSym(var)) {
      RAISE << "can't assign to non-sym " << var << endl;
      return nil;
    }
    Cell* val = eval(car(cdr(args)));
    if (dynamics[var].empty())
      newDynamicScope(var, val);
    else
      assignDynamicVar(var, val);
    return val;   // already mkref'd
  }

  // lists
  if (f == newSym("cons")) {
    Cell* result = newCons(eval(car(args)), eval(car(cdr(args))));
    rmref(car(result));
    rmref(cdr(result));
    return mkref(result);
  }
  if (f == newSym("car")) {
    Cell* arg = eval(car(args));
    Cell* result = car(arg);
    rmref(arg);
    return mkref(result);
  }
  if (f == newSym("cdr")) {
    Cell* arg = eval(car(args));
    Cell* result = cdr(arg);
    rmref(arg);
    return mkref(result);
  }

  // numbers
  if (f == newSym("+")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    Cell* result = nil;
    if (x->type == FLOAT || y->type == FLOAT)
      result = newNum(toFloat(x) + toFloat(y));
    else
      result = newNum(toInt(x) + toInt(y));
    rmref(x);
    rmref(y);
    return mkref(result);
  }
  if (f == newSym("-")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    Cell* result = nil;
    if (x->type == FLOAT || y->type == FLOAT)
      result = newNum(toFloat(x) - toFloat(y));
    else
      result = newNum(toInt(x) - toInt(y));
    rmref(x);
    rmref(y);
    return mkref(result);
  }
  if (f == newSym("*")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    Cell* result = nil;
    if (x->type == FLOAT || y->type == FLOAT)
      result = newNum(toFloat(x) * toFloat(y));
    else
      result = newNum(toInt(x) * toInt(y));
    rmref(x);
    rmref(y);
    return mkref(result);
  }
  if (f == newSym("/")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    Cell* result = newNum(toFloat(x) / toFloat(y));
    rmref(x);
    rmref(y);
    return mkref(result);
  }
  if (f == newSym("%")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    Cell* result = newNum(toInt(x) % toInt(y));
    rmref(x);
    rmref(y);
    return mkref(result);
  }
  if (f == newSym("<")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    Cell* result = nil;
    if (x == nil || y == nil)
      ;
    else if (toFloat(x) < toFloat(y))
      result = mkref(y);  // guard against gc below
    rmref(x);
    rmref(y);
    return result;  // already mkref'd
  }
  if (f == newSym("int")) {
    Cell* arg = eval(car(args));
    Cell* result = newNum(toInt(arg));
    rmref(arg);
    return mkref(result);
  }
  return NULL;
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

// fn = {sig => .., body => ..}
//  both are optional
bool isFn(Cell* x) {
  return isTable(x);
}

Cell* sig(Cell* fn) {
  return get(fn, sym_sig);
}

Cell* body(Cell* fn) {
  return get(fn, sym_body);
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

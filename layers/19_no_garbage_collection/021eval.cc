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
    return expr;

  if (isSym(expr))
    return lookup(expr);

  if (isAtom(expr))
    return expr;

  if (isQuoted(expr))
    return cdr(expr);

  Cell* result = evalPrimitive(car(expr), cdr(expr));
  if (result) return result;

  // expr is a call
  Cell* fn = eval(car(expr));
  if (!isFn(fn))
    RAISE << "Not a call: " << expr << endl
        << "Perhaps you need to split the line in two." << endl;

  // eval its args, create new bindings
  evalBindAll(sig(fn), cdr(expr));

  // eval all forms in body, save result of final form
  for (Cell* form = body(fn); form != nil; form=cdr(form))
    result = eval(car(form));
  return result;
}

Cell* evalPrimitive(Cell* f, Cell* args) {
  if (f == newSym("fn")) {
    Cell* f = newTable();
    set(f, sym_sig, car(args));
    set(f, sym_body, cdr(args));
    return f;
  }

  if (f == newSym("eval")) {
    Cell* arg = eval(car(args));
    return eval(arg);
  }

  if (f == newSym("if")) {
    Cell* check = eval(car(args));
    return (check != nil) ? eval(car(cdr(args))) : eval(car(cdr(cdr(args))));
  }

  if (f == newSym("not")) {
    Cell* arg = eval(car(args));
    return (arg == nil) ? newNum(1) : nil;
  }

  if (f == newSym("=")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    if (x == nil && y == nil)
      return newNum(1);
    else if (x == nil || y == nil)
      return nil;
    else if (x == y)
      return x;
    else if (x->type == FLOAT || y->type == FLOAT)
      return equalFloats(toFloat(x), toFloat(y)) ? x : nil;
    else if (isString(x) && isString(y) && toString(x) == toString(y))
      return x;
    else
      return nil;
  }

  if (f == newSym("<-")) {
    Cell* var = car(args);
    if (!isSym(var)) {
      RAISE << "can't assign to non-sym " << var << endl;
      return nil;
    }
    Cell* val = eval(car(cdr(args)));
    newBinding(var, val);
    return val;
  }

  // lists
  if (f == newSym("cons")) {
    return newCons(eval(car(args)), eval(car(cdr(args))));
  }
  if (f == newSym("car")) {
    Cell* arg = eval(car(args));
    return car(arg);
  }
  if (f == newSym("cdr")) {
    Cell* arg = eval(car(args));
    return cdr(arg);
  }

  // numbers
  if (f == newSym("+")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    if (x->type == FLOAT || y->type == FLOAT)
      return newNum(toFloat(x) + toFloat(y));
    else
      return newNum(toInt(x) + toInt(y));
  }
  if (f == newSym("-")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    if (x->type == FLOAT || y->type == FLOAT)
      return newNum(toFloat(x) - toFloat(y));
    else
      return newNum(toInt(x) - toInt(y));
  }
  if (f == newSym("*")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    return nil;
    if (x->type == FLOAT || y->type == FLOAT)
      return newNum(toFloat(x) * toFloat(y));
    else
      return newNum(toInt(x) * toInt(y));
  }
  if (f == newSym("/")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    return newNum(toFloat(x) / toFloat(y));
  }
  if (f == newSym("%")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    return newNum(toInt(x) % toInt(y));
  }
  if (f == newSym("<")) {
    Cell* x = eval(car(args));
    Cell* y = eval(car(cdr(args)));
    if (x == nil || y == nil)
      return nil;
    else if (toFloat(x) < toFloat(y))
      return y;
    return nil;
  }
  if (f == newSym("int")) {
    Cell* arg = eval(car(args));
    return newNum(toInt(arg));
  }
  return NULL;
}

// bind params to args in newScope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
void evalBindAll(Cell* params, Cell* args) {
  if (params == nil)
    return ;

  if (isSym(params))
    bindParams(params, evalAll(args));

  else if (!isCons(params))
    ;

  else {
    bindParams(car(params), eval(car(args)));
    evalBindAll(cdr(params), cdr(args));
  }
}

void bindParams(Cell* params, Cell* args) {
  if (params == nil)
    ;

  else if (isSym(params))
    newBinding(params, args);

  else if (!isCons(params))
    ;

  else if (args != nil && !isCons(args))
    bindParams(params, nil);

  else {
    bindParams(car(params), car(args));
    bindParams(cdr(params), cdr(args));
  }
}

Cell* evalAll(Cell* args) {
  if (!isCons(args))
    return eval(args);
  Cell* pResult = newCell(), *curr = pResult;
  for (; args != nil; args=cdr(args), curr=cdr(curr))
    addCons(curr, eval(car(args)));
  return cdr(pResult);
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
  return cdr(result);
}

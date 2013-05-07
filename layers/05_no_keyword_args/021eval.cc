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
    return mkref(lookup(expr, scope, keepAlreadyEvald()));

  if (isAtom(expr))
    return mkref(expr);

  if (isObject(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  if (isBackQuoted(expr))
    return processUnquotes(cdr(expr), 1, scope);  // already mkref'd

  if (isAlreadyEvald(expr))
    return mkref(keepAlreadyEvald() ? expr : stripAlreadyEvald(expr));

  // expr is a call
  Cell* fn = toFn(eval(car(expr), scope));
  if (!isFn(fn))
    RAISE << "Not a call: " << expr << endl
        << "Perhaps you need to split the line in two." << endl;

  // eval its args in the caller's lexical environment
  Cell* splicedArgs = spliceArgs(cdr(expr), scope, fn);
  Cell* newScope = newTable();
  evalBindAll(sig(fn), splicedArgs, scope, newScope);

  // swap in the function's lexical environment
  newDynamicScope(CURR_LEXICAL_SCOPE, isCompiledFn(body(fn)) ? scope : env(fn));
  addLexicalScope(newScope);
  addLexicalBinding(sym_caller_scope, scope);

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
  rmref(splicedArgs);
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

  Cell* val = evalArg(arg2, scope);
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

//// eval args - while respecting alreadyEvald

Cell* evalAll(Cell* args, Cell* scope) {
  if (!isCons(args))
    return evalArg(args, scope);
  Cell* pResult = newCell(), *curr = pResult;
  for (; args != nil; args=cdr(args), curr=cdr(curr)) {
    Cell* val = evalArg(car(args), scope);
    addCons(curr, val);
    rmref(val);
  }
  return dropPtr(pResult);
}

// eval, but always strip '' regardless of keepAlreadyEvald()
Cell* evalArg(Cell* arg, Cell* scope) {
  if (isAlreadyEvald(arg)) return mkref(stripAlreadyEvald(arg));
  return eval(arg, scope);
}



//// eval @exprs and inline them into args
// tag them with '' (already eval'd) so they can be used with macros

Cell* spliceArgs(Cell* args, Cell* scope, Cell* fn) {
  Cell *pResult = newCell(), *tip = pResult;
  for (Cell* curr = args; curr != nil; curr=cdr(curr)) {
    if (!isSpliced(car(curr))) {
      addCons(tip, car(curr));
      tip=cdr(tip);
      continue;
    }

    if (isMacro(fn) && !contains(body(fn), sym_backquote))
      RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)" << endl;
    Cell* x = unsplice(car(curr), scope);
    for (Cell* curr2 = x; curr2 != nil; curr2=cdr(curr2), tip=cdr(tip))
      addCons(tip, tagAlreadyEvald(car(curr2)));
    rmref(x);
  }
  return dropPtr(pResult);
}

Cell* unsplice(Cell* arg, Cell* scope) {
  return eval(cdr(arg), scope);
}

// supporting @ in macro calls
stack<bool> inMacro;

// keep sync'd with mac
bool isMacro(Cell* fn) {
  if (!isObject(fn)) return false;
  if (!isQuoted(sig(fn))) return false;
  Cell* forms = body(fn);
  if (cdr(forms) != nil) return false;
  Cell* form = car(forms);
  if (car(form) != sym_eval) return false;
  if (car(cdr(cdr(form))) != sym_caller_scope) return false;
  if (cdr(cdr(cdr(form))) != nil) return false;
  return true;
}

bool keepAlreadyEvald() {
  if (inMacro.empty()) inMacro.push(false);
  return inMacro.top();
}

Cell* tagAlreadyEvald(Cell* cell) {
  if (isColonSym(cell)) return cell;
  return newCons(sym_alreadyEvald, cell);
}

bool isAlreadyEvald(Cell* cell) {
  return isCons(cell) && car(cell) == sym_alreadyEvald;
}

Cell* stripAlreadyEvald(Cell* cell) {
  while (isAlreadyEvald(cell))
    cell = cdr(cell);
  return cell;
}



//// backquoted exprs

// when inMacro did we encounter ''?
bool skippedAlreadyEvald = false;

Cell* processUnquotes(Cell* x, long depth, Cell* scope) {
  if (!isCons(x)) return mkref(x);

  if (unquoteDepth(x) == depth) {
    skippedAlreadyEvald = false;
    Cell* result = eval(stripUnquote(x), scope);
    return skippedAlreadyEvald ? pushCons(sym_alreadyEvald, result) : result;
  }
  else if (unquoteSpliceDepth(car(x)) == depth) {
    Cell* result = eval(stripUnquoteSplice(car(x)), scope);
    Cell* splice = processUnquotes(cdr(x), depth, scope);
    if (result == nil) return splice;

    // always splice in a copy
    Cell* resultcopy = copyList(result);
    rmref(result);
    append(resultcopy, splice);
    rmref(splice);
    return mkref(resultcopy);
  }
  else if (unquoteDepth(x) > 0) {
    return mkref(x);
  }

  if (isBackQuoted(x)) {
    Cell* result = newCons(car(x), processUnquotes(cdr(x), depth+1, scope));
    rmref(cdr(result));
    return mkref(result);
  }

  Cell* result = newCons(processUnquotes(car(x), depth, scope),
                         processUnquotes(cdr(x), depth, scope));
  rmref(car(result));
  rmref(cdr(result));
  return mkref(result);
}

Cell* maybeStripAlreadyEvald(bool keepAlreadyEvald, Cell* x) {
  skippedAlreadyEvald = isAlreadyEvald(x);
  return keepAlreadyEvald ? x : stripAlreadyEvald(x);
}

long unquoteDepth(Cell* x) {
  if (isUnquoted(x))
    return unquoteDepth(cdr(x))+1;
  return 0;
}

Cell* stripUnquote(Cell* x) {
  if (isUnquoted(x))
    return stripUnquote(cdr(x));
  return x;
}

long unquoteSpliceDepth(Cell* x) {
  if (isUnquoteSpliced(x))
    return 1;
  if (isUnquoted(x))
    return unquoteSpliceDepth(cdr(x))+1;
  return 1000;  // never try to splice
}

Cell* stripUnquoteSplice(Cell* x) {
  return cdr(stripUnquote(x));
}



//// helpers

bool isQuoted(Cell* cell) {
  return isCons(cell) && car(cell) == sym_quote;
}

bool isBackQuoted(Cell* cell) {
  return isCons(cell) && car(cell) == sym_backquote;
}

Cell* stripQuote(Cell* cell) {
  return isQuoted(cell) ? cdr(cell) : cell;
}

bool isUnquoted(Cell* arg) {
  return isCons(arg) && car(arg) == sym_unquote;
}

bool isSpliced(Cell* arg) {
  return isCons(arg) && car(arg) == sym_splice;
}

bool isUnquoteSpliced(Cell* arg) {
  return isCons(arg) && car(arg) == sym_unquoteSplice;
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

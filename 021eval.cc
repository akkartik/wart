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
//  functions can reorder args using keyords: ((fn (a b c) b) :b 3 1 2) => 3
//  functions can document keyword args with a different name: ((fn (a b|returning c) b) :returning 3 1 2) => 3
//  callers can pass in keyword args to nested functions
//  callers can pass in just needed args: ((fn (a b c) (list a b)) :b 3) => (nil 3)
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
  Cell* evaldArgs = processArgs(expr, scope, fn);
  // swap in the function's lexical environment
  newDynamicScope(CURR_LEXICAL_SCOPE, isCompiledFn(body(fn)) ? scope : env(fn));
  newLexicalScope();
  bindParams(sig(fn), evaldArgs);
  addLexicalBinding(sym_caller_scope, scope);

  Cell* result = nil;
  if (isCompiledFn(body(fn)))
    result = toCompiledFn(body(fn))();  // all compiledFns must mkref result
  else
    // eval all forms in body, save result of final form
    for (Cell* form = impl(fn); form != nil; form=cdr(form)) {
      rmref(result);
      result = eval(car(form), currLexicalScope);
    }

  endLexicalScope();
  endDynamicScope(CURR_LEXICAL_SCOPE);
  rmref(evaldArgs);
  rmref(fn);
  return result;  // already mkref'd
}

Cell* processArgs(Cell* call, Cell* scope, Cell* fn) {
  Cell* splicedArgs = spliceArgs(cdr(call), scope, fn);
  Cell* orderedArgs = reorderKeywordArgs(splicedArgs, sig(fn));   rmref(splicedArgs);
  Cell* evaldArgs = evalArgs(orderedArgs, sig(fn), scope);  rmref(orderedArgs);
  dbg << car(call) << "/" << keepAlreadyEvald() << ": " << evaldArgs << endl;
  return evaldArgs;   // already mkref'd
}



// bind params to appropriate args -- including param aliases

void bindParams(Cell* params, Cell* args) {
  params = stripQuote(params);
  if (params == nil) return;

  if (isCons(params) && car(params) == sym_param_alias) {
    bindParamAliases(cdr(params), args);
    return;
  }

  Cell* orderedArgs = reorderKeywordArgs(args, params);
  if (isSym(params)) {
    addLexicalBinding(params, orderedArgs);
  }
  else {
    bindParams(car(params), car(orderedArgs));
    bindParams(cdr(params), cdr(orderedArgs));
  }
  rmref(orderedArgs);
}

void bindParamAliases(Cell* aliases, Cell* arg) {
  for (; aliases != nil; aliases=cdr(aliases))
    bindParams(car(aliases), arg);
}



// process :keyword args and reorder args to param order -- respecting param aliases

Cell* reorderKeywordArgs(Cell* args, Cell* params) {
  if (!isCons(stripQuote(params))) return mkref(args);

  CellMap keywordArgs;
  Cell* nonKeywordArgs = extractKeywordArgs(params, args, keywordArgs);
  Cell* result = argsInParamOrder(params, nonKeywordArgs, keywordArgs);   rmref(nonKeywordArgs);
  return result;  // already mkref'd
}

// extract keyword args into the CellMap provided; return non-keyword args
Cell* extractKeywordArgs(Cell* params, Cell* args, CellMap& keywordArgs) {
  Cell *pNonKeywordArgs = newCell(), *curr = pNonKeywordArgs;
  for (; isCons(args); args=cdr(args)) {
    Cell* keywordParam = keywordArg(car(args), params);
    if (keywordParam == nil) {
      addCons(curr, car(args));
      curr=cdr(curr);
    }
    // keyword arg for rest param alias
    else if (isCons(keywordParam) && cdr(keywordParam) == nil
             && isCons(car(keywordParam)) && car(car(keywordParam)) == sym_param_alias) {
      args = cdr(args);   // skip keyword arg
      for (Cell* p = cdr(car(keywordParam)); p != nil; p=cdr(p))
        keywordArgs[car(p)] = args;
      rmref(keywordParam);
      args = nil;
    }
    // keyword arg for param alias
    else if (isCons(keywordParam) && (car(keywordParam) == sym_param_alias)) {
      args = cdr(args);   // skip keyword arg
      for (Cell* p = cdr(keywordParam); p != nil; p=cdr(p))
        keywordArgs[car(p)] = car(args);
    }
    // simple rest keyword arg
    else if (isCons(keywordParam)) {   // rest keyword arg
      keywordArgs[car(keywordParam)] = cdr(args);  // ..must be final keyword arg
      rmref(keywordParam);
      args = nil;
    }
    // simple keyword arg
    else {
      args = cdr(args);   // skip keyword arg
      keywordArgs[keywordParam] = car(args);
    }
  }
  if (!isCons(args))  // improper list
    setCdr(curr, args);
  return dropPtr(pNonKeywordArgs);
}

Cell* argsInParamOrder(Cell* params, Cell* nonKeywordArgs, CellMap& keywordArgs) {
  Cell *pReconstitutedArgs = newCell(), *curr = pReconstitutedArgs;
  for (params=stripQuote(params); params != nil; curr=cdr(curr), params=stripQuote(cdr(params))) {
    if (!isCons(params)) {
      setCdr(curr, keywordArgs[params] ? keywordArgs[params] : nonKeywordArgs);
      break;
    }

    if (isCons(params) && car(params) == sym_param_alias) {
      if (keywordArgs[car(cdr(params))]) {
        setCdr(curr, keywordArgs[car(cdr(params))]);
        break;
      }
      else {
        setCdr(curr, nonKeywordArgs);
        break;
      }
    }

    Cell* param = stripQuote(car(params));
    if (isCons(param) && car(param) == sym_param_alias)
      param = car(cdr(param));

    if (keywordArgs[param]) {
      addCons(curr, keywordArgs[param]);
    }
    else {
      addCons(curr, car(nonKeywordArgs));
      nonKeywordArgs = cdr(nonKeywordArgs);
    }
  }
  return dropPtr(pReconstitutedArgs);
}

// return the appropriate param if arg is a valid keyword arg
// handle param aliases; :a => (| a b)
// respond to rest keyword args with (rest-param)
// combining the two: respond to rest param aliases with ((| do body))
// doesn't look inside destructured params
Cell* keywordArg(Cell* arg, Cell* params) {
  if (!isColonSym(arg)) return nil;
  Cell* candidate = newSym(toString(arg).substr(1));
  for (params=stripQuote(params); params != nil; params=stripQuote(cdr(params))) {
    if (!isCons(params)) { // rest param
      if (params == candidate)
        return newCons(candidate);
    }
    else if (car(params) == sym_param_alias) { // rest param aliases
      if (paramAliasMatch(cdr(params), candidate))
        return newCons(params);
    }
    // ignore destructuring except param aliases
    else if (isCons(stripQuote(car(params)))
             && car(stripQuote(car(params))) == sym_param_alias) {
      if (paramAliasMatch(cdr(stripQuote(car(params))), candidate))
        return stripQuote(car(params));
    }
    else if (stripQuote(car(params)) == candidate) {
      return candidate;
    }
  }
  return nil;
}

bool paramAliasMatch(Cell* aliases, Cell* candidate) {
  for (; aliases != nil; aliases=cdr(aliases)) {
    if (car(aliases) == candidate)
      return true;
  }
  return false;
}



// eval args as necessary depending on corresponding params

Cell* evalArgs(Cell* args, Cell* params, Cell* scope) {
  if (args == nil) return nil;
  if (isQuoted(params)) return mkref(args);

  Cell* result = newCell();
  setCar(result, evalArg(car(args), params, scope));  rmref(car(result));
  setCdr(result, evalArgs(cdr(args), cdr(params), scope));  rmref(cdr(result));
  return mkref(result);
}

Cell* evalArg(Cell* arg, Cell* params, Cell* scope) {
  if (isAlreadyEvald(arg)) return mkref(stripAlreadyEvald(arg));
  if (isCons(params) && isQuoted(car(params))) return mkref(arg);
  return eval(arg, scope);
}



// eval @exprs and inline them into args
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
  if (car(form) != sym_mac_eval) return false;
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



// backquoted exprs

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



// misc helpers

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

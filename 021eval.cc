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
//  support symbolicEval mode as a primitive for optimizations
//    symbolicEvalArgs returns bindings for a call without actually evaluating args

Cell* eval(Cell* expr) {
  return eval(expr, currLexicalScope);
}

bool doLog = false;
stack<int> log_levels;
void new_level() {
  if (log_levels.empty()) log_levels.push(0);   // initialization
  if (doLog)
    log_levels.push(log_levels.top()+1);
}

void end_level() {
  if (doLog)
    log_levels.pop();
}

Cell* eval(Cell* expr, Cell* scope) {
  if (!doLog) return eval2(expr, scope);
  new_level();
  log() << expr << endl;
  Cell* result = eval2(expr, scope);
  log() << " &rArr; " << result << endl;
  end_level();
  return result;
}

ofstream& log() {
  return log(log_levels.top());
}

ofstream& log(int n) {
  static ofstream log("log");
  log << "</div><div class='level";
  if (n > 1) log << " hidden";
  log << "' level_index='" << n << "'>";
  return log;
}

COMPILE_FN(trace, compiledFn_trace, "'($form)",
  doLog = true;
  Cell* result = eval(lookup("$form"), currLexicalScope);
  doLog = false;
  return result;
)

long evalCount = 0;

Cell* eval2(Cell* expr, Cell* scope) {
  ++evalCount;
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

  if (isIncompleteEval(expr))
    return eval(rep(expr), scope);

  if (isObject(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  if (isBackQuoted(expr))
    return processUnquotes(cdr(expr), 1, scope);  // already mkref'd

  if (isAlreadyEvald(expr))
    return mkref(keepAlreadyEvald() ? expr : stripAlreadyEvald(expr));

  if (doLog) log(log_levels.top()+1) << "eval'ing args\n";

  // expr is a call
  Cell* fn = toFn(eval(car(expr), scope));
  if (isIncompleteEval(fn)) {
    Cell* result = newObject("incomplete_eval", newCons(rep(fn), cdr(expr)));
    rmref(fn);
    return mkref(result);
  }
  if (!isFn(fn))
    RAISE << "Not a call: " << expr << endl
        << "Perhaps you need to split the line in two." << endl;

  // eval its args in the caller's lexical environment
  Cell* splicedArgs = spliceArgs(cdr(expr), scope, fn);
  Cell* orderedArgs = reorderKeywordArgs(splicedArgs, sig(fn));
  Cell* newScope = newTable();
  evalBindAll(sig(fn), orderedArgs, scope, newScope);

  if (car(expr) != newSym("speculatively")
      && anyIncompleteEval(newScope)) {
    Cell* result = rippleIncompleteEval(fn, newScope);
    rmref(newScope);
    rmref(orderedArgs);
    rmref(splicedArgs);
    rmref(fn);
    return mkref(result);
  }

  // swap in the function's lexical environment
  newDynamicScope(CURR_LEXICAL_SCOPE, isCompiledFn(body(fn)) ? scope : env(fn));
  addLexicalScope(newScope);
  addLexicalBinding(sym_caller_scope, scope);

  Cell* result = nil;
  if (isCompiledFn(body(fn))) {
    if (doLog) log(log_levels.top()+1) << "executing primitive " << car(expr) << endl;
    result = toCompiledFn(body(fn))();  // all compiledFns must mkref result
  }
  else {
    if (doLog) log(log_levels.top()+1) << "eval'ing body\n";
    // eval all forms in body, save result of final form
    for (Cell* form = impl(fn); form != nil; form=cdr(form)) {
      rmref(result);
      result = eval(car(form), currLexicalScope);
    }
  }

  endLexicalScope();  // implicitly rmrefs newScope
  endDynamicScope(CURR_LEXICAL_SCOPE);
  rmref(orderedArgs);
  rmref(splicedArgs);
  rmref(fn);
  return result;  // already mkref'd
}

// bind params to args in newScope, taking into account:
//  quoted params (eval'ing args as necessary; args is never quoted, though)
//  destructured params
//  aliased params
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
    Cell* dummy = NULL;
    evalBindRest(params, args2, &dummy, scope, newScope);
  }

  else if (!isCons(params))
    ;

  else if (isAlias(params))
    evalBindRestAliases(params, args2, scope, newScope);

  else {
    evalBindParam(car(params), car(args2), scope, newScope);
    evalBindAll(cdr(params), cdr(args2), scope, newScope);
  }
  rmref(args2);
}

void evalBindRest(Cell* param, Cell* args, Cell** cachedVal, Cell* scope, Cell* newScope) {
  if (isCons(param))
    evalBindAll(param, args, scope, newScope);

  else {
    *cachedVal = evalAll(args, scope);
    bindParams(param, *cachedVal, args, newScope);
    rmref(*cachedVal);
  }
}

void evalBindParam(Cell* param, Cell* arg, Cell* scope, Cell* newScope) {
  Cell* arg2 = NULL;
  if (isQuoted(param)) {
    param = stripQuote(param);
    arg2 = mkref(newCons(sym_quote, arg));
  }
  else
    arg2 = mkref(arg);

  if (isAlias(param))
    evalBindAliases(param, arg2, scope, newScope);

  else {
    Cell* val = evalArg(arg2, scope);
    if (isIncompleteEval(val) && isCons(param))
      addLexicalBinding(param, val, newScope);
    else
      bindParams(param, val, arg2, newScope);
    rmref(val);
  }
  rmref(arg2);
}

void evalBindRestAliases(Cell* params /* (| ...) */, Cell* args, Cell* scope, Cell* newScope) {
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  Cell* cachedVal = NULL;   // to ensure we don't multiply-eval
  for (Cell* aliases = cdr(params); aliases != nil; aliases=cdr(aliases)) {
    if (cachedVal)
      bindParams(car(aliases), cachedVal, args, newScope);
    else
      evalBindRest(car(aliases), args, &cachedVal, scope, newScope);
  }
}

void evalBindAliases(Cell* params /* (| ...) */, Cell* arg, Cell* scope, Cell* newScope) {
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  Cell* cachedVal = NULL;   // to ensure we don't multiply-eval
  for (Cell *aliases=cdr(params), *alias=car(aliases); aliases != nil; aliases=cdr(aliases),alias=car(aliases)) {
    if (isQuoted(alias))
      bindParams(alias, arg, NULL, newScope);
    else if (cachedVal)
      bindParams(alias, cachedVal, arg, newScope);
    else if (isAlias(alias))
      evalBindAliases(alias, arg, scope, newScope);
    else {
      cachedVal = evalArg(arg, scope);
      bindParams(alias, cachedVal, arg, newScope);
      rmref(cachedVal);
    }
  }
}

// NULL unevaldArgs => args are already quoted
void bindParams(Cell* params, Cell* args, Cell* unevaldArgs, Cell* newScope) {
  if (isQuoted(params)) {
    if (unevaldArgs)
      bindParams(stripQuote(params), unevaldArgs, NULL, newScope);
    else
      bindParams(stripQuote(params), args, NULL, newScope);
  }

  else if (params == nil)
    ;

  else if (isSym(params))
    addLexicalBinding(params, args, newScope);

  else if (!isCons(params))
    ;

  else if (!isAlias(params) && args != nil && !isCons(args))
    bindParams(params, nil, nil, newScope);

  else if (isAlias(params))
    bindAliases(params, args, unevaldArgs, newScope);

  else {
    Cell* orderedArgs = reorderKeywordArgs(args, params);
    bindParams(car(params), car(orderedArgs), unevaldArgs && isCons(unevaldArgs) ? car(unevaldArgs) : unevaldArgs, newScope);
    bindParams(cdr(params), cdr(orderedArgs), unevaldArgs && isCons(unevaldArgs) ? cdr(unevaldArgs) : unevaldArgs, newScope);
    rmref(orderedArgs);
  }
}

void bindAliases(Cell* params /* (| ...) */, Cell* arg, Cell* unevaldArg, Cell* newScope) {
  if (len(params) <= 2)
    RAISE << "just one param alias: " << params << ". Are you sure?\n";
  for (Cell* aliases=cdr(params); aliases != nil; aliases=cdr(aliases))
    if (!unsafeGet(newScope, car(aliases))) // skip duplicate destructured aliases
      bindParams(car(aliases), arg, unevaldArg, newScope);
}

//// eval args - while respecting alreadyEvald and symbolicEval

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

stack<bool> symbolicEval;

// eval, but always strip '' regardless of keepAlreadyEvald()
Cell* evalArg(Cell* arg, Cell* scope) {
  if (isAlreadyEvald(arg)) return mkref(stripAlreadyEvald(arg));
  if (symbolicEval.empty()) symbolicEval.push(false);
  if (symbolicEval.top()) return mkref(arg);
  return eval(arg, scope);
}

COMPILE_FN(symbolicEvalArgs, compiledFn_symbolicEvalArgs, "($expr)",
  Cell* expr = lookup("$expr");
  Cell* fn = car(expr);
  if (!isFn(fn)) {
    RAISE << "Not a call: " << expr << endl;
    return nil;
  }
  Cell* splicedArgs = spliceArgs(cdr(expr), currLexicalScope, fn);
  Cell* orderedArgs = reorderKeywordArgs(splicedArgs, sig(fn));
  symbolicEval.push(true);
    Cell* bindings = mkref(newTable());
    evalBindAll(sig(fn), orderedArgs, currLexicalScope, bindings);
  symbolicEval.pop();
  rmref(orderedArgs);
  rmref(splicedArgs);
  return bindings;
)



//// process :keyword args and reorder args to param order -- respecting param aliases

Cell* reorderKeywordArgs(Cell* args, Cell* params) {
  if (!isCons(stripQuote(params))) return mkref(args);

  CellMap keywordArgs;  // all values will be refcounted.
  Cell* nonKeywordArgs = extractKeywordArgs(params, args, keywordArgs);
  Cell* result = argsInParamOrder(params, nonKeywordArgs, keywordArgs);   rmref(nonKeywordArgs);

  for (CellMap::iterator p = keywordArgs.begin(); p != keywordArgs.end(); ++p)
    if (p->second) rmref(p->second);
  return result;  // already mkref'd
}

// extract keyword args into the CellMap provided; return non-keyword args
// always mkref what you insert into the CellMap
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
             && isAlias(car(keywordParam))) {
      args = cdr(args);   // skip keyword arg
      Cell* endRest = nextKeyword(args, params);
      Cell* restArgs = snip(args, endRest);
      for (Cell* p = cdr(car(keywordParam)); p != nil; p=cdr(p))
        keywordArgs[car(p)] = mkref(restArgs);
      rmref(restArgs);
      rmref(keywordParam);
      args = endRest;
    }
    // keyword arg for param alias
    else if (isAlias(keywordParam)) {
      args = cdr(args);   // skip keyword arg
      for (Cell* p = cdr(keywordParam); p != nil; p=cdr(p))
        keywordArgs[car(p)] = mkref(car(args));
    }
    // simple rest keyword arg
    else if (isCons(keywordParam)) {   // rest keyword arg
      args = cdr(args);
      Cell* endRest = nextKeyword(args, params);
      keywordArgs[car(keywordParam)] = snip(args, endRest);  // already mkref'd
      rmref(keywordParam);
      args = endRest;
    }
    // simple keyword arg
    else {
      args = cdr(args);   // skip keyword arg
      keywordArgs[keywordParam] = mkref(car(args));
    }
  }
  if (!isCons(args))  // improper list
    setCdr(curr, args);
  return dropPtr(pNonKeywordArgs);
}

Cell* nextKeyword(Cell* args, Cell* params) {
  for (args=cdr(args); args != nil; args=cdr(args)) {
    if (keywordArg(car(args), params) != nil)
      return args;
  }
  return nil;
}

Cell* snip(Cell* x, Cell* next) {
  if (next == nil) return mkref(x);
  Cell* pResult = newCell();
  for (Cell* curr = pResult; x != next; x=cdr(x),curr=cdr(curr))
    addCons(curr, car(x));
  return dropPtr(pResult);
}

Cell* argsInParamOrder(Cell* params, Cell* nonKeywordArgs, CellMap& keywordArgs) {
  Cell *pReconstitutedArgs = newCell(), *curr = pReconstitutedArgs;
  for (params=stripQuote(params); params != nil; curr=cdr(curr), params=stripQuote(cdr(params))) {
    if (!isCons(params)) {
      setCdr(curr, keywordArgs[params] ? keywordArgs[params] : nonKeywordArgs);
      break;
    }

    if (isAlias(params)) {
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
    if (isAlias(param))
      param = car(cdr(param));

    if (keywordArgs[param]) {
      addCons(curr, keywordArgs[param]);
    }
    else {
      addCons(curr, car(nonKeywordArgs));
      nonKeywordArgs = cdr(nonKeywordArgs);
    }
  }
  if (nonKeywordArgs != nil)
    setCdr(curr, nonKeywordArgs);   // any remaining args
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
    else if (isAlias(params)) {   // rest param aliases
      if (paramAliasMatch(cdr(params), candidate))
        return newCons(params);
    }
    // ignore destructuring except param aliases
    else if (isAlias(stripQuote(car(params)))) {
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
    if (isIncompleteEval(x))
      addCons(tip, newCons(sym_splice, rep(x)));
    else
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



//// support for partial-eval

bool isIncompleteEval(Cell* x) {
  return type(x) == sym_incomplete_eval;
}

bool anyIncompleteEval(Cell* scope) {
  CellMap table = toTable(scope)->table;
  for (CellMap::iterator p = table.begin(); p != table.end(); ++p)
    if (p->second && isIncompleteEval(p->second))
      return true;
  return false;
}

Cell* rippleIncompleteEval(Cell* f, Cell* scope) {
  Cell *args=newCell(), *curr=args;
  CellMap table = toTable(scope)->table;
  for (Cell* params = stripQuote(sig(f)); params != nil && (!isCons(params) || !isObject(params)); params=cdr(params), curr=cdr(curr)) {
    Cell* param = car(params);
    if (isQuoted(param))
      param = stripQuote(param);
    if (isAlias(param))
      param = car(cdr(param));
    if (!table[param]) continue;
    if (isIncompleteEval(table[param]))
      addCons(curr, rep(table[param]));
    else
      addCons(curr, tagAlreadyEvald(table[param]));
  }
  Cell* result = newCons(f, dropPtr(args));
  rmref(cdr(result));
  result = newObject("incomplete_eval", result);
  return result;
}

COMPILE_FN(speculatively, compiledFn_speculatively, "($x)",
  Cell* x = lookup("$x");
  if (!isIncompleteEval(x)) return mkref(x);
  return mkref(newObject("incomplete_eval_data", rep(x)));
)



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
  if (isIncompleteEval(x)) return x;
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

bool isAlias(Cell* l) {
  return isCons(l) && car(l) == sym_param_alias;
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

//// eval: lookup symbols, respect quotes, rewrite fn calls

                                  bool isQuoted(Cell* cell) {
                                    return isCons(cell) && car(cell) == newSym("'");
                                  }

                                  bool isAlreadyEvald(Cell* cell) {
                                    return isCons(cell) && car(cell) == newSym("''");
                                  }

                                  bool isBackQuoted(Cell* cell) {
                                    return isCons(cell) && car(cell) == newSym("`");
                                  }

                                  bool isSplice(Cell* arg) {
                                    return isCons(arg) && car(arg) == newSym("@");
                                  }

                                  bool isUnquoteSplice(Cell* arg) {
                                    return isCons(arg) && car(arg) == newSym(",@");
                                  }

                                  bool isColonSym(Cell* x) {
                                    return isSym(x) && toString(x)[0] == L':';
                                  }

                                  // callee = (object function {sig, body, env, ..})
                                  Cell* calleeSig(Cell* callee) {
                                    return get(rep(callee), newSym("sig"));
                                  }

                                  Cell* calleeBody(Cell* callee) {
                                    return get(rep(callee), newSym("body"));
                                  }

                                  Cell* calleeImpl(Cell* callee) {
                                    Cell* impl = get(rep(callee), newSym("optimized-body"));
                                    return (impl != nil) ? impl : calleeBody(callee);
                                  }

                                  Cell* calleeEnv(Cell* callee) {
                                    return get(rep(callee), newSym("env"));
                                  }

                                  Cell* callArgs(Cell* call) {
                                    return cdr(call);
                                  }



                                  Cell* unsplice(Cell* arg, Cell* scope, bool dontStripAlreadyEval) {
                                    return eval(cdr(arg), scope, dontStripAlreadyEval);
                                  }

                                  // keep sync'd with mac
                                  bool isMacro(Cell* fn) {
                                    if (!isObject(fn)) return false;
                                    if (!isQuoted(calleeSig(fn))) return false;
                                    Cell* body = calleeBody(fn);
                                    if (cdr(body) != nil) return false;
                                    Cell* form = car(body);
                                    if (car(form) != newSym("eval")) return false;
                                    if (car(cdr(cdr(form))) != newSym("caller-scope")) return false;
                                    if (cdr(cdr(cdr(form))) != nil) return false;
                                    return true;
                                  }

                                  bool isMacroWithoutBackquotes(Cell* fn) {
                                    if (!isMacro(fn)) return false;
                                    return !contains(calleeBody(fn), newSym("`"));
                                  }

// eval @exprs and inline them into args, tagging them with '' (already eval'd)
Cell* spliceArgs(Cell* args, Cell* scope, Cell* fn, bool dontStripAlreadyEval) {
  Cell *pResult = newCell(), *tip = pResult;
  for (Cell* curr = args; curr != nil; curr=cdr(curr)) {
    if (isSplice(car(curr))) {
      if (isMacroWithoutBackquotes(fn))
        RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)" << endl;
      Cell* x = unsplice(car(curr), scope, dontStripAlreadyEval);
      for (Cell* curr2 = x; curr2 != nil; curr2=cdr(curr2), tip=cdr(tip))
        if (isColonSym(car(curr2)))
          addCons(tip, car(curr2));
        else
          addCons(tip, newCons(newSym("''"), car(curr2)));
      rmref(x);
    }
    else {
      addCons(tip, car(curr));
      tip=cdr(tip);
    }
  }
  return dropPtr(pResult);
}

Cell* spliceArgs(Cell* args, Cell* scope, Cell* fn) {
  return spliceArgs(args, scope, fn, false);
}

                                  Cell* stripQuote(Cell* cell) {
                                    return isQuoted(cell) ? cdr(cell) : cell;
                                  }

                                  bool paramAliasMatch(Cell* arg, Cell* param) {
                                    if (arg == param) return true;
                                    if (!isSym(arg)) return false;

                                    string name = toString(param);
                                    if (name.find(L'/') == string::npos || name.find(L'/') == 0)
                                      return false;

                                    string expected = toString(arg);
                                    char ns[name.length()+1];
                                    strcpy(ns, name.c_str());
                                    for (char *tok = strtok(ns, "/"); tok; tok=strtok(NULL, "/"))
                                      if (expected == tok) return true;
                                    return false;
                                  }

                                  // doesn't look inside destructured params
                                  Cell* keywordArg(Cell* arg, Cell* params) {
                                    if (!isColonSym(arg)) return nil;
                                    Cell* realArg = newSym(toString(arg).substr(1));
                                    for (; params != nil; params=cdr(params)) {
                                      if (realArg == params) // rest keyword arg must be last
                                        return newSym("__wartRestKeywordArg");
                                      if (realArg == newSym("do") && params == newSym("body"))
                                        return newSym("__wartRestKeywordArg");
                                      if (!isCons(params)) continue;
                                      Cell* param = stripQuote(car(params));
                                      if (paramAliasMatch(realArg, param))
                                        return param;
                                    }
                                    return nil;
                                  }

                                  // extract keyword args into the CellMap provided; return non-keyword args
                                  Cell* extractKeywordArgs(Cell* params, Cell* args, CellMap& keywordArgs) {
                                    Cell *pNonKeywordArgs = newCell(), *curr = pNonKeywordArgs;
                                    for (; isCons(args); args=cdr(args)) {
                                      Cell* currArg = keywordArg(car(args), params);
                                      if (currArg == newSym("__wartRestKeywordArg")) {
                                        setCdr(curr, cdr(args));
                                        break;
                                      }
                                      else if (currArg == nil) {
                                        addCons(curr, car(args));
                                        curr=cdr(curr);
                                      }
                                      else {
                                        args = cdr(args); // skip keyword arg
                                        keywordArgs[currArg] = car(args);
                                      }
                                    }
                                    if (!isCons(args)) // improper list
                                      setCdr(curr, args);
                                    return dropPtr(pNonKeywordArgs);
                                  }

                                  Cell* argsInParamOrder(Cell* params, Cell* nonKeywordArgs, CellMap& keywordArgs) {
                                    Cell* pReconstitutedArgs = newCell();
                                    params = stripQuote(params);
                                    for (Cell* curr = pReconstitutedArgs; params != nil; curr=cdr(curr), params=cdr(params)) {
                                      if (!isCons(params)) {
                                        setCdr(curr, keywordArgs[params] ? keywordArgs[params] : nonKeywordArgs);
                                        break;
                                      }

                                      Cell* param = stripQuote(car(params));
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

Cell* reorderKeywordArgs(Cell* params, Cell* args) {
  if (!isCons(params)) return mkref(args);
  if (isQuoted(params) && !isCons(cdr(params))) return mkref(args);
  CellMap keywordArgs;
  Cell* nonKeywordArgs = extractKeywordArgs(params, args, keywordArgs);
  Cell* result = argsInParamOrder(params, nonKeywordArgs, keywordArgs);
  rmref(nonKeywordArgs);
  return result; // already mkref'd
}



Cell* evalArgs(Cell* params, Cell* args, Cell* scope, bool dontStripAlreadyEval) {
  if (args == nil) return nil;

  if (isQuoted(params))
    return mkref(args);

  Cell* result = newCell();
  setCdr(result, evalArgs(cdr(params), cdr(args), scope, dontStripAlreadyEval));
  rmref(cdr(result));

  if (isAlreadyEvald(car(args)))
    setCar(result, cdr(car(args)));
  else if (isCons(params) && isQuoted(car(params)))
    setCar(result, car(args));
  else {
    setCar(result, eval(car(args), scope, dontStripAlreadyEval));
    rmref(car(result));
  }
  return mkref(result);
}

Cell* evalArgs(Cell* params, Cell* args, Cell* scope) {
  return evalArgs(params, args, scope, false);
}



                                  // split param sym at '/' and bind all resulting syms to val
                                  void bindParamAliases(Cell* param, Cell* val) {
                                    string name = toString(param);
                                    if (name.find(L'/') == string::npos || name.find(L'/') == 0) {
                                      addLexicalBinding(param, val);
                                      return;
                                    }

                                    char ns[name.length()+1];
                                    strcpy(ns, name.c_str());
                                    for (char *tok = strtok(ns, "/"); tok; tok=strtok(NULL, "/"))
                                      addLexicalBinding(newSym(tok), val);
                                  }

void bindParams(Cell* params, Cell* args) {
  if (params == nil) return;
  Cell* orderedArgs = reorderKeywordArgs(params, args);

  if (isQuoted(params)) {
    bindParams(cdr(params), orderedArgs);
    rmref(orderedArgs);
    return;
  }

  if (isSym(params))
    bindParamAliases(params, orderedArgs);
  else
    bindParams(car(params), car(orderedArgs));

  bindParams(cdr(params), cdr(orderedArgs));
  rmref(orderedArgs);
}



                                  long unquoteDepth(Cell* x) {
                                    if (!isCons(x) || car(x) != newSym(","))
                                      return 0;
                                    return unquoteDepth(cdr(x))+1;
                                  }

                                  Cell* stripUnquote(Cell* x) {
                                    if (!isCons(x) || car(x) != newSym(","))
                                      return x;
                                    return stripUnquote(cdr(x));
                                  }

Cell* processUnquotes(Cell* x, long depth, Cell* scope) {
  if (!isCons(x)) return mkref(x);

  if (unquoteDepth(x) == depth) {
    skippedAlreadyEvald = false;
    Cell* result = eval(stripUnquote(x), scope, true);
    if (!skippedAlreadyEvald) return result;
    result = newCons(newSym("''"), result);
    rmref(cdr(result));
    return mkref(result);
  }
  else if (car(x) == newSym(","))
    return mkref(x);

  if (isBackQuoted(x)) {
    Cell* result = newCons(car(x), processUnquotes(cdr(x), depth+1, scope));
    rmref(cdr(result));
    return mkref(result);
  }

  if (depth == 1 && isUnquoteSplice(car(x))) {
    Cell* result = eval(cdr(car(x)), scope, true);
    Cell* splice = processUnquotes(cdr(x), depth, scope);
    if (result == nil) return splice;
    // always splice in a copy
    Cell* resultcopy = copyList(result);
    rmref(result);
    append(resultcopy, splice);
    rmref(splice);
    return mkref(resultcopy);
  }

  Cell* result = newCons(processUnquotes(car(x), depth, scope),
                         processUnquotes(cdr(x), depth, scope));
  rmref(car(result));
  rmref(cdr(result));
  return mkref(result);
}

Cell* processUnquotes(Cell* x, long depth) {
  return processUnquotes(x, depth, currLexicalScopes.top());
}



                                  bool isFn(Cell* x) {
                                    if (!isCons(x)) return false;
                                    if (isCompiledFn(car(x))) return true;
                                    return toString(type(x)) == "function";
                                  }

                                  // (fn params . body) => (object function {sig, body, env})
                                  Cell* newFn(string type, Cell* expr, Cell* scope) {
                                    Cell* f = newTable();
                                    set(f, newSym("sig"), car(cdr(expr)));
                                    set(f, newSym("body"), cdr(cdr(expr)));
                                    set(f, newSym("env"), scope);
                                    return newObject(type, f);
                                  }

// HACK: explicitly reads from passed-in scope, but implicitly creates bindings
// to currLexicalScope. Carefully make sure it's popped off.
Cell* eval(Cell* expr, Cell* scope, bool dontStripAlreadyEval) {
  if (!expr)
    RAISE << "eval: cell should never be NUL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isColonSym(expr))
    return mkref(expr);

  if (isSym(expr))
    return mkref(lookup(expr, scope, dontStripAlreadyEval));

  if (isAtom(expr))
    return mkref(expr);

  if (isObject(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  if (isBackQuoted(expr))
    return processUnquotes(cdr(expr), 1, scope); // already mkref'd

  if (car(expr) == newSym("fn"))
    return mkref(newFn("function", expr, scope));
  else if (isFn(expr))
    // lexical scope is already attached
    return mkref(expr);

  newDynamicScope(CURR_LEXICAL_SCOPE, scope);
  // expr is a function call
  Cell* fn0 = eval(car(expr), scope, dontStripAlreadyEval);
  Cell* fn = fn0;
  if (fn0 != nil && !isFn(fn0))
    fn = coerceQuoted(fn0, newSym("function"), lookup("coercions*"));
  else
    fn = mkref(fn0);
  if (!isFn(fn))
    RAISE << "not a call: " << expr << endl
        << "  Perhaps the line is indented too much." << endl
        << "  Or you need to split it in two." << endl << DIE;

  // eval all its args in the current lexical scope
  Cell* splicedArgs = spliceArgs(callArgs(expr), scope, fn, dontStripAlreadyEval);
  // keyword args can change what we eval
  Cell* orderedArgs = reorderKeywordArgs(calleeSig(fn), splicedArgs);
  Cell* evaldArgs = evalArgs(calleeSig(fn), orderedArgs, scope, dontStripAlreadyEval);

  // swap in the function's lexical environment
  if (!isCompiledFn(calleeBody(fn)))
    newDynamicScope(CURR_LEXICAL_SCOPE, calleeEnv(fn));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindParams(calleeSig(fn), evaldArgs);
  if (!isCompiledFn(calleeBody(fn)))
    addLexicalBinding("caller-scope", scope);

  // eval all forms in body, save result of final form
  Cell* result = nil;
  if (isCompiledFn(calleeBody(fn)))
    result = toCompiledFn(calleeBody(fn))(); // all compiledFns must mkref result
  else
    for (Cell* form = calleeImpl(fn); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form), currLexicalScopes.top(), dontStripAlreadyEval);
    }

  endLexicalScope();
  if (!isCompiledFn(calleeBody(fn)))
    endDynamicScope(CURR_LEXICAL_SCOPE);

  rmref(evaldArgs);
  rmref(orderedArgs);
  rmref(splicedArgs);
  rmref(fn);
  rmref(fn0);
  endDynamicScope(CURR_LEXICAL_SCOPE);
  return result; // already mkref'd
}

Cell* eval(Cell* expr, Cell* scope) {
  return eval(expr, scope, false);
}

Cell* eval(Cell* expr) {
  return eval(expr, currLexicalScopes.top());
}

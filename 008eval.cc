//// eval: lookup symbols, respect quotes, rewrite fn calls

                                  bool isQuoted(Cell* cell) {
                                    return isCons(cell) && car(cell) == newSym("'");
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

                                  // fn = (fn params . body)
                                  Cell* sig(Cell* fn) {
                                    return car(cdr(fn));
                                  }

                                  Cell* body(Cell* fn) {
                                    return cdr(cdr(fn));
                                  }

                                  // callee = (type function|macro {sig, body, env, ..})
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



                                  Cell* unsplice(Cell* arg) {
                                    return eval(cdr(arg));
                                  }

// eval @exprs and inline them into args, tagging them with ''
Cell* spliceArgs(Cell* args) {
  Cell *pResult = newCell(), *tip = pResult;
  for (Cell* curr = args; curr != nil; curr=cdr(curr)) {
    if (isSplice(car(curr))) {
      Cell* x = unsplice(car(curr));
      for (Cell* curr2 = x; curr2 != nil; curr2=cdr(curr2), tip=cdr(tip))
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
                                    for (; args != nil; args=cdr(args)) {
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
  return result;
}



                                  Cell* unspliceAll(Cell* args) {
                                    Cell* pResult = newCell();
                                    for (Cell *curr=pResult, *arg=car(args); args != nil; args=cdr(args), arg=car(args), curr=last(curr)) {
                                      if (!isSplice(arg)) {
                                        addCons(curr, arg);
                                        continue;
                                      }
                                      RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)" << endl;
                                      Cell* newLimb = unsplice(arg);
                                      setCdr(curr, newLimb);
                                      rmref(newLimb);
                                    }
                                    return dropPtr(pResult);
                                  }

                                  Cell* evalArgs(Cell*, Cell*);
                                  Cell* spliceFirst(Cell* params, Cell* args) {
                                    Cell* result = unsplice(car(args));
                                    if (result == nil)
                                      return evalArgs(params, cdr(args));
                                    if (isCons(params) ? isQuoted(car(params)) : isQuoted(params))
                                      RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)" << endl;
                                    Cell* curr = result;
                                    for (; cdr(curr) != nil; curr=cdr(curr))
                                      params=cdr(params); // don't eval spliced args again, even if param is unquoted
                                    setCdr(curr, evalArgs(cdr(params), cdr(args)));
                                    rmref(cdr(curr));
                                    return result; // already mkref'd
                                  }

Cell* evalArgs(Cell* params, Cell* args) {
  if (args == nil) return nil;
  if (isQuoted(params))
    return unspliceAll(args); // already mkref'd

  if (isSplice(car(args)))
    return spliceFirst(params, args); // already mkref'd

  Cell* result = newCell();
  setCdr(result, evalArgs(cdr(params), cdr(args)));
  rmref(cdr(result));
  if (!isCons(params) || !isQuoted(car(params))) {
    setCar(result, eval(car(args)));
    rmref(car(result));
  }
  else {
    setCar(result, car(args));
  }
  return mkref(result);
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

  if (isQuoted(params)) {
    bindParams(cdr(params), args);
    return;
  }

  if (isSym(params))
    bindParamAliases(params, args);
  else
    bindParams(car(params), car(args));

  bindParams(cdr(params), cdr(args));
}



                                  int unquoteDepth(Cell* x) {
                                    if (!isCons(x) || car(x) != newSym(","))
                                      return 0;
                                    return unquoteDepth(cdr(x))+1;
                                  }

                                  Cell* stripUnquote(Cell* x) {
                                    if (!isCons(x) || car(x) != newSym(","))
                                      return x;
                                    return stripUnquote(cdr(x));
                                  }

Cell* processUnquotes(Cell* x, int depth) {
  if (!isCons(x)) return mkref(x);

  if (unquoteDepth(x) == depth)
    return eval(stripUnquote(x));
  else if (car(x) == newSym(","))
    return mkref(x);

  if (isBackQuoted(x)) {
    Cell* result = newCons(car(x), processUnquotes(cdr(x), depth+1));
    rmref(cdr(result));
    return mkref(result);
  }

  if (depth == 1 && isUnquoteSplice(car(x))) {
    Cell* result = eval(cdr(car(x)));
    Cell* splice = processUnquotes(cdr(x), depth);
    if (result == nil) return splice;
    // always splice in a copy
    Cell* resultcopy = copyList(result);
    rmref(result);
    append(resultcopy, splice);
    rmref(splice);
    return mkref(resultcopy);
  }

  Cell* result = newCons(processUnquotes(car(x), depth), processUnquotes(cdr(x), depth));
  rmref(car(result));
  rmref(cdr(result));
  return mkref(result);
}



                                  bool isFunc(Cell* x) {
                                    if (!isCons(x)) return false;
                                    if (isPrimFunc(car(x))) return true;
                                    string label = toString(type(x));
                                    return label == "function" || label == "macro";
                                  }

                                  Cell* newFunc(string type, Cell* expr) {
                                    Cell* f = newTable();
                                    unsafeSet(f, newSym("sig"), sig(expr), false);
                                    unsafeSet(f, newSym("body"), body(expr), false);
                                    unsafeSet(f, newSym("env"), currLexicalScopes.top(), false);
                                    return newObject(type, f);
                                  }

                                  Cell* functionify(Cell* obj) {
                                    if (obj == nil) return obj;
                                    Cell* coerceExpr = newCons(newSym("coerce-quoted"), newCons(obj, newCons(newSym("function"), nil)));
                                    rmref(obj);
                                    Cell* ans = eval(coerceExpr);
                                    rmref(coerceExpr);
                                    return ans;
                                  }

                                  Cell* implicitlyEval(Cell* x) {
                                    Cell* result = eval(x);
                                    rmref(x);
                                    return result;
                                  }

Cell* eval(Cell* expr) {
  if (!expr)
    RAISE << "eval: cell should never be NUL" << endl << DIE;

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

  if (isBackQuoted(expr))
    return processUnquotes(cdr(expr), 1); // already mkref'd

  if (car(expr) == newSym("fn"))
    return mkref(newFunc("function", expr));
  else if (car(expr) == newSym("mfn"))
    return mkref(newFunc("macro", expr));
  else if (isFunc(expr))
    // lexical scope is already attached
    return mkref(expr);

  // expr is a function call
  Cell* fn = eval(car(expr));
  if (!isFunc(fn))
    fn = functionify(fn);
  if (!isFunc(fn))
    RAISE << "not a call: " << expr << endl
        << "- Should it not be a call? Perhaps the expression is indented too much." << endl << DIE;

  // eval all its args in the current lexical scope
  Cell* orderedArgs = reorderKeywordArgs(calleeSig(fn), callArgs(expr));
  Cell* evaldArgs = evalArgs(calleeSig(fn), orderedArgs);

  // swap in the function's lexical environment
  if (!isPrimFunc(calleeBody(fn)))
    newDynamicScope(CURR_LEXICAL_SCOPE, calleeEnv(fn));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindParams(calleeSig(fn), evaldArgs);

  // eval all forms in body, save result of final form
  Cell* result = nil;
  if (isPrimFunc(calleeBody(fn)))
    result = toPrimFunc(calleeBody(fn))(); // all primFuncs must mkref result
  else
    for (Cell* form = calleeImpl(fn); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }

  endLexicalScope();
  if (!isPrimFunc(calleeBody(fn)))
    endDynamicScope(CURR_LEXICAL_SCOPE);

  // macros implicitly eval their result in the caller's scope
  if (type(fn) == newSym("macro"))
    result = implicitlyEval(result);

  rmref(evaldArgs);
  rmref(orderedArgs);
  rmref(fn);
  return result; // already mkref'd
}

//// eval: lookup symbols, respect quotes, rewrite fn calls

                                  bool isQuoted(Cell* cell) {
                                    return isCons(cell) && car(cell) == newSym(L"'");
                                  }

                                  Cell* stripQuote(Cell* cell) {
                                    return isQuoted(cell) ? cdr(cell) : cell;
                                  }

                                  bool isBackQuoted(Cell* cell) {
                                    return isCons(cell) && car(cell) == newSym(L"`");
                                  }

                                  bool isColonSym(Cell* x) {
                                    return isSym(x) && toString(x)[0] == L':';
                                  }

                                  Cell* sig(Cell* fn) {
                                    return car(cdr(fn));
                                  }

                                  Cell* body(Cell* fn) {
                                    return cdr(cdr(fn));
                                  }

                                  Cell* calleeBody(Cell* callee) {
                                    return car(cdr(cdr(callee)));
                                  }

                                  Cell* calleeEnv(Cell* callee) {
                                    return cdr(cdr(cdr(callee)));
                                  }

                                  Cell* callArgs(Cell* call) {
                                    return cdr(call);
                                  }



                                  // doesn't look inside destructured params
                                  Cell* keywordArg(Cell* params, Cell* args) {
                                    Cell* arg = car(args);
                                    if (!isColonSym(arg)) return nil;
                                    Cell* realArg = newSym(toString(arg).substr(1));
                                    for (; params != nil; params=cdr(params)) {
                                      if (realArg == params) // rest keyword arg must be last
                                        return newSym(L"__wartRestKeywordArg");
                                      if (realArg == newSym(L"do") && params == newSym(L"body"))
                                        return newSym(L"__wartRestKeywordArg");
                                      if (isCons(params) && realArg == car(params))
                                        return realArg;
                                    }
                                    return nil;
                                  }

                                  // extract keyword args into the CellMap provided; return non-keyword args
                                  Cell* extractKeywordArgs(Cell* params, Cell* args, CellMap& keywordArgs) {
                                    Cell *pNonKeywordArgs = newCell(), *curr = pNonKeywordArgs;
                                    for (; args != nil; args=cdr(args)) {
                                      Cell* currArg = keywordArg(params, args);
                                      if (currArg == newSym(L"__wartRestKeywordArg")) {
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

                                      if (keywordArgs[car(params)]) {
                                        addCons(curr, keywordArgs[car(params)]);
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



                                  bool isSplice(Cell* arg) {
                                    return isCons(arg) && car(arg) == newSym(L"@");
                                  }

                                  Cell* unsplice(Cell* arg) {
                                    return eval(cdr(arg));
                                  }

                                  Cell* unspliceAll(Cell* args) {
                                    Cell* pResult = newCell();
                                    for (Cell *curr=pResult, *arg=car(args); args != nil; args=cdr(args), arg=car(args), curr=last(curr)) {
                                      if (!isSplice(arg)) {
                                        addCons(curr, arg);
                                        continue;
                                      }
                                      Cell* newLimb = unsplice(arg);
                                      setCdr(curr, newLimb);
                                      rmref(newLimb);
                                    }
                                    return dropPtr(pResult); // mkref's
                                  }

                                  Cell* evalArgs(Cell*, Cell*);
                                  Cell* spliceFirst(Cell* params, Cell* args) {
                                    Cell* result = unsplice(car(args));
                                    if (result == nil)
                                      return evalArgs(params, cdr(args));
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

void bindArgs(Cell* params, Cell* args) {
  if (params == nil) return;

  if (isQuoted(params)) {
    bindArgs(cdr(params), args);
    return;
  }

  if (isSym(params))
    addLexicalBinding(params, args);
  else
    bindArgs(car(params), car(args));

  bindArgs(cdr(params), cdr(args));
}



                                  int unquoteDepth(Cell* x) {
                                    if (!isCons(x) || car(x) != newSym(L","))
                                      return 0;
                                    return unquoteDepth(cdr(x))+1;
                                  }

                                  Cell* stripUnquote(Cell* x) {
                                    if (!isCons(x) || car(x) != newSym(L","))
                                      return x;
                                    return stripUnquote(cdr(x));
                                  }

Cell* processUnquotes(Cell* x, int depth) {
  if (!isCons(x)) return mkref(x);

  if (unquoteDepth(x) == depth)
    return eval(stripUnquote(x));
  else if (car(x) == newSym(L","))
    return mkref(x);

  if (isBackQuoted(x)) {
    Cell* result = newCons(car(x), processUnquotes(cdr(x), depth+1));
    rmref(cdr(result));
    return mkref(result);
  }

  if (depth == 1 && isCons(car(x)) && car(car(x)) == newSym(L",@")) {
    Cell* result = eval(cdr(car(x)));
    Cell* splice = processUnquotes(cdr(x), depth);
    if (result == nil) return splice;
    append(result, splice);
    rmref(splice);
    return result; // already mkref'd
  }

  Cell* result = newCons(processUnquotes(car(x), depth), processUnquotes(cdr(x), depth));
  rmref(car(result));
  rmref(cdr(result));
  return mkref(result);
}



                                  bool isFunc(Cell* x) {
                                    return isCons(x)
                                      && (isPrimFunc(car(x)) || car(x) == newSym(L"evald-fn"));
                                  }

                                  void inlineCurrLexicalScope() {
                                    if (currLexicalScopes.top() == newSym(L"dynamic")) {
                                      assignDynamicVar(CURR_LEXICAL_SCOPE, nil);
                                      return;
                                    }
                                    for (Cell* scope = currLexicalScopes.top(); scope != nil; scope = cdr(scope)) { // Don't modify caller scopes
                                      if (cdr(scope) == DYNAMIC_SCOPE)
                                        setCdr(scope, nil);
                                    }
                                  }

                                  Cell* evalLambda(Cell* expr) {
                                    return newCons(newSym(L"evald-fn"),
                                        newCons(sig(expr),
                                            newCons(body(expr), currLexicalScopes.top())));
                                  }

Cell* eval(Cell* expr) {
  if (!expr)
    err << "eval: cell should never be NULL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isColonSym(expr))
    return mkref(expr);

  if (isSym(expr))
    return mkref(lookup(expr));

  if (isAtom(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  if (isBackQuoted(expr))
    return processUnquotes(cdr(expr), 1); // already mkref'd

  // experimental fix for macros
  if (car(expr) == newSym(L"ifn") && !isCons(currLexicalScopes.top()))
    inlineCurrLexicalScope(); // destructive: will mess with outer scopes

  if (car(expr) == newSym(L"fn") || car(expr) == newSym(L"ifn"))
    // attach current lexical scope
    return mkref(evalLambda(expr));
  else if (isFunc(expr))
    // lexical scope is already attached
    return mkref(expr);

  // expr is a function call
  Cell* fn = eval(car(expr));
  if (!isFunc(fn))
    err << "not a function call: " << expr << endl << DIE;

  // eval all its args in the current lexical scope
  Cell* realArgs = reorderKeywordArgs(sig(fn), callArgs(expr));
  Cell* evaldArgs = evalArgs(sig(fn), realArgs);

  // swap in the function's lexical environment
  if (!isPrimFunc(car(fn)))
    newDynamicScope(L"currLexicalScope",
        // most lookups will go straight to the cdr;
        // failed lookups may look at the caller's environment in the car
        newCons(currLexicalScopes.top(), calleeEnv(fn)));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindArgs(sig(fn), evaldArgs);

  // eval all forms in body, save result of final form
  Cell* result = nil;
  if (isPrimFunc(car(fn)))
    result = toPrimFunc(car(fn))(); // all primFuncs must mkref result
  else
    for (Cell* form = calleeBody(fn); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }

  endLexicalScope();
  if (!isPrimFunc(car(fn)))
    endDynamicScope(L"currLexicalScope");
  rmref(evaldArgs);
  rmref(realArgs);
  rmref(fn);
  return result; // already mkref'd
}

//// eval: lookup symbols, respect quotes, rewrite lambda calls

                                  bool isQuoted(Cell* cell) {
                                    return isCons(cell)
                                        && (car(cell) == newSym(L"'") || car(cell) == newSym(L"`"));
                                  }

                                  bool isColonSym(Cell* x) {
                                    return isSym(x) && toString(x)[0] == L':';
                                  }

                                  Cell* sig(Cell* lambda) {
                                    return car(cdr(lambda));
                                  }

                                  Cell* body(Cell* lambda) {
                                    return cdr(cdr(lambda));
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
                                        setCdr(curr, newCons(arg, nil));
                                        continue;
                                      }
                                      Cell* newLimb = unsplice(arg);
                                      setCdr(curr, newLimb);
                                      rmref(newLimb);
                                    }
                                    return dropPtr(pResult); // mkref's
                                  }

                                  Cell* evalArgs(Cell* params, Cell* args);
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



                                  Cell* processUnquotes(Cell* x) {
                                    if (!isCons(x)) return mkref(x);

                                    if (car(x) == newSym(L","))
                                      return eval(cdr(x));

                                    if (isCons(car(x)) && car(car(x)) == newSym(L",@")) {
                                      Cell* result = eval(cdr(car(x)));
                                      Cell* splice = processUnquotes(cdr(x));
                                      if (result == nil) return splice;
                                      append(result, splice);
                                      rmref(splice);
                                      return result; // already mkref'd
                                    }

                                    Cell* result = newCons(processUnquotes(car(x)), processUnquotes(cdr(x)));
                                    rmref(car(result));
                                    rmref(cdr(result));
                                    return mkref(result);
                                  }

                                  bool isFunc(Cell* x) {
                                    return isCons(x)
                                      && (isPrimFunc(car(x)) || car(x) == newSym(L"evald-lambda"));
                                  }

                                  Cell* evalLambda(Cell* expr) {
                                    return newCons(newSym(L"evald-lambda"),
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
    return processUnquotes(cdr(expr)); // already mkref'd

  if (car(expr) == newSym(L"lambda"))
    // attach current lexical scope
    return mkref(evalLambda(expr));
  else if (isFunc(expr))
    // lexical scope is already attached
    return mkref(expr);

  // expr is a function call
  Cell* lambda = eval(car(expr));
  if (!isFunc(lambda))
    err << "not a function call: " << expr << endl << DIE;

  // eval all its args in the current lexical scope
  Cell* evaldArgs = evalArgs(sig(lambda), callArgs(expr));

  // swap in the function's lexical environment
  if (!isPrimFunc(car(lambda)))
    newDynamicScope(L"currLexicalScope",
        newCons(calleeEnv(lambda), currLexicalScopes.top()));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindArgs(sig(lambda), evaldArgs);

  // eval all forms in body, save result of final form
  Cell* result = nil;
  if (isPrimFunc(car(lambda)))
    result = toPrimFunc(car(lambda))(); // all primFuncs must mkref result
  else
    for (Cell* form = calleeBody(lambda); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }

  endLexicalScope();
  if (!isPrimFunc(car(lambda)))
    endDynamicScope(L"currLexicalScope");
  rmref(evaldArgs);
  rmref(lambda);
  return result; // already mkref'd
}

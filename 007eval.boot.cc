//// eval: lookup symbols, respect quotes, rewrite lambda calls

                                  bool isQuoted(Cell* cell) {
                                    return isCons(cell)
                                        && (car(cell) == newSym(L"'") || car(cell) == newSym(L"`"));
                                  }

void bindArgs(Cell* params, Cell* args, Cell* expr) {
  if (params == nil) return;
  if (expr != nil)
    dbg2 << expr << ": bind " << params << endl;

  if (isQuoted(params)) {
    bindArgs(cdr(params), args, expr);
    return;
  }

  if (isSym(params)) {
    dbg2 << expr << ":   to " << args << endl;
    addLexicalBinding(params, args);
  }
  else
    bindArgs(car(params), car(args), expr);

  bindArgs(cdr(params), cdr(args), expr);
}

void bindArgs(Cell* params, Cell* args) {
  bindArgs(params, args, nil);
}



                                  Cell* sig(Cell* lambda) {
                                    return car(cdr(lambda));
                                  }

                                  Cell* body(Cell* lambda) {
                                    return cdr(cdr(lambda));
                                  }

                                  Cell* callee_body(Cell* callee) {
                                    return car(cdr(cdr(callee)));
                                  }

                                  Cell* callee_env(Cell* callee) {
                                    return cdr(cdr(cdr(callee)));
                                  }

                                  Cell* call_args(Cell* call) {
                                    return cdr(call);
                                  }

                                  extern Cell* eval(Cell*);

                                  Cell* eval_args(Cell* params, Cell* args) {
                                    if (args == nil) return nil;
                                    Cell* result = newCell();
                                    if (isQuoted(params)) {
                                      setCar(result, car(args));
                                      setCdr(result, cdr(args));
                                      return mkref(result);
                                    }
                                    setCdr(result, eval_args(cdr(params), cdr(args)));
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

                                  void append(Cell* x, Cell* y) {
                                    while(cdr(x) != nil)
                                      x = cdr(x);
                                    setCdr(x, y);
                                  }

                                  Cell* processUnquotes(Cell* x) {
                                    if (!isCons(x)) return x;

                                    if (car(x) == newSym(L","))
                                      return rmref(eval(cdr(x)));

                                    if (isCons(car(x)) && car(car(x)) == newSym(L",@")) {
                                      Cell* result = rmref(eval(cdr(car(x))));
                                      append(result, processUnquotes(cdr(x)));
                                      return result;
                                    }

                                    Cell* result = newCell();
                                    setCar(result, processUnquotes(car(x)));
                                    setCdr(result, processUnquotes(cdr(x)));
                                    return result;
                                  }

                                  bool isFunc(Cell* x) {
                                    return isCons(x)
                                      && (isPrimFunc(car(x)) || (car(x) == newSym(L"lambda")));
                                  }

Cell* eval(Cell* expr) {
  if (!expr)
    cerr << "eval: cell should never be NULL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isSym(expr))
    return mkref(lookup(expr));

  if (isAtom(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(processUnquotes(cdr(expr)));

  if (car(expr) == newSym(L"lambda")) {
    // attach current lexical scope
    Cell* ans = newCell();
    setCar(ans, car(expr));
    setCdr(ans, newCell());
    setCar(cdr(ans), sig(expr));
    setCdr(cdr(ans), newCell());
    setCar(cdr(cdr(ans)), body(expr));
    setCdr(cdr(cdr(ans)), currLexicalScopes.top());
    return mkref(ans);
  }

  if (car(expr) == newSym(L"eval")) {
    Cell* arg = eval(car(cdr(expr)));
    Cell* result = eval(arg);
    rmref(arg);
    return result;
  }

  // expr is a function call
  Cell* lambda = eval(car(expr));
  if (!isFunc(lambda))
    cerr << "not a function call: " << expr << endl << DIE;
  // eval all its args in the current lexical scope
  Cell* evald_args = eval_args(sig(lambda), call_args(expr));
  dbg2 << expr << ": " << "args " << evald_args << endl;
  dbg2 << currLexicalScopes.top() << endl;
  // swap in the function's lexical environment
  newDynamicScope(L"currLexicalScope", callee_env(lambda));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindArgs(sig(lambda), evald_args, expr);

  // eval all forms in body, save result of final form
  Cell* result = nil;
  if (isPrimFunc(car(lambda))) {
    result = toPrimFunc(car(lambda))(); // all primFuncs must mkref
  }
  else {
    for (Cell* form = callee_body(lambda); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }
  }

  endLexicalScope();
  endDynamicScope(L"currLexicalScope");
  rmref(evald_args);
  rmref(lambda);
  return result; // already mkref'd
}

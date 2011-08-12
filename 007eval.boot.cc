//// eval: lookup symbols, respect quotes, rewrite lambda calls

                                  bool isQuoted(Cell* cell) {
                                    return isCons(cell)
                                        && (car(cell) == newSym(L"'") || car(cell) == newSym(L"`"));
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

                                  void appendAndRmref(Cell* x, Cell* y) {
                                    while(cdr(x) != nil)
                                      x = cdr(x);
                                    setCdr(x, y);
                                    rmref(y);
                                  }

                                  Cell* processUnquotes(Cell* x) {
                                    if (!isCons(x)) return mkref(x);

                                    if (car(x) == newSym(L","))
                                      return eval(cdr(x));

                                    if (isCons(car(x)) && car(car(x)) == newSym(L",@")) {
                                      Cell* result = eval(cdr(car(x)));
                                      if (result == nil)
                                        return processUnquotes(cdr(x));
                                      appendAndRmref(result, processUnquotes(cdr(x)));
                                      return result;
                                    }

                                    Cell* result = newCell();
                                    setCar(result, processUnquotes(car(x)));
                                    setCdr(result, processUnquotes(cdr(x)));
                                    rmref(car(result));
                                    rmref(cdr(result));
                                    return mkref(result);
                                  }

                                  bool isFunc(Cell* x) {
                                    return isCons(x)
                                      && (isPrimFunc(car(x)) || (car(x) == newSym(L"evald-lambda")) || (car(x) == newSym(L"evald-elambda")));
                                  }

                                  Cell* implicitlyEval(Cell* x) {
                                    Cell* result = eval(x);
                                    rmref(x);
                                    return result;
                                  }

                                  int indent = 0;
                                  void printIndent() {
                                    for (int i = 0; i < indent; ++i)
                                      dbg2 << " ";
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
    return processUnquotes(cdr(expr));

  if (car(expr) == newSym(L"lambda") || car(expr) == newSym(L"elambda")) {
    // attach current lexical scope
    Cell* ans = newCell();
    setCar(ans, newSym(L"evald-"+toString(car(expr))));
    setCdr(ans, newCell());
    setCar(cdr(ans), sig(expr));
    setCdr(cdr(ans), newCell());
    setCar(cdr(cdr(ans)), body(expr));
    setCdr(cdr(cdr(ans)), currLexicalScopes.top());
    return mkref(ans);
  }

  if (car(expr) == newSym(L"evald-lambda") || car(expr) == newSym(L"evald-elambda")) {
    // lexical scope is already attached
    return mkref(expr);
  }

  if (car(expr) == newSym(L"eval")) {
    Cell* arg = eval(car(cdr(expr)));
    Cell* result = eval(arg);
    rmref(arg);
    return result;
  }

  if (isPrimFunc(car(expr)))
    return mkref(expr);

  // expr is a function call
  Cell* lambda = eval(car(expr));
  if (isPrimFunc(car(lambda))) {
    // primFuncs must eval their own args and mkref their result
    Cell* result = toPrimFunc(car(lambda))(cdr(expr));
    rmref(lambda);
    return result;
  }

  if (!isFunc(lambda))
    cerr << "not a function call: " << expr << endl << DIE;
  // eval all its args in the current lexical scope
  Cell* evald_args = eval_args(sig(lambda), call_args(expr));
  // swap in the function's lexical environment
  newDynamicScope(L"currLexicalScope", callee_env(lambda));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindArgs(sig(lambda), evald_args);

  // eval all forms in body, save result of final form
  Cell* result = nil;
  for (Cell* form = callee_body(lambda); form != nil; form = cdr(form)) {
    rmref(result);
    result = eval(car(form));
  }

  endLexicalScope();
  endDynamicScope(L"currLexicalScope");

  if (car(lambda) == newSym(L"evald-elambda"))
    result = implicitlyEval(result);

  rmref(evald_args);
  rmref(lambda);
  return result; // already mkref'd
}

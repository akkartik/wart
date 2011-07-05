//// eval: lookup symbols, respect quotes, rewrite lambda calls

                                  bool isQuoted(Cell* cell) {
                                    return isCons(cell)
                                        && (car(cell) == newSym(L"'") || car(cell) == newSym(L"`"));
                                  }

void bindArgs(Cell* params, Cell* args, bool quoted) {
  if (params == nil) return;

  if (isQuoted(params)) {
    bindArgs(cdr(params), args, true);
    return;
  }

  if (isSym(params))
    addLexicalBinding(params, args);
  else if (isQuoted(params))
    addLexicalBinding(cdr(params), args);
  else
    bindArgs(car(params), car(args), quoted);

  bindArgs(cdr(params), cdr(args), quoted);
}

void bindArgs(Cell* params, Cell* args) {
  bindArgs(params, args, false);
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
                                    if (isQuoted(params)) return args;
                                    setCdr(args, eval_args(cdr(params), cdr(args)));
                                    if (!isCons(params) || !isQuoted(car(params))) {
                                      Cell* result = eval(car(args));
                                      setCar(args, result);
                                      rmref(result);
                                    }
                                    return args;
                                  }

Cell* eval(Cell* expr) {
  if (!expr)
    cerr << "eval: cell should never be NULL" << endl << DIE;

  if (expr == nil)
    return nil;

  // Every path through eval must mkref the return value exactly once.
  // This implies rmref'ing the result of nested evals.
  if (isSym(expr))
    return mkref(lookup(expr));

  if (isAtom(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

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

  // expr is a function call
  Cell* lambda = eval(car(expr));
  // eval all its args in the current lexical scope
  Cell* evald_args = eval_args(sig(lambda), call_args(expr));
  // swap in the function's lexical environment
  newDynamicScope(L"currLexicalScope", callee_env(lambda));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindArgs(sig(lambda), evald_args);

  // eval all forms in body; save result of final form
  Cell* result = nil;
  if (isPrimFunc(car(lambda))) {
    result = mkref(toPrimFunc(car(lambda))());
  }
  else {
    for (Cell* form = callee_body(lambda); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }
  }

  endLexicalScope();
  endDynamicScope(newSym(L"currLexicalScope"));
  rmref(lambda);
  return result; // already mkref'd
}



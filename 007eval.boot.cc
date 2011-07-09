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
                                    if (isQuoted(params)) return args;
                                    setCdr(args, eval_args(cdr(params), cdr(args)));
                                    if (!isCons(params) || !isQuoted(car(params))) {
                                      Cell* result = eval(car(args));
                                      setCar(args, result);
                                      rmref(result);
                                    }
                                    return args;
                                  }

                                  bool isComma(Cell* x) {
                                    return isCons(x) && car(x) == newSym(L",");
                                  }

                                  bool isSplice(Cell* x) {
                                    return isCons(x) && car(x) == newSym(L",@");
                                  }

                                  Cell* processUnquotes(Cell* x) {
                                    if (!isCons(x)) return x;
                                    if (isComma(car(x))) {
                                      setCar(x, eval(cdr(car(x))));
                                      rmref(car(x));
                                    }
                                    else if (isSplice(car(x))) {
                                      Cell* rest = mkref(cdr(x));
                                      Cell* snip = eval(cdr(car(x)));
                                      setCar(x, car(snip));
                                      setCdr(x, cdr(snip));
                                      rmref(snip);
                                      while (cdr(x) != nil)
                                        x = cdr(x);
                                      setCdr(x, rest);
                                      rmref(rest);
                                    }
                                    processUnquotes(cdr(x));
                                    return x;
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

  if (car(expr) == newSym(L"lambda") || car(expr) == newSym(L"collect")) {
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

  // eval all forms in body, save result of final form
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

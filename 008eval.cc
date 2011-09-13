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



                                  bool isColonSym(Cell* x) {
                                    return isSym(x) && toString(x)[0] == L':';
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

                                  bool containsSplice(Cell* call) {
                                    for (Cell* expr = cdr(call); isCons(expr); expr=cdr(expr)) {
                                      Cell* arg = car(expr);
                                      if (isCons(arg) && car(arg) == newSym(L"@"))
                                        return true;
                                    }
                                    return false;
                                  }

                                  Cell* expandSplice(Cell* substrate, Cell* arg) {
                                    if (!isCons(arg) || car(arg) != newSym(L"@")) {
                                      // no splice
                                      setCdr(substrate, newCons(car(arg), nil));
                                      return cdr(substrate);
                                    }

                                    Cell* newLimb = eval(cdr(arg));
                                    if (newLimb != nil && !isCons(newLimb))
                                      warn << "No cons to splice in " << arg << endl;

                                    for (Cell* cell = newLimb; cell != nil; cell=cdr(cell), substrate=cdr(substrate)) {
                                      // spliced-in args shouldn't get re-eval'd
                                      // bind a new sym to them and splice it in instead
                                      Cell* sym = genSym(nil);
                                      addLexicalBinding(sym, car(cell));
                                      setCdr(substrate, newCons(sym, nil));
                                    }

                                    rmref(newLimb);
                                    return cdr(substrate);
                                  }

                                  // modifies currentLexicalScope; give it a a fresh scope
                                  Cell* expandSplices(Cell* call) {
                                    if (!containsSplice(call)) return call;

                                    Cell* result = newCons(car(call), nil); // we never check function position
                                    for (Cell *args=cdr(call), *curr=result; args != nil; args=cdr(args))
                                      curr = expandSplice(curr, car(args));
                                    addLexicalBinding(genSym(nil), result); // hook for GC
                                    return result;
                                  }

                                  Cell* eval_args(Cell* params, Cell* args) {
                                    if (args == nil) return nil;
                                    if (isQuoted(params)) {
                                      return mkref(args);
                                    }

                                    Cell* result = newCell();
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
                                      return result;
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

                                  Cell* eval_lambda(Cell* expr) {
                                    return newCons(newSym(L"evald-lambda"),
                                        newCons(sig(expr),
                                            newCons(body(expr), currLexicalScopes.top())));
                                  }

                                  int indent = 0;
                                  void printIndent() {
                                    for (int i = 0; i < indent; ++i)
                                      dbg2 << " ";
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
    return mkref(eval_lambda(expr));
  else if (isFunc(expr))
    // lexical scope is already attached
    return mkref(expr);

  newLexicalScope(); // for variables created by expandSlice
  expr = expandSplices(expr);

  // expr is a function call
  Cell* lambda = eval(car(expr));

  if (!isFunc(lambda))
    err << "not a function call: " << expr << endl << DIE;

  if (isPrimFunc(car(lambda))) {
    // primFuncs must eval their own args and mkref their result
    Cell* result = toPrimFunc(car(lambda))(cdr(expr));
    rmref(lambda);
    endLexicalScope(); // splice
    return result; // already mkref'd
  }

  // eval all its args in the current lexical scope
  Cell* evald_args = eval_args(sig(lambda), call_args(expr));

  // swap in the function's lexical environment
  newDynamicScope(L"currLexicalScope",
      newCons(callee_env(lambda), currLexicalScopes.top()));
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
  rmref(evald_args);
  rmref(lambda);
  endLexicalScope(); // splice
  return result; // already mkref'd
}

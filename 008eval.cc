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



                                  Cell* unsplice(Cell* arg, Cell* env) {
                                    return eval(cdr(arg), env);
                                  }

// eval @exprs and inline them into args, tagging them with ''
Cell* spliceArgs(Cell* args, Cell* fn, Cell* env) {
  Cell *pResult = newCell(), *tip = pResult;
  for (Cell* curr = args; curr != nil; curr=cdr(curr)) {
    if (isSplice(car(curr))) {
      if (type(fn) == newSym("macro"))
        RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)" << endl;

      Cell* x = unsplice(car(curr), env);
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

Cell* spliceArgs(Cell* args, Cell* fn) {
  return spliceArgs(args, fn, currLexicalScopes.top());
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
  return result; // already mkref'd
}



                                  Cell* stripAlreadyEval(Cell* args) {
                                    Cell *pResult = newCell();
                                    for (Cell *from=args, *to=pResult; from != nil; from=cdr(from), to=cdr(to)) {
                                      if (isAlreadyEvald(car(from)))
                                        addCons(to, cdr(car(from)));
                                      else
                                        addCons(to, car(from));
                                    }
                                    return dropPtr(pResult);
                                  }

Cell* evalArgs(Cell* params, Cell* args, Cell* env) {
  if (args == nil) return nil;

  if (isQuoted(params))
    return stripAlreadyEval(args);

  Cell* result = newCell();
  setCdr(result, evalArgs(cdr(params), cdr(args), env));
  rmref(cdr(result));

  if (isAlreadyEvald(car(args)))
    setCar(result, cdr(car(args)));
  else if (isCons(params) && isQuoted(car(params)))
    setCar(result, car(args));
  else {
    setCar(result, eval(car(args), env));
    rmref(car(result));
  }
  return mkref(result);
}

Cell* evalArgs(Cell* params, Cell* args) {
  return evalArgs(params, args, currLexicalScopes.top());
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

Cell* processUnquotes(Cell* x, int depth, Cell* env) {
  if (!isCons(x)) return mkref(x);

  if (unquoteDepth(x) == depth)
    return eval(stripUnquote(x), env);
  else if (car(x) == newSym(","))
    return mkref(x);

  if (isBackQuoted(x)) {
    Cell* result = newCons(car(x), processUnquotes(cdr(x), depth+1, env));
    rmref(cdr(result));
    return mkref(result);
  }

  if (depth == 1 && isUnquoteSplice(car(x))) {
    Cell* result = eval(cdr(car(x)), env);
    Cell* splice = processUnquotes(cdr(x), depth, env);
    if (result == nil) return splice;
    // always splice in a copy
    Cell* resultcopy = copyList(result);
    rmref(result);
    append(resultcopy, splice);
    rmref(splice);
    return mkref(resultcopy);
  }

  Cell* result = newCons(processUnquotes(car(x), depth, env),
                         processUnquotes(cdr(x), depth, env));
  rmref(car(result));
  rmref(cdr(result));
  return mkref(result);
}

Cell* processUnquotes(Cell* x, int depth) {
  return processUnquotes(x, depth, currLexicalScopes.top());
}



                                  bool isFunc(Cell* x) {
                                    if (!isCons(x)) return false;
                                    if (isPrimFunc(car(x))) return true;
                                    string label = toString(type(x));
                                    return label == "function" || label == "macro" || label == "vau";
                                  }

                                  Cell* newFunc(string type, Cell* expr) {
                                    Cell* f = newTable();
                                    set(f, newSym("sig"), sig(expr));
                                    set(f, newSym("body"), body(expr));
                                    set(f, newSym("env"), currLexicalScopes.top());
                                    return newObject(type, f);
                                  }

                                  Cell* newVau(Cell* expr) {
                                    Cell* f = newTable();
                                    set(f, newSym("sig"), newCons(newSym("'"), sig(expr)));
                                    set(f, newSym("body"), body(expr));
                                    set(f, newSym("env"), currLexicalScopes.top());
                                    return newObject("vau", f);
                                  }

                                  Cell* implicitlyEval(Cell* x, Cell* env) {
                                    Cell* result = eval(x, env);
                                    rmref(x);
                                    return result;
                                  }

// HACK: explicitly reads from passed-in env, but implicitly creates bindings
// to currLexicalScope. Carefully make sure it's popped off.
Cell* eval(Cell* expr, Cell* env) {
  if (!expr)
    RAISE << "eval: cell should never be NUL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isColonSym(expr))
    return mkref(expr);

  if (isSym(expr))
    return mkref(lookup(expr, env));

  if (isAtom(expr))
    return mkref(expr);

  if (isObject(expr))
    return mkref(expr);

  if (isQuoted(expr) || isAlreadyEvald(expr))
    return mkref(cdr(expr));

  if (isBackQuoted(expr))
    return processUnquotes(cdr(expr), 1, env); // already mkref'd

  if (car(expr) == newSym("fn"))
    return mkref(newFunc("function", expr));
  else if (car(expr) == newSym("mfn"))
    return mkref(newFunc("macro", expr));
  else if (car(expr) == newSym("vau"))
    return mkref(newVau(expr));
  else if (isFunc(expr))
    // lexical scope is already attached
    return mkref(expr);

  // expr is a function call
  Cell* fn = eval(car(expr), env);
  if (fn != nil && !isFunc(fn))
    fn = coerceQuoted(fn, newSym("function"), lookup("coercions*"));
  if (!isFunc(fn))
    RAISE << "not a call: " << expr << endl
        << "- Should it not be a call? Perhaps the expression is indented too much." << endl << DIE;

  // eval all its args in the current lexical scope
  Cell* splicedArgs = spliceArgs(callArgs(expr), fn, env);
  Cell* orderedArgs = reorderKeywordArgs(calleeSig(fn), splicedArgs);
  Cell* evaldArgs = evalArgs(calleeSig(fn), orderedArgs, env);

  Cell* callerEnv = currLexicalScopes.top();

  // swap in the function's lexical environment
  if (!isPrimFunc(calleeBody(fn)))
    newDynamicScope(CURR_LEXICAL_SCOPE, calleeEnv(fn));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindParams(calleeSig(fn), evaldArgs);
  if (type(fn) == newSym("vau"))
    addLexicalBinding("caller-env", callerEnv);

  // eval all forms in body, save result of final form
  Cell* result = nil;
  if (isPrimFunc(calleeBody(fn)))
    result = toPrimFunc(calleeBody(fn))(); // all primFuncs must mkref result
  else
    for (Cell* form = calleeImpl(fn); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form)); // use fn's env
    }

  endLexicalScope();
  if (!isPrimFunc(calleeBody(fn)))
    endDynamicScope(CURR_LEXICAL_SCOPE);

  // macros implicitly eval their result in the caller's scope
  if (type(fn) == newSym("macro"))
    result = implicitlyEval(result, env);

  rmref(evaldArgs);
  rmref(orderedArgs);
  rmref(splicedArgs);
  rmref(fn);
  return result; // already mkref'd
}

Cell* eval(Cell* expr) {
  return eval(expr, currLexicalScopes.top());
}

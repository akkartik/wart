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

                                  // fn = (object function {sig, body, env, ..})
                                  bool isFn(Cell* x) {
                                    return isCons(x) && toString(type(x)) == "function";
                                  }

                                  Cell* sig(Cell* fn) {
                                    return get(rep(fn), newSym("sig"));
                                  }

                                  Cell* body(Cell* fn) {
                                    return get(rep(fn), newSym("body"));
                                  }

                                  Cell* impl(Cell* fn) {
                                    Cell* impl = get(rep(fn), newSym("optimized-body"));
                                    return (impl != nil) ? impl : body(fn);
                                  }

                                  Cell* env(Cell* fn) {
                                    return get(rep(fn), newSym("env"));
                                  }



                                  Cell* unsplice(Cell* arg, Cell* scope) {
                                    return eval(cdr(arg), scope);
                                  }

                                  // supporting @ in macro calls
                                  stack<bool> inMacro;
                                  bool keepAlreadyEvald() {
                                    if (inMacro.empty()) inMacro.push(false);
                                    return inMacro.top();
                                  }

                                  Cell* tagAlreadyEvald(Cell* cell) {
                                    if (isColonSym(cell)) return cell;
                                    return newCons(newSym("''"), cell);
                                  }

                                  bool isAlreadyEvald(Cell* cell) {
                                    return isCons(cell) && car(cell) == newSym("''");
                                  }

                                  Cell* stripAlreadyEvald(Cell* cell) {
                                    while (isAlreadyEvald(cell))
                                      cell = cdr(cell);
                                    return cell;
                                  }

                                  // keep sync'd with mac
                                  bool isMacro(Cell* fn) {
                                    if (!isObject(fn)) return false;
                                    if (!isQuoted(sig(fn))) return false;
                                    Cell* forms = body(fn);
                                    if (cdr(forms) != nil) return false;
                                    Cell* form = car(forms);
                                    if (car(form) != newSym("mac-eval")) return false;
                                    if (car(cdr(cdr(form))) != newSym("caller-scope")) return false;
                                    if (cdr(cdr(cdr(form))) != nil) return false;
                                    return true;
                                  }

                                  bool isMacroWithoutBackquotes(Cell* fn) {
                                    if (!isMacro(fn)) return false;
                                    return !contains(body(fn), newSym("`"));
                                  }

// eval @exprs and inline them into args, tagging them with '' (already eval'd)
Cell* spliceArgs(Cell* args, Cell* scope, Cell* fn) {
  Cell *pResult = newCell(), *tip = pResult;
  for (Cell* curr = args; curr != nil; curr=cdr(curr)) {
    if (isSplice(car(curr))) {
      if (isMacroWithoutBackquotes(fn))
        RAISE << "calling macros with splice can have subtle effects (http://arclanguage.org/item?id=15659)" << endl;
      Cell* x = unsplice(car(curr), scope);
      for (Cell* curr2 = x; curr2 != nil; curr2=cdr(curr2), tip=cdr(tip))
        addCons(tip, tagAlreadyEvald(car(curr2)));
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

                                  // returns the appropriate param if arg is a valid keyword arg
                                  // responds to rest keyword args with (rest-param)
                                  // doesn't look inside destructured params
                                  Cell* keywordArg(Cell* arg, Cell* params) {
                                    if (!isColonSym(arg)) return nil;
                                    Cell* realArg = newSym(toString(arg).substr(1));
                                    for (; params != nil; params=cdr(params)) {
                                      if (!isCons(params)) {
                                        if (paramAliasMatch(realArg, params))
                                          return newCons(params);
                                      }
                                      else {
                                        Cell* param = stripQuote(car(params));
                                        if (paramAliasMatch(realArg, param))
                                          return param;
                                      }
                                    }
                                    return nil;
                                  }

                                  // extract keyword args into the CellMap provided; return non-keyword args
                                  Cell* extractKeywordArgs(Cell* params, Cell* args, CellMap& keywordArgs) {
                                    Cell *pNonKeywordArgs = newCell(), *curr = pNonKeywordArgs;
                                    for (; isCons(args); args=cdr(args)) {
                                      Cell* currArg = keywordArg(car(args), params);
                                      if (currArg == nil) {
                                        addCons(curr, car(args));
                                        curr=cdr(curr);
                                      }
                                      else if (isCons(currArg)) { // rest keyword arg
                                        keywordArgs[car(currArg)] = cdr(args); // ..must be final keyword arg
                                        rmref(currArg);
                                        args = nil;
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
                                    for (Cell* curr = pReconstitutedArgs; params != nil; curr=cdr(curr), params=stripQuote(cdr(params))) {
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
  if (!isCons(stripQuote(params))) return mkref(args);

  CellMap keywordArgs;
  Cell* nonKeywordArgs = extractKeywordArgs(params, args, keywordArgs);
  Cell* result = argsInParamOrder(params, nonKeywordArgs, keywordArgs);   rmref(nonKeywordArgs);
  return result; // already mkref'd
}



                                  Cell* evalArg(Cell* params, Cell* arg, Cell* scope) {
                                    if (isAlreadyEvald(arg)) return mkref(stripAlreadyEvald(arg));
                                    if (isCons(params) && isQuoted(car(params))) return mkref(arg);
                                    return eval(arg, scope);
                                  }

Cell* evalArgs(Cell* params, Cell* args, Cell* scope) {
  if (args == nil) return nil;
  if (isQuoted(params)) return mkref(args);

  Cell* result = newCell();
  setCar(result, evalArg(params, car(args), scope));  rmref(car(result));
  setCdr(result, evalArgs(cdr(params), cdr(args), scope));  rmref(cdr(result));
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
  params = stripQuote(params);
  if (params == nil) return;

  Cell* orderedArgs = reorderKeywordArgs(params, args);
  if (isSym(params)) {
    bindParamAliases(params, orderedArgs);
  }
  else {
    bindParams(car(params), car(orderedArgs));
    bindParams(cdr(params), cdr(orderedArgs));
  }
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

                                  // when inMacro did we encounter ''?
                                  bool skippedAlreadyEvald = false;
                                  Cell* maybeStripAlreadyEvald(bool keepAlreadyEvald, Cell* x) {
                                    skippedAlreadyEvald = isAlreadyEvald(x);
                                    return keepAlreadyEvald ? x : stripAlreadyEvald(x);
                                  }

Cell* processUnquotes(Cell* x, long depth, Cell* scope) {
  if (!isCons(x)) return mkref(x);

  if (unquoteDepth(x) == depth) {
    skippedAlreadyEvald = false;
    Cell* result = eval(stripUnquote(x), scope);
    return skippedAlreadyEvald ? pushCons(newSym("''"), result) : result;
  }
  else if (unquoteDepth(x) > 0) {
    return mkref(x);
  }

  if (isBackQuoted(x)) {
    Cell* result = newCons(car(x), processUnquotes(cdr(x), depth+1, scope));
    rmref(cdr(result));
    return mkref(result);
  }

  if (depth == 1 && isUnquoteSplice(car(x))) {
    Cell* result = eval(cdr(car(x)), scope);
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



                                  Cell* toFn(Cell* x) {
                                    if (x == nil || isFn(x)) return x;
                                    Cell* result = coerceQuoted(x, newSym("function"), lookup("coercions*"));   rmref(x);
                                    return result;
                                  }

                                  Cell* processArgs(Cell* call, Cell* scope, Cell* fn) {
                                    Cell* splicedArgs = spliceArgs(cdr(call), scope, fn);
                                    Cell* orderedArgs = reorderKeywordArgs(sig(fn), splicedArgs);   rmref(splicedArgs);
                                    Cell* evaldArgs = evalArgs(sig(fn), orderedArgs, scope);  rmref(orderedArgs);
                                    dbg << car(call) << "/" << keepAlreadyEvald() << ": " << evaldArgs << endl;
                                    return evaldArgs;
                                  }

Cell* eval(Cell* expr, Cell* scope) {
  if (!expr)
    RAISE << "eval: cell should never be NUL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isColonSym(expr))
    return mkref(expr);

  if (isSym(expr))
    return mkref(lookup(expr, scope, keepAlreadyEvald()));

  if (isAtom(expr))
    return mkref(expr);

  if (isObject(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  if (isBackQuoted(expr))
    return processUnquotes(cdr(expr), 1, scope); // already mkref'd

  if (isAlreadyEvald(expr))
    return mkref(keepAlreadyEvald() ? expr : stripAlreadyEvald(expr));

  // expr is a call
  Cell* fn = toFn(eval(car(expr), scope));
  if (!isFn(fn))
    RAISE << "not a call: " << expr << endl
        << "  Perhaps the line is indented too much." << endl
        << "  Or you need to split it in two." << endl << DIE;

  // eval its args in the caller's lexical environment
  Cell* evaldArgs = processArgs(expr, scope, fn);
  // swap in the function's lexical environment
  newDynamicScope(CURR_LEXICAL_SCOPE, isCompiledFn(body(fn)) ? scope : env(fn));
  newLexicalScope();
  bindParams(sig(fn), evaldArgs);
  addLexicalBinding("caller-scope", scope);

  Cell* result = nil;
  if (isCompiledFn(body(fn)))
    result = toCompiledFn(body(fn))(); // all compiledFns must mkref result
  else
    // eval all forms in body, save result of final form
    for (Cell* form = impl(fn); form != nil; form=cdr(form)) {
      rmref(result);
      result = eval(car(form), currLexicalScopes.top());
    }

  endLexicalScope();
  endDynamicScope(CURR_LEXICAL_SCOPE);
  rmref(evaldArgs);
  rmref(fn);
  return result; // already mkref'd
}

Cell* eval(Cell* expr) {
  return eval(expr, currLexicalScopes.top());
}

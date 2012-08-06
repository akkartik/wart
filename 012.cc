//// core compiled primitives

// these have access to caller scope
// params start with $ by convention to avoid shadowing

COMPILE_FN(eval, compiledFn_eval, "($x . $scope)",
  Cell* scope = lookup("$scope");
  if (scope == nil) scope = currLexicalScopes.top();
  else scope = car(scope);
  return eval(lookup("$x"), scope);
)

// eval with extra smarts for handling @args
COMPILE_FN(mac-eval, compiledFn_mac_eval, "('$x $scope)",
  inMacro.push(true);
  Cell* x = eval(lookup("$x"), currLexicalScopes.top());
  Cell* ans = eval(x, lookup("$scope"));
  rmref(x);
  inMacro.pop();
  return ans;
)

COMPILE_FN(mac?, compiledFn_isMacro, "($f)",
  Cell* f = lookup("$f");
  return isMacro(f) ? mkref(f) : nil;
)

COMPILE_FN(try-eval, compiledFn_try_eval, "($x . $scope)",
  bool oldPretendRaise = pretendRaise;
  pretendRaise = true;
    Cell* ans = compiledFn_eval();
  pretendRaise = oldPretendRaise;

  if (raiseCount == 0) return ans;
  // error
  raiseCount = 0;
  rmref(ans);
  return nil;
)

COMPILE_FN(fn, compiledFn_fn, "'($params . $body)",
  Cell* f = newTable();
  set(f, newSym("sig"), lookup("$params"));
  set(f, newSym("body"), lookup("$body"));
  set(f, newSym("env"), cdr(currLexicalScopes.top()));
  return mkref(newObject("function", f));
)

COMPILE_FN(if, compiledFn_if, "($cond '$then '$else)",
  return lookup("$cond") != nil ? eval(lookup("$then")) : eval(lookup("$else"));
)

COMPILE_FN(not, compiledFn_not, "($x)",
  return lookup("$x") == nil ? mkref(newNum(1)) : nil;
)

COMPILE_FN(uniq, compiledFn_uniq, "($x)",
  return mkref(genSym(lookup("$x")));
)

                                  void assign(Cell* var, Cell* val) {
                                    Cell* scope = scopeContainingBinding(var, currLexicalScopes.top());
                                    if (!scope)
                                      newDynamicScope(var, val);
                                    else if (scope == nil)
                                      assignDynamicVar(var, val);
                                    else
                                      unsafeSet(scope, var, val, false);
                                  }

COMPILE_FN(=, compiledFn_assign, "('$var $val)",
  Cell* var = lookup("$var");
  Cell* val = lookup("$val");
  assign(var, val);
  return mkref(val);
)

COMPILE_FN(dyn_bind, compiledFn_dyn_bind, "('$var $val)",
  newDynamicScope(lookup("$var"), lookup("$val"));
  return nil;
)

COMPILE_FN(dyn_unbind, compiledFn_dyn_unbind, "('$var)",
  endDynamicScope(lookup("$var"));
  return nil;
)

COMPILE_FN(bound?, compiledFn_isBound, "($var)",
  Cell* var = lookup("$var");
  if (var == nil) return mkref(newNum(1));
  if (!scopeContainingBinding(var, currLexicalScopes.top()))
    return nil;
  return mkref(var);
)

COMPILE_FN(make-unbound, compiledFn_make_unbound, "($var)",
  Cell* var = lookup("$var");
  stack<Cell*>& bindings = dynamics[var]; // unbind just in dynamic scopes
  while (!bindings.empty()) {
    rmref(var);
    rmref(bindings.top());
    bindings.pop();
  }
  return nil;
)

// type can't take 2 args; calls would look like type constructors
COMPILE_FN(type, compiledFn_type, "($x)",
  return mkref(type(lookup("$x")));
)

COMPILE_FN(coerce-quoted, compiledFn_coerce_quoted, "'($x $dest-type)",
  return coerceQuoted(lookup("$x"), lookup("$dest-type"), lookup("coercions*")); // already mkref'd
)

COMPILE_FN(iso, compiledFn_iso, "($x $y)",
  Cell* x = lookup("$x");
  Cell* y = lookup("$y");
  Cell* result = nil;
  if (x == nil && y == nil)
    result = newNum(1);
  else if (x == y)
    result = x;
  else if (x->type == FLOAT || y->type == FLOAT)
    result = (equalFloats(toFloat(x), toFloat(y)) ? x : nil);
  else if (isString(x) && isString(y) && toString(x) == toString(y))
    result = x;
  else
    result = nil;
  return mkref(result);
)



// list? will not be true of (object _ _) expressions, but cons? will
COMPILE_FN(cons?, compiledFn_isCons, "($x)",
  Cell* x = lookup("$x");
  if (!isCons(x)) return nil;
  return mkref(x);
)

COMPILE_FN(cons, compiledFn_cons, "($x $y)",
  return mkref(newCons(lookup("$x"), lookup("$y")));
)

COMPILE_FN(car, compiledFn_car, "($l)",
  return mkref(car(lookup("$l")));
)

COMPILE_FN(cdr, compiledFn_cdr, "($l)",
  return mkref(cdr(lookup("$l")));
)

COMPILE_FN(set_car, compiledFn_set_car, "($cons $val)",
  setCar(lookup("$cons"), lookup("$val"));
  return mkref(lookup("$val"));
)

COMPILE_FN(set_cdr, compiledFn_set_cdr, "($cons $val)",
  setCdr(lookup("$cons"), lookup("$val"));
  return mkref(lookup("$val"));
)

COMPILE_FN(len, compiledFn_len, "($x)",
  Cell* x = lookup("$x");
  if (isString(x))
    return mkref(newNum((long)toString(x).length()));

  long ans = 0;
  for (; x != nil; x=cdr(x))
    ++ans;
  return mkref(newNum(ans));
)

//// core compiled primitives

// these have access to caller scope
// use $vars to avoid shadowing
// they never call each other; needn't generate gensyms for $vars

COMPILE_PRIM_FUNC(eval, primFunc_eval, "($x)",
  return eval(lookup("$x"));
)

COMPILE_PRIM_FUNC(if, primFunc_if, "($cond '$then '$else)",
  return lookup("$cond") != nil ? eval(lookup("$then")) : eval(lookup("$else"));
)

COMPILE_PRIM_FUNC(not, primFunc_not, "($x)",
  return lookup("$x") == nil ? mkref(newNum(1)) : nil;
)

COMPILE_PRIM_FUNC(uniq, primFunc_uniq, "($x)",
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

COMPILE_PRIM_FUNC(=, primFunc_assign, "('$var $val)",
  Cell* var = lookup("$var");
  Cell* val = lookup("$val");
  assign(var, val);
  return mkref(val);
)

COMPILE_PRIM_FUNC(dyn_bind, primFunc_dyn_bind, "('$var $val)",
  newDynamicScope(lookup("$var"), lookup("$val"));
  return nil;
)

COMPILE_PRIM_FUNC(dyn_unbind, primFunc_dyn_unbind, "('$var)",
  endDynamicScope(lookup("$var"));
  return nil;
)

COMPILE_PRIM_FUNC(bound?, primFunc_isBound, "($var)",
  Cell* var = lookup("$var");
  if (var == nil) return mkref(newNum(1));
  if (!scopeContainingBinding(var, currLexicalScopes.top()))
    return nil;
  return mkref(var);
)

COMPILE_PRIM_FUNC(type, primFunc_type, "($x)",
  return mkref(type(lookup("$x")));
)

COMPILE_PRIM_FUNC(iso, primFunc_iso, "($x $y)",
  Cell* x = lookup("$x");
  Cell* y = lookup("$y");
  Cell* result = nil;
  if (x == nil && y == nil)
    result = newNum(1);
  else if (x == y)
    result = x;
  else if (isString(x) && isString(y) && toString(x) == toString(y))
    result = x;
  else
    result = nil;
  return mkref(result);
)



// list? will not be true of user-defined (type 'foo ...) but cons? will.
COMPILE_PRIM_FUNC(cons?, primFunc_isCons, "($x)",
  Cell* x = lookup("$x");
  if (!isCons(x)) return nil;
  return mkref(x);
)

COMPILE_PRIM_FUNC(cons, primFunc_cons, "($x $y)",
  return mkref(newCons(lookup("$x"), lookup("$y")));
)

COMPILE_PRIM_FUNC(car, primFunc_car, "($l)",
  return mkref(car(lookup("$l")));
)

COMPILE_PRIM_FUNC(cdr, primFunc_cdr, "($l)",
  return mkref(cdr(lookup("$l")));
)

COMPILE_PRIM_FUNC(set_car, primFunc_set_car, "($cons $val)",
  setCar(lookup("$cons"), lookup("$val"));
  return mkref(lookup("$val"));
)

COMPILE_PRIM_FUNC(set_cdr, primFunc_set_cdr, "($cons $val)",
  setCdr(lookup("$cons"), lookup("$val"));
  return mkref(lookup("$val"));
)

COMPILE_PRIM_FUNC(len, primFunc_len, "($x)",
  Cell* x = lookup("$x");
  if (isString(x))
    return mkref(newNum(toString(x).length()));

  int ans = 0;
  for (; x != nil; x=cdr(x))
    ++ans;
  return mkref(newNum(ans));
)

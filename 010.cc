//// core compiled primitives

// primFuncs take $vars as params.
// $vars needn't turn into gensyms if primFuncs don't call each other.

COMPILE_PRIM_FUNC(eval, primFunc_eval, L"($x)",
  return eval(lookup(L"$x"));
)

COMPILE_PRIM_FUNC(if, primFunc_if, L"($cond '$then '$else)",
  return lookup(L"$cond") != nil ? eval(lookup(L"$then")) : eval(lookup(L"$else"));
)

COMPILE_PRIM_FUNC(not, primFunc_not, L"($x)",
  return lookup(L"$x") == nil ? mkref(newNum(1)) : nil;
)

COMPILE_PRIM_FUNC(uniq, primFunc_uniq, L"($x)",
  return mkref(genSym(lookup(L"$x")));
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

COMPILE_PRIM_FUNC(=, primFunc_assign, L"('$var $val)",
  Cell* var = lookup(L"$var");
  Cell* val = lookup(L"$val");
  assign(var, val);
  return mkref(val);
)



COMPILE_PRIM_FUNC(cons, primFunc_cons, L"($x $y)",
  return mkref(newCons(lookup(L"$x"), lookup(L"$y")));
)

COMPILE_PRIM_FUNC(car, primFunc_car, L"($l)",
  return mkref(car(lookup(L"$l")));
)

COMPILE_PRIM_FUNC(cdr, primFunc_cdr, L"($l)",
  return mkref(cdr(lookup(L"$l")));
)

COMPILE_PRIM_FUNC(set_car, primFunc_set_car, L"($cons $val)",
  setCar(lookup(L"$cons"), lookup(L"$val"));
  return mkref(lookup(L"$val"));
)

COMPILE_PRIM_FUNC(set_cdr, primFunc_set_cdr, L"($cons $val)",
  setCdr(lookup(L"$cons"), lookup(L"$val"));
  return mkref(lookup(L"$val"));
)

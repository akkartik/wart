//// core compiled primitives

// these have access to caller scope
// params start with $ by convention to avoid shadowing

COMPILE_PRIM_FUNC(eval, primFunc_eval, "($x $scope)",
  Cell* scope = lookup("$scope");
  if (scope == nil)
    return eval(lookup("$x"), lookup("caller-scope"));
  else
    return eval(lookup("$x"), scope);
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

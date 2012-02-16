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

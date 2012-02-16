//// core compiled primitives

// these have access to caller scope
// use $vars to avoid shadowing
// they never call each other; needn't generate gensyms for $vars

COMPILE_PRIM_FUNC(eval, primFunc_eval, "($x $scope)",
  Cell* scope = lookup("$scope");
  if (scope == nil)
    return eval(lookup("$x"), lookup("caller-scope"));
  else
    return eval(lookup("$x"), scope);
)

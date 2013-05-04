//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix
//  always increment the nrefs of a single Cell along all codepaths

//// types

COMPILE_FN(type, compiledFn_type, "($x)",
  return mkref(type(lookup("$x")));
)

//// bindings

COMPILE_FN(bind, compiledFn_bind, "('$var $val)",
  newDynamicScope(lookup("$var"), lookup("$val"));
  return nil;
)

COMPILE_FN(unbind, compiledFn_unbind, "('$var)",
  Cell* var = lookup("$var");
  if (!dynamics[var].empty())
    endDynamicScope(var);
  return nil;
)

COMPILE_FN(bound?, compiledFn_isBound, "($var $scope)",
  Cell* var = lookup("$var");
  if (var == nil) return mkref(newNum(1));
  if (dynamics[var].empty()) return nil;
  return mkref(var);
)

COMPILE_FN(numBindings, compiledFn_numBindings, "($var)",
  return mkref(newNum((long)dynamics[lookup("$var")].size()));
)

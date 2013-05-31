//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix
//  always increment the nrefs of a single cell along all codepaths

//// types

COMPILE_FN(type, compiledfn_type, "($x)",
  return mkref(type(lookup("$x")));
)

//// bindings

COMPILE_FN(bind, compiledfn_bind, "('$var $val)",
  new_dynamic_scope(lookup("$var"), lookup("$val"));
  return nil;
)

COMPILE_FN(unbind, compiledfn_unbind, "('$var)",
  cell* var = lookup("$var");
  if (!Dynamics[var].empty())
    end_dynamic_scope(var);
  return nil;
)

COMPILE_FN(bound?, compiledfn_isBound, "($var $scope)",
  cell* var = lookup("$var");
  if (var == nil) return mkref(new_num(1));
  if (Dynamics[var].empty()) return nil;
  return mkref(var);
)

COMPILE_FN(num_bindings, compiledfn_num_bindings, "($var)",
  return mkref(new_num((long)Dynamics[lookup("$var")].size()));
)

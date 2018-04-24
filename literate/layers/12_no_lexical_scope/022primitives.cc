//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix
//  always increment the nrefs of a single cell along all codepaths

COMPILE_FN(fn, compiledfn_fn, "'($params ... $body)",
  cell* f = new_table();
  put(f, sym_sig, lookup("$params"));
  put(f, sym_body, lookup("$body"));
  return mkref(new_object("function", f));
)

COMPILE_FN(if, compiledfn_if, "($cond '$then '$else)",
  return lookup("$cond") != nil ? eval(lookup("$then")) : eval(lookup("$else"));
)

COMPILE_FN(not, compiledfn_not, "($x)",
  return lookup("$x") == nil ? mkref(new_num(1)) : nil;
)

COMPILE_FN(=, compiledfn_equal, "($x $y)",
  cell* x = lookup("$x");
  cell* y = lookup("$y");
  cell* result = nil;
  if (x == nil && y == nil)
    result = new_num(1);
  else if (x == nil || y == nil)
    result = nil;
  else if (x == y)
    result = x;
  else if (x->type == FLOAT || y->type == FLOAT)
    result = (equal_floats(to_float(x), to_float(y)) ? x : nil);
  else if (is_string(x) && is_string(y) && to_string(x) == to_string(y))
    result = x;
  else
    result = nil;
  return mkref(result);
)

//// types

COMPILE_FN(type, compiledfn_type, "($x)",
  return mkref(type(lookup("$x")));
)

//// bindings

COMPILE_FN(<-, compiledfn_assign, "('$var $val)",
  cell* var = lookup("$var");
  cell* val = lookup("$val");
  assign(var, val);
  return mkref(val);
)

void assign(cell* var, cell* val) {
  if (!is_sym(var)) {
    RAISE << "can't assign to non-sym " << var << '\n';
    return;
  }
  if (Dynamics[var].empty())
    new_dynamic_scope(var, val);
  else
    assign_dynamic_var(var, val);
}

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

COMPILE_FN(bound?, compiledfn_is_bound, "($var $scope)",
  cell* var = lookup("$var");
  if (var == nil) return mkref(new_num(1));
  if (Dynamics[var].empty()) return nil;
  return mkref(var);
)

COMPILE_FN(num_bindings, compiledfn_num_bindings, "($var)",
  return mkref(new_num((long)Dynamics[lookup("$var")].size()));
)

COMPILE_FN(eval, compiledfn_eval, "($x)",
  return eval(lookup("$x"));
)

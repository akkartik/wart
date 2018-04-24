//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix; user-defined functions won't have it because of implicit gensyms
//    so compiledfns never call other compiledfns
//  always increment the nrefs of a single cell along all codepaths

COMPILE_FN(fn, compiledfn_fn, "'($params ... $body)",
  cell* f = new_table();
  put(f, sym_sig, lookup("$params"));
  put(f, sym_body, lookup("$body"));
  put(f, sym_env, cdr(Curr_lexical_scope));
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

COMPILE_FN(coerce_quoted, compiledfn_coerce_quoted, "'($x $dest_type)",
  return coerce_quoted(lookup("$x"), lookup("$dest_type"), lookup(sym_Coercions));  // already mkref'd
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
  cell* scope = scope_containing_binding(var, Curr_lexical_scope);
  if (!scope)
    new_dynamic_scope(var, val);
  else if (scope == nil)
    assign_dynamic_var(var, val);
  else
    unsafe_put(scope, var, val, false);
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
  if (!scope_containing_binding(var, lookup("$scope")))
    return nil;
  return mkref(var);
)

COMPILE_FN(num_bindings, compiledfn_num_bindings, "($var)",
  return mkref(new_num((long)Dynamics[lookup("$var")].size()));
)

//// macros

COMPILE_FN(eval, compiledfn_eval, "('$x $scope)",
  In_macro.push(true);
  // sidestep eval_args for x to handle @args
  cell* x = eval(lookup("$x"), Curr_lexical_scope);
  cell* ans = eval(
    (type(x) == new_sym("incomplete_eval_data")) ? rep(x) : x,
    lookup("$scope"));
  rmref(x);
  In_macro.pop();
  return ans;
)

COMPILE_FN(mac?, compiledfn_is_macro, "($f)",
  cell* f = lookup("$f");
  return is_macro(f) ? mkref(f) : nil;
)

COMPILE_FN(uniq, compiledfn_uniq, "($x)",
  return mkref(gensym(lookup("$x")));
)

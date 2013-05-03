//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix
//  always increment the nrefs of a single Cell along all codepaths

COMPILE_FN(fn, compiledFn_fn, "'($params ... $body)",
  Cell* f = newTable();
  set(f, sym_sig, lookup("$params"));
  set(f, sym_body, lookup("$body"));
  return mkref(newObject("function", f));
)

COMPILE_FN(if, compiledFn_if, "($cond '$then '$else)",
  return lookup("$cond") != nil ? eval(lookup("$then")) : eval(lookup("$else"));
)

COMPILE_FN(not, compiledFn_not, "($x)",
  return lookup("$x") == nil ? mkref(newNum(1)) : nil;
)

COMPILE_FN(=, compiledFn_equal, "($x $y)",
  Cell* x = lookup("$x");
  Cell* y = lookup("$y");
  Cell* result = nil;
  if (x == nil && y == nil)
    result = newNum(1);
  else if (x == nil || y == nil)
    result = nil;
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

//// types

COMPILE_FN(type, compiledFn_type, "($x)",
  return mkref(type(lookup("$x")));
)

//// bindings

COMPILE_FN(<-, compiledFn_assign, "('$var $val)",
  Cell* var = lookup("$var");
  Cell* val = lookup("$val");
  assign(var, val);
  return mkref(val);
)

void assign(Cell* var, Cell* val) {
  if (!isSym(var)) {
    RAISE << "can't assign to non-sym " << var << endl;
    return;
  }
  if (dynamics[var].empty())
    newDynamicScope(var, val);
  else
    assignDynamicVar(var, val);
}

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

COMPILE_FN(eval, compiledFn_eval, "($x)",
  return eval(lookup("$x"));
)

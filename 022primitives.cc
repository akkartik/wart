//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix; user-defined functions won't have it because of implicit gensyms
//  always increment the nrefs of a single Cell along all codepaths

COMPILE_FN(fn, compiledFn_fn, "'($params ... $body)",
  Cell* f = newTable();
  set(f, sym_sig, lookup("$params"));
  set(f, sym_body, lookup("$body"));
  set(f, sym_env, cdr(currLexicalScope));
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

COMPILE_FN(coerce_quoted, compiledFn_coerce_quoted, "'($x $dest_type)",
  return coerceQuoted(lookup("$x"), lookup("$dest_type"), lookup(sym_Coercions));  // already mkref'd
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
  Cell* scope = scopeContainingBinding(var, currLexicalScope);
  if (!scope)
    newDynamicScope(var, val);
  else if (scope == nil)
    assignDynamicVar(var, val);
  else
    unsafeSet(scope, var, val, false);
}

COMPILE_FN(bind, compiledFn_bind, "('$var $val)",
  newDynamicScope(lookup("$var"), lookup("$val"));
  return nil;
)

COMPILE_FN(unbind, compiledFn_unbind, "('$var)",
  endDynamicScope(lookup("$var"));
  return nil;
)

COMPILE_FN(bound?, compiledFn_isBound, "($var $scope)",
  Cell* var = lookup("$var");
  if (var == nil) return mkref(newNum(1));
  if (!scopeContainingBinding(var, lookup("$scope")))
    return nil;
  return mkref(var);
)

COMPILE_FN(numBindings, compiledFn_numBindings, "($var)",
  return mkref(newNum((long)dynamics[lookup("$var")].size()));
)

//// macros

COMPILE_FN(eval, compiledFn_eval, "('$x $scope)",
  inMacro.push(true);
  // sidestep evalArgs for x to handle @args
  Cell* x = eval(lookup("$x"), currLexicalScope, 0);
  Cell* ans = eval(
    (type(x) == newSym("incomplete_eval_data")) ? rep(x) : x,
    lookup("$scope"),
    0);
  rmref(x);
  inMacro.pop();
  return ans;
)

COMPILE_FN(mac?, compiledFn_isMacro, "($f)",
  Cell* f = lookup("$f");
  return isMacro(f) ? mkref(f) : nil;
)

COMPILE_FN(uniq, compiledFn_uniq, "($x)",
  return mkref(genSym(lookup("$x")));
)

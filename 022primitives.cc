//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix; user-defined functions won't have it because of implicit gensyms
//  always increment the nrefs of a single Cell along all codepaths

COMPILE_FN(fn, compiledFn_fn, "'($params ... $body)",
  Cell* f = newTable();
  set(f, newSym("sig"), lookup("$params"));
  set(f, newSym("body"), lookup("$body"));
  set(f, newSym("env"), cdr(currLexicalScope));
  return mkref(newObject("function", f));
)

COMPILE_FN(eval, compiledFn_eval, "($x ... $scope)",
  Cell* scope = lookup("$scope");
  scope = (scope != nil) ? car(scope) : currLexicalScope;
  return eval(lookup("$x"), scope);
)

COMPILE_FN(if, compiledFn_if, "($cond '$then '$else)",
  return lookup("$cond") != nil ? eval(lookup("$then")) : eval(lookup("$else"));
)

COMPILE_FN(not, compiledFn_not, "($x)",
  return lookup("$x") == nil ? mkref(newNum(1)) : nil;
)

COMPILE_FN(iso, compiledFn_iso, "($x $y)",
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

// types

COMPILE_FN(type, compiledFn_type, "($x)",
  return mkref(type(lookup("$x")));
)

COMPILE_FN(coerce_quoted, compiledFn_coerce_quoted, "'($x $dest_type)",
  return coerceQuoted(lookup("$x"), lookup("$dest_type"), lookup("Coercions"));  // already mkref'd
)

// bindings

COMPILE_FN(<-, compiledFn_assign, "('$var $val)",
  Cell* var = lookup("$var");
  Cell* val = lookup("$val");
  assign(var, val);
  return mkref(val);
)

void assign(Cell* var, Cell* val) {
  Cell* scope = scopeContainingBinding(var, currLexicalScope);
  if (!scope)
    newDynamicScope(var, val);
  else if (scope == nil)
    assignDynamicVar(var, val);
  else
    unsafeSet(scope, var, val, false);
}

COMPILE_FN(dyn_bind, compiledFn_dyn_bind, "('$var $val)",
  newDynamicScope(lookup("$var"), lookup("$val"));
  return nil;
)

COMPILE_FN(dyn_unbind, compiledFn_dyn_unbind, "('$var)",
  endDynamicScope(lookup("$var"));
  return nil;
)

COMPILE_FN(bound?, compiledFn_isBound, "($var)",
  Cell* var = lookup("$var");
  if (var == nil) return mkref(newNum(1));
  if (!scopeContainingBinding(var, currLexicalScope))
    return nil;
  return mkref(var);
)

COMPILE_FN(make_unbound, compiledFn_make_unbound, "($var)",
  Cell* var = lookup("$var");
  stack<Cell*>& bindings = dynamics[var];   // unbind just in dynamic scopes
  while (!bindings.empty()) {
    rmref(var);
    rmref(bindings.top());
    bindings.pop();
  }
  return nil;
)

// macros

// eval with extra smarts for handling @args
COMPILE_FN(mac_eval, compiledFn_mac_eval, "('$x $scope)",
  inMacro.push(true);
  Cell* x = eval(lookup("$x"), currLexicalScope);
  Cell* ans = eval(x, lookup("$scope"));
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

//// core compiled primitives

// Design considered the following:
//  compiled 'if' needs access to caller scope
//  avoid accidental shadowing for params
//    so params have a '$' prefix; user-defined functions won't have it because of implicit gensyms
//  always increment the nrefs of a single Cell along all codepaths

COMPILE_FN(fn, compiledFn_fn, "'($params ... $body)",
  Cell* f = newTable();
  set(f, sym_sig, distributeQuotes(lookup("$params")));
  set(f, sym_body, lookup("$body"));
  set(f, sym_env, cdr(currLexicalScope));
  if (allQuoted2(lookup("$params")))
    set(f, sym_all_quoted, newNum(1));
  return mkref(newObject("function", f));
)

Cell* distributeQuotes(Cell* l) {
  if (!isQuoted(l)) return l;
  if (stripQuote(l) == nil) return nil;
  if (!isCons(stripQuote(l))) return l;

  Cell* pResult = newCell(), *curr = pResult;
  for (l=stripQuote(l); l != nil; l=cdr(l), curr=cdr(curr)) {
    if (car(l) == nil) {
      addCons(curr, nil);
      continue;
    }

    if (car(l) == sym_param_alias) {
      Cell* temp = mkref(newCons(sym_quote, cdr(l)));
      setCdr(curr, newCons(car(l), distributeQuotes(temp)));
      rmref(temp);
      break;
    }

    Cell* temp = mkref(newCons(sym_quote, car(l)));
    addCons(curr, distributeQuotes(temp));
    rmref(temp);
  }
  return dropPtr(pResult);
}

bool allQuoted2(Cell* sig) {
  if (isQuoted(sig)) return true;
  for (; sig != nil; sig=cdr(sig)) {
    if (isQuoted(car(sig))) continue;
    if (isCons(car(sig)) && (car(car(sig)) == sym_param_alias))
      if (allQuoted(cdr(car(sig)))) continue;
    return false;
  }
  return true;
}

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

COMPILE_FN(bound?, compiledFn_isBound, "($var)",
  Cell* var = lookup("$var");
  if (var == nil) return mkref(newNum(1));
  if (!scopeContainingBinding(var, currLexicalScope))
    return nil;
  return mkref(var);
)

COMPILE_FN(numBindings, compiledFn_numBindings, "($var)",
  return mkref(newNum((long)dynamics[lookup("$var")].size()));
)

//// macros

// eval with extra smarts for handling @args
COMPILE_FN(eval, compiledFn_eval, "('$x $scope)",
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

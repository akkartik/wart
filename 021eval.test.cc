void test_spliceArgs_works() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newCons(newNum(4), newCons(newNum(5))));   // (4 5)
  Cell* args = read("(a @b a)");
  Cell* f = read("(fn nil 3)");
  Cell* fn = eval(f);
  Cell* splicedArgs = spliceArgs(args, nil, fn);
  // (a ''4 ''5 a)
  checkEq(car(splicedArgs), newSym("a"));
  checkEq(car(car(cdr(splicedArgs))), sym_alreadyEvald);
  checkEq(cdr(car(cdr(splicedArgs))), newNum(4));
  checkEq(car(car(cdr(cdr(splicedArgs)))), sym_alreadyEvald);
  checkEq(cdr(car(cdr(cdr(splicedArgs)))), newNum(5));
  checkEq(car(cdr(cdr(cdr(splicedArgs)))), newSym("a"));
  checkEq(cdr(cdr(cdr(cdr(splicedArgs)))), nil);
  rmref(splicedArgs);
  rmref(fn);
  rmref(f);
  rmref(args);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_spliceArgs_works_with_nil() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", nil);
  Cell* args = read("(a @b a)");
  Cell* f = read("(fn nil 3)");
  Cell* fn = eval(f);
  Cell* splicedArgs = spliceArgs(args, nil, fn);
  // (a a)
  checkEq(car(splicedArgs), newSym("a"));
  checkEq(car(cdr(splicedArgs)), newSym("a"));
  checkEq(cdr(cdr(splicedArgs)), nil);
  rmref(splicedArgs);
  rmref(fn);
  rmref(f);
  rmref(args);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_spliceArgs_works_with_keywords() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newCons(newNum(4), newCons(newSym(":x"))));  // (4 :x)
  Cell* args = read("(a @b a)");
  Cell* f = read("(fn nil 3)");
  Cell* fn = eval(f);
  Cell* splicedArgs = spliceArgs(args, nil, fn);
  // (a ''4 :x a)
  checkEq(car(splicedArgs), newSym("a"));
  checkEq(car(car(cdr(splicedArgs))), sym_alreadyEvald);
  checkEq(cdr(car(cdr(splicedArgs))), newNum(4));
  checkEq(car(cdr(cdr(splicedArgs))), newSym(":x"));
  checkEq(car(cdr(cdr(cdr(splicedArgs)))), newSym("a"));
  checkEq(cdr(cdr(cdr(cdr(splicedArgs)))), nil);
  rmref(splicedArgs);
  rmref(fn);
  rmref(f);
  rmref(args);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_reorderKeywordArgs_keeps_nil_rest_args() {
  checkEq(reorderKeywordArgs(nil, newSym("a")), nil);
  Cell* params = mkref(newCons(sym_quote, newSym("a")));
  checkEq(reorderKeywordArgs(nil, params), nil);
  rmref(params);
}

void test_reorderKeywordArgs_handles_improper_lists() {
  Cell* args = mkref(newCons(newNum(3), newNum(4)));
  Cell* params = mkref(newCons(newSym("a"), newSym("b")));
  Cell* orderedArgs = reorderKeywordArgs(args, params);
  // args == orderedArgs
  checkEq(car(orderedArgs), car(args));
  checkEq(cdr(orderedArgs), cdr(args));
  rmref(orderedArgs);
  rmref(args);
  rmref(params);
}

void test_reorderKeywordArgs_handles_overlong_lists() {
  Cell* args = mkref(newCons(newNum(3), newCons(newNum(4), newCons(newNum(5)))));  // (3 4 5)
  Cell* params = mkref(newCons(newSym("a"), newCons(newSym("b"))));  // (a b)
  Cell* orderedArgs = reorderKeywordArgs(args, params);
  // args == orderedArgs
  checkEq(car(orderedArgs), car(args));
  checkEq(car(cdr(orderedArgs)), car(cdr(args)));
  checkEq(car(cdr(cdr(orderedArgs))), car(cdr(cdr(args))));
  rmref(orderedArgs);
  rmref(args);
  rmref(params);
}



void test_evalBindAll_handles_unquoted_param() {
  Cell* params = read("(x)");
  Cell* args = read("(a)");
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  checkEq(unsafeGet(newScope, "x"), newNum(3));
  rmref(scope);
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_binds_missing_params() {
  Cell* params = read("(x y)");
  Cell* args = read("(a)");
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  checkEq(unsafeGet(newScope, "x"), newNum(3));
  checkEq(unsafeGet(newScope, newSym("y")), nil);
  rmref(scope);
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_quoted_param() {
  Cell* params = read("('x)");
  Cell* args = read("(a)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(unsafeGet(newScope, "x"), newSym("a"));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_alreadyEvald_arg() {
  Cell* params = read("(x)");
  Cell* args = mkref(newCons(tagAlreadyEvald(newSym("a"))));   // (''a)
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  Cell* newScope = mkref(newTable());
  inMacro.push(true);
  evalBindAll(params, args, scope, newScope);
  inMacro.pop();
  checkEq(unsafeGet(newScope, "x"), newSym("a"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_multiply_alreadyEvald_arg() {
  Cell* params = read("(x)");
  Cell* args = mkref(newCons(tagAlreadyEvald(tagAlreadyEvald(newSym("a")))));  // (''''a)
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  Cell* newScope = mkref(newTable());
  inMacro.push(true);
  evalBindAll(params, args, scope, newScope);
  inMacro.pop();
  checkEq(unsafeGet(newScope, "x"), newSym("a"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_alreadyEvald_aliased_arg() {
  Cell* params = read("(x|y)");
  Cell* args = mkref(newCons(tagAlreadyEvald(newSym("a"))));   // (''a)
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  Cell* newScope = mkref(newTable());
  inMacro.push(true);
  evalBindAll(params, args, scope, newScope);
  inMacro.pop();
  checkEq(unsafeGet(newScope, "x"), newSym("a"));
  checkEq(unsafeGet(newScope, "y"), newSym("a"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_alreadyEvald_rest_arg() {
  Cell* params = read("x");
  Cell* args = mkref(newCons(tagAlreadyEvald(newSym("a"))));
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  Cell* newScope = mkref(newTable());
  inMacro.push(true);
  evalBindAll(params, args, scope, newScope);
  inMacro.pop();
  checkEq(car(unsafeGet(newScope, "x")), newSym("a"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_varargs_param() {
  Cell* params = read("x");
  Cell* args = read("(a b)");
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  set(scope, "b", newNum(4));
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {x: (3 4)}
  checkEq(car(unsafeGet(newScope, "x")), newNum(3));
  checkEq(car(cdr(unsafeGet(newScope, "x"))), newNum(4));
  checkEq(cdr(cdr(unsafeGet(newScope, "x"))), nil);
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_quoted_varargs_param() {
  Cell* params = read("'x");
  Cell* args = read("(a b)");
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  set(scope, "b", newNum(4));
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {x: (a b)}
  checkEq(car(unsafeGet(newScope, "x")), newSym("a"));
  checkEq(car(cdr(unsafeGet(newScope, "x"))), newSym("b"));
  checkEq(cdr(cdr(unsafeGet(newScope, "x"))), nil);
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_rest_param() {
  Cell* params = read("(x ... y)");
  Cell* args = read("(a b)");
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  set(scope, "b", newNum(4));
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {x: 3, y: (4)}
  checkEq(unsafeGet(newScope, "x"), newNum(3));
  checkEq(car(unsafeGet(newScope, "y")), newNum(4));
  checkEq(cdr(unsafeGet(newScope, "y")), nil);
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_quoted_rest_param() {
  Cell* params = read("(x ... 'y)");
  Cell* args = read("(a b)");
  Cell* scope = mkref(newTable());
  set(scope, "a", newNum(3));
  set(scope, "b", newNum(4));
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {x: 3, y: (b)}
  checkEq(unsafeGet(newScope, "x"), newNum(3));
  checkEq(car(unsafeGet(newScope, "y")), newSym("b"));
  checkEq(cdr(unsafeGet(newScope, "y")), nil);
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_destructured_params() {
  Cell* params = read("((a b))");
  Cell* args = read("(`(,x ,y))");
  Cell* scope = mkref(newTable());
  unsafeSet(scope, "x", newNum(3), false);
  unsafeSet(scope, "y", newNum(4), false);
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {a: 3, b: 4}
  checkEq(unsafeGet(newScope, "a"), newNum(3));
  checkEq(unsafeGet(newScope, "b"), newNum(4));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_param_aliases() {
  Cell* params = read("(a|b)");
  Cell* args = read("(3)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  // {a: 3, b: 3}
  checkEq(unsafeGet(newScope, "a"), newNum(3));
  checkEq(unsafeGet(newScope, "b"), newNum(3));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_only_reorders_when_necessary() {
  Cell* params = read("((a|b))");
  Cell* x = read("(3)");
  setCdr(x, x);   // cycle
  Cell* scope = mkref(newTable());
  set(scope, newSym("x"), x);
  Cell* arg = read("((fn args args) x)");   // (list x)
  Cell* args = mkref(newCons(arg));
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // should terminate
  setCdr(x, nil);
  rmref(newScope);
  rmref(args);
  rmref(arg);
  rmref(scope);
  rmref(x);
  rmref(params);
}

void test_evalBindAll_binds_as_params() {
  Cell* params = read("(a | (b c))");
  Cell* args = read("(1 2)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  // {a: (1 2), b: 1, c: 2}
  checkEq(car(unsafeGet(newScope, "a")), newNum(1));
  checkEq(car(cdr(unsafeGet(newScope, "a"))), newNum(2));
  checkEq(cdr(cdr(unsafeGet(newScope, "a"))), nil);
  checkEq(unsafeGet(newScope, "b"), newNum(1));
  checkEq(unsafeGet(newScope, "c"), newNum(2));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_binds_as_params_recursively() {
  Cell* params = read("(a | (b ... (c | (d e))))");
  Cell* args = read("(1 2 3)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  // {a: (1 2 3), b: 1, c: (2 3), d: 2, e: 3}
  checkEq(car(unsafeGet(newScope, "a")), newNum(1));
  checkEq(car(cdr(unsafeGet(newScope, "a"))), newNum(2));
  checkEq(car(cdr(cdr(unsafeGet(newScope, "a")))), newNum(3));
  checkEq(cdr(cdr(cdr(unsafeGet(newScope, "a")))), nil);
  checkEq(unsafeGet(newScope, "b"), newNum(1));
  checkEq(car(unsafeGet(newScope, "c")), newNum(2));
  checkEq(car(cdr(unsafeGet(newScope, "c"))), newNum(3));
  checkEq(cdr(cdr(unsafeGet(newScope, "c"))), nil);
  checkEq(unsafeGet(newScope, "d"), newNum(2));
  checkEq(unsafeGet(newScope, "e"), newNum(3));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_quoted_param_aliases() {
  Cell* params = read("((a | 'b))");
  Cell* args = read("(x)");
  Cell* scope = mkref(newTable());
  unsafeSet(scope, "x", newNum(3), false);
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {a: 3, b: x}
  checkEq(unsafeGet(newScope, "a"), newNum(3));
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_quoted_rest_param_aliases() {
  Cell* params = read("(a | 'b)");
  Cell* args = read("(x)");
  Cell* scope = mkref(newTable());
  unsafeSet(scope, "x", newNum(3), false);
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {a: (3), b: (x)}
  checkEq(car(unsafeGet(newScope, "a")), newNum(3));
  checkEq(car(unsafeGet(newScope, "b")), newSym("x"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_quoted_destructured_rest_param_aliases0() {
  Cell* params = read("('a | ('b))");
  Cell* args = read("(x)");
  Cell* scope = mkref(newTable());
  unsafeSet(scope, "x", newNum(3), false);
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {a: (x), b: x}
  checkEq(car(unsafeGet(newScope, "a")), newSym("x"));
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_quoted_destructured_rest_param_aliases() {
  Cell* params = read("(a | ('b))");
  Cell* args = read("(x)");
  Cell* scope = mkref(newTable());
  unsafeSet(scope, "x", newNum(3), false);
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  // {a: (3), b: x}
  checkEq(car(unsafeGet(newScope, "a")), newNum(3));
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_evals_aliases_only_when_necessary() {
  Cell* params = read("(('a | 'b))");
  Cell* args = read("(x)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(raiseCount, 0);
  // {a: x, b: x}
  checkEq(unsafeGet(newScope, "a"), newSym("x"));
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_evals_aliases_only_when_necessary2() {
  Cell* params = read("('a | ('b))");
  Cell* args = read("(x)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(raiseCount, 0);
  // {a: (x), b: x}
  checkEq(car(unsafeGet(newScope, "a")), newSym("x"));
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_evals_aliases_only_when_necessary3() {
  Cell* params = read("(| 'a ('b c))");
  Cell* args = read("(x y)");
  Cell* scope = mkref(newTable());
  unsafeSet(scope, "y", newNum(3), false);
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  checkEq(raiseCount, 0);
  // {a: (x y), b: x, c: 3}
  checkEq(car(unsafeGet(newScope, "a")), newSym("x"));
  checkEq(car(cdr(unsafeGet(newScope, "a"))), newSym("y"));
  checkEq(cdr(cdr(unsafeGet(newScope, "a"))), nil);
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  checkEq(unsafeGet(newScope, "c"), newNum(3));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_evals_aliases_only_when_necessary4() {
  Cell* params = read("((| 'a (| 'b c)))");
  Cell* args = read("(x)");
  Cell* scope = mkref(newTable());
  unsafeSet(scope, "x", newNum(3), false);
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, scope, newScope);
  checkEq(raiseCount, 0);
  // {a: x, b: x, c: 3}
  checkEq(unsafeGet(newScope, "a"), newSym("x"));
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  checkEq(unsafeGet(newScope, "c"), newNum(3));
  rmref(newScope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_evals_aliases_only_when_necessary5() {
  Cell* params = read("((| 'a (| 'b 'c)))");
  Cell* args = read("(x)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(raiseCount, 0);
  // {a: x, b: x, c: x}
  checkEq(unsafeGet(newScope, "a"), newSym("x"));
  checkEq(unsafeGet(newScope, "b"), newSym("x"));
  checkEq(unsafeGet(newScope, "c"), newSym("x"));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_evals_aliases_only_when_necessary6() {
  Cell* params = read("(| 'a (| 'b 'c))");
  Cell* args = read("(x)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(raiseCount, 0);
  // {a: (x), b: (x), c: (x)}
  checkEq(car(unsafeGet(newScope, "a")), newSym("x"));
  checkEq(car(unsafeGet(newScope, "b")), newSym("x"));
  checkEq(car(unsafeGet(newScope, "c")), newSym("x"));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

// gotcha: a|(b c) won't work
void test_evalBindAll_warns_on_unary_as() {
  Cell* params = read("(| a)");
  Cell* args = read("(1 2)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(raiseCount, 1);   raiseCount=0;
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_binds_missing_as_params_to_nil() {
  Cell* params = read("(a | (b c))");
  Cell* args = read("1");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(raiseCount, 0);
  // {a: x}
  checkEq(unsafeGet(newScope, "a"), newNum(1));
  checkEq(unsafeGet(newScope, "b"), nil);
  checkEq(unsafeGet(newScope, "c"), nil);
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_binds_alternatives_to_non_cons() {
  Cell* params = read("(a b|c)");
  Cell* args = read("(1 2)");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  checkEq(raiseCount, 0);
  // {a: 1, b: 2, c: 2}
  checkEq(unsafeGet(newScope, "a"), newNum(1));
  checkEq(unsafeGet(newScope, "b"), newNum(2));
  checkEq(unsafeGet(newScope, "c"), newNum(2));
  rmref(newScope);
  rmref(args);
  rmref(params);
}

void test_evalBindAll_handles_duplicate_destructured_aliases() {
  Cell* params = read("((a b|x) (c d|x))");
  Cell* args = read("('(1 :x 2) '(3 :x 4))");
  Cell* newScope = mkref(newTable());
  evalBindAll(params, args, nil, newScope);
  // {a: 1, b: 2, c: 3, d: 4, x: 2 or 4}
  checkEq(unsafeGet(newScope, "a"), newNum(1));
  checkEq(unsafeGet(newScope, "b"), newNum(2));
  checkEq(unsafeGet(newScope, "c"), newNum(3));
  checkEq(unsafeGet(newScope, "d"), newNum(4));
  Cell* x = unsafeGet(newScope, "x");
  check(x == newNum(2) || x == newNum(4));
  rmref(newScope);
  rmref(args);
  rmref(params);
}



Cell* processUnquotes(Cell* x, long depth) {
  return processUnquotes(x, depth, currLexicalScope);
}

void test_processUnquotes_handles_unquote() {
  newDynamicScope("a", newNum(3));
  Cell* expr = read("(,a)");
  Cell* result = processUnquotes(expr, 1);
  check(isCons(result));
  // (3)
  checkEq(car(result), newNum(3));
  checkEq(cdr(result), nil);
  rmref(result);
  rmref(expr);
  endDynamicScope("a");
}

void test_processUnquotes_handles_unquote_splice() {
  newDynamicScope("a", newCons(newNum(3)));
  Cell* expr = read("(,@a)");
  Cell* result = processUnquotes(expr, 1);
  check(isCons(result));
  // (3)
  checkEq(car(result), newNum(3));
  checkEq(cdr(result), nil);
  rmref(result);
  rmref(expr);
  endDynamicScope("a");
}

void test_processUnquotes_handles_unquote_splice_and_unquote() {
  newDynamicScope("a", newCons(newNum(3)));
  newDynamicScope("b", newCons(newNum(4)));
  Cell* expr = read("(,@a ,b)");
  Cell* result = processUnquotes(expr, 1);
  // (3 (4))
  check(isCons(result));
  checkEq(car(result), newNum(3));
  check(isCons(car(cdr(result))));
  checkEq(car(car(cdr(result))), newNum(4));
  checkEq(cdr(car(cdr(result))), nil);
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_processUnquotes_splices_copies_of_lists() {
  newDynamicScope("a", newCons(newNum(3)));
  newDynamicScope("b", newCons(newNum(4)));
  Cell* expr = read("(,@a ,b)");
  Cell* result = processUnquotes(expr, 1);
  check(isCons(result));
  check(result != lookup("a"))
  rmref(result);
  rmref(expr);
  endDynamicScope("b");
  endDynamicScope("a");
}



void test_nil_evals_to_itself() {
  Cell* expr = read("()");
  Cell* result = eval(expr);
  checkEq(result, nil);
  rmref(result);
  rmref(expr);
}

void test_num_evals_to_itself() {
  Cell* expr = read("34");
  Cell* result = eval(expr);
  checkEq(result, expr);
  rmref(result);
  rmref(expr);
}

void test_colonsym_evals_to_itself() {
  Cell* expr = read(":abc");
  Cell* result = eval(expr);
  checkEq(result, expr);
  rmref(result);
  rmref(expr);
}

void test_colon_evals() {
  Cell* expr = read(":");
  newDynamicScope(":", nil);
  Cell* result = eval(expr);
  checkEq(result, nil);
  endDynamicScope(":");
  rmref(expr);
}

void test_string_evals_to_itself() {
  Cell* expr = read("\"ac bd\"");
  Cell* result = eval(expr);
  checkEq(result, expr);
  rmref(result);
  rmref(expr);
}

void test_sym_evals_to_value() {
  newDynamicScope("a", newNum(34));
  Cell* expr = read("a");
  Cell* result = eval(expr);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(expr);
  endDynamicScope("a");
}

void test_sym_evals_to_itself() {
  newDynamicScope("a", newSym("a"));
  Cell* expr = read("a");
  Cell* result = eval(expr);
  checkEq(result, expr);
  rmref(result);
  rmref(expr);
  endDynamicScope("a");
}

void test_eval_on_incompleteEval_retries() {
  Cell* expr = read("a");
  Cell* incompleteResult = eval(expr);
  check(isObject(incompleteResult));
  checkEq(type(incompleteResult), sym_incomplete_eval);
  newDynamicScope("a", newNum(34));
  Cell* doublyEvaldResult = eval(incompleteResult);
  checkEq(doublyEvaldResult, newNum(34));
  rmref(doublyEvaldResult);
  endDynamicScope("a");
  rmref(incompleteResult);
  rmref(expr);
}

void test_object_expr_evals_to_itself() {
  Cell* expr = read("(object foo 4)");
  Cell* result = eval(expr);
  checkEq(result, expr);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_quoted_atoms() {
  Cell* expr = read("'a");
  Cell* result = eval(expr);
  checkEq(result, newSym("a"));
  rmref(result);
  rmref(expr);

  expr = read("'34");
  result = eval(expr);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(expr);
}

void test_eval_handles_quoted_lists() {
  Cell* expr = read("'(a b)");
  Cell* result = eval(expr);
  // (a b)
  checkEq(car(result), newSym("a"));
  checkEq(car(cdr(result)), newSym("b"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_backquoted_lists() {
  Cell* expr = read("`(a b)");
  Cell* result = eval(expr);
  // (a b)
  checkEq(car(result), newSym("a"));
  checkEq(car(cdr(result)), newSym("b"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_unquote() {
  Cell* expr = read("`(a ,b)");
  newDynamicScope("b", newNum(34));
  Cell* result = eval(expr);
  // (a 34)
  checkEq(car(result), newSym("a"));
  checkEq(car(cdr(result)), newNum(34));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_handles_unquote_splice() {
  Cell* expr = read("`(a ,@b)");
  Cell* val = read("(34 35)");
  newDynamicScope("b", val);
  Cell* result = eval(expr);
  // (a 34 35)
  checkEq(car(result), newSym("a"));
  checkEq(car(cdr(result)), newNum(34));
  checkEq(car(cdr(cdr(result))), newNum(35));
  checkEq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(val);
  rmref(expr);
}

void test_eval_handles_unquote_splice_of_nil() {
  Cell* expr = read("`(a ,@b 3)");
  newDynamicScope("b", nil);
  Cell* result = eval(expr);
  checkEq(cdr(nil), nil);
  // (a 3)
  checkEq(car(result), newSym("a"));
  checkEq(car(cdr(result)), newNum(3));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_quotes_quote_comma() {
  Cell* expr = read("`(a ',b)");
  newDynamicScope("b", newSym("x"));
  Cell* result = eval(expr);
  // (a 'x)
  checkEq(car(result), newSym("a"));
  check(isCons(car(cdr(result))));
  checkEq(car(car(cdr(result))), newSym("'"));
  checkEq(cdr(car(cdr(result))), newSym("x"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_evals_comma_quote() {
  Cell* expr = read("`(a ,'b)");
  newDynamicScope("b", newSym("x"));
  Cell* result = eval(expr);
  // (a b)
  checkEq(car(result), newSym("a"));
  checkEq(car(cdr(result)), newSym("b"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_handles_nested_quotes() {
  Cell* expr = read("`(,a `(,a ,,a ,,@b))");
  newDynamicScope("a", newSym("x"));
  newDynamicScope("b", newCons(newSym("x"), newCons(newSym("y"))));  // (x y)
  Cell* result = eval(expr);
  // (x `(,a x x y))
  checkEq(car(result), newSym("x"));
  Cell* nestedExpr = car(cdr(result));
  check(isCons(nestedExpr));
  checkEq(car(nestedExpr), newSym("`"));
  check(isCons(cdr(nestedExpr)));
  Cell* nestedExpr2 = cdr(nestedExpr);
  check(isCons(car(nestedExpr2)));
  checkEq(car(car(nestedExpr2)), newSym(","));
  checkEq(cdr(car(nestedExpr2)), newSym("a"));
  nestedExpr2 = cdr(nestedExpr2);
  checkEq(car(nestedExpr2), newSym("x"));
  nestedExpr2 = cdr(nestedExpr2);
  checkEq(car(nestedExpr2), newSym("x"));
  checkEq(car(cdr(nestedExpr2)), newSym("y"));
  checkEq(cdr(cdr(nestedExpr2)), nil);
  rmref(result);
  endDynamicScope("b");
  endDynamicScope("a");
  rmref(expr);
}

void test_eval_handles_quoted_destructured_params() {
  Cell* call = read("((fn ('(a b)) b) (1 2))");
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toInt(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_rest_params() {
  Cell* call = read("((fn (a b ... c) c) 1 2 3 4 5)");
  Cell* result = eval(call);
  check(isCons(result));
  check(isNum(car(result)));
  // (3 4 5)
  checkEq(toInt(car(result)), 3);
  check(isNum(car(cdr(result))));
  checkEq(toInt(car(cdr(result))), 4);
  checkEq(toInt(car(cdr(cdr(result)))), 5);
  checkEq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
}

void test_eval_handles_splice() {
  Cell* expr = read("(cons @b)");
  Cell* val = read("(3 4)");
  newDynamicScope("b", val);
  Cell* result = eval(expr);
  // (3 ... 4)
  check(isCons(result));
  checkEq(car(result), newNum(3));
  checkEq(cdr(result), newNum(4));
  rmref(result);
  endDynamicScope("b");
  rmref(val);
  rmref(expr);
}

void test_eval_handles_splice2() {
  Cell* fn = read("(fn x (cons @x))");
  Cell* def = eval(fn);
  newDynamicScope("f", def);
  Cell* call1 = read("(f 1 2)");
  Cell* result = eval(call1);
  // (1 ... 2)
  check(isCons(result));
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), newNum(2));
  rmref(result);

  Cell* call2 = read("(f 3 4)");
  result = eval(call2);
  // (3 ... 4)
  check(isCons(result));
  checkEq(car(result), newNum(3));
  checkEq(cdr(result), newNum(4));
  rmref(result);

  rmref(call2);
  rmref(call1);
  endDynamicScope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice3() {
  Cell* fn = read("(fn (x y) (cons x y))");
  Cell* def = eval(fn);
  newDynamicScope("f", def);
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* argval = read("(a b)");
  newDynamicScope("args", argval);
  Cell* call = read("(f @args)");
  Cell* result = eval(call);
  // (a ... b)
  check(isCons(result));
  checkEq(car(result), newSym("a"));
  checkEq(cdr(result), newSym("b"));
  rmref(result);
  rmref(call);
  endDynamicScope("args");
  rmref(argval);
  endDynamicScope("b");
  endDynamicScope("a");
  endDynamicScope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice4() {
  Cell* fn = read("(fn ('x y) (cons x y))");
  Cell* def = eval(fn);
  newDynamicScope("f", def);
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* argval = read("(b)");
  newDynamicScope("args", argval);
  Cell* call = read("(f a @args)");
  Cell* result = eval(call);
  // (a ... b)
  check(isCons(result));
  checkEq(car(result), newSym("a"));
  checkEq(cdr(result), newSym("b"));
  rmref(result);
  rmref(call);
  endDynamicScope("args");
  rmref(argval);
  endDynamicScope("b");
  endDynamicScope("a");
  endDynamicScope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice5() {
  Cell* fn = read("(fn (x y) (cons x y))");
  Cell* def = eval(fn);
  newDynamicScope("f", def);
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* argval = read("(b)");
  newDynamicScope("args", argval);
  Cell* call = read("(f a @args)");
  Cell* result = eval(call);
  // (3 ... b)
  check(isCons(result));
  checkEq(car(result), newNum(3));
  checkEq(cdr(result), newSym("b"));
  rmref(result);
  rmref(call);
  endDynamicScope("args");
  rmref(argval);
  endDynamicScope("b");
  endDynamicScope("a");
  endDynamicScope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice6() {
  Cell* fn = read("(fn (x 'y) (cons x y))");
  Cell* def = eval(fn);
  newDynamicScope("f", def);
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* argval = read("(a b)");
  newDynamicScope("args", argval);
  Cell* call = read("(f @args)");
  Cell* result = eval(call);
  // (a ... b)
  check(isCons(result));
  checkEq(car(result), newSym("a"));
  checkEq(cdr(result), newSym("b"));
  rmref(result);
  rmref(call);
  endDynamicScope("args");
  rmref(argval);
  endDynamicScope("b");
  endDynamicScope("a");
  endDynamicScope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_splice_on_macros_warns() {
  Cell* expr = read("(fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))");
  Cell* fn = eval(expr);
  newDynamicScope("f", fn);
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* argval = read("(a b)");
  newDynamicScope("args", argval);
  Cell* call = read("(f @args)");
  Cell* result = eval(call);
  checkEq(raiseCount, 1);   raiseCount=0;
  rmref(result);
  rmref(call);
  endDynamicScope("args");
  rmref(argval);
  endDynamicScope("b");
  endDynamicScope("a");
  endDynamicScope("f");
  rmref(fn);
  rmref(expr);
}

void test_eval_splice_on_macros_with_backquote() {
  Cell* expr = read("(fn '(x y) (eval `(cons ,x ,y) caller_scope))");
  Cell* fn = eval(expr);
  newDynamicScope("f", fn);
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* argval = read("(a b)");
  newDynamicScope("args", argval);
  Cell* call = read("(f @args)");
  Cell* result = eval(call);
  checkEq(raiseCount, 0);
  rmref(result);
  rmref(call);
  endDynamicScope("args");
  rmref(argval);
  endDynamicScope("b");
  endDynamicScope("a");
  endDynamicScope("f");
  rmref(fn);
  rmref(expr);
}

void test_eval_doesnt_modify_fn() {
  Cell* fn = read("(fn(x) (eval x))");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* oldf = copyList(f);
  Cell* call = read("(f 34)");
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(oldf);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_doesnt_modify_fn2() {
  Cell* fn = read("(fn(x) (eval x))");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* oldf = copyList(f);
  Cell* call = read("(f '(cons 3 4))");
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(oldf);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_doesnt_modify_fn3() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* fn = read("(fn(x y) `(assign ,x ,y))");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* oldf = copyList(f);
  Cell* call = read("(f a b)");
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(oldf);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_eval_doesnt_modify_fn4() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* fn = read("(fn y `(assign @,y))");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* oldf = copyList(f);
  Cell* call = read("(f a b)");
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(oldf);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_eval_handles_simple_fn() {
  Cell* expr = read("(fn () 34)");
  Cell* fn = eval(expr);
  // (object function {sig: nil, body: (34), env: nil})
  checkEq(type(fn), newSym("function"));
  checkEq(sig(fn), nil);
  check(isCons(body(fn)));
  checkEq(car(body(fn)), newNum(34));
  checkEq(env(fn), nil);
  rmref(fn);
  rmref(expr);
}

void test_eval_on_fn_is_idempotent() {
  Cell* expr = read("(fn () 34)");
  Cell* fn = eval(expr);
  Cell* fn2 = eval(fn);
  // fn == fn2
  checkEq(type(fn2), newSym("function"));
  checkEq(sig(fn2), nil);
  check(isCons(body(fn2)));
  checkEq(car(body(fn2)), newNum(34));
  checkEq(env(fn2), nil);
  rmref(fn2);
  rmref(fn);
  rmref(expr);
}

void test_eval_handles_closure() {
  Cell* expr = read("(fn () 34)");
  newLexicalScope();
    Cell* newLexicalScope = currLexicalScope;
    checkEq(newLexicalScope->nrefs, 1);
    Cell* result = eval(expr);
    checkEq(newLexicalScope->nrefs, 2);
  endLexicalScope();
  checkEq(newLexicalScope->nrefs, 1);
  // (object function {sig: nil, body: (34), env: {}})
  checkEq(type(result), newSym("function"));
  checkEq(sig(result), nil);
  checkEq(car(body(result)), newNum(34));
  checkEq(env(result), newLexicalScope);
  rmref(result);
  checkEq(newLexicalScope->nrefs, 0);
  rmref(expr);
}

void test_eval_handles_fn_calls() {
  Cell* call = read("((fn () 34))");
  Cell* result = eval(call);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(call);
}

void test_eval_expands_syms_in_fn_bodies() {
  Cell* fn = read("((fn () a))");
  newDynamicScope("a", newNum(34));
  Cell* result = eval(fn);
  checkEq(result, newNum(34));
  endDynamicScope("a");
  rmref(result);
  rmref(fn);
}

void test_eval_handles_assigned_fn_calls() {
  Cell* fn = read("(fn () 34)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
    Cell* call = read("(f)");
    Cell* result = eval(call);
    checkEq(result, newNum(34));
  endDynamicScope("f");
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
}

void test_eval_expands_lexically_scoped_syms_in_fn_bodies() {
  Cell* call = read("((fn () a))");
  newLexicalScope();
    addLexicalBinding("a", newNum(34));
    Cell* result = eval(call);
    checkEq(result, newNum(34));
  endLexicalScope();
  rmref(result);
  rmref(call);
}

void test_eval_expands_syms_in_original_lexical_scope() {
  newDynamicScope("a", newNum(23));
  Cell* fn = read("(fn () a)");
  newLexicalScope();
  addLexicalBinding("a", newNum(34));
    Cell* f = eval(fn);
    newDynamicScope("f", f);
  endLexicalScope();
  Cell* call = read("(f)");
  Cell* result = eval(call);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
  endDynamicScope("a");
}

void test_eval_expands_args_in_caller_scope() {
  newDynamicScope("a", newNum(23));
  Cell* fn = read("(fn (arg1) arg1)");
  newLexicalScope();
  addLexicalBinding("arg1", newNum(34));
    Cell* f = eval(fn);
    newDynamicScope("f", f);
  endLexicalScope();
  Cell* call = read("(f a)");
  Cell* result = eval(call);
  checkEq(result, newNum(23));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
  endDynamicScope("a");
}

void test_eval_doesnt_eval_quoted_params() {
  newDynamicScope("a", newNum(23));
  Cell* fn = read("(fn ('arg1) arg1)");
  newLexicalScope();
  addLexicalBinding("arg1", newNum(34));
    Cell* f = eval(fn);
    newDynamicScope("f", f);
  endLexicalScope();
  Cell* call = read("(f a)");
  Cell* result = eval(call);
  checkEq(result, newSym("a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
  endDynamicScope("a");
}

void test_eval_handles_quoted_param_list() {
  newDynamicScope("a", newNum(23));
  Cell* fn = read("(fn '(arg1) arg1)");
  newLexicalScope();
  addLexicalBinding("arg1", newNum(34));
    Cell* f = eval(fn);
    newDynamicScope("f", f);
  endLexicalScope();
  Cell* call = read("(f a)");
  Cell* result = eval(call);
  checkEq(result, newSym("a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
  endDynamicScope("a");
}

void test_eval_handles_multiple_args() {
  Cell* fn = read("(fn (a b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f 1 2)");
  Cell* result = eval(call);
  checkEq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_multiple_body_exprs() {
  Cell* fn = read("(fn () 1 2)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f)");
  Cell* result = eval(call);
  checkEq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_vararg_param() {
  Cell* call = read("((fn args args) 1)");
  Cell* result = eval(call);
  check(isCons(result));
  checkEq(car(result), newNum(1));
  rmref(result);
  rmref(call);
}

void test_eval_evals_args() {
  Cell* call = read("((fn (f) (f)) (fn () 34))");
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toInt(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_doesnt_leak_body_evals() {
  Cell* call = read("((fn (f) (f) (f)) (fn () 34))");
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toInt(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_handles_destructured_params() {
  Cell* call = read("((fn ((a b)) b) '(1 2))");
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toInt(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_keyword_args_for_fns() {
  Cell* fn = read("(fn (a b c) c)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :c 1 2)");
  Cell* result = eval(call);
  checkEq(result, newNum(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_keyword_args_for_fns2() {
  Cell* fn = read("(fn (a b c|x) c)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :c 1 2)");
  Cell* result = eval(call);
  checkEq(result, newNum(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_quoted_keyword_args_for_fns() {
  Cell* fn = read("(fn (a b 'c) c)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :c 1 2)");
  Cell* result = eval(call);
  checkEq(result, newNum(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_quoted_keyword_args_for_fns2() {
  Cell* fn = read("(fn '(a b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :b 1 2)");
  Cell* result = eval(call);
  checkEq(result, newNum(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_rest_keyword_arg_at_end() {
  Cell* fn = read("(fn (a ... b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f 2 :b 1 3)");
  Cell* result = eval(call);
  // (1 3)
  checkEq(car(result), newNum(1));
  checkEq(car(cdr(result)), newNum(3));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_rest_keyword_arg_at_end2() {
  Cell* fn = read("(fn (a ... b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :b 1 2 3)");
  Cell* result = eval(call);
  // (1 2 3)
  checkEq(car(result), newNum(1));
  checkEq(car(cdr(result)), newNum(2));
  checkEq(car(cdr(cdr(result))), newNum(3));
  checkEq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_args_after_rest_keyword() {
  Cell* fn = read("(fn (a|with ... b|over) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :over 1 2 :with 3)");
  Cell* result = eval(call);
  // (1 2)
  checkEq(car(result), newNum(1));
  checkEq(car(cdr(result)), newNum(2));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_quoted_rest_keyword_arg() {
  Cell* fn = read("(fn (a ... 'b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :b 1 2 3)");
  Cell* result = eval(call);
  // (1 2 3)
  checkEq(car(result), newNum(1));
  checkEq(car(cdr(result)), newNum(2));
  checkEq(car(cdr(cdr(result))), newNum(3));
  checkEq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_non_keyword_arg_colon_syms() {
  Cell* fn = read("(fn (a b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f :x 1)");
  Cell* result = eval(call);
  checkEq(result, newNum(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_body_keyword_synonym() {
  Cell* fn = read("(fn (a ... body|do) body)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f 2 :do 1 3)");
  Cell* result = eval(call);
  // (1 3)
  checkEq(car(result), newNum(1));
  checkEq(car(cdr(result)), newNum(3));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_body_keyword_synonym2() {
  Cell* fn = read("(fn (a b ... body|do) `(,a ,b ,body))");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f 2 :do 1 3)");
  Cell* result = eval(call);
  // (2 nil (1 3))
  check(isCons(result));
  checkEq(car(result), newNum(2));
  checkEq(car(cdr(result)), nil);
  check(isCons(car(cdr(cdr(result)))));
  checkEq(car(car(cdr(cdr(result)))), newNum(1));
  checkEq(car(cdr(car(cdr(cdr(result))))), newNum(3));
  checkEq(cdr(cdr(car(cdr(cdr(result))))), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_keyword_args_inside_splice() {
  Cell* fn = read("(fn (a b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f @'(3 :a 4))");
  Cell* result = eval(call);
  checkEq(result, newNum(3));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_keyword_args_inside_destructured_params() {
  Cell* fn = read("(fn ((a b)) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f '(3 :a 4))");
  Cell* result = eval(call);
  checkEq(result, newNum(3));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(fn);
  endDynamicScope("f");
}

void test_eval_handles_unknown_call() {
  Cell* expr = read("(f 3)");
  Cell* attempt1 = eval(expr);
  check(isIncompleteEval(attempt1));
  rmref(attempt1);
  rmref(expr);
}

void test_eval_handles_unknown_arg() {
  Cell* fn = read("(fn (a) a)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f a)");
  Cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f a))
  check(isIncompleteEval(attempt1));
  check(isCons(rep(attempt1)));
  checkEq(car(rep(attempt1)), f);
  checkEq(car(cdr(rep(attempt1))), newSym("a"));
  checkEq(cdr(cdr(rep(attempt1))), nil);
  rmref(attempt1);
  rmref(call);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_known_and_unknown_args() {
  Cell* fn = read("(fn (a b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  newDynamicScope("a", newNum(3));
  Cell* call = read("(f a b)");
  Cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f ''3 b))
  check(isIncompleteEval(attempt1));
  check(isCons(rep(attempt1)));
  checkEq(car(rep(attempt1)), f);
  check(isCons(car(cdr(rep(attempt1)))));
  checkEq(car(car(cdr(rep(attempt1)))), sym_alreadyEvald);
  checkEq(cdr(car(cdr(rep(attempt1)))), newNum(3));
  checkEq(car(cdr(cdr(rep(attempt1)))), newSym("b"));
  checkEq(cdr(cdr(cdr(rep(attempt1)))), nil);
  rmref(attempt1);
  rmref(call);
  endDynamicScope("a");
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_quoted_and_unknown_args() {
  Cell* fn = read("(fn ('a b) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f a b)");
  Cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f ''a b))
  check(isIncompleteEval(attempt1));
  check(isCons(rep(attempt1)));
  checkEq(car(rep(attempt1)), f);
  check(isCons(car(cdr(rep(attempt1)))));
  checkEq(car(car(cdr(rep(attempt1)))), sym_alreadyEvald);
  checkEq(cdr(car(cdr(rep(attempt1)))), newSym("a"));
  checkEq(car(cdr(cdr(rep(attempt1)))), newSym("b"));
  checkEq(cdr(cdr(cdr(rep(attempt1)))), nil);
  rmref(attempt1);
  rmref(call);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_unknown_destructured_args() {
  Cell* fn = read("(fn ((a b)) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f args)");
  Cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f args))
  check(isIncompleteEval(attempt1));
  check(isCons(rep(attempt1)));
  checkEq(car(rep(attempt1)), f);
  checkEq(car(cdr(rep(attempt1))), newSym("args"));
  checkEq(cdr(cdr(rep(attempt1))), nil);
  rmref(attempt1);
  rmref(call);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
}

void test_eval_handles_unknown_spliced_args() {
  Cell* fn = read("(fn ((a b)) b)");
  Cell* f = eval(fn);
  newDynamicScope("f", f);
  Cell* call = read("(f @args)");
  Cell* attempt1 = eval(call);
  // `(object incomplete_eval (,f @args))
  check(isIncompleteEval(attempt1));
  check(isCons(rep(attempt1)));
  checkEq(car(rep(attempt1)), f);
  Cell* args = car(cdr(rep(attempt1)));
  checkEq(car(args), sym_splice);
  checkEq(cdr(args), newSym("args"));
  rmref(attempt1);
  rmref(call);
  endDynamicScope("f");
  rmref(f);
  rmref(fn);
}

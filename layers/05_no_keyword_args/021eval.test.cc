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
  set(scope, "x", newNum(3));
  set(scope, "y", newNum(4));
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

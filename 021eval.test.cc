void test_spliceArgs_works() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newCons(newNum(4), newCons(newNum(5))));
  Cell* args = read("(a @b a)");
  Cell* f = read("fn nil 3");
  Cell* fn = eval(f);
  Cell* splicedArgs = spliceArgs(args, nil, fn);
  checkEq(car(splicedArgs), newSym("a"));
  checkEq(car(car(cdr(splicedArgs))), newSym("''"));
  checkEq(cdr(car(cdr(splicedArgs))), newNum(4));
  checkEq(car(car(cdr(cdr(splicedArgs)))), newSym("''"));
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
  Cell* f = read("fn nil 3");
  Cell* fn = eval(f);
  Cell* splicedArgs = spliceArgs(args, nil, fn);
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
  newDynamicScope("b", newCons(newNum(4), newCons(newSym(":x"))));
  Cell* args = read("(a @b a)");
  Cell* f = read("fn nil 3");
  Cell* fn = eval(f);
  Cell* splicedArgs = spliceArgs(args, nil, fn);
  checkEq(car(splicedArgs), newSym("a"));
  checkEq(car(car(cdr(splicedArgs))), newSym("''"));
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
  Cell* params = newCons(newSym("'"), newSym("a"));
  checkEq(reorderKeywordArgs(nil, params), nil);
  rmref(params);
}

void test_reorderKeywordArgs_handles_improper_lists() {
  Cell* args = newCons(newNum(3), newNum(4));
  Cell* params = newCons(newSym("a"), newSym("b"));
  Cell* orderedArgs = reorderKeywordArgs(args, params);
  checkEq(car(orderedArgs), car(args));
  checkEq(cdr(orderedArgs), cdr(args));
  rmref(orderedArgs);
  rmref(args);
  rmref(params);
}



Cell* evalArgs(Cell* args, Cell* params) {
  return evalArgs(args, params, currLexicalScope);
}

void test_evalArgs_handles_unquoted_param() {
  newDynamicScope("a", newNum(3));
  Cell* params = read("(x)");
  Cell* args = read("(a)");
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(cdr(evaldArgs), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("a");
}

void test_evalArgs_handles_quoted_param() {
  newDynamicScope("a", newNum(3));
  Cell* params = read("('x)");
  Cell* args = read("(a)");
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newSym("a"));
  checkEq(cdr(evaldArgs), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("a");
}

void test_evalArgs_handles_alreadyEvald_arg() {
  newDynamicScope("a", newNum(3));
  Cell* params = read("(x)");
  Cell* args = newCons(tagAlreadyEvald(newSym("a")));
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newSym("a"));
  checkEq(cdr(evaldArgs), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("a");
}

void test_evalArgs_handles_multiply_alreadyEvald_arg() {
  newDynamicScope("a", newNum(3));
  Cell* params = read("(x)");
  Cell* args = newCons(tagAlreadyEvald(tagAlreadyEvald(newSym("a"))));
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newSym("a"));
  checkEq(cdr(evaldArgs), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("a");
}

void test_evalArgs_handles_varargs_param() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* params = read("x");
  Cell* args = read("(a b)");
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(car(cdr(evaldArgs)), newNum(4));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_evalArgs_handles_quoted_varargs_param() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* params = read("'x");
  Cell* args = read("(a b)");
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newSym("a"));
  checkEq(car(cdr(evaldArgs)), newSym("b"));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_evalArgs_handles_rest_param() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* params = read("x ... y");
  Cell* args = read("(a b)");
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(car(cdr(evaldArgs)), newNum(4));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("b");
  endDynamicScope("a");
}

void test_evalArgs_handles_quoted_rest_param() {
  newDynamicScope("a", newNum(3));
  newDynamicScope("b", newNum(4));
  Cell* params = read("x ... 'y");
  Cell* args = read("(a b)");
  Cell* evaldArgs = evalArgs(args, params);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(car(cdr(evaldArgs)), newSym("b"));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope("b");
  endDynamicScope("a");
}



void test_bindParams_handles_vararg() {
  Cell* params = read("a");
  Cell* args = read("(1)");
  newLexicalScope();
  bindParams(params, args);
  Cell* result = unsafeGet(currLexicalScope, newSym("a"));
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), nil);
  endLexicalScope();
  rmref(args);
  rmref(params);
}

void test_bindParams_binds_multiple_params() {
  Cell* params = read("(a|b)");
  Cell* args = read("(1)");
  newLexicalScope();
  bindParams(params, args);
  checkEq(unsafeGet(currLexicalScope, newSym("a")), newNum(1));
  checkEq(unsafeGet(currLexicalScope, newSym("b")), newNum(1));
  endLexicalScope();
  rmref(args);
  rmref(params);
}

void test_bindParams_binds_as_params() {
  Cell* params = read("(a | (b c))");
  Cell* args = read("(1 2)");
  newLexicalScope();
  bindParams(params, args);
  checkEq(car(unsafeGet(currLexicalScope, newSym("a"))), newNum(1));
  checkEq(car(cdr(unsafeGet(currLexicalScope, newSym("a")))), newNum(2));
  checkEq(unsafeGet(currLexicalScope, newSym("b")), newNum(1));
  checkEq(unsafeGet(currLexicalScope, newSym("c")), newNum(2));
  endLexicalScope();
  rmref(args);
  rmref(params);
}

void test_bindParams_warns_on_unary_as() {
  Cell* params = read("(| a)");
  Cell* args = read("(1 2)");
  newLexicalScope();
  bindParams(params, args);
  checkEq(raiseCount, 1);   raiseCount=0;
  endLexicalScope();
  rmref(args);
  rmref(params);
}

void test_bindParams_skips_missing_as_params() {
  Cell* params = read("(a | (b c))");
  Cell* args = read("1");
  newLexicalScope();
  bindParams(params, args);
  checkEq(raiseCount, 0);
  checkEq(unsafeGet(currLexicalScope, newSym("a")), newNum(1));
  check(!unsafeGet(currLexicalScope, newSym("b")));
  check(!unsafeGet(currLexicalScope, newSym("c")));
  endLexicalScope();
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

void test_object_expr_evals_to_itself() {
  Cell* expr = read("object foo 4");
  Cell* result = eval(expr);
  checkEq(result, expr);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_quoted_atoms() {
  stringstream s("'a\n'34");
  Cell* expr = read(s);
  Cell* result = eval(expr);
  checkEq(result, newSym("a"));
  rmref(result);
  rmref(expr);
  expr = read(s);
  result = eval(expr);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(expr);
}

void test_eval_handles_quoted_lists() {
  Cell* expr = read("'(a b)");
  Cell* result = eval(expr);
  checkEq(car(rep(result)), newSym("a"));
  checkEq(car(cdr(rep(result))), newSym("b"));
  checkEq(cdr(cdr(rep(result))), nil);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_backquoted_lists() {
  Cell* expr = read("`(a b)");
  Cell* result = eval(expr);
  checkEq(car(rep(result)), newSym("a"));
  checkEq(car(cdr(rep(result))), newSym("b"));
  checkEq(cdr(cdr(rep(result))), nil);
  rmref(result);
  rmref(expr);
}

void test_eval_handles_unquote() {
  Cell* expr = read("`(a ,b)");
  newDynamicScope("b", newNum(34));
  Cell* result = eval(expr);
  checkEq(car(rep(result)), newSym("a"));
  checkEq(car(cdr(rep(result))), newNum(34));
  checkEq(cdr(cdr(rep(result))), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_handles_unquote_splice() {
  Cell* expr = read("`(a ,@b)");
  Cell* val = read("34 35");
  newDynamicScope("b", val);
  Cell* result = eval(expr);
  checkEq(car(rep(result)), newSym("a"));
  checkEq(car(cdr(rep(result))), newNum(34));
  checkEq(car(cdr(cdr(rep(result)))), newNum(35));
  checkEq(cdr(cdr(cdr(rep(result)))), nil);
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
  checkEq(car(rep(result)), newSym("a"));
  checkEq(car(cdr(rep(result))), newNum(3));
  checkEq(cdr(cdr(rep(result))), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_quotes_quote_comma() {
  Cell* expr = read("`(a ',b)");
  newDynamicScope("b", newSym("x"));
  Cell* result = eval(expr);
  checkEq(car(rep(result)), newSym("a"));
  check(isCons(car(cdr(rep(result)))));
  checkEq(car(car(cdr(rep(result)))), newSym("'"));
  checkEq(cdr(car(cdr(rep(result)))), newSym("x"));
  checkEq(cdr(cdr(rep(result))), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_evals_comma_quote() {
  Cell* expr = read("`(a ,'b)");
  newDynamicScope("b", newSym("x"));
  Cell* result = eval(expr);
  checkEq(car(rep(result)), newSym("a"));
  checkEq(car(cdr(rep(result))), newSym("b"));
  checkEq(cdr(cdr(rep(result))), nil);
  rmref(result);
  endDynamicScope("b");
  rmref(expr);
}

void test_eval_handles_nested_quotes() {
  Cell* expr = read("`(,a `(,a ,,a ,,@b))");
  newDynamicScope("a", newSym("x"));
  newDynamicScope("b", newCons(newSym("x"), newCons(newSym("y"), nil)));
  Cell* result = eval(expr);
  checkEq(car(rep(result)), newSym("x"));
  Cell* nestedExpr = car(cdr(rep(result)));
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
  check(isNum(car(rep(result))));
  checkEq(toInt(car(rep(result))), 3);
  check(isNum(car(cdr(rep(result)))));
  checkEq(toInt(car(cdr(rep(result)))), 4);
  checkEq(toInt(car(cdr(cdr(rep(result))))), 5);
  checkEq(cdr(cdr(cdr(rep(result)))), nil);
  rmref(result);
  rmref(call);
}

void test_eval_handles_splice() {
  Cell* expr = read("(cons @b)");
  Cell* val = read("3 4");
  newDynamicScope("b", val);
  Cell* result = eval(expr);
  check(isCons(result));
  checkEq(car(rep(result)), newNum(3));
  checkEq(cdr(rep(result)), newNum(4));
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
  check(isCons(result));
  checkEq(car(rep(result)), newNum(1));
  checkEq(cdr(rep(result)), newNum(2));
  rmref(result);

  Cell* call2 = read("(f 3 4)");
  result = eval(call2);
  check(isCons(result));
  checkEq(car(rep(result)), newNum(3));
  checkEq(cdr(rep(result)), newNum(4));
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
  check(isCons(result));
  checkEq(car(rep(result)), newSym("a"));
  checkEq(cdr(rep(result)), newSym("b"));
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
  check(isCons(result));
  checkEq(car(rep(result)), newSym("a"));
  checkEq(cdr(rep(result)), newSym("b"));
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
  check(isCons(result));
  checkEq(car(rep(result)), newNum(3));
  checkEq(cdr(rep(result)), newSym("b"));
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
  check(isCons(result));
  checkEq(car(rep(result)), newSym("a"));
  checkEq(cdr(rep(result)), newSym("b"));
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
  Cell* expr = read("(fn '(x y) (mac_eval (cons 'cons (cons x (cons y nil))) caller_scope))");
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
  Cell* expr = read("(fn '(x y) (mac_eval `(cons ,x ,y) caller_scope))");
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
  checkEq(car(rep(result)), newNum(1));
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
  checkEq(car(rep(result)), newNum(1));
  checkEq(car(cdr(rep(result))), newNum(3));
  checkEq(cdr(cdr(rep(result))), nil);
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
  checkEq(car(rep(result)), newNum(1));
  checkEq(car(cdr(rep(result))), newNum(2));
  checkEq(car(cdr(cdr(rep(result)))), newNum(3));
  checkEq(cdr(cdr(cdr(rep(result)))), nil);
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
  checkEq(car(rep(result)), newNum(1));
  checkEq(car(cdr(rep(result))), newNum(2));
  checkEq(car(cdr(cdr(rep(result)))), newNum(3));
  checkEq(cdr(cdr(cdr(rep(result)))), nil);
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
  checkEq(car(rep(result)), newNum(1));
  checkEq(car(cdr(rep(result))), newNum(3));
  checkEq(cdr(cdr(rep(result))), nil);
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
  check(isCons(result));
  checkEq(car(rep(result)), newNum(2));
  checkEq(car(cdr(rep(result))), nil);
  check(isCons(car(cdr(cdr(rep(result))))));
  checkEq(car(car(cdr(cdr(rep(result))))), newNum(1));
  checkEq(car(cdr(car(cdr(cdr(rep(result)))))), newNum(3));
  checkEq(cdr(cdr(car(cdr(cdr(rep(result)))))), nil);
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

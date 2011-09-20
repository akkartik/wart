void test_evalArgs_handles_unquoted_param() {
  newDynamicScope(L"a", newNum(3));
  Cell* params = mkref(wartRead(stream(L"(x)")).front());
  Cell* args = mkref(wartRead(stream(L"(a)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(cdr(evaldArgs), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"a");
}

void test_evalArgs_handles_quoted_param() {
  newDynamicScope(L"a", newNum(3));
  Cell* params = mkref(wartRead(stream(L"('x)")).front());
  Cell* args = mkref(wartRead(stream(L"(a)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newSym(L"a"));
  checkEq(cdr(evaldArgs), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"a");
}

void test_evalArgs_handles_varargs_param() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newNum(4));
  Cell* params = mkref(wartRead(stream(L"x")).front());
  Cell* args = mkref(wartRead(stream(L"(a b)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(car(cdr(evaldArgs)), newNum(4));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_evalArgs_handles_quoted_varargs_param() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newNum(4));
  Cell* params = mkref(wartRead(stream(L"'x")).front());
  Cell* args = mkref(wartRead(stream(L"(a b)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newSym(L"a"));
  checkEq(car(cdr(evaldArgs)), newSym(L"b"));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_evalArgs_handles_rest_param() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newNum(4));
  Cell* params = mkref(wartRead(stream(L"x . y")).front());
  Cell* args = mkref(wartRead(stream(L"(a b)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(car(cdr(evaldArgs)), newNum(4));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_evalArgs_handles_quoted_rest_param() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newNum(4));
  Cell* params = mkref(wartRead(stream(L"x . 'y")).front());
  Cell* args = mkref(wartRead(stream(L"(a b)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(car(cdr(evaldArgs)), newSym(L"b"));
  checkEq(cdr(cdr(evaldArgs)), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_evalArgs_handles_spliced_vararg_arg() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newCons(newNum(4), newCons(newNum(5), nil)));
  Cell* params = mkref(wartRead(stream(L"'x")).front());
  Cell* args = mkref(wartRead(stream(L"(a @b)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newSym(L"a"));
  checkEq(car(cdr(evaldArgs)), newNum(4));
  checkEq(car(cdr(cdr(evaldArgs))), newNum(5));
  checkEq(cdr(cdr(cdr(evaldArgs))), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_evalArgs_handles_spliced_arg() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newCons(newNum(4), newCons(newNum(5), nil)));
  Cell* params = mkref(wartRead(stream(L"(x y 'z)")).front());
  Cell* args = mkref(wartRead(stream(L"(a @b)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newNum(3));
  checkEq(car(cdr(evaldArgs)), newNum(4));
  checkEq(car(cdr(cdr(evaldArgs))), newNum(5));
  checkEq(cdr(cdr(cdr(evaldArgs))), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_evalArgs_skips_spliced_args_of_nil() {
  newDynamicScope(L"a", nil);
  newDynamicScope(L"b", newNum(4));
  Cell* params = mkref(wartRead(stream(L"x 'y")).front());
  Cell* args = mkref(wartRead(stream(L"(@a b)")).front());
  Cell* evaldArgs = evalArgs(params, args);
  checkEq(car(evaldArgs), newNum(4));
  checkEq(cdr(evaldArgs), nil);
  rmref(evaldArgs);
  rmref(args);
  rmref(params);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}



void test_bindArgs_handles_vararg() {
  Cell* params = wartRead(stream(L"a")).front();
  Cell* args = wartRead(stream(L"(1)")).front();
  newLexicalScope();
  bindArgs(params, args);
  Cell* result = unsafeGet(currLexicalScopes.top(), newSym(L"a"));
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), nil);
  endLexicalScope();
  rmref(params);
}



void test_nil_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L"()"));
  checkEq(cells.size(), 1);
  Cell* result = eval(cells.front());
  checkEq(result, nil);
  rmref(result);
  rmref(cells.front());
}

void test_num_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L"34"));
  checkEq(cells.size(), 1);
  Cell* result = eval(cells.front());
  checkEq(result, cells.front());
  rmref(result);
  rmref(cells.front());
}

void test_colonsym_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L":abc"));
  checkEq(cells.size(), 1);
  Cell* result = eval(cells.front());
  checkEq(result, cells.front());
  rmref(result);
  rmref(cells.front());
}

void test_string_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L"\"ac bd\""));
  checkEq(cells.size(), 1);
  Cell* result = eval(cells.front());
  checkEq(result, cells.front());
  rmref(result);
  // HACK: just a string by itself doesn't need rmref'ing
}

void test_eval_handles_quoted_atoms() {
  list<Cell*> cells = wartRead(stream(L"'a\n'34"));
  checkEq(cells.size(), 2);
  Cell* result = eval(cells.front());
  checkEq(result, newSym(L"a"));
  rmref(result);
  result = eval(cells.back());
  checkEq(result, newNum(34));
  rmref(result);
  rmref(cells.front());
  rmref(cells.back());
}

void test_eval_handles_quoted_lists() {
  list<Cell*> cells = wartRead(stream(L"'(a b)"));
  Cell* result = eval(cells.front());
  checkEq(car(result), newSym(L"a"));
  checkEq(car(cdr(result)), newSym(L"b"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(cells.front());
}

void test_eval_handles_backquoted_lists() {
  list<Cell*> cells = wartRead(stream(L"`(a b)"));
  Cell* result = eval(cells.front());
  checkEq(car(result), newSym(L"a"));
  checkEq(car(cdr(result)), newSym(L"b"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(cells.front());
}

void test_eval_handles_unquote() {
  list<Cell*> cells = wartRead(stream(L"`(a ,b)"));
  newDynamicScope(L"b", newNum(34));
  Cell* result = eval(cells.front());
  checkEq(car(result), newSym(L"a"));
  checkEq(car(cdr(result)), newNum(34));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_unquote_splice() {
  list<Cell*> cells = wartRead(stream(L"`(a ,@b)"));
  newDynamicScope(L"b", wartRead(stream(L"34 35")).front());
  Cell* result = eval(cells.front());
  checkEq(car(result), newSym(L"a"));
  checkEq(car(cdr(result)), newNum(34));
  checkEq(car(cdr(cdr(result))), newNum(35));
  checkEq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_unquote_splice_of_nil() {
  list<Cell*> cells = wartRead(stream(L"`(a ,@b 3)"));
  newDynamicScope(L"b", nil);
  Cell* result = eval(cells.front());
  checkEq(cdr(nil), nil);
  checkEq(car(result), newSym(L"a"));
  checkEq(car(cdr(result)), newNum(3));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_quotes_quote_comma() {
  list<Cell*> cells = wartRead(stream(L"`(a ',b)"));
  newDynamicScope(L"b", newSym(L"x"));
  Cell* result = eval(cells.front());
  checkEq(car(result), newSym(L"a"));
  check(isCons(car(cdr(result))));
  checkEq(car(car(cdr(result))), newSym(L"'"));
  checkEq(cdr(car(cdr(result))), newSym(L"x"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_evals_comma_quote() {
  list<Cell*> cells = wartRead(stream(L"`(a ,'b)"));
  newDynamicScope(L"b", newSym(L"x"));
  Cell* result = eval(cells.front());
  checkEq(car(result), newSym(L"a"));
  checkEq(car(cdr(result)), newSym(L"b"));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_nested_quotes() {
  Cell* expr = wartRead(stream(L"`(,a `(,a ,,a))")).front();
  newDynamicScope(L"a", newSym(L"x"));
  Cell* result = eval(expr);
  checkEq(car(result), newSym(L"x"));
  Cell* nestedExpr = car(cdr(result));
  check(isCons(nestedExpr));
  checkEq(car(nestedExpr), newSym(L"`"));
  check(isCons(cdr(nestedExpr)));
  Cell* nestedExpr2 = cdr(nestedExpr);
  check(isCons(car(nestedExpr2)));
  checkEq(car(car(nestedExpr2)), newSym(L","));
  checkEq(cdr(car(nestedExpr2)), newSym(L"a"));
  nestedExpr2 = cdr(nestedExpr2);
  checkEq(car(nestedExpr2), newSym(L"x"));
  checkEq(cdr(nestedExpr2), nil);
  rmref(result);
  endDynamicScope(L"a");
  rmref(expr);
}

void test_eval_handles_quoted_destructured_params() {
  Cell* call = wartRead(stream(L"((lambda ('(a b)) b) (1 2))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toNum(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_rest_params() {
  Cell* call = wartRead(stream(L"((lambda (a b . c) c) 1 2 3 4 5)")).front();
  Cell* result = eval(call);
  check(isCons(result));
  check(isNum(car(result)));
  checkEq(toNum(car(result)), 3);
  check(isNum(car(cdr(result))));
  checkEq(toNum(car(cdr(result))), 4);
  checkEq(toNum(car(cdr(cdr(result)))), 5);
  checkEq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
}

void test_eval_handles_splice() {
  list<Cell*> cells = wartRead(stream(L"(+ @b)"));
  newDynamicScope(L"b", wartRead(stream(L"3 4")).front());
  Cell* result = eval(cells.front());
  checkEq(result, newNum(7));
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_splice2() {
  Cell* lambda = wartRead(stream(L"(lambda x (cons @x))")).front();
  Cell* def = eval(lambda);
  newDynamicScope(L"f", def);
  Cell* call1 = wartRead(stream(L"(f 1 2)")).front();
  Cell* result = eval(call1);
  check(isCons(result));
  checkEq(car(result), newNum(1));
  checkEq(cdr(result), newNum(2));
  rmref(result);

  Cell* call2 = wartRead(stream(L"(f 3 4)")).front();
  result = eval(call2);
  check(isCons(result));
  checkEq(car(result), newNum(3));
  checkEq(cdr(result), newNum(4));
  rmref(result);

  rmref(call2);
  rmref(call1);
  endDynamicScope(L"f");
  rmref(def);
  rmref(lambda);
}

void test_eval_handles_splice3() {
  Cell* lambda = wartRead(stream(L"(lambda (x 'y) (cons x y))")).front();
  Cell* def = eval(lambda);
  newDynamicScope(L"f", def);
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newNum(4));
  newDynamicScope(L"args", wartRead(stream(L"(a b)")).front());
  Cell* call = wartRead(stream(L"(f @args)")).front();
  Cell* result = eval(call);
  check(isCons(result));
  checkEq(car(result), newSym(L"a"));
  checkEq(cdr(result), newSym(L"b"));
  rmref(result);
  rmref(call);
  endDynamicScope(L"args");
  endDynamicScope(L"b");
  endDynamicScope(L"a");
  endDynamicScope(L"f");
  rmref(def);
  rmref(lambda);
}

                                  Cell* copyList(Cell* x) {
                                    if (!isCons(x)) return x;
                                    return newCons(copyList(car(x)),
                                        copyList(cdr(x)));
                                  }

                                  bool equalList(Cell* a, Cell* b) {
                                    if (!isCons(a)) return a == b;
                                    return equalList(car(a), car(b))
                                        && equalList(cdr(a), cdr(b));
                                  }

void test_eval_doesnt_modify_lambda() {
  Cell* lambda = wartRead(stream(L"(lambda(x) (eval x))")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* oldf = copyList(f);
  Cell* call = wartRead(stream(L"(f 34)")).front();
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(oldf);
  rmref(lambda);
  endDynamicScope(L"f");
}

void test_eval_doesnt_modify_lambda2() {
  Cell* lambda = wartRead(stream(L"(lambda(x) (eval x))")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* oldf = copyList(f);
  Cell* call = wartRead(stream(L"(f '(cons 3 4))")).front();
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(oldf);
  rmref(lambda);
  endDynamicScope(L"f");
}

void test_eval_doesnt_modify_lambda3() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newNum(4));
  Cell* lambda = wartRead(stream(L"(lambda(x y) `(assign ,x ,y))")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* oldf = copyList(f);
  Cell* call = wartRead(stream(L"(f a b)")).front();
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(oldf);
  endDynamicScope(L"f");
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_eval_doesnt_modify_lambda4() {
  newDynamicScope(L"a", newNum(3));
  newDynamicScope(L"b", newNum(4));
  Cell* lambda = wartRead(stream(L"(lambda y `(assign @,y))")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* oldf = copyList(f);
  Cell* call = wartRead(stream(L"(f a b)")).front();
  Cell* result = eval(call);
  check(equalList(f, oldf));
  rmref(result);
  rmref(call);
  rmref(oldf);
  endDynamicScope(L"f");
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"b");
  endDynamicScope(L"a");
}

void test_eval_handles_simple_lambda() {
  list<Cell*> cells = wartRead(stream(L"(lambda () 34)"));
  checkEq(cells.size(), 1);
  Cell* lambda = eval(cells.front());
  checkEq(car(lambda), newSym(L"evald-lambda"));
  checkEq(sig(lambda), nil);
  check(isCons(calleeBody(lambda)));
  checkEq(car(calleeBody(lambda)), newNum(34));
  checkEq(calleeEnv(lambda), newSym(L"dynamicScope"));
  rmref(lambda);
  rmref(cells.front());
}

void test_eval_on_lambda_is_idempotent() {
  list<Cell*> cells = wartRead(stream(L"(lambda () 34)"));
  checkEq(cells.size(), 1);
  Cell* lambda = eval(cells.front());
  Cell* lambda2 = eval(lambda);
  checkEq(car(lambda2), newSym(L"evald-lambda"));
  checkEq(sig(lambda2), nil);
  check(isCons(calleeBody(lambda2)));
  checkEq(car(calleeBody(lambda2)), newNum(34));
  checkEq(calleeEnv(lambda2), newSym(L"dynamicScope"));
  rmref(lambda2);
  rmref(lambda);
  rmref(cells.front());
}

void test_eval_handles_closure() {
  list<Cell*> cells = wartRead(stream(L"(lambda () 34)"));
  checkEq(cells.size(), 1);
  newLexicalScope();
    Cell* newLexicalScope = currLexicalScopes.top();
    checkEq(newLexicalScope->nrefs, 1);
    Cell* result = eval(cells.front());
    checkEq(newLexicalScope->nrefs, 2);
  endLexicalScope();
  checkEq(newLexicalScope->nrefs, 1);
  checkEq(car(result), newSym(L"evald-lambda"));
  checkEq(car(cdr(result)), nil);
  checkEq(car(car(cdr(cdr(result)))), newNum(34));
  checkEq(cdr(cdr(cdr(result))), newLexicalScope);
  rmref(result);
  checkEq(newLexicalScope->nrefs, 0);
  rmref(cells.front());
}

void test_eval_handles_lambda_calls() {
  Cell* call = wartRead(stream(L"((lambda () 34))")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(call);
}

void test_eval_expands_syms_in_lambda_bodies() {
  Cell* lambda = wartRead(stream(L"((lambda () a))")).front();
  newDynamicScope(L"a", newNum(34));
  Cell* result = eval(lambda);
  checkEq(result, newNum(34));
  endDynamicScope(L"a");
  rmref(result);
  rmref(lambda);
}

void test_eval_handles_assigned_lambda_calls() {
  Cell* lambda = wartRead(stream(L"(lambda () 34)")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
    Cell* call = wartRead(stream(L"(f)")).front();
    Cell* result = eval(call);
    checkEq(result, newNum(34));
  endDynamicScope(L"f");
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
}

void test_eval_expands_lexically_scoped_syms_in_lambda_bodies() {
  Cell* call = wartRead(stream(L"((lambda () a))")).front();
  newLexicalScope();
    addLexicalBinding(newSym(L"a"), newNum(34));
    Cell* result = eval(call);
    checkEq(result, newNum(34));
  endLexicalScope();
  rmref(result);
  rmref(call);
}

void test_eval_expands_syms_in_original_lexical_scope() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = wartRead(stream(L"(lambda () a)")).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"a"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = wartRead(stream(L"(f)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(34));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
  endDynamicScope(L"a");
}

void test_eval_expands_args_in_caller_scope() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = wartRead(stream(L"(lambda (arg1) arg1)")).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = wartRead(stream(L"(f a)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(23));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
  endDynamicScope(L"a");
}

void test_eval_doesnt_eval_quoted_params() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = wartRead(stream(L"(lambda ('arg1) arg1)")).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = wartRead(stream(L"(f a)")).front();
  Cell* result = eval(call);
  checkEq(result, newSym(L"a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
  endDynamicScope(L"a");
}

void test_eval_handles_quoted_param_list() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = wartRead(stream(L"(lambda '(arg1) arg1)")).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = wartRead(stream(L"(f a)")).front();
  Cell* result = eval(call);
  checkEq(result, newSym(L"a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
  endDynamicScope(L"a");
}

void test_eval_handles_multiple_args() {
  Cell* lambda = wartRead(stream(L"(lambda (a b) b)")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = wartRead(stream(L"(f 1 2)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
}

void test_eval_handles_multiple_body_exprs() {
  Cell* lambda = wartRead(stream(L"(lambda () 1 2)")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = wartRead(stream(L"(f)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
}

void test_eval_handles_vararg_param() {
  Cell* call = wartRead(stream(L"((lambda args args) 1)")).front();
  Cell* result = eval(call);
  check(isCons(result));
  checkEq(car(result), newNum(1));
  rmref(result);
  rmref(call);
}

void test_eval_evals_args() {
  Cell* call = wartRead(stream(L"((lambda (f) (f)) (lambda () 34))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toNum(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_doesnt_leak_body_evals() {
  Cell* call = wartRead(stream(L"((lambda (f) (f) (f)) (lambda () 34))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toNum(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_handles_destructured_params() {
  Cell* call = wartRead(stream(L"((lambda ((a b)) b) '(1 2))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  checkEq(toNum(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_keyword_args_for_lambdas() {
  Cell* lambda = wartRead(stream(L"(lambda (a b) b)")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = wartRead(stream(L"(f :b 1 2)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
}

void test_eval_handles_rest_keyword_arg_at_end() {
  Cell* lambda = wartRead(stream(L"(lambda (a . b) b)")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = wartRead(stream(L"(f 2 :b 1 3)")).front();
  Cell* result = eval(call);
  checkEq(car(result), newNum(1));
  checkEq(car(cdr(result)), newNum(3));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
}

void test_eval_handles_non_keyword_arg_colon_syms() {
  Cell* lambda = wartRead(stream(L"(lambda (a b) b)")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = wartRead(stream(L"(f :x 1)")).front();
  Cell* result = eval(call);
  checkEq(result, newNum(1));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
}

void test_eval_handles_body_keyword_synonym() {
  Cell* lambda = wartRead(stream(L"(lambda (a . body) body)")).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = wartRead(stream(L"(f 2 :do 1 3)")).front();
  Cell* result = eval(call);
  checkEq(car(result), newNum(1));
  checkEq(car(cdr(result)), newNum(3));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(L"f");
}

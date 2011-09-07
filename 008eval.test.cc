void test_bindArgs_handles_vararg() {
  Cell* params = wartRead(stream(L"a")).front();
  Cell* args = wartRead(stream(L"(1)")).front();
  newLexicalScope();
  bindArgs(params, args);
  Cell* result = unsafeGet(currLexicalScopes.top(), newSym(L"a"));
  check_eq(car(result), newNum(1));
  check_eq(cdr(result), nil);
  endLexicalScope();
  rmref(params);
}



void test_nil_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L"()"));
  check_eq(cells.size(), 1);
  Cell* result = eval(cells.front());
  check_eq(result, nil);
  rmref(result);
  rmref(cells.front());
}

void test_num_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L"34"));
  check_eq(cells.size(), 1);
  Cell* result = eval(cells.front());
  check_eq(result, cells.front());
  rmref(result);
  rmref(cells.front());
}

void test_colonsym_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L":abc"));
  check_eq(cells.size(), 1);
  Cell* result = eval(cells.front());
  check_eq(result, cells.front());
  rmref(result);
  rmref(cells.front());
}

void test_string_evals_to_itself() {
  list<Cell*> cells = wartRead(stream(L"\"ac bd\""));
  check_eq(cells.size(), 1);
  Cell* result = eval(cells.front());
  check_eq(result, cells.front());
  rmref(result);
  // HACK: just a string by itself doesn't need rmref'ing
}

void test_eval_handles_quoted_atoms() {
  list<Cell*> cells = wartRead(stream(L"'a\n'34"));
  check_eq(cells.size(), 2);
  Cell* result = eval(cells.front());
  check_eq(result, newSym(L"a"));
  rmref(result);
  result = eval(cells.back());
  check_eq(result, newNum(34));
  rmref(result);
  rmref(cells.front());
  rmref(cells.back());
}

void test_eval_handles_quoted_lists() {
  list<Cell*> cells = wartRead(stream(L"'(a b)"));
  Cell* result = eval(cells.front());
  check_eq(car(result), newSym(L"a"));
  check_eq(car(cdr(result)), newSym(L"b"));
  check_eq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(cells.front());
}

void test_eval_handles_backquoted_lists() {
  list<Cell*> cells = wartRead(stream(L"`(a b)"));
  Cell* result = eval(cells.front());
  check_eq(car(result), newSym(L"a"));
  check_eq(car(cdr(result)), newSym(L"b"));
  check_eq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(cells.front());
}

void test_eval_handles_unquote() {
  list<Cell*> cells = wartRead(stream(L"`(a ,b)"));
  newDynamicScope(L"b", newNum(34));
  Cell* result = eval(cells.front());
  check_eq(car(result), newSym(L"a"));
  check_eq(car(cdr(result)), newNum(34));
  check_eq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_splice() {
  list<Cell*> cells = wartRead(stream(L"`(a ,@b)"));
  newDynamicScope(L"b", wartRead(stream(L"34 35")).front());
  Cell* result = eval(cells.front());
  check_eq(car(result), newSym(L"a"));
  check_eq(car(cdr(result)), newNum(34));
  check_eq(car(cdr(cdr(result))), newNum(35));
  check_eq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_splice2() {
  list<Cell*> cells = wartRead(stream(L"(add @b)"));
  newDynamicScope(L"b", wartRead(stream(L"3 4")).front());
  Cell* result = eval(cells.front());
  check_eq(result, newNum(7));
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_splice_of_nil() {
  list<Cell*> cells = wartRead(stream(L"`(a ,@b 3)"));
  newDynamicScope(L"b", nil);
  Cell* result = eval(cells.front());
  check_eq(cdr(nil), nil);
  check_eq(car(result), newSym(L"a"));
  check_eq(car(cdr(result)), newNum(3));
  check_eq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_quotes_quote_comma() {
  list<Cell*> cells = wartRead(stream(L"`(a ',b)"));
  newDynamicScope(L"b", newSym(L"x"));
  Cell* result = eval(cells.front());
  check_eq(car(result), newSym(L"a"));
  check(isCons(car(cdr(result))));
  check_eq(car(car(cdr(result))), newSym(L"'"));
  check_eq(cdr(car(cdr(result))), newSym(L"x"));
  check_eq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_evals_comma_quote() {
  list<Cell*> cells = wartRead(stream(L"`(a ,'b)"));
  newDynamicScope(L"b", newSym(L"x"));
  Cell* result = eval(cells.front());
  check_eq(car(result), newSym(L"a"));
  check_eq(car(cdr(result)), newSym(L"b"));
  check_eq(cdr(cdr(result)), nil);
  rmref(result);
  endDynamicScope(L"b");
  rmref(cells.front());
}

void test_eval_handles_simple_lambda() {
  list<Cell*> cells = wartRead(stream(L"(lambda () 34)"));
  check_eq(cells.size(), 1);
  Cell* lambda = eval(cells.front());
  check_eq(car(lambda), newSym(L"evald-lambda"));
  check_eq(sig(lambda), nil);
  check(isCons(callee_body(lambda)));
  check_eq(car(callee_body(lambda)), newNum(34));
  check_eq(callee_env(lambda), newSym(L"dynamicScope"));
  rmref(lambda);
  rmref(cells.front());
}

void test_eval_on_lambda_is_idempotent() {
  list<Cell*> cells = wartRead(stream(L"(lambda () 34)"));
  check_eq(cells.size(), 1);
  Cell* lambda = eval(cells.front());
  Cell* lambda2 = eval(lambda);
  check_eq(car(lambda2), newSym(L"evald-lambda"));
  check_eq(sig(lambda2), nil);
  check(isCons(callee_body(lambda2)));
  check_eq(car(callee_body(lambda2)), newNum(34));
  check_eq(callee_env(lambda2), newSym(L"dynamicScope"));
  rmref(lambda2);
  rmref(lambda);
  rmref(cells.front());
}

void test_eval_handles_closure() {
  list<Cell*> cells = wartRead(stream(L"(lambda () 34)"));
  check_eq(cells.size(), 1);
  newLexicalScope();
    Cell* newLexicalScope = currLexicalScopes.top();
    check_eq(newLexicalScope->nrefs, 1);
    Cell* result = eval(cells.front());
    check_eq(newLexicalScope->nrefs, 2);
  endLexicalScope();
  check_eq(newLexicalScope->nrefs, 1);
  check_eq(car(result), newSym(L"evald-lambda"));
  check_eq(car(cdr(result)), nil);
  check_eq(car(car(cdr(cdr(result)))), newNum(34));
  check_eq(cdr(cdr(cdr(result))), newLexicalScope);
  rmref(result);
  check_eq(newLexicalScope->nrefs, 0);
  rmref(cells.front());
}

void test_eval_handles_lambda_calls() {
  Cell* call = wartRead(stream(L"((lambda () 34))")).front();
  Cell* result = eval(call);
  check_eq(result, newNum(34));
  rmref(result);
  rmref(call);
}

void test_eval_expands_syms_in_lambda_bodies() {
  Cell* lambda = wartRead(stream(L"((lambda () a))")).front();
  newDynamicScope(L"a", newNum(34));
  Cell* result = eval(lambda);
  check_eq(result, newNum(34));
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
    check_eq(result, newNum(34));
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
    check_eq(result, newNum(34));
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
  check_eq(result, newNum(34));
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
  check_eq(result, newNum(23));
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
  check_eq(result, newSym(L"a"));
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
  check_eq(result, newSym(L"a"));
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
  check_eq(result, newNum(2));
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
  check_eq(result, newNum(2));
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
  check_eq(car(result), newNum(1));
  rmref(result);
  rmref(call);
}

void test_eval_evals_args() {
  Cell* call = wartRead(stream(L"((lambda (f) (f)) (lambda () 34))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_doesnt_leak_body_evals() {
  Cell* call = wartRead(stream(L"((lambda (f) (f) (f)) (lambda () 34))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 34);
  rmref(result);
  rmref(call);
}

void test_eval_handles_destructured_params() {
  Cell* call = wartRead(stream(L"((lambda ((a b)) b) '(1 2))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_quoted_destructured_params() {
  Cell* call = wartRead(stream(L"((lambda ('(a b)) b) (1 2))")).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 2);
  rmref(result);
  rmref(call);
}

void test_eval_handles_rest_params() {
  Cell* call = wartRead(stream(L"((lambda (a b . c) c) 1 2 3 4 5)")).front();
  Cell* result = eval(call);
  check(isCons(result));
  check(isNum(car(result)));
  check_eq(toNum(car(result)), 3);
  check(isNum(car(cdr(result))));
  check_eq(toNum(car(cdr(result))), 4);
  check_eq(toNum(car(cdr(cdr(result)))), 5);
  check_eq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
}

                                  Cell* copyList(Cell* x) {
                                    if (!isCons(x)) return x;
                                    Cell* result = newCell();
                                    setCar(result, copyList(car(x)));
                                    setCdr(result, copyList(cdr(x)));
                                    return result;
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
  Cell* lambda = wartRead(stream(L"(lambda y `(assign ,@y))")).front();
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

void test_eval_handles_eval() {
  newDynamicScope(L"a", newNum(34));
  newDynamicScope(L"x", newSym(L"a"));
  Cell* call = wartRead(stream(L"(eval x)")).front();
  Cell* result = eval(call);
  check_eq(result, newNum(34));
  rmref(result);
  rmref(call);
  endDynamicScope(L"x");
  endDynamicScope(L"a");
}

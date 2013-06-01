void test_nil_evals_to_itself() {
  run("()");
  CHECK_TRACE_TOP("eval", "nil branch=> nil");
}

void test_num_evals_to_itself() {
  run("34");
  CHECK_TRACE_TOP("eval", "literal=> 34");
}

void test_keyword_sym_evals_to_itself() {
  run(":abc");
  CHECK_TRACE_TOP("eval", "keyword sym=> :abc");
}

void test_string_evals_to_itself() {
  run("\"ac bd\"");
  CHECK_TRACE_TOP("eval", "literal=> \"ac bd\"");
}

void test_sym_evals_to_value() {
  new_dynamic_scope("a", new_num(34));
  run("a");
  CHECK_TRACE_TOP("eval", "sym=> 34");
  end_dynamic_scope("a");
}

void test_sym_evals_to_itself() {
  new_dynamic_scope("a", new_sym("a"));
  run("a");
  CHECK_TRACE_TOP("eval", "sym=> a");
  end_dynamic_scope("a");
}

void test_eval_handles_quoted_atoms() {
  run("'a\n'34");
  CHECK_TRACE_CONTENTS("eval", "'aquote=> a'34quote=> 34");
}

void test_object_expr_evals_to_itself() {
  run("(object foo 4)");
  CHECK_TRACE_TOP("eval", "object=> (object foo 4)");
}

void test_eval_handles_quoted_lists() {
  run("'(a b)");
  CHECK_TRACE_TOP("eval", "quote=> (a b)");
}

void test_eval_handles_backquoted_lists() {
  run("`(a b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a b)");
  CHECK_TRACE_CONTENTS("backquote", "atom: aatom: b");
}

void test_eval_handles_unquote() {
  new_dynamic_scope("b", new_num(34));
  run("`(a ,b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 34)");
  CHECK_TRACE_CONTENTS("backquote", "atom: aeval: 34");
  end_dynamic_scope("b");
}

void test_eval_handles_unquote_splice() {
  TEMP(val, read("(34 35)"));
  new_dynamic_scope("b", val);
  run("`(a ,@b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 34 35)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: asplice: (34 35)");
  end_dynamic_scope("b");
}

void test_eval_splices_copies_of_lists() {
  TEMP(a, read("(3)"));
  new_dynamic_scope("a", a);
  TEMP(result, eval("`(,@a)"));
  CHECK(result != a);
  end_dynamic_scope("a");
}

void test_eval_handles_unquote_and_unquote_splice() {
  TEMP(a, read("(3)"));
  new_dynamic_scope("a", a);
  TEMP(b, read("(4)"));
  new_dynamic_scope("b", b);
  run("`(,@a ,b)");
  CHECK_TRACE_TOP("eval", "backquote=> (3 (4))");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "splice: (3)eval: (4)");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}


void test_eval_handles_unquote_splice_of_nil() {
  new_dynamic_scope("b", nil);
  run("`(a ,@b 3)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 3)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: asplice: nilatom: 3");
  end_dynamic_scope("b");
}

void test_eval_quotes_quote_comma() {
  new_dynamic_scope("b", new_sym("x"));
  run("`(a ',b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a 'x)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: aeval: x");
  end_dynamic_scope("b");
}

void test_eval_evals_comma_quote() {
  new_dynamic_scope("b", new_sym("x"));
  run("`(a ,'b)");
  CHECK_TRACE_TOP("eval", "backquote=> (a b)");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "atom: aeval: b");
  end_dynamic_scope("b");
}

void test_eval_handles_nested_quotes() {
  new_dynamic_scope("a", new_sym("x"));
  TEMP(b, read("(x y)"));
  new_dynamic_scope("b", b);
  run("`(,a `(,a ,,a ,,@b))");
  CHECK_TRACE_TOP("eval", "backquote=> (x `(,a x x y))");
  CHECK_TRACE_CONTENTS("backquote", /*any frame*/ "(,a `(,a ,,a ,,@b)) 1eval: x(,a ,,a ,,@b) 2not deep enough: ,aeval: xsplice: (x y)");
  end_dynamic_scope("a");
  end_dynamic_scope("b");
}



void test_eval_handles_fn_calls() {
  run("((fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34");
}

void test_eval_expands_syms_in_fn_bodies() {
  new_dynamic_scope("x", new_num(34));
  run("((fn () x))");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("x");
}

void test_eval_expands_lexically_scoped_syms_in_fn_bodies() {
  new_lexical_scope();
  add_lexical_binding("a", new_num(34));
    run("((fn () a))");
    CHECK_TRACE_TOP("eval", "=> 34");
  end_lexical_scope();
}

void test_eval_expands_syms_in_original_lexical_scope() {
  new_dynamic_scope("a", new_num(23));
  new_lexical_scope();
  add_lexical_binding("a", new_num(34));
    run("(<- f (fn () a))");
  end_lexical_scope();
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_expands_args_in_caller_scope() {
  new_dynamic_scope("a", new_num(23));
  new_lexical_scope();
  add_lexical_binding("a", new_num(34));
    run("(<- f (fn (a) a))");
  end_lexical_scope();
  run("(f a)");
  CHECK_TRACE_TOP("eval", "=> 23");
  end_dynamic_scope("f");
  end_dynamic_scope("a");
}

void test_eval_handles_multiple_body_exprs() {
  run("((fn () 1 2))");
  CHECK_TRACE_TOP("eval", "=> 2");
}

void test_eval_evals_arg() {
  new_dynamic_scope("x", new_num(3));
  CLEAR_TRACE;
  run("((fn (a)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: 3");
  end_dynamic_scope("x");
}

void test_eval_evals_arg2() {
  run("((fn (f) (f)) (fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34");
}

void test_eval_handles_multiple_args() {
  run("((fn (a b) b) 1 2)");
  CHECK_TRACE_TOP("eval", "=> 2");
}

void test_eval_binds_missing_params() {
  new_dynamic_scope("x", new_num(3));
  CLEAR_TRACE;
  run("((fn (a b)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: nil");
  end_dynamic_scope("x");
}

void test_eval_binds_quoted_param() {
  run("((fn ('a)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: x");
}

void test_eval_handles_quoted_param_list() {
  new_dynamic_scope("a", new_num(23));
  run("((fn '(arg1) arg1) a)");
  CHECK_TRACE_CONTENTS("bind", "arg1: a");
  CHECK_TRACE_TOP("eval", "=> a");
  end_dynamic_scope("a");
}

void test_eval_handles_vararg_param() {
  run("((fn args args) 1)");
  CHECK_TRACE_TOP("eval", "=> (1)");
}

void test_eval_evals_vararg_args() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn args) x y)");
  CHECK_TRACE_CONTENTS("eval", "xsym=> 3ysym=> 4");
  CHECK_TRACE_CONTENTS("bind", "args: (3 4)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_binds_quoted_varargs_param() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn 'args) x y)");
  CHECK_TRACE_CONTENTS("bind", "args: (x y)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_handles_rest_params() {
  run("((fn (a b ... c) c) 1 2 3 4 5)");
  CHECK_TRACE_TOP("eval", "=> (3 4 5)");
}

void test_eval_evals_rest_args() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn (a ... b)) x y)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: (4)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_binds_quoted_rest_param() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn (a ... 'b)) x y)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: (y)");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_handles_destructured_params() {
  run("((fn ((a b)) b) '(1 2))");
  CHECK_TRACE_CONTENTS("bind", "a: 1b: 2");
}

void test_eval_evals_destructured_args() {
  new_dynamic_scope("x", new_num(3));
  new_dynamic_scope("y", new_num(4));
  CLEAR_TRACE;
  run("((fn ((a b))) `(,x ,y))");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: 4");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_handles_quoted_destructured_params() {
  run("((fn ('(a b)) b) (1 2))");
  CHECK_TRACE_CONTENTS("bind", "a: 1b: 2");
}



void test_splice_args_works() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_cons(new_num(4), new_cons(new_num(5))));   // (4 5)
  cell* args = read("(a @b a)");
  cell* f = read("(fn nil 3)");
  cell* fn = eval(f);
  cell* splicedArgs = splice_args(args, nil, fn);
  // (a ''4 ''5 a)
  CHECK_EQ(car(splicedArgs), new_sym("a"));
  CHECK_EQ(car(car(cdr(splicedArgs))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(splicedArgs))), new_num(4));
  CHECK_EQ(car(car(cdr(cdr(splicedArgs)))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(cdr(splicedArgs)))), new_num(5));
  CHECK_EQ(car(cdr(cdr(cdr(splicedArgs)))), new_sym("a"));
  CHECK_EQ(cdr(cdr(cdr(cdr(splicedArgs)))), nil);
  rmref(splicedArgs);
  rmref(fn);
  rmref(f);
  rmref(args);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_splice_args_works_with_nil() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", nil);
  cell* args = read("(a @b a)");
  cell* f = read("(fn nil 3)");
  cell* fn = eval(f);
  cell* splicedArgs = splice_args(args, nil, fn);
  // (a a)
  CHECK_EQ(car(splicedArgs), new_sym("a"));
  CHECK_EQ(car(cdr(splicedArgs)), new_sym("a"));
  CHECK_EQ(cdr(cdr(splicedArgs)), nil);
  rmref(splicedArgs);
  rmref(fn);
  rmref(f);
  rmref(args);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_splice_args_works_with_keywords() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_cons(new_num(4), new_cons(new_sym(":x"))));  // (4 :x)
  cell* args = read("(a @b a)");
  cell* f = read("(fn nil 3)");
  cell* fn = eval(f);
  cell* splicedArgs = splice_args(args, nil, fn);
  // (a ''4 :x a)
  CHECK_EQ(car(splicedArgs), new_sym("a"));
  CHECK_EQ(car(car(cdr(splicedArgs))), sym_already_evald);
  CHECK_EQ(cdr(car(cdr(splicedArgs))), new_num(4));
  CHECK_EQ(car(cdr(cdr(splicedArgs))), new_sym(":x"));
  CHECK_EQ(car(cdr(cdr(cdr(splicedArgs)))), new_sym("a"));
  CHECK_EQ(cdr(cdr(cdr(cdr(splicedArgs)))), nil);
  rmref(splicedArgs);
  rmref(fn);
  rmref(f);
  rmref(args);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_eval_bind_all_handles_already_evald_arg() {
  cell* params = read("(x)");
  cell* args = mkref(new_cons(tag_already_evald(new_sym("a"))));   // (''a)
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  In_macro.push(true);
  eval_bind_all(params, args, scope, new_scope);
  In_macro.pop();
  CHECK_EQ(unsafe_get(new_scope, "x"), new_sym("a"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_multiply_already_evald_arg() {
  cell* params = read("(x)");
  cell* args = mkref(new_cons(tag_already_evald(tag_already_evald(new_sym("a")))));  // (''''a)
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  In_macro.push(true);
  eval_bind_all(params, args, scope, new_scope);
  In_macro.pop();
  CHECK_EQ(unsafe_get(new_scope, "x"), new_sym("a"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_bind_all_handles_already_evald_rest_arg() {
  cell* params = read("x");
  cell* args = mkref(new_cons(tag_already_evald(new_sym("a"))));
  cell* scope = mkref(new_table());
  set(scope, "a", new_num(3));
  cell* new_scope = mkref(new_table());
  In_macro.push(true);
  eval_bind_all(params, args, scope, new_scope);
  In_macro.pop();
  CHECK_EQ(car(unsafe_get(new_scope, "x")), new_sym("a"));
  rmref(new_scope);
  rmref(scope);
  rmref(args);
  rmref(params);
}

void test_eval_handles_splice() {
  cell* expr = read("(cons @b)");
  cell* val = read("(3 4)");
  new_dynamic_scope("b", val);
  cell* result = eval(expr);
  // (3 ... 4)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), new_num(4));
  rmref(result);
  end_dynamic_scope("b");
  rmref(val);
  rmref(expr);
}

void test_eval_handles_splice2() {
  cell* fn = read("(fn x (cons @x))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  cell* call1 = read("(f 1 2)");
  cell* result = eval(call1);
  // (1 ... 2)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(1));
  CHECK_EQ(cdr(result), new_num(2));
  rmref(result);

  cell* call2 = read("(f 3 4)");
  result = eval(call2);
  // (3 ... 4)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), new_num(4));
  rmref(result);

  rmref(call2);
  rmref(call1);
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice3() {
  cell* fn = read("(fn (x y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  // (a ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice4() {
  cell* fn = read("(fn ('x y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f a @args)");
  cell* result = eval(call);
  // (a ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice5() {
  cell* fn = read("(fn (x y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f a @args)");
  cell* result = eval(call);
  // (3 ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_handles_splice6() {
  cell* fn = read("(fn (x 'y) (cons x y))");
  cell* def = eval(fn);
  new_dynamic_scope("f", def);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  // (a ... b)
  CHECK(is_cons(result));
  CHECK_EQ(car(result), new_sym("a"));
  CHECK_EQ(cdr(result), new_sym("b"));
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(def);
  rmref(fn);
}

void test_eval_splice_on_macros_warns() {
  cell* expr = read("(fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))");
  cell* fn = eval(expr);
  new_dynamic_scope("f", fn);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  CHECK_EQ(Raise_count, 1);   Raise_count=0;
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(fn);
  rmref(expr);
}

void test_eval_splice_on_macros_with_backquote() {
  cell* expr = read("(fn '(x y) (eval `(cons ,x ,y) caller_scope))");
  cell* fn = eval(expr);
  new_dynamic_scope("f", fn);
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", new_num(4));
  cell* argval = read("(a b)");
  new_dynamic_scope("args", argval);
  cell* call = read("(f @args)");
  cell* result = eval(call);
  CHECK_EQ(Raise_count, 0);
  rmref(result);
  rmref(call);
  end_dynamic_scope("args");
  rmref(argval);
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
  rmref(fn);
  rmref(expr);
}



void test_eval_handles_assigned_fn_calls() {
  run("(<- f (fn () 34))");
  CLEAR_TRACE;
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("f");
}

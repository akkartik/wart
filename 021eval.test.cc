void test_nil_evals_to_itself() {
  trace("test") << "- eval";
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
  trace("test") << "quoting and backquoting";
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

void test_eval_handles_backquote_unquote() {
  run("`(list `,,@'(1 2))");
  CHECK_TRACE_TOP("eval", "backquote=> (list 1 2)");
}

void test_eval_handles_unquote_splice_unquote() {
  new_dynamic_scope("a", new_cons(new_sym("x"), new_cons(new_sym("y"))));
  run("`(list `(list ,@,a))");
  CHECK_TRACE_TOP("eval", "backquote=> (list `(list x y))");
  end_dynamic_scope("a");
}



void test_eval_handles_literal_function_call() {
  trace("test") << "fn params";
  TEMP(f, read("(object function)"));
  cell* t = new_table();
  set_cdr(cdr(f), new_cons(t));
  TEMP(body, read("(34)"));
  set(t, sym_body, body);
  TEMP(call, mkref(new_cons(f)));
  rmref(eval(call));
  CHECK_TRACE_TOP("eval", "=> 34");
}

void test_eval_handles_literal_function_call2() {
  TEMP(f, read("(object function)"));
  cell* t = new_table();
  set_cdr(cdr(f), new_cons(t));
  TEMP(body, read("(a)"));
  set(t, sym_body, body);
  TEMP(sig, read("(a)"));
  set(t, sym_sig, sig);
  TEMP(call, mkref(new_cons(f, new_cons(new_num(34)))));
  rmref(eval(call));
  CHECK_TRACE_TOP("eval", "=> 34");
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



void test_eval_splices_args() {
  trace("test") << "fn args";
  TEMP(val, read("(3 4)"));
  new_dynamic_scope("b", val);
  run("cons @b");
  CHECK_TRACE_CONTENTS("splice", "(''3 ''4)");
  CHECK_TRACE_TOP("eval", "=> (3 ... 4)");
  end_dynamic_scope("b");
}

void test_eval_splices_args2() {
  new_dynamic_scope("a", new_num(3));
  TEMP(b, read("(4 5)"));
  new_dynamic_scope("b", b);
  run("((fn nil 3) a @b a)");
  CHECK_TRACE_CONTENTS("splice", "(a ''4 ''5 a)");  // other args get eval'd later
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_eval_splices_args3() {
  run("f <- (fn (x y) (cons x y))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  CLEAR_TRACE;
  run("f @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
}

void test_eval_splices_nil_args() {
  new_dynamic_scope("a", new_num(3));
  new_dynamic_scope("b", nil);
  run("((fn nil 3) a @b a)");
  CHECK_TRACE_CONTENTS("splice", "(a a)");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_eval_splices_keyword_syms_into_args() {
  new_dynamic_scope("a", new_num(3));
  TEMP(b, read("(4 :x)"));
  new_dynamic_scope("b", b);
  run("((fn nil 3) a @b a)");
  CHECK_TRACE_CONTENTS("splice", "(a ''4 :x a)");  // keyword syms aren't tagged with ''
  end_dynamic_scope("b");
  end_dynamic_scope("a");
}

void test_eval_handles_splice_inside_fn_body() {
  run("((fn x (cons @x)) 1 2)");
  CHECK_TRACE_TOP("eval", "=> (1 ... 2)");
}

void test_eval_handles_splice_and_selective_quoting() {
  run("f <- (fn ('x y) (cons x y))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(b)");
  CLEAR_TRACE;
  run("f a @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
}

void test_eval_overrides_quoted_params_with_spliced_args() {
  run("f <- (fn (x 'y) (cons x y))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  CLEAR_TRACE;
  run("f @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("f");
}

void test_eval_handles_already_evald_arg() {
  new_dynamic_scope("a", new_num(3));
  TEMP(call, read("((fn (x) 3))"));
  cell* arg = tag_already_evald(new_sym("a"));  // ''a but can't go through read
  append(call, new_cons(arg));
  In_macro.push(true);
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: a");
  In_macro.pop();
  end_dynamic_scope("a");
}

void test_eval_handles_already_evald_arg_quoted_param() {
  new_dynamic_scope("a", new_num(3));
  TEMP(call, read("((fn '(x) 3))"));
  cell* arg = tag_already_evald(new_sym("a"));
  append(call, new_cons(arg));
  In_macro.push(true);
  CLEAR_TRACE;
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: ''a");  // still already-eval'd
  In_macro.pop();
  end_dynamic_scope("a");
}

void test_eval_handles_already_evald_arg_quoted_rest_param() {
  new_dynamic_scope("a", new_num(3));
  TEMP(call, read("((fn 'x 3))"));
  cell* arg = tag_already_evald(new_sym("a"));
  append(call, new_cons(arg));
  In_macro.push(true);
  CLEAR_TRACE;
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: (a)");  // stripped already-evald
  In_macro.pop();
  end_dynamic_scope("a");
}

void test_eval_handles_multiply_already_evald_arg() {
  TEMP(call, read("((fn (x) 3))"));
  cell* arg = tag_already_evald(tag_already_evald(new_sym("a")));  // ''''a
  append(call, new_cons(arg));
  In_macro.push(true);
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: a");
  In_macro.pop();
}

void test_eval_handles_already_evald_rest_arg() {
  new_dynamic_scope("a", new_num(3));
  TEMP(call, read("((fn x 3))"));
  cell* arg = tag_already_evald(new_sym("a"));  // ''a but can't go through read
  append(call, new_cons(arg));
  In_macro.push(true);
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: (a)");
  In_macro.pop();
  end_dynamic_scope("a");
}

void test_eval_splice_on_macros_with_backquote() {
  run("m <- (fn '(x y) (eval `(cons ,x ,y) caller_scope))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  Hide_warnings = true;
  CLEAR_TRACE;
  run("m @args");
  CHECK_TRACE_TOP("eval", "=> (a ... b)");  // spliced args override quoted params
  CHECK_TRACE_DOESNT_WARN();
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("m");
}

void test_eval_splice_on_backquoteless_macros_warns() {
  run("m <- (fn '(x y) (eval (cons 'cons (cons x (cons y nil))) caller_scope))");
  run("(a <- 3) (b <- 4)");
  run("args <- '(a b)");
  Hide_warnings = true;
  run("m @args");
  CHECK_TRACE_WARNS();
  end_dynamic_scope("args");
  end_dynamic_scope("b");
  end_dynamic_scope("a");
  end_dynamic_scope("m");
}



void test_eval_handles_keyword_arg() {
  run("((fn (a b) a) 2 :a 1)");
  CHECK_TRACE_CONTENTS("bind", "a: 1");
}

void test_eval_handles_nil_keyword_arg() {
  run("((fn (a) a) 2 :a nil)");
  CHECK_TRACE_CONTENTS("bind", "a: nil");
}

void test_eval_handles_keyword_args() {
  run("((fn (a b c) c) :c 1 2)");
  CHECK_TRACE_CONTENTS("bind", "a: 2b: nilc: 1");
  CHECK_TRACE_TOP("eval", "=> 1");
}

void test_eval_handles_quoted_keyword_args() {
  run("((fn (a b 'c) c) :c 1 2)");
  CHECK_TRACE_CONTENTS("bind", "a: 2b: nilc: 1");
  CHECK_TRACE_TOP("eval", "=> 1");
}

void test_eval_handles_quoted_keyword_args2() {
  run("x <- 1");
  run("((fn (a b 'c) c) :c x 2)");
  CHECK_TRACE_CONTENTS("bind", "a: 2b: nilc: x");
  CHECK_TRACE_TOP("eval", "=> x");
  end_dynamic_scope("x");
}

void test_eval_handles_rest_keyword_arg() {
  run("((fn (a ... b) b) 2 :b 1 3)");
  CHECK_TRACE_CONTENTS("bind", "a: 2b: (1 3)");
  CHECK_TRACE_TOP("eval", "=> (1 3)");
}

void test_eval_handles_rest_keyword_arg2() {
  run("((fn (a ... b) b) :b 1 2 3)");
  CHECK_TRACE_CONTENTS("bind", "a: nilb: (1 2 3)");
  CHECK_TRACE_TOP("eval", "=> (1 2 3)");
}

void test_eval_handles_args_after_rest_keyword() {
  run("((fn (a ... b) b) :b 1 2 :a 3)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: (1 2)");
  CHECK_TRACE_TOP("eval", "=> (1 2)");
}

void test_eval_handles_args_after_rest_keyword2() {
  run("((fn (a b ... c) b) :c 1 2 :b 3)");
  CHECK_TRACE_CONTENTS("bind", "a: nilb: 3c: (1 2)");
  CHECK_TRACE_TOP("eval", "=> 3");
}

void test_eval_handles_quoted_rest_keyword_arg() {
  run("x <- 2");
  run("((fn (a ... 'b) b) :b 1 x 3)");
  CHECK_TRACE_CONTENTS("bind", "a: nilb: (1 x 3)");
  CHECK_TRACE_TOP("eval", "=> (1 x 3)");
  end_dynamic_scope("x");
}

void test_eval_handles_non_keyword_arg_colon_syms() {
  run("((fn (a b) a) :x 1)");
  CHECK_TRACE_CONTENTS("bind", "a: :xb: 1");
  CHECK_TRACE_TOP("eval", "=> :x");
}

void test_eval_handles_keyword_args_inside_splice() {
  run("((fn (a b) b) @'(3 :a 4))");
  CHECK_TRACE_CONTENTS("bind", "a: 4b: 3");
  CHECK_TRACE_TOP("eval", "=> 3");
}

void test_eval_handles_keyword_args_inside_destructured_params() {
  run("((fn ((a b)) b) '(3 :a 4))");
  CHECK_TRACE_CONTENTS("bind", "a: 4b: 3");
  CHECK_TRACE_TOP("eval", "=> 3");
}



void test_eval_handles_param_aliases() {
  run("((fn (x|y) 3) 4)");
  CHECK_TRACE_CONTENTS("bind", "x: 4y: 4");
}

void test_eval_handles_aliased_keyword_args() {
  run("((fn (a b c|x) c) :x 1 2)");
  CHECK_TRACE_TOP("eval", "=> 1");
}

void test_eval_handles_quoted_param_aliases() {
  new_dynamic_scope("x", new_num(3));
  run("((fn ((a | 'b)) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: x");
  end_dynamic_scope("x");
}

void test_eval_handles_aliased_rest_keyword_args() {
  run("((fn (a ... body|do) body) 2 :do 1 3)");
  CHECK_TRACE_CONTENTS("bind", "a: 2body: (1 3)");
  CHECK_TRACE_TOP("eval", "=> (1 3)");
}

void test_eval_handles_aliased_rest_keyword_args2() {
  run("((fn (a b ... body|do) `(,a ,b ,body)) 2 :do 1 3)");
  CHECK_TRACE_CONTENTS("bind", "a: 2b: nilbody: (1 3)");
  CHECK_TRACE_TOP("eval", "=> (2 nil (1 3))");
}

void test_eval_handles_quoted_rest_param_aliases() {
  new_dynamic_scope("x", new_num(3));
  run("((fn (a | 'b) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: (3)b: (x)");
  end_dynamic_scope("x");
}

// param aliases also stand in for as-params like in haskell
void test_eval_binds_as_params() {
  run("((fn (a | (b c)) 3) 1 2)");
  CHECK_TRACE_CONTENTS("bind", "a: (1 2)b: 1c: 2");
}

void test_eval_binds_as_params_with_keyword_args() {
  run("((fn (a | (b c)) 3) 1 :b 2)");
  CHECK_TRACE_CONTENTS("bind", "a: (1 :b 2)b: 2c: 1");
}

void test_eval_binds_as_params_with_keyword_args2() {
  run("((fn (a | (b c d)) 3) 1 :b 2 3)");
  CHECK_TRACE_CONTENTS("bind", "a: (1 :b 2 3)b: 2c: 1d: 3");
}

void test_eval_warns_on_keyword_args_for_conflicting_aliases() {
  exit(0);
  Hide_warnings = true;
  run("((fn (a|b c|d) 3) :c 1 :d 2)");
  CHECK_TRACE_WARNS();
  CHECK_TRACE_CONTENTS("bind", "a: (:d 1)a: :db: :dc: 1d: 1");
}

void test_eval_binds_as_params_recursively() {
  run("((fn (a | (b ... (c | (d e)))) 3) 1 2 3)");
  CHECK_TRACE_CONTENTS("bind", "a: (1 2 3)b: 1c: (2 3)d: 2e: 3");
}

void test_eval_binds_as_params_recursively_using_keyword_args() {
  run("((fn (a | (b ... (c | (d e)))) 3) 1 :e 2 3)");
  CHECK_TRACE_CONTENTS("bind", "a: (1 :e 2 3)b: 1c: (:e 2 3)d: 3e: 2");
}

void test_eval_binds_quoted_as_params_recursively_using_keyword_args() {
  run("((fn ('a | (b|c)) 3) :b 1)");
  CHECK_TRACE_CONTENTS("bind", "a: (1)b: 1");
}

void test_eval_binds_quoted_rest_as_params_recursively_using_keyword_args() {
  run("((fn ('a | (b ... (c | (d e)))) 3) 1 :e 2 3)");
  CHECK_TRACE_CONTENTS("bind", "a: (1 :e 2 3)b: 1c: (:e 2 3)d: 3e: 2");
}

void test_eval_binds_quoted_as_params() {
  new_dynamic_scope("x", new_num(3));
  run("((fn ('a | ('b)) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: (x)b: x");
  CHECK_TRACE_DOESNT_CONTAIN("eval", "x");
  end_dynamic_scope("x");
}

void test_eval_handles_quoted_as_params() {
  new_dynamic_scope("x", new_num(3));
  run("((fn (a | ('b)) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: (3)b: x");
  end_dynamic_scope("x");
}

// gotcha: a|(b c) won't work
void test_eval_warns_on_unary_as() {
  Hide_warnings = true;
  run("((fn (| a) 3) 1 2)");
  CHECK_TRACE_WARNS();
}

void test_eval_binds_missing_as_params_to_nil() {
  run("((fn ((a | (b c))) 3) 1)");
  CHECK_TRACE_CONTENTS("bind", "a: 1b: nilc: nil");
}

void test_eval_handles_duplicate_destructured_aliases() {
  run("((fn ((a b|x) (c d|x)) 3) '(1 :x 2) '(3 :x 4))");
  CHECK_TRACE_CONTENTS("bind", "a: 1b: 2c: 3d: 4");  // x might end up bound as either 2 or 4
}

void test_eval_handles_already_evald_aliased_arg() {
  new_dynamic_scope("a", new_num(3));
  // construct ((fn (x|y) 3) ''a)
  TEMP(call, read("((fn (x|y) 3))"));
  cell* arg = tag_already_evald(new_sym("a"));
  append(call, new_cons(arg));
  In_macro.push(true);
  rmref(eval(call));
  CHECK_TRACE_CONTENTS("bind", "x: ay: a");
  In_macro.pop();
  end_dynamic_scope("a");
}

void test_eval_only_reorders_when_necessary() {
  trace_stream* old = Trace_stream;  Trace_stream = NULL;  // trace can't handle cycles yet
  TEMP(x, read("(3)"));
  set_cdr(x, x);  // cycle
  new_dynamic_scope("x", x);

  run("((fn (x|y) 3)    ((fn args args) x))");  // arg is (list x)
  // should terminate
  set_cdr(x, nil);
  end_dynamic_scope("x");
  Trace_stream = old;
}

void test_fn_evals_arg_only_when_necessary() {
  run("((fn ('a | (| 'b 'c)) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: (x)b: (x)c: (x)");
  CHECK_TRACE_DOESNT_CONTAIN("eval", "x");
}

void test_fn_evals_arg_only_when_necessary2() {
  new_dynamic_scope("y", new_num(3));
  run("((fn (| 'a ('b c)) 3) x y)");
  CHECK_TRACE_CONTENTS("bind", "a: (x y)b: xc: 3");
  CHECK_TRACE_DOESNT_CONTAIN("eval", "x");
  CHECK_TRACE_CONTENTS("eval", "y");
  end_dynamic_scope("y");
}

void test_fn_evals_destructured_arg_only_when_necessary() {
  run("((fn (('a | 'b)) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: xb: x");
  CHECK_TRACE_DOESNT_CONTAIN("eval", "x");
  CHECK_TRACE_DOESNT_CONTAIN("eval", "'x");
}

void test_fn_evals_destructured_arg_only_when_necessary2() {
  run("((fn (('a | (| 'b 'c))) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: xb: xc: x");
  CHECK_TRACE_DOESNT_CONTAIN("eval", "x");
  CHECK_TRACE_DOESNT_CONTAIN("eval", "'x");
}

void test_fn_evals_destructured_arg_only_when_necessary3() {
  new_dynamic_scope("x", new_num(3));
  run("((fn ((| 'a (| 'b c))) 3) x)");
  CHECK_TRACE_CONTENTS("bind", "a: xb: xc: 3");
  end_dynamic_scope("x");
}



void test_eval_handles_assigned_fn_calls() {
  run("(<- f (fn () 34))");
  CLEAR_TRACE;
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("f");
}



void test_eval_on_incomplete_eval_retries() {
  trace("test") << "incomplete eval";
  Warn_on_unknown_var = false;
  TEMP(attempt1, eval("a"));
  CHECK_TRACE_CONTENTS("lookup", "incomplete_eval");
  CHECK_TRACE_CONTENTS("eval", "=> (object incomplete_eval a)");
  new_dynamic_scope("a", new_num(34));
  CLEAR_TRACE;
  TEMP(attempt2, eval(attempt1));
  CHECK_TRACE_CONTENTS("eval", "incomplete_eval");
  CHECK_TRACE_CONTENTS("eval", "=> 34");
  end_dynamic_scope("a");
}

void test_eval_handles_unknown_call() {
  TEMP(attempt1, eval("(f 34)"));
  CHECK_TRACE_CONTENTS("eval", "incomplete_eval fn");
  CHECK_TRACE_CONTENTS("eval", "=> (object incomplete_eval (f 34))");
  CLEAR_TRACE;
  run("f <- (fn(a) a)");
  TEMP(attempt2, eval(attempt1));
  CHECK_TRACE_CONTENTS("eval", "=> 34");
  end_dynamic_scope("f");
}

void test_eval_handles_unknown_arg() {
  run("f <- (fn(a) a)");
  TEMP(attempt1, eval("(f x)"));
  CHECK_TRACE_CONTENTS("eval", "ripple");
  CHECK_TRACE_CONTENTS("eval", "=> (object incomplete_eval ((object function {sig, body, }) x))");
  CLEAR_TRACE;
  new_dynamic_scope("x", new_num(34));
  TEMP(attempt2, eval(attempt1));
  CHECK_TRACE_CONTENTS("eval", "=> 34");
  end_dynamic_scope("x");
  end_dynamic_scope("f");
}

void test_eval_handles_known_and_unknown_args() {
  new_dynamic_scope("x", new_num(3));
  TEMP(result, eval("((fn(a b) b) x y)"));
  CHECK_TRACE_TOP("eval", "=> (object incomplete_eval ((object function {sig, body, }) ''3 y))");
  end_dynamic_scope("x");
}

void test_eval_handles_quoted_and_unknown_args() {
  TEMP(result, eval("((fn ('a b) b) x y)"));
  CHECK_TRACE_TOP("eval", "=> (object incomplete_eval ((object function {sig, body, }) ''x y))");
}

void test_eval_handles_unknown_destructured_args() {
  TEMP(result, eval("((fn((a b)) b) args)"));
  CHECK_TRACE_TOP("eval", "=> (object incomplete_eval ((object function {sig, body, }) args))");
}

void test_eval_handles_unknown_spliced_args() {
  TEMP(result, eval("((fn((a b)) b) @args)"));
  CHECK_TRACE_TOP("eval", "=> (object incomplete_eval ((object function {sig, body, }) @args))");
}

void test_eval_handles_literal_incomplete_args() {
  run("((fn(x) x) (object incomplete_eval 34))");
  CHECK_TRACE_CONTENTS("eval", "=> 34");
}

void test_eval_handles_incomplete_args_for_aliased_params() {
  TEMP(result, eval("((fn(a|b) a) x)"));
  CHECK_TRACE_TOP("eval", "=> (object incomplete_eval ((object function {sig, body, }) x))");
}

void test_eval_handles_incomplete_args_for_quoted_aliased_params() {
  run("((fn ((| 'a b))))");
  // doesn't die
}



void test_is_macro_on_primitives() {
  CHECK(!is_macro(lookup("fn")));
}

void test_is_macro_on_non_functions() {
  CHECK(!is_macro(lookup("stdout")));
}

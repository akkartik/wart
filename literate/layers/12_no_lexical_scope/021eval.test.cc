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
  run("'a");
  CHECK_TRACE_CONTENTS("eval", "'aquote=> a");
  run("'34");
  CHECK_TRACE_CONTENTS("eval", "'34quote=> 34");
}

void test_object_expr_evals_to_itself() {
  run("(object foo 4)");
  CHECK_TRACE_TOP("eval", "object=> (object foo 4)");
}

void test_eval_handles_quoted_lists() {
  run("'(a b)");
  CHECK_TRACE_TOP("eval", "quote=> (a b)");
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
  run("((fn ((a b))) (cons x (cons y)))");
  CHECK_TRACE_CONTENTS("bind", "a: 3b: 4");
  end_dynamic_scope("x");
  end_dynamic_scope("y");
}

void test_eval_handles_quoted_destructured_params() {
  run("((fn ('(a b)) b) (1 2))");
  CHECK_TRACE_CONTENTS("bind", "a: 1b: 2");
}



void test_eval_handles_assigned_fn_calls() {
  run("(<- f (fn () 34))");
  CLEAR_TRACE;
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34");
  end_dynamic_scope("f");
}

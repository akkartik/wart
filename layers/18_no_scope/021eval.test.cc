void test_nil_evals_to_itself() {
  run("()");
  CHECK_TRACE_TOP("eval", "nil branch\n=> nil");
}

void test_num_evals_to_itself() {
  run("34");
  CHECK_TRACE_TOP("eval", "literal\n=> 34\n");
}

void test_keyword_sym_evals_to_itself() {
  run(":abc");
  CHECK_TRACE_TOP("eval", "keyword sym\n=> :abc\n");
}

void test_string_evals_to_itself() {
  run("\"ac bd\"");
  CHECK_TRACE_TOP("eval", "literal\n=> \"ac bd\"\n");
}

void test_sym_evals_to_value() {
  new_binding("a", new_num(34));
  run("a");
  CHECK_TRACE_TOP("eval", "sym\n=> 34\n");
}

void test_sym_evals_to_itself() {
  new_binding("a", new_sym("a"));
  run("a");
  CHECK_TRACE_TOP("eval", "sym\n=> a\n");
}

void test_eval_handles_quoted_atoms() {
  run("'a '34");
  CHECK_TRACE_CONTENTS("eval", "'a\nquote\n=> a\n'34\nquote\n=> 34\n");
}

void test_eval_handles_quoted_lists() {
  run("'(a b)");
  CHECK_TRACE_TOP("eval", "quote\n=> (a b)\n");
}



void test_eval_handles_fn_calls() {
  run("((fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_eval_expands_syms_in_fn_bodies() {
  new_binding("x", new_num(34));
  run("((fn () x))");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_eval_handles_multiple_body_exprs() {
  run("((fn () 1 2))");
  CHECK_TRACE_TOP("eval", "=> 2\n");
}

void test_eval_evals_arg() {
  new_binding("x", new_num(3));
  CLEAR_TRACE;
  run("((fn (a)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: 3\n");
}

void test_eval_evals_arg2() {
  run("((fn (f) (f)) (fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_eval_handles_multiple_args() {
  run("((fn (a b) b) 1 2)");
  CHECK_TRACE_TOP("eval", "=> 2\n");
}

void test_eval_binds_missing_params() {
  new_binding("x", new_num(3));
  CLEAR_TRACE;
  run("((fn (a b)) x)");
  CHECK_TRACE_CONTENTS("bind", "a: 3\nb: nil\n");
}

void test_eval_handles_vararg_param() {
  run("((fn args args) 1)");
  CHECK_TRACE_TOP("eval", "=> (1)\n");
}

void test_eval_evals_vararg_args() {
  new_binding("x", new_num(3));
  new_binding("y", new_num(4));
  CLEAR_TRACE;
  run("((fn args) x y)");
  CHECK_TRACE_CONTENTS("eval", "x\nsym\n=> 3\ny\nsym\n=> 4\n");
  CHECK_TRACE_CONTENTS("bind", "args: (3 4)\n");
}

void test_eval_handles_rest_params() {
  run("((fn (a b ... c) c) 1 2 3 4 5)");
  CHECK_TRACE_TOP("eval", "=> (3 4 5)\n");
}

void test_eval_evals_rest_args() {
  new_binding("x", new_num(3));
  new_binding("y", new_num(4));
  CLEAR_TRACE;
  run("((fn (a ... b)) x y)");
  CHECK_TRACE_CONTENTS("bind", "a: 3\nb: (4)\n");
}

void test_eval_handles_destructured_params() {
  run("((fn ((a b)) b) '(1 2))");
  CHECK_TRACE_CONTENTS("bind", "a: 1\nb: 2\n");
}

void test_eval_evals_destructured_args() {
  new_binding("x", new_num(3));
  new_binding("y", new_num(4));
  CLEAR_TRACE;
  run("((fn ((a b))) (cons x (cons y)))");
  CHECK_TRACE_CONTENTS("bind", "a: 3\nb: 4\n");
}



void test_eval_handles_assigned_fn_calls() {
  run("(<- f (fn () 34))");
  CLEAR_TRACE;
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_evalBindAll_handles_unquoted_param() {
  newBinding("a", newNum(3));
  CLEAR_TRACE;
  run("((fn (x)) a)");
  CHECK_TRACE_CONTENTS("bind", "x: 3\n");
}

void test_evalBindAll_binds_missing_params() {
  newBinding("a", newNum(3));
  CLEAR_TRACE;
  run("((fn (x y)) a)");
  CHECK_TRACE_CONTENTS("bind", "x: 3\ny: nil\n");
}

void test_evalBindAll_handles_varargs_param() {
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  CLEAR_TRACE;
  run("((fn x) a b)");
  CHECK_TRACE_CONTENTS("eval", "a\nsym\n=> 3\nb\nsym\n=> 4\n");
  CHECK_TRACE_CONTENTS("bind", "x: (3 4)\n");
}

void test_evalBindAll_handles_rest_param() {
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  CLEAR_TRACE;
  run("((fn (x ... y)) a b)");
  CHECK_TRACE_CONTENTS("bind", "x: 3\ny: (4)\n");
}

void test_evalBindAll_handles_destructured_params() {
  newBinding("x", newNum(3));
  newBinding("y", newNum(4));
  CLEAR_TRACE;
  run("((fn ((a b))) (cons x (cons y)))");
  CHECK_TRACE_CONTENTS("bind", "a: 3\nb: 4\n");
}



void test_nil_evals_to_itself() {
  run("()");
  CHECK_TRACE_TOP("eval", "nil branch\n=> nil");
}

void test_num_evals_to_itself() {
  run("34");
  CHECK_TRACE_TOP("eval", "literal\n=> 34\n");
}

void test_colonsym_evals_to_itself() {
  run(":abc");
  CHECK_TRACE_TOP("eval", "keyword sym\n=> :abc\n");
}

void test_colon_evals() {
  newBinding(":", newNum(34));
  run(":");
  CHECK_TRACE_TOP("eval", "sym\n=> 34\n");
}

void test_string_evals_to_itself() {
  run("\"ac bd\"");
  CHECK_TRACE_TOP("eval", "literal\n=> \"ac bd\"\n");
}

void test_sym_evals_to_value() {
  newBinding("a", newNum(34));
  run("a");
  CHECK_TRACE_TOP("eval", "sym\n=> 34\n");
}

void test_sym_evals_to_itself() {
  newBinding("a", newSym("a"));
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

void test_eval_handles_rest_params() {
  run("((fn (a b ... c) c) 1 2 3 4 5)");
  CHECK_TRACE_TOP("eval", "=> (3 4 5)\n");
}

void test_eval_handles_fn_calls() {
  run("((fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_eval_expands_syms_in_fn_bodies() {
  newBinding("a", newNum(34));
  run("((fn () a))");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_eval_handles_assigned_fn_calls() {
  run("(<- f (fn () 34))");
  CLEAR_TRACE;
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_eval_handles_multiple_args() {
  run("(<- f (fn (a b) b))");
  CLEAR_TRACE;
  run("(f 1 2)");
  CHECK_TRACE_TOP("eval", "=> 2\n");
}

void test_eval_handles_multiple_body_exprs() {
  run("(<- f (fn () 1 2))");
  CLEAR_TRACE;
  run("(f)");
  CHECK_TRACE_TOP("eval", "=> 2\n");
}

void test_eval_handles_vararg_param() {
  run("((fn args args) 1)");
  CHECK_TRACE_TOP("eval", "=> (1)\n");
}

void test_eval_evals_args() {
  run("((fn (f) (f)) (fn () 34))");
  CHECK_TRACE_TOP("eval", "=> 34\n");
}

void test_eval_handles_destructured_params() {
  run("((fn ((a b)) b) '(1 2))");
  CHECK_TRACE_TOP("eval", "=> 2\n");
}

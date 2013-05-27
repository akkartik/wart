void test_evalBindAll_handles_unquoted_param() {
  Cell* params = read("(x)");
  Cell* args = read("(a)");
  newBinding("a", newNum(3));
  CLEAR_TRACE;
  evalBindAll(params, args);
  checkTraceContents("bind", "x: 3\n");
}

void test_evalBindAll_binds_missing_params() {
  Cell* params = read("(x y)");
  Cell* args = read("(a)");
  newBinding("a", newNum(3));
  CLEAR_TRACE;
  evalBindAll(params, args);
  checkTraceContents("bind", "x: 3\ny: nil\n");
}

void test_evalBindAll_handles_varargs_param() {
  Cell* params = read("x");
  Cell* args = read("(a b)");
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  CLEAR_TRACE;
  evalBindAll(params, args);
  checkTraceContents("eval", "a\n=> sym: 3\nb\n=> sym: 4\n");
  checkTraceContents("bind", "x: (3 4)\n");
}

void test_evalBindAll_handles_rest_param() {
  Cell* params = read("(x ... y)");
  Cell* args = read("(a b)");
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  CLEAR_TRACE;
  evalBindAll(params, args);
  checkTraceContents("bind", "x: 3\ny: (4)\n");
}

void test_evalBindAll_handles_destructured_params() {
  Cell* params = read("((a b))");
  Cell* args = read("((cons x (cons y)))");
  newBinding("x", newNum(3));
  newBinding("y", newNum(4));
  CLEAR_TRACE;
  evalBindAll(params, args);
  checkTraceContents("bind", "a: 3\nb: 4\n");
}



void test_nil_evals_to_itself() {
  run("()");
  checkTraceContents("eval", "nil\n=> nil\n");
}

void test_num_evals_to_itself() {
  run("34");
  checkTraceContents("eval", "34\n=> literal: 34\n");
}

void test_colonsym_evals_to_itself() {
  run(":abc");
  checkTraceContents("eval", ":abc\n=> keyword sym: :abc\n");
}

void test_colon_evals() {
  newBinding(":", newNum(34));
  run(":");
  checkTraceContents("eval", ":\n=> sym: 34\n");
}

void test_string_evals_to_itself() {
  run("\"ac bd\"");
  checkTraceContents("eval", "\"ac bd\"\n=> literal: \"ac bd\"\n");
}

void test_sym_evals_to_value() {
  newBinding("a", newNum(34));
  run("a");
  checkTraceContents("eval", "a\n=> sym: 34\n");
}

void test_sym_evals_to_itself() {
  newBinding("a", newSym("a"));
  run("a");
  checkTraceContents("eval", "a\n=> sym: a\n");
}

void test_eval_handles_quoted_atoms() {
  run("'a '34");
  checkTraceContents("eval", "'a\n=> quote: a\n'34\n=> quote: 34\n");
}

void test_eval_handles_quoted_lists() {
  run("'(a b)");
  checkTraceContents("eval", "'(a b)\n=> quote: (a b)\n");
}

void test_eval_handles_rest_params() {
  run("((fn (a b ... c) c) 1 2 3 4 5)");
  checkTraceContents("eval", 1, "((fn (a b ... c) c) 1 2 3 4 5)\n=> (3 4 5)\n");
}

void test_eval_handles_fn_calls() {
  run("((fn () 34))");
  checkTraceContents("eval", 1, "((fn nil 34))\n=> 34\n");
}

void test_eval_expands_syms_in_fn_bodies() {
  newBinding("a", newNum(34));
  run("((fn () a))");
  checkTraceContents("eval", 1, "((fn nil a))\n=> 34\n");
}

void test_eval_handles_assigned_fn_calls() {
  run("(<- f (fn () 34))");
  CLEAR_TRACE;
  run("(f)");
  checkTraceContents("eval", 1, "(f)\n=> 34\n");
}

void test_eval_handles_multiple_args() {
  run("(<- f (fn (a b) b))");
  CLEAR_TRACE;
  run("(f 1 2)");
  checkTraceContents("eval", 1, "(f 1 2)\n=> 2\n");
}

void test_eval_handles_multiple_body_exprs() {
  run("(<- f (fn () 1 2))");
  CLEAR_TRACE;
  run("(f)");
  checkTraceContents("eval", 1, "(f)\n=> 2\n");
}

void test_eval_handles_vararg_param() {
  run("((fn args args) 1)");
  checkTraceContents("eval", 1, "((fn args args) 1)\n=> (1)\n");
}

void test_eval_evals_args() {
  run("((fn (f) (f)) (fn () 34))");
  checkTraceContents("eval", 1, "((fn (f) (f)) (fn nil 34))\n=> 34\n");
}

void test_eval_handles_destructured_params() {
  run("((fn ((a b)) b) '(1 2))");
  checkTraceContents("eval", 1, "((fn ((a b)) b) '(1 2))\n=> 2\n");
}

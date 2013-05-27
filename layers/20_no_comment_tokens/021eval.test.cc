void test_evalBindAll_handles_unquoted_param() {
  Cell* params = read("(x)");
  Cell* args = read("(a)");
  newBinding("a", newNum(3));
  delete global_trace_stream, global_trace_stream = new TraceStream;
  evalBindAll(params, args);
  checkTraceContents("lookup", "x: 3\n");
}

void test_evalBindAll_binds_missing_params() {
  Cell* params = read("(x y)");
  Cell* args = read("(a)");
  newBinding("a", newNum(3));
  delete global_trace_stream, global_trace_stream = new TraceStream;
  evalBindAll(params, args);
  checkTraceContents("lookup", "x: 3\ny: nil\n");
}

void test_evalBindAll_handles_varargs_param() {
  Cell* params = read("x");
  Cell* args = read("(a b)");
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  delete global_trace_stream, global_trace_stream = new TraceStream;
  evalBindAll(params, args);
  checkTraceContents("eval", "a\nsym: 3\nb\nsym: 4\n");
  checkTraceContents("lookup", "x: (3 4)\n");
}

void test_evalBindAll_handles_rest_param() {
  Cell* params = read("(x ... y)");
  Cell* args = read("(a b)");
  newBinding("a", newNum(3));
  newBinding("b", newNum(4));
  delete global_trace_stream, global_trace_stream = new TraceStream;
  evalBindAll(params, args);
  checkTraceContents("lookup", "x: 3\ny: (4)\n");
}

void test_evalBindAll_handles_destructured_params() {
  Cell* params = read("((a b))");
  Cell* args = read("((cons x (cons y)))");
  newBinding("x", newNum(3));
  newBinding("y", newNum(4));
  delete global_trace_stream, global_trace_stream = new TraceStream;
  evalBindAll(params, args);
  checkTraceContents("lookup", "a: 3\nb: 4\n");
}



void test_nil_evals_to_itself() {
  stringstream in("()");
  run(in);
  checkTraceContents("eval", "nil\n=> nil\n");
}

void test_num_evals_to_itself() {
  stringstream in("34");
  run(in);
  checkTraceContents("eval", "34\nliteral: 34\n");
}

void test_colonsym_evals_to_itself() {
  stringstream in(":abc");
  run(in);
  checkTraceContents("eval", ":abc\nkeyword sym: :abc\n");
}

void test_colon_evals() {
  stringstream in(":");
  newBinding(":", newNum(34));
  run(in);
  checkTraceContents("eval", ":\nsym: 34\n");
}

void test_string_evals_to_itself() {
  stringstream in("\"ac bd\"");
  run(in);
  checkTraceContents("eval", "\"ac bd\"\nliteral: \"ac bd\"\n");
}

void test_sym_evals_to_value() {
  newBinding("a", newNum(34));
  stringstream in("a");
  run(in);
  checkTraceContents("eval", "a\nsym: 34\n");
}

void test_sym_evals_to_itself() {
  newBinding("a", newSym("a"));
  stringstream in("a");
  run(in);
  checkTraceContents("eval", "a\nsym: a\n");
}

void test_eval_handles_quoted_atoms() {
  stringstream in("'a '34");
  run(in);
  checkTraceContents("eval", "'a\nquote: a\n'34\nquote: 34\n");
}

void test_eval_handles_quoted_lists() {
  stringstream in("'(a b)");
  run(in);
  checkTraceContents("eval", "'(a b)\nquote: (a b)\n");
}

void test_eval_handles_rest_params() {
  stringstream in("((fn (a b ... c) c) 1 2 3 4 5)");
  run(in);
  checkTraceContents2("eval", 1, "((fn (a b ... c) c) 1 2 3 4 5)\n=> (3 4 5)\n");
}

void test_eval_handles_fn_calls() {
  stringstream in("((fn () 34))");
  run(in);
  checkTraceContents2("eval", 1, "((fn nil 34))\n=> 34\n");
}

void test_eval_expands_syms_in_fn_bodies() {
  stringstream in("((fn () a))");
  newBinding("a", newNum(34));
  run(in);
  checkTraceContents2("eval", 1, "((fn nil a))\n=> 34\n");
}

void test_eval_handles_assigned_fn_calls() {
  {
    stringstream in("(<- f (fn () 34))");
    run(in);
    delete global_trace_stream, global_trace_stream = new TraceStream;
  }
  stringstream in("(f)");
  run(in);
  checkTraceContents2("eval", 1, "(f)\n=> 34\n");
}

void test_eval_handles_multiple_args() {
  {
    stringstream in("(<- f (fn (a b) b))");
    run(in);
    delete global_trace_stream, global_trace_stream = new TraceStream;
  }
  stringstream in("(f 1 2)");
  run(in);
  checkTraceContents2("eval", 1, "(f 1 2)\n=> 2\n");
}

void test_eval_handles_multiple_body_exprs() {
  {
    stringstream in("(<- f (fn () 1 2))");
    run(in);
    delete global_trace_stream, global_trace_stream = new TraceStream;
  }
  stringstream in("(f)");
  run(in);
  checkTraceContents2("eval", 1, "(f)\n=> 2\n");
}

void test_eval_handles_vararg_param() {
  stringstream in("((fn args args) 1)");
  run(in);
  checkTraceContents2("eval", 1, "((fn args args) 1)\n=> (1)\n");
}

void test_eval_evals_args() {
  stringstream in("((fn (f) (f)) (fn () 34))");
  run(in);
  checkTraceContents2("eval", 1, "((fn (f) (f)) (fn nil 34))\n=> 34\n");
}

void test_eval_handles_destructured_params() {
  stringstream in("((fn ((a b)) b) '(1 2))");
  run(in);
  checkTraceContents2("eval", 1, "((fn ((a b)) b) '(1 2))\n=> 2\n");
}

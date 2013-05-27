void test_add_works() {
  run("(+ 1 2)");
  CHECK_TRACE_TOP("eval", "compiled fn\n=> 3\n");
}

void test_add_works_for_floats() {
  Cell* result = run("(+ 1.0 2.0)");
  CHECK_TRACE_TOP("eval", "compiled fn\n=> 3\n");
  CHECK_EQ(result->type, FLOAT);
}

void test_division_always_returns_floats() {
  Cell* result = run("(/ 4 2)");
  CHECK_TRACE_TOP("eval", "compiled fn\n=> 2\n");
  CHECK_EQ(result->type, FLOAT);
}

void test_integer_drops_decimals() {
  Cell* result = run("(int -2.7)");
  CHECK_TRACE_TOP("eval", "compiled fn\n=> -2\n");
  CHECK_EQ(result->type, INTEGER);
}

void test_lesser_always_passes_nil() {
  run("(< 3 nil)");
  CHECK_TRACE_TOP("eval", "compiled fn\n=> nil\n");
}

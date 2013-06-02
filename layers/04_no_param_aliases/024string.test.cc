void test_string_range() {
  run("(string_range \"abc\" 0 2)");
  CHECK_TRACE_TOP("eval", "=> \"ab\"");
}

void test_string_compare() {
  run("(string_lesser \"abc\" \"abd\")");
  CHECK_TRACE_TOP("eval", "=> 1");
}

void test_string_split() {
  run("(split \"abc abd\")");
  CHECK_TRACE_TOP("eval", "=> (\"abc\" \"abd\")");
}

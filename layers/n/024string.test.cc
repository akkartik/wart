void test_string_range() {
  run("(string_range \"abc\" 0 2)");
  CHECK_TRACE_TOP("eval", "=> \"ab\"");
}

void test_string_compare() {
  run("(string_lesser \"abc\" \"abd\")");
  CHECK_TRACE_TOP("eval", "=> \"abd\"");
}

void test_string_compare2() {
  run("(string_lesser \"abe\" \"abd\")");
  CHECK_TRACE_TOP("eval", "=> false");
}

void test_string_compare_passes_false() {
  run("(string_lesser false \"abd\")");
  CHECK_TRACE_TOP("eval", "=> false");
}

void test_string_split() {
  run("(split \"abc abd\")");
  CHECK_TRACE_TOP("eval", "=> (\"abc\" \"abd\")");
}

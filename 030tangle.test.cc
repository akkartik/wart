void test_hunks_works_without_metadata() {
  std::istringstream in("a\nb\nc\n");
  hunks(in);
  CHECK_TRACE_CONTENTS("tangle", "hunk: ahunk: bhunk: c");
}

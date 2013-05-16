void test_trace_check_compares() {
  trace << "foo\n";
  checkTraceContents("foo\n");
}

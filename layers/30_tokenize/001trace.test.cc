void test_trace_check_compares() {
  START_TRACING;
  trace << "foo\n";
  checkTraceContents("foo\n");
}

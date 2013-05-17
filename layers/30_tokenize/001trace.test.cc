void test_trace_check_compares() {
  checkTraceContents("test layer", "");
  trace("test layer") << "foo\n";
  checkTraceContents("test layer", "foo\n");
}

void test_trace_check_filters_layers() {
  checkTraceContents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  checkTraceContents("test layer 1", "foo\n");
}

void test_trace_orders_across_layers() {
  checkTraceContents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  trace("test layer 1") << "qux\n";
  checkTraceContents("", "foo\nbar\nqux\n");
}

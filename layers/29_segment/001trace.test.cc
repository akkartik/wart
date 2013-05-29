void test_trace_check_compares() {
  check_trace_contents("test layer", "");
  trace("test layer") << "foo\n";
  check_trace_contents("test layer", "foo\n");
}

void test_trace_check_filters_layers() {
  check_trace_contents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  check_trace_contents("test layer 1", "foo\n");
}

void test_trace_orders_across_layers() {
  check_trace_contents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  trace("test layer 1") << "qux\n";
  check_trace_contents("", "foo\nbar\nqux\n");
}

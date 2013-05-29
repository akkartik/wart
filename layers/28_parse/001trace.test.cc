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

void test_trace_segments_within_layers() {
  check_trace_contents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  new_trace_frame("test layer 1");
  trace("test layer 1") << "qux\n";
  check_trace_contents("test layer 1", "foo\nqux\n");
  check_trace_contents2("test layer 1", 0, "foo\n");
}

void trace_test_fn(int n) {
  if (n == 0) return;
  new_trace_frame("foo");
  trace("foo") << "before: " << n << "\n";
  trace_test_fn(n-1);
  trace("foo") << "after: " << n << "\n";
}

void test_trace_keeps_level_together() {
  check_trace_contents("foo", "");
  trace_test_fn(4);
  check_trace_contents2("foo", 2, "before: 3\nafter: 3\n");
}

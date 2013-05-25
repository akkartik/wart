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
  checkTraceContents("test layer 1,test layer 2", "foo\nbar\nqux\n");
}

void test_trace_segments_within_layers() {
  checkTraceContents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  incTraceForRestOfScope("test layer 1");
  trace("test layer 1") << "qux\n";
  checkTraceContents("test layer 1", "foo\nqux\n");
  checkTraceContents2("test layer 1", 0, "foo\n");
}

void trace_test_fn(int n) {
  if (n == 0) return;
  incTraceForRestOfScope("foo");
  trace("foo") << "before: " << n << "\n";
  trace_test_fn(n-1);
  trace("foo") << "after: " << n << "\n";
}

void test_trace_keeps_level_together() {
  checkTraceContents("foo", "");
  trace_test_fn(4);
  checkTraceContents2("foo", 2, "before: 3\nafter: 3\n");
}

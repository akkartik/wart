void test_split_returns_at_least_one_elem() {
  vector<string> result = split("", ',');
  CHECK_EQ(result.size(), 1);
  CHECK_EQ(result[0], "");
}

void test_split_returns_entire_input_when_no_delim() {
  vector<string> result = split("abc", ',');
  CHECK_EQ(result.size(), 1);
  CHECK_EQ(result[0], "abc");
}

void test_split_works() {
  vector<string> result = split("abc,def", ',');
  CHECK_EQ(result.size(), 2);
  CHECK_EQ(result[0], "abc");
  CHECK_EQ(result[1], "def");
}

void test_split_works2() {
  vector<string> result = split("abc,def,ghi", ',');
  cerr << global_trace_stream->readable_contents("");
  CHECK_EQ(result.size(), 3);
  CHECK_EQ(result[0], "abc");
  CHECK_EQ(result[1], "def");
  CHECK_EQ(result[2], "ghi");
}

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

void test_trace_check_ignores_other_lines() {
  checkTraceContents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 1") << "bar\n";
  checkTraceContents("test layer 1", "foo\n");
}

void test_trace_check_ignores_other_lines2() {
  checkTraceContents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 1") << "bar\n";
  checkTraceContents("test layer 1", "");
}

void test_trace_orders_across_layers() {
  checkTraceContents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  trace("test layer 1") << "qux\n";
  cerr << "---\n";
  checkTraceContents("", "foo\nbar\nqux\n");
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

void test_trace_supports_multiple_layers() {
  checkTraceContents("test layer", "");
  trace("test layer 1") << "foo\n";
  trace("test layer 2") << "bar\n";
  trace("test layer 1") << "qux\n";
  checkTraceContents("test layer 1,test layer 2", "foo\nbar\nqux\n");
}

void test_trace_supports_hierarchical_layers() {
  checkTraceContents("test layer", "");
  trace("test layer/a") << "foo\n";
  trace("different layer/c") << "foo 2\n";
  trace("test layer/b") << "bar\n";
  checkTraceContents("test layer/", "foo\nbar\n");
}

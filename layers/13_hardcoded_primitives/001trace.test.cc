void test_trace_check_compares() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer") << "foo";
  CHECK_TRACE_CONTENTS("test layer", "foo");
}

void test_trace_check_filters_layers() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 2") << "bar";
  CHECK_TRACE_CONTENTS("test layer 1", "foo");
}

void test_trace_check_ignores_other_lines() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 1") << "bar";
  CHECK_TRACE_CONTENTS("test layer 1", "foo");
}

void test_trace_check_always_finds_empty_lines() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 1") << "bar";
  CHECK_TRACE_CONTENTS("test layer 1", "");
}

void test_trace_check_treats_empty_layers_as_wildcards() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 2") << "bar";
  CHECK_TRACE_CONTENTS("", "foobar");
}

void test_trace_check_always_finds_empty_lines2() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 1") << "bar";
  CHECK_TRACE_CONTENTS("test layer 1", "");
}

void test_trace_orders_across_layers() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 2") << "bar";
  trace("test layer 1") << "qux";
  CHECK_TRACE_CONTENTS("", "foobarqux");
}

void test_trace_segments_within_layers() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 2") << "bar";
  new_trace_frame("test layer 1");
  trace("test layer 1") << "qux";
  CHECK_TRACE_CONTENTS("test layer 1", "fooqux");
  CHECK_TRACE_CONTENTS("test layer 1", 0, "foo");
}

void trace_test_fn(int n) {
  if (n == 0) return;
  new_trace_frame("foo");
  trace("foo") << "before: " << n;
  trace_test_fn(n-1);
  trace("foo") << "after: " << n;
}

void test_trace_keeps_level_together() {
  CHECK_TRACE_CONTENTS("foo", "");
  trace_test_fn(4);
  CHECK_TRACE_CONTENTS("foo", 2, "before: 3after: 3");
}

void test_trace_supports_multiple_layers() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 2") << "bar";
  trace("test layer 1") << "qux";
  CHECK_TRACE_CONTENTS("test layer 1,test layer 2", "foobarqux");
}

void test_trace_supports_hierarchical_layers() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer/a") << "foo";
  trace("different layer/c") << "foo 2";
  trace("test layer/b") << "bar";
  CHECK_TRACE_CONTENTS("test layer/", "foobar");
}

void test_trace_supports_count() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 1") << "foo";
  CHECK_EQ(trace_count("test layer 1", "foo"), 2);
}

void test_trace_supports_count2() {
  CHECK_TRACE_CONTENTS("test layer", "");
  trace("test layer 1") << "foo";
  trace("test layer 1") << "bar";
  CHECK_EQ(trace_count("test layer 1"), 2);
}



// Can't test on traces here because trace methods call trace.

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
  CHECK_EQ(result.size(), 3);
  CHECK_EQ(result[0], "abc");
  CHECK_EQ(result[1], "def");
  CHECK_EQ(result[2], "ghi");
}

void test_tangle() {
  istringstream in("a\nb\nc\n:(before b)\nd\n");
  list<string> dummy;
  tangle(in, dummy);
  CHECK_TRACE_CONTENTS("tangle", "adbc");
}

void test_tangle2() {
  istringstream in("a\nb\nc\n:(after b)\nd\n");
  list<string> dummy;
  tangle(in, dummy);
  CHECK_TRACE_CONTENTS("tangle", "abdc");
}

void test_tangle_at_end() {
  istringstream in("a\nb\nc\n:(after c)\nd\n");
  list<string> dummy;
  tangle(in, dummy);
  CHECK_TRACE_CONTENTS("tangle", "abcd");
}

void test_tangle_indents_hunks_correctly() {
  istringstream in("a\n  b\nc\n:(after b)\nd\n");
  list<string> dummy;
  tangle(in, dummy);
  CHECK_TRACE_CONTENTS("tangle", "a  b  dc");
}

void test_tangle_supports_scenarios() {
  istringstream in(":(scenario foo does_bar)\nabc def\n-layer1: pqr\n-layer2: xyz");
  list<string> dummy;
  tangle(in, dummy);
  CHECK_EQ(dummy.front(), "void test_does_bar() {"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "  foo(\"abc def\");"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqrlayer2: xyz\");"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "}"); dummy.pop_front();
  CHECK(dummy.empty());
}

void test_tangle_supports_strings_in_scenarios() {
  istringstream in(":(scenario foo does_bar)\nabc \"def\"\n-layer1: pqr\n-layer2: \"xyz\"");
  list<string> dummy;
  tangle(in, dummy);
  CHECK_EQ(dummy.front(), "void test_does_bar() {"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "  foo(\"abc \\\"def\\\"\");"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqrlayer2: \\\"xyz\\\"\");"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "}"); dummy.pop_front();
  CHECK(dummy.empty());
}

void test_tangle_supports_strings_in_scenarios2() {
  istringstream in(":(scenario foo does_bar)\nabc \"\"\n-layer1: pqr\n-layer2: \"\"");
  list<string> dummy;
  tangle(in, dummy);
  CHECK_EQ(dummy.front(), "void test_does_bar() {"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "  foo(\"abc \\\"\\\"\");"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqrlayer2: \\\"\\\"\");"); dummy.pop_front();
  CHECK_EQ(dummy.front(), "}"); dummy.pop_front();
  CHECK(dummy.empty());
}



void test_trim() {
  CHECK_EQ(trim(""), "");
  CHECK_EQ(trim(" "), "");
  CHECK_EQ(trim("  "), "");
  CHECK_EQ(trim("a"), "a");
  CHECK_EQ(trim(" a"), "a");
  CHECK_EQ(trim("  a"), "a");
  CHECK_EQ(trim("  ab"), "ab");
  CHECK_EQ(trim("a "), "a");
  CHECK_EQ(trim("a  "), "a");
  CHECK_EQ(trim("ab  "), "ab");
  CHECK_EQ(trim(" a "), "a");
  CHECK_EQ(trim("  a  "), "a");
  CHECK_EQ(trim("  ab  "), "ab");
}

void test_strip_indent() {
  CHECK_EQ(strip_indent("", 0), "");
  CHECK_EQ(strip_indent("", 1), "");
  CHECK_EQ(strip_indent("", 3), "");
  CHECK_EQ(strip_indent(" ", 0), " ");
  CHECK_EQ(strip_indent(" a", 0), " a");
  CHECK_EQ(strip_indent(" ", 1), "");
  CHECK_EQ(strip_indent(" a", 1), "a");
  CHECK_EQ(strip_indent(" ", 2), "");
  CHECK_EQ(strip_indent(" a", 2), "a");
  CHECK_EQ(strip_indent("  ", 0), "  ");
  CHECK_EQ(strip_indent("  a", 0), "  a");
  CHECK_EQ(strip_indent("  ", 1), " ");
  CHECK_EQ(strip_indent("  a", 1), " a");
  CHECK_EQ(strip_indent("  ", 2), "");
  CHECK_EQ(strip_indent("  a", 2), "a");
  CHECK_EQ(strip_indent("  ", 3), "");
  CHECK_EQ(strip_indent("  a", 3), "a");
}

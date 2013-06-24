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
  istringstream in(":(scenario does_bar)\nabc def\n+layer1: pqr\n+layer2: xyz");
  list<string> lines;
  tangle(in, lines);
  CHECK_EQ(lines.front(), "void test_does_bar() {"); lines.pop_front();
  CHECK_EQ(lines.front(), "  run(\"abc def\\n\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqrlayer2: xyz\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "}"); lines.pop_front();
  CHECK(lines.empty());
}

void test_tangle_supports_configurable_toplevel() {
  istringstream in(":(scenarios foo)\n:(scenario does_bar)\nabc def\n+layer1: pqr");
  list<string> lines;
  tangle(in, lines);
  CHECK_EQ(lines.front(), "void test_does_bar() {"); lines.pop_front();
  CHECK_EQ(lines.front(), "  foo(\"abc def\\n\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqr\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "}"); lines.pop_front();
  CHECK(lines.empty());

  istringstream cleanup(":(scenarios run)\n");
  tangle(cleanup, lines);
}

void test_tangle_supports_strings_in_scenarios() {
  istringstream in(":(scenario does_bar)\nabc \"def\"\n+layer1: pqr\n+layer2: \"xyz\"");
  list<string> lines;
  tangle(in, lines);
  CHECK_EQ(lines.front(), "void test_does_bar() {"); lines.pop_front();
  CHECK_EQ(lines.front(), "  run(\"abc \\\"def\\\"\\n\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqrlayer2: \\\"xyz\\\"\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "}"); lines.pop_front();
  CHECK(lines.empty());
}

void test_tangle_supports_strings_in_scenarios2() {
  istringstream in(":(scenario does_bar)\nabc \"\"\n+layer1: pqr\n+layer2: \"\"");
  list<string> lines;
  tangle(in, lines);
  CHECK_EQ(lines.front(), "void test_does_bar() {"); lines.pop_front();
  CHECK_EQ(lines.front(), "  run(\"abc \\\"\\\"\\n\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqrlayer2: \\\"\\\"\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "}"); lines.pop_front();
  CHECK(lines.empty());
}

void test_tangle_supports_multiline_input_in_scenarios() {
  istringstream in(":(scenario does_bar)\nabc def\n  efg\n+layer1: pqr\n+layer2: \"\"");
  list<string> lines;
  tangle(in, lines);
  CHECK_EQ(lines.front(), "void test_does_bar() {"); lines.pop_front();
  CHECK_EQ(lines.front(), "  run(\"abc def\\n  efg\\n\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqrlayer2: \\\"\\\"\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "}"); lines.pop_front();
  CHECK(lines.empty());
}

void test_tangle_can_check_for_absence_at_end_of_scenarios() {
  istringstream in(":(scenario does_bar)\nabc def\n  efg\n+layer1: pqr\n-layer1: xyz");
  list<string> lines;
  tangle(in, lines);
  CHECK_EQ(lines.front(), "void test_does_bar() {"); lines.pop_front();
  CHECK_EQ(lines.front(), "  run(\"abc def\\n  efg\\n\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_CONTENTS(\"layer1: pqr\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_DOESNT_CONTAIN(\"layer1: xyz\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "}"); lines.pop_front();
  CHECK(lines.empty());
}

void test_tangle_can_check_for_absence_at_end_of_scenarios2() {
  istringstream in(":(scenario does_bar)\nabc def\n  efg\n-layer1: pqr\n-layer1: xyz");
  list<string> lines;
  tangle(in, lines);
  CHECK_EQ(lines.front(), "void test_does_bar() {"); lines.pop_front();
  CHECK_EQ(lines.front(), "  run(\"abc def\\n  efg\\n\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_DOESNT_CONTAIN(\"layer1: pqr\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "  CHECK_TRACE_DOESNT_CONTAIN(\"layer1: xyz\");"); lines.pop_front();
  CHECK_EQ(lines.front(), "}"); lines.pop_front();
  CHECK(lines.empty());
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

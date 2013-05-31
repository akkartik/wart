void test_parse_handles_empty_stream() {
  read_all("");
  CHECK_TRACE_CONTENTS("parse", "");
}

void test_parse_handles_trailing_comment() {
  read_all("34 # abc");
  CHECK_TRACE_CONTENTS("parse", "34\n");
}

void test_parse_handles_atom() {
  read_all("34");
  CHECK_TRACE_CONTENTS("parse", "34\n");
}

void test_parse_handles_atoms() {
  read_all("34\n\"a b c\"\n3.4");
  CHECK_TRACE_CONTENTS("parse", "34\n\"a b c\"\n3.4\n");
}

void test_parse_handles_forms() {
  read_all("(34 \"a b c\")");
  CHECK_TRACE_TOP("parse", "(34 \"a b c\")\n");
}

void test_parse_handles_nested_forms() {
  read_all("(34 (2 3) \"a b c\")");
  CHECK_TRACE_CONTENTS("parse", 1, "(34 (2 3) \"a b c\")\n");
  CHECK_TRACE_CONTENTS("parse", 2, "34\n(2 3)\n\"a b c\"\n)\n");
  CHECK_TRACE_CONTENTS("parse", 3, "2\n3\n)\n");
}

void test_parse_handles_nested_forms_with_comments() {
  read_all("(a b (c d #\n))");
  CHECK_TRACE_CONTENTS("parse", 1, "(a b (c d))\n");
  CHECK_TRACE_CONTENTS("parse", 2, "a\nb\n(c d)\n)\n");
}

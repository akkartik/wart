void test_parse_handles_empty_stream() {
  readAll("");
  checkTraceContents("parse", "\n");  // extra newline artifact
}

void test_parse_handles_trailing_comment() {
  readAll("34 # abc");
  checkTraceContents("parse", "34\n\n");  // extra newline artifact
}

void test_parse_handles_atom() {
  readAll("34");
  checkTraceContents("parse", "34\n");
}

void test_parse_handles_atoms() {
  readAll("34\n\"a b c\"\n3.4");
  checkTraceContents("parse", "34\n\"a b c\"\n3.4\n");
}

void test_parse_handles_forms() {
  readAll("(34 \"a b c\")");
  checkTraceContents2("parse", 1, "(34 \"a b c\")\n");
}

void test_parse_handles_nested_forms() {
  readAll("(34 (2 3) \"a b c\")");
  checkTraceContents2("parse", 1, "(34 (2 3) \"a b c\")\n");
  checkTraceContents2("parse", 2, "34\n(2 3)\n\"a b c\"\n)\n");
  checkTraceContents2("parse", 3, "2\n3\n)\n");
}

void test_parse_handles_nested_forms_with_comments() {
  readAll("(a b (c d #\n))");
  checkTraceContents2("parse", 1, "(a b (c d))\n");
  checkTraceContents2("parse", 2, "a\nb\n(c d)\n)\n");
}

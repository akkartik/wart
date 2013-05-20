void test_parse_handles_empty_stream() {
  stringstream in("");
  readAll(in);
  checkTraceContents("parse", "\n");  // extra newline artifact
}

void test_parse_handles_trailing_comment() {
  stringstream in("34 # abc");
  readAll(in);
  checkTraceContents("parse", "34\n\n");  // extra newline artifact
}

void test_parse_handles_atom() {
  stringstream in("34");
  readAll(in);
  checkTraceContents("parse", "34\n");
}

void test_parse_handles_atoms() {
  stringstream in("34\n\"a b c\"\n3.4");
  readAll(in);
  checkTraceContents("parse", "34\n\"a b c\"\n3.4\n");
}

void test_parse_handles_forms() {
  stringstream in("(34 \"a b c\")");
  readAll(in);
  checkTraceContents2("parse", 1, "(34 \"a b c\")\n");
}

void test_parse_handles_nested_forms() {
  stringstream in("(34 (2 3) \"a b c\")");
  readAll(in);
  checkTraceContents2("parse", 1, "(34 (2 3) \"a b c\")\n");
  checkTraceContents2("parse", 2, "34\n(2 3)\n\"a b c\"\n)\n");
  checkTraceContents2("parse", 3, "2\n3\n)\n");
}

void test_parse_handles_nested_forms_with_comments() {
  stringstream in("(a b (c d #\n))");
  readAll(in);
  checkTraceContents2("parse", 1, "(a b (c d))\n");
  checkTraceContents2("parse", 2, "a\nb\n(c d)\n)\n");
}

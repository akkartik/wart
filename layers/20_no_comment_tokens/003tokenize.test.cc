void test_tokenize_handles_multiple_atoms() {
  stringstream in("34 abc 3.4");
  readAll(in);
  checkTraceContents("tokenize", "34\nabc\n3.4\n");
}

void test_tokenize_handles_string_literal() {
  stringstream in("34 \"abc\"");
  readAll(in);
  checkTraceContents("tokenize", "34\n\"abc\"\n\n");
}

void test_tokenize_handles_multiple_lines() {
  stringstream in("34\n\"abc\"");
  readAll(in);
  checkTraceContents("tokenize", "34\n\"abc\"\n\n");
}

void test_tokenize_handles_string_with_space() {
  stringstream in("34\n\"abc def\"");
  readAll(in);
  checkTraceContents("tokenize", "34\n\"abc def\"\n\n");
}

void test_tokenize_handles_string_with_escape() {
  stringstream in("34\n\"abc \\\"quote def\"");
  readAll(in);
  checkTraceContents("tokenize", "34\n\"abc \\\"quote def\"\n\n");
}

void test_tokenize_handles_comment() {
  stringstream in("()'a #abc def ghi");
  readAll(in);
  checkTraceContents("tokenize", "(\n)\n'\na\n\n");   // extra newline just an artifact
}

void test_tokenize_ends_comment_at_newline() {
  stringstream in("#abc def ghi\nabc");
  readAll(in);
  checkTraceContents("tokenize", "abc\n");
}

void test_tokenize_suppresses_comments() {
  stringstream in("abc\n#abc\ndef\nghi");
  readAll(in);
  checkTraceContents("tokenize", "abc\ndef\nghi\n");
}

void test_tokenize_suppresses_comments2() {
  stringstream in("a b\n  c\n#abc\ndef\n  ghi\n\njkl");
  readAll(in);
  checkTraceContents("tokenize", "a\nb\nc\ndef\nghi\njkl\n");
}

void test_tokenize_suppresses_trailing_whitespace() {
  stringstream in("a \nb\r\nc");
  readAll(in);
  checkTraceContents("tokenize", "a\nb\nc\n");
}

void test_tokenize_suppresses_repeated_newline() {
  stringstream in("34\n\n\"abc \\\"quote def\"");
  readAll(in);
  checkTraceContents("tokenize", "34\n\"abc \\\"quote def\"\n\n");
}

void test_tokenize_suppresses_whitespace_lines() {
  stringstream in("abc def ghi\n\n    \n  def");
  readAll(in);
  checkTraceContents("tokenize", "abc\ndef\nghi\ndef\n");
}

void test_tokenize_suppresses_whitespace_lines2() {
  stringstream in("  \nabc def ghi\n\n    \n  def");
  readAll(in);
  checkTraceContents("tokenize", "abc\ndef\nghi\ndef\n");
}

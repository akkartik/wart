void test_tokenize_handles_multiple_atoms() {
  readAll("34 abc 3.4");
  checkTraceContents("tokenize", "34\nabc\n3.4\n");
}

void test_tokenize_handles_string_literal() {
  readAll("34 \"abc\"");
  checkTraceContents("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_multiple_lines() {
  readAll("34\n\"abc\"");
  checkTraceContents("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_string_with_space() {
  readAll("34\n\"abc def\"");
  checkTraceContents("tokenize", "34\n\"abc def\"\n");
}

void test_tokenize_handles_string_with_escape() {
  readAll("34\n\"abc \\\"quote def\"");
  checkTraceContents("tokenize", "34\n\"abc \\\"quote def\"\n");
}

void test_tokenize_handles_comment() {
  readAll("()'a #abc def ghi");
  checkTraceContents("tokenize", "(\n)\n'\na\n\n");   // extra newline just an artifact
}

void test_tokenize_ends_comment_at_newline() {
  readAll("#abc def ghi\nabc");
  checkTraceContents("tokenize", "abc\n");
}

void test_tokenize_suppresses_comments() {
  readAll("abc\n#abc\ndef\nghi");
  checkTraceContents("tokenize", "abc\ndef\nghi\n");
}

void test_tokenize_suppresses_comments2() {
  readAll("a b\n  c\n#abc\ndef\n  ghi\n\njkl");
  checkTraceContents("tokenize", "a\nb\nc\ndef\nghi\njkl\n");
}

void test_tokenize_suppresses_trailing_whitespace() {
  readAll("a \nb\r\nc");
  checkTraceContents("tokenize", "a\nb\nc\n");
}

void test_tokenize_suppresses_repeated_newline() {
  readAll("34\n\n\"abc \\\"quote def\"");
  checkTraceContents("tokenize", "34\n\"abc \\\"quote def\"\n");
}

void test_tokenize_suppresses_whitespace_lines() {
  readAll("abc def ghi\n\n    \n  def");
  checkTraceContents("tokenize", "abc\ndef\nghi\ndef\n");
}

void test_tokenize_suppresses_whitespace_lines2() {
  readAll("  \nabc def ghi\n\n    \n  def");
  checkTraceContents("tokenize", "abc\ndef\nghi\ndef\n");
}

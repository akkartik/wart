void test_tokenize_handles_multiple_atoms() {
  readAll("34 abc 3.4");
  CHECK_TRACE_CONTENTS("tokenize", "34\nabc\n3.4\n");
}

void test_tokenize_handles_string_literal() {
  readAll("34 \"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_multiple_lines() {
  readAll("34\n\"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_string_with_space() {
  readAll("34\n\"abc def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc def\"\n");
}

void test_tokenize_handles_string_with_escape() {
  readAll("34\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc \\\"quote def\"\n");
}

void test_tokenize_handles_comment() {
  readAll("()'a #abc def ghi");
  CHECK_TRACE_CONTENTS("tokenize", "(\n)\n'\na\n");
}

void test_tokenize_ends_comment_at_newline() {
  readAll("#abc def ghi\nabc");
  CHECK_TRACE_CONTENTS("tokenize", "abc\n");
}

void test_tokenize_suppresses_comments() {
  readAll("abc\n#abc\ndef\nghi");
  CHECK_TRACE_CONTENTS("tokenize", "abc\ndef\nghi\n");
}

void test_tokenize_suppresses_comments2() {
  readAll("a b\n  c\n#abc\ndef\n  ghi\n\njkl");
  CHECK_TRACE_CONTENTS("tokenize", "a\nb\nc\ndef\nghi\njkl\n");
}

void test_tokenize_suppresses_trailing_whitespace() {
  readAll("a \nb\r\nc");
  CHECK_TRACE_CONTENTS("tokenize", "a\nb\nc\n");
}

void test_tokenize_suppresses_repeated_newline() {
  readAll("34\n\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc \\\"quote def\"\n");
}

void test_tokenize_suppresses_whitespace_lines() {
  readAll("abc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", "abc\ndef\nghi\ndef\n");
}

void test_tokenize_suppresses_whitespace_lines2() {
  readAll("  \nabc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", "abc\ndef\nghi\ndef\n");
}

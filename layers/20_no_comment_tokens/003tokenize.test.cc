void test_tokenize_handles_multiple_atoms() {
  read_all("34 abc 3.4");
  CHECK_TRACE_CONTENTS("tokenize", "34\nabc\n3.4\n");
}

void test_tokenize_handles_string_literal() {
  read_all("34 \"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_multiple_lines() {
  read_all("34\n\"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_string_with_space() {
  read_all("34\n\"abc def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc def\"\n");
}

void test_tokenize_handles_string_with_escape() {
  read_all("34\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc \\\"quote def\"\n");
}

void test_tokenize_handles_comment() {
  read_all("()'a #abc def ghi");
  CHECK_TRACE_CONTENTS("tokenize", "(\n)\n'\na\n");
}

void test_tokenize_ends_comment_at_newline() {
  read_all("#abc def ghi\nabc");
  CHECK_TRACE_CONTENTS("tokenize", "abc\n");
}

void test_tokenize_suppresses_comments() {
  read_all("abc\n#abc\ndef\nghi");
  CHECK_TRACE_CONTENTS("tokenize", "abc\ndef\nghi\n");
}

void test_tokenize_suppresses_comments2() {
  read_all("a b\n  c\n#abc\ndef\n  ghi\n\njkl");
  CHECK_TRACE_CONTENTS("tokenize", "a\nb\nc\ndef\nghi\njkl\n");
}

void test_tokenize_suppresses_trailing_whitespace() {
  read_all("a \nb\r\nc");
  CHECK_TRACE_CONTENTS("tokenize", "a\nb\nc\n");
}

void test_tokenize_suppresses_repeated_newline() {
  read_all("34\n\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\n\"abc \\\"quote def\"\n");
}

void test_tokenize_suppresses_whitespace_lines() {
  read_all("abc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", "abc\ndef\nghi\ndef\n");
}

void test_tokenize_suppresses_whitespace_lines2() {
  read_all("  \nabc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", "abc\ndef\nghi\ndef\n");
}

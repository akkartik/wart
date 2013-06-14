                                  token indent(long n) { return token(n); }

void test_tokenize_always_starts_a_line_with_indent() {
  read_all("34");
  CHECK_TRACE_CONTENTS("tokenize", ":034");
}

void test_tokenize_handles_multiple_atoms() {
  read_all("34 abc 3.4");
  CHECK_TRACE_CONTENTS("tokenize", ":034abc3.4");
}

void test_tokenize_handles_string_literal() {
  read_all("34 \"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", ":034\"abc\"");
}

void test_tokenize_handles_multiple_lines() {
  read_all("34\n\"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", ":034\\n:0\"abc\"");
}

void test_tokenize_handles_string_with_space() {
  read_all("34\n\"abc def\"");
  CHECK_TRACE_CONTENTS("tokenize", ":034\\n\"abc def\"");
}

void test_tokenize_handles_string_with_escape() {
  read_all("34\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", ":034\\n\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  read_all("',35");
  CHECK_TRACE_CONTENTS("tokenize", ":0',35");
}

void test_tokenize_handles_quote_comma_paren() {
  CLEAR_TRACE;
  read_all("(',a)");
  CHECK_TRACE_CONTENTS("tokenize", ":0(',a)");
}

void test_tokenize_handles_splice_operators() {
  read_all("()',@a @,b @c");
  CHECK_TRACE_CONTENTS("tokenize", ":0()',@a@,b@c");
}

void test_tokenize_handles_comment() {
  read_all("()'a #abc def ghi");
  CHECK_TRACE_CONTENTS("tokenize", ":0()'a");
}

void test_tokenize_ends_comment_at_newline() {
  read_all("#abc def ghi\nabc");
  CHECK_TRACE_CONTENTS("tokenize", ":0abc");
}

void test_tokenize_suppresses_comments() {
  read_all("abc\n#abc\ndef\nghi");
  CHECK_TRACE_CONTENTS("tokenize", ":0abcdefghi");
}

void test_tokenize_suppresses_comments2() {
  read_all("a : b\n  : c\n#abc\ndef :\n  ghi\n\njkl");
  CHECK_TRACE_CONTENTS("tokenize", ":0abcdefghijkl");
  CHECK_EQ(trace_count("skip during tokenize", "comment token"), 3);
}

void test_tokenize_suppresses_trailing_whitespace() {
  read_all("a \nb\r\nc");
  CHECK_TRACE_CONTENTS("tokenize", ":0abc");
}

void test_tokenize_suppresses_repeated_newline() {
  read_all("34\n\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", ":034\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  read_all("abc def ghi\n\n    abc\n  def");
  CHECK_TRACE_CONTENTS("tokenize", ":0abcdefghi\\n:4abc:2def");
}

void test_tokenize_suppresses_whitespace_lines() {
  read_all("abc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", ":0abcdefghi\\n:2def");
}

void test_tokenize_suppresses_whitespace_lines2() {
  read_all("  \nabc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", ":0abcdefghi\\n:2def");
}

void test_tokenize_handles_sexpr() {
  read_all("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef");
  CHECK_TRACE_CONTENTS("tokenize", ":0('a'(boo)\"foo\nbar\"`c`,d,@e)\\n:0abc\\n:0def");
}

void test_quote_misuse_warns() {
  Hide_raises = true;
  read_all("' a");
  CHECK_EQ(trace_count("warn"), 1);
}

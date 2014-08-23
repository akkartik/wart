void test_tokenize_handles_multiple_atoms() {
  read_all("34 abc 3.4");
  CHECK_TRACE_CONTENTS("tokenize", "34abc3.4");
}

void test_tokenize_handles_string_literal() {
  read_all("34 \"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc\"");
}

void test_tokenize_handles_multiple_lines() {
  read_all("34\n\"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc\"");
}

void test_tokenize_handles_string_with_space() {
  read_all("34\n\"abc def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc def\"");
}

void test_tokenize_handles_string_with_escape() {
  read_all("34\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  read_all("',35");
  CHECK_TRACE_CONTENTS("tokenize", "',35");
}

void test_tokenize_handles_quote_comma_paren() {
  CLEAR_TRACE;
  read_all("(',a)");
  CHECK_TRACE_CONTENTS("tokenize", "(',a)");
}

void test_tokenize_handles_splice_operators() {
  read_all("()',@a @,b @c");
  CHECK_TRACE_CONTENTS("tokenize", "()',@a@,b@c");
}

void test_tokenize_handles_comment() {
  read_all("()'a #abc def ghi");
  CHECK_TRACE_CONTENTS("tokenize", "()'a");
}

void test_tokenize_ends_comment_at_newline() {
  read_all("#abc def ghi\nabc");
  CHECK_TRACE_CONTENTS("tokenize", "abc");
}

void test_tokenize_suppresses_comments() {
  read_all("abc\n#abc\ndef\nghi");
  CHECK_TRACE_CONTENTS("tokenize", "abcdefghi");
}

void test_tokenize_suppresses_comments2() {
  read_all("a : b\n  : c\n#abc\ndef :\n  ghi\n\njkl");
  CHECK_TRACE_CONTENTS("tokenize", "askip comment tokenbskip comment tokencdefskip comment tokenghijkl");
}

void test_tokenize_suppresses_trailing_whitespace() {
  read_all("a \nb\r\nc");
  CHECK_TRACE_CONTENTS("tokenize", "abc");
}

void test_tokenize_suppresses_repeated_newline() {
  read_all("34\n\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc \\\"quote def\"");
}

void test_tokenize_suppresses_whitespace_lines() {
  read_all("abc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", "abcdefghidef");
}

void test_tokenize_suppresses_whitespace_lines2() {
  read_all("  \nabc def ghi\n\n    \n  def");
  CHECK_TRACE_CONTENTS("tokenize", "abcdefghidef");
}

void test_tokenize_handles_sexpr() {
  read_all("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef");
  CHECK_TRACE_CONTENTS("tokenize", "('a'(boo)\"foo\nbar\"`c`,d,@e)abcdef");
}

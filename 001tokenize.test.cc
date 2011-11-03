void test_tokenize_handles_empty_input() {
  CodeStream c(stream(L""));
  check(eof(c.fd));
}

void test_tokenize_always_starts_a_line_with_indent() {
  CodeStream c(stream(L"34"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"34");
  check(eof(c.fd));
}

void test_tokenize_handles_multiple_atoms() {
  CodeStream c(stream(L"34 abc"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"34");
  checkEq(nextToken(c), L"abc");
  check(eof(c.fd));
}

void test_tokenize_handles_string_literal() {
  CodeStream c(stream(L"34 \"abc\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"34");
  checkEq(nextToken(c), L"\"abc\"");
  check(eof(c.fd));
}

void test_tokenize_handles_multiple_lines() {
  CodeStream c(stream(L"34\n\"abc\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"\"abc\"");
  check(eof(c.fd));
}

void test_tokenize_handles_string_with_space() {
  CodeStream c(stream(L"34\n\"abc def\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"\"abc def\"");
  check(eof(c.fd));
}

void test_tokenize_handles_string_with_escape() {
  CodeStream c(stream(L"34\n\"abc \\\"quote def\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"\"abc \\\"quote def\"");
  check(eof(c.fd));
}

void test_tokenize_handles_quote_comma() {
  CodeStream c(stream(L"',35"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"'");
  checkEq(nextToken(c), L",");
  checkEq(nextToken(c), L"35");
  check(eof(c.fd));
}

void test_tokenize_handles_quote_comma_paren() {
  CodeStream c(stream(L"(',)"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"(");
  checkEq(nextToken(c), L"'");
  checkEq(nextToken(c), L",");
  checkEq(nextToken(c), L")");
  check(eof(c.fd));
}

void test_tokenize_doesnt_break_comma_right_after_ssyntax_char() {
  CodeStream c(stream(L"'a:,b"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"'");
  checkEq(nextToken(c), L"a:,b");
  check(eof(c.fd));
}

void test_tokenize_handles_splice_operators() {
  CodeStream c(stream(L"()',@ @, @b"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"(");
  checkEq(nextToken(c), L")");
  checkEq(nextToken(c), L"'");
  checkEq(nextToken(c), L",@");
  checkEq(nextToken(c), L"@");
  checkEq(nextToken(c), L",");
  checkEq(nextToken(c), L"@");
  checkEq(nextToken(c), L"b");
  check(eof(c.fd));
}

void test_tokenize_handles_comment() {
  CodeStream c(stream(L"()',@ ;abc def ghi"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"(");
  checkEq(nextToken(c), L")");
  checkEq(nextToken(c), L"'");
  checkEq(nextToken(c), L",@");
}

void test_tokenize_ends_comment_at_newline() {
  CodeStream c(stream(L";abc def ghi\nabc"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"abc");
  check(eof(c.fd));
}

void test_tokenize_suppresses_comments() {
  CodeStream c(stream(L"abc\n;abc\ndef\nghi"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"abc");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"def");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"ghi");
  check(eof(c.fd));
}

void test_tokenize_suppresses_comments2() {
  CodeStream c(stream(L"a b\n  c\n;abc\ndef\n  ghi\n\njkl"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"a");
  checkEq(nextToken(c), L"b");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), L"c");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"def");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), L"ghi");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"jkl");
  check(eof(c.fd));
}

void test_tokenize_suppresses_trailing_whitespace() {
  CodeStream c(stream(L"a \nb\r\nc"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"a");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"b");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"c");
  check(eof(c.fd));
}

void test_tokenize_suppresses_repeated_newline() {
  CodeStream c(stream(L"34\n\n\"abc \\\"quote def\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"\"abc \\\"quote def\"");
  check(eof(c.fd));
}

void test_tokenize_handles_indent_outdent() {
  CodeStream c(stream(L"abc def ghi\n\n    abc\n  def"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"abc");
  checkEq(nextToken(c), L"def");
  checkEq(nextToken(c), L"ghi");
  checkEq(nextToken(c), Token::indent(4));
  checkEq(nextToken(c), L"abc");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), L"def");
  check(eof(c.fd));
}

void test_tokenize_suppresses_whitespace_lines() {
  CodeStream c(stream(L"abc def ghi\n\n    \n  def"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"abc");
  checkEq(nextToken(c), L"def");
  checkEq(nextToken(c), L"ghi");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), L"def");
  check(eof(c.fd));
}

void test_tokenize_suppresses_whitespace_lines2() {
  CodeStream c(stream(L"  \nabc def ghi\n\n    \n  def"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"abc");
  checkEq(nextToken(c), L"def");
  checkEq(nextToken(c), L"ghi");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), L"def");
  check(eof(c.fd));
}

void test_tokenize_handles_sexpr() {
  CodeStream c(stream(L"('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\ndef"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"(");
  checkEq(nextToken(c), L"'");
  checkEq(nextToken(c), L"a");
  checkEq(nextToken(c), L"'");
  checkEq(nextToken(c), L"(");
  checkEq(nextToken(c), L"boo");
  checkEq(nextToken(c), L")");
  checkEq(nextToken(c), L"\"foo\nbar\"");
  checkEq(nextToken(c), L"`");
  checkEq(nextToken(c), L"c");
  checkEq(nextToken(c), L"`");
  checkEq(nextToken(c), L",");
  checkEq(nextToken(c), L"d");
  checkEq(nextToken(c), L",@");
  checkEq(nextToken(c), L"e");
  checkEq(nextToken(c), L")");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"abc");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), L"def");
  check(eof(c.fd));
}

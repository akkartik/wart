void test_tokenize_handles_empty_input() {
  CodeStream c(stream(L""));
  check(eof(c.fd));
}

void test_tokenize_always_starts_a_line_with_indent() {
  list<Token> tokens = tokenize(stream(L"34"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"34"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_atoms() {
  list<Token> tokens = tokenize(stream(L"34 abc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_literal() {
  list<Token> tokens = tokenize(stream(L"34 \"abc\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_lines() {
  list<Token> tokens = tokenize(stream(L"34\n\"abc\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_space() {
  list<Token> tokens = tokenize(stream(L"34\n\"abc def\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"\"abc def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_escape() {
  list<Token> tokens = tokenize(stream(L"34\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_quote_comma() {
  list<Token> tokens = tokenize(stream(L"',35"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L"35"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_quote_comma_paren() {
  list<Token> tokens = tokenize(stream(L"(',)"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_tokenize_doesnt_break_comma_right_after_ssyntax_char() {
  list<Token> tokens = tokenize(stream(L"'a:,b"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"a:,b"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_splice_operators() {
  list<Token> tokens = tokenize(stream(L"()',@ @, @b"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L",@"); ++p;
  checkEq(*p, L"@"); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L"@"); ++p;
  checkEq(*p, L"b"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_comment() {
  list<Token> tokens = tokenize(stream(L"()',@ ;abc def ghi"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L",@"); ++p;
}

void test_tokenize_ends_comment_at_newline() {
  list<Token> tokens = tokenize(stream(L";abc def ghi\nabc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_comments() {
  list<Token> tokens = tokenize(stream(L"abc\n;abc\ndef\nghi"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"ghi"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_comments2() {
  list<Token> tokens = tokenize(stream(L"a b\n  c\n;abc\ndef\n  ghi\n\njkl"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, Token::indent(2)); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, Token::indent(2)); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"jkl"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_trailing_whitespace() {
  list<Token> tokens = tokenize(stream(L"a \nb\r\nc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"c"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_repeated_newline() {
  list<Token> tokens = tokenize(stream(L"34\n\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_indent_outdent() {
  list<Token> tokens = tokenize(stream(L"abc def ghi\n\n    abc\n  def"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, Token::indent(4)); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, Token::indent(2)); ++p;
  checkEq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_lines() {
  list<Token> tokens = tokenize(stream(L"abc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, Token::indent(2)); ++p;
  checkEq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_lines2() {
  list<Token> tokens = tokenize(stream(L"  \nabc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, Token::indent(2)); ++p;
  checkEq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_sexpr() {
  list<Token> tokens = tokenize(stream(L"('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\ndef"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"boo"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"\"foo\nbar\""); ++p;
  checkEq(*p, L"`"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"`"); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L",@"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, Token::indent(0)); ++p;
  checkEq(*p, L"def"); ++p;
  check(p == tokens.end());
}

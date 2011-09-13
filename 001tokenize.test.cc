void test_countIndent() {
  // countIndent requires a non-empty stream
  checkEq(countIndent(stream(L"\t")), 1);
  checkEq(countIndent(stream(L" ")), 1+LAST_CHAR_IS_SPACE);
  checkEq(countIndent(stream(L"   ")), 3+LAST_CHAR_IS_SPACE);
  checkEq(countIndent(stream(L" \t ")), 3+LAST_CHAR_IS_SPACE); // tab == 1 space
  checkEq(countIndent(stream(L" \n ")), 1+LAST_CHAR_IS_SPACE); // skip empty lines
  checkEq(countIndent(stream(L" \r\n  ")), 2+LAST_CHAR_IS_SPACE); // dos
  checkEq(countIndent(stream(L"\n\na")), 0);
}



void test_tokenize_handles_empty_input() {
  list<Token> tokens = tokenize(stream(L""));
  check(tokens.empty());
}

void test_tokenize_handles_atom() {
  list<Token> tokens = tokenize(stream(L"34"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_atoms() {
  list<Token> tokens = tokenize(stream(L"34 abc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_literal() {
  list<Token> tokens = tokenize(stream(L"34 \"abc\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_lines() {
  list<Token> tokens = tokenize(stream(L"34\n\"abc\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_space() {
  list<Token> tokens = tokenize(stream(L"34\n\"abc def\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"\"abc def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_escape() {
  list<Token> tokens = tokenize(stream(L"34\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_quote_comma_paren() {
  list<Token> tokens = tokenize(stream(L"(',)"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_splice_operator() {
  list<Token> tokens = tokenize(stream(L"()',@ @, @b"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
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
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L",@"); ++p;
}

void test_tokenize_ends_comment_at_newline() {
  list<Token> tokens = tokenize(stream(L";abc def ghi\nabc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_sexpr() {
  list<Token> tokens = tokenize(stream(L"('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\nabc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
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
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_trailing_whitespace() {
  list<Token> tokens = tokenize(stream(L"34 \nabc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_terminal_whitespace() {
  list<Token> tokens = tokenize(stream(L"34 abc\n  "));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_repeated_newline() {
  list<Token> tokens = tokenize(stream(L"34\n\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"34"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_indent_outdent() {
  list<Token> tokens = tokenize(stream(L"abc def ghi\n\n    abc\n  def"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, INDENT); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, OUTDENT); ++p;
  checkEq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_lines() {
  list<Token> tokens = tokenize(stream(L"abc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, INDENT); ++p;
  checkEq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_at_eol() {
  list<Token> tokens = tokenize(stream(L"a \nb\r\nc"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"c"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_initial_whitespace_lines() {
  list<Token> tokens = tokenize(stream(L"  \nabc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, INDENT); ++p;
  checkEq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_comments() {
  list<Token> tokens = tokenize(stream(L"abc\n;abc\ndef\nghi"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"ghi"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_comments2() {
  list<Token> tokens = tokenize(stream(L"a b\n  c\n;abc\ndef\n  ghi\n\njkl"));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, INDENT); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, OUTDENT); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, INDENT); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, OUTDENT); ++p;
  checkEq(*p, L"jkl"); ++p;
  check(p == tokens.end());
}

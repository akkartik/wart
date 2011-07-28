void test_countIndent() {
  // countIndent requires a non-empty stream
  check_eq(countIndent(teststream(L"\t")), 1);
  check_eq(countIndent(teststream(L" ")), 1+LAST_CHAR_IS_SPACE);
  check_eq(countIndent(teststream(L"   ")), 3+LAST_CHAR_IS_SPACE);
  check_eq(countIndent(teststream(L" \t ")), 3+LAST_CHAR_IS_SPACE); // tab == 1 space
  check_eq(countIndent(teststream(L" \n ")), 1+LAST_CHAR_IS_SPACE); // skip empty lines
  check_eq(countIndent(teststream(L" \r\n  ")), 2+LAST_CHAR_IS_SPACE); // dos
  check_eq(countIndent(teststream(L"\n\na")), 0);
}



void test_tokenize_handles_empty_input() {
  list<Token> tokens = tokenize(teststream(L""));
  check(tokens.empty());
}

void test_tokenize_handles_atom() {
  list<Token> tokens = tokenize(teststream(L"34"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_atoms() {
  list<Token> tokens = tokenize(teststream(L"34 abc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_literal() {
  list<Token> tokens = tokenize(teststream(L"34 \"abc\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_lines() {
  list<Token> tokens = tokenize(teststream(L"34\n\"abc\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_space() {
  list<Token> tokens = tokenize(teststream(L"34\n\"abc def\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_escape() {
  list<Token> tokens = tokenize(teststream(L"34\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_quote_comma_paren() {
  list<Token> tokens = tokenize(teststream(L"(',)"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L","); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_splice_operator() {
  list<Token> tokens = tokenize(teststream(L"()',@"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L",@"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_comment() {
  list<Token> tokens = tokenize(teststream(L"()',@ ;abc def ghi"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L",@"); ++p;
}

void test_tokenize_ends_comment_at_newline() {
  list<Token> tokens = tokenize(teststream(L";abc def ghi\nabc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_sexpr() {
  list<Token> tokens = tokenize(teststream(L"('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\nabc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"boo"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"\"foo\nbar\""); ++p;
  check_eq(*p, L"`"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"`"); ++p;
  check_eq(*p, L","); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L",@"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L";def ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_trailing_whitespace() {
  list<Token> tokens = tokenize(teststream(L"34 \nabc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_terminal_whitespace() {
  list<Token> tokens = tokenize(teststream(L"34 abc\n  "));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_repeated_newline() {
  list<Token> tokens = tokenize(teststream(L"34\n\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_indent_outdent() {
  list<Token> tokens = tokenize(teststream(L"abc def ghi\n\n    abc\n  def"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, OUTDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_lines() {
  list<Token> tokens = tokenize(teststream(L"abc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_at_eol() {
  list<Token> tokens = tokenize(teststream(L"a \nb\r\nc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"c"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_initial_whitespace_lines() {
  list<Token> tokens = tokenize(teststream(L"  \nabc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_comments() {
  list<Token> tokens = tokenize(teststream(L"abc\n;abc\ndef\nghi"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"ghi"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_comments2() {
  list<Token> tokens = tokenize(teststream(L"a b\n  c\n;abc\ndef\n  ghi\n\njkl"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, OUTDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, OUTDENT); ++p;
  check_eq(*p, L"jkl"); ++p;
  check(p == tokens.end());
}

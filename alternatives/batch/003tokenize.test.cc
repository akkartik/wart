                                  Token indent(long n) { return Token(n); }

void test_tokenize_always_starts_a_line_with_indent() {
  IndentSensitiveStream c(stream("34"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
}

void test_tokenize_handles_multiple_atoms() {
  IndentSensitiveStream c(stream("34 abc 3.4"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, "abc");           t=nextToken(c);
  checkEq(t, "3.4");           t=nextToken(c);
}

void test_tokenize_handles_string_literal() {
  IndentSensitiveStream c(stream("34 \"abc\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, "\"abc\"");       t=nextToken(c);
}

void test_tokenize_handles_multiple_lines() {
  IndentSensitiveStream c(stream("34\n\"abc\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "\"abc\"");       t=nextToken(c);
}

void test_tokenize_handles_string_with_space() {
  IndentSensitiveStream c(stream("34\n\"abc def\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "\"abc def\"");   t=nextToken(c);
}

void test_tokenize_handles_string_with_escape() {
  IndentSensitiveStream c(stream("34\n\"abc \\\"quote def\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  IndentSensitiveStream c(stream("',35"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, ",");            t=nextToken(c);
  checkEq(t, "35");           t=nextToken(c);
}

void test_tokenize_handles_quote_comma_paren() {
  IndentSensitiveStream c(stream("(',)"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "(");            t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, ",");            t=nextToken(c);
  checkEq(t, ")");            t=nextToken(c);
}

void test_tokenize_handles_splice_operators() {
  IndentSensitiveStream c(stream("()',@ @, @b"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "(");            t=nextToken(c);
  checkEq(t, ")");            t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, ",@");           t=nextToken(c);
  checkEq(t, "@");            t=nextToken(c);
  checkEq(t, ",");            t=nextToken(c);
  checkEq(t, "@");            t=nextToken(c);
  checkEq(t, "b");            t=nextToken(c);
}

void test_tokenize_handles_comment() {
  IndentSensitiveStream c(stream("()',@ #abc def ghi"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "(");            t=nextToken(c);
  checkEq(t, ")");            t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, ",@");           t=nextToken(c);
}

void test_tokenize_ends_comment_at_newline() {
  IndentSensitiveStream c(stream("#abc def ghi\nabc"));
  Token t = nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
}

void test_tokenize_suppresses_comments() {
  IndentSensitiveStream c(stream("abc\n#abc\ndef\nghi"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
}

void test_tokenize_suppresses_comments2() {
  IndentSensitiveStream c(stream("a b\n  c\n#abc\ndef\n  ghi\n\njkl"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "a");            t=nextToken(c);
  checkEq(t, "b");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "c");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "jkl");          t=nextToken(c);
}

void test_tokenize_suppresses_trailing_whitespace() {
  IndentSensitiveStream c(stream("a \nb\r\nc"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "a");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "b");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "c");            t=nextToken(c);
}

void test_tokenize_suppresses_repeated_newline() {
  IndentSensitiveStream c(stream("34\n\n\"abc \\\"quote def\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");           t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  IndentSensitiveStream c(stream("abc def ghi\n\n    abc\n  def"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(4));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

void test_tokenize_suppresses_whitespace_lines() {
  IndentSensitiveStream c(stream("abc def ghi\n\n    \n  def"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

void test_tokenize_suppresses_whitespace_lines2() {
  IndentSensitiveStream c(stream("  \nabc def ghi\n\n    \n  def"));
  Token t = nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

void test_tokenize_handles_sexpr() {
  IndentSensitiveStream c(stream("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "(");            t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, "a");            t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, "(");            t=nextToken(c);
  checkEq(t, "boo");          t=nextToken(c);
  checkEq(t, ")");            t=nextToken(c);
  checkEq(t, "\"foo\nbar\""); t=nextToken(c);
  checkEq(t, "`");            t=nextToken(c);
  checkEq(t, "c");            t=nextToken(c);
  checkEq(t, "`");            t=nextToken(c);
  checkEq(t, ",");            t=nextToken(c);
  checkEq(t, "d");            t=nextToken(c);
  checkEq(t, ",@");           t=nextToken(c);
  checkEq(t, "e");            t=nextToken(c);
  checkEq(t, ")");            t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, Token::Newline()); t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

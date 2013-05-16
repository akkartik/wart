                                  Token indent(long n) { return Token(n); }

void test_tokenize_always_starts_a_line_with_indent() {
  IndentSensitiveStream in("34");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
}

void test_tokenize_handles_multiple_atoms() {
  IndentSensitiveStream in("34 abc 3.4");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, "3.4");           t=nextToken(in);
}

void test_tokenize_handles_string_literal() {
  IndentSensitiveStream in("34 \"abc\"");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, "\"abc\"");       t=nextToken(in);
}

void test_tokenize_handles_multiple_lines() {
  IndentSensitiveStream in("34\n\"abc\"");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "\"abc\"");       t=nextToken(in);
}

void test_tokenize_handles_string_with_space() {
  IndentSensitiveStream in("34\n\"abc def\"");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "\"abc def\"");   t=nextToken(in);
}

void test_tokenize_handles_string_with_escape() {
  IndentSensitiveStream in("34\n\"abc \\\"quote def\"");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  IndentSensitiveStream in("',35");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "'");             t=nextToken(in);
  CHECK_EQ(t, ",");             t=nextToken(in);
  CHECK_EQ(t, "35");            t=nextToken(in);
}

void test_tokenize_handles_quote_comma_paren() {
  IndentSensitiveStream in("(',)");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "(");             t=nextToken(in);
  CHECK_EQ(t, "'");             t=nextToken(in);
  CHECK_EQ(t, ",");             t=nextToken(in);
  CHECK_EQ(t, ")");             t=nextToken(in);
}

void test_tokenize_handles_splice_operators() {
  IndentSensitiveStream in("()',@ @, @b");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "(");             t=nextToken(in);
  CHECK_EQ(t, ")");             t=nextToken(in);
  CHECK_EQ(t, "'");             t=nextToken(in);
  CHECK_EQ(t, ",@");            t=nextToken(in);
  CHECK_EQ(t, "@");             t=nextToken(in);
  CHECK_EQ(t, ",");             t=nextToken(in);
  CHECK_EQ(t, "@");             t=nextToken(in);
  CHECK_EQ(t, "b");             t=nextToken(in);
}

void test_tokenize_handles_comment() {
  IndentSensitiveStream in("()',@ #abc def ghi");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "(");             t=nextToken(in);
  CHECK_EQ(t, ")");             t=nextToken(in);
  CHECK_EQ(t, "'");             t=nextToken(in);
  CHECK_EQ(t, ",@");            t=nextToken(in);
}

void test_tokenize_ends_comment_at_newline() {
  IndentSensitiveStream in("#abc def ghi\nabc");
  Token t = nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
}

void test_tokenize_suppresses_comments() {
  IndentSensitiveStream in("abc\n#abc\ndef\nghi");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
}

void test_tokenize_suppresses_comments2() {
  IndentSensitiveStream in("a : b\n  : c\n#abc\ndef :\n  ghi\n\njkl");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "a");             t=nextToken(in);
  CHECK_EQ(t, "b");             t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(2));       t=nextToken(in);
  CHECK_EQ(t, "c");             t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(2));       t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "jkl");           t=nextToken(in);
}

void test_tokenize_suppresses_trailing_whitespace() {
  IndentSensitiveStream in("a \nb\r\nc");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "a");             t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "b");             t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "c");             t=nextToken(in);
}

void test_tokenize_suppresses_repeated_newline() {
  IndentSensitiveStream in("34\n\n\"abc \\\"quote def\"");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  IndentSensitiveStream in("abc def ghi\n\n    abc\n  def");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(4));       t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(2));       t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
}

void test_tokenize_suppresses_whitespace_lines() {
  IndentSensitiveStream in("abc def ghi\n\n    \n  def");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(2));       t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
}

void test_tokenize_suppresses_whitespace_lines2() {
  IndentSensitiveStream in("  \nabc def ghi\n\n    \n  def");
  Token t = nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(2));       t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
}

void test_tokenize_handles_sexpr() {
  IndentSensitiveStream in("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef");
  Token t = nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "(");             t=nextToken(in);
  CHECK_EQ(t, "'");             t=nextToken(in);
  CHECK_EQ(t, "a");             t=nextToken(in);
  CHECK_EQ(t, "'");             t=nextToken(in);
  CHECK_EQ(t, "(");             t=nextToken(in);
  CHECK_EQ(t, "boo");           t=nextToken(in);
  CHECK_EQ(t, ")");             t=nextToken(in);
  CHECK_EQ(t, "\"foo\nbar\"");  t=nextToken(in);
  CHECK_EQ(t, "`");             t=nextToken(in);
  CHECK_EQ(t, "c");             t=nextToken(in);
  CHECK_EQ(t, "`");             t=nextToken(in);
  CHECK_EQ(t, ",");             t=nextToken(in);
  CHECK_EQ(t, "d");             t=nextToken(in);
  CHECK_EQ(t, ",@");            t=nextToken(in);
  CHECK_EQ(t, "e");             t=nextToken(in);
  CHECK_EQ(t, ")");             t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, Token::Newline()); t=nextToken(in);
  CHECK_EQ(t, indent(0));       t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
}

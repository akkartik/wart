                                  Token indent(long n) { return Token(n); }

void test_tokenize_always_starts_a_line_with_indent() {
  CodeStream cs(stream("34"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "34");            t=nextToken(cs);
}

void test_tokenize_handles_multiple_atoms() {
  CodeStream cs(stream("34 abc 3.4"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "34");            t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
  checkEq(t, "3.4");           t=nextToken(cs);
}

void test_tokenize_handles_string_literal() {
  CodeStream cs(stream("34 \"abc\""));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "34");            t=nextToken(cs);
  checkEq(t, "\"abc\"");       t=nextToken(cs);
}

void test_tokenize_handles_multiple_lines() {
  CodeStream cs(stream("34\n\"abc\""));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "34");            t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "\"abc\"");       t=nextToken(cs);
}

void test_tokenize_handles_string_with_space() {
  CodeStream cs(stream("34\n\"abc def\""));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "34");            t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "\"abc def\"");   t=nextToken(cs);
}

void test_tokenize_handles_string_with_escape() {
  CodeStream cs(stream("34\n\"abc \\\"quote def\""));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "34");            t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  CodeStream cs(stream("',35"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "'");             t=nextToken(cs);
  checkEq(t, ",");             t=nextToken(cs);
  checkEq(t, "35");            t=nextToken(cs);
}

void test_tokenize_handles_quote_comma_paren() {
  CodeStream cs(stream("(',)"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "(");             t=nextToken(cs);
  checkEq(t, "'");             t=nextToken(cs);
  checkEq(t, ",");             t=nextToken(cs);
  checkEq(t, ")");             t=nextToken(cs);
}

void test_tokenize_handles_splice_operators() {
  CodeStream cs(stream("()',@ @, @b"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "(");             t=nextToken(cs);
  checkEq(t, ")");             t=nextToken(cs);
  checkEq(t, "'");             t=nextToken(cs);
  checkEq(t, ",@");            t=nextToken(cs);
  checkEq(t, "@");             t=nextToken(cs);
  checkEq(t, ",");             t=nextToken(cs);
  checkEq(t, "@");             t=nextToken(cs);
  checkEq(t, "b");             t=nextToken(cs);
}

void test_tokenize_handles_comment() {
  CodeStream cs(stream("()',@ #abc def ghi"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "(");             t=nextToken(cs);
  checkEq(t, ")");             t=nextToken(cs);
  checkEq(t, "'");             t=nextToken(cs);
  checkEq(t, ",@");            t=nextToken(cs);
}

void test_tokenize_ends_comment_at_newline() {
  CodeStream cs(stream("#abc def ghi\nabc"));
  Token t = nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
}

void test_tokenize_suppresses_comments() {
  CodeStream cs(stream("abc\n#abc\ndef\nghi"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "ghi");           t=nextToken(cs);
}

void test_tokenize_suppresses_comments2() {
  CodeStream cs(stream("a b\n  c\n#abc\ndef\n  ghi\n\njkl"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "a");             t=nextToken(cs);
  checkEq(t, "b");             t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(2));       t=nextToken(cs);
  checkEq(t, "c");             t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(2));       t=nextToken(cs);
  checkEq(t, "ghi");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "jkl");           t=nextToken(cs);
}

void test_tokenize_suppresses_trailing_whitespace() {
  CodeStream cs(stream("a \nb\r\nc"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "a");             t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "b");             t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "c");             t=nextToken(cs);
}

void test_tokenize_suppresses_repeated_newline() {
  CodeStream cs(stream("34\n\n\"abc \\\"quote def\""));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "34");            t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  CodeStream cs(stream("abc def ghi\n\n    abc\n  def"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
  checkEq(t, "ghi");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(4));       t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(2));       t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
}

void test_tokenize_suppresses_whitespace_lines() {
  CodeStream cs(stream("abc def ghi\n\n    \n  def"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
  checkEq(t, "ghi");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(2));       t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
}

void test_tokenize_suppresses_whitespace_lines2() {
  CodeStream cs(stream("  \nabc def ghi\n\n    \n  def"));
  Token t = nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
  checkEq(t, "ghi");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(2));       t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
}

void test_tokenize_handles_sexpr() {
  CodeStream cs(stream("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef"));
  Token t = nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "(");             t=nextToken(cs);
  checkEq(t, "'");             t=nextToken(cs);
  checkEq(t, "a");             t=nextToken(cs);
  checkEq(t, "'");             t=nextToken(cs);
  checkEq(t, "(");             t=nextToken(cs);
  checkEq(t, "boo");           t=nextToken(cs);
  checkEq(t, ")");             t=nextToken(cs);
  checkEq(t, "\"foo\nbar\"");  t=nextToken(cs);
  checkEq(t, "`");             t=nextToken(cs);
  checkEq(t, "c");             t=nextToken(cs);
  checkEq(t, "`");             t=nextToken(cs);
  checkEq(t, ",");             t=nextToken(cs);
  checkEq(t, "d");             t=nextToken(cs);
  checkEq(t, ",@");            t=nextToken(cs);
  checkEq(t, "e");             t=nextToken(cs);
  checkEq(t, ")");             t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "abc");           t=nextToken(cs);
  checkEq(t, Token::Newline()); t=nextToken(cs);
  checkEq(t, indent(0));       t=nextToken(cs);
  checkEq(t, "def");           t=nextToken(cs);
}

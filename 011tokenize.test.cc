                                  Token indent(long n) { return Token(n); }

void test_tokenize_always_starts_a_line_with_indent() {
  CodeStream c(stream("34"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
}

void test_tokenize_handles_multiple_atoms() {
  CodeStream c(stream("34 abc 3.4"));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, "abc");           t=nextToken(c);
  checkEq(t, "3.4");           t=nextToken(c);
}

void test_tokenize_handles_string_literal() {
  CodeStream c(stream("34 \"abc\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, "\"abc\"");       t=nextToken(c);
}

void test_tokenize_handles_multiple_lines() {
  CodeStream c(stream("34\n\"abc\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "\"abc\"");       t=nextToken(c);
}

void test_tokenize_handles_string_with_space() {
  CodeStream c(stream("34\n\"abc def\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "\"abc def\"");   t=nextToken(c);
}

void test_tokenize_handles_string_with_escape() {
  CodeStream c(stream("34\n\"abc \\\"quote def\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "34");            t=nextToken(c);
  checkEq(t, indent(0));       t=nextToken(c);
  checkEq(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  CodeStream c(stream("',35"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, ",");            t=nextToken(c);
  checkEq(t, "35");           t=nextToken(c);
}

void test_tokenize_handles_quote_comma_paren() {
  CodeStream c(stream("(',)"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "(");            t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, ",");            t=nextToken(c);
  checkEq(t, ")");            t=nextToken(c);
}

void test_tokenize_doesnt_break_comma_right_after_ssyntax_char() {
  CodeStream c(stream("'a:,b"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, "a:,b");         t=nextToken(c);
}

void test_tokenize_handles_splice_operators() {
  CodeStream c(stream("()',@ @, @b"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
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
  CodeStream c(stream("()',@ ;abc def ghi"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "(");            t=nextToken(c);
  checkEq(t, ")");            t=nextToken(c);
  checkEq(t, "'");            t=nextToken(c);
  checkEq(t, ",@");           t=nextToken(c);
}

void test_tokenize_ends_comment_at_newline() {
  CodeStream c(stream(";abc def ghi\nabc"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
}

void test_tokenize_suppresses_comments() {
  CodeStream c(stream("abc\n;abc\ndef\nghi"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
}

void test_tokenize_suppresses_comments2() {
  CodeStream c(stream("a : b\n  : c\n;abc\ndef :\n  ghi\n\njkl"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "a");            t=nextToken(c);
  checkEq(t, "b");            t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "c");            t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "jkl");          t=nextToken(c);
}

void test_tokenize_suppresses_trailing_whitespace() {
  CodeStream c(stream("a \nb\r\nc"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "a");            t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "b");            t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "c");            t=nextToken(c);
}

void test_tokenize_suppresses_repeated_newline() {
  CodeStream c(stream("34\n\n\"abc \\\"quote def\""));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "34");           t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  CodeStream c(stream("abc def ghi\n\n    abc\n  def"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, indent(4));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

void test_tokenize_suppresses_whitespace_lines() {
  CodeStream c(stream("abc def ghi\n\n    \n  def"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

void test_tokenize_suppresses_whitespace_lines2() {
  CodeStream c(stream("  \nabc def ghi\n\n    \n  def"));
  Token t = nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
  checkEq(t, "ghi");          t=nextToken(c);
  checkEq(t, indent(2));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

void test_tokenize_handles_sexpr() {
  CodeStream c(stream("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\ndef"));
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
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "abc");          t=nextToken(c);
  checkEq(t, indent(0));      t=nextToken(c);
  checkEq(t, "def");          t=nextToken(c);
}

void test_tokenize_populates_spaces_before() {
  CodeStream c(stream("  \nabc (def  ghi \n\n    \n  def"));
  Token t = nextToken(c);
  checkEq(t, indent(0));        t=nextToken(c);
                                t=nextToken(c);   // abc
  checkEq(t.spacesBefore, 1);   t=nextToken(c);   // (
  checkEq(t.spacesBefore, 0);   t=nextToken(c);   // def
  checkEq(t.spacesBefore, 2);   t=nextToken(c);   // ghi
  checkEq(t, indent(2));        t=nextToken(c);
                                t=nextToken(c);   // def
}

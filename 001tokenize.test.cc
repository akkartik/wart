void test_tokenize_handles_empty_input() {
  CodeStream c(stream(""));
}

void test_tokenize_always_starts_a_line_with_indent() {
  CodeStream c(stream("34"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "34");
}

void test_tokenize_handles_multiple_atoms() {
  CodeStream c(stream("34 abc 3.4"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "34");
  checkEq(nextToken(c), "abc");
  checkEq(nextToken(c), "3.4");
}

void test_tokenize_handles_string_literal() {
  CodeStream c(stream("34 \"abc\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "34");
  checkEq(nextToken(c), "\"abc\"");
}

void test_tokenize_handles_multiple_lines() {
  CodeStream c(stream("34\n\"abc\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "\"abc\"");
}

void test_tokenize_handles_string_with_space() {
  CodeStream c(stream("34\n\"abc def\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "\"abc def\"");
}

void test_tokenize_handles_string_with_escape() {
  CodeStream c(stream("34\n\"abc \\\"quote def\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  CodeStream c(stream("',35"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "'");
  checkEq(nextToken(c), ",");
  checkEq(nextToken(c), "35");
}

void test_tokenize_handles_quote_comma_paren() {
  CodeStream c(stream("(',)"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "(");
  checkEq(nextToken(c), "'");
  checkEq(nextToken(c), ",");
  checkEq(nextToken(c), ")");
}

void test_tokenize_doesnt_break_comma_right_after_ssyntax_char() {
  CodeStream c(stream("'a:,b"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "'");
  checkEq(nextToken(c), "a:,b");
}

void test_tokenize_handles_splice_operators() {
  CodeStream c(stream("()',@ @, @b"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "(");
  checkEq(nextToken(c), ")");
  checkEq(nextToken(c), "'");
  checkEq(nextToken(c), ",@");
  checkEq(nextToken(c), "@");
  checkEq(nextToken(c), ",");
  checkEq(nextToken(c), "@");
  checkEq(nextToken(c), "b");
}

void test_tokenize_handles_comment() {
  CodeStream c(stream("()',@ ;abc def ghi"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "(");
  checkEq(nextToken(c), ")");
  checkEq(nextToken(c), "'");
  checkEq(nextToken(c), ",@");
}

void test_tokenize_ends_comment_at_newline() {
  CodeStream c(stream(";abc def ghi\nabc"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "abc");
}

void test_tokenize_suppresses_comments() {
  CodeStream c(stream("abc\n;abc\ndef\nghi"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "abc");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "def");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "ghi");
}

void test_tokenize_suppresses_comments2() {
  CodeStream c(stream("a b\n  c\n;abc\ndef\n  ghi\n\njkl"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "a");
  checkEq(nextToken(c), "b");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), "c");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "def");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), "ghi");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "jkl");
}

void test_tokenize_suppresses_trailing_whitespace() {
  CodeStream c(stream("a \nb\r\nc"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "a");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "b");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "c");
}

void test_tokenize_suppresses_repeated_newline() {
  CodeStream c(stream("34\n\n\"abc \\\"quote def\""));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "34");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  CodeStream c(stream("abc def ghi\n\n    abc\n  def"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "abc");
  checkEq(nextToken(c), "def");
  checkEq(nextToken(c), "ghi");
  checkEq(nextToken(c), Token::indent(4));
  checkEq(nextToken(c), "abc");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), "def");
}

void test_tokenize_suppresses_whitespace_lines() {
  CodeStream c(stream("abc def ghi\n\n    \n  def"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "abc");
  checkEq(nextToken(c), "def");
  checkEq(nextToken(c), "ghi");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), "def");
}

void test_tokenize_suppresses_whitespace_lines2() {
  CodeStream c(stream("  \nabc def ghi\n\n    \n  def"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "abc");
  checkEq(nextToken(c), "def");
  checkEq(nextToken(c), "ghi");
  checkEq(nextToken(c), Token::indent(2));
  checkEq(nextToken(c), "def");
}

void test_tokenize_handles_sexpr() {
  CodeStream c(stream("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\ndef"));
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "(");
  checkEq(nextToken(c), "'");
  checkEq(nextToken(c), "a");
  checkEq(nextToken(c), "'");
  checkEq(nextToken(c), "(");
  checkEq(nextToken(c), "boo");
  checkEq(nextToken(c), ")");
  checkEq(nextToken(c), "\"foo\nbar\"");
  checkEq(nextToken(c), "`");
  checkEq(nextToken(c), "c");
  checkEq(nextToken(c), "`");
  checkEq(nextToken(c), ",");
  checkEq(nextToken(c), "d");
  checkEq(nextToken(c), ",@");
  checkEq(nextToken(c), "e");
  checkEq(nextToken(c), ")");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "abc");
  checkEq(nextToken(c), Token::indent(0));
  checkEq(nextToken(c), "def");
}

                                  token indent(long n) { return token(n); }

void test_tokenize_always_starts_a_line_with_indent() {
  indent_sensitive_stream in("34");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
}

void test_tokenize_handles_multiple_atoms() {
  indent_sensitive_stream in("34 abc 3.4");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "3.4");           t=next_token(in);
}

void test_tokenize_handles_string_literal() {
  indent_sensitive_stream in("34 \"abc\"");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "\"abc\"");       t=next_token(in);
}

void test_tokenize_handles_multiple_lines() {
  indent_sensitive_stream in("34\n\"abc\"");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "\"abc\"");       t=next_token(in);
}

void test_tokenize_handles_string_with_space() {
  indent_sensitive_stream in("34\n\"abc def\"");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "\"abc def\"");   t=next_token(in);
}

void test_tokenize_handles_string_with_escape() {
  indent_sensitive_stream in("34\n\"abc \\\"quote def\"");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  indent_sensitive_stream in("',35");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, ",");             t=next_token(in);
  CHECK_EQ(t, "35");            t=next_token(in);
}

void test_tokenize_handles_quote_comma_paren() {
  indent_sensitive_stream in("(',)");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "(");             t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, ",");             t=next_token(in);
  CHECK_EQ(t, ")");             t=next_token(in);
}

void test_tokenize_handles_splice_operators() {
  indent_sensitive_stream in("()',@ @, @b");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "(");             t=next_token(in);
  CHECK_EQ(t, ")");             t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, ",@");            t=next_token(in);
  CHECK_EQ(t, "@");             t=next_token(in);
  CHECK_EQ(t, ",");             t=next_token(in);
  CHECK_EQ(t, "@");             t=next_token(in);
  CHECK_EQ(t, "b");             t=next_token(in);
}

void test_tokenize_handles_comment() {
  indent_sensitive_stream in("()',@ #abc def ghi");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "(");             t=next_token(in);
  CHECK_EQ(t, ")");             t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, ",@");            t=next_token(in);
}

void test_tokenize_ends_comment_at_newline() {
  indent_sensitive_stream in("#abc def ghi\nabc");
  token t = next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
}

void test_tokenize_suppresses_comments() {
  indent_sensitive_stream in("abc\n#abc\ndef\nghi");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
}

void test_tokenize_suppresses_comments2() {
  indent_sensitive_stream in("a : b\n  : c\n#abc\ndef :\n  ghi\n\njkl");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "a");             t=next_token(in);
  CHECK_EQ(t, "b");             t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(2));       t=next_token(in);
  CHECK_EQ(t, "c");             t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(2));       t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "jkl");           t=next_token(in);
}

void test_tokenize_suppresses_trailing_whitespace() {
  indent_sensitive_stream in("a \nb\r\nc");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "a");             t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "b");             t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "c");             t=next_token(in);
}

void test_tokenize_suppresses_repeated_newline() {
  indent_sensitive_stream in("34\n\n\"abc \\\"quote def\"");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  indent_sensitive_stream in("abc def ghi\n\n    abc\n  def");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(4));       t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(2));       t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
}

void test_tokenize_suppresses_whitespace_lines() {
  indent_sensitive_stream in("abc def ghi\n\n    \n  def");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(2));       t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
}

void test_tokenize_suppresses_whitespace_lines2() {
  indent_sensitive_stream in("  \nabc def ghi\n\n    \n  def");
  token t = next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(2));       t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
}

void test_tokenize_handles_sexpr() {
  indent_sensitive_stream in("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef");
  token t = next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "(");             t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, "a");             t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, "(");             t=next_token(in);
  CHECK_EQ(t, "boo");           t=next_token(in);
  CHECK_EQ(t, ")");             t=next_token(in);
  CHECK_EQ(t, "\"foo\nbar\"");  t=next_token(in);
  CHECK_EQ(t, "`");             t=next_token(in);
  CHECK_EQ(t, "c");             t=next_token(in);
  CHECK_EQ(t, "`");             t=next_token(in);
  CHECK_EQ(t, ",");             t=next_token(in);
  CHECK_EQ(t, "d");             t=next_token(in);
  CHECK_EQ(t, ",@");            t=next_token(in);
  CHECK_EQ(t, "e");             t=next_token(in);
  CHECK_EQ(t, ")");             t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, token::Newline()); t=next_token(in);
  CHECK_EQ(t, indent(0));       t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
}

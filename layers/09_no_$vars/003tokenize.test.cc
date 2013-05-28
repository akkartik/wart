void test_tokenize_handles_multiple_atoms() {
  stringstream in("34 abc 3.4");
  token t = next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "3.4");           t=next_token(in);
}

void test_tokenize_handles_string_literal() {
  stringstream in("34 \"abc\"");
  token t = next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "\"abc\"");       t=next_token(in);
}

void test_tokenize_handles_multiple_lines() {
  stringstream in("34\n\"abc\"");
  token t = next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "\"abc\"");       t=next_token(in);
}

void test_tokenize_handles_string_with_space() {
  stringstream in("34\n\"abc def\"");
  token t = next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "\"abc def\"");   t=next_token(in);
}

void test_tokenize_handles_string_with_escape() {
  stringstream in("34\n\"abc \\\"quote def\"");
  token t = next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  stringstream in("',35");
  token t = next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, ",");             t=next_token(in);
  CHECK_EQ(t, "35");            t=next_token(in);
}

void test_tokenize_handles_quote_comma_paren() {
  stringstream in("(',)");
  token t = next_token(in);
  CHECK_EQ(t, "(");             t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, ",");             t=next_token(in);
  CHECK_EQ(t, ")");             t=next_token(in);
}

void test_tokenize_handles_splice_operators() {
  stringstream in("()',@ @, @b");
  token t = next_token(in);
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
  stringstream in("()',@ #abc def ghi");
  token t = next_token(in);
  CHECK_EQ(t, "(");             t=next_token(in);
  CHECK_EQ(t, ")");             t=next_token(in);
  CHECK_EQ(t, "'");             t=next_token(in);
  CHECK_EQ(t, ",@");            t=next_token(in);
}

void test_tokenize_ends_comment_at_newline() {
  stringstream in("#abc def ghi\nabc");
  token t = next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
}

void test_tokenize_suppresses_comments() {
  stringstream in("abc\n#abc\ndef\nghi");
  token t = next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
}

void test_tokenize_suppresses_comments2() {
  stringstream in("a : b\n  : c\n#abc\ndef :\n  ghi\n\njkl");
  token t = next_token(in);
  CHECK_EQ(t, "a");             t=next_token(in);
  CHECK_EQ(t, "b");             t=next_token(in);
  CHECK_EQ(t, "c");             t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
  CHECK_EQ(t, "jkl");           t=next_token(in);
}

void test_tokenize_suppresses_trailing_whitespace() {
  stringstream in("a \nb\r\nc");
  token t = next_token(in);
  CHECK_EQ(t, "a");             t=next_token(in);
  CHECK_EQ(t, "b");             t=next_token(in);
  CHECK_EQ(t, "c");             t=next_token(in);
}

void test_tokenize_suppresses_repeated_newline() {
  stringstream in("34\n\n\"abc \\\"quote def\"");
  token t = next_token(in);
  CHECK_EQ(t, "34");            t=next_token(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_suppresses_whitespace_lines() {
  stringstream in("abc def ghi\n\n    \n  def");
  token t = next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
}

void test_tokenize_suppresses_whitespace_lines2() {
  stringstream in("  \nabc def ghi\n\n    \n  def");
  token t = next_token(in);
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
  CHECK_EQ(t, "ghi");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
}

void test_tokenize_handles_sexpr() {
  stringstream in("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef");
  token t = next_token(in);
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
  CHECK_EQ(t, "abc");           t=next_token(in);
  CHECK_EQ(t, "def");           t=next_token(in);
}

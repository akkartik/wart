                                  Token indent(long n) { return Token(n); }

void test_tokenize_always_starts_a_line_with_indent() {
  stringstream in("34");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "34");            ++p;
}

void test_tokenize_handles_multiple_atoms() {
  stringstream in("34 abc 3.4");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "34");            ++p;
  checkEq(*p, "abc");           ++p;
  checkEq(*p, "3.4");           ++p;
}

void test_tokenize_handles_string_literal() {
  stringstream in("34 \"abc\"");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "34");            ++p;
  checkEq(*p, "\"abc\"");       ++p;
}

void test_tokenize_handles_multiple_lines() {
  stringstream in("34\n\"abc\"");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "34");            ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "\"abc\"");       ++p;
}

void test_tokenize_handles_string_with_space() {
  stringstream in("34\n\"abc def\"");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "34");            ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "\"abc def\"");   ++p;
}

void test_tokenize_handles_string_with_escape() {
  stringstream in("34\n\"abc \\\"quote def\"");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "34");            ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_quote_comma() {
  stringstream in("',35");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "'");             ++p;
  checkEq(*p, ",");             ++p;
  checkEq(*p, "35");            ++p;
}

void test_tokenize_handles_quote_comma_paren() {
  stringstream in("(',)");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "(");             ++p;
  checkEq(*p, "'");             ++p;
  checkEq(*p, ",");             ++p;
  checkEq(*p, ")");             ++p;
}

void test_tokenize_handles_splice_operators() {
  stringstream in("()',@ @, @b");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "(");             ++p;
  checkEq(*p, ")");             ++p;
  checkEq(*p, "'");             ++p;
  checkEq(*p, ",@");            ++p;
  checkEq(*p, "@");             ++p;
  checkEq(*p, ",");             ++p;
  checkEq(*p, "@");             ++p;
  checkEq(*p, "b");             ++p;
}

void test_tokenize_handles_comment() {
  stringstream in("()',@ #abc def ghi");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "(");             ++p;
  checkEq(*p, ")");             ++p;
  checkEq(*p, "'");             ++p;
  checkEq(*p, ",@");            ++p;
}

void test_tokenize_ends_comment_at_newline() {
  stringstream in("#abc def ghi\nabc");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "abc");           ++p;
}

void test_tokenize_suppresses_comments() {
  stringstream in("abc\n#abc\ndef\nghi");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "abc");           ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "def");           ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "ghi");           ++p;
}

void test_tokenize_suppresses_comments2() {
  stringstream in("a b\n  c\n#abc\ndef\n  ghi\n\njkl");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "a");             ++p;
  checkEq(*p, "b");             ++p;
  checkEq(*p, indent(2));       ++p;
  checkEq(*p, "c");             ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "def");           ++p;
  checkEq(*p, indent(2));       ++p;
  checkEq(*p, "ghi");           ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "jkl");           ++p;
}

void test_tokenize_suppresses_trailing_whitespace() {
  stringstream in("a \nb\r\nc");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "a");             ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "b");             ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "c");             ++p;
}

void test_tokenize_suppresses_repeated_newline() {
  stringstream in("34\n\n\"abc \\\"quote def\"");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "34");            ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_indent_outdent() {
  stringstream in("abc def ghi\n\n    abc\n  def");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "abc");           ++p;
  checkEq(*p, "def");           ++p;
  checkEq(*p, "ghi");           ++p;
  checkEq(*p, indent(4));       ++p;
  checkEq(*p, "abc");           ++p;
  checkEq(*p, indent(2));       ++p;
  checkEq(*p, "def");           ++p;
}

void test_tokenize_suppresses_whitespace_lines() {
  stringstream in("abc def ghi\n\n    \n  def");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "abc");           ++p;
  checkEq(*p, "def");           ++p;
  checkEq(*p, "ghi");           ++p;
  checkEq(*p, indent(2));       ++p;
  checkEq(*p, "def");           ++p;
}

void test_tokenize_suppresses_whitespace_lines2() {
  stringstream in("  \nabc def ghi\n\n    \n  def");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "abc");           ++p;
  checkEq(*p, "def");           ++p;
  checkEq(*p, "ghi");           ++p;
  checkEq(*p, indent(2));       ++p;
  checkEq(*p, "def");           ++p;
}

void test_tokenize_handles_sexpr() {
  stringstream in("('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc #def ghi\ndef");
  list<Token> tokens = tokenize(in);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "(");             ++p;
  checkEq(*p, "'");             ++p;
  checkEq(*p, "a");             ++p;
  checkEq(*p, "'");             ++p;
  checkEq(*p, "(");             ++p;
  checkEq(*p, "boo");           ++p;
  checkEq(*p, ")");             ++p;
  checkEq(*p, "\"foo\nbar\"");  ++p;
  checkEq(*p, "`");             ++p;
  checkEq(*p, "c");             ++p;
  checkEq(*p, "`");             ++p;
  checkEq(*p, ",");             ++p;
  checkEq(*p, "d");             ++p;
  checkEq(*p, ",@");            ++p;
  checkEq(*p, "e");             ++p;
  checkEq(*p, ")");             ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "abc");           ++p;
  checkEq(*p, indent(0));       ++p;
  checkEq(*p, "def");           ++p;
}

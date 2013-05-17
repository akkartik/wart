void test_tracing_tokenize_handles_multiple_atoms() {
  stringstream in("34 abc 3.4");
  readAll(in);
  checkTraceContents("tokenize", "34\nabc\n3.4\n");
}

void test_tokenize_handles_string_literal() {
  stringstream in("34 \"abc\"");
  readAll(in);
  checkTraceContents("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_multiple_lines() {
  stringstream in("34\n\"abc\"");
  readAll(in);
  checkTraceContents("tokenize", "34\n\"abc\"\n");
}

void test_tokenize_handles_string_with_space() {
  stringstream in("34\n\"abc def\"");
  Token t = nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, "\"abc def\"");   t=nextToken(in);
}

void test_tokenize_handles_string_with_escape() {
  stringstream in("34\n\"abc \\\"quote def\"");
  Token t = nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_handles_comment() {
  stringstream in("()'a #abc def ghi");
  Token t = nextToken(in);
  CHECK_EQ(t, "(");             t=nextToken(in);
  CHECK_EQ(t, ")");             t=nextToken(in);
  CHECK_EQ(t, "'");             t=nextToken(in);
  CHECK_EQ(t, "a");            t=nextToken(in);
}

void test_tokenize_ends_comment_at_newline() {
  stringstream in("#abc def ghi\nabc");
  Token t = nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
}

void test_tokenize_suppresses_comments() {
  stringstream in("abc\n#abc\ndef\nghi");
  Token t = nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
}

void test_tokenize_suppresses_comments2() {
  stringstream in("a b\n  c\n#abc\ndef\n  ghi\n\njkl");
  Token t = nextToken(in);
  CHECK_EQ(t, "a");             t=nextToken(in);
  CHECK_EQ(t, "b");             t=nextToken(in);
  CHECK_EQ(t, "c");             t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
  CHECK_EQ(t, "jkl");           t=nextToken(in);
}

void test_tokenize_suppresses_trailing_whitespace() {
  stringstream in("a \nb\r\nc");
  Token t = nextToken(in);
  CHECK_EQ(t, "a");             t=nextToken(in);
  CHECK_EQ(t, "b");             t=nextToken(in);
  CHECK_EQ(t, "c");             t=nextToken(in);
}

void test_tokenize_suppresses_repeated_newline() {
  stringstream in("34\n\n\"abc \\\"quote def\"");
  Token t = nextToken(in);
  CHECK_EQ(t, "34");            t=nextToken(in);
  CHECK_EQ(t, "\"abc \\\"quote def\"");
}

void test_tokenize_suppresses_whitespace_lines() {
  stringstream in("abc def ghi\n\n    \n  def");
  Token t = nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
}

void test_tokenize_suppresses_whitespace_lines2() {
  stringstream in("  \nabc def ghi\n\n    \n  def");
  Token t = nextToken(in);
  CHECK_EQ(t, "abc");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
  CHECK_EQ(t, "ghi");           t=nextToken(in);
  CHECK_EQ(t, "def");           t=nextToken(in);
}

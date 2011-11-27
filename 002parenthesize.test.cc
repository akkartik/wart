void test_parenthesize_handles_lines_with_initial_parens() {
  CodeStream c(stream("(a b c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_skips_indent_tokens() {
  CodeStream c(stream("  (a\tb c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_skips_outdent_tokens() {
  CodeStream c(stream("(a b c\n  bc\n    def\n  gh)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "bc"); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "gh"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  CodeStream c(stream("(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "gh"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "i"); ++p;
  checkEq(*p, "j"); ++p;
  checkEq(*p, "k"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "lm"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "no"); ++p;
  checkEq(*p, "p"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_single_word_lines() {
  CodeStream c(stream("a"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "a"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_single_word_lines2() {
  CodeStream c(stream("a  \nb\nc"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "a"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "b"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "c"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_words_on_single_line() {
  CodeStream c(stream("a b c  "));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  CodeStream c(stream(" a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_quoted_words() {
  CodeStream c(stream(",a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, ","); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_quoted_words2() {
  CodeStream c(stream(",@a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, ",@"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_quoted_words3() {
  CodeStream c(stream("'a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "'"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_nested_quoted_words() {
  CodeStream c(stream("a b\n  'c\n  ,d\n  @e"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "'"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ","); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "@"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_quoted_groups() {
  CodeStream c(stream(",(a b c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ","); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_quoted_groups2() {
  CodeStream c(stream(",@(a b c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ",@"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_words_on_single_indented_line() {
  CodeStream c(stream("    a b c\n  34"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "34"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  CodeStream c(stream("a b c  \nd ef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "ef"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_across_indent() {
  CodeStream c(stream("a b c  \n  d ef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "ef"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_across_indent2() {
  CodeStream c(stream("a b c  \n  (d ef)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "ef"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_across_indent3() {
  CodeStream c(stream("a b c  \n  (d ef)\n\n  g"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "ef"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "g"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_nested_indents() {
  CodeStream c(stream("a b c\n  d e\n    f\ny"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, "f"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "y"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_handles_quotes_and_comments() {
  CodeStream c(stream("a b c  \n  '(d ef)\n\n  g ;abc"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "'"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "ef"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "g"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_before_outdents() {
  CodeStream c(stream("a b c  \n    '(d ef)\n\n  g ;abc"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "'"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "ef"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "g"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_before_outdents2() {
  CodeStream c(stream("def foo\n    a b c\n  d e\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "newdef"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_before_too_much_outdent() {
  CodeStream c(stream("  a a\n    a\ny"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "y"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_across_comments() {
  CodeStream c(stream("def foo\n;a b c\n  d e\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "newdef"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_inside_parens() {
  CodeStream c(stream("(def foo\n    ;a b c\n  d e)\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "newdef"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_inside_parens2() {
  CodeStream c(stream("`(def foo\n    ;a b c\n  d e)\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "`"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "newdef"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_groups_inside_indented_parens() {
  CodeStream c(stream("  (a b c\n    d e)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_arglists() {
  CodeStream c(stream("def foo(a (b)\n    c d)\n  d e\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, "newdef"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_when_indented_by_one_space() {
  CodeStream c(stream("    (a b c\n     d e\n     f g)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, "f"); ++p;
  checkEq(*p, "g"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
  check(c.fd.eof());
}

void test_parenthesize_passes_through_unbalanced_open_paren() {
  CodeStream c(stream("("));
  list<Token> tokens = nextExpr(c);
  checkEq(tokens.size(), 1);
  checkEq(tokens.front(), "(");
  check(c.fd.eof());
}

void test_parenthesize_errors_on_unbalanced_closed_paren() {
  CodeStream c(stream(")"));
  list<Token> tokens = nextExpr(c);
  checkEq(errorCount, 1);   errorCount=0;
  check(c.fd.eof());
}

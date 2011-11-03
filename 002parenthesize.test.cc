void test_parenthesize_handles_lines_with_initial_parens() {
  CodeStream c(stream(L"(a b c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_skips_indent_tokens() {
  CodeStream c(stream(L"  (a\tb c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_skips_outdent_tokens() {
  CodeStream c(stream(L"(a b c\n  bc\n    def\n  gh)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"bc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"gh"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  CodeStream c(stream(L"(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"gh"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"i"); ++p;
  checkEq(*p, L"j"); ++p;
  checkEq(*p, L"k"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"lm"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"no"); ++p;
  checkEq(*p, L"p"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_passes_through_single_word_lines() {
  CodeStream c(stream(L"a"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"a"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_passes_through_single_word_lines2() {
  CodeStream c(stream(L"a  \nb\nc"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"a"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"b"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"c"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_words_on_single_line() {
  CodeStream c(stream(L"a b c  "));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  CodeStream c(stream(L" a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_quoted_words() {
  CodeStream c(stream(L",a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_quoted_words2() {
  CodeStream c(stream(L",@a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L",@"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_quoted_words3() {
  CodeStream c(stream(L"'a b c"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_passes_through_nested_quoted_words() {
  CodeStream c(stream(L"a b\n  'c\n  ,d\n  @e"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"@"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_passes_through_quoted_groups() {
  CodeStream c(stream(L",(a b c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L","); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_passes_through_quoted_groups2() {
  CodeStream c(stream(L",@(a b c)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L",@"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_words_on_single_indented_line() {
  CodeStream c(stream(L"    a b c\n  34"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"34"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  CodeStream c(stream(L"a b c  \nd ef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"ef"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_across_indent() {
  CodeStream c(stream(L"a b c  \n  d ef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"ef"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_across_indent2() {
  CodeStream c(stream(L"a b c  \n  (d ef)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"ef"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_across_indent3() {
  CodeStream c(stream(L"a b c  \n  (d ef)\n\n  g"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"ef"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"g"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_nested_indents() {
  CodeStream c(stream(L"a b c\n  d e\n    f\ny"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L"f"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"y"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_handles_quotes_and_comments() {
  CodeStream c(stream(L"a b c  \n  '(d ef)\n\n  g ;abc"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"ef"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"g"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_before_outdents() {
  CodeStream c(stream(L"a b c  \n    '(d ef)\n\n  g ;abc"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"ef"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"g"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_before_outdents2() {
  CodeStream c(stream(L"def foo\n    a b c\n  d e\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_before_too_much_outdent() {
  CodeStream c(stream(L"  a a\n    a\ny"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"y"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_across_comments() {
  CodeStream c(stream(L"def foo\n;a b c\n  d e\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_inside_parens() {
  CodeStream c(stream(L"(def foo\n    ;a b c\n  d e)\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_inside_parens2() {
  CodeStream c(stream(L"`(def foo\n    ;a b c\n  d e)\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"`"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_groups_inside_indented_parens() {
  CodeStream c(stream(L"  (a b c\n    d e)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_passes_through_arglists() {
  CodeStream c(stream(L"def foo(a (b)\n    c d)\n  d e\nnewdef"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  tokens = nextExpr(c); p = tokens.begin();
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

void test_parenthesize_passes_through_when_indented_by_one_space() {
  CodeStream c(stream(L"    (a b c\n     d e\n     f g)"));
  list<Token> tokens = nextExpr(c);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L"f"); ++p;
  checkEq(*p, L"g"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
  check(nextExpr(c).empty());
}

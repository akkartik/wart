void test_parenthesize_handles_lines_with_initial_parens() {
  stringstream in("(a b c)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_skips_indent_tokens() {
  stringstream in("  (a\tb c)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_skips_outdent_tokens() {
  stringstream in("(a b c\n  bc\n    def\n  gh)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  stringstream in("(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_passes_through_single_word_lines() {
  stringstream in("a");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "a"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_single_word_lines2() {
  stringstream in("a  \nb\nc");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line() {
  stringstream in("a b c  ");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  stringstream in(" a b c");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_quoted_words() {
  stringstream in(",a b c");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, ","); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_quoted_words2() {
  stringstream in(",@a b c");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, ",@"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_quoted_words3() {
  stringstream in("'a b c");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "'"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_nested_quoted_words() {
  stringstream in("a b\n  'c\n  ,d\n  @e");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_passes_through_quoted_groups() {
  stringstream in(",(a b c)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ","); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups2() {
  stringstream in(",@(a b c)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ",@"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups3() {
  stringstream in(",,(a b c)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ","); ++p;
  checkEq(*p, ","); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_indented_line() {
  stringstream in("    a b c\n  34");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "34"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  stringstream in("a b c  \nd ef");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "ef"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_across_indent() {
  stringstream in("a b c  \n  d ef");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_groups_across_indent2() {
  stringstream in("a b c  \n  (d ef)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_groups_across_indent3() {
  stringstream in("a b c  \n  (d ef)\n\n  g");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_groups_nested_indents() {
  stringstream in("a b c\n  d e\n    f\ny");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
  checkEq(*p, "y"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_handles_quotes_and_comments() {
  stringstream in("a b c  \n  '(d ef)\n\n  g #abc");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_groups_before_outdents() {
  stringstream in("a b c  \n    '(d ef)\n\n  g #abc");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
}

void test_parenthesize_groups_before_outdents2() {
  stringstream in("def foo\n    a b c\n  d e\nnewdef");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
  checkEq(*p, "newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_before_too_much_outdent() {
  stringstream in("  a a\n    a\ny");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "y"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_across_comments() {
  stringstream in("def foo\n#a b c\n  d e\nnew");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "new"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens() {
  stringstream in("(def foo\n    #a b c\n  d e)\nnew");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "new"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens2() {
  stringstream in("`(def foo\n    #a b c\n  d e)\nnew");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "`"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "def"); ++p;
  checkEq(*p, "foo"); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, "new"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens3() {
  stringstream in("  (a b c\n    d e)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, "c"); ++p;
  checkEq(*p, "d"); ++p;
  checkEq(*p, "e"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_arglists() {
  stringstream in("def foo(a (b)\n    c d)\n  d e\nnew");
  list<Token> tokens = insertImplicitParens(tokenize(in));
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
  checkEq(*p, "new"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_unbalanced_open_paren() {
  stringstream in("(");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  checkEq(tokens.size(), 1);
  checkEq(tokens.front(), "(");
}

void test_parenthesize_errors_on_unbalanced_closed_paren() {
  stringstream in(")");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  check(raiseCount > 0);   raiseCount=0;
}

void test_parenthesize_handles_early_paren() {
  stringstream in("a (b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == tokens.end());
}

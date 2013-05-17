void test_parenthesize_handles_initial_comment() {
  IndentSensitiveStream in("#a\na b");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_lines_with_initial_parens() {
  IndentSensitiveStream in("(a b c)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_lines_with_initial_parens2() {
  IndentSensitiveStream in("(a (b c))");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_skips_indent_tokens() {
  IndentSensitiveStream in("  (a\tb c)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_skips_outdent_tokens() {
  IndentSensitiveStream in("(a b c\n  bc\n    def\n  gh)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "bc"); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "gh"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_preserves_following_indent() {
  IndentSensitiveStream in("a\n  b");
  nextExpr(in);
  CHECK_EQ(in.fd.peek(), ' ');
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  IndentSensitiveStream in("(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "gh"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "i"); ++p;
  CHECK_EQ(*p, "j"); ++p;
  CHECK_EQ(*p, "k"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "lm"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "no"); ++p;
  CHECK_EQ(*p, "p"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_single_word_lines() {
  IndentSensitiveStream in("a");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "a"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_single_word_lines2() {
  IndentSensitiveStream in("a  \nb\nc");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "a"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "b"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "c"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line() {
  IndentSensitiveStream in("a b c  ");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line2() {
  IndentSensitiveStream in("a (b c)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line4() {
  IndentSensitiveStream in("a (b c) d");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line3() {
  IndentSensitiveStream in("a `(b c)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "`"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_words_on_single_line() {
  IndentSensitiveStream in("(a () b)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  IndentSensitiveStream in(" a b c");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_quoted_words() {
  IndentSensitiveStream in(",a b c");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_quoted_words2() {
  IndentSensitiveStream in(",@a b c");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ",@"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_quoted_words3() {
  IndentSensitiveStream in("'a b c");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_nested_quoted_words() {
  IndentSensitiveStream in("a b\n  'c\n  ,d\n  e");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups() {
  IndentSensitiveStream in(",(a b c)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups2() {
  IndentSensitiveStream in(",@(a b c)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, ",@"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups3() {
  IndentSensitiveStream in(",,(a b c)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_indented_line() {
  IndentSensitiveStream in("    a b c\n  34");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p=tokens.begin();
  CHECK_EQ(*p, "34"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  IndentSensitiveStream in("a b c  \nd ef");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p=tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "ef"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_across_indent() {
  IndentSensitiveStream in("a b c  \n  d ef");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "ef"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_across_indent2() {
  IndentSensitiveStream in("a b c  \n  (d ef)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "ef"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_across_indent3() {
  IndentSensitiveStream in("a b c  \n  (d ef)\n\n  g");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "ef"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "g"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_nested_indents() {
  IndentSensitiveStream in("a b c\n  d e\n    f\ny");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, "f"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "y"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_quotes_and_comments() {
  IndentSensitiveStream in("a b c  \n  '(d ef)\n\n  : g #abc");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "ef"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "g"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_takes_indent_from_colon() {
  IndentSensitiveStream in("a b c  \n    d ef\n  : g");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "(");     ++p;
  CHECK_EQ(*p, "a");     ++p;
  CHECK_EQ(*p, "b");     ++p;
  CHECK_EQ(*p, "c");     ++p;
  CHECK_EQ(*p, "(");     ++p;
  CHECK_EQ(*p, "d");     ++p;
  CHECK_EQ(*p, "ef");    ++p;
  CHECK_EQ(*p, ")");     ++p;
  CHECK_EQ(*p, "g");     ++p;
  CHECK_EQ(*p, ")");     ++p;
  CHECK(p == tokens.end());
  CHECK(in.eof());
}

void test_parenthesize_groups_before_outdents() {
  IndentSensitiveStream in("a b c  \n    '(d ef)\n\n  g #abc");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "ef"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "g"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_before_outdents2() {
  IndentSensitiveStream in("def foo\n    a b c\n  d e\nnewdef");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "newdef"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_before_too_much_outdent() {
  IndentSensitiveStream in("  a a\n    a\ny");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "y"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_across_comments() {
  IndentSensitiveStream in("def foo\n#a b c\n  d e\nnew");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens() {
  IndentSensitiveStream in("(def foo\n    #a b c\n  d e)\nnew");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens2() {
  IndentSensitiveStream in("`(def foo\n    #a b c\n  d e)\nnew");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "`"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens3() {
  IndentSensitiveStream in("  (a b c\n    d e)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_arglists() {
  IndentSensitiveStream in("def foo(a (b)\n    c d)\n  d e\nnew");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_unbalanced_open_paren() {
  IndentSensitiveStream in("(");
  list<Token> tokens = nextExpr(in);
  CHECK_EQ(tokens.size(), 1);
  CHECK_EQ(tokens.front(), "(");
}

void test_parenthesize_errors_on_unbalanced_closed_paren() {
  IndentSensitiveStream in(")");
  list<Token> tokens = nextExpr(in);
  CHECK(raiseCount > 0);   raiseCount=0;
}

void test_parenthesize_handles_early_paren() {
  IndentSensitiveStream in("a (b)");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_multiple_exprs_on_a_line() {
  IndentSensitiveStream in("() 1 2");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "1"); ++p;
  CHECK(p == tokens.end());
  tokens = nextExpr(in); p = tokens.begin();
  CHECK_EQ(*p, "2"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_breaks_at_empty_lines_when_interactive() {
  IndentSensitiveStream in("34 35\n\n  36");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "34"); ++p;
  CHECK_EQ(*p, "35"); ++p;
  CHECK_EQ(*p, "36"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  interactive = true;
  rewind(in);
  tokens = nextExpr(in);
    p = tokens.begin();
    CHECK_EQ(*p, "("); ++p;
    CHECK_EQ(*p, "34"); ++p;
    CHECK_EQ(*p, "35"); ++p;
    CHECK_EQ(*p, ")"); ++p;
    CHECK(p == tokens.end());
  interactive = false;
}

void test_parenthesize_breaks_indent_at_empty_lines_when_interactive() {
  IndentSensitiveStream in("a b\n  c\n\n  d");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  interactive = true;
  rewind(in);
  tokens = nextExpr(in);
    p = tokens.begin();
    CHECK_EQ(*p, "("); ++p;
    CHECK_EQ(*p, "a"); ++p;
    CHECK_EQ(*p, "b"); ++p;
    CHECK_EQ(*p, "c"); ++p;
    CHECK_EQ(*p, ")"); ++p;
    CHECK(p == tokens.end());
  interactive = false;
}

void test_parenthesize_resets_stream_after_multiple_exprs_in_a_line() {
  IndentSensitiveStream in("() a\nc d");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  tokens = nextExpr(in);
  p = tokens.begin();
  CHECK_EQ(*p, "a"); ++p;
  CHECK(p == tokens.end());

  tokens = nextExpr(in);
  p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_multiline_parenthesized_exprs_when_interactive() {
  IndentSensitiveStream in("(a b\n)\n");
  list<Token> tokens = nextExpr(in);
  list<Token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  interactive = true;
  rewind(in);
  tokens = nextExpr(in);
    p = tokens.begin();
    CHECK_EQ(*p, "("); ++p;
    CHECK_EQ(*p, "a"); ++p;
    CHECK_EQ(*p, "b"); ++p;
    CHECK_EQ(*p, ")"); ++p;
    CHECK(p == tokens.end());
  CHECK(!in.eof());  // no eof at the interactive prompt
  interactive = false;
}

void rewind(IndentSensitiveStream& in) {
  in.fd.clear();
  in.fd.seekg(0);
  in.atStartOfLine = true;
}

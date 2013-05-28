void test_parenthesize_handles_initial_comment() {
  indent_sensitive_stream in("#a\na b");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_lines_with_initial_parens() {
  indent_sensitive_stream in("(a b c)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_lines_with_initial_parens2() {
  indent_sensitive_stream in("(a (b c))");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("  (a\tb c)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_skips_outdent_tokens() {
  indent_sensitive_stream in("(a b c\n  bc\n    def\n  gh)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a\n  b");
  next_expr(in);
  CHECK_EQ(in.fd.peek(), ' ');
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  indent_sensitive_stream in("(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "a"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_single_word_lines2() {
  indent_sensitive_stream in("a  \nb\nc");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "a"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "b"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "c"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line() {
  indent_sensitive_stream in("a b c  ");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line2() {
  indent_sensitive_stream in("a (b c)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a (b c) d");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a `(b c)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("(a () b)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  indent_sensitive_stream in(" a b c");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_quoted_words() {
  indent_sensitive_stream in(",a b c");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_quoted_words2() {
  indent_sensitive_stream in(",@a b c");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ",@"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_quoted_words3() {
  indent_sensitive_stream in("'a b c");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_nested_quoted_words() {
  indent_sensitive_stream in("a b\n  'c\n  ,d\n  @e");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "@"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups() {
  indent_sensitive_stream in(",(a b c)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups2() {
  indent_sensitive_stream in(",@(a b c)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, ",@"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups3() {
  indent_sensitive_stream in(",,(a b c)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("    a b c\n  34");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p=tokens.begin();
  CHECK_EQ(*p, "34"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  indent_sensitive_stream in("a b c  \nd ef");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p=tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "ef"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_across_indent() {
  indent_sensitive_stream in("a b c  \n  d ef");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a b c  \n  (d ef)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a b c  \n  (d ef)\n\n  g");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a b c\n  d e\n    f\ny");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "y"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_quotes_and_comments() {
  indent_sensitive_stream in("a b c  \n  '(d ef)\n\n  : g #abc");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a b c  \n    d ef\n  : g");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("a b c  \n    '(d ef)\n\n  g #abc");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("def foo\n    a b c\n  d e\nnewdef");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "newdef"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_before_too_much_outdent() {
  indent_sensitive_stream in("  a a\n    a\ny");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "y"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_groups_across_comments() {
  indent_sensitive_stream in("def foo\n#a b c\n  d e\nnew");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens() {
  indent_sensitive_stream in("(def foo\n    #a b c\n  d e)\nnew");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens2() {
  indent_sensitive_stream in("`(def foo\n    #a b c\n  d e)\nnew");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "`"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "def"); ++p;
  CHECK_EQ(*p, "foo"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, "e"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_does_not_group_inside_parens3() {
  indent_sensitive_stream in("  (a b c\n    d e)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  indent_sensitive_stream in("def foo(a (b)\n    c d)\n  d e\nnew");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
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
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "new"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_passes_through_unbalanced_open_paren() {
  indent_sensitive_stream in("(");
  list<token> tokens = next_expr(in);
  CHECK_EQ(tokens.size(), 1);
  CHECK_EQ(tokens.front(), "(");
}

void test_parenthesize_errors_on_unbalanced_closed_paren() {
  indent_sensitive_stream in(")");
  list<token> tokens = next_expr(in);
  CHECK(Raise_count > 0);   Raise_count=0;
}

void test_parenthesize_handles_early_paren() {
  indent_sensitive_stream in("a (b)");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_multiple_exprs_on_a_line() {
  indent_sensitive_stream in("() 1 2");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "1"); ++p;
  CHECK(p == tokens.end());
  tokens = next_expr(in); p = tokens.begin();
  CHECK_EQ(*p, "2"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_breaks_at_empty_lines_when_interactive() {
  indent_sensitive_stream in("34 35\n\n  36");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "34"); ++p;
  CHECK_EQ(*p, "35"); ++p;
  CHECK_EQ(*p, "36"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  Interactive = true;
  rewind(in);
  tokens = next_expr(in);
    p = tokens.begin();
    CHECK_EQ(*p, "("); ++p;
    CHECK_EQ(*p, "34"); ++p;
    CHECK_EQ(*p, "35"); ++p;
    CHECK_EQ(*p, ")"); ++p;
    CHECK(p == tokens.end());
  Interactive = false;
}

void test_parenthesize_breaks_indent_at_empty_lines_when_interactive() {
  indent_sensitive_stream in("a b\n  c\n\n  d");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  Interactive = true;
  rewind(in);
  tokens = next_expr(in);
    p = tokens.begin();
    CHECK_EQ(*p, "("); ++p;
    CHECK_EQ(*p, "a"); ++p;
    CHECK_EQ(*p, "b"); ++p;
    CHECK_EQ(*p, "c"); ++p;
    CHECK_EQ(*p, ")"); ++p;
    CHECK(p == tokens.end());
  Interactive = false;
}

void test_parenthesize_resets_stream_after_multiple_exprs_in_a_line() {
  indent_sensitive_stream in("() a\nc d");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  tokens = next_expr(in);
  p = tokens.begin();
  CHECK_EQ(*p, "a"); ++p;
  CHECK(p == tokens.end());

  tokens = next_expr(in);
  p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, "d"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());
}

void test_parenthesize_handles_multiline_parenthesized_exprs_when_interactive() {
  indent_sensitive_stream in("(a b\n)\n");
  list<token> tokens = next_expr(in);
  list<token>::iterator p = tokens.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == tokens.end());

  Interactive = true;
  rewind(in);
  tokens = next_expr(in);
    p = tokens.begin();
    CHECK_EQ(*p, "("); ++p;
    CHECK_EQ(*p, "a"); ++p;
    CHECK_EQ(*p, "b"); ++p;
    CHECK_EQ(*p, ")"); ++p;
    CHECK(p == tokens.end());
  CHECK(!in.eof());  // no eof at the interactive prompt
  Interactive = false;
}

void rewind(indent_sensitive_stream& in) {
  in.fd.clear();
  in.fd.seekg(0);
  in.at_start_of_line = true;
}

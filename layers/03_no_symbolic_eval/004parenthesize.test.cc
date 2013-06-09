void test_parenthesize_handles_initial_comment() {
  read_all("#a\na b");
  CHECK_TRACE_CONTENTS("parenthesize", "(ab)");
}

void test_parenthesize_handles_lines_with_initial_parens() {
  read_all("(a b c)");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)");
}

void test_parenthesize_handles_lines_with_initial_parens2() {
  read_all("(a (b c))");
  CHECK_TRACE_CONTENTS("parenthesize", "(a(bc))");
}

void test_parenthesize_skips_indent_tokens() {
  read_all("  (a\tb c)");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)");
}

void test_parenthesize_skips_outdent_tokens() {
  read_all("(a b c\n  bc\n    def\n  gh)");
  CHECK_TRACE_CONTENTS("parenthesize", "(abcbcdefgh)");
}

void test_parenthesize_preserves_following_indent() {
  indent_sensitive_stream in("a\n  b");
  next_expr(in);
  CHECK_EQ(in.fd.peek(), ' ');
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  read_all("(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc(defgh)(ijk)lm(nop))");
}

void test_parenthesize_passes_through_single_word_lines() {
  read_all("a");
  CHECK_TRACE_CONTENTS("parenthesize", "a");
}

void test_parenthesize_passes_through_single_word_lines2() {
  read_all("a  \nb\nc");
  CHECK_TRACE_CONTENTS("parenthesize", "abc");
}

void test_parenthesize_groups_words_on_single_line() {
  read_all("a b c  ");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)");
}

void test_parenthesize_groups_words_on_single_line2() {
  read_all("a (b c)");
  CHECK_TRACE_CONTENTS("parenthesize", "(a(bc))");
}

void test_parenthesize_groups_words_on_single_line3() {
  read_all("a (b c) d");
  CHECK_TRACE_CONTENTS("parenthesize", "(a(bc)d)");
}

void test_parenthesize_groups_words_on_single_line4() {
  read_all("a `(b c)");
  CHECK_TRACE_CONTENTS("parenthesize", "(a`(bc))");
}

void test_parenthesize_passes_through_words_on_single_line() {
  read_all("(a () b)");
  CHECK_TRACE_CONTENTS("parenthesize", "(a()b)");
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  read_all(" a b c");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)");
}

void test_parenthesize_groups_quoted_words() {
  read_all(",a b c");
  CHECK_TRACE_CONTENTS("parenthesize", "(,abc)");
}

void test_parenthesize_groups_quoted_words2() {
  read_all(",@a b c");
  CHECK_TRACE_CONTENTS("parenthesize", "(,@abc)");
}

void test_parenthesize_groups_quoted_words3() {
  read_all("'a b c");
  CHECK_TRACE_CONTENTS("parenthesize", "('abc)");
}

void test_parenthesize_passes_through_nested_quoted_words() {
  read_all("a b\n  'c\n  ,d\n  e");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc,de)");
}

void test_parenthesize_passes_through_quoted_groups() {
  read_all(",(a b c)");
  CHECK_TRACE_CONTENTS("parenthesize", ",(abc)");
}

void test_parenthesize_passes_through_quoted_groups2() {
  read_all(",@(a b c)");
  CHECK_TRACE_CONTENTS("parenthesize", ",@(abc)");
}

void test_parenthesize_passes_through_quoted_groups3() {
  read_all(",,(a b c)");
  CHECK_TRACE_CONTENTS("parenthesize", ",,(abc)");
}

void test_parenthesize_groups_words_on_single_indented_line() {
  read_all("    a b c\n  34");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)34");
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  read_all("a b c  \nd ef");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)(def)");
}

void test_parenthesize_groups_across_indent() {
  read_all("a b c  \n  d ef");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc(def))");
}

void test_parenthesize_groups_across_indent2() {
  read_all("a b c  \n  (d ef)");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc(def))");
}

void test_parenthesize_groups_across_indent3() {
  read_all("a b c  \n  (d ef)\n\n  g");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc(def)g)");
}

void test_parenthesize_groups_nested_indents() {
  read_all("a b c\n  d e\n    f\ny");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc(def))y");
}

void test_parenthesize_handles_quotes_and_comments() {
  read_all("a b c  \n  '(d ef)\n\n  g #abc");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc'(def)g)");
}

void test_parenthesize_takes_indent_from_colon() {
  read_all("a b c  \n    d ef\n  : g");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc(def)g)");
}

void test_parenthesize_groups_before_outdents() {
  read_all("a b c  \n    d ef\n\n  g #abc");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc(def)g)");
}

void test_parenthesize_groups_before_outdents2() {
  read_all("def foo\n    a b c\n  d e\nnewdef");
  CHECK_TRACE_CONTENTS("parenthesize", "(deffoo(abc)(de))newdef");
}

void test_parenthesize_groups_before_too_much_outdent() {
  read_all("  a a\n    a\ny");
  CHECK_TRACE_CONTENTS("parenthesize", "(aaa)y");
}

void test_parenthesize_groups_across_comments() {
  read_all("def foo\n#a b c\n  d e\nnew");
  CHECK_TRACE_CONTENTS("parenthesize", "(deffoo(de))new");
}

void test_parenthesize_does_not_group_inside_parens() {
  read_all("(def foo\n    #a b c\n  d e)\nnew");
  CHECK_TRACE_CONTENTS("parenthesize", "(deffoode)new");
}

void test_parenthesize_does_not_group_inside_parens2() {
  read_all("`(def foo\n    #a b c\n  d e)\nnew");
  CHECK_TRACE_CONTENTS("parenthesize", "`(deffoode)new");
}

void test_parenthesize_does_not_group_inside_parens3() {
  read_all("  (a b c\n    d e)");
  CHECK_TRACE_CONTENTS("parenthesize", "(abcde)");
}

void test_parenthesize_does_not_group_inside_arglists() {
  read_all("def foo(a (b)\n    c d)\n  d e\nnew");
  CHECK_TRACE_CONTENTS("parenthesize", "(deffoo(a(b)cd)(de))new");
}

void test_next_expr_passes_through_unbalanced_open_paren() {
  indent_sensitive_stream in("(");
  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "(");
}

void test_parenthesize_errors_on_unbalanced_closed_paren() {
  Do_raise = false;
  indent_sensitive_stream in(")");
  next_expr(in);
  CHECK(Raise_count > 0);   Raise_count=0;
}

void test_parenthesize_handles_early_paren() {
  read_all("a (b)");
  CHECK_TRACE_CONTENTS("parenthesize", "(a(b))");
}

void test_parenthesize_handles_multiple_exprs_on_a_line() {
  read_all("() 1 2");
  CHECK_TRACE_CONTENTS("parenthesize", "()12");
}

void test_parenthesize_breaks_at_empty_lines_when_interactive() {
  read_all("a b\n\n  c");
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)");

  CLEAR_TRACE;
  Interactive = true;
    read_all("a b\n\n  c");
    CHECK_TRACE_CONTENTS("parenthesize", "(ab)");
  Interactive = false;
}

void test_parenthesize_breaks_indent_at_empty_lines_when_interactive() {
  read_all("a b\n  c\n\n  d");
  CHECK_TRACE_CONTENTS("parenthesize", "(abcd)");

  CLEAR_TRACE;
  Interactive = true;
    read_all("a b\n  c\n\n  d");
    CHECK_TRACE_CONTENTS("parenthesize", "(abc)");
  Interactive = false;
}

void test_parenthesize_refuses_to_group_from_middle_of_line() {
  indent_sensitive_stream in("a b\n  c\n");
  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "(abc)");

  rewind(in);
  in.at_start_of_line = false;

  CLEAR_TRACE;
  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "a");
  CLEAR_TRACE;
  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "b");
  CLEAR_TRACE;
  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "c");
}

void test_parenthesize_resets_stream_after_multiple_exprs_in_a_line() {
  indent_sensitive_stream in("() a\nc d");
  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "()");

  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "a");

  next_expr(in);
  CHECK_TRACE_CONTENTS("parenthesize", "(cd)");
}

void test_parenthesize_handles_multiline_parenthesized_exprs_when_interactive() {
  read_all("(a b\n)\n");
  CHECK_TRACE_CONTENTS("parenthesize", "(ab)");

  CLEAR_TRACE;
  Interactive = true;
    read_all("(a b\n)\n");
    CHECK_TRACE_CONTENTS("parenthesize", "(ab)");
  Interactive = false;
}

void rewind(indent_sensitive_stream& in) {
  in.fd.clear();
  in.fd.seekg(0);
  in.at_start_of_line = true;
}

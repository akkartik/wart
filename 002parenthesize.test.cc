void test_parenthesize_handles_lines_with_initial_parens() {
  CodeStream cs(stream("(a b c)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_skips_indent_tokens() {
  CodeStream cs(stream("  (a\tb c)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_skips_outdent_tokens() {
  CodeStream cs(stream("(a b c\n  bc\n    def\n  gh)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "bc");    ++p;
  checkEq(*p, "def");   ++p;
  checkEq(*p, "gh");    ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_preserves_following_indent() {
  CodeStream cs(stream("a\n  b"));
  nextExpr(cs);
  checkEq(cs.fd.peek(), ' ');
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  CodeStream cs(stream("(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "def");   ++p;
  checkEq(*p, "gh");    ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "i");     ++p;
  checkEq(*p, "j");     ++p;
  checkEq(*p, "k");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "lm");    ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "no");    ++p;
  checkEq(*p, "p");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_passes_through_single_word_lines() {
  CodeStream cs(stream("a"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "a");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_passes_through_single_word_lines2() {
  CodeStream cs(stream("a  \nb\nc"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "a");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "b");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "c");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_words_on_single_line() {
  CodeStream cs(stream("a b c  "));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  CodeStream cs(stream(" a b c"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_quoted_words() {
  CodeStream cs(stream(",a b c"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, ",");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_quoted_words2() {
  CodeStream cs(stream(",@a b c"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, ",@");    ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_quoted_words3() {
  CodeStream cs(stream("'a b c"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "'");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_passes_through_nested_quoted_words() {
  CodeStream cs(stream("a b\n  'c\n  ,d\n  @e"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "'");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ",");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "@");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_passes_through_quoted_groups() {
  CodeStream cs(stream(",(a b c)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ",");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_passes_through_quoted_groups2() {
  CodeStream cs(stream(",@(a b c)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ",@");    ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_passes_through_quoted_groups3() {
  CodeStream cs(stream(",,(a b c)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, ",");     ++p;
  checkEq(*p, ",");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_words_on_single_indented_line() {
  CodeStream cs(stream("    a b c\n  34"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p=tokens.begin();
  checkEq(*p, "34");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  CodeStream cs(stream("a b c  \nd ef"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p=tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "ef");    ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_across_indent() {
  CodeStream cs(stream("a b c  \n  d ef"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "ef");    ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_across_indent2() {
  CodeStream cs(stream("a b c  \n  (d ef)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "ef");    ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_across_indent3() {
  CodeStream cs(stream("a b c  \n  (d ef)\n\n  g"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "ef");    ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "g");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_nested_indents() {
  CodeStream cs(stream("a b c\n  d e\n    f\ny"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, "f");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "y");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_handles_quotes_and_comments() {
  CodeStream cs(stream("a b c  \n  '(d ef)\n\n  : g ;abc"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "'");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "ef");    ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "g");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_takes_indent_from_colon() {
  CodeStream cs(stream("a b c  \n  d ef\n  : g"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "ef");    ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "g");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_before_outdents() {
  CodeStream cs(stream("a b c  \n    '(d ef)\n\n  g ;abc"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "'");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "ef");    ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "g");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_before_outdents2() {
  CodeStream cs(stream("def foo\n    a b c\n  d e\nnewdef"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "def");   ++p;
  checkEq(*p, "foo");   ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs); p = tokens.begin();
  checkEq(*p, "newdef");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_before_too_much_outdent() {
  CodeStream cs(stream("  a a\n    a\ny"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "y");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_groups_across_comments() {
  CodeStream cs(stream("def foo\n;a b c\n  d e\nnew"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "def");   ++p;
  checkEq(*p, "foo");   ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "new");   ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_does_not_group_inside_parens() {
  CodeStream cs(stream("(def foo\n    ;a b c\n  d e)\nnew"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "def");   ++p;
  checkEq(*p, "foo");   ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "new");   ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_does_not_group_inside_parens2() {
  CodeStream cs(stream("`(def foo\n    ;a b c\n  d e)\nnew"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "`");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "def");   ++p;
  checkEq(*p, "foo");   ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "new");   ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_does_not_group_inside_parens3() {
  CodeStream cs(stream("  (a b c\n    d e)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_does_not_group_inside_arglists() {
  CodeStream cs(stream("def foo(a (b)\n    c d)\n  d e\nnew"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, "(");     ++p;
  checkEq(*p, "def");   ++p;
  checkEq(*p, "foo");   ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "a");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "b");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "c");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, "(");     ++p;
  checkEq(*p, "d");     ++p;
  checkEq(*p, "e");     ++p;
  checkEq(*p, ")");     ++p;
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  tokens = nextExpr(cs);      p = tokens.begin();
  checkEq(*p, "new");   ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_passes_through_unbalanced_open_paren() {
  CodeStream cs(stream("("));
  list<Token> tokens = nextExpr(cs);
  checkEq(tokens.size(), 1);
  checkEq(tokens.front(), "(");
  check(cs.eof());
}

void test_parenthesize_errors_on_unbalanced_closed_paren() {
  CodeStream cs(stream(")"));
  list<Token> tokens = nextExpr(cs);
  checkEq(raiseCount, 1);       raiseCount=0;
  check(cs.eof());
}

void test_parenthesize_knows_spaces_before() {
  CodeStream cs(stream("(a b  c)"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(p->spacesBefore, -1);
  checkEq(*p, "(");     ++p;
  checkEq(p->spacesBefore, 0);
  checkEq(*p, "a");   ++p;
  checkEq(p->spacesBefore, 1);
  checkEq(*p, "b");   ++p;
  checkEq(p->spacesBefore, 2);
  checkEq(*p, "c");     ++p;
  checkEq(p->spacesBefore, 0);
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_parenthesize_knows_spaces_before2() {
  CodeStream cs(stream("a b  c"));
  list<Token> tokens = nextExpr(cs);
  list<Token>::iterator p = tokens.begin();
  checkEq(p->spacesBefore, -1);
  checkEq(*p, "(");     ++p;
  checkEq(p->spacesBefore, -1);
  checkEq(*p, "a");   ++p;
  checkEq(p->spacesBefore, 1);
  checkEq(*p, "b");   ++p;
  checkEq(p->spacesBefore, 2);
  checkEq(*p, "c");     ++p;
  checkEq(p->spacesBefore, -1);
  checkEq(*p, ")");     ++p;
  check(p == tokens.end());
  check(cs.eof());
}

void test_slurpNextLine_adds_all_words_in_next_line() {
  list<Token> tokens = tokenize(stream(L"abc def\nghi jkl"));
  list<Token> line;
  slurpNextLine(line, tokens.begin(), tokens.end());
  list<Token>::iterator p = line.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  check(p == line.end());
}

void test_slurpNextLine_includes_indent_for_current_and_next_line() {
  list<Token> tokens = tokenize(stream(L"  abc def\nghi jkl"));
  list<Token> line;
  slurpNextLine(line, tokens.begin(), tokens.end());
  list<Token>::iterator p = line.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, INDENT); ++p;
  checkEq(*p, L"abc"); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, OUTDENT); ++p;
  check(p == line.end());
}

void test_slurpNextLine_deletes_previous_line_on_recall() {
  list<Token> tokens = tokenize(stream(L"  abc def\nghi jkl\n  mnop"));
  list<Token> line;
  list<Token>::iterator q = slurpNextLine(line, tokens.begin(), tokens.end());
  slurpNextLine(line, q, tokens.end());
  list<Token>::iterator p = line.begin();
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, OUTDENT); ++p;
  checkEq(*p, L"ghi"); ++p;
  checkEq(*p, L"jkl"); ++p;
  checkEq(*p, START_OF_LINE); ++p;
  checkEq(*p, INDENT); ++p;
  check(p == line.end());
}



void test_parenthesize_handles_lines_with_initial_parens() {
  list<Token> tokens = parenthesize(tokenize(stream(L"(a b c)")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_skips_indent_tokens() {
  list<Token> tokens = parenthesize(tokenize(stream(L"  (a\tb c)")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_skips_outdent_tokens() {
  list<Token> tokens = parenthesize(tokenize(stream(L"(a b c\n  bc\n    def\n  gh)")));
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
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  list<Token> tokens = parenthesize(tokenize(stream(L"(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))")));
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
}

void test_parenthesize_passes_through_single_word_lines() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"a"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_single_word_lines2() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a  \nb\nc")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c  ")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_accidentally_indented_line() {
  debug = 1;
  list<Token> tokens = parenthesize(tokenize(stream(L" a b c")));
  debug = 0;
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_quoted_words() {
  list<Token> tokens = parenthesize(tokenize(stream(L",a b c")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L","); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_quoted_words2() {
  list<Token> tokens = parenthesize(tokenize(stream(L",@a b c")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L",@"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_quoted_words3() {
  list<Token> tokens = parenthesize(tokenize(stream(L"'a b c")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"'"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_nested_quoted_words() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b\n  'c\n  ,d\n  @e")));
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
}

void test_parenthesize_passes_through_quoted_groups() {
  list<Token> tokens = parenthesize(tokenize(stream(L",(a b c)")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L","); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_quoted_groups2() {
  list<Token> tokens = parenthesize(tokenize(stream(L",@(a b c)")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L",@"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_indented_line() {
  list<Token> tokens = parenthesize(tokenize(stream(L"    a b c\n  34")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"34"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c  \nd ef")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"ef"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_across_indent() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c  \n  d ef")));
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
}

void test_parenthesize_groups_across_indent2() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c  \n  (d ef)")));
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
}

void test_parenthesize_groups_across_indent3() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c  \n  (d ef)\n\n  g")));
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
}

void test_parenthesize_groups_nested_indents() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c\n  d e\n    f\ny")));
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
  checkEq(*p, L"y"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_handles_quotes_and_comments() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c  \n  '(d ef)\n\n  g ;abc")));
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
}

void test_parenthesize_groups_before_outdents() {
  list<Token> tokens = parenthesize(tokenize(stream(L"a b c  \n    '(d ef)\n\n  g ;abc")));
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
}

void test_parenthesize_groups_before_outdents2() {
  list<Token> tokens = parenthesize(tokenize(stream(L"def foo\n    a b c\n  d e\nnewdef")));
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
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_before_too_much_outdent() {
  list<Token> tokens = parenthesize(tokenize(stream(L"  a a\n    a\ny")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"y"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_across_comments() {
  list<Token> tokens = parenthesize(tokenize(stream(L"def foo\n;a b c\n  d e\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_inside_parens() {
  list<Token> tokens = parenthesize(tokenize(stream(L"(def foo\n    ;a b c\n  d e)\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_inside_parens2() {
  list<Token> tokens = parenthesize(tokenize(stream(L"`(def foo\n    ;a b c\n  d e)\nnewdef")));
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
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_inside_indented_parens() {
  list<Token> tokens = parenthesize(tokenize(stream(L"  (a b c\n    d e)")));
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
}

void test_parenthesize_passes_through_arglists() {
  list<Token> tokens = parenthesize(tokenize(stream(L"def foo(a b\n    c d)\n  d e\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"def"); ++p;
  checkEq(*p, L"foo"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L")"); ++p;
  checkEq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_when_indented_by_one_space() {
  list<Token> tokens = parenthesize(tokenize(stream(L"    (a b c\n     d e f)")));
  list<Token>::iterator p = tokens.begin();
  checkEq(*p, L"("); ++p;
  checkEq(*p, L"a"); ++p;
  checkEq(*p, L"b"); ++p;
  checkEq(*p, L"c"); ++p;
  checkEq(*p, L"d"); ++p;
  checkEq(*p, L"e"); ++p;
  checkEq(*p, L"f"); ++p;
  checkEq(*p, L")"); ++p;
  check(p == tokens.end());
}



void checkTokensMatchSub(const ascii* msg, list<Token> x0, list<Token> x1) {
  bool pass = true;
  for (list<Token>::iterator p0 = x0.begin(), p1 = x1.begin(); p0 != x0.end() && p1 != x1.end(); ++p0, ++p1)
    if (*p0 != *p1) pass = false;
  if (pass) return;
  cerr << endl << x0.size() << " " << x1.size() << endl;
  for (list<Token>::iterator p0 = x0.begin(), p1 = x1.begin(); p0 != x0.end() && p1 != x1.end(); ++p0, ++p1)
    if (*p0 == *p1)
      cerr << *p0 << endl;
    else
      cerr << "F " << msg << " expected " << *p0 << " got " << *p1 << endl;
}

#define checkTokensMatch(x, y) checkTokensMatchSub(__FUNCTION__, x, y)

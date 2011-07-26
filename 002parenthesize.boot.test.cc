void test_slurpNextLine_adds_all_words_in_next_line() {
  list<Token> tokens = tokenize(teststream(L"abc def\nghi jkl"));
  list<Token> line;
  slurpNextLine(line, tokens.begin(), tokens.end());
  list<Token>::iterator p = line.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check(p == line.end());
}

void test_slurpNextLine_includes_indent_for_current_and_next_line() {
  list<Token> tokens = tokenize(teststream(L"  abc def\nghi jkl"));
  list<Token> line;
  slurpNextLine(line, tokens.begin(), tokens.end());
  list<Token>::iterator p = line.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, OUTDENT); ++p;
  check(p == line.end());
}

void test_slurpNextLine_deletes_previous_line_on_recall() {
  list<Token> tokens = tokenize(teststream(L"  abc def\nghi jkl\n  mnop"));
  list<Token> line;
  list<Token>::iterator q = slurpNextLine(line, tokens.begin(), tokens.end());
  slurpNextLine(line, q, tokens.end());
  list<Token>::iterator p = line.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, OUTDENT); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, L"jkl"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check(p == line.end());
}



void test_parenthesize_handles_lines_with_initial_parens() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"(a b c)")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_skips_indent_tokens() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"  (a\tb c)")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_skips_outdent_tokens() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"(a b c\n  bc\n    def\n  gh)")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"bc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"gh"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"gh"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"i"); ++p;
  check_eq(*p, L"j"); ++p;
  check_eq(*p, L"k"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"lm"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"no"); ++p;
  check_eq(*p, L"p"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_single_word_lines() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"a"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_passes_through_single_word_lines2() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a  \nb\nc")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_line() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a b c  ")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_single_indented_line() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"    a b c\n  34")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"34"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a b c  \nd ef")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_across_indent() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a b c  \n  d ef")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_across_indent2() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a b c  \n  (d ef)")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_groups_across_indent3() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a b c  \n  (d ef)\n\n  g")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"g"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_handles_quotes_and_comments() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a b c  \n  '(d ef)\n\n  g ;abc")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"g"); ++p;
  check_eq(*p, L";abc"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_around_outdents() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"a b c  \n    '(d ef)\n\n  g ;abc")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"g"); ++p;
  check_eq(*p, L";abc"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_around_outdents2() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"def foo\n    a b c\n  d e\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"foo"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_around_too_much_outdent() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"  a a\n    a\ny")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"y"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_across_comments() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"def foo\n    ;a b c\n  d e\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"foo"); ++p;
  check_eq(*p, L";a b c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_inside_parens() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"(def foo\n    ;a b c\n  d e)\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"foo"); ++p;
  check_eq(*p, L";a b c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_inside_parens2() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"`(def foo\n    ;a b c\n  d e)\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"`"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"foo"); ++p;
  check_eq(*p, L";a b c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_arglists() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"def foo(a b\n    c d)\n  d e\nnewdef")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"foo"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"newdef"); ++p;
  check(p == tokens.end());
}

void test_parenthesize_wraps_when_indented_by_one_space() {
  list<Token> tokens = parenthesize(tokenize(teststream(L"    (a b c\n     d e f)")));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L"f"); ++p;
  check_eq(*p, L")"); ++p;
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

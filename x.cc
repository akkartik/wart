                                  #include<string>
                                  typedef std::wstring string;
                                  #include<list>
                                  using std::list;
                                  #include<iostream>
                                  typedef std::wistream istream;
                                  typedef std::wostream ostream;
                                  #define cin std::wcin
                                  #define cout std::wcout
                                  #define cerr std::wcerr
                                  using std::endl;
                                  #include<sstream>
                                  typedef std::wstringstream stringstream;
                                  typedef std::wostringstream ostringstream;

                                  typedef char ascii; // must come after system includes
                                  #define char wchar_t

                                  int numFailures = 0;

                                  #define check(X) if (!(X)) { \
                                      ++numFailures; \
                                      cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
                                    } \
                                    else { cerr << "."; fflush(stderr); }

                                  #define check_eq(X, Y) if ((X) != (Y)) { \
                                      ++numFailures; \
                                      cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
                                      cerr << "  got " << (X) << endl; /* BEWARE: multiple eval */ \
                                    } \
                                    else { cerr << "."; fflush(stderr); }



//// tokenize. newlines and indent matter.

enum TokenType {
  NON_WHITESPACE,
  START_OF_LINE,
  INDENT, MAYBE_WRAP,
  OUTDENT,
};

bool whitespace(TokenType t) {
  return t != NON_WHITESPACE;
}

struct Token {
  const TokenType type;
  const string token;
  const int indentLevel;

  // private
  Token(const TokenType t, const string x, const int l) :type(t), token(x), indentLevel(l) {}

  // static methods for single-line stack allocation
  static Token of(string s) {
    Token result(NON_WHITESPACE, s, 0);
    return result;
  }
  static Token sol() {
    Token result(START_OF_LINE, L"", 0);
    return result;
  }
  static Token indent(int l, bool justOneExtraChar) {
    Token result(justOneExtraChar ? MAYBE_WRAP : INDENT, L"", l);
    return result;
  }
  static Token outdent(int l) {
    Token result(OUTDENT, L"", l);
    return result;
  }

  bool operator==(string x) {
    return type == NON_WHITESPACE && token == x;
  }
  bool operator==(TokenType x) {
    return type == x;
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
  bool operator!=(TokenType x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, Token p) {
  if (p.type != NON_WHITESPACE) os << p.type;
  else os << p.token;
  return os;
}



                                  void skip(istream& in) {
                                    char dummy;
                                    in >> dummy;
                                  }

                                  void skipWhitespace(istream& in) {
                                    while (isspace(in.peek()) && in.peek() != L'\n')
                                      skip(in);
                                  }

                                  bool eof(istream& in) {
                                    in.peek();
                                    return in.eof();
                                  }

                                  class Die {};
                                  ostream& operator<<(ostream& os, Die die) {
                                    exit(1);
                                  }
                                  Die DIE;

// counts number of whitespace chars between next non-whitespace char and previous newline
// BEWARE: tab = 1 space; don't mix the two
//
// But do track whether the final indent char is a space. We'll need that farther down.
const int LAST_CHAR_IS_SPACE = 100;
int countIndent(istream& in) {
  int count = 0;
  bool lastCharIsSpace = false;
  char c;
  while (!eof(in)) {
    if (!isspace(in.peek()))
      break;
    in >> c;
    lastCharIsSpace = (c == L' ');
    ++count;
    if (c == L'\n')
      count = 0;
  }
  if (count >= LAST_CHAR_IS_SPACE)
    cerr << L"eek, too much indent\n" << DIE;
  if (lastCharIsSpace)
    count += LAST_CHAR_IS_SPACE;
  return count;
}

                                  stringstream& teststream(string s) {
                                    stringstream& result = *new stringstream(s);
                                    result << std::noskipws;
                                    return result;
                                  }

void test_countIndent() {
  // countIndent requires a non-empty stream
  check_eq(countIndent(teststream(L"\t")), 1);
  check_eq(countIndent(teststream(L" ")), 1+LAST_CHAR_IS_SPACE);
  check_eq(countIndent(teststream(L"   ")), 3+LAST_CHAR_IS_SPACE);
  check_eq(countIndent(teststream(L" \t ")), 3+LAST_CHAR_IS_SPACE); // tab == 1 space
  check_eq(countIndent(teststream(L" \n ")), 1+LAST_CHAR_IS_SPACE); // skip empty lines
  check_eq(countIndent(teststream(L" \r\n  ")), 2+LAST_CHAR_IS_SPACE); // dos
  check_eq(countIndent(teststream(L"\n\na")), 0);
}



                                  // slurp functions read a token when you're sure to be at it
                                  void slurpChar(istream& in, ostream& out) {
                                    char c;
                                    in >> c; out << c;
                                  }

                                  void slurpWord(istream& in, ostream& out) {
                                    char c;
                                    while (!eof(in)) {
                                      in >> c;
                                      if (isspace(c) || c == L',' || c == ';'
                                          // keep this list sync'd with the parseToken switch below
                                          || c == L'(' || c == L')' || c == L'\'' || c == L'"') {
                                        in.putback(c);
                                        break;
                                      }
                                      out << c;
                                    }
                                  }

                                  void slurpString(istream& in, ostream& out) {
                                    slurpChar(in, out); // initial quote
                                    char c;
                                    while (!eof(in)) {
                                      in >> c; out << c;
                                      if (c == L'\\') {
                                        slurpChar(in, out); // blindly read next
                                      }
                                      else if (c == L'"') {
                                        break;
                                      }
                                    }
                                  }

                                  void slurpComment(istream& in, ostream& out) {
                                    char c;
                                    while (!eof(in)) {
                                      in >> c;
                                      if (c == L'\n') {
                                        in.putback(c);
                                        break;
                                      }
                                      out << c;
                                    }
                                  }

int indentLevel = 0;
TokenType prevTokenType = START_OF_LINE;
Token parseToken(istream& in) {
  if (prevTokenType != START_OF_LINE) {
    skipWhitespace(in);
    if (in.peek() == L'\n') {
      skip(in);
      return Token::sol();
    }
  }

  if (prevTokenType == START_OF_LINE) {
    int prevIndentLevel = indentLevel;
    indentLevel = countIndent(in);
    bool lastCharIsSpace = (indentLevel >= LAST_CHAR_IS_SPACE);
    if (lastCharIsSpace) indentLevel -= LAST_CHAR_IS_SPACE;
    if (indentLevel > prevIndentLevel+1)
      return Token::indent(indentLevel, false);
    else if (indentLevel == prevIndentLevel+1)
      return Token::indent(indentLevel, lastCharIsSpace);
    else if (indentLevel < prevIndentLevel)
      return Token::outdent(indentLevel);
    // otherwise prevIndentLevel == indentLevel; fall through
  }

  ostringstream out;
  switch (in.peek()) { // now can't be whitespace
    case L'"':
      slurpString(in, out); break;

    case L'(':
    case L')':
    case L'\'':
    case L'`':
      slurpChar(in, out); break;

    case L',':
      slurpChar(in, out);
      if (in.peek() == L'@')
        slurpChar(in, out);
      break;

    case L';':
      slurpComment(in, out); break;

    default:
      slurpWord(in, out); break;
  }
  return Token::of(out.rdbuf()->str());
}

list<Token> tokenize(istream& in) {
  indentLevel = 0;
  prevTokenType = START_OF_LINE;
  in >> std::noskipws;
  list<Token> result;
  while (!eof(in)) {
    result.push_back(parseToken(in));
    prevTokenType = result.back().type;
  }

  while(!result.empty()
        && (whitespace(result.back().type) || result.back().token == L""))
    result.pop_back();
  return result;
}

void test_tokenize_empty_input() {
  list<Token> ast = tokenize(teststream(L""));
  check(ast.empty());
}

void test_tokenize_atom() {
  list<Token> ast = tokenize(teststream(L"34"));
  check_eq(ast.front(), L"34");
}

void test_tokenize_multiple_atoms() {
  list<Token> ast = tokenize(teststream(L"34 abc"));
  check_eq(ast.size(), 2);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"abc");
}

void test_tokenize_string_literal() {
  list<Token> ast = tokenize(teststream(L"34 \"abc\""));
  check_eq(ast.size(), 2);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc\"");
}

void test_tokenize_multiple_lines() {
  list<Token> ast = tokenize(teststream(L"34\n\"abc\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc\"");
}

void test_tokenize_string_with_space() {
  list<Token> ast = tokenize(teststream(L"34\n\"abc def\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc def\"");
}

void test_tokenize_string_with_escape() {
  list<Token> ast = tokenize(teststream(L"34\n\"abc \\\"quote def\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc \\\"quote def\"");
}

void test_tokenize_quotes_commas_parens() {
  list<Token> ast = tokenize(teststream(L"(',)"));
  check_eq(ast.size(), 4);
  check_eq(ast.front(), L"(");
  check_eq(ast.back(), L")");
}

void test_tokenize_splice_operator() {
  list<Token> ast = tokenize(teststream(L"()',@"));
  check_eq(ast.size(), 4);
  check_eq(ast.front(), L"(");
  check_eq(ast.back(), L",@");
}

void test_tokenize_comments() {
  list<Token> ast = tokenize(teststream(L"()',@ ;abc def ghi"));
  check_eq(ast.size(), 5);
  check_eq(ast.front(), L"(");
  check_eq(ast.back(), L";abc def ghi");
}

void test_tokenize_comments_end_at_newline() {
  list<Token> ast = tokenize(teststream(L";abc def ghi\nabc"));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L";abc def ghi");
}

void test_tokenize_sexpr() {
  list<Token> ast = tokenize(teststream(L"('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\nabc"));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"boo"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"\"foo\nbar\""); ++p;
  check_eq(*p, L"`"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"`"); ++p;
  check_eq(*p, L","); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L",@"); ++p;
  check_eq(*p, L"e"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L";def ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == ast.end());
}

void test_tokenize_suppress_trailing_whitespace() {
  list<Token> ast = tokenize(teststream(L"34 \nabc"));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"abc");
}

void test_tokenize_suppress_terminal_whitespace() {
  list<Token> ast = tokenize(teststream(L"34 abc\n  "));
  check_eq(ast.size(), 2);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"abc");
}

void test_tokenize_repeated_newline() {
  list<Token> ast = tokenize(teststream(L"34\n\n\"abc \\\"quote def\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc \\\"quote def\"");
}

void test_tokenize_indent_outdent() {
  list<Token> ast = tokenize(teststream(L"abc def ghi\n\n    abc\n  def"));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, OUTDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == ast.end());
}

void test_tokenize_whitespace_lines() {
  list<Token> ast = tokenize(teststream(L"abc def ghi\n\n    \n  def"));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == ast.end());
}

void test_tokenize_whitespace_at_eol() {
  list<Token> ast = tokenize(teststream(L"a \nb\r\nc"));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"a"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"c"); ++p;
  check(p == ast.end());
}

void test_tokenize_initial_whitespace_lines() {
  list<Token> ast = tokenize(teststream(L"  \nabc def ghi\n\n    \n  def"));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == ast.end());
}



//// insert explicit parens based on indentation.

#define inc(p) { ++p; if (p == end) return result; }
list<Token> slurpLine(list<Token>::iterator p, list<Token>::iterator end) {
  list<Token> result;
  if (*p == START_OF_LINE) inc(p);
  if (*p == INDENT) inc(p);

  while (!whitespace(p->type)) {
    result.push_back(*p);
    inc(p);
  }

  if (*p == START_OF_LINE) inc(p);
  if (*p == INDENT || *p == MAYBE_WRAP || *p == OUTDENT) {
    result.push_back(*p);
    inc(p);
  }

  return result;
}
#undef inc

void test_slurpLine() {
  list<Token> ast = tokenize(teststream(L"abc"));
  list<Token> line = slurpLine(ast.begin(), ast.end());
  list<Token>::iterator p = line.begin();
  check_eq(*p, L"abc"); ++p;
  check(p == line.end());
}

void test_slurpLine_empty() {
  list<Token> ast = tokenize(teststream(L""));
  list<Token> line = slurpLine(ast.begin(), ast.end());
  check(line.empty());
}

void test_slurpLine_skips_indent() {
  list<Token> ast = tokenize(teststream(L"abc"));
  list<Token> line = slurpLine(ast.begin(), ast.end());
  list<Token>::iterator p = line.begin();
  check_eq(*p, L"abc"); ++p;
  check(p == line.end());
}

void test_slurpLine_includes_indent_from_next_line() {
  list<Token> ast = tokenize(teststream(L"  \nabc def ghi\n\n    \n  def"));
  list<Token> line = slurpLine(ast.begin(), ast.end());
  list<Token>::iterator p = line.begin();
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, INDENT); ++p;
  check(p == line.end());
}



                                  list<Token>::iterator skipLine(list<Token>::iterator p, list<Token>::iterator end) {
                                    while(whitespace(p->type) && p != end) ++p;
                                    while(!whitespace(p->type) && p != end) ++p;
                                    while(whitespace(p->type) && p != end) ++p;
                                    return p;
                                  }

                                  int numWordsInLine(list<Token> line) {
                                    int numWords = line.size();
                                    for (list<Token>::reverse_iterator p = line.rbegin(); p != line.rend(); ++p) {
                                      if (whitespace(p->type)) --numWords; // indent
                                      else if (*p == L")") --numWords;
                                      else if (p->token[0] == L';') --numWords; // comment
                                      else break;
                                    }
                                    return numWords;
                                  }

                                  bool isIndent(Token t) {
                                    return whitespace(t.type); // No more START_OF_LINE at this point
                                  }

                                  int indentLevelOfLine(list<Token>::iterator p, list<Token>::iterator end) {
                                    if (p == end) return 0;
                                    list<Token>::iterator prev = p;
                                    if (!whitespace(prev->type)) return 0;

                                    for (++p; p != end; prev=p, ++p) {
                                      if (*p == START_OF_LINE) continue;
                                      if (!isIndent(*p)) break;
                                    }

                                    if (*prev == START_OF_LINE) return 0;
                                    if (!isIndent(*prev))
                                      cerr << "error in indentLevel" << DIE;
                                    return prev->indentLevel;
                                  }

list<Token> parenthesize(list<Token> in) {
  list<Token> result;
  list<int> parenStack;
  for (list<Token>::iterator p = in.begin(); p != in.end(); p = skipLine(p, in.end())) {
    list<Token> line = slurpLine(p, in.end());
    int numWords = numWordsInLine(line);
    int indentLevel = indentLevelOfLine(p, in.end());
    if (numWords == 0)
      cerr << "tokenize shouldn't have passed through empty lines\n" << DIE;

    if (numWords > 1
        && line.front() != L"(" && line.front() != L"'" && line.front() != L"`") {
      result.push_back(Token::of(L"("));
      parenStack.push_back(indentLevel);
    }

    for (list<Token>::iterator q = line.begin(); q != line.end(); ++q) {
      if (!whitespace(q->type))
        result.push_back(*q);
    }

    if (!isIndent(line.back()) && numWords > 1
        && line.front() != L"(" && line.front() != L"'" && line.front() != L"`") {
      result.push_back(Token::of(L")"));
      parenStack.pop_back();
    }

    if (line.back() == OUTDENT
        && !parenStack.empty() && parenStack.back() == line.back().indentLevel) {
      result.push_back(Token::of(L")"));
      parenStack.pop_back();
    }
  }

  for (unsigned int i=0; i < parenStack.size(); ++i)
    result.push_back(Token::of(L")"));
  return result;
}

void test_parenthesize_lines_with_initial_parens() {
  list<Token> ast = parenthesize(tokenize(teststream(L"(a b c)")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == ast.end());
}

void test_parenthesize_skips_indent_tokens() {
  list<Token> ast = parenthesize(tokenize(teststream(L"  (a\tb c)")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == ast.end());
}

void test_parenthesize_skips_outdent_tokens() {
  list<Token> ast = parenthesize(tokenize(teststream(L"(a b c\n  bc\n    def\n  gh)")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"bc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"gh"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == ast.end());
}

void test_parenthesize_handles_fully_parenthesized_expressions_regardless_of_indent() {
  list<Token> ast = parenthesize(tokenize(teststream(L"(a b c\n  (def gh)\n    (i j k)\n  lm\n\n\n    (no p))")));
  list<Token>::iterator p = ast.begin();
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
  check(p == ast.end());
}

void test_parenthesize_passes_through_single_word_lines() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"a"); ++p;
  check(p == ast.end());
}

void test_parenthesize_passes_through_single_word_lines2() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a  \nb\nc")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check(p == ast.end());
}

void test_parenthesize_groups_words_on_single_line() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a b c  ")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == ast.end());
}

void test_parenthesize_groups_words_on_each_line_without_indent() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a b c  \nd ef")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == ast.end());
}

void test_parenthesize_groups_across_indent() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a b c  \n  d ef")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == ast.end());
}

void test_parenthesize_groups_across_indent2() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a b c  \n  (d ef)")));
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"d"); ++p;
  check_eq(*p, L"ef"); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L")"); ++p;
  check(p == ast.end());
}

void test_parenthesize_groups_across_indent3() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a b c  \n  (d ef)\n\n  g")));
  list<Token>::iterator p = ast.begin();
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
  check(p == ast.end());
}

void test_parenthesize_handles_quotes_and_comments() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a b c  \n  '(d ef)\n\n  g ;abc")));
  list<Token>::iterator p = ast.begin();
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
  check(p == ast.end());
}

void test_parenthesize_wraps_around_outdents() {
  list<Token> ast = parenthesize(tokenize(teststream(L"a b c  \n    '(d ef)\n\n  g ;abc")));
  list<Token>::iterator p = ast.begin();
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
  check(p == ast.end());
}



ostream& operator<<(ostream& os, list<Token> l) {
  for (list<Token>::iterator p = l.begin(); p != l.end(); ++p) {
    if (*p != L")") os << " ";
    os << *p;
  }
  os << endl;
  return os;
}



typedef void (*testfunc)(void);

const testfunc tests[] = {
  #include"test_list"
};

void runTests() {
  for (unsigned int i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    (*tests[i])();
  }
  cerr << endl;
  if (numFailures == 0) return;

  cerr << numFailures << " failure";
      if (numFailures > 1) { cerr << "s"; }
      cerr << endl;
}

int main(int argc, ascii* argv[]) {
  if (argc > 1) {
    std::string arg1(argv[1]);
    if (arg1 == "test") {
      runTests();
      return 0;
    }
  }

  cout << parenthesize(tokenize(cin));
  return 0;
}

// style:
//  wide unicode strings everywhere
//  no function prototypes
//  indented functions are deemphasized, would be pushed farther down if C permitted
//  no new except in tests
//  minimal STL
//  immutable objects; copy everywhere; no references or pointers

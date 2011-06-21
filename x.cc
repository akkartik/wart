                                  #include<cstdio>
                                  #include<string>
                                  typedef std::wstring string;
                                  #include<list>
                                  using std::list;
                                  #include<stack>
                                  using std::stack;
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
                                  #include <ext/hash_map>
                                  using __gnu_cxx::hash_map;
                                  using __gnu_cxx::hash;

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

                                  #define __unused__ __attribute__((unused))



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
  TokenType type;
  string token;
  int indentLevel;

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
                                  ostream& operator<<(ostream& os __unused__, Die die __unused__) {
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
  in >> std::noskipws;
  list<Token> result;
  result.push_back(Token::sol());
  prevTokenType = START_OF_LINE;
  while (!eof(in)) {
    result.push_back(parseToken(in));
    prevTokenType = result.back().type;
  }

  while(!result.empty()
        && (whitespace(result.back().type) || result.back().token == L""))
    result.pop_back();
  return result;
}

void test_tokenize_handles_empty_input() {
  list<Token> tokens = tokenize(teststream(L""));
  check(tokens.empty());
}

void test_tokenize_handles_atom() {
  list<Token> tokens = tokenize(teststream(L"34"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_atoms() {
  list<Token> tokens = tokenize(teststream(L"34 abc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_literal() {
  list<Token> tokens = tokenize(teststream(L"34 \"abc\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_multiple_lines() {
  list<Token> tokens = tokenize(teststream(L"34\n\"abc\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_space() {
  list<Token> tokens = tokenize(teststream(L"34\n\"abc def\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_string_with_escape() {
  list<Token> tokens = tokenize(teststream(L"34\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_quote_comma_paren() {
  list<Token> tokens = tokenize(teststream(L"(',)"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L","); ++p;
  check_eq(*p, L")"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_splice_operator() {
  list<Token> tokens = tokenize(teststream(L"()',@"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L",@"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_comment() {
  list<Token> tokens = tokenize(teststream(L"()',@ ;abc def ghi"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"("); ++p;
  check_eq(*p, L")"); ++p;
  check_eq(*p, L"'"); ++p;
  check_eq(*p, L",@"); ++p;
  check_eq(*p, L";abc def ghi"); ++p;
}

void test_tokenize_ends_comment_at_newline() {
  list<Token> tokens = tokenize(teststream(L";abc def ghi\nabc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L";abc def ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_sexpr() {
  list<Token> tokens = tokenize(teststream(L"('a '(boo) \"foo\nbar\" `c `,d ,@e)\nabc ;def ghi\nabc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
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
  check(p == tokens.end());
}

void test_tokenize_suppresses_trailing_whitespace() {
  list<Token> tokens = tokenize(teststream(L"34 \nabc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_terminal_whitespace() {
  list<Token> tokens = tokenize(teststream(L"34 abc\n  "));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, L"abc"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_repeated_newline() {
  list<Token> tokens = tokenize(teststream(L"34\n\n\"abc \\\"quote def\""));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"34"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"\"abc \\\"quote def\""); ++p;
  check(p == tokens.end());
}

void test_tokenize_handles_indent_outdent() {
  list<Token> tokens = tokenize(teststream(L"abc def ghi\n\n    abc\n  def"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, OUTDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_lines() {
  list<Token> tokens = tokenize(teststream(L"abc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_whitespace_at_eol() {
  list<Token> tokens = tokenize(teststream(L"a \nb\r\nc"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"c"); ++p;
  check(p == tokens.end());
}

void test_tokenize_suppresses_initial_whitespace_lines() {
  list<Token> tokens = tokenize(teststream(L"  \nabc def ghi\n\n    \n  def"));
  list<Token>::iterator p = tokens.begin();
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == tokens.end());
}



//// insert explicit parens based on indentation.

#define inc(p) { ++p; if (p == end) return p; }
#define pop(l) { l.pop_front(); if (l.empty()) break; }
list<Token>::iterator slurpNextLine(list<Token>& line, list<Token>::iterator p, list<Token>::iterator end) {
  while (!line.empty() && whitespace(line.front().type)) pop(line);
  while (!line.empty() && !whitespace(line.front().type)) pop(line);

  if (line.empty()) // initial condition
    while (p != end) {
      if (!whitespace(p->type)) break;
      line.push_back(*p);
      inc(p);
    }

  while (p != end) {
    if (whitespace(p->type)) break;
    line.push_back(*p);
    inc(p);
  }
  while (p != end) {
    if (!whitespace(p->type)) break;
    line.push_back(*p);
    inc(p);
  }
  return p;
}
#undef pop
#undef inc

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



                                  int numWordsInLine(list<Token> line) {
                                    int numWords = 0;
                                    for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
                                      if (!whitespace(p->type)
                                          && *p != L"(" && *p != L")"
                                          && p->token[0] != L';')
                                        ++numWords;
                                    }
                                    return numWords;
                                  }

                                  bool isIndent(Token t) {
                                    return whitespace(t.type) && t != START_OF_LINE;
                                  }

                                  Token indentBefore(list<Token> line) {
                                    line.pop_front(); // there's always a START_OF_LINE
                                    if (isIndent(line.front())) return line.front();
                                    return Token::sol();
                                  }

                                  Token indentAfter(list<Token> line) {
                                    if (isIndent(line.back())) return line.back();
                                    if (line.back() == START_OF_LINE) return line.back();
                                    return Token::sol();
                                  }

                                  Token firstTokenInLine(list<Token> line) {
                                    for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
                                      if (!whitespace(p->type))
                                        return *p;
                                    }
                                    return Token::sol();
                                  }

                                  int parenCount = 0;
                                  void add(list<Token>& l, Token x) {
                                    if (!whitespace(x.type))
                                      l.push_back(x);
                                    if (x == L"(") ++parenCount;
                                    if (x == L")") --parenCount;
                                  }

                                  bool parenNotAtStartOfLine(list<Token>::iterator q, list<Token>::iterator begin) {
                                    if (*begin == START_OF_LINE) begin++;
                                    if (*begin == L"`") begin++;
                                    if (q == begin) return false;
                                    return (*q == L"(");
                                  }

list<Token> parenthesize(list<Token> in) {
  list<Token> result;
  stack<int> parenStack;
  int suppressInsert = 0;

  list<Token> line;
  Token prevLineIndent=Token::sol(), thisLineIndent=Token::sol(), nextLineIndent=Token::sol();
  for (list<Token>::iterator p = in.begin(), q = slurpNextLine(line, p, in.end());
        p != in.end();
        p = q, q = slurpNextLine(line, p, in.end())) {
    prevLineIndent=thisLineIndent, thisLineIndent=indentBefore(line), nextLineIndent=indentAfter(line);

    bool insertedParenThisLine = false;
    Token firstToken = firstTokenInLine(line);
    if (!suppressInsert && numWordsInLine(line) > 1
        && firstToken != L"(" && firstToken != L"'" && firstToken != L"`"
        && !(thisLineIndent.indentLevel == prevLineIndent.indentLevel+1 && thisLineIndent.type == MAYBE_WRAP)) {
      // open paren
      add(result, Token::of(L"("));
      parenStack.push(thisLineIndent.indentLevel);
      insertedParenThisLine = true;
    }

    // copy line tokens
    for (list<Token>::iterator q = line.begin(); q != line.end(); ++q) {
      add(result, *q);

      if (parenNotAtStartOfLine(q, line.begin()))
        suppressInsert = parenCount; // no more paren-insertion until it closes

      if (*q == L")" && parenCount <= suppressInsert) // it closed
        suppressInsert = 0;
    }

    if (suppressInsert) continue;

    if (nextLineIndent != INDENT /*TODO: MAYBE_WRAP?*/ && insertedParenThisLine) {
      // close paren for this line
      add(result, Token::of(L")"));
      parenStack.pop();
    }

    if (nextLineIndent == OUTDENT
        && !parenStack.empty() && parenStack.top() == nextLineIndent.indentLevel) {
      // close paren for a previous line
      add(result, Token::of(L")"));
      parenStack.pop();
    }
  }

  for (unsigned int i=0; i < parenStack.size(); ++i)
    result.push_back(Token::of(L")"));
  return result;
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



ostream& operator<<(ostream& os, list<Token> l) {
  bool prevWasOpen = true;
  for (list<Token>::iterator p = l.begin(); p != l.end(); ++p) {
    if (!(*p == L")" || prevWasOpen)) os << " ";
    prevWasOpen = (*p == L"(" || *p == L"'" || *p == L"," || *p == L",@");

    if (*p == START_OF_LINE || p->token[0] == L';')
      os << endl;
    else if (isIndent(*p))
      for (int i=0; i < p->indentLevel; ++i)
        os << " ";
    else
      os << *p;
  }
  os << endl;
  return os;
}



//// parse

struct AstNode {
  Token atom;
  list<AstNode> form;
  AstNode(Token t) :atom(t) {}
  AstNode(list<AstNode> l) :atom(Token::sol()), form(l) {}
  static AstNode of(Token t) {
    AstNode result(t);
    return result;
  }
  static AstNode of(list<AstNode> l) {
    AstNode result(l);
    return result;
  }
  bool isForm() {
    return !form.empty();
  }
  bool isNil() {
    return form.size() == 2
      && form.front() == L"(" && form.back() == L")";
  }
  bool operator==(Token x) {
    return form.empty() && atom == x.token; // whitespace should be gone by now.
  }
  bool operator==(string x) {
    return form.empty() && atom == x;
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, AstNode x) {
  if (x.form.empty()) os << x.atom;
  else {
    bool prevWasOpen = true;
    for (list<AstNode>::iterator p = x.form.begin(); p != x.form.end(); ++p) {
      if (!(*p == L")" || prevWasOpen)) os << " ";
      prevWasOpen = (*p == L"(" || *p == L"'" || *p == L"," || *p == L",@");
      os << *p;
    }
    os << endl;
  }
  return os;
}

list<Token>::iterator parseNext(list<Token>::iterator curr, list<Token>::iterator end, list<AstNode>& out) {
  if (curr == end) return curr;

  while (curr->token[0] == L';')
    ++curr;

  if (*curr == L")")
    cerr << "Unbalanced (" << endl << DIE;

  if (*curr != L"(" && *curr != L"'" && *curr != L"`" && *curr != L"," && *curr != L",@") {
    out.push_back(AstNode::of(*curr));
    return ++curr;
  }

  list<AstNode> subform;
  if (*curr != L"(") { // quote/comma
    subform.push_back(*curr);
    ++curr;
  }

  if (*curr == L"(") {
    subform.push_back(*curr);
    ++curr;
    while (curr != end && *curr != L")")
      curr = parseNext(curr, end, subform);
    if (curr == end)
      cerr << "Unbalanced (" << endl << DIE;
    subform.push_back(*curr);
    ++curr;
  }
  else {
    subform.push_back(*curr);
    ++curr;
  }

  out.push_back(subform);
  return curr;
}

list<AstNode> parse(list<Token> tokens) {
  list<AstNode> result;
  list<Token>::iterator p=tokens.begin();
  while (p != tokens.end())
    p=parseNext(p, tokens.end(), result);
  return result;
}

void test_parse_handles_empty_input() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L""))));
  check(ast.empty());
}

void test_parse_handles_atom() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34"))));
  list<AstNode>::iterator p = ast.begin();
  check_eq(*p, Token::of(L"34")); ++p;
  check(p == ast.end());
}

void test_parse_handles_atoms() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34\n\"a b c\""))));
  list<AstNode>::iterator p = ast.begin();
  check_eq(*p, Token::of(L"34")); ++p;
  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check(p == ast.end());
}

void test_parse_handles_forms() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34 \"a b c\""))));
  check_eq(ast.size(), 1);
  check(ast.front().isForm());
  list<AstNode>::iterator p = ast.front().form.begin();
  check_eq(*p, Token::of(L"(")); ++p;
  check_eq(*p, Token::of(L"34")); ++p;
  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check_eq(*p, Token::of(L")")); ++p;
  check(p == ast.front().form.end());
}

void test_parse_handles_nested_forms() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34 (2 3) \"a b c\""))));
  check_eq(ast.size(), 1);
  check(ast.front().isForm());
  list<AstNode>::iterator p = ast.front().form.begin();
  check_eq(*p, Token::of(L"(")); ++p;
  check_eq(*p, Token::of(L"34")); ++p;
  check(p->isForm());
    list<AstNode> ast2 = p->form; ++p;
    list<AstNode>::iterator q = ast2.begin();
    check_eq(*q, Token::of(L"(")); ++q;
    check_eq(*q, Token::of(L"2")); ++q;
    check_eq(*q, Token::of(L"3")); ++q;
    check_eq(*q, Token::of(L")")); ++q;
    check(q == ast2.end());

  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check_eq(*p, Token::of(L")")); ++p;
  check(p == ast.front().form.end());
}



//// data
////
//// stolen from picolisp: http://software-lab.de/doc/ref.html#data

                                  struct cell;
                                  extern cell* nil;

                                  #define addr(x) ((unsigned long)x)
                                  #define num(x) ((long)x)

                                  #define PTR sizeof(cell*)
                                  #define WORD sizeof(long)
                                  // tag bits
                                  #define CONS 0x0
                                  #define NUM 0x2
                                  #define SYM 0x4
                                  #define MASK 0x7

struct cell {
  cell* car;
  cell* cdr;
  long nrefs;
  cell() :car(nil), cdr(nil), nrefs(0) {}
  void init() { car=cdr=nil, nrefs=0; }
  void clear() { car=cdr=nil, nrefs=0; }
};

cell* nil = new cell;
void setupNil() {
  nil->car = nil->cdr = nil;
}

void test_pointers_from_nil_are_nil() {
  check_eq(nil->car, nil);
  check_eq(nil->cdr, nil);
}

#define HEAPCELLS (1024*1024/sizeof(cell)) // 1MB
struct Heap {
  cell cells[HEAPCELLS];
  Heap *next;
  Heap() :next(NULL) {}
};

Heap* currHeap = new Heap();
cell* currCell = &currHeap->cells[4]; // leave room for nil
cell* heapEnd = &currHeap->cells[HEAPCELLS];
cell* freelist = NULL;

                                  void growHeap() {
                                    currHeap = currHeap->next = new Heap();
                                    if (!currHeap)
                                      cerr << "Out of memory" << endl << DIE;
                                    currCell = &currHeap->cells[0];
                                    heapEnd = &currHeap->cells[HEAPCELLS];
                                  }

// cell addresses must have lower 3 bits unset
cell* newCell() {
  cell* result = NULL;
  if (freelist) {
    result = freelist;
    freelist = freelist->cdr;
    result->init();
    return result;
  }

  if (currCell == heapEnd)
    growHeap();

  result = currCell;
  ++currCell;
  return result;
}

void test_newCell_has_nil_car_and_cdr() {
  check_eq(newCell()->car, nil);
  check_eq(newCell()->cdr, nil);
}

void mkref(cell* c) {
  ++c->nrefs;
}

void rmref(cell* c) {
  if (!c) cerr << "cell should never point to NULL\n" << DIE;
  if (c == nil) return; // never gc nil

  --c->nrefs;
  if (c->nrefs > 0) return;

  rmref(c->car);
  rmref(c->cdr);

  c->clear();
  c->cdr = freelist;
  freelist = c;
}

cell* newNum(long x) {
  cell* result = newCell();
  result->car = (cell*)x;
  return (cell*)(addr(result)+NUM);
}

bool isNum(cell* x) {
  return (addr(x)&MASK) == NUM;
}

long toNum(cell* c) {
  if (!isNum(c)) return 0;
  return addr(((cell*)(addr(c)-NUM))->car);
}

bool isCons(cell* x) {
  return (addr(x)&MASK) == CONS;
}

void setCar(cell* x, cell* y) {
  if (x->car && x->car != nil && isCons(x->car)) {
    rmref(x->car);
  }
  x->car = y;
  mkref(y);
}

void setCdr(cell* x, cell* y) {
  if (x->cdr && x->cdr != nil && isCons(x->cdr)) {
    rmref(x->cdr);
  }
  x->cdr = y;
  mkref(y);
}

cell* car(cell* c) {
  return ((cell*)(addr(c)&~MASK))->car;
}
cell* cdr(cell* c) {
  return ((cell*)(addr(c)&~MASK))->cdr;
}

ostream& operator<<(ostream& os, cell* c) {
  if (!c) return os;

  if (c == nil) {
    os << "nil";
    return os;
  }

  switch(addr(c)&MASK) {
  case NUM:
    os << toNum(c); break;
  case SYM:
  case CONS:
  default:
    os << L"(" << c->car << " " << c->cdr << L")";
  }
  return os;
}

ostream& operator<<(ostream& os, list<cell*> l) {
  for (list<cell*>::iterator p = l.begin(); p != l.end(); ++p) {
    os << *p;
  }
  os << endl;
  return os;
}

                                  extern list<AstNode>::iterator buildCell(list<AstNode>::iterator, list<cell*>&);

list<cell*> buildCells(list<AstNode> in) {
  list<cell*> result;
  if (in.empty()) return result;

  list<AstNode>::iterator p = in.begin();
  while (p != in.end()) {
    p = buildCell(p, result);
  }

  return result;
}

list<AstNode>::iterator buildCell(list<AstNode>::iterator p, list<cell*>& out) {
  if (p->isNil())
    out.push_back(nil);

  else if (p->isForm()) {
    out.push_back(newCell());
//?       buildCells(n.form);
  }
  else
    out.push_back(newNum(wcstol(p->atom.token.c_str(), NULL, 0)));

  return ++p;
}

void test_build_handles_empty_input() {
  check(buildCells(parse(parenthesize(tokenize(teststream(L""))))).empty());
}

void test_build_handles_nil() {
  check_eq(buildCells(parse(parenthesize(tokenize(teststream(L"()"))))).front(), nil);
}

void test_build_handles_number() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34")))));
  check_eq(cells.size(), 1);
  check(isNum(cells.front()));
  check_eq(toNum(cells.front()), 34);
}



//// bindings

                                  struct strEq {
                                    bool operator() (const string& s1, const string& s2) const {
                                      return s1 == s2;
                                    }
                                  };

                                  struct strHash {
                                    static hash<char*> h;
                                    size_t operator() (const string& in) const {
                                      unsigned long h = 0;
                                      for (const char* s=in.c_str(); *s; ++s)
                                        h = 5 * h + *s;
                                      return size_t(h);
                                    }
                                  };

                                  template<class Data>
                                  class StringMap :public hash_map<string, Data, strHash, strEq>{};



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
  setupNil();

  int pass = 0;
  if (argc > 1) {
    std::string arg1(argv[1]);
    if (arg1 == "test") {
      runTests();
      return 0;
    }
    else if (arg1[0] >= L'0' || arg1[0] <= L'9')
      pass = atoi(arg1.c_str());
  }

  switch (pass) {
  case 1:
    cout << tokenize(cin); break;
  case 2:
    cout << parenthesize(tokenize(cin)); break;
  case 3:
    cout << parse(parenthesize(tokenize(cin))); break;
  case 4:
  default:
    cout << buildCells(parse(parenthesize(tokenize(cin)))); break;
  }
  return 0;
}

// style:
//  wide unicode strings everywhere
//  no function prototypes
//  indented functions are deemphasized, would be pushed farther down if C permitted
//  no new except in tests
//  immutable objects; copy everywhere; no references or pointers except cell*

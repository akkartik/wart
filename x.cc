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

                                  #define __unused__ __attribute__((unused))

                                  bool debug = false;
                                  #define dbg if(debug) cerr

                                  bool runningTests = false;
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
  TokenType type;
  string token;
  int indentLevel;

  Token(const TokenType t, const string x, const int l) :type(t), token(x), indentLevel(l) {}

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
  if (p.type != NON_WHITESPACE) return os << p.type;
  else return os << p.token;
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
// But do track whether the final indent char is a space. Sometimes you want to wrap a long form.
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
                                      // keep this list sync'd with the parseToken switch below
                                      if (isspace(c) || c == L',' || c == ';'
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
                                      if (c == L'\\')
                                        slurpChar(in, out); // blindly read next
                                      else if (c == L'"')
                                        break;
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



                                  bool isIndent(Token t) {
                                    return whitespace(t.type) && t != START_OF_LINE;
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
                                    return os << endl;
                                  }

                                  int numWordsInLine(list<Token> line) {
                                    int numWords = 0;
                                    for (list<Token>::iterator p = line.begin(); p != line.end(); ++p)
                                      if (!whitespace(p->type)
                                          && *p != L"(" && *p != L")"
                                          && p->token[0] != L';')
                                        ++numWords;
                                    return numWords;
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
                                    for (list<Token>::iterator p = line.begin(); p != line.end(); ++p)
                                      if (!whitespace(p->type))
                                        return *p;
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



//// parse

struct AstNode {
  Token atom;
  list<AstNode> elems;

  AstNode(Token t) :atom(t) {}
  AstNode(list<AstNode> l) :atom(Token::sol()), elems(l) {}
  static AstNode of(Token t) {
    AstNode result(t);
    return result;
  }
  static AstNode of(list<AstNode> l) {
    AstNode result(l);
    return result;
  }

  bool isAtom() {
    return elems.empty();
  }
  bool isList() {
    return !elems.empty();
  }
  bool isNil() {
    return atom == L"nil"
        || (elems.size() == 2 && elems.front() == L"(" && elems.back() == L")");
  }

  bool operator==(Token x) {
    return elems.empty() && atom == x.token; // whitespace should be gone by now.
  }
  bool operator==(string x) {
    return elems.empty() && atom == x;
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, AstNode x) {
  if (x.elems.empty()) return os << x.atom;
  bool prevWasOpen = true;
  for (list<AstNode>::iterator p = x.elems.begin(); p != x.elems.end(); ++p) {
    if (!(*p == L")" || prevWasOpen)) os << " ";
    prevWasOpen = (*p == L"(" || *p == L"'" || *p == L"," || *p == L",@");
    os << *p;
  }
  return os << endl;
}

list<Token>::iterator parseNext(list<Token>::iterator curr, list<Token>::iterator end, list<AstNode>& out) {
  if (curr == end) return curr;

  while (curr->token[0] == L';')
    ++curr;

  if (*curr == L")") cerr << "Unbalanced (" << endl << DIE;

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
    if (curr == end) cerr << "Unbalanced (" << endl << DIE;
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
  check(ast.front().isList());
  list<AstNode>::iterator p = ast.front().elems.begin();
  check_eq(*p, Token::of(L"(")); ++p;
  check_eq(*p, Token::of(L"34")); ++p;
  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check_eq(*p, Token::of(L")")); ++p;
  check(p == ast.front().elems.end());
}

void test_parse_handles_nested_forms() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34 (2 3) \"a b c\""))));
  check_eq(ast.size(), 1);
  check(ast.front().isList());
  list<AstNode>::iterator p = ast.front().elems.begin();
  check_eq(*p, Token::of(L"(")); ++p;
  check_eq(*p, Token::of(L"34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    check_eq(*q, Token::of(L"(")); ++q;
    check_eq(*q, Token::of(L"2")); ++q;
    check_eq(*q, Token::of(L"3")); ++q;
    check_eq(*q, Token::of(L")")); ++q;
    check(q == ast2.end());

  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check_eq(*p, Token::of(L")")); ++p;
  check(p == ast.front().elems.end());
}



//// data

                                  struct cell;
                                  extern cell* nil;

struct cell {
  cell* car;
  cell* cdr;
  long type;
    #define CONS 0
    #define NUM 1
    #define SYM 2
    #define STRING 3
    #define TABLE 4
  long nrefs;
  cell() :car(nil), cdr(nil), type(CONS), nrefs(0) {}
  void init() { car=cdr=nil, type=CONS, nrefs=0; }
  void clear() { car=cdr=NULL, type=CONS, nrefs=0; }
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
cell* currCell = &currHeap->cells[0];
cell* heapEnd = &currHeap->cells[HEAPCELLS];
cell* freelist = NULL;
cell* postinitCell = NULL;

void growHeap() {
  currHeap = currHeap->next = new Heap();
  if (!currHeap) cerr << "Out of memory" << endl << DIE;
  currCell = &currHeap->cells[0];
  heapEnd = &currHeap->cells[HEAPCELLS];
}

cell* newCell() {
  cell* result = NULL;
  if (freelist) {
    result = freelist;
    freelist = freelist->cdr;
    result->init();
    dbg << endl << "recycling: " << result << " " << result->type << endl;
    return result;
  }

  if (currCell == heapEnd)
    growHeap();

  result = currCell;
  ++currCell;
  return result;
}

void checkUnfreed() {
  int n = currCell-postinitCell;
  for (; freelist; freelist = freelist->cdr)
    --n;
  if (n > 0) cerr << n << " cells unfreed" << endl;
}

                                  extern void resetState();

void test_newCell_has_nil_car_and_cdr() {
  check_eq(newCell()->car, nil);
  check_eq(newCell()->cdr, nil);
}



                                  extern void rmref(cell*);

struct Table {
  hash_map<long, cell*> table;
  ~Table() {
    for (hash_map<long, cell*>::iterator p = table.begin(); p != table.end(); ++p) {
      if (!p->second) continue;
      rmref((cell*)p->first);
      rmref(p->second);
    }
  }
};

void mkref(cell* c) {
  if (c == nil) return;
  dbg << "mkref: " << c << " " << c->nrefs << endl;
  ++c->nrefs;
}

void rmref(cell* c) {
  if (!c) cerr << "rmref: cell should never point to NULL\n" << DIE;
  if (c == nil) return;
  dbg << endl << "rmref: " << c << ": " << c->nrefs << " " << c->type << endl;

  --c->nrefs;
  if (c->nrefs > 0) return;

  if (c->type != CONS && !runningTests) cerr << "deleted atom" << endl;

  switch (c->type) {
  case NUM:
    break; // numbers don't need freeing
  case STRING:
  case SYM:
    dbg << "  delete: " << *(string*)c->car << endl;
    delete (string*)c->car;
    break;
  case TABLE:
    dbg << "  delete table" << endl;
    delete (Table*)c->car;
    break;
  case CONS:
  default:
    rmref(c->car);
  }

  dbg << "  freeing " << c << endl;
  rmref(c->cdr);

  c->clear();
  c->cdr = freelist;
  freelist = c;
}

void test_rmref_frees_space() {
  cell* c = newCell();
  check_eq(c->car, nil);
  check_eq(freelist, NULL);
  rmref(c);
  check(!c->car);
  check_eq(freelist, c);
  resetState();
}

void test_rmref_handles_nums() {
  cell* c = newCell();
  c->type = NUM;
  c->car = (cell*)34;
  rmref(c);
  check(!c->car);
  check_eq(freelist, c);
  resetState();
}



                                  hash_map<long, cell*> numLiterals;
                                  cell* intern(long x) {
                                    if (numLiterals[x]) {
                                      dbg << endl << "reuse: " << x << " " << numLiterals[x] << endl;
                                      return numLiterals[x];
                                    }
                                    numLiterals[x] = newCell();
                                    numLiterals[x]->car = (cell*)x;
                                    numLiterals[x]->type = NUM;
                                    mkref(numLiterals[x]);
                                    dbg << endl << "new: " << x << " " << numLiterals[x] << endl;
                                    return numLiterals[x];
                                  }

cell* newNum(long x) {
  return intern(x);
}

bool isNum(cell* x) {
  return x->type == NUM;
}

long toNum(cell* x) {
  if (!isNum(x)) return 0;
  return (long)x->car;
}

bool isCons(cell* x) {
  return x != nil && x->type == CONS;
}

bool isAtom(cell* x) {
  return x->type != CONS && x->type != TABLE;
}

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


                                  StringMap<cell*> stringLiterals;
                                  cell* intern(string x) {
                                    if (stringLiterals[x]) {
                                      dbg << endl << "reuse: " << x << endl;
                                      return stringLiterals[x];
                                    }
                                    stringLiterals[x] = newCell();
                                    dbg << endl << "new: " << x << " " << stringLiterals[x] << endl;
                                    stringLiterals[x]->car = (cell*)new string(x); // not aligned like cells; can fragment memory
                                    mkref(stringLiterals[x]);
                                    return stringLiterals[x];
                                  }

                                  void clearLiteralTables() { // just for testing
                                    for (hash_map<long, cell*>::iterator p = numLiterals.begin(); p != numLiterals.end(); ++p) {
                                      if (p->second->nrefs > 1)
                                        cerr << "forcing unintern: " << (void*)p->second << " " << (long)p->second->car << " " << p->second->nrefs << endl;
                                      while (p->second->nrefs > 0)
                                        rmref(p->second);
                                    }
                                    numLiterals.clear();
                                    for (StringMap<cell*>::iterator p = stringLiterals.begin(); p != stringLiterals.end(); ++p) {
                                      if (p->first == L"currLexicalScope") continue; // memory leak
                                      if (p->second->nrefs > 1)
                                        cerr << "forcing unintern: " << (void*)p->second << " " << *(string*)p->second->car << " " << p->second->nrefs << endl;
                                      while (p->second->nrefs > 0)
                                        rmref(p->second);
                                    }
                                    stringLiterals.clear();
                                  }

cell* newSym(string x) {
  cell* result = intern(x);
  result->type = SYM;
  return result;
}

bool isSym(cell* x) {
  return x->type == SYM;
}

cell* newString(string x) {
  cell* result = intern(x);
  result->type = STRING;
  return result;
}

bool isString(cell* x) {
  return x->type == STRING;
}

string toString(cell* x) {
  if (!isString(x) && !isSym(x))
    return L"";
  return *(string*)x->car;
}

cell* newTable() {
  cell* result = newCell();
  result->type = TABLE;
  result->car = (cell*)new Table();
  return result;
}

bool isTable(cell* x) {
  return x->type == TABLE;
}



cell* car(cell* x) {
  if (x->type != CONS) {
    cerr << "car of non-cons" << endl;
    return nil;
  }
  return x->car;
}

cell* cdr(cell* x) {
  return x->cdr;
}

void setCar(cell* x, cell* y) {
  if (isCons(car(x)))
    rmref(car(x));
  x->car = y;
  mkref(y);
}

void setCdr(cell* x, cell* y) {
  if (cdr(x) != nil)
    rmref(cdr(x));
  x->cdr = y;
  mkref(y);
}

                                  void unsafeSet(cell* t, cell* k, cell* val, bool deleteNils) {
                                    if (!isTable(t)) {
                                      cerr << "set on a non-table" << endl;
                                      return;
                                    }
                                    hash_map<long, cell*>& table = ((Table*)t->car)->table;
                                    long key = (long)k;
                                    if (table[key])
                                      rmref(table[key]);
                                    if (deleteNils && val == nil) {
                                      rmref(k);
                                      table[key] = NULL;
                                      return;
                                    }
                                    if (!table[key]) mkref(k);
                                    mkref(val);
                                    table[key] = val;
                                  }

void set(cell* t, cell* k, cell* val) {
  unsafeSet(t, k, val, true);
}

                                  cell* unsafeGet(cell* t, cell* k) {
                                    if (!isTable(t)) {
                                      cerr << "get on a non-table" << endl;
                                      return nil;
                                    }
                                    hash_map<long, cell*>& table = ((Table*)t->car)->table;
                                    long key = (long)k;
                                    return table[key];
                                  }

cell* get(cell* t, cell* k) {
  cell* result = unsafeGet(t, k);
  if (!result) return nil;
  return result;
}



                                  ostream& operator<<(ostream& os, cell* c);

                                  ostream& operator<<(ostream& os, Table* t) {
                                    os << "{" << endl;
                                    for (hash_map<long, cell*>::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                      os << (cell*)p->first << ": " << p->second << endl;
                                    }
                                    return os << "}" << endl;
                                  }

                                  ostream& operator<<(ostream& os, cell* c) {
                                    if (c == NULL) return os << "NULLNULLNULL";
                                    if (c == nil) return os << "nil";
                                    switch(c->type) {
                                    case NUM:
                                      os << toNum(c); break;
                                    case SYM:
                                    case STRING:
                                      os << toString(c); break;
                                    case TABLE:
                                      os << (Table*)c->car;
                                      os << cdr(c);
                                      break;
                                    case CONS:
                                    default:
                                      os << L"<" << car(c) << " . " << cdr(c) << L">";
                                    }
                                    return os;
                                  }

ostream& operator<<(ostream& os, list<cell*> l) {
  for (list<cell*>::iterator p = l.begin(); p != l.end(); ++p)
    os << *p;
  return os << endl;
}



                                  extern cell* buildCell(AstNode);

list<cell*> buildCells(list<AstNode> in) {
  list<cell*> result;
  if (in.empty()) return result;
  for (list<AstNode>::iterator p = in.begin(); p != in.end(); ++p)
    result.push_back(buildCell(*p));
  return result;
}

cell* buildCell(AstNode n) {
  if (n.isNil())
    return nil;

  if (n.isAtom()) {
    if (n.atom.token == L")")
      cerr << "Syntax error: unbalanced )" << endl << DIE;

    char* end;
    long v = wcstol(n.atom.token.c_str(), &end, 0);
    if (*end == L'\0')
      return newNum(v);

    if (n.atom.token.c_str()[0] == L'"')
      return newString(n.atom.token);
    return newSym(n.atom.token);
  }

  if (n.elems.size() == 2 && (n.elems.front() == L"'" || n.elems.front() == L"`")
      && n.elems.back().isAtom()) {
    cell* newForm = newCell();
    setCar(newForm, buildCell(n.elems.front()));
    setCdr(newForm, buildCell(n.elems.back()));
    return newForm;
  }

  cell* newForm = NULL;
  cell* curr = NULL;
  for (list<AstNode>::iterator q = n.elems.begin(); q != n.elems.end(); ++q) {
    if (q->atom == L"(")
      continue;
    if (q->atom == L")")
      break;

    if (q->atom == L".") {
      ++q;
      if (!curr) cerr << "Syntax error: dot at start of expression" << endl << DIE;
      setCdr(curr, buildCell(*q));
      break;
    }

    if (!curr) {
      newForm = curr = newCell();
    }
    else {
      setCdr(curr, newCell());
      curr = cdr(curr);
    }

    setCar(curr, buildCell(*q));
  }

  return newForm;
}

void test_build_handles_empty_input() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"")))));
  check(cells.empty());
  resetState();
}

void test_build_handles_nil() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"()")))));
  check_eq(cells.front(), nil);
  resetState();
}

void test_build_handles_nil2() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"nil")))));
  check_eq(cells.front(), nil);
  resetState();
}

void test_build_handles_number() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34")))));
  check_eq(cells.size(), 1);
  check(isNum(cells.front()));
  check_eq(toNum(cells.front()), 34);
  check_eq(cells.front()->nrefs, 1);
  resetState();
}

void test_build_handles_symbol() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"a")))));
  check_eq(cells.size(), 1);
  check(isSym(cells.front()));
  check_eq(toString(cells.front()), L"a");
  check_eq(cells.front()->nrefs, 1);
  resetState();
}

void test_build_handles_quoted_symbol() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"'a")))));
  check_eq(cells.size(), 1);
  check(isCons(cells.front()));
  check(isSym(car(cells.front())));
  check_eq(toString(car(cells.front())), L"'");
  check_eq(car(cells.front())->nrefs, 2);
  check(isSym(car(cells.front())));
  check_eq(toString(cdr(cells.front())), L"a");
  check_eq(cdr(cells.front())->nrefs, 2);
  rmref(cells.front());
  resetState();
}

void test_build_handles_multiple_atoms() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34\n35")))));
  check_eq(cells.size(), 2);
  cell* c = cells.front();
  check(isNum(c));
  check_eq(toNum(c), 34);
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

  c = cells.back();
  check(isNum(c));
  check_eq(toNum(c), 35);
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

  resetState();
}

void test_build_handles_form() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34 35")))));
  check_eq(cells.size(), 1);
  cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 34);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 35);
  check_eq(car(c)->nrefs, 2);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  resetState();
}

void test_build_handles_dot() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34 . 35")))));
  check_eq(cells.size(), 1);
  cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 34);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isNum(c));
  check_eq(toNum(c), 35);
  check_eq(c->nrefs, 2);

  rmref(cells.front());
  resetState();
}

void test_build_handles_nested_form() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 23))")))));
  check_eq(cells.size(), 1);
  cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 3);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 7);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
    cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(c2->car));
    check_eq(toNum(c2->car), 33);
    check_eq(c2->car->nrefs, 2);
    c2 = c2->cdr;
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(c2->car));
    check_eq(toNum(c2->car), 23);
    check_eq(c2->car->nrefs, 2);
    check_eq(c2->cdr, nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  resetState();
}

void test_build_handles_strings() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 \"abc\" 23))")))));
  check_eq(cells.size(), 1);
  cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 3);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 7);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
    cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(c2->car));
    check_eq(toNum(c2->car), 33);
    check_eq(c2->car->nrefs, 2);
    c2 = c2->cdr;
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isString(c2->car));
    check_eq(toString(c2->car), L"\"abc\"");
    check_eq(c2->car->nrefs, 2);
    c2 = c2->cdr;
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(c2->car));
    check_eq(toNum(c2->car), 23);
    check_eq(c2->car->nrefs, 2);
    check_eq(c2->cdr, nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  resetState();
}

void test_build_handles_syms() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 \"abc\" 3de 23))")))));
  check_eq(cells.size(), 1);
  cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 3);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 7);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
    cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(c2->car));
    check_eq(toNum(c2->car), 33);
    check_eq(c2->car->nrefs, 2);
    c2 = c2->cdr;
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isString(c2->car));
    check_eq(toString(c2->car), L"\"abc\"");
    check_eq(c2->car->nrefs, 2);
    c2 = c2->cdr;
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isSym(c2->car));
    check_eq(toString(c2->car), L"3de");
    check_eq(c2->car->nrefs, 2);
    c2 = c2->cdr;
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(c2->car));
    check_eq(toNum(c2->car), 23);
    check_eq(c2->car->nrefs, 2);
    check_eq(c2->cdr, nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  resetState();
}

void test_build_handles_quotes() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"`(34 ,35)")))));
  check_eq(cells.size(), 1);
  cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isSym(car(c)));
  check_eq(toString(car(c)), L"`");
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 34);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
    cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isSym(c2->car));
    check_eq(toString(c2->car), L",");
    check_eq(c2->car->nrefs, 2);
    c2 = c2->cdr;
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(c2->car));
    check_eq(toNum(c2->car), 35);
    check_eq(c2->car->nrefs, 2);
    check_eq(c2->cdr, nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  resetState();
}



//// bindings

                                  hash_map<long, stack<cell*> > dynamics;
                                  cell* lookupDynamicBinding(cell* sym) {
                                    stack<cell*> bindings = dynamics[(long)sym];
                                    if (bindings.empty()) return NULL;
                                    return bindings.top();
                                  }

                                  void newDynamicScope(cell* sym, cell* val) {
                                    mkref(sym);
                                    mkref(val);
                                    dynamics[(long)sym].push(val);
                                  }

                                  void newDynamicScope(string s, cell* val) {
                                    newDynamicScope(newSym(s), val);
                                  }

                                  void endDynamicScope(cell* sym) {
                                    stack<cell*>& bindings = dynamics[(long)sym];
                                    if (bindings.empty()) {
                                      cerr << "No dynamic binding for " << sym << endl;
                                      return;
                                    }
                                    rmref(sym);
                                    rmref(bindings.top());
                                    bindings.pop();
                                  }

                                  void assignDynamicVar(cell* sym, cell* val) {
                                    stack<cell*>& bindings = dynamics[(long)sym];
                                    if (bindings.empty()) {
                                      cerr << "No dynamic binding to assign for " << sym << endl;
                                      return;
                                    }
                                    rmref(bindings.top());
                                    bindings.pop();
                                    mkref(val);
                                    bindings.push(val);
                                  }

                                  // the current lexical scope is a first-class dynamic variable
                                  #define currLexicalScopes dynamics[(long)newSym(L"currLexicalScope")]
                                  void setupLexicalScope() {
                                    newDynamicScope(L"currLexicalScope", nil);
                                  }

                                  void test_lexical_scope_has_nil_cdr_on_startup() {
                                    check_eq(currLexicalScopes.size(), 1);
                                    cell* currLexicalScope = currLexicalScopes.top();
                                    check_eq(currLexicalScope->cdr, nil);
                                  }

                                  cell* lookupLexicalBinding(cell* sym) {
                                    for (cell* scope = currLexicalScopes.top(); scope != nil; scope = scope->cdr) {
                                      cell* result = unsafeGet(scope, sym);
                                      if (result) return result;
                                    }
                                    return NULL;
                                  }

                                  // entering and leaving lexical scopes *assigns the current dynamic*
                                  // binding of the currLexicalScope sym.
                                  // Calling functions will create new dynamic bindings.
                                  void newLexicalScope(cell* newScope) {
                                    cell* oldScope = currLexicalScopes.top();
                                    dbg << "new lexical scope: " << newScope << endl;
                                    setCdr(newScope, oldScope);
                                    mkref(newScope);
                                    assignDynamicVar(newSym(L"currLexicalScope"), newScope);
                                  }
                                  void newLexicalScope() {
                                    newLexicalScope(newTable());
                                  }

                                  void endLexicalScope() {
                                    cell* currScope = currLexicalScopes.top();
                                    if (currScope == nil)
                                      cerr << "No lexical scope to end" << endl << DIE;
                                    dbg << "end lexical scope: " << currScope << endl;
                                    cell* oldScope = cdr(currScope);
                                    rmref(currScope);
                                    assignDynamicVar(newSym(L"currLexicalScope"), oldScope);
                                  }

                                  void addLexicalBinding(cell* scope, cell* sym, cell* val) {
                                    dbg << "creating binding: " << (void*)scope << " " << sym << endl;
                                    if (unsafeGet(scope, sym)) cerr << "Can't rebind within a lexical scope" << endl << DIE;
                                    unsafeSet(scope, sym, val, false);
                                  }
                                  void addLexicalBinding(cell* sym, cell* val) {
                                    addLexicalBinding(currLexicalScopes.top(), sym, val);
                                  }

cell* lookup(cell* sym) {
  cell* result = lookupLexicalBinding(sym);
  if (result) return result;
  result = lookupDynamicBinding(sym);
  if (result) return result;
  cerr << "No binding for " << toString(sym) << endl;
  return nil;
}

void test_lookup_returns_dynamic_binding() {
  cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newDynamicScope(sym, val);
    check_eq(lookup(sym), val);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 2);
  endDynamicScope(sym);
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  resetState();
}

void test_lookup_returns_lexical_binding() {
  cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      check_eq(lookup(sym), val);
      check_eq(sym->nrefs, 2);
      check_eq(val->nrefs, 2);
  endLexicalScope();
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  resetState();
}

void test_lexical_binding_always_overrides_dynamic() {
  cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  cell* dynVal = newNum(35);
  check_eq(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
    newLexicalScope();
      addLexicalBinding(sym, val);
        check_eq(lookup(sym), val);
        check_eq(sym->nrefs, 3);
        check_eq(val->nrefs, 2);
        check_eq(dynVal->nrefs, 2);
    endLexicalScope();

    check_eq(lookup(sym), newNum(35));
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
  endDynamicScope(sym);
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  check_eq(dynVal->nrefs, 1);
  resetState();
}

void test_nil_lexical_binding_works() {
  cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  cell* dynVal = newNum(35);
  newDynamicScope(sym, dynVal);
    newLexicalScope();
      addLexicalBinding(sym, nil);
        check_eq(lookup(sym), nil);
    endLexicalScope();
  endDynamicScope(sym);
  resetState();
}

void test_lexical_scopes_nest_correctly() {
  cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  cell* val2 = newNum(35);
  check_eq(val->nrefs, 1);
  cell* dynVal = newNum(36);
  check_eq(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(val2->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
    newLexicalScope();
      check(currLexicalScopes.top() != nil);
      check_eq(currLexicalScopes.top()->nrefs, 2);
      addLexicalBinding(sym, val);
        check_eq(lookup(sym), val);
        check_eq(sym->nrefs, 3);
        check_eq(val->nrefs, 2);
        check_eq(val2->nrefs, 1);
        check_eq(dynVal->nrefs, 2);
        newLexicalScope();
          check_eq(currLexicalScopes.top()->cdr->nrefs, 2);
          check_eq(currLexicalScopes.top()->nrefs, 2);
          addLexicalBinding(sym, val2);
            check_eq(lookup(sym), val2);
            check_eq(sym->nrefs, 4);
            check_eq(val->nrefs, 2);
            check_eq(val2->nrefs, 2);
            check_eq(dynVal->nrefs, 2);
        endLexicalScope();
      check_eq(currLexicalScopes.top()->nrefs, 2);
      check_eq(sym->nrefs, 3);
      check_eq(val->nrefs, 2);
      check_eq(val2->nrefs, 1);
      check_eq(dynVal->nrefs, 2);
    endLexicalScope();
    check_eq(lookup(sym), dynVal);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(val2->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
  endDynamicScope(sym);
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  check_eq(val2->nrefs, 1);
  check_eq(dynVal->nrefs, 1);
  resetState();
}

void test_lower_lexical_scopes_are_available() {
  cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      check_eq(lookup(sym), val);
      newLexicalScope();
        check_eq(lookup(sym), val);
      endLexicalScope();
  endLexicalScope();
  resetState();
}



                                  cell* sig(cell* lambda) {
                                    return car(cdr(lambda));
                                  }

                                  cell* body(cell* lambda) {
                                    return cdr(cdr(lambda));
                                  }

                                  cell* callee_body(cell* callee) {
                                    return car(cdr(cdr(callee)));
                                  }

                                  cell* callee_env(cell* callee) {
                                    return cdr(cdr(cdr(callee)));
                                  }

                                  cell* call_args(cell* call) {
                                    return cdr(call);
                                  }

                                  bool isQuoted(cell* cell) {
                                    return isCons(cell) && car(cell) == newSym(L"'");
                                  }

                                  cell* unQuote(cell* cell) {
                                    if (isQuoted(cell))
                                      return cdr(cell);
                                    return cell;
                                  }

                                  extern cell* eval(cell*);

                                  cell* eval_all(cell* arg) {
                                    if (arg == nil) return arg;
                                    if (!isCons(arg)) return eval(arg);
                                    cell* result = newCell();
                                    setCar(result, eval(car(arg)));
                                    setCdr(result, eval_all(cdr(arg)));
                                    return result;
                                  }

void bindArg(cell* scope, cell* param, cell* arg, bool quoted) {
  if (param == nil) return;

  if (isQuoted(param)) {
    bindArg(scope, cdr(param), arg, true);
    return;
  }

  if (isSym(param) || isQuoted(param)) {
    cell* result = (quoted || isQuoted(param)) ? arg : eval_all(arg);
    addLexicalBinding(scope, unQuote(param), result);
  }
  else {
    bindArg(scope, car(param), car(arg), quoted);
  }

  bindArg(scope, cdr(param), cdr(arg), quoted);
}

cell* bindArgs(cell* params, cell* args) {
  cell* scope = newTable();
  bindArg(scope, params, args, false);
  return scope;
}

// eval each arg or not, depending on quotes in params
// eval each arg in rest, then return a list
void test_bindArgs_handles_vararg() {
  cell* params = buildCells(parse(parenthesize(tokenize(teststream(L"a"))))).front();
  cell* args = buildCells(parse(parenthesize(tokenize(teststream(L"(1)"))))).front();
  cell* result = unsafeGet(bindArgs(params, args), newSym(L"a"));
  check_eq(car(result), newNum(1));
  check_eq(cdr(result), nil);
  rmref(result);
  rmref(args);
  rmref(params);
  resetState();
}

cell* eval(cell* expr) {
  if (!expr)
    cerr << "eval: cell should never be NULL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isSym(expr))
    return lookup(expr);

  if (isAtom(expr))
    return expr;

  if (isQuoted(expr))
    return expr->cdr;

  // lambda expressions get the current lexical scope attached to them
  if (car(expr) == newSym(L"lambda")) {
    cell* ans = newCell();
    setCar(ans, car(expr));
    setCdr(ans, newCell());
    setCar(cdr(ans), sig(expr));
    setCdr(cdr(ans), newCell());
    setCar(cdr(cdr(ans)), body(expr));
    setCdr(cdr(cdr(ans)), currLexicalScopes.top());
    return ans;
  }

  cell* lambda = eval(car(expr));
  mkref(lambda);

  // construct a new scope with args based on current scope and sig
  cell* newScope = bindArgs(sig(lambda), call_args(expr));
  // swap in the function's lexical environment
  newDynamicScope(L"currLexicalScope", callee_env(lambda));
  // throw the new scope on it
  newLexicalScope(newScope);

  // eval all forms in body; save result of final form
  cell* result = nil;
  for (cell* form = callee_body(lambda); form != nil; form = cdr(form))
    result = eval(form->car);
  rmref(lambda);

  mkref(result);
  endLexicalScope();
  endDynamicScope(newSym(L"currLexicalScope"));
  return result;
}

void test_nil_evals_to_itself() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"()")))));
  check_eq(cells.size(), 1);
  check_eq(eval(cells.front()), nil);
  rmref(cells.front());
  resetState();
}

void test_num_evals_to_itself() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34")))));
  check_eq(cells.size(), 1);
  check_eq(eval(cells.front()), cells.front());
  rmref(cells.front());
  resetState();
}

void test_string_evals_to_itself() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"\"ac bd\"")))));
  check_eq(cells.size(), 1);
  check_eq(eval(cells.front()), cells.front());
  rmref(cells.front());
  resetState();
}

void test_eval_handles_quoted_atoms() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"'a '34")))));
  check_eq(cells.size(), 2);
  check_eq(eval(cells.front()), newSym(L"a"));
  check_eq(eval(cells.back()), newNum(34));
  rmref(cells.front());
  rmref(cells.back());
  resetState();
}

void test_eval_handles_quoted_lists() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"'(a b)")))));
  cell* c = eval(cells.front());
  check_eq(car(c), newSym(L"a"));
  c = cdr(c);
  check_eq(car(c), newSym(L"b"));
  check_eq(cdr(c), nil);
  rmref(cells.front());
  resetState();
}

void test_eval_handles_simple_lambda() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 34)")))));
  check_eq(cells.size(), 1);
  cell* lambda = eval(cells.front());
  check_eq(lambda->car, newSym(L"lambda"));
  check_eq(lambda->cdr->car, nil);
  check(isCons(lambda->cdr->cdr->car));
  check_eq(lambda->cdr->cdr->car->car, newNum(34));
  check_eq(lambda->cdr->cdr->cdr, nil);
  rmref(cells.front());
  rmref(lambda);
  resetState();
}

void test_eval_handles_closure() {
  list<cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 34)")))));
  check_eq(cells.size(), 1);
  newLexicalScope();
    cell* newLexicalScope = currLexicalScopes.top();
    check_eq(newLexicalScope->nrefs, 2);
    cell* c = eval(cells.front());
    check_eq(newLexicalScope->nrefs, 3);
  endLexicalScope();
  check_eq(newLexicalScope->nrefs, 1);
  check_eq(car(c), newSym(L"lambda"));
  check_eq(car(cdr(c)), nil);
  check_eq(car(car(cdr(cdr(c)))), newNum(34));
  check_eq(cdr(cdr(cdr(c))), newLexicalScope);
  rmref(c);
  check_eq(newLexicalScope->nrefs, 0);
  rmref(cells.front());
  resetState();
}

void test_eval_handles_lambda_calls() {
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda () 34))"))))).front();
  cell* result = eval(call);
  check_eq(result, newNum(34));
  rmref(call);
  rmref(result);
  resetState();
}

void test_eval_expands_syms_in_lambda_bodies() {
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda () a))"))))).front();
  newDynamicScope(L"a", newNum(34));
  cell* result = eval(lambda);
  check_eq(result, newNum(34));
  endDynamicScope(newSym(L"a"));
  rmref(lambda);
  rmref(result);
  resetState();
}

void test_eval_handles_assigned_lambda_calls() {
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 34)"))))).front();
  newDynamicScope(L"f", eval(lambda));
    cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f)"))))).front();
    cell* result = eval(call);
    check_eq(result, newNum(34));
  endDynamicScope(newSym(L"f"));
  rmref(result);
  rmref(call);
  rmref(lambda);
  resetState();
}

void test_eval_expands_lexically_scoped_syms_in_lambda_bodies() {
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda () a))"))))).front();
  newLexicalScope();
    addLexicalBinding(newSym(L"a"), newNum(34));
    cell* result = eval(call);
    check_eq(result, newNum(34));
  endLexicalScope();
  rmref(result);
  rmref(call);
  resetState();
}

void test_eval_expands_syms_in_original_lexical_scope() {
  newDynamicScope(L"a", newNum(23));
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () a)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"a"), newNum(34));
    newDynamicScope(L"f", eval(lambda));
  endLexicalScope();
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f)"))))).front();
  cell* result = eval(call);
  check_eq(result, newNum(34));
  rmref(result);
  rmref(call);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  resetState();
}

void test_eval_expands_args_in_caller_scope() {
  newDynamicScope(L"a", newNum(23));
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda (arg1) arg1)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    newDynamicScope(L"f", eval(lambda));
  endLexicalScope();
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f a)"))))).front();
  cell* result = eval(call);
  check_eq(result, newNum(23));
  rmref(result);
  rmref(call);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  resetState();
}

void test_eval_doesnt_eval_quoted_params() {
  newDynamicScope(L"a", newNum(23));
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda ('arg1) arg1)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    newDynamicScope(L"f", eval(lambda));
  endLexicalScope();
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f a)"))))).front();
  cell* result = eval(call);
  check_eq(result, newSym(L"a"));
  rmref(result);
  rmref(call);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  resetState();
}

void test_eval_handles_quoted_param_list() {
  newDynamicScope(L"a", newNum(23));
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda '(arg1) arg1)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    newDynamicScope(L"f", eval(lambda));
  endLexicalScope();
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f a)"))))).front();
  cell* result = eval(call);
  check_eq(result, newSym(L"a"));
  rmref(result);
  rmref(call);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  resetState();
}

void test_eval_handles_multiple_args() {
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda (a b) b)"))))).front();
  newDynamicScope(L"f", eval(lambda));
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f 1 2)"))))).front();
  cell* result = eval(call);
  check_eq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  resetState();
}

void test_eval_handles_multiple_body_exprs() {
  cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 1 2)"))))).front();
  newDynamicScope(L"f", eval(lambda));
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f)"))))).front();
  cell* result = eval(call);
  check_eq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  resetState();
}

void test_eval_handles_vararg_param() {
  cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda args args) 1)"))))).front();
  cell* result = eval(call);
  check(isCons(result));
  check_eq(car(result), newNum(1));
  rmref(result);
  rmref(call);
  resetState();
}



                                  typedef void (*testfunc)(void);

                                  const testfunc tests[] = {
                                    #include"test_list"
                                  };

                                  void runTests() {
                                    runningTests = true; // never reset
                                    for (unsigned int i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
                                      (*tests[i])();
                                    }
                                    cerr << endl;
                                    if (numFailures == 0) return;

                                    cerr << numFailures << " failure";
                                        if (numFailures > 1) cerr << "s";
                                        cerr << endl;
                                  }

                                  void resetState() {
                                    clearLiteralTables();
                                    checkUnfreed();
                                    setupLexicalScope();
                                  }

int main(int argc, ascii* argv[]) {
  setupNil();
  setupLexicalScope();
  postinitCell = currCell;

  int pass = 0;
  if (argc > 1) {
    std::string arg1(argv[1]);
    if (arg1 == "test") {
      runTests();
      return 0;
    }
    else if (arg1[0] >= L'0' || arg1[0] <= L'9') {
      pass = atoi(arg1.c_str());
    }
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

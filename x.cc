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



//// tokenize input including newlines and indent

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

// BEWARE: tab = 1 space; don't mix the two
// But do track whether the final indent char is a space. Sometimes you want
// to wrap a long form.
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



//// insert explicit parens based on indentation

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



//// construct parse tree out of tokens

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



//// cell: core lisp data structure with ref-counted garbage collection

                                  struct Cell;
                                  extern Cell* nil;

struct Cell {
  Cell* car;
  Cell* cdr;
  long type;
    #define CONS 0
    #define NUM 1
    #define SYM 2
    #define STRING 3
    #define TABLE 4
    #define PRIM_FUNC 5
  long nrefs;
  Cell() :car(nil), cdr(nil), type(CONS), nrefs(0) {}
  void init() { car=cdr=nil, type=CONS, nrefs=0; }
  void clear() { car=cdr=NULL, type=CONS, nrefs=0; }
};

Cell* nil = new Cell;
void setupNil() {
  nil->car = nil->cdr = nil;
}

void test_pointers_from_nil_are_nil() {
  check_eq(nil->car, nil);
  check_eq(nil->cdr, nil);
}



#define HEAPCELLS (1024*1024/sizeof(Cell)) // 1MB
struct Heap {
  Cell Cells[HEAPCELLS];
  Heap *next;
  Heap() :next(NULL) {}
};

Heap* currHeap = new Heap();
Cell* heapStart = &currHeap->Cells[0];
Cell* heapEnd = &currHeap->Cells[HEAPCELLS];
Cell* currCell = heapStart;
Cell* freelist = NULL;

void growHeap() {
  currHeap = currHeap->next = new Heap();
  if (!currHeap) cerr << "Out of memory" << endl << DIE;
  currCell = &currHeap->Cells[0];
  heapEnd = &currHeap->Cells[HEAPCELLS];
}

Cell* newCell() {
  Cell* result = NULL;
  if (freelist) {
    result = freelist;
    freelist = freelist->cdr;
    result->init();
    dbg << endl << "newCell r: " << result << " " << result->type << endl;
    return result;
  }

  if (currCell == heapEnd)
    growHeap();

  result = currCell;
  ++currCell;
  dbg << endl << "newCell a: " << result << " " << result->type << endl;
  return result;
}

void checkUnfreed() {
  int n = currCell-heapStart-1; // ignore empty currLexicalScopes
  for (; freelist; freelist = freelist->cdr)
    --n;
  check_eq(n, 0);
}

                                  extern void resetState();
                                  extern void checkState();

                                  extern void rmref(Cell*);

void test_newCell_has_nil_car_and_cdr() {
  Cell* x = newCell();
  check_eq(x->car, nil);
  check_eq(x->cdr, nil);
  rmref(x);
  checkState();
}



struct Table {
  hash_map<long, Cell*> table;
  ~Table() {
    for (hash_map<long, Cell*>::iterator p = table.begin(); p != table.end(); ++p) {
      if (!p->second) continue;
      rmref((Cell*)p->first);
      rmref(p->second);
    }
  }
};

Cell* mkref(Cell* c) {
  if (c == nil) return nil;
  dbg << "mkref: " << c << " " << c->nrefs << endl;
  ++c->nrefs;
  return c;
}

void rmref(Cell* c) {
  if (!c)
    cerr << "rmref: cell should never point to NULL\n" << DIE;
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
    delete (string*)c->car; break;
  case CONS:
    rmref(c->car); break;
  case TABLE:
    dbg << "  delete table" << endl;
    delete (Table*)c->car; break;
  case PRIM_FUNC:
    break; // compiled functions don't need freeing
  default:
    cerr << "Can't rmref type " << c->type << endl << DIE;
  }

  dbg << "  freeing " << c << endl;
  rmref(c->cdr);

  c->clear();
  c->cdr = freelist;
  freelist = c;
}

void test_rmref_frees_space() {
  Cell* c = newCell();
  check_eq(c->car, nil);
  check_eq(freelist, NULL);
  rmref(c);
  check(!c->car);
  check_eq(freelist, c);
  checkState();
}

void test_rmref_handles_nums() {
  Cell* c = newCell();
  c->type = NUM;
  c->car = (Cell*)34;
  rmref(c);
  check(!c->car);
  check_eq(freelist, c);
  checkState();
}



bool isCons(Cell* x) {
  return x != nil && x->type == CONS;
}

bool isAtom(Cell* x) {
  return x == nil || x->type == NUM || x->type == STRING || x->type == SYM;
}

Cell* newTable() {
  Cell* result = newCell();
  result->type = TABLE;
  result->car = (Cell*)new Table();
  return result;
}

bool isTable(Cell* x) {
  return x->type == TABLE;
}

                                  hash_map<long, Cell*> numLiterals;
                                  Cell* intern(long x) {
                                    if (numLiterals[x]) {
                                      dbg << endl << "reuse: " << x << " " << numLiterals[x] << endl;
                                      return numLiterals[x];
                                    }
                                    numLiterals[x] = newCell();
                                    numLiterals[x]->car = (Cell*)x;
                                    numLiterals[x]->type = NUM;
                                    mkref(numLiterals[x]);
                                    dbg << endl << "new: " << x << " " << numLiterals[x] << endl;
                                    return numLiterals[x];
                                  }

Cell* newNum(long x) {
  return intern(x);
}

bool isNum(Cell* x) {
  return x->type == NUM;
}

long toNum(Cell* x) {
  if (!isNum(x)) return 0;
  return (long)x->car;
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


                                  StringMap<Cell*> stringLiterals;
                                  Cell* intern(string x) {
                                    if (stringLiterals[x]) {
                                      dbg << endl << "reuse: " << x << endl;
                                      return stringLiterals[x];
                                    }
                                    stringLiterals[x] = newCell();
                                    dbg << endl << "new: " << x << " " << stringLiterals[x] << endl;
                                    stringLiterals[x]->car = (Cell*)new string(x); // not aligned like cells; can fragment memory
                                    mkref(stringLiterals[x]);
                                    return stringLiterals[x];
                                  }

Cell* newSym(string x) {
  Cell* result = intern(x);
  result->type = SYM;
  return result;
}

bool isSym(Cell* x) {
  return x->type == SYM;
}

Cell* newString(string x) {
  Cell* result = intern(x);
  result->type = STRING;
  return result;
}

bool isString(Cell* x) {
  return x->type == STRING;
}

string toString(Cell* x) {
  if (!isString(x) && !isSym(x))
    return L"";
  return *(string*)x->car;
}

typedef Cell* (*PrimFunc)(void);
Cell* newPrimFunc(PrimFunc f) {
  Cell* result = newCell();
  result->type = PRIM_FUNC;
  result->car = (Cell*)f;
  return result;
}

bool isPrimFunc(Cell* x) {
  return x->type == PRIM_FUNC;
}

PrimFunc toPrimFunc(Cell* x) {
  if (!isPrimFunc(x))
    cerr << "Not a compiled function" << endl << DIE;
  return (PrimFunc)x->car;
}



Cell* car(Cell* x) {
  if (x->type != CONS) {
    cerr << "car of non-cons" << endl;
    return nil;
  }
  return x->car;
}

Cell* cdr(Cell* x) {
  return x->cdr;
}

void setCar(Cell* x, Cell* y) {
  mkref(y);
  if (isCons(x))
    rmref(car(x));
  x->car = y;
}

void setCdr(Cell* x, Cell* y) {
  mkref(y);
  rmref(cdr(x));
  x->cdr = y;
}

void test_setCar_decrements_nrefs() {
  Cell* cons = newCell();
  Cell* car = newCell();
  check_eq(car->nrefs, 0);
  Cell* newCar = newCell();
  check_eq(newCar->nrefs, 0);
  setCar(cons, car);
  check_eq(car->nrefs, 1);
  check_eq(newCar->nrefs, 0);
  setCar(cons, newCar);
  check_eq(car->nrefs, 0);
  check_eq(newCar->nrefs, 1);
  rmref(cons);
  checkState();
}

void test_setCar_decrements_nrefs_for_non_cons() {
  Cell* cons = newCell();
  Cell* num = newNum(23);
  check_eq(num->nrefs, 1);
  Cell* newCar = newCell();
  check_eq(newCar->nrefs, 0);
  setCar(cons, num);
  check_eq(num->nrefs, 2);
  check_eq(newCar->nrefs, 0);
  setCar(cons, newCar);
  check_eq(num->nrefs, 1);
  check_eq(newCar->nrefs, 1);
  rmref(cons);
  checkState();
}

void test_setCar_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  check_eq(x->nrefs, 0);
  setCar(cons, x);
  check_eq(x->nrefs, 1);
  setCar(cons, x);
  check_eq(x->nrefs, 1);
  check(car(x));
  check(cdr(x));
  rmref(cons);
  checkState();
}

void test_setCdr_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  check_eq(x->nrefs, 0);
  setCdr(cons, x);
  check_eq(x->nrefs, 1);
  setCdr(cons, x);
  check_eq(x->nrefs, 1);
  check(car(x));
  check(cdr(x));
  rmref(cons);
  checkState();
}

                                  void unsafeSet(Cell* t, Cell* k, Cell* val, bool deleteNils) {
                                    if (!isTable(t)) {
                                      cerr << "set on a non-table" << endl;
                                      return;
                                    }
                                    hash_map<long, Cell*>& table = ((Table*)t->car)->table;
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

void set(Cell* t, Cell* k, Cell* val) {
  unsafeSet(t, k, val, true);
}

                                  Cell* unsafeGet(Cell* t, Cell* k) {
                                    if (!isTable(t)) {
                                      cerr << "get on a non-table" << endl;
                                      return nil;
                                    }
                                    hash_map<long, Cell*>& table = ((Table*)t->car)->table;
                                    long key = (long)k;
                                    return table[key];
                                  }

Cell* get(Cell* t, Cell* k) {
  Cell* result = unsafeGet(t, k);
  if (!result) return nil;
  return result;
}



                                  ostream& operator<<(ostream& os, Cell* c);

                                  ostream& operator<<(ostream& os, Table* t) {
                                    os << "{" << endl;
                                    for (hash_map<long, Cell*>::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                      os << (Cell*)p->first << ": " << p->second << endl;
                                    }
                                    return os << "}" << endl;
                                  }

                                  ostream& operator<<(ostream& os, Cell* c) {
                                    if (c == NULL) return os << "NULLNULLNULL";
                                    if (c == nil) return os << "nil";
                                    switch(c->type) {
                                    case NUM:
                                      os << toNum(c); break;
                                    case SYM:
                                    case STRING:
                                      os << toString(c); break;
                                    case TABLE:
                                      os << (Table*)c->car << cdr(c); break;
                                    case CONS:
                                      os << L"<" << car(c) << " . " << cdr(c) << L">"; break;
                                    default:
                                      os << "Can't print type " << c->type << endl << DIE;
                                    }
                                    return os;
                                  }

ostream& operator<<(ostream& os, list<Cell*> l) {
  for (list<Cell*>::iterator p = l.begin(); p != l.end(); ++p)
    os << *p;
  return os << endl;
}



//// construct parse tree out of cells

                                  extern Cell* buildCell(AstNode);

list<Cell*> buildCells(list<AstNode> in) {
  list<Cell*> result;
  if (in.empty()) return result;
  for (list<AstNode>::iterator p = in.begin(); p != in.end(); ++p)
    result.push_back(buildCell(*p));
  return result;
}

Cell* buildCell(AstNode n) {
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
    Cell* newForm = newCell();
    setCar(newForm, buildCell(n.elems.front()));
    setCdr(newForm, buildCell(n.elems.back()));
    return newForm;
  }

  Cell* newForm = NULL;
  Cell* curr = NULL;
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
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"")))));
  check(Cells.empty());
  checkState();
}

void test_build_handles_nil() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"()")))));
  check_eq(Cells.front(), nil);
  checkState();
}

void test_build_handles_nil2() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"nil")))));
  check_eq(Cells.front(), nil);
  checkState();
}

void test_build_handles_number() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"34")))));
  check_eq(Cells.size(), 1);
  check(isNum(Cells.front()));
  check_eq(toNum(Cells.front()), 34);
  check_eq(Cells.front()->nrefs, 1);
  checkState();
}

void test_build_handles_symbol() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"a")))));
  check_eq(Cells.size(), 1);
  check(isSym(Cells.front()));
  check_eq(toString(Cells.front()), L"a");
  check_eq(Cells.front()->nrefs, 1);
  checkState();
}

void test_build_handles_quoted_symbol() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"'a")))));
  check_eq(Cells.size(), 1);
  check(isCons(Cells.front()));
  check(isSym(car(Cells.front())));
  check_eq(toString(car(Cells.front())), L"'");
  check_eq(car(Cells.front())->nrefs, 2);
  check(isSym(car(Cells.front())));
  check_eq(toString(cdr(Cells.front())), L"a");
  check_eq(cdr(Cells.front())->nrefs, 2);
  rmref(Cells.front());
  checkState();
}

void test_build_handles_multiple_atoms() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"34\n35")))));
  check_eq(Cells.size(), 2);
  Cell* c = Cells.front();
  check(isNum(c));
  check_eq(toNum(c), 34);
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

  c = Cells.back();
  check(isNum(c));
  check_eq(toNum(c), 35);
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

  checkState();
}

void test_build_handles_form() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"34 35")))));
  check_eq(Cells.size(), 1);
  Cell* c = Cells.front();
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

  rmref(Cells.front());
  checkState();
}

void test_build_handles_dot() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"34 . 35")))));
  check_eq(Cells.size(), 1);
  Cell* c = Cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 34);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isNum(c));
  check_eq(toNum(c), 35);
  check_eq(c->nrefs, 2);

  rmref(Cells.front());
  checkState();
}

void test_build_handles_nested_form() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 23))")))));
  check_eq(Cells.size(), 1);
  Cell* c = Cells.front();
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
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 33);
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 23);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(Cells.front());
  checkState();
}

void test_build_handles_strings() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 \"abc\" 23))")))));
  check_eq(Cells.size(), 1);
  Cell* c = Cells.front();
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
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 33);
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isString(car(c2)));
    check_eq(toString(car(c2)), L"\"abc\"");
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 23);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(Cells.front());
  checkState();
}

void test_build_handles_syms() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 \"abc\" 3de 23))")))));
  check_eq(Cells.size(), 1);
  Cell* c = Cells.front();
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
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 33);
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isString(car(c2)));
    check_eq(toString(car(c2)), L"\"abc\"");
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isSym(car(c2)));
    check_eq(toString(car(c2)), L"3de");
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 23);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(Cells.front());
  checkState();
}

void test_build_handles_quotes() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"`(34 ,35)")))));
  check_eq(Cells.size(), 1);
  Cell* c = Cells.front();
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
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isSym(car(c2)));
    check_eq(toString(car(c2)), L",");
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 35);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(Cells.front());
  checkState();
}



//// manage symbol bindings

                                  hash_map<long, stack<Cell*> > dynamics;
                                  Cell* lookupDynamicBinding(Cell* sym) {
                                    stack<Cell*> bindings = dynamics[(long)sym];
                                    if (bindings.empty()) return NULL;
                                    return bindings.top();
                                  }

                                  void newDynamicScope(Cell* sym, Cell* val) {
                                    mkref(sym);
                                    mkref(val);
                                    dynamics[(long)sym].push(val);
                                  }

                                  void newDynamicScope(string s, Cell* val) {
                                    newDynamicScope(newSym(s), val);
                                  }

                                  void endDynamicScope(Cell* sym) {
                                    stack<Cell*>& bindings = dynamics[(long)sym];
                                    if (bindings.empty()) {
                                      cerr << "No dynamic binding for " << sym << endl;
                                      return;
                                    }
                                    rmref(sym);
                                    rmref(bindings.top());
                                    bindings.pop();
                                  }

                                  void assignDynamicVar(Cell* sym, Cell* val) {
                                    stack<Cell*>& bindings = dynamics[(long)sym];
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
                                    Cell* currLexicalScope = currLexicalScopes.top();
                                    check_eq(cdr(currLexicalScope), nil);
                                  }

                                  Cell* lookupLexicalBinding(Cell* sym) {
                                    for (Cell* scope = currLexicalScopes.top(); scope != nil; scope = cdr(scope)) {
                                      Cell* result = unsafeGet(scope, sym);
                                      if (result) return result;
                                    }
                                    return NULL;
                                  }

                                  // entering and leaving lexical scopes *assigns the current dynamic*
                                  // binding of the currLexicalScope sym.
                                  // Calling functions will create new dynamic bindings.
                                  void newLexicalScope() {
                                    Cell* newScope = newTable();
                                    dbg << "new lexical scope: " << newScope << endl;
                                    setCdr(newScope, currLexicalScopes.top());
                                    mkref(newScope);
                                    assignDynamicVar(newSym(L"currLexicalScope"), newScope);
                                  }

                                  void endLexicalScope() {
                                    Cell* currScope = currLexicalScopes.top();
                                    if (currScope == nil)
                                      cerr << "No lexical scope to end" << endl << DIE;
                                    dbg << "end lexical scope: " << currScope << endl;
                                    Cell* oldScope = cdr(currScope);
                                    rmref(currScope);
                                    assignDynamicVar(newSym(L"currLexicalScope"), oldScope);
                                  }

                                  void addLexicalBinding(Cell* sym, Cell* val) {
                                    dbg << "creating binding: " << (void*)currLexicalScopes.top() << " " << sym << endl;
                                    if (unsafeGet(currLexicalScopes.top(), sym)) cerr << "Can't rebind within a lexical scope" << endl << DIE;
                                    unsafeSet(currLexicalScopes.top(), sym, val, false);
                                  }

Cell* lookup(Cell* sym) {
  Cell* result = lookupLexicalBinding(sym);
  if (result) return result;
  result = lookupDynamicBinding(sym);
  if (result) return result;
  cerr << "No binding for " << toString(sym) << endl;
  return nil;
}

void test_lookup_returns_dynamic_binding() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newDynamicScope(sym, val);
    check_eq(lookup(sym), val);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 2);
  endDynamicScope(sym);
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  checkState();
}

void test_lookup_returns_lexical_binding() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      check_eq(lookup(sym), val);
      check_eq(sym->nrefs, 2);
      check_eq(val->nrefs, 2);
  endLexicalScope();
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  checkState();
}

void test_lexical_binding_always_overrides_dynamic() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  Cell* dynVal = newNum(35);
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
  checkState();
}

void test_nil_lexical_binding_works() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* dynVal = newNum(35);
  newDynamicScope(sym, dynVal);
    newLexicalScope();
      addLexicalBinding(sym, nil);
        check_eq(lookup(sym), nil);
    endLexicalScope();
  endDynamicScope(sym);
  checkState();
}

void test_lexical_scopes_nest_correctly() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  Cell* val2 = newNum(35);
  check_eq(val->nrefs, 1);
  Cell* dynVal = newNum(36);
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
          check_eq(cdr(currLexicalScopes.top())->nrefs, 2);
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
  checkState();
}

void test_lower_lexical_scopes_are_available() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      check_eq(lookup(sym), val);
      newLexicalScope();
        check_eq(lookup(sym), val);
      endLexicalScope();
  endLexicalScope();
  checkState();
}



//// eval: lookup symbols, respect quotes, rewrite lambda calls

                                  bool isQuoted(Cell* Cell) {
                                    return isCons(Cell) && car(Cell) == newSym(L"'");
                                  }

                                  Cell* unQuote(Cell* Cell) {
                                    if (isQuoted(Cell))
                                      return cdr(Cell);
                                    return Cell;
                                  }

void bindArgs(Cell* params, Cell* args, bool quoted) {
  if (params == nil) return;

  if (isQuoted(params)) {
    bindArgs(cdr(params), args, true);
    return;
  }

  if (isSym(params) || isQuoted(params))
    addLexicalBinding(unQuote(params), args);
  else
    bindArgs(car(params), car(args), quoted);

  bindArgs(cdr(params), cdr(args), quoted);
}

void bindArgs(Cell* params, Cell* args) {
  bindArgs(params, args, false);
}

void test_bindArgs_handles_vararg() {
  Cell* params = buildCells(parse(parenthesize(tokenize(teststream(L"a"))))).front();
  Cell* args = buildCells(parse(parenthesize(tokenize(teststream(L"(1)"))))).front();
  newLexicalScope();
  bindArgs(params, args);
  Cell* result = unsafeGet(currLexicalScopes.top(), newSym(L"a"));
  check_eq(car(result), newNum(1));
  check_eq(cdr(result), nil);
  endLexicalScope();
  rmref(params);
  checkState();
}

                                  Cell* sig(Cell* lambda) {
                                    return car(cdr(lambda));
                                  }

                                  Cell* body(Cell* lambda) {
                                    return cdr(cdr(lambda));
                                  }

                                  Cell* callee_body(Cell* callee) {
                                    return car(cdr(cdr(callee)));
                                  }

                                  Cell* callee_env(Cell* callee) {
                                    return cdr(cdr(cdr(callee)));
                                  }

                                  Cell* call_args(Cell* call) {
                                    return cdr(call);
                                  }

                                  extern Cell* eval(Cell*);

                                  Cell* eval_args(Cell* params, Cell* args) {
                                    if (args == nil) return nil;
                                    if (isQuoted(params)) return args;
                                    setCdr(args, eval_args(cdr(params), cdr(args)));
                                    if (!isCons(params) || !isQuoted(car(params))) {
                                      Cell* result = eval(car(args));
                                      setCar(args, result);
                                      rmref(result);
                                    }
                                    return args;
                                  }

Cell* eval(Cell* expr) {
  if (!expr)
    cerr << "eval: cell should never be NULL" << endl << DIE;

  if (expr == nil)
    return nil;

  if (isSym(expr))
    return mkref(lookup(expr));

  if (isAtom(expr))
    return mkref(expr);

  if (isQuoted(expr))
    return mkref(cdr(expr));

  if (car(expr) == newSym(L"lambda")) {
    // attach current lexical scope
    Cell* ans = newCell();
    setCar(ans, car(expr));
    setCdr(ans, newCell());
    setCar(cdr(ans), sig(expr));
    setCdr(cdr(ans), newCell());
    setCar(cdr(cdr(ans)), body(expr));
    setCdr(cdr(cdr(ans)), currLexicalScopes.top());
    return mkref(ans);
  }

  // expr is a function call
  Cell* lambda = eval(car(expr));
  // eval all its args in the current lexical scope
  Cell* evald_args = eval_args(sig(lambda), call_args(expr));
  // swap in the function's lexical environment
  newDynamicScope(L"currLexicalScope", callee_env(lambda));
  // now bind its params to args in the new environment
  newLexicalScope();
  bindArgs(sig(lambda), evald_args);

  // eval all forms in body; save result of final form
  Cell* result = nil;
  if (isPrimFunc(car(lambda))) {
    result = toPrimFunc(car(lambda))();
  }
  else {
    for (Cell* form = callee_body(lambda); form != nil; form = cdr(form)) {
      rmref(result);
      result = eval(car(form));
    }
  }

  endLexicalScope();
  endDynamicScope(newSym(L"currLexicalScope"));
  rmref(lambda);
  return result;
}

void test_nil_evals_to_itself() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"()")))));
  check_eq(Cells.size(), 1);
  Cell* result = eval(Cells.front());
  check_eq(result, nil);
  rmref(result);
  rmref(Cells.front());
  checkState();
}

void test_num_evals_to_itself() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"34")))));
  check_eq(Cells.size(), 1);
  Cell* result = eval(Cells.front());
  check_eq(result, Cells.front());
  rmref(result);
  rmref(Cells.front());
  checkState();
}

void test_string_evals_to_itself() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"\"ac bd\"")))));
  check_eq(Cells.size(), 1);
  Cell* result = eval(Cells.front());
  check_eq(result, Cells.front());
  rmref(result);
  rmref(Cells.front());
  checkState();
}

void test_eval_handles_quoted_atoms() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"'a '34")))));
  check_eq(Cells.size(), 2);
  Cell* result = eval(Cells.front());
  check_eq(result, newSym(L"a"));
  rmref(result);
  result = eval(Cells.back());
  check_eq(result, newNum(34));
  rmref(result);
  rmref(Cells.front());
  rmref(Cells.back());
  checkState();
}

void test_eval_handles_quoted_lists() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"'(a b)")))));
  Cell* result = eval(Cells.front());
  check_eq(car(result), newSym(L"a"));
  check_eq(car(cdr(result)), newSym(L"b"));
  check_eq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(Cells.front());
  checkState();
}

void test_eval_handles_simple_lambda() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 34)")))));
  check_eq(Cells.size(), 1);
  Cell* lambda = eval(Cells.front());
  check_eq(car(lambda), newSym(L"lambda"));
  check_eq(car(cdr(lambda)), nil);
  check(isCons(car(cdr(cdr(lambda)))));
  check_eq(car(car(cdr(cdr(lambda)))), newNum(34));
  check_eq(cdr(cdr(cdr(lambda))), nil);
  rmref(lambda);
  rmref(Cells.front());
  checkState();
}

void test_eval_handles_closure() {
  list<Cell*> Cells = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 34)")))));
  check_eq(Cells.size(), 1);
  newLexicalScope();
    Cell* newLexicalScope = currLexicalScopes.top();
    check_eq(newLexicalScope->nrefs, 2);
    Cell* result = eval(Cells.front());
    check_eq(newLexicalScope->nrefs, 3);
  endLexicalScope();
  check_eq(newLexicalScope->nrefs, 1);
  check_eq(car(result), newSym(L"lambda"));
  check_eq(car(cdr(result)), nil);
  check_eq(car(car(cdr(cdr(result)))), newNum(34));
  check_eq(cdr(cdr(cdr(result))), newLexicalScope);
  rmref(result);
  check_eq(newLexicalScope->nrefs, 0);
  rmref(Cells.front());
  checkState();
}

void test_eval_handles_lambda_calls() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda () 34))"))))).front();
  Cell* result = eval(call);
  check_eq(result, newNum(34));
  rmref(result);
  rmref(call);
  checkState();
}

void test_eval_expands_syms_in_lambda_bodies() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda () a))"))))).front();
  newDynamicScope(L"a", newNum(34));
  Cell* result = eval(lambda);
  check_eq(result, newNum(34));
  endDynamicScope(newSym(L"a"));
  rmref(result);
  rmref(lambda);
  checkState();
}

void test_eval_handles_assigned_lambda_calls() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 34)"))))).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
    Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f)"))))).front();
    Cell* result = eval(call);
    check_eq(result, newNum(34));
  endDynamicScope(newSym(L"f"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  checkState();
}

void test_eval_expands_lexically_scoped_syms_in_lambda_bodies() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda () a))"))))).front();
  newLexicalScope();
    addLexicalBinding(newSym(L"a"), newNum(34));
    Cell* result = eval(call);
    check_eq(result, newNum(34));
  endLexicalScope();
  rmref(result);
  rmref(call);
  checkState();
}

void test_eval_expands_syms_in_original_lexical_scope() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () a)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"a"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newNum(34));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  checkState();
}

void test_eval_expands_args_in_caller_scope() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda (arg1) arg1)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f a)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newNum(23));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  checkState();
}

void test_eval_doesnt_eval_quoted_params() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda ('arg1) arg1)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f a)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newSym(L"a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  checkState();
}

void test_eval_handles_quoted_param_list() {
  newDynamicScope(L"a", newNum(23));
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda '(arg1) arg1)"))))).front();
  newLexicalScope();
  addLexicalBinding(newSym(L"arg1"), newNum(34));
    Cell* f = eval(lambda);
    newDynamicScope(L"f", f);
  endLexicalScope();
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f a)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newSym(L"a"));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  endDynamicScope(newSym(L"a"));
  checkState();
}

void test_eval_handles_multiple_args() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda (a b) b)"))))).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f 1 2)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  checkState();
}

void test_eval_handles_multiple_body_exprs() {
  Cell* lambda = buildCells(parse(parenthesize(tokenize(teststream(L"(lambda () 1 2)"))))).front();
  Cell* f = eval(lambda);
  newDynamicScope(L"f", f);
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"(f)"))))).front();
  Cell* result = eval(call);
  check_eq(result, newNum(2));
  rmref(result);
  rmref(call);
  rmref(f);
  rmref(lambda);
  endDynamicScope(newSym(L"f"));
  checkState();
}

void test_eval_handles_vararg_param() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda args args) 1)"))))).front();
  Cell* result = eval(call);
  check(isCons(result));
  check_eq(car(result), newNum(1));
  rmref(result);
  rmref(call);
  checkState();
}

void test_eval_evals_args() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda (f) (f)) (lambda () 34))"))))).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 34);
  rmref(result);
  rmref(call);
  checkState();
}

void test_eval_doesnt_leak_body_evals() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda (f) (f) (f)) (lambda () 34))"))))).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 34);
  rmref(result);
  rmref(call);
  checkState();
}

void test_eval_handles_destructured_params() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda ((a b)) b) '(1 2))"))))).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 2);
  rmref(result);
  rmref(call);
  checkState();
}

void test_eval_handles_quoted_destructured_params() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda ('(a b)) b) (1 2))"))))).front();
  Cell* result = eval(call);
  check(isNum(result));
  check_eq(toNum(result), 2);
  rmref(result);
  rmref(call);
  checkState();
}

void test_eval_handles_rest_params() {
  Cell* call = buildCells(parse(parenthesize(tokenize(teststream(L"((lambda (a b . c) c) 1 2 3 4 5)"))))).front();
  Cell* result = eval(call);
  check(isCons(result));
  check(isNum(car(result)));
  check_eq(toNum(car(result)), 3);
  check(isNum(car(cdr(result))));
  check_eq(toNum(car(cdr(result))), 4);
  check_eq(toNum(car(cdr(cdr(result)))), 5);
  check_eq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(call);
  checkState();
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

void setupState() {
  setupNil();
  setupLexicalScope();
}

                                  void resetState() {
                                    freelist = NULL;
                                    for(Cell* curr=currCell; curr >= heapStart; --curr)
                                      curr->init();
                                    currCell = heapStart;
                                    dynamics.clear(); // leaks memory for strings and tables
                                    setupLexicalScope();
                                  }

                                  void clearLiteralTables() {
                                    for (hash_map<long, Cell*>::iterator p = numLiterals.begin(); p != numLiterals.end(); ++p) {
                                      if (p->second->nrefs > 1)
                                        cerr << "forcing unintern: " << (void*)p->second << " " << (long)cdr(p->second) << " " << p->second->nrefs << endl;
                                      while (p->second->nrefs > 0)
                                        rmref(p->second);
                                    }
                                    numLiterals.clear();
                                    for (StringMap<Cell*>::iterator p = stringLiterals.begin(); p != stringLiterals.end(); ++p) {
                                      if (p->first == L"currLexicalScope") continue; // memory leak
                                      if (p->second->nrefs > 1)
                                        cerr << "forcing unintern: " << (void*)p->second << " " << *(string*)car(p->second) << " " << p->second->nrefs << endl;
                                      while (p->second->nrefs > 0)
                                        rmref(p->second);
                                    }
                                    stringLiterals.clear();
                                  }

                                  void checkState() {
                                    clearLiteralTables();
                                    checkUnfreed();
                                    resetState();
                                  }

int main(int argc, ascii* argv[]) {
  setupState();

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
//  no new
//  immutable objects; copy everywhere; no references or pointers except Cell*

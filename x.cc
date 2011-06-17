                                  #include<string>
                                  typedef std::wstring string;
                                  #include<list>
                                  using std::list;
                                  #include<iostream>
                                  typedef std::wistream istream;
                                  typedef std::wostream ostream;
                                  #define cerr std::wcerr
                                  using std::endl;
                                  #include<sstream>
                                  typedef std::wstringstream stringstream;
                                  typedef std::wostringstream ostringstream;

                                  // This must come after system includes.
                                  typedef char ascii;
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
  TOKEN, // First enum value must not be whitespace.
  START_OF_LINE,
  INDENT,
  OUTDENT,
};

struct Token {
  const TokenType type;
  const string token;

  Token(const string x) :token(x), type(TOKEN) {}
  Token(TokenType x) :type(x) {}
  // static convenience methods are more concisely used without new than constructors
  static Token of(TokenType t) {
    Token result(t);
    return result;
  }
  static Token of(string s) {
    Token result(s);
    return result;
  }

  bool operator==(string x) {
    return type == TOKEN && token == x;
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
  if (p.type != TOKEN) os << p.type;
  else os << p.token;
  return os;
}



                                  void skip(istream& in) {
                                    char dummy;
                                    in >> dummy;
                                  }

                                  void skipWhitespace(istream& in) {
                                    while (isspace(in.peek()))
                                      skip(in);
                                  }

                                  bool eof(istream& in) {
                                    in.peek();
                                    return in.eof();
                                  }

                                  int slurpIndentChars(istream& in) {
                                    int count = 0;
                                    char c;
                                    in >> c;
                                    while (isspace(c)) { // BEWARE: tab = 1 space; don't mix
                                      ++count;
                                      if (c == L'\n') count = 0;
                                      in >> c;
                                    }
                                    in.putback(c);
                                    return count;
                                  }

                                  TokenType slurpIndent(istream& in) {
                                    static int indentLevel = 0;
                                    // reset indentLevel if new istream
                                    static istream* prevStream = &in;
                                    if (prevStream != &in) {
                                      prevStream = &in;
                                      indentLevel = 0;
                                    }

                                    int prevIndent = indentLevel;
                                    indentLevel = slurpIndentChars(in);
                                    if (indentLevel > prevIndent) {
                                      return INDENT;
                                    }
                                    else if (indentLevel < prevIndent) {
                                      return OUTDENT;
                                    }
                                    else { // indentLevel == prevIndent
                                      return (TokenType)0;
                                    }
                                  }

// emit whitespace token if found, or a null token
Token processWhitespace(istream& in, TokenType prev) {
next_token:
  if (prev == START_OF_LINE) {
    if (isspace(in.peek())) {
      TokenType type;
      if (in.peek() == L'\n') {
        skip(in);
        goto next_token;
      }
      else {
        return Token::of(slurpIndent(in));
      }
    }
  }
  else {
    if (in.peek() == L'\n') {
      skip(in);
      return Token::of(START_OF_LINE);
    }
    else {
      skipWhitespace(in);
    }
  }

  return Token::of((TokenType)0);
}

                                    stringstream& teststream(string s) {
                                      stringstream& result = *new stringstream(s);
                                      result << std::noskipws;
                                      return result;
                                    }

void test_processWhitespace_generates_newline() {
  check_eq(processWhitespace(teststream(L"\nabc"), TOKEN),
           START_OF_LINE);
  check_eq(processWhitespace(teststream(L"  \nabc"), TOKEN),
           (TokenType)0); // next call will pick up the newline
}

void test_processWhitespace_collapses_continguous_newlines() {
  check_eq(processWhitespace(teststream(L"\nabc"), START_OF_LINE),
           (TokenType)0);
  check_eq(processWhitespace(teststream(L"   \nabc"), START_OF_LINE),
           (TokenType)0);
}

void test_processWhitespace_generates_indent() {
  check_eq(processWhitespace(teststream(L"   abc"), START_OF_LINE), INDENT);
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

Token parseToken(istream& in) {
  static TokenType prev = START_OF_LINE;
  Token ws = processWhitespace(in, prev);
  if (ws != (TokenType)0) {
    prev = ws.type;
    return ws;
  }

  ostringstream out;
  switch (in.peek()) {
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
  prev = TOKEN;
  return Token::of(out.rdbuf()->str());
}

list<Token> tokenize(istream& in) {
  in >> std::noskipws;
  list<Token> result;
  while (!eof(in))
    result.push_back(parseToken(in));
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

void test_tokenize_repeated_newline() {
  list<Token> ast = tokenize(teststream(L"34\n\n\"abc \\\"quote def\""));
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
  check_eq(ast.size(), 21);
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

void test_tokenize_indent_outdent() {
  list<Token> ast = tokenize(teststream(L"abc def ghi\n\n    abc\n  def"));
  check_eq(ast.size(), 9);
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
  check_eq(ast.size(), 6);
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"abc"); ++p;
  check_eq(*p, L"def"); ++p;
  check_eq(*p, L"ghi"); ++p;
  check_eq(*p, START_OF_LINE); ++p;
  check_eq(*p, INDENT); ++p;
  check_eq(*p, L"def"); ++p;
  check(p == ast.end());
}

void test_tokenize_initial_whitespace_lines() {
  list<Token> ast = tokenize(teststream(L"  \nabc def ghi\n\n    \n  def"));
  check_eq(ast.size(), 6);
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

list<Token> parenthesize(list<Token> in) {
  return in;
}

void test_parenthesize_lines_with_initial_parens() {
  list<Token> ast = parenthesize(tokenize(teststream(L"(a b c)")));
  check_eq(ast.size(), 5);
  list<Token>::iterator p = ast.begin();
  check_eq(*p, L"("); ++p;
  check_eq(*p, L"a"); ++p;
  check_eq(*p, L"b"); ++p;
  check_eq(*p, L"c"); ++p;
  check_eq(*p, L")");
}



typedef void (*testfunc)(void);

const testfunc tests[] = {
  #include"test_list"
};

void runTests() {
  for (int i = 0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    (*tests[i])();
  }
  cerr << endl;
  if (numFailures == 0) return;

  cerr << numFailures << " failure";
      if (numFailures > 1) { cerr << "s"; }
      cerr << endl;
}

int main(int argc, ascii* argv[]) {
  if (argc == 1) return 0;
  std::string arg1(argv[1]);
  if (arg1 == "test") {
    runTests();
  }
  return 0;
}

// style:
//  wide unicode strings everywhere
//  no function prototypes
//  indented functions are deemphasized, would be pushed farther down if C permitted
//  no new except in tests
//  minimal STL
//  immutable objects; copy everywhere; no references or pointers

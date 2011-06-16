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
        cerr << "  got " << (X) << endl; \
      } \
      else { cerr << "."; fflush(stderr); }



//// tokenize. newlines and indent matter.

enum TokenType {
  TOKEN,
  START_OF_LINE,
  INDENT,
  OUTDENT,
};

struct Token {
  const TokenType code;
  const string token;

  Token(const string x) :token(x), code(TOKEN) {}
  Token(TokenType x) :code(x) {}

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
    return code == TOKEN && token == x;
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, Token p) {
  if (p.code != TOKEN) os << p.code;
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

    // slurp functions read a token when you're sure to be at it
    void slurpChar(istream& in, ostream& out) {
      char c;
      in >> c; out << c;
    }

    void slurpWord(istream& in, ostream& out) {
      while (!eof(in)) {
        char c;
        in >> c;
        if (isspace(c)) {
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

Token parseToken(istream& in) {
  static TokenType prev = START_OF_LINE;
start_parse:
  skipWhitespace(in);
  ostringstream out;

  switch (in.peek()) {
    case L'\n':
      skip(in);
      if (prev == START_OF_LINE)
        goto start_parse;
      prev = START_OF_LINE;
      return Token::of(START_OF_LINE);

    case L'"':
      slurpString(in, out); break;

    case L'(':
    case L')':
    case L'\'':
      slurpChar(in, out); break;

    case L',':
      slurpChar(in, out);
      if (in.peek() == L'@')
        slurpChar(in, out);
      break;

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

void test_emptyInput() {
  stringstream ss(L"");
  check(tokenize(ss).empty());
}

void test_atom() {
  stringstream ss(L"34");
  check_eq(tokenize(ss).front(), L"34");
}

void test_multiple_atoms() {
  list<Token> ast = tokenize(*new stringstream(L"34 abc"));
  check_eq(ast.size(), 2);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"abc");
}

void test_string_literals() {
  list<Token> ast = tokenize(*new stringstream(L"34 \"abc\""));
  check_eq(ast.size(), 2);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc\"");
}

void test_multiple_lines() {
  list<Token> ast = tokenize(*new stringstream(L"34\n\"abc\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc\"");
}

void test_strings_with_spaces() {
  list<Token> ast = tokenize(*new stringstream(L"34\n\"abc def\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc def\"");
}

void test_strings_with_escapes() {
  list<Token> ast = tokenize(*new stringstream(L"34\n\"abc \\\"quote def\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc \\\"quote def\"");
}

void test_repeated_newlines() {
  list<Token> ast = tokenize(*new stringstream(L"34\n\n\"abc \\\"quote def\""));
  check_eq(ast.size(), 3);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"\"abc \\\"quote def\"");
}

void test_quotes_commas_parens_are_separate_tokens() {
  list<Token> ast = tokenize(*new stringstream(L"(',)"));
  check_eq(ast.size(), 4);
  check_eq(ast.front(), L"(");
  check_eq(ast.back(), L")");
}

void test_splice_operator_is_one_token() {
  list<Token> ast = tokenize(*new stringstream(L"()',@"));
  check_eq(ast.size(), 4);
  check_eq(ast.front(), L"(");
  check_eq(ast.back(), L",@");
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

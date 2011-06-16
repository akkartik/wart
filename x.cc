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

const string dummy_token = L"";
struct Token {
  TokenType code;
  const string& token;

  Token(const string x) :token(x), code(TOKEN) { cerr << "0\n"; cerr << x << endl; cerr.flush(); }
  Token(TokenType x) :code(x), token(dummy_token) { cerr << "1\n"; cerr.flush(); }

  bool operator==(string x) {
    return code == TOKEN && token == x;
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, Token p) {
  if (p.code == TOKEN) os << p.code;
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

    // slurp functions are for reading a kind of token when you know
    Token slurpWord(istream& in) {
      ostringstream out;
      while (!eof(in)) {
        char c;
        in >> c;
        if (isspace(c)) {
          in.putback(c);
          break;
        }

        out << c;
      }

      Token result(out.rdbuf()->str());
      return result;
    }

Token parseToken(istream& in) {
  skipWhitespace(in);

  switch (in.peek()) {
    case L'\n': {
      skip(in);
      Token result(START_OF_LINE);
      return result;
    }
    case L'(':
    case L')':
    case L'"':
    case L'\'':
    default:
      return slurpWord(in);
  }
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

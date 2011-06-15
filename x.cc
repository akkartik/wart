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



//// insert explicit parens

enum ParenTokenType {
  TOKEN,
  START_OF_LINE,
  INDENT,
  OUTDENT,
  STRING,
};

struct ParenToken {
  ParenTokenType code;
  string token;

  ParenToken(){}
  ParenToken(string x) :token(x) {}
  ParenToken(ParenTokenType x) :code(x) {}

  bool operator==(string x) {
    return token == x;
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, ParenToken p) {
  if (p.code == TOKEN) os << p.code;
  else os << p.token;
  return os;
}

    void skipWhitespace(istream& in) {
      char curr;
      while ((curr = in.peek()) == L' '
              || curr == L'\t')
        in >> curr;
    }

    ParenToken nextParenToken(istream& in) {
      ParenToken result;
      string s;
      in >> s;
      result.token = s;
      return result;
    }

list<ParenToken> parseParens(istream& in) {
  static ParenToken prev(START_OF_LINE);
  list<ParenToken> result;

  skipWhitespace(in);
  while (!in.eof())
    result.push_back(nextParenToken(in));
  return result;
}

void test_emptyInput() {
  stringstream ss(L"");
  check(parseParens(ss).empty());
}

void test_atom() {
  stringstream ss(L"34");
  check_eq(parseParens(ss).front(), L"34");
}

void test_multiple_atoms() {
  list<ParenToken> ast = parseParens(*new stringstream(L"34 abc"));
  check_eq(ast.size(), 2);
  check_eq(ast.front(), L"34");
  check_eq(ast.back(), L"abc");
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

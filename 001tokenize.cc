//// tokenize input including newlines and indent

// line contains 1 indent and zero or more regular tokens
struct Token {
  string* token; // pointer only because mac os has a buggy string-copy
  int indentLevel; // all tokens on a line share its indentLevel

  Token(const string x, const int l) :indentLevel(l) {
    token = new string(x);
  }
  ~Token() {
    delete token;
  }
  Token(const Token& rhs) :indentLevel(rhs.indentLevel){
    token = new string(*rhs.token);
  }
  Token& operator=(const Token& rhs) {
    if (this == &rhs) return *this;
    token = new string(*rhs.token);
    indentLevel = rhs.indentLevel;
    return *this;
  }

  static Token of(string s) {
    return of(s, 0);
  }
  static Token of(string s, int indent) {
    Token result(s, indent);
    return result;
  }
  static Token indent(int indent) {
    Token result(L"", indent);
    return result;
  }

  bool isIndent() {
    return *token == L"";
  }

  bool operator==(string x) {
    return *token == x;
  }
  bool operator==(int x) {
    return indentLevel == x;
  }
  bool operator==(Token x) {
    return *token == *x.token && indentLevel == x.indentLevel;
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, Token p) {
  if (*p.token != L"") return os << *p.token;
  os << endl;
  for (int i = 0; i < p.indentLevel; ++i)
    os << L"_";
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
                                    char c = in.peek(); // set eof bit
                                    if (c == -1) return true;
                                    return in.eof();
                                  }

                                  // slurp functions read a token when you're sure to be at it
                                  void slurpChar(istream& in, ostream& out) {
                                    out << (char)in.get();
                                  }

                                  void slurpWord(istream& in, ostream& out) {
                                    static const string quoteChars = L",'`@";
                                    static const string ssyntaxChars = L":~!.&"; // disjoint from quoteChars
                                    char lastc = L'\0';
                                    while (!eof(in)) {
                                      char c = in.get();
                                      if (ssyntaxChars.find(lastc) != string::npos
                                          && quoteChars.find(c) != string::npos)
                                        ; // wait for ssyntax expansion
                                      // keep this list sync'd with the nextToken switch below
                                      else if (isspace(c) || c == ';' || c == L'(' || c == L')' || c == L'"'
                                              || quoteChars.find(c) != string::npos) {
                                        in.putback(c);
                                        break;
                                      }
                                      out << c;
                                      lastc = c;
                                    }
                                  }

                                  void slurpString(istream& in, ostream& out) {
                                    slurpChar(in, out); // initial quote
                                    while (!eof(in)) {
                                      char c = in.get();
                                      out << c;
                                      if (c == L'\\')
                                        slurpChar(in, out); // blindly read next
                                      else if (c == L'"')
                                        break;
                                    }
                                  }

                                  void skipComment(istream& in) {
                                    while (!eof(in)) {
                                      char c = in.get();
                                      if (c == L'\n') {
                                        in.putback(c);
                                        break;
                                      }
                                    }
                                  }

                                  bool endOfInput(istream& in) {
                                    if (eof(in)) return true;
                                    if (!interactive) return false;
                                    bool ans = false;
                                    char c = in.get();
                                    if (c == '\n' && !eof(in) && in.peek() == '\n')
                                      ans = true;
                                    in.putback(c);
                                    return ans;
                                  }

                                  int indent(istream& in) {
                                    int indent = 0;
                                    while (!eof(in)) {
                                      char c = in.get();
                                      if (c == L';') {
                                        skipComment(in);
                                        if (endOfInput(in)) break;
                                      }

                                      else if (!isspace(c)) {
                                        in.putback(c);
                                        break;
                                      }

                                      else if (c == L' ') ++indent;
                                      else if (c == L'\t') indent+=2;
                                      else if (c == L'\n') indent=0;
                                    }
                                    return indent;
                                  }

Token nextToken(istream& in, int& currIndent) {
  if (currIndent == -1) // initial
    return Token::indent(currIndent=indent(in));
  skipWhitespace(in);
  if (in.peek() == L'\n' || in.peek() == L';')
    return Token::indent(currIndent=indent(in));

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

    case L'@':
      slurpChar(in, out); break;

    default:
      slurpWord(in, out); break;
  }
  return Token::of(out.str(), currIndent);
}

list<Token> tokenize(istream& in) {
  in >> std::noskipws;

  list<Token> result;
  int currIndent = -1;
  while (!endOfInput(in))
    result.push_back(nextToken(in, currIndent));

  while(!result.empty() && *result.back().token == L"")
    result.pop_back();
  return result;
}

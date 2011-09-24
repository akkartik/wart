//// tokenize input including newlines and indent

// line contains 1 indent and zero or more regular tokens
struct Token {
  string token;
  int indentLevel; // all tokens on a line share its indentLevel

  Token(const string x, const int l) :token(x), indentLevel(l) {}

  static Token of(string s) {
    Token result(s, 0);
    return result;
  }
  static Token indent(int l) {
    Token result(L"", l);
    return result;
  }

  bool isIndent() {
    return token == L"";
  }

  bool operator==(string x) {
    return token == x;
  }
  bool operator==(int x) {
    return indentLevel == x;
  }
  bool operator==(Token x) {
    return token == x.token && indentLevel == x.indentLevel;
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, Token p) {
  if (p.token != L"") return os << p.token;
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

int countIndent(istream& in) {
  int count = 0;
  char c;
  while (!eof(in)) {
    if (!isspace(in.peek()))
      break;
    in >> c;
    if (c == L'\t')
      count += 2;
    else if (c == L'\n')
      count = 0;
    else
      count++;
  }
  return count;
}



                                  // slurp functions read a token when you're sure to be at it
                                  void slurpChar(istream& in, ostream& out) {
                                    char c;
                                    in >> c; out << c;
                                  }

                                  void slurpWord(istream& in, ostream& out) {
                                    static const string quoteChars = L",'`@";
                                    static const string ssyntaxChars = L":~!.&"; // disjoint from quoteChars
                                    char c, lastc = L'\0';
                                    while (!eof(in)) {
                                      in >> c;
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
                                    char c;
                                    while (!eof(in)) {
                                      in >> c; out << c;
                                      if (c == L'\\')
                                        slurpChar(in, out); // blindly read next
                                      else if (c == L'"')
                                        break;
                                    }
                                  }

                                  void skipComment(istream& in) {
                                    char c;
                                    while (!eof(in)) {
                                      in >> c;
                                      if (c == L'\n') {
                                        in.putback(c);
                                        break;
                                      }
                                    }
                                  }

                                  int indent(istream& in) {
                                    int indent = 0;
                                    char c;
                                    while (!eof(in) &&
                                        (isspace(in.peek()) || in.peek() == L';')) {
                                      in >> c;
                                      switch(c) {
                                      case L' ': ++indent; break;
                                      case L'\t': indent+=2; break;
                                      case L'\n': indent=0; break;
                                      case L';':
                                        skipComment(in);
                                        indent=0;
                                      }
                                    }
                                    return indent;
                                  }

int prevTokenIndentLevel;
Token nextToken(istream& in) {
  skipWhitespace(in);
  if (in.peek() == L'\n' || in.peek() == L';')
    return Token::indent(indent(in));
  skipWhitespace(in);

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

    case L';':
      break;

    default:
      slurpWord(in, out); break;
  }
  return Token::of(out.str());
}

list<Token> tokenize(istream& in) {
  prevTokenIndentLevel = 0;
  in >> std::noskipws;

  list<Token> result;
  result.push_back(Token::indent(indent(in)));
  while (!eof(in)) {
    result.push_back(nextToken(in));
    prevTokenIndentLevel = result.back().indentLevel;
    if (interactive && result.back().isIndent() && in.peek() == L'\n')
      break;
  }

  while(!result.empty() && result.back().token == L"")
    result.pop_back();
  return result;
}

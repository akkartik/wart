//// tokenize input including newlines and indent

// line contains 1 indent and zero or more regular tokens
struct Token {
  string token;
  long indentLevel; // all tokens on a line share its indentLevel

  explicit Token(string s)
    :token(s), indentLevel(0) {}
  explicit Token(long indent)
    :token(""), indentLevel(indent) {}
  Token(const string x, const long l)
    :token(x), indentLevel(l) {}
  Token(const Token& rhs)
    :token(rhs.token), indentLevel(rhs.indentLevel) {}
  Token& operator=(const Token& rhs) {
    if (this == &rhs) return *this;
    token = rhs.token;
    indentLevel = rhs.indentLevel;
    return *this;
  }

  bool isIndent() {
    return token == "";
  }
  bool isParen() {
    return token == "(" || token == ")";
  }
  bool isQuoteOrUnquote() {
    return token == "'" || token == "`"
        || token == "," || token == ",@" || token == "@";
  }

  bool operator==(string x) {
    return token == x;
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
  bool operator==(Token x) {
    return token == x.token && indentLevel == x.indentLevel;
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};



                                  void skip(istream& in) {
                                    char dummy;
                                    in >> dummy;
                                  }

                                  void skipWhitespace(istream& in) {
                                    while (isspace(in.peek()) && in.peek() != '\n')
                                      skip(in);
                                  }

                                  // slurp functions read a token when you're sure to be at it
                                  void slurpChar(istream& in, ostream& out) {
                                    out << (char)in.get();
                                  }

                                  void slurpWord(istream& in, ostream& out) {
                                    static const string quoteChars = ",'`@";
                                    static const string ssyntaxChars = ":~!.&"; // disjoint from quoteChars
                                    char lastc = '\0';
                                    char c;
                                    while (in >> c) {
                                      // keep this list sync'd with the nextToken switch below
                                      if (isspace(c) || c == ';' || c == '(' || c == ')' || c == '"'
                                          || (quoteChars.find(c) != string::npos
                                              // put off quotes inside ssyntax
                                              && ssyntaxChars.find(lastc) == string::npos)) {
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
                                    while (in >> c) {
                                      out << c;
                                      if (c == '\\')
                                        slurpChar(in, out); // blindly read next
                                      else if (c == '"')
                                        break;
                                    }
                                  }

                                  void skipComment(istream& in) {
                                    char c;
                                    while (in >> c) {
                                      if (c == '\n') {
                                        in.putback(c);
                                        break;
                                      }
                                    }
                                  }

                                  void reset(istream& in) {
                                    in.get(); in.get();
                                  }

                                  long indent(istream& in) {
                                    long indent = 0;
                                    char c;
                                    while (in >> c) {
                                      if (c == ';')
                                        skipComment(in);

                                      else if (!isspace(c)) {
                                        in.putback(c);
                                        break;
                                      }

                                      else if (c == ' ') ++indent;
                                      else if (c == '\t') indent+=2;
                                      else if (c == '\n') indent=0;
                                    }
                                    return indent;
                                  }

Token nextToken(istream& in, long& currIndent) {
  if (currIndent == -1) // initial
    return Token(currIndent=indent(in));
  skipWhitespace(in);
  if (in.peek() == '\n' || in.peek() == ';')
    return Token(currIndent=indent(in));

  ostringstream out;
  switch (in.peek()) { // now can't be whitespace
    case '"':
      slurpString(in, out); break;

    case '(':
    case ')':
    case '\'':
    case '`':
    case '@':
      slurpChar(in, out); break;

    case ',':
      slurpChar(in, out);
      if (in.peek() == '@')
        slurpChar(in, out);
      break;

    default:
      slurpWord(in, out); break;
  }

  if (out.str() == ":") return nextToken(in, currIndent);

  return Token(out.str(), currIndent);
}

struct CodeStream {
  istream& fd;
  long currIndent;

  CodeStream(istream& in) :fd(in), currIndent(-1) {
    fd >> std::noskipws;
  }
};

Token nextToken(CodeStream& c) {
  return nextToken(c.fd, c.currIndent);
}

//// tokenize input including newlines and indent

// line contains 1 indent and zero or more regular tokens
struct Token {
  string token;
  long indentLevel;   // all tokens on a line share its indentLevel

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

Token nextToken(CodeStream& c) {
  if (c.currIndent == -1)   // initial
    return Token(c.currIndent=indent(c.fd));
  skipWhitespace(c.fd);
  if (c.fd.peek() == '\n' || c.fd.peek() == ';')
    return Token(c.currIndent=indent(c.fd));

  ostringstream out;
  switch (c.fd.peek()) {  // now can't be whitespace
    case '"':
      slurpString(c.fd, out); break;

    case '(':
    case ')':
    case '\'':
    case '`':
    case '@':
      slurpChar(c.fd, out); break;

    case ',':
      slurpChar(c.fd, out);
      if (c.fd.peek() == '@')
        slurpChar(c.fd, out);
      break;

    default:
      slurpWord(c.fd, out); break;
  }

  if (out.str() == ":") return nextToken(c);

  return Token(out.str(), c.currIndent);
}



// internals

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
  static const string ssyntaxChars = ":~!.&";   // disjoint from quoteChars
  char lastc = '\0';
  char c;
  while (in >> c) {
    // keep this list sync'd with the nextToken switch
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
  slurpChar(in, out);   // initial quote
  char c;
  while (in >> c) {
    out << c;
    if (c == '\\')
      slurpChar(in, out);   // blindly read next
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

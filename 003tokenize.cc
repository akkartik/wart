//// split input into tokens separated by newlines, indent, and the following boundaries:
const string punctuationChars = "()#\"";  // the skeleton of a wart program
const string quoteAndUnquoteChars = ",'`@";   // controlling eval and macros

// Design considered the following:
//  doing the minimum necessary to support macros later
//    so backquote and unquote and splice are supported
//    so infix ops are ignored
//  supporting whitespace sensitivity
//    preserve indent information because later passes can't recreate it
//  avoid modifying strings
//    so parse them here and make them easy for later passes to detect

// line contains 1 indent and zero or more regular tokens
// and a newline token at the end
struct Token {
  string token;
  long indentLevel;   // all tokens on a line share its indentLevel
  bool newline;

  explicit Token(string s)
    :token(s), indentLevel(0), newline(false) {}
  explicit Token(long indent)
    :token(""), indentLevel(indent), newline(false) {}
  Token(const string x, const long l)
    :token(x), indentLevel(l), newline(false) {}
  Token(const Token& rhs)
    :token(rhs.token), indentLevel(rhs.indentLevel), newline(rhs.newline) {}
  Token& operator=(const Token& rhs) {
    if (this == &rhs) return *this;
    token = rhs.token;
    indentLevel = rhs.indentLevel;
    newline = rhs.newline;
    return *this;
  }
  static Token Newline() {
    Token t(0);
    t.newline = true;
    return t;
  }

  bool isIndent() {
    return token == "" && !newline;
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
    return token == x.token && indentLevel == x.indentLevel && newline == x.newline;
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};

Token nextToken(CodeStream& c) {
  static bool atStartOfLine = true;
  if (c.currIndent == -1) atStartOfLine = true;

  if (atStartOfLine) {
    if (c.fd.peek() == '#')
      skipComment(c.fd);
    if (c.fd.peek() == '\n') {
      c.fd.get();
      return Token::Newline();
    }
    Token t = Token(indent(c.fd));
    if (c.fd.peek() == '#')
      skipComment(c.fd);
    if (c.fd.peek() == '\n')
      return nextToken(c);
    atStartOfLine = false;
    c.currIndent=t.indentLevel;
    return t;
  }

  skipWhitespace(c.fd);
  if (c.fd.peek() == '#')
    skipComment(c.fd);
  if (c.fd.peek() == '\n') {
    c.fd.get();
    atStartOfLine = true;
    return Token::Newline();
  }

  ostringstream out;
  if (c.fd.peek() == '"')
    slurpString(c.fd, out);
  else if (find(punctuationChars, c.fd.peek()))
    slurpChar(c.fd, out);
  else if (c.fd.peek() == ',')
    slurpUnquote(c.fd, out);
  else if (find(quoteAndUnquoteChars, c.fd.peek()))
    slurpChar(c.fd, out);
  else
    slurpWord(c.fd, out);

  return Token(out.str(), c.currIndent);
}



// internals

// slurp functions read a token when you're sure to be at it
void slurpChar(istream& in, ostream& out) {
  out << (char)in.get();
}

void slurpWord(istream& in, ostream& out) {
  char c;
  while (in >> c) {
    if (isspace(c) || find(punctuationChars, c) || find(quoteAndUnquoteChars, c)) {
      in.putback(c);
      break;
    }
    out << c;
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

void slurpUnquote(istream& in, ostream& out) {
  slurpChar(in, out);   // comma
  if (in.peek() == '@')
    slurpChar(in, out); // ..and maybe splice
}



long indent(istream& in) {
  long indent = 0;
  char c;
  while (in >> c) {
    if (!isspace(c) || c == '\n') {
      in.putback(c);
      break;
    }

    else if (c == ' ') ++indent;
    else if (c == '\t') indent+=2;
  }
  return indent;
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


void skipWhitespace(istream& in) {
  while (isspace(in.peek()) && in.peek() != '\n')
    in.get();
}



const size_t NOT_FOUND = string::npos;
bool find(string s, char c) {
  return s.find(c) != NOT_FOUND;
}

ostream& operator<<(ostream& os, Token y) {
  if (y.newline) return os << "\\n";
  if (y == "") return os << ":" << y.indentLevel;
  else return os << y.token;
}

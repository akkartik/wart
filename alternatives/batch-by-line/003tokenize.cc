//// split input into tokens separated by indent, and the following boundaries:
const string punctuationChars = "()#\"";  // the skeleton of a wart program
const string quoteAndUnquoteChars = ",'`@";   // controlling eval and macros

// Design considered the following:
//  doing the minimum necessary to support macros later
//    so backquote and unquote and splice are supported
//    so infix ops are ignored
//  supporting whitespace sensitivity
//    preserve indent information because later passes can't recreate it
//    skip indent in empty lines
//  avoid modifying strings
//    so parse them here and make them easy for later passes to detect

// line contains 1 indent and zero or more regular tokens
struct Token {
  string token;
  long indentLevel;   // all tokens on a line share its indentLevel

  explicit Token(string s)
    :token(s), indentLevel(-1) {}
  explicit Token(long indent)
    :token(""), indentLevel(indent) {}
  explicit Token(string s, long indent)
    :token(s), indentLevel(indent) {}

  bool operator==(const string& x) const {
    return token == x;
  }
  bool operator!=(const string& x) const {
    return !(*this == x);
  }
  bool operator==(const Token& x) const {
    return token == x.token && indentLevel == x.indentLevel;
  }
  bool operator!=(const Token& x) const {
    return !(*this == x);
  }
};

list<Token> tokenize(istream& in) {
  in >> std::noskipws;
  list<Token> result;
  bool atStartOfLine = true;
  long currIndent = 0;
  while (!in.eof()) {
    if (atStartOfLine) {
      Token t(currIndent=indent(in));
      if (in.peek() == '#' || in.peek() == '\n') {
        skipComment(in);
        if (in.peek() == '\n') in.get();   // newline
        atStartOfLine = true;
        continue;
      }
      result.push_back(t);
      atStartOfLine = false;
    }
    else {
      skipWhitespace(in);
    }

    if (in.peek() == '#' || in.peek() == '\n') {
      skipComment(in);
      in.get();   // newline
      atStartOfLine = true;
      continue;
    }

    ostringstream out;
    if (in.peek() == '"')
      slurpString(in, out);
    else if (find(punctuationChars, in.peek()))
      slurpChar(in, out);
    else if (in.peek() == ',')
      slurpUnquote(in, out);
    else if (find(quoteAndUnquoteChars, in.peek()))
      slurpChar(in, out);
    else
      slurpWord(in, out);
    result.push_back(Token(out.str(), currIndent));
  }
  return result;
}



//// internals

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
    else if (c == '#') skipComment(in);
    else if (c == '\n') indent=0;
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
  if (y == "") return os << ":" << y.indentLevel;
  else return os << y.token;
}

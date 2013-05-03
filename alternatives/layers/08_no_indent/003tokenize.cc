//// split input into tokens separated by the following boundaries:
const string punctuationChars = "()#\"";  // the skeleton of a wart program
const string quoteAndUnquoteChars = ",'`@";   // controlling eval and macros

// Design considered the following:
//  doing the minimum necessary to support macros later
//    so backquote and unquote and splice are supported
//    so infix ops are ignored
//  avoid modifying strings
//    so parse them here and make them easy for later passes to detect

typedef string Token;

Token nextToken(istream& in) {
  in >> std::noskipws;
  while (in.peek() == '#' || isspace(in.peek())) {
    skipWhitespace(in);
    if (in.peek() == '#')
      skipComment(in);
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

  if (out.str() == ":") return nextToken(in);

  return Token(out.str());
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
  while (isspace(in.peek()))
    in.get();
}



const size_t NOT_FOUND = string::npos;
bool find(string s, char c) {
  return s.find(c) != NOT_FOUND;
}

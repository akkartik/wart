//// split input into tokens separated by the following boundaries:
const string Punctuation_chars = "()#\"";  // the skeleton of a wart program
const string Quote_and_unquote_chars = "'`,@";  // controlling eval and macros

// slurp functions read a token when you're sure to be at it
void slurp_char(istream& in, ostream& out) {
  out << (char)in.get();
}

void slurp_word(istream& in, ostream& out) {
  char c;
  while (in >> c) {
    if (isspace(c) || find(Punctuation_chars, c) || find(Quote_and_unquote_chars, c)) {
      in.putback(c);
      break;
    }
    out << c;
  }
}

void slurp_string(istream& in, ostream& out) {
  slurp_char(in, out);   // initial quote
  char c;
  while (in >> c) {
    out << c;
    if (c == '\\')
      slurp_char(in, out);   // blindly read next
    else if (c == '"')
      break;
  }
}

void skip_comment(istream& in) {
  char c;
  while (in >> c) {
    if (c == '\n') {
      in.putback(c);
      break;
    }
  }
}


void skip_whitespace(istream& in) {
  while (isspace(in.peek()))
    in.get();
}

bool find(string s, char c) {
  return s.find(c) != NOT_FOUND;
}

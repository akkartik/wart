//// split input into tokens separated by the following boundaries:
const string Punctuation_chars = "()#\"";  // the skeleton of a wart program
const string Quote_and_unquote_chars = "'";  // controlling eval

// Design considered the following:
//  avoid modifying strings
//    so parse them here and make them easy for later passes to detect

typedef string token;

token next_token(istream& in) {
  in >> std::noskipws;
  while (in.peek() == '#' || isspace(in.peek())) {
    skip_whitespace(in);
    if (in.peek() == '#')
      skip_comment(in);
  }

  ostringstream out;
  if (in.peek() == '"')
    slurp_string(in, out);
  else if (find(Punctuation_chars, in.peek()))
    slurp_char(in, out);
  else if (find(Quote_and_unquote_chars, in.peek()))
    slurp_char(in, out);
  else
    slurp_word(in, out);

  if (out.str() == ":") {
    trace("tokenize") << "skip comment token";
    return next_token(in);
  }

  trace("tokenize") << out.str();
  return token(out.str());
}



//// internals

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

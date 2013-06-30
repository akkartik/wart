//// split input into tokens separated by newlines, indent, and the following boundaries:
const string Punctuation_chars = "()#\"";  // the skeleton of a wart program
const string Quote_and_unquote_chars = "'`,@";  // controlling eval and macros

// Design considered the following:
//  doing the minimum necessary to support macros later
//    so backquote and unquote and splice are supported
//    so infix ops and implicit gensyms are ignored
//  supporting whitespace sensitivity
//    preserve indent information because later passes can't recreate it
//    skip indent in empty lines
//  avoid modifying strings
//    so parse them here and make them easy for later passes to detect

// line contains 1 indent and zero or more regular tokens
// and a newline token at the end
struct token {
  string value;
  long indent_level;
  bool newline;

  explicit token(string s)
    :value(s), indent_level(-1), newline(false) {}
  explicit token(long indent)
    :value(""), indent_level(indent), newline(false) {}
  static token Newline() {
    token t(0); t.newline = true; return t; }

  bool operator==(const string& x) const {
    return value == x;
  }
  bool operator!=(const string& x) const {
    return !(*this == x);
  }
  bool operator==(const token& x) const {
    return value == x.value && indent_level == x.indent_level && newline == x.newline;
  }
  bool operator!=(const token& x) const {
    return !(*this == x);
  }
};

token next_token(indent_sensitive_stream& in) {
  if (!in.at_start_of_line)
    skip_whitespace(in.fd);

  token maybe_indent("");
  if (in.at_start_of_line)
    maybe_indent = token(indent(in.fd));

  if (in.fd.peek() == '#')
    skip_comment(in.fd);
  if (in.fd.peek() == '\n') {
    in.fd.get();
    in.at_start_of_line = true;
    trace("tokenize") << token::Newline();
    return token::Newline();
  }

  if (in.at_start_of_line) {
    // still here? no comment or newline?
    in.at_start_of_line = false;
    trace("tokenize") << maybe_indent;
    return maybe_indent;
  }

  ostringstream out;
  if (in.fd.peek() == '"')
    slurp_string(in.fd, out);
  else if (find(Punctuation_chars, in.fd.peek()))
    slurp_char(in.fd, out);
  else if (in.fd.peek() == ',')
    slurp_unquote(in.fd, out);
  else if (find(Quote_and_unquote_chars, in.fd.peek())) {
    slurp_char(in.fd, out);
    if (isspace(in.fd.peek()) || in.fd.peek() == ')') {
      if (Interactive)
        RAISE << "You can't put strings in single-quotes\n";
      else {
        cell* context = peek_next_atom(in.fd);
        RAISE << "You can't put strings in single-quotes: '" << context << '\n';
        rmref(context);
      }
    }
  }
  else
    slurp_word(in.fd, out);

  if (out.str() == ":") {
    trace("skip during tokenize") << "comment token";
    return next_token(in);
  }

  trace("tokenize") << out.str();
  return token(out.str());
}

void slurp_unquote(istream& in, ostream& out) {
  slurp_char(in, out);  // comma
  if (in.peek() == '@')
    slurp_char(in, out);
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
  slurp_char(in, out);  // initial quote
  char c;
  while (in >> c) {
    out << c;
    if (c == '\\')
      slurp_char(in, out);  // blindly read next
    else if (c == '"')
      break;
  }
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
  while (isspace(in.peek()) && in.peek() != '\n')
    in.get();
}

cell* peek_next_atom(istream& in) {  // should always undo changes to 'in'
  std::streampos curr = in.tellg();
  indent_sensitive_stream dummy(in);
  cell* result = read(dummy);
  in.seekg(curr);
  in.clear();
  return result;
}



bool find(string s, char c) {
  return s.find(c) != NOT_FOUND;
}

ostream& operator<<(ostream& os, token y) {
  if (y.newline) return os << "\\n";
  if (y == "") return os << ":" << y.indent_level;
  else return os << y.value;
}

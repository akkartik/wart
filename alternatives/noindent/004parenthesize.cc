//// segment the tokens into top-level expressions

list<Token> nextExpr(istream& in) {
  list<Token> result;   // emit tokens here

  long explicitOpenParens = 0;
  for(;;) {
    if (in.eof()) break;
    result.push_back(nextToken(in));
    if (result.back() == "(") ++explicitOpenParens;
    if (result.back() == ")") --explicitOpenParens;
    if (!isQuoteOrUnquote(result.back()) && explicitOpenParens == 0) break;
  }
  if (result.empty()) result.push_back(eof());
  return result;
}



// internals

bool isIndent(const Token& t) {
  return t.token == "" && !t.newline;
}

bool isParen(const Token& t) {
  return t == "(" || t == ")";
}

bool isQuoteOrUnquote(const Token& t) {
  return t == "'" || t == "`"
      || t == "," || t == ",@" || t == "@";
}

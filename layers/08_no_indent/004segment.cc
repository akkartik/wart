//// segment the tokens into top-level expressions

list<token> next_expr(istream& in) {
  list<token> result;   // emit tokens here

  long explicit_open_parens = 0;
  for(;;) {
    if (in.eof()) break;
    result.push_back(next_token(in));
    if (result.back() == "(") ++explicit_open_parens;
    if (result.back() == ")") --explicit_open_parens;
    if (!is_quote_or_unquote(result.back()) && explicit_open_parens == 0) break;
  }
  if (result.empty()) result.push_back("");
  return result;
}



//// internals

bool is_quote_or_unquote(const token& t) {
  return t == "'" || t == "`" || t == "," || t == ",@";
}

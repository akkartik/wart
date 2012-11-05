//// construct parse tree out of a list of tokens

struct AstNode {
  Token atom;
  list<AstNode> elems;

  explicit AstNode(Token t) :atom(t) {}
  explicit AstNode(list<AstNode> l) :atom(eof()), elems(l) {}

  bool operator==(const Token& x) const {
    return elems.empty() && atom == x.token;  // whitespace should be gone by now
  }
  bool operator==(const string& x) const {
    return elems.empty() && atom == x;
  }
  bool operator!=(const Token& x) const {
    return !(*this == x);
  }
  bool operator!=(const string& x) const {
    return !(*this == x);
  }
};

AstNode nextAstNode(IndentSensitiveStream& in) {
  list<Token> bufferedTokens = nextExpr(in);
  return nextAstNode(bufferedTokens);
}

AstNode nextAstNode(list<Token>& buffer) {
  list<AstNode> subform;
  if (buffer.empty()) return AstNode(subform);

  subform.push_back(AstNode(nextToken(buffer)));
  while (!buffer.empty() && isQuoteOrUnquote(subform.back().atom))
    subform.push_back(AstNode(nextToken(buffer)));

  if (subform.back() == "(") {
    while (!buffer.empty() && subform.back() != ")")
      subform.push_back(nextAstNode(buffer));
    if (subform.back() != ")") RAISE << "Unbalanced (" << endl << DIE;
  }

  if (subform.size() == 1)
    return AstNode(subform.back());
  return AstNode(subform);
}



// internals

Token nextToken(list<Token>& buffer) {
  Token result = buffer.front(); buffer.pop_front();
  return result;
}

Token eof() {
  return Token(0);
}

ostream& operator<<(ostream& os, AstNode x) {
  if (x.elems.empty()) return os << x.atom;
  bool skipNextSpace = true;
  for (list<AstNode>::iterator p = x.elems.begin(); p != x.elems.end(); ++p) {
    if (*p != ")" && !skipNextSpace)
      os << " ";
    os << *p;
    skipNextSpace = (*p == "(" || isQuoteOrUnquote(*p));
  }
  return os;
}

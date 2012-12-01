//// construct parse tree out of tokens

// To disable whitespace-sensitivity, replace calls to nextParenInsertedToken
// with nextNonWhitespaceToken.

// Currently ,@(list x) creates a flat list: ,@ ( list x )
// Equally valid to have it create a 2 level list: ,@ followed by ( list x )
// Would require changing build phase appropriately.

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

AstNode nextAstNode(IndentSensitiveStream& c) {
  Token curr = nextParenInsertedToken(c);
  if (curr != "(" && !isQuoteOrUnquote(curr))
    return AstNode(curr);

  list<AstNode> subform;
  subform.push_back(AstNode(curr));
  while (!eof(subform.back()) && isQuoteOrUnquote(subform.back().atom))
    subform.push_back(AstNode(nextParenInsertedToken(c)));

  if (subform.back() == "(") {
    while (!eof(subform.back()) && subform.back().atom != ")")
      subform.push_back(nextAstNode(c));
    if (eof(subform.back())) RAISE << "Unbalanced (" << endl << DIE;
  }

  return AstNode(subform);
}



// internals

Token nextNonWhitespaceToken(IndentSensitiveStream& c) {
  while (!c.eof()) {
    Token curr = nextToken(c);
    if (!isIndent(curr)) return curr;
  }
  return eof();
}

list<Token> bufferedTokens;

Token nextParenInsertedToken(IndentSensitiveStream& c) {
  if (bufferedTokens.empty()) bufferedTokens = nextExpr(c);
  if (bufferedTokens.empty()) return eof();
  Token result = bufferedTokens.front();
  bufferedTokens.pop_front();
  return result;
}

Token eof() {
  return Token(0);
}

bool isList(const AstNode& n) {
  return !n.elems.empty();
}

bool isAtom(const AstNode& n) {
  return n.elems.empty();
}

bool isQuoteOrUnquote(const AstNode& n) {
  return isAtom(n) && isQuoteOrUnquote(n.atom);
}

bool eof(AstNode n) {
  return n.atom.token == "" && n.elems.empty();
}

ostream& operator<<(ostream& os, AstNode x) {
  if (x.elems.empty()) return os << x.atom;
  bool prevWasOpen = true;
  for (list<AstNode>::iterator p = x.elems.begin(); p != x.elems.end(); ++p) {
    if (!(*p == ")" || prevWasOpen)) os << " ";
    prevWasOpen = (*p == "(" || *p == "'" || *p == "," || *p == ",@" || *p == "@");
    os << *p;
  }
  return os;
}

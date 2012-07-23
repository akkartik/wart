//// construct parse tree out of tokens

struct AstNode {
  Token atom;
  list<AstNode> elems;

  AstNode(Token t) :atom(t) {}
  AstNode(list<AstNode> l) :atom(Token::indent(0)), elems(l) {}
  static AstNode of(Token t) {
    AstNode result(t);
    return result;
  }
  static AstNode of(list<AstNode> l) {
    AstNode result(l);
    return result;
  }

  bool isAtom() {
    return elems.empty();
  }
  bool isList() {
    return !elems.empty();
  }
  bool isNil() {
    return atom == "nil"
        || (elems.size() == 2 && elems.front() == "(" && elems.back() == ")");
  }

  bool operator==(Token x) {
    return elems.empty() && atom == x.token; // whitespace should be gone by now.
  }
  bool operator==(string x) {
    return elems.empty() && atom == x;
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, Token y) {
  if (y == "") return os << ":" << y.indentLevel;
  else return os << y.token;
}

ostream& operator<<(ostream& os, AstNode x) {
  if (x.elems.empty()) return os << x.atom;
  bool prevWasOpen = true;
  for (list<AstNode>::iterator p = x.elems.begin(); p != x.elems.end(); ++p) {
    if (!(*p == ")" || prevWasOpen)) os << " ";
    prevWasOpen = (*p == "(" || *p == "'" || *p == "," || *p == ",@" || *p == "@");
    os << *p;
  }
  return os << endl;
}

// To disable whitespace-sensitivity, replace calls to nextParenInsertedToken
// with nextNonWhitespaceToken.
Token nextNonWhitespaceToken(CodeStream& c) {
  while (!c.fd.eof()) {
    Token curr = nextToken(c);
    if (!curr.isIndent()) return curr;
  }
  return Token::indent(0); // eof
}

Token nextParenInsertedToken(CodeStream& c) {
  static list<Token> currExpr;
  if (currExpr.empty()) currExpr = nextExpr(c);
  if (currExpr.empty()) return Token::indent(0); // eof
  Token result = currExpr.front();
  currExpr.pop_front();
  return result;
}

bool eof(AstNode n) {
  return n.atom.token == "" && n.elems.empty();
}

AstNode nextAstNode(CodeStream& c) {
  Token curr = nextParenInsertedToken(c);
  if (curr != "(" && !curr.isQuoteOrUnquote())
    return curr;

  list<AstNode> subform;
  subform.push_back(curr);
  while (!eof(subform.back()) && subform.back().atom.isQuoteOrUnquote())
    subform.push_back(nextParenInsertedToken(c));

  if (subform.back() == "(") {
    while (!eof(subform.back()) && subform.back().atom != ")")
      subform.push_back(nextAstNode(c));
    if (eof(subform.back())) RAISE << "Unbalanced (" << endl << DIE;
  }

  return AstNode::of(subform);
}

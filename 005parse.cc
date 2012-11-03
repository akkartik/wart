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
    return elems.empty() && atom == x.token;  // whitespace should be gone by now
  }
  bool operator==(string x) {
    return elems.empty() && atom == x;
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
  bool operator!=(string x) {
    return !(*this == x);
  }
};

AstNode nextAstNode(istream& in) {
  CodeStream cs(in);
  return nextAstNode(cs);
}

AstNode nextAstNode(CodeStream& cs) {
  Token curr = nextParenInsertedToken(cs);
  if (curr != "(" && !curr.isQuoteOrUnquote())
    return AstNode(curr);

  list<AstNode> subform;
  subform.push_back(AstNode(curr));
  while (!eof(subform.back()) && subform.back().atom.isQuoteOrUnquote())
    subform.push_back(AstNode(nextParenInsertedToken(cs)));

  if (subform.back() == "(") {
    while (!eof(subform.back()) && subform.back().atom != ")")
      subform.push_back(nextAstNode(cs));
    if (eof(subform.back())) RAISE << "Unbalanced (" << endl << DIE;
  }

  return AstNode(subform);
}



// internals

Token nextNonWhitespaceToken(CodeStream& cs) {
  while (!cs.eof()) {
    Token curr = nextToken(cs);
    if (!curr.isIndent()) return curr;
  }
  return eof();
}

Token nextParenInsertedToken(CodeStream& cs) {
  if (cs.bufferedTokens.empty()) cs.bufferedTokens = nextExpr(cs);
  if (cs.bufferedTokens.empty()) return eof();
  Token result = cs.bufferedTokens.front();
  cs.bufferedTokens.pop_front();
  return result;
}

Token eof() {
  return Token(0);
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

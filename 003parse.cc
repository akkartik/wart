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

list<Token>::iterator parseNext(list<Token>::iterator curr, list<Token>::iterator end, list<AstNode>& out) {
  if (curr == end) return curr;

  if (*curr == ")") RAISE << "Unbalanced )" << endl << DIE;

  if (*curr != "(" && !curr->isQuoteOrUnquote()) {
    out.push_back(AstNode::of(*curr));
    return ++curr;
  }

  list<AstNode> subform;
  while (curr != end && curr->isQuoteOrUnquote()) {
    subform.push_back(*curr);
    ++curr;
  }

  if (*curr == "(") {
    subform.push_back(*curr);
    ++curr;
    while (curr != end && *curr != ")")
      curr = parseNext(curr, end, subform);
    if (curr == end) RAISE << "Unbalanced (" << endl << DIE;
    subform.push_back(*curr);
    ++curr;
  }
  else {
    subform.push_back(*curr);
    ++curr;
  }

  out.push_back(subform);
  return curr;
}

AstNode nextAstNode(CodeStream& c) {
  list<Token> tokens = nextExpr(c);
  list<AstNode> result;
  parseNext(tokens.begin(), tokens.end(), result);
  if (result.size() > 1) RAISE << "parse error\n" << DIE;
  if (result.empty()) return AstNode::of(Token::of("nil"));
  return result.front();
}

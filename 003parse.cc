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
    return atom == L"nil"
        || (elems.size() == 2 && elems.front() == L"(" && elems.back() == L")");
  }

  bool operator==(Token x) {
    return elems.empty() && atom == *x.token; // whitespace should be gone by now.
  }
  bool operator==(string x) {
    return elems.empty() && atom == x;
  }
  bool operator!=(Token x) {
    return !(*this == x);
  }
};

ostream& operator<<(ostream& os, AstNode x) {
  if (x.elems.empty()) return os << x.atom;
  bool prevWasOpen = true;
  for (list<AstNode>::iterator p = x.elems.begin(); p != x.elems.end(); ++p) {
    if (!(*p == L")" || prevWasOpen)) os << " ";
    prevWasOpen = (*p == L"(" || *p == L"'" || *p == L"," || *p == L",@" || *p == L"@");
    os << *p;
  }
  return os << endl;
}

list<Token>::iterator parseNext(list<Token>::iterator curr, list<Token>::iterator end, list<AstNode>& out) {
  if (curr == end) return curr;

  if (*curr == L")") err << "Unbalanced )" << endl << DIE;

  if (*curr != L"(" && !isQuoteOrUnquote(*curr)) {
    out.push_back(AstNode::of(*curr));
    return ++curr;
  }

  list<AstNode> subform;
  while (curr != end && isQuoteOrUnquote(*curr)) {
    subform.push_back(*curr);
    ++curr;
  }

  if (*curr == L"(") {
    subform.push_back(*curr);
    ++curr;
    while (curr != end && *curr != L")")
      curr = parseNext(curr, end, subform);
    if (curr == end) err << "Unbalanced (" << endl << DIE;
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

list<AstNode> parse(istream& in) {
  CodeStream c(in);
  list<AstNode> result;
  while (!eof(in)) {
    list<Token> tokens = nextExpr(c);
    parseNext(tokens.begin(), tokens.end(), result);
  }
  return result;
}

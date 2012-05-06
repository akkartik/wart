//// construct parse tree out of cells

                                  bool isQuoteOrUnquote(AstNode n) {
                                    return n == "'" || n == "`" || n == "," || n == ",@" || n == "@";
                                  }

Cell* buildCell(AstNode n) {
  if (n.isNil())
    return nil;
  if (n.isList() && n.elems.front() == ")") {
    if (n.elems.size() > 1) RAISE << "Syntax error: ) not at end of expr" << endl << DIE;
    return nil;
  }

  if (n.isAtom()) {
    char* end;
    int v = strtol(n.atom.token.c_str(), &end, 0);
    if (*end == '\0')
      return newNum(v);

    if (n.atom.token.c_str()[0] == '"')
      return newString(n.atom.token.substr(1, n.atom.token.length()-2));

    return newSym(n.atom.token);
  }

  list<AstNode>::iterator first = n.elems.begin();
  if (*first == "(") {
    n.elems.pop_front();
    return buildCell(n);
  }

  Cell* newForm = newCell();
  setCar(newForm, buildCell(n.elems.front()));

  list<AstNode>::iterator next = first; ++next;
  if (*next == ".") {
    if (n.elems.size() == 2) RAISE << "Syntax error: . can't terminate expr" << endl << DIE;
    setCdr(newForm, buildCell(*++next)); // dotted pair
  }
  else if (isQuoteOrUnquote(*first) && n.elems.size() == 2) {
    setCdr(newForm, buildCell(*next)); // dotted pair
  }
  else {
    n.elems.pop_front();
    if (n.elems.empty())
      RAISE << "Error in parsing " << n << endl << DIE;
    setCdr(newForm, buildCell(n));
  }

  return newForm;
}

Cell* nextRawCell(CodeStream c) {
  c.fd.peek();
  if (c.fd.eof()) return nil;
  return buildCell(nextAstNode(c));
}

//// construct parse tree out of cells

extern Cell* buildCell(AstNode);

list<Cell*> buildCells(list<AstNode> in) {
  list<Cell*> result;
  if (in.empty()) return result;
  for (list<AstNode>::iterator p = in.begin(); p != in.end(); ++p)
    result.push_back(buildCell(*p));
  return result;
}

                                  bool isQuoteOrUnquote(AstNode n) {
                                    return n == L"'" || n == L"`" || n == L"," || n == L",@" || n == L"@";
                                  }

Cell* buildCell(AstNode n) {
  if (n.isNil())
    return nil;
  if (n.elems.front() == L")") {
    if (n.elems.size() > 1) err << "Syntax error: ) not at end of expr" << endl << DIE;
    return nil;
  }

  if (n.isAtom()) {
    char* end;
    long v = wcstol(n.atom.token.c_str(), &end, 0);
    if (*end == L'\0')
      return newNum(v);

    if (n.atom.token.c_str()[0] == L'"')
      return newString(n.atom.token.substr(1, n.atom.token.length()-2));

    return newSym(n.atom.token);
  }

  list<AstNode>::iterator first = n.elems.begin();
  if (*first == L"(") {
    n.elems.pop_front();
    return buildCell(n);
  }

  Cell* newForm = newCell();
  setCar(newForm, buildCell(n.elems.front()));

  list<AstNode>::iterator next = first; ++next;
  if (*next == L".") {
    if (n.elems.size() == 2) err << "Syntax error: . can't terminate expr" << endl << DIE;
    setCdr(newForm, buildCell(*++next)); // dotted pair
  }
  else if (isQuoteOrUnquote(*first) && n.elems.size() == 2) {
    setCdr(newForm, buildCell(*next)); // dotted pair
  }
  else {
    n.elems.pop_front();
    if (n.elems.empty())
      err << "Error in parsing " << n << endl << DIE;
    setCdr(newForm, buildCell(n));
  }

  return newForm;
}

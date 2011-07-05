//// construct parse tree out of cells

extern Cell* buildCell(AstNode);

list<Cell*> buildCells(list<AstNode> in) {
  list<Cell*> result;
  if (in.empty()) return result;
  for (list<AstNode>::iterator p = in.begin(); p != in.end(); ++p)
    result.push_back(buildCell(*p));
  return result;
}

Cell* buildCell(AstNode n) {
  if (n.isNil())
    return nil;

  if (n.isAtom()) {
    if (n.atom.token == L")")
      cerr << "Syntax error: unbalanced )" << endl << DIE;

    char* end;
    long v = wcstol(n.atom.token.c_str(), &end, 0);
    if (*end == L'\0')
      return newNum(v);

    if (n.atom.token.c_str()[0] == L'"')
      return newString(n.atom.token.substr(1, n.atom.token.length()-2));
    return newSym(n.atom.token);
  }

  if (n.elems.size() == 2
      && (n.elems.front() == L"'" || n.elems.front() == L"`" || n.elems.front() == L"," || n.elems.front() == L",@")
      && n.elems.back().isAtom()) {
    Cell* newForm = newCell();
    setCar(newForm, buildCell(n.elems.front()));
    setCdr(newForm, buildCell(n.elems.back()));
    return newForm;
  }

  Cell* newForm = NULL;
  Cell* curr = NULL;
  for (list<AstNode>::iterator q = n.elems.begin(); q != n.elems.end(); ++q) {
    if (q->atom == L"(")
      continue;
    if (q->atom == L")")
      break;

    if (q->atom == L".") {
      ++q;
      if (!curr) cerr << "Syntax error: dot at start of expression" << endl << DIE;
      setCdr(curr, buildCell(*q));
      break;
    }

    if (!curr) {
      newForm = curr = newCell();
    }
    else {
      setCdr(curr, newCell());
      curr = cdr(curr);
    }

    setCar(curr, buildCell(*q));
  }

  return newForm;
}

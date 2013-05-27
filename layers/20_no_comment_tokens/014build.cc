//// construct parse tree out of cells

Cell* nextCell(istream& in) {
  return buildCell(nextAstNode(in));
}

Cell* buildCell(AstNode n) {
  incTraceForRestOfScope("cell");
  if (n == "") return nil;  // void

  if (isNil(n)) {
    trace("cell") << "nil";
    return nil;
  }
  if (isList(n) && n.elems.front() == ")") {
    if (n.elems.size() > 1) RAISE << "Syntax error: ) not at end of expr" << endl << DIE;
    return nil;
  }

  if (isAtom(n)) {
    errno = 0;
    char* end;
    long v = strtol(n.atom.c_str(), &end, 0);
    if (*end == '\0' && errno == 0) {
      trace("cell") << "num: " << v;
      return newNum(v);
    }

    if (errno == ERANGE || errno == EOVERFLOW)
      RAISE << "dropping precision for bignum " << n.atom << endl;

    float f = strtof(n.atom.c_str(), &end);
    if (*end == '\0') {
      trace("cell") << "float: " << f;
      if (n.atom.substr(0, 2) == "-.")
        RAISE << "assuming '" << n.atom << "' is a float; to remove this warning say '-0" << n.atom.substr(1) << "'.\n"
            << "If you mean to negate an int, skip the ssyntax: '-" << n.atom.substr(2) << "'.\n";
      return newNum(f);
    }

    if (n.atom.c_str()[0] == '"') {
      trace("cell") << "string: " << n.atom;
      return newString(n.atom.substr(1, n.atom.length()-2));
    }

    trace("cell") << "sym: " << n.atom;
    return newSym(n.atom);
  }

  list<AstNode>::iterator first = n.elems.begin();
  if (*first == "(") {
    n.elems.pop_front();
    Cell* result = buildCell(n);
    trace("cell") << result;
    return result;
  }

  Cell* newForm = newCell();
  setCar(newForm, buildCell(n.elems.front()));

  list<AstNode>::iterator next = first; ++next;
  if (*next == "...") {
    if (next != --n.elems.end())
      setCdr(newForm, buildCell(*++next));  // dotted pair
    else
      setCdr(newForm, buildCell(*next));
  }
  else if (isQuoteOrUnquote(*first) && n.elems.size() == 2) {
    setCdr(newForm, buildCell(*next));  // dotted pair
  }
  else {
    n.elems.pop_front();
    if (n.elems.empty())
      RAISE << "Error in parsing " << n << endl << DIE;
    setCdr(newForm, buildCell(n));
  }

  trace("cell") << newForm;
  return newForm;
}

bool isNil(const AstNode& n) {
  return n.atom == "nil"
      || (n.elems.size() == 2 && n.elems.front() == "(" && n.elems.back() == ")");
}

//// transform infix expressions into prefix
const string extraSymChars = "$?!_";

AstNode transformInfix(AstNode n) {
  if (infixOpCalledWithoutArgs(n))
    return *++n.elems.begin();
  return n;
}



bool isInfixOp(string name) {
  string::iterator p = name.begin();
  if (*p != '$' && !isInfixChar(*p))
    return false;
  for (++p; p != name.end(); ++p)
    if (!isInfixChar(*p))
      return false;
  return true;
}

bool containsInfixChar(string name) {
  for (string::iterator p = name.begin(); p != name.end(); ++p) {
    if (isspace(*p) || find(punctuationChars, *p))
      RAISE << "checked for infix chars in non-atom " << name << endl;

    if (p == name.begin() && *p == '-')
      continue;

    if (isInfixChar(*p)) return true;
  }
  return false;
}

bool isInfixChar(char c) {
  return !find(quoteAndUnquoteChars, c) && !find(ssyntaxChars, c)
      && !isalnum(c) && !find(extraSymChars, c);
}

bool infixOpCalledWithoutArgs(AstNode n) {
  if (!n.isList() || n.elems.size() != 3) return false;
  list<AstNode>::iterator p = n.elems.begin();
  if (*p != Token("(")) return false;
  ++p;
  if (!p->isAtom() || !isInfixOp(p->atom.token))
    return false;
  ++p;
  return *p == Token(")");
}

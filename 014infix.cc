//// transform infix expressions into prefix
const string extraSymChars = "$?!_";

AstNode transformInfix(AstNode n) {
  if (infixOpCalledWithoutArgs(n))
    return *++n.elems.begin();

  // TODO: split symbols and set their spacesBefore
  if (n.isAtom())
    return n;

  if (n.elems.front() != Token("("))
    return transformAll(n);

  // parse with an op stack
  stack<AstNode> ops, args;
  auto p = ++n.elems.begin();  // skip parens
  if (p->isAtom() && isInfixOp(p->atom.token))
    return n;   // disable infix when infix op is used in prefix
  for (; p != --n.elems.end(); ++p) {  // skip parens
    if (*p == Token("(") || *p == Token(")"))
      RAISE << "Unexpected paren inside AstNode " << n << endl;
    if (p->isAtom() && isInfixOp(p->atom.token))
      ops.push(*p);
    else
      args.push(transformInfix(*p));
  }

  if (args.empty()) return transformAll(n); // TODO: handle curried infix here?
  if (ops.empty()) return transformAll(n);

  while (!ops.empty()) {
    list<AstNode> tmp;
    tmp.push_front(args.top());     args.pop();
    tmp.push_front(args.top());     args.pop();
    tmp.push_front(ops.top());      ops.pop();

    tmp.push_front(AstNode(Token("(")));
    tmp.push_back(AstNode(Token(")")));
    args.push(AstNode(tmp));
  }

  if (args.size() != 1)
    RAISE << "Error in parsing infix: " << n << endl << DIE;

  return args.top();
}

AstNode transformAll(AstNode n) {
  if (n.isAtom()) return n;
  list<AstNode> results;
  for (auto p = n.elems.begin(); p != n.elems.end(); ++p)
    results.push_back(transformInfix(*p));
  n.elems.swap(results);
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
  return !find(punctuationChars, c)
      && !find(quoteAndUnquoteChars, c) && !find(ssyntaxChars, c)
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

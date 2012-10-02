void test_infix_passes_atoms() {
  CodeStream cs(stream("abc"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_atoms2() {
  CodeStream cs(stream("-3.2"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_strings() {
  CodeStream cs(stream("\"a\""));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_dollar_vars() {
  CodeStream cs(stream("$a"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_handles_op_without_args() {
  CodeStream cs(stream("(+)"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isAtom());
  checkEq(n.atom, Token("+"));
}

void test_infix_handles_dollar_op_without_args() {
  CodeStream cs(stream("($+)"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isAtom());
  checkEq(n.atom, Token("$+"));
}

void test_infix_handles_quoting() {
  CodeStream cs(stream("',a"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("'"));    ++p;
  checkEq(*p, Token(","));    ++p;
  checkEq(*p, Token("a"));    ++p;
  check(p == n.elems.end());
}

void test_infix_handles_simple_lists() {
  CodeStream cs(stream("a + b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("("));    ++p;
  checkEq(*p, Token("+"));    ++p;
  checkEq(*p, Token("a"));    ++p;
  checkEq(*p, Token("b"));    ++p;
  checkEq(*p, Token(")"));    ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_prefix() {
  CodeStream cs(stream("+ a b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("("));    ++p;
  checkEq(*p, Token("+"));    ++p;
  checkEq(*p, Token("a"));    ++p;
  checkEq(*p, Token("b"));    ++p;
  checkEq(*p, Token(")"));    ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_unary_prefix() {
  CodeStream cs(stream("+ a"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("("));    ++p;
  checkEq(*p, Token("+"));    ++p;
  checkEq(*p, Token("a"));    ++p;
  checkEq(*p, Token(")"));    ++p;
  check(p == n.elems.end());
}

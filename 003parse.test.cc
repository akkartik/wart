void test_parse_handles_empty_stream() {
  CodeStream c(stream(""));
  checkEq(nextAstNode(c), eof());
  check(c.fd.eof());
}

void test_parse_handles_trailing_comment() {
  CodeStream c(stream("34 ; abc"));
  checkEq(nextAstNode(c), Token("34"));
  checkEq(nextAstNode(c), eof());
  check(c.fd.eof());
}

void test_parse_handles_atom() {
  CodeStream c(stream("34"));
  checkEq(nextAstNode(c), Token("34"));
  check(c.fd.eof());
}

void test_parse_handles_atoms() {
  CodeStream c(stream("34\n\"a b c\"\n3.4"));
  checkEq(nextAstNode(c), Token("34"));
  checkEq(nextAstNode(c), Token("\"a b c\""));
  checkEq(nextAstNode(c), Token("3.4"));
  check(c.fd.eof());
}

void test_parse_handles_forms() {
  CodeStream c(stream("34 \"a b c\""));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  checkEq(*p, Token("\"a b c\"")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_nested_forms() {
  CodeStream c(stream("34 (2 3) \"a b c\""));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("(")); ++q;
    checkEq(*q, Token("2")); ++q;
    checkEq(*q, Token("3")); ++q;
    checkEq(*q, Token(")")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token("\"a b c\"")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_nested_forms_with_comments() {
  CodeStream c(stream("(a b (c d ;\n))"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("(")); ++q;
    checkEq(*q, Token("c")); ++q;
    checkEq(*q, Token("d")); ++q;
    checkEq(*q, Token(")")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_quotes() {
  CodeStream c(stream("34 `(2 ,b) ',35 ,',36 ,'a"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("`")); ++q;
    checkEq(*q, Token("(")); ++q;
    checkEq(*q, Token("2")); ++q;
    check(q->isList());
      list<AstNode> ast3 = q->elems; ++q;
      list<AstNode>::iterator r = ast3.begin();
      checkEq(*r, Token(",")); ++r;
      checkEq(*r, Token("b")); ++r;
      check(r == ast3.end());
    checkEq(*q, Token(")")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token("'")); ++q;
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("35")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("'")); ++q;
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("36")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("'")); ++q;
    checkEq(*q, Token("a")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_splice_operators() {
  CodeStream c(stream("`(2 ,@b @,c)"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("`")); ++p;
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("2")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token(",@")); ++q;
    checkEq(*q, Token("b")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token("@")); ++q;
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("c")); ++q;
    check(q == ast2.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_indented_toplevel_forms() {
  CodeStream c(stream("a\n  a b c\n    d"));
  AstNode n = nextAstNode(c);
  check(n.isAtom());
  checkEq(n.atom.token, "a");

  n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  check(p->isAtom());
  checkEq(p->atom.token, "("); ++p;
  checkEq(p->atom.token, "a"); ++p;
  checkEq(p->atom.token, "b"); ++p;
  checkEq(p->atom.token, "c"); ++p;
  checkEq(p->atom.token, "d"); ++p;
  checkEq(p->atom.token, ")"); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

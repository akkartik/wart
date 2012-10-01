void test_parse_handles_empty_stream() {
  CodeStream cs(stream(""));
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_trailing_comment() {
  CodeStream cs(stream("34 ; abc"));
  checkEq(nextAstNode(cs), Token("34"));
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_atom() {
  CodeStream cs(stream("34"));
  checkEq(nextAstNode(cs), Token("34"));
  check(cs.eof());
}

void test_parse_handles_atoms() {
  CodeStream cs(stream("34\n\"a b c\"\n3.4"));
  checkEq(nextAstNode(cs), Token("34"));
  checkEq(nextAstNode(cs), Token("\"a b c\""));
  checkEq(nextAstNode(cs), Token("3.4"));
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_forms() {
  CodeStream cs(stream("34 \"a b c\""));
  AstNode n = nextAstNode(cs);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("("));          ++p;
  checkEq(*p, Token("34"));         ++p;
  checkEq(*p, Token("\"a b c\""));  ++p;
  checkEq(*p, Token(")"));          ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_nested_forms() {
  CodeStream cs(stream("34 (2 3) \"a b c\""));
  AstNode n = nextAstNode(cs);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("("));          ++p;
  checkEq(*p, Token("34"));         ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems;  ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("("));        ++q;
    checkEq(*q, Token("2"));        ++q;
    checkEq(*q, Token("3"));        ++q;
    checkEq(*q, Token(")"));        ++q;
    check(q == ast2.end());

  checkEq(*p, Token("\"a b c\""));  ++p;
  checkEq(*p, Token(")"));          ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_nested_forms_with_comments() {
  CodeStream cs(stream("(a b (c d ;\n))"));
  AstNode n = nextAstNode(cs);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("("));          ++p;
  checkEq(*p, Token("a"));          ++p;
  checkEq(*p, Token("b"));          ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems;  ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("("));        ++q;
    checkEq(*q, Token("c"));        ++q;
    checkEq(*q, Token("d"));        ++q;
    checkEq(*q, Token(")"));        ++q;
    check(q == ast2.end());

  checkEq(*p, Token(")"));          ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_quotes() {
  CodeStream cs(stream("34 `(2 ,b) ',35 ,',36 ,'a"));
  AstNode n = nextAstNode(cs);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("("));          ++p;
  checkEq(*p, Token("34"));         ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems;  ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("`"));        ++q;
    checkEq(*q, Token("("));        ++q;
    checkEq(*q, Token("2"));        ++q;
    check(q->isList());
      list<AstNode> ast3 = q->elems; ++q;
      list<AstNode>::iterator r = ast3.begin();
      checkEq(*r, Token(","));      ++r;
      checkEq(*r, Token("b"));      ++r;
      check(r == ast3.end());
    checkEq(*q, Token(")"));        ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems;                ++p;
    q = ast2.begin();
    checkEq(*q, Token("'"));        ++q;
    checkEq(*q, Token(","));        ++q;
    checkEq(*q, Token("35"));       ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems;                ++p;
    q = ast2.begin();
    checkEq(*q, Token(","));        ++q;
    checkEq(*q, Token("'"));        ++q;
    checkEq(*q, Token(","));        ++q;
    checkEq(*q, Token("36"));       ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems;                ++p;
    q = ast2.begin();
    checkEq(*q, Token(","));        ++q;
    checkEq(*q, Token("'"));        ++q;
    checkEq(*q, Token("a"));        ++q;
    check(q == ast2.end());

  checkEq(*p, Token(")"));          ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_splice_operators() {
  CodeStream cs(stream("`(2 ,@b @,c)"));
  AstNode n = nextAstNode(cs);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("`"));          ++p;
  checkEq(*p, Token("("));          ++p;
  checkEq(*p, Token("2"));          ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems;  ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token(",@"));       ++q;
    checkEq(*q, Token("b"));        ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token("@"));        ++q;
    checkEq(*q, Token(","));        ++q;
    checkEq(*q, Token("c"));        ++q;
    check(q == ast2.end());
  checkEq(*p, Token(")"));          ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_handles_indented_toplevel_forms() {
  CodeStream cs(stream("a\n  a b c\n    d"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(n.atom.token, "a");

  n = nextAstNode(cs);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  check(p->isAtom());
  checkEq(p->atom.token, "(");      ++p;
  checkEq(p->atom.token, "a");      ++p;
  checkEq(p->atom.token, "b");      ++p;
  checkEq(p->atom.token, "c");      ++p;
  checkEq(p->atom.token, "d");      ++p;
  checkEq(p->atom.token, ")");      ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

void test_parse_receives_spacesBefore() {
  CodeStream cs(stream("a  (b    c)"));
  AstNode n = nextAstNode(cs);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  check(p->isAtom());
  checkEq(p->atom.token, "(");      ++p;
  checkEq(p->atom.token, "a");      ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems;  ++p;
    list<AstNode>::iterator q = ast2.begin();
    check(q->isAtom());
    checkEq(q->atom.spacesBefore, 2);
    checkEq(q->atom.token, "(");    ++q;
    checkEq(q->atom.token, "b");    ++q;
    checkEq(q->atom.spacesBefore, 4);
    checkEq(q->atom.token, "c");    ++q;
    checkEq(q->atom.token, ")");    ++q;
    check(q == ast2.end());
  check(p->isAtom());
  checkEq(p->atom.token, ")");      ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(cs), eof());
  check(cs.eof());
}

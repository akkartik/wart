void test_parse_handles_empty_stream() {
  CodeStream c(stream(""));
  checkEq(nextAstNode(c), Token::of("nil"));
  check(c.fd.eof());
}

void test_parse_handles_trailing_comment() {
  CodeStream c(stream("34 ; abc"));
  checkEq(nextAstNode(c), Token::of("34"));
  checkEq(nextAstNode(c), Token::of("nil"));
  check(c.fd.eof());
}

void test_parse_handles_atom() {
  CodeStream c(stream("34"));
  checkEq(nextAstNode(c), Token::of("34"));
  check(c.fd.eof());
}

void test_parse_handles_atoms() {
  CodeStream c(stream("34\n\"a b c\"\n3.4"));
  checkEq(nextAstNode(c), Token::of("34"));
  checkEq(nextAstNode(c), Token::of("\"a b c\""));
  checkEq(nextAstNode(c), Token::of("3.4"));
  check(c.fd.eof());
}

void test_parse_handles_forms() {
  CodeStream c(stream("34 \"a b c\""));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of("(")); ++p;
  checkEq(*p, Token::of("34")); ++p;
  checkEq(*p, Token::of("\"a b c\"")); ++p;
  checkEq(*p, Token::of(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_nested_forms() {
  CodeStream c(stream("34 (2 3) \"a b c\""));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of("(")); ++p;
  checkEq(*p, Token::of("34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of("(")); ++q;
    checkEq(*q, Token::of("2")); ++q;
    checkEq(*q, Token::of("3")); ++q;
    checkEq(*q, Token::of(")")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token::of("\"a b c\"")); ++p;
  checkEq(*p, Token::of(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_nested_forms_with_comments() {
  CodeStream c(stream("(a b (c d ;\n))"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of("(")); ++p;
  checkEq(*p, Token::of("a")); ++p;
  checkEq(*p, Token::of("b")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of("(")); ++q;
    checkEq(*q, Token::of("c")); ++q;
    checkEq(*q, Token::of("d")); ++q;
    checkEq(*q, Token::of(")")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token::of(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_quotes() {
  CodeStream c(stream("34 `(2 ,b) ',35 ,',36 ,'a"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of("(")); ++p;
  checkEq(*p, Token::of("34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of("`")); ++q;
    checkEq(*q, Token::of("(")); ++q;
    checkEq(*q, Token::of("2")); ++q;
    check(q->isList());
      list<AstNode> ast3 = q->elems; ++q;
      list<AstNode>::iterator r = ast3.begin();
      checkEq(*r, Token::of(",")); ++r;
      checkEq(*r, Token::of("b")); ++r;
      check(r == ast3.end());
    checkEq(*q, Token::of(")")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of("'")); ++q;
    checkEq(*q, Token::of(",")); ++q;
    checkEq(*q, Token::of("35")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of(",")); ++q;
    checkEq(*q, Token::of("'")); ++q;
    checkEq(*q, Token::of(",")); ++q;
    checkEq(*q, Token::of("36")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of(",")); ++q;
    checkEq(*q, Token::of("'")); ++q;
    checkEq(*q, Token::of("a")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token::of(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

void test_parse_handles_splice_operators() {
  CodeStream c(stream("`(2 ,@b @,c)"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of("`")); ++p;
  checkEq(*p, Token::of("(")); ++p;
  checkEq(*p, Token::of("2")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of(",@")); ++q;
    checkEq(*q, Token::of("b")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of("@")); ++q;
    checkEq(*q, Token::of(",")); ++q;
    checkEq(*q, Token::of("c")); ++q;
    check(q == ast2.end());
  checkEq(*p, Token::of(")")); ++p;
  check(p == n.elems.end());
  check(c.fd.eof());
}

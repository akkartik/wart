void test_parse_handles_atom() {
  CodeStream c(stream(L"34"));
  checkEq(nextAstNode(c), Token::of(L"34"));
  check(eof(c.fd));
}

void test_parse_handles_atoms() {
  CodeStream c(stream(L"34\n\"a b c\""));
  checkEq(nextAstNode(c), Token::of(L"34"));
  checkEq(nextAstNode(c), Token::of(L"\"a b c\""));
  check(eof(c.fd));
}

void test_parse_handles_forms() {
  CodeStream c(stream(L"34 \"a b c\""));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of(L"(")); ++p;
  checkEq(*p, Token::of(L"34")); ++p;
  checkEq(*p, Token::of(L"\"a b c\"")); ++p;
  checkEq(*p, Token::of(L")")); ++p;
  check(p == n.elems.end());
  check(eof(c.fd));
}

void test_parse_handles_nested_forms() {
  CodeStream c(stream(L"34 (2 3) \"a b c\""));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of(L"(")); ++p;
  checkEq(*p, Token::of(L"34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of(L"(")); ++q;
    checkEq(*q, Token::of(L"2")); ++q;
    checkEq(*q, Token::of(L"3")); ++q;
    checkEq(*q, Token::of(L")")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token::of(L"\"a b c\"")); ++p;
  checkEq(*p, Token::of(L")")); ++p;
  check(p == n.elems.end());
  check(eof(c.fd));
}

void test_parse_handles_nested_forms_with_comments() {
  CodeStream c(stream(L"(a b (c d ;\n))"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of(L"(")); ++p;
  checkEq(*p, Token::of(L"a")); ++p;
  checkEq(*p, Token::of(L"b")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of(L"(")); ++q;
    checkEq(*q, Token::of(L"c")); ++q;
    checkEq(*q, Token::of(L"d")); ++q;
    checkEq(*q, Token::of(L")")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token::of(L")")); ++p;
  check(p == n.elems.end());
  check(eof(c.fd));
}

void test_parse_handles_quotes() {
  CodeStream c(stream(L"34 `(2 ,b) ',35 ,',36 ,'a"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of(L"(")); ++p;
  checkEq(*p, Token::of(L"34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of(L"`")); ++q;
    checkEq(*q, Token::of(L"(")); ++q;
    checkEq(*q, Token::of(L"2")); ++q;
    check(q->isList());
      list<AstNode> ast3 = q->elems; ++q;
      list<AstNode>::iterator r = ast3.begin();
      checkEq(*r, Token::of(L",")); ++r;
      checkEq(*r, Token::of(L"b")); ++r;
      check(r == ast3.end());
    checkEq(*q, Token::of(L")")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of(L"'")); ++q;
    checkEq(*q, Token::of(L",")); ++q;
    checkEq(*q, Token::of(L"35")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of(L",")); ++q;
    checkEq(*q, Token::of(L"'")); ++q;
    checkEq(*q, Token::of(L",")); ++q;
    checkEq(*q, Token::of(L"36")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of(L",")); ++q;
    checkEq(*q, Token::of(L"'")); ++q;
    checkEq(*q, Token::of(L"a")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token::of(L")")); ++p;
  check(p == n.elems.end());
  check(eof(c.fd));
}

void test_parse_handles_splice_operators() {
  CodeStream c(stream(L"`(2 ,@b @,c)"));
  AstNode n = nextAstNode(c);
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token::of(L"`")); ++p;
  checkEq(*p, Token::of(L"(")); ++p;
  checkEq(*p, Token::of(L"2")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token::of(L",@")); ++q;
    checkEq(*q, Token::of(L"b")); ++q;
    check(q == ast2.end());
  check(p->isList());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token::of(L"@")); ++q;
    checkEq(*q, Token::of(L",")); ++q;
    checkEq(*q, Token::of(L"c")); ++q;
    check(q == ast2.end());
  checkEq(*p, Token::of(L")")); ++p;
  check(p == n.elems.end());
  check(eof(c.fd));
}

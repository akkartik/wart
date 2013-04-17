void test_parse_handles_empty_stream() {
  IndentSensitiveStream in("");
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_trailing_comment() {
  IndentSensitiveStream in("34 # abc");
  checkEq(nextAstNode(in), Token("34"));
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_atom() {
  IndentSensitiveStream in("34");
  checkEq(nextAstNode(in), Token("34"));
}

void test_parse_handles_atoms() {
  IndentSensitiveStream in("34\n\"a b c\"\n3.4");
  checkEq(nextAstNode(in), Token("34"));
  checkEq(nextAstNode(in), Token("\"a b c\""));
  checkEq(nextAstNode(in), Token("3.4"));
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_forms() {
  IndentSensitiveStream in("(34 \"a b c\")");
  AstNode n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  checkEq(*p, Token("\"a b c\"")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_nested_forms() {
  IndentSensitiveStream in("(34 (2 3) \"a b c\")");
  AstNode n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  check(!p->elems.empty());
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
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_nested_forms_with_comments() {
  IndentSensitiveStream in("(a b (c d #\n))");
  AstNode n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  check(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("(")); ++q;
    checkEq(*q, Token("c")); ++q;
    checkEq(*q, Token("d")); ++q;
    checkEq(*q, Token(")")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_quotes() {
  IndentSensitiveStream in("(34 `(2 ,b) ',35 ,',36 ,'a)");
  AstNode n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  check(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token("`")); ++q;
    checkEq(*q, Token("(")); ++q;
    checkEq(*q, Token("2")); ++q;
    check(!q->elems.empty());
      list<AstNode> ast3 = q->elems; ++q;
      list<AstNode>::iterator r = ast3.begin();
      checkEq(*r, Token(",")); ++r;
      checkEq(*r, Token("b")); ++r;
      check(r == ast3.end());
    checkEq(*q, Token(")")); ++q;
    check(q == ast2.end());
  check(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token("'")); ++q;
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("35")); ++q;
    check(q == ast2.end());
  check(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("'")); ++q;
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("36")); ++q;
    check(q == ast2.end());
  check(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("'")); ++q;
    checkEq(*q, Token("a")); ++q;
    check(q == ast2.end());

  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_splice_operators() {
  IndentSensitiveStream in("`(2 ,@b @,c)");
  AstNode n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("`")); ++p;
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("2")); ++p;
  check(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token(",@")); ++q;
    checkEq(*q, Token("b")); ++q;
    check(q == ast2.end());
  check(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    checkEq(*q, Token("@")); ++q;
    checkEq(*q, Token(",")); ++q;
    checkEq(*q, Token("c")); ++q;
    check(q == ast2.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_indented_toplevel_forms() {
  IndentSensitiveStream in("a\n  a b c\n    d");
  AstNode n = nextAstNode(in);
  check(n.elems.empty());
  checkEq(n.atom.token, "a");

  n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  check(p->elems.empty());
  checkEq(p->atom.token, "("); ++p;
  checkEq(p->atom.token, "a"); ++p;
  checkEq(p->atom.token, "b"); ++p;
  checkEq(p->atom.token, "c"); ++p;
  checkEq(p->atom.token, "d"); ++p;
  checkEq(p->atom.token, ")"); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_metadata() {
  IndentSensitiveStream in(":(2 3)");
  AstNode n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token(":(")); ++p;
  checkEq(*p, Token("2")); ++p;
  checkEq(*p, Token("3")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_metadata2() {
  IndentSensitiveStream in("(34 :(2 3))");
  AstNode n = nextAstNode(in);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  check(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    checkEq(*q, Token(":(")); ++q;
    checkEq(*q, Token("2")); ++q;
    checkEq(*q, Token("3")); ++q;
    checkEq(*q, Token(")")); ++q;
    check(q == ast2.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(in), eof());
}

void test_parse_handles_empty_stream() {
  stringstream in("");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_trailing_comment() {
  stringstream in("34 # abc");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  checkEq(nextAstNode(tokens), Token("34"));
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_atom() {
  stringstream in("34");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  checkEq(nextAstNode(tokens), Token("34"));
}

void test_parse_handles_atoms() {
  stringstream in("34\n\"a b c\"\n3.4");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  checkEq(nextAstNode(tokens), Token("34"));
  checkEq(nextAstNode(tokens), Token("\"a b c\""));
  checkEq(nextAstNode(tokens), Token("3.4"));
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_forms() {
  stringstream in("(34 \"a b c\")");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = nextAstNode(tokens);
  check(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("34")); ++p;
  checkEq(*p, Token("\"a b c\"")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_nested_forms() {
  stringstream in("(34 (2 3) \"a b c\")");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = nextAstNode(tokens);
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
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_nested_forms_with_comments() {
  stringstream in("(a b (c d #\n))");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = nextAstNode(tokens);
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
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_quotes() {
  stringstream in("(34 `(2 ,b) ',35 ,',36 ,'a)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = nextAstNode(tokens);
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
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_splice_operators() {
  stringstream in("`(2 ,@b @,c)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = nextAstNode(tokens);
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
  checkEq(nextAstNode(tokens), eof());
}

void test_parse_handles_indented_toplevel_forms() {
  stringstream in("a\n  a b c\n    d");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = nextAstNode(tokens);
  check(n.elems.empty());
  checkEq(n.atom.token, "a");

  n = nextAstNode(tokens);
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
  checkEq(nextAstNode(tokens), eof());
}

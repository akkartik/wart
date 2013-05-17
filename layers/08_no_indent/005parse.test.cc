void test_parse_handles_empty_stream() {
  stringstream in("");
  CHECK_EQ(nextAstNode(in), "");
}

void test_parse_handles_trailing_comment() {
  stringstream in("34 # abc");
  CHECK_EQ(nextAstNode(in), Token("34"));
  CHECK_EQ(nextAstNode(in), "");
}

void test_parse_handles_atom() {
  stringstream in("34");
  CHECK_EQ(nextAstNode(in), Token("34"));
}

void test_parse_handles_atoms() {
  stringstream in("34\n\"a b c\"\n3.4");
  CHECK_EQ(nextAstNode(in), Token("34"));
  CHECK_EQ(nextAstNode(in), Token("\"a b c\""));
  CHECK_EQ(nextAstNode(in), Token("3.4"));
  CHECK_EQ(nextAstNode(in), "");
}

void test_parse_handles_forms() {
  stringstream in("(34 \"a b c\")");
  AstNode n = nextAstNode(in);
  CHECK(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  CHECK_EQ(*p, Token("(")); ++p;
  CHECK_EQ(*p, Token("34")); ++p;
  CHECK_EQ(*p, Token("\"a b c\"")); ++p;
  CHECK_EQ(*p, Token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(nextAstNode(in), "");
}

void test_parse_handles_nested_forms() {
  stringstream in("(34 (2 3) \"a b c\")");
  AstNode n = nextAstNode(in);
  CHECK(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  CHECK_EQ(*p, Token("(")); ++p;
  CHECK_EQ(*p, Token("34")); ++p;
  CHECK(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    CHECK_EQ(*q, Token("(")); ++q;
    CHECK_EQ(*q, Token("2")); ++q;
    CHECK_EQ(*q, Token("3")); ++q;
    CHECK_EQ(*q, Token(")")); ++q;
    CHECK(q == ast2.end());

  CHECK_EQ(*p, Token("\"a b c\"")); ++p;
  CHECK_EQ(*p, Token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(nextAstNode(in), "");
}

void test_parse_handles_nested_forms_with_comments() {
  stringstream in("(a b (c d #\n))");
  AstNode n = nextAstNode(in);
  CHECK(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  CHECK_EQ(*p, Token("(")); ++p;
  CHECK_EQ(*p, Token("a")); ++p;
  CHECK_EQ(*p, Token("b")); ++p;
  CHECK(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    CHECK_EQ(*q, Token("(")); ++q;
    CHECK_EQ(*q, Token("c")); ++q;
    CHECK_EQ(*q, Token("d")); ++q;
    CHECK_EQ(*q, Token(")")); ++q;
    CHECK(q == ast2.end());

  CHECK_EQ(*p, Token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(nextAstNode(in), "");
}

void test_parse_handles_quotes() {
  stringstream in("(34 `(2 ,b) ',35 ,',36 ,'a)");
  AstNode n = nextAstNode(in);
  CHECK(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  CHECK_EQ(*p, Token("(")); ++p;
  CHECK_EQ(*p, Token("34")); ++p;
  CHECK(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    CHECK_EQ(*q, Token("`")); ++q;
    CHECK_EQ(*q, Token("(")); ++q;
    CHECK_EQ(*q, Token("2")); ++q;
    CHECK(!q->elems.empty());
      list<AstNode> ast3 = q->elems; ++q;
      list<AstNode>::iterator r = ast3.begin();
      CHECK_EQ(*r, Token(",")); ++r;
      CHECK_EQ(*r, Token("b")); ++r;
      CHECK(r == ast3.end());
    CHECK_EQ(*q, Token(")")); ++q;
    CHECK(q == ast2.end());
  CHECK(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    CHECK_EQ(*q, Token("'")); ++q;
    CHECK_EQ(*q, Token(",")); ++q;
    CHECK_EQ(*q, Token("35")); ++q;
    CHECK(q == ast2.end());
  CHECK(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    CHECK_EQ(*q, Token(",")); ++q;
    CHECK_EQ(*q, Token("'")); ++q;
    CHECK_EQ(*q, Token(",")); ++q;
    CHECK_EQ(*q, Token("36")); ++q;
    CHECK(q == ast2.end());
  CHECK(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    CHECK_EQ(*q, Token(",")); ++q;
    CHECK_EQ(*q, Token("'")); ++q;
    CHECK_EQ(*q, Token("a")); ++q;
    CHECK(q == ast2.end());

  CHECK_EQ(*p, Token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(nextAstNode(in), "");
}

void test_parse_handles_splice_operators() {
  stringstream in("`(2 ,@b)");
  AstNode n = nextAstNode(in);
  CHECK(!n.elems.empty());
  list<AstNode>::iterator p = n.elems.begin();
  CHECK_EQ(*p, Token("`")); ++p;
  CHECK_EQ(*p, Token("(")); ++p;
  CHECK_EQ(*p, Token("2")); ++p;
  CHECK(!p->elems.empty());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    CHECK_EQ(*q, Token(",@")); ++q;
    CHECK_EQ(*q, Token("b")); ++q;
    CHECK(q == ast2.end());
  CHECK_EQ(*p, Token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(nextAstNode(in), "");
}

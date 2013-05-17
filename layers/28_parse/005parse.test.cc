void test_parse_handles_empty_stream() {
  stringstream in("");
  readAll(in);
  checkTraceContents("parse", "\n");  // extra newline artifact
}

void test_parse_handles_trailing_comment() {
  stringstream in("34 # abc");
  readAll(in);
  checkTraceContents("parse", "34\n\n");  // extra newline artifact
}

void test_parse_handles_atom() {
  stringstream in("34");
  readAll(in);
  checkTraceContents("parse", "34\n");
}

void test_parse_handles_atoms() {
  stringstream in("34\n\"a b c\"\n3.4");
  readAll(in);
  checkTraceContents("parse", "34\n\"a b c\"\n3.4\n");
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

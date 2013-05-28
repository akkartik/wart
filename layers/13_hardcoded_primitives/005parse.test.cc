void test_parse_handles_empty_stream() {
  stringstream in("");
  CHECK_EQ(next_ast_node(in), "");
}

void test_parse_handles_trailing_comment() {
  stringstream in("34 # abc");
  CHECK_EQ(next_ast_node(in), token("34"));
  CHECK_EQ(next_ast_node(in), "");
}

void test_parse_handles_atom() {
  stringstream in("34");
  CHECK_EQ(next_ast_node(in), token("34"));
}

void test_parse_handles_atoms() {
  stringstream in("34\n\"a b c\"\n3.4");
  CHECK_EQ(next_ast_node(in), token("34"));
  CHECK_EQ(next_ast_node(in), token("\"a b c\""));
  CHECK_EQ(next_ast_node(in), token("3.4"));
  CHECK_EQ(next_ast_node(in), "");
}

void test_parse_handles_forms() {
  stringstream in("(34 \"a b c\")");
  ast_node n = next_ast_node(in);
  CHECK(!n.elems.empty());
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, token("(")); ++p;
  CHECK_EQ(*p, token("34")); ++p;
  CHECK_EQ(*p, token("\"a b c\"")); ++p;
  CHECK_EQ(*p, token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(next_ast_node(in), "");
}

void test_parse_handles_nested_forms() {
  stringstream in("(34 (2 3) \"a b c\")");
  ast_node n = next_ast_node(in);
  CHECK(!n.elems.empty());
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, token("(")); ++p;
  CHECK_EQ(*p, token("34")); ++p;
  CHECK(!p->elems.empty());
    list<ast_node> ast2 = p->elems; ++p;
    list<ast_node>::iterator q = ast2.begin();
    CHECK_EQ(*q, token("(")); ++q;
    CHECK_EQ(*q, token("2")); ++q;
    CHECK_EQ(*q, token("3")); ++q;
    CHECK_EQ(*q, token(")")); ++q;
    CHECK(q == ast2.end());

  CHECK_EQ(*p, token("\"a b c\"")); ++p;
  CHECK_EQ(*p, token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(next_ast_node(in), "");
}

void test_parse_handles_nested_forms_with_comments() {
  stringstream in("(a b (c d #\n))");
  ast_node n = next_ast_node(in);
  CHECK(!n.elems.empty());
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, token("(")); ++p;
  CHECK_EQ(*p, token("a")); ++p;
  CHECK_EQ(*p, token("b")); ++p;
  CHECK(!p->elems.empty());
    list<ast_node> ast2 = p->elems; ++p;
    list<ast_node>::iterator q = ast2.begin();
    CHECK_EQ(*q, token("(")); ++q;
    CHECK_EQ(*q, token("c")); ++q;
    CHECK_EQ(*q, token("d")); ++q;
    CHECK_EQ(*q, token(")")); ++q;
    CHECK(q == ast2.end());

  CHECK_EQ(*p, token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(next_ast_node(in), "");
}

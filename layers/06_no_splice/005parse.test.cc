void test_parse_handles_empty_stream() {
  indent_sensitive_stream in("");
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_trailing_comment() {
  indent_sensitive_stream in("34 # abc");
  CHECK_EQ(next_ast_node(in), token("34"));
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_atom() {
  indent_sensitive_stream in("34");
  CHECK_EQ(next_ast_node(in), token("34"));
}

void test_parse_handles_atoms() {
  indent_sensitive_stream in("34\n\"a b c\"\n3.4");
  CHECK_EQ(next_ast_node(in), token("34"));
  CHECK_EQ(next_ast_node(in), token("\"a b c\""));
  CHECK_EQ(next_ast_node(in), token("3.4"));
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_forms() {
  indent_sensitive_stream in("(34 \"a b c\")");
  ast_node n = next_ast_node(in);
  CHECK(!n.elems.empty());
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, token("(")); ++p;
  CHECK_EQ(*p, token("34")); ++p;
  CHECK_EQ(*p, token("\"a b c\"")); ++p;
  CHECK_EQ(*p, token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_nested_forms() {
  indent_sensitive_stream in("(34 (2 3) \"a b c\")");
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
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_nested_forms_with_comments() {
  indent_sensitive_stream in("(a b (c d #\n))");
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
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_quotes() {
  indent_sensitive_stream in("(34 `(2 ,b) ',35 ,',36 ,'a)");
  ast_node n = next_ast_node(in);
  CHECK(!n.elems.empty());
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, token("(")); ++p;
  CHECK_EQ(*p, token("34")); ++p;
  CHECK(!p->elems.empty());
    list<ast_node> ast2 = p->elems; ++p;
    list<ast_node>::iterator q = ast2.begin();
    CHECK_EQ(*q, token("`")); ++q;
    CHECK_EQ(*q, token("(")); ++q;
    CHECK_EQ(*q, token("2")); ++q;
    CHECK(!q->elems.empty());
      list<ast_node> ast3 = q->elems; ++q;
      list<ast_node>::iterator r = ast3.begin();
      CHECK_EQ(*r, token(",")); ++r;
      CHECK_EQ(*r, token("b")); ++r;
      CHECK(r == ast3.end());
    CHECK_EQ(*q, token(")")); ++q;
    CHECK(q == ast2.end());
  CHECK(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    CHECK_EQ(*q, token("'")); ++q;
    CHECK_EQ(*q, token(",")); ++q;
    CHECK_EQ(*q, token("35")); ++q;
    CHECK(q == ast2.end());
  CHECK(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    CHECK_EQ(*q, token(",")); ++q;
    CHECK_EQ(*q, token("'")); ++q;
    CHECK_EQ(*q, token(",")); ++q;
    CHECK_EQ(*q, token("36")); ++q;
    CHECK(q == ast2.end());
  CHECK(!p->elems.empty());
    ast2 = p->elems; ++p;
    q = ast2.begin();
    CHECK_EQ(*q, token(",")); ++q;
    CHECK_EQ(*q, token("'")); ++q;
    CHECK_EQ(*q, token("a")); ++q;
    CHECK(q == ast2.end());

  CHECK_EQ(*p, token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_splice_operators() {
  indent_sensitive_stream in("`(2 ,@b)");
  ast_node n = next_ast_node(in);
  CHECK(!n.elems.empty());
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, token("`")); ++p;
  CHECK_EQ(*p, token("(")); ++p;
  CHECK_EQ(*p, token("2")); ++p;
  CHECK(!p->elems.empty());
    list<ast_node> ast2 = p->elems; ++p;
    list<ast_node>::iterator q = ast2.begin();
    CHECK_EQ(*q, token(",@")); ++q;
    CHECK_EQ(*q, token("b")); ++q;
    CHECK(q == ast2.end());
  CHECK_EQ(*p, token(")")); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(next_ast_node(in), eof());
}

void test_parse_handles_indented_toplevel_forms() {
  indent_sensitive_stream in("a\n  a b c\n    d");
  ast_node n = next_ast_node(in);
  CHECK(n.elems.empty());
  CHECK_EQ(n.atom.value, "a");

  n = next_ast_node(in);
  CHECK(!n.elems.empty());
  list<ast_node>::iterator p = n.elems.begin();
  CHECK(p->elems.empty());
  CHECK_EQ(p->atom.value, "("); ++p;
  CHECK_EQ(p->atom.value, "a"); ++p;
  CHECK_EQ(p->atom.value, "b"); ++p;
  CHECK_EQ(p->atom.value, "c"); ++p;
  CHECK_EQ(p->atom.value, "d"); ++p;
  CHECK_EQ(p->atom.value, ")"); ++p;
  CHECK(p == n.elems.end());
  CHECK_EQ(next_ast_node(in), eof());
}

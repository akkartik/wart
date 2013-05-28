void test_infix_passes_atoms() {
  indent_sensitive_stream in("abc");
  ast_node n = next_ast_node(in);
  CHECK(is_atom(n));
  CHECK_EQ(transform_infix(n), n.atom);
}

void test_infix_passes_ellipses() {
  indent_sensitive_stream in("...");
  ast_node n = next_ast_node(in);
  CHECK(is_atom(n));
  CHECK_EQ(transform_infix(n), n.atom);
}

void test_infix_passes_atoms2() {
  indent_sensitive_stream in("-3.2");
  ast_node n = next_ast_node(in);
  CHECK(is_atom(n));
  CHECK_EQ(transform_infix(n), n.atom);
}

void test_infix_passes_strings() {
  indent_sensitive_stream in("\"a b+c\"");
  ast_node n = next_ast_node(in);
  CHECK(is_atom(n));
  CHECK_EQ(transform_infix(n), n.atom);
}

void test_infix_passes_floats() {
  indent_sensitive_stream in("2e-2");
  ast_node n = next_ast_node(in);
  CHECK_EQ(transform_infix(n), n.atom);
}

void test_infix_passes_dollar_vars() {
  indent_sensitive_stream in("$a");
  ast_node n = next_ast_node(in);
  CHECK(is_atom(n));
  CHECK_EQ(transform_infix(n), n.atom);
}

void test_infix_handles_dollar_op() {
  indent_sensitive_stream in("$+");
  CHECK_EQ(transform_infix(next_ast_node(in)), "$+");
}

void test_infix_handles_op_without_args() {
  indent_sensitive_stream in("(+)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_atom(n));
  CHECK_EQ(n.atom, "+");
}

void test_infix_handles_op_without_args2() {
  indent_sensitive_stream in("(= (+) 3)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "="); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "3"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_dollar_op_without_args() {
  indent_sensitive_stream in("($+)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_atom(n));
  CHECK_EQ(n.atom, "$+");
}

void test_infix_handles_quoting() {
  indent_sensitive_stream in("',a");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_quoting2() {
  indent_sensitive_stream in("',a+b");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_quoting3() {
  indent_sensitive_stream in("',(a + b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_quoting4() {
  indent_sensitive_stream in("',(+)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "'"); ++p;
  CHECK_EQ(*p, ","); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_simple_lists() {
  indent_sensitive_stream in("(a + b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_passes_wrapped_op() {
  indent_sensitive_stream in("(a (+) b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_passes_wrapped_op2() {
  indent_sensitive_stream in("(a (+) b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_prefix() {
  indent_sensitive_stream in("(+ a b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_passes_ellipses_in_infix() {
  indent_sensitive_stream in("(a ... b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "..."); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_passes_op_with_ellipses() {
  indent_sensitive_stream in("(+ ... b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "..."); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_unary_prefix() {
  indent_sensitive_stream in("(+ a)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists() {
  indent_sensitive_stream in("((a + b))");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  CHECK_EQ(n.elems.size(), 3);
  ast_node n2 = *++n.elems.begin();
  CHECK(is_list(n2));
  list<ast_node>::iterator p = n2.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n2.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists2() {
  indent_sensitive_stream in("(do (a + b))");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "do"); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "+"); ++p2;
    CHECK_EQ(*p2, "a"); ++p2;
    CHECK_EQ(*p2, "b"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists3() {
  indent_sensitive_stream in("(a = (a + 1))");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "="); ++p;
  CHECK_EQ(*p, "a"); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "+"); ++p2;
    CHECK_EQ(*p2, "a"); ++p2;
    CHECK_EQ(*p2, "1"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_left_associates() {
  indent_sensitive_stream in("(a + b + c)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "+"); ++p2;
    CHECK_EQ(*p2, "a"); ++p2;
    CHECK_EQ(*p2, "b"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, "c"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_op() {
  indent_sensitive_stream in("(a + b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_always_has_higher_precedence_than_call() {
  indent_sensitive_stream in("(f a + b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "f"); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "+"); ++p2;
    CHECK_EQ(*p2, "a"); ++p2;
    CHECK_EQ(*p2, "b"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_always_has_lower_precedence_than_prefix() {
  indent_sensitive_stream in("-a+b");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "-"); ++p2;
    CHECK_EQ(*p2, "a"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_multiple_infix_ops() {
  indent_sensitive_stream in("(f a + b c + d)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "f"); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "+"); ++p2;
    CHECK_EQ(*p2, "a"); ++p2;
    CHECK_EQ(*p2, "b"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  n2 = *p; ++p;
    CHECK(is_list(n2));
    p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "+"); ++p2;
    CHECK_EQ(*p2, "c"); ++p2;
    CHECK_EQ(*p2, "d"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_op_without_spaces() {
  indent_sensitive_stream in("a+b");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_op_without_spaces2() {
  indent_sensitive_stream in("$a+b");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "$a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_op_with_float() {
  indent_sensitive_stream in("a+1.0");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "1.0"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_op_with_float2() {
  indent_sensitive_stream in("3.0+1.0");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "3.0"); ++p;
  CHECK_EQ(*p, "1.0"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_op_with_float3() {
  indent_sensitive_stream in("a3.b");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "."); ++p;
  CHECK_EQ(*p, "a3"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_gives_ops_without_spaces_precedence() {
  indent_sensitive_stream in("(n * n-1)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "*"); ++p;
  CHECK_EQ(*p, "n"); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "-"); ++p2;
    CHECK_EQ(*p2, "n"); ++p2;
    CHECK_EQ(*p2, "1"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_complement_as_usual() {
  indent_sensitive_stream in("~a.b");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "."); ++p;
  ast_node n2 = *p; ++p;
    CHECK(is_list(n2));
    list<ast_node>::iterator p2 = n2.elems.begin();
    CHECK_EQ(*p2, "("); ++p2;
    CHECK_EQ(*p2, "~"); ++p2;
    CHECK_EQ(*p2, "a"); ++p2;
    CHECK_EQ(*p2, ")"); ++p2;
    CHECK(p2 == n2.elems.end());
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_backquote() {
  indent_sensitive_stream in("`(a + b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, "`"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

void test_infix_handles_unquote_splice() {
  indent_sensitive_stream in(",@a+b");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, ",@"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}
void test_infix_handles_unquote_splice2() {
  indent_sensitive_stream in(",@(a + b)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_list(n));
  list<ast_node>::iterator p = n.elems.begin();
  CHECK_EQ(*p, ",@"); ++p;
  CHECK_EQ(*p, "("); ++p;
  CHECK_EQ(*p, "+"); ++p;
  CHECK_EQ(*p, "a"); ++p;
  CHECK_EQ(*p, "b"); ++p;
  CHECK_EQ(*p, ")"); ++p;
  CHECK(p == n.elems.end());
}

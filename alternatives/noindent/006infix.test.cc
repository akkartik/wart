void test_infix_passes_atoms() {
  stringstream in("abc");
  AstNode n = nextAstNode(in);
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_ellipses() {
  stringstream in("...");
  AstNode n = nextAstNode(in);
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_atoms2() {
  stringstream in("-3.2");
  AstNode n = nextAstNode(in);
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_strings() {
  stringstream in("\"a b+c\"");
  AstNode n = nextAstNode(in);
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_floats() {
  stringstream in("2e-2");
  AstNode n = nextAstNode(in);
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_dollar_vars() {
  stringstream in("$a");
  AstNode n = nextAstNode(in);
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_handles_dollar_op() {
  stringstream in("$+");
  checkEq(transformInfix(nextAstNode(in)), Token("$+"));
}

void test_infix_handles_op_without_args() {
  stringstream in("(+)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isAtom(n));
  checkEq(n.atom, Token("+"));
}

void test_infix_handles_op_without_args2() {
  stringstream in("(= (+) 3)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("=")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("3")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_dollar_op_without_args() {
  stringstream in("($+)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isAtom(n));
  checkEq(n.atom, Token("$+"));
}

void test_infix_handles_quoting() {
  stringstream in("',a");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("'")); ++p;
  checkEq(*p, Token(",")); ++p;
  checkEq(*p, Token("a")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_simple_lists() {
  stringstream in("(a + b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_wrapped_op() {
  stringstream in("(a (+) b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_wrapped_op2() {
  stringstream in("(a (:) b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token(":")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_prefix() {
  stringstream in("(+ a b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_ellipses_in_infix() {
  stringstream in("(a ... b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("...")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_op_with_ellipses() {
  stringstream in("(+ ... b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("...")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_unary_prefix() {
  stringstream in("(+ a)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists() {
  stringstream in("((a + b))");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  checkEq(n.elems.size(), 3);
  AstNode n2 = *++n.elems.begin();
  check(isList(n2));
  list<AstNode>::iterator p = n2.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n2.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists2() {
  stringstream in("(do (a + b))");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("do")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("+")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token("b")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists3() {
  stringstream in("(a = (a + 1))");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("=")); ++p;
  checkEq(*p, Token("a")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("+")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token("1")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_left_associates() {
  stringstream in("(a + b + c)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("+")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token("b")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token("c")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op() {
  stringstream in("(a + b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_always_has_higher_precedence_than_call() {
  stringstream in("(f a + b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("f")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("+")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token("b")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_always_has_lower_precedence_than_prefix() {
  stringstream in("-a+b");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("-")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_multiple_infix_ops() {
  stringstream in("(f a + b c + d)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("f")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("+")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token("b")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  n2 = *p; ++p;
    check(isList(n2));
    p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("+")); ++p2;
    checkEq(*p2, Token("c")); ++p2;
    checkEq(*p2, Token("d")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_without_spaces() {
  stringstream in("a+b");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_without_spaces2() {
  stringstream in("$a+b");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("$a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float() {
  stringstream in("a+1.0");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("1.0")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float2() {
  stringstream in("3.0+1.0");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("3.0")); ++p;
  checkEq(*p, Token("1.0")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float3() {
  stringstream in("a3.b");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token(".")); ++p;
  checkEq(*p, Token("a3")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_gives_ops_without_spaces_precedence() {
  stringstream in("(n * n-1)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("*")); ++p;
  checkEq(*p, Token("n")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("-")); ++p2;
    checkEq(*p2, Token("n")); ++p2;
    checkEq(*p2, Token("1")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_keyword_syms() {
  stringstream in(":a");
  AstNode n = nextAstNode(in);
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_keyword_syms2() {
  stringstream in("(f :a x)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("f")); ++p;
  checkEq(*p, Token(":a")); ++p;
  checkEq(*p, Token("x")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_complement_as_usual() {
  stringstream in("~a.b");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token(".")); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("~")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_backquote() {
  stringstream in("`(a + b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("`")); ++p;
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_splice() {
  stringstream in("@a+b");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("@")); ++p;
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_unquote_splice() {
  stringstream in(",@a+b");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token(",@")); ++p;
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}
void test_infix_handles_unquote_splice2() {
  stringstream in(",@(a + b)");
  AstNode n = transformInfix(nextAstNode(in));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token(",@")); ++p;
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

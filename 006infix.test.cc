void test_infix_passes_atoms() {
  CodeStream cs(stream("abc"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_ellipses() {
  CodeStream cs(stream("..."));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_atoms2() {
  CodeStream cs(stream("-3.2"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_strings() {
  CodeStream cs(stream("\"a b+c\""));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_floats() {
  CodeStream cs(stream("2e-2"));
  AstNode n = nextAstNode(cs);
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_dollar_vars() {
  CodeStream cs(stream("$a"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_handles_dollar_op() {
  CodeStream cs(stream("$+"));
  checkEq(transformInfix(nextAstNode(cs)), Token("$+"));
}

void test_infix_handles_op_without_args() {
  CodeStream cs(stream("(+)"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isAtom());
  checkEq(n.atom, Token("+"));
}

void test_infix_handles_op_without_args2() {
  CodeStream cs(stream("= (+) 3"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("=")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("3")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_dollar_op_without_args() {
  CodeStream cs(stream("($+)"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isAtom());
  checkEq(n.atom, Token("$+"));
}

void test_infix_handles_quoting() {
  CodeStream cs(stream("',a"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("'")); ++p;
  checkEq(*p, Token(",")); ++p;
  checkEq(*p, Token("a")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_simple_lists() {
  CodeStream cs(stream("a + b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_wrapped_op() {
  CodeStream cs(stream("a (+) b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_wrapped_op2() {
  CodeStream cs(stream("a (:) b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token(":")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_prefix() {
  CodeStream cs(stream("+ a b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_ellipses_in_infix() {
  CodeStream cs(stream("a ... b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("...")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_unary_prefix() {
  CodeStream cs(stream("+ a"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists() {
  CodeStream cs(stream("((a + b))"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  checkEq(n.elems.size(), 3);
  AstNode n2 = *++n.elems.begin();
  check(n2.isList());
  list<AstNode>::iterator p = n2.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n2.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists2() {
  CodeStream cs(stream("(do (a + b))"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("do")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream("(a = (a + 1))"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("=")); ++p;
  checkEq(*p, Token("a")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream("(a + b + c)"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream("a + b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_always_has_higher_precedence_than_call() {
  CodeStream cs(stream("f a + b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("f")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream("-a+b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream("f a + b c + d"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("f")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, Token("(")); ++p2;
    checkEq(*p2, Token("+")); ++p2;
    checkEq(*p2, Token("a")); ++p2;
    checkEq(*p2, Token("b")); ++p2;
    checkEq(*p2, Token(")")); ++p2;
    check(p2 == n2.elems.end());
  n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream("a+b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_without_spaces2() {
  CodeStream cs(stream("$a+b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("$a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float() {
  CodeStream cs(stream("a+1.0"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("1.0")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float2() {
  CodeStream cs(stream("3.0+1.0"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("3.0")); ++p;
  checkEq(*p, Token("1.0")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float3() {
  CodeStream cs(stream("a3.b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token(".")); ++p;
  checkEq(*p, Token("a3")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_gives_ops_without_spaces_precedence() {
  CodeStream cs(stream("n * n-1"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("*")); ++p;
  checkEq(*p, Token("n")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream(":a"));
  AstNode n = nextAstNode(cs);
  check(n.isAtom());
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_keyword_syms2() {
  CodeStream cs(stream("f :a x"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("f")); ++p;
  checkEq(*p, Token(":a")); ++p;
  checkEq(*p, Token("x")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_complement_as_usual() {
  CodeStream cs(stream("~a.b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token(".")); ++p;
  AstNode n2 = *p; ++p;
    check(n2.isList());
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
  CodeStream cs(stream("`(a + b)"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
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
  CodeStream cs(stream("@a+b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
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
  CodeStream cs(stream(",@a+b"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
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
  CodeStream cs(stream(",@(a + b)"));
  AstNode n = transformInfix(nextAstNode(cs));
  check(n.isList());
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, Token(",@")); ++p;
  checkEq(*p, Token("(")); ++p;
  checkEq(*p, Token("+")); ++p;
  checkEq(*p, Token("a")); ++p;
  checkEq(*p, Token("b")); ++p;
  checkEq(*p, Token(")")); ++p;
  check(p == n.elems.end());
}
